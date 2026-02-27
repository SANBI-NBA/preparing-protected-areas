# This script is based on DFFE's PACA database in gdb format.
# It is designed to create a snapshot of the protected area network (terrestrial or marine)
# for a particular year. The code should only be used for years before the current year - otherwise
# the map may be incomplete.

# Assumptions:
# Where currently active (declared) PAs overlap, the PA with the most recent parcel declaration date 
# (D_DLARP) anywhere in the PA represents the current version of the PA (except for WHS, which are added in where they 
# do not overlap with PA). Small parts/slivers of older versions that do not align with the latest/newest version
# are merged in with the larger PA to cut down on slivers and number of PAs that need to be processed for effectiveness.
# This process employs quite a brutal elimination method, but results in a much cleaner layer at the end
# The reasoning for using this method is that it preserves the total area protected, but cuts down
# on the number of named PAs that need to be rated for effectiveness.

# Limitations:
# This process is reasonably good at eliminating PA overlaps. It is less good at eliminating
# gaps between adjacent PAs larger than a very small tolerance limit (10 m). Testing found that a
# larger tolerance limit distorts PA polygons too much.
# One could clean up gaps manually, but this also risks introducing consistencies and is therefore not
# recommended.

# Outputs:
# The script is set up so that it creates a snapshot for a specified year. 
# To create a snapshot for a different year (for example creating a PA expansion time series)
# set it to a different year and run the script again. 
# Different snapshots from the same PACA version are written
# to the same gpkg, but it will create a new gpkg when a different version of PACA is used.

# Outputs of some intermediary processing steps are written to a gpkg called "paca_processing.gpkg"
# Use this to inspect outputs in a GIS to check that everything is working OK

# WARNING: DO NOT RUN THIS SCRIPT ALL IN ONE GO. 
# There are some steps that are better done outside R in a GIS


# ---- 1. Setup -------------------------------------------------------------------------------------------

# The following variables must be defined for the script to work

# Input file path for the PACA gdb
  paca_path <- "PACA_June_2025.gdb"

# Information about the PACA version used (month-year)  
  paca_version <- "jun-2025"

# Specify the year for which the PA map must be produced  
  map_year <- 2024

# The script produces separate terrestrial and marine maps
# Use this variable to specify whether you want a "terrestrial" or "marine" PA map
  map_type <- "terrestrial"

# Specify a path to a folder for output map
# Output map will be created in gpkg format
  map_output_path <- "outputs/"
  
# Specify the projection to use
# Recommend using the standard NBA projection
  pa_crs <- "+proj=aea +lat_0=0 +lon_0=25 +lat_1=-24 +lat_2=-33 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
# Distance threshold for identifying PA clusters
# What is the maximum distance for PA polygons to be apart to be considered part of the same cluster
# Units = meters
  distance_threshold <- 1000
  
# Load libaries
  library(sf) # For processing and manipulating vector spatial data
  library(lubridate) # For processing dates & times
  library(dplyr) # For wrangling attribute data
  library(tibble) # Attribute structure manipulation
  library(igraph) # For storing related polygon information for defining clusters
  
# Source functions: requires functions.R to be in the same folder as this script
source("functions.R")  
  
# ---- 2. Load data --------------------------------------------------------------------------------------
  
  # Note this code assumes a single layer in the PACA gdb
  # If there are multiple layers, specify with layer = "layer_name"
  # This takes a while
  paca <- st_read(dsn = paca_path)
  
  # Convert multipolygon to polygon
  paca <- st_cast(paca, "POLYGON")
  
  # For some reason the OBJECTIDs are sometimes lost, assign a unique ID to each polygon in case
  paca <- rowid_to_column(paca, "shape_id")
  
  # Project to NBA standard projection
  paca <- st_transform(paca, pa_crs)
  
  
# ---- 3. Select attributes and filter data --------------------------------------------------------------
  
  # Here we exclude polygons from PACA meeting the following criteria:
  # - not PA
  # - if WHS: WHS - Buffer 
  # - LEGAL_STAT not "Declared" or "Withdrawn"
  # - D_DCLARP (date of parcel declaration) later than map date specified in map_year
  # Filter for marine or terrestrial PA as specified in map_type
  
  # Select attributes
  # Note, this code assumes that PACA's attribute names have not been updated or modified
  
  paca <- paca %>% select(shape_id,
                          WDPAID,
                          CUR_NME,
                          MAJ_TYPE,
                          SITE_TYPE,
                          SITE_STYPE,
                          D_DCLARP,
                          LEGAL_STAT,
                          D_UNDCLAP,
                          MANAGEMENT,
                          PROVINCE)
  
  # Extract year from declaration dates (for use in filtering and finding newest versions of PAs)
  paca <- paca %>% mutate(year_decl = year(paca$D_DCLARP),
                          year_deproc = year(paca$D_UNDCLAP))
  
  # Filter (one attribute at a time)
  paca <- paca %>% filter(MAJ_TYPE == "PA")
  paca <- paca %>% filter(SITE_STYPE != "WHS - Buffer")
  
  # Marine or terrestrial
  if (map_type == "terrestrial"){
    paca <- paca %>% filter(SITE_TYPE != "Marine Protected Area" & PROVINCE != "Marine")
  } else if (map_type == "marine"){
    paca <- paca %>% filter(SITE_TYPE == "Marine Protected Area" | PROVINCE == "Marine")
  } else {
    print("map_type is incorrectly specified. Please indicate terrestrial or marine and run code again.")
  }
  
  # Fix topology errors
  paca <- st_make_valid(paca)
  paca <- st_collection_extract(paca, "POLYGON")
  
  # Split into proclaimed and deproclaimed
  # Filtering excludes all nulls
  paca_proc <- paca %>% filter(year_decl <= map_year & LEGAL_STAT == "Declared")
  paca_deproc <- paca %>% filter(year_deproc <= map_year & LEGAL_STAT == "Withdrawn")
  
# ---- 4. Remove deproclaimed areas --------------------------------------------------------------
  
  # A challenge with PACA is that deproclamations are captured separately from proclamations
  # Deproclaimed areas therefore need to be detected through spatial joins
  # One also needs to navigate the many overlapping versions of some PAs and make sure the deproclamation matches
  # the correct proclaimed shape. Identifier fields such as WDPAID, CUR_NME, FARMNME, FARMNO, SUBDIVNO do not consistently
  # match. The only working approach is to find polygons with equal/matching 
  # geometries between paca_proc and paca_deproc, but even this is not 100% foolproof.
  # At the stage of code development (2026) there are shape mismatches for at least one PA (e.g Driftsands NR).
  # Therefore the approach is to first find more or less matching geometries among the proclaimed polygons and 
  # eliminating them by dropping them from the map. This is a much cleaner approach than erase. Then only, for remaining
  # unmatched deproclamations, erase is used where there are partial overlaps between deproclaimed and proclaimed polygons.
  
  # ---- 4.1 Drop deproclaimed areas where geometries match -----------------------------------------------
  
  # First use sf's st_equals to find polygons in paca_deproc that more or less matches polygons in paca_proc
  # par is the tolerance level for matching geometries in meters
  match_proc_deproc <- st_equals_exact(paca_proc, paca_deproc, par = 5) 
  
  # Convert matches to dataframe to also filter on proclamation/deproclamation date
  match_proc_deproc_df <- data.frame(
                            proc_id = rep(seq_along(match_proc_deproc), lengths(match_proc_deproc)),
                            deproc_id = unlist(match_proc_deproc)
  )
  
  # Join proclamation/deproclamation dates from attributes
  match_proc_deproc_df <- match_proc_deproc_df %>% 
                              mutate(
                                D_DCLARP = paca_proc$D_DCLARP[proc_id],
                                D_UNDCLAP = paca_deproc$D_UNDCLAP[deproc_id])
  
  # Keep only the records where deproclamation date > proclamation date
  # Note that you have to use the complete date to avoid eliminating pieces that were
  # deproclaimed but immediately replaced by something else
  deproc_to_drop <- match_proc_deproc_df %>% 
                            filter(D_UNDCLAP >= D_DCLARP) %>% 
                            pull(proc_id) %>% 
                            unique()
  
  # Remove them from paca_proc
  paca_proc_matched_dropped <- paca_proc[-deproc_to_drop,]
  
  # Check what was dropped
  dropped <- paca_proc[deproc_to_drop,]
  nrow(dropped)
 
  # ---- 4.2 Erase where deproclaimed areas partially overlap proclaimed areas -----------------------
  
  # WIP: This section could be improved by dropping overlap matches where the overlap is a very small % of
  # the proclaimed polygon
  
  # Filter paca_deproc to only include features not spatially matched to paca_proc
  # These represent deproclaimed polygons that do not match any proclaimed polygons
  # ito shape, size and location
  paca_deproc_matched <- unique(unlist(match_proc_deproc))
  paca_deproc_unmatched <- paca_deproc[-paca_deproc_matched,]
  
  # Optional: write outputs to gpkg for comparison to SAPAD in GIS
  st_write(paca_proc_matched_dropped, file.path(map_output_path, "paca_processing.gpkg"), layer = "proclaimed", append = FALSE)
  st_write(dropped, file.path(map_output_path, "paca_processing.gpkg"), layer = "deproclaimed_matched", append = FALSE)
  st_write(paca_deproc_unmatched, file.path(map_output_path, "paca_processing.gpkg"), layer = "deproclaimed_unmatched", append = FALSE)
  
  # In the second step we look among the remaining unmatched deproclaimed sites
  # for overlaps with the retained areas (paca_proc_matched_dropped)
  # We then use an erase function to eliminate just the deproclaimed areas
  
  # Since we need to implement geoprocessing functions, it is a good idea to check for and fix topology errors
  # st_make_valid can result in weird bits (linestrings, points) so cast back to polygon to avoid that
  paca_proc_matched_dropped <- st_cast(paca_proc_matched_dropped, "POLYGON")
  paca_proc_matched_dropped <- st_make_valid(paca_proc_matched_dropped)

  paca_deproc_unmatched <- st_make_valid(paca_deproc_unmatched)
  paca_deproc_unmatched <- st_cast(paca_deproc_unmatched, "POLYGON")
  
  # For clean erasing it is very important that adjacent polygons are snapped to avoid tiny gaps leaving slivers
  # This is a general snapping function (stored in the functions file)
  paca_deproc_unmatched <- fix_polygon_gaps(paca_deproc_unmatched, 
                                            snap_tolerance = 10)
  
  # Find overlapping polygons
  overlap_proc_deproc <- st_overlaps(paca_proc_matched_dropped, paca_deproc_unmatched)
  
  # Convert matches to df, add in declaration dates, filter where deproclaimed is later than proclaimed
  overlap_proc_deproc_df <- data.frame(
            proc_id = rep(seq_along(overlap_proc_deproc), lengths(overlap_proc_deproc)),
            deproc_id = unlist(overlap_proc_deproc)
    ) %>%
    mutate(
      D_DCLARP = paca_proc_matched_dropped$D_DCLARP[proc_id],
      D_UNDCLAP = paca_deproc_unmatched$D_UNDCLAP[deproc_id],
      proc_shape_id = paca_proc_matched_dropped$shape_id[proc_id],
      deproc_shape_id = paca_deproc_unmatched$shape_id[deproc_id],
      proc_name = paca_proc_matched_dropped$CUR_NME[proc_id],
      deproc_name = paca_deproc_unmatched$CUR_NME[deproc_id]
    ) %>% 
    filter(D_UNDCLAP >= D_DCLARP
    ) %>% 
    select(proc_id, proc_shape_id, proc_name, 
           deproc_id, deproc_shape_id, deproc_name, 
           D_DCLARP, D_UNDCLAP)
  
  # Group together polygons that erase the same feature
  erase_groups <- overlap_proc_deproc_df %>% 
                      group_by(proc_shape_id) %>% 
                      dplyr::summarise(deproc_ids = list(deproc_shape_id), .groups = "drop")
  
  # Pull out polygons that need erasing and set the rest aside
  paca_proc_clean <- paca_proc_matched_dropped %>% filter(!(shape_id %in% erase_groups$proc_shape_id))
  paca_proc_matched_dropped <- paca_proc_matched_dropped %>% filter(shape_id %in% erase_groups$proc_shape_id)


  # Now loop through the erase pairs and erase the area in paca_proc_matched_dropped with the matched polygon(s)
  # in paca_deproc_unmatched
  for (i in erase_groups$proc_shape_id) {
    
    deproc_ids <- erase_groups %>% filter(proc_shape_id == i)

    input_polygon <- paca_proc_matched_dropped %>% filter(paca_proc_matched_dropped$shape_id == i)
    erase_polygon <- paca_deproc_unmatched %>% filter(shape_id %in% deproc_ids$deproc_ids[[1]])
    erase_polygon <- st_union(st_geometry(erase_polygon))
    
    erased_geometry <- erase_with_tolerance(input_polygon, erase_polygon, tolerance=1)
    
    if (st_is_empty(erased_geometry)) {
      paca_proc_matched_dropped <- paca_proc_matched_dropped %>% filter(shape_id != i)
    } else {
      st_geometry(paca_proc_matched_dropped)[paca_proc_matched_dropped$shape_id == i] <- erased_geometry
    }
  }
  
  # Fix topology
  paca_proc_matched_dropped <- st_make_valid(paca_proc_matched_dropped)
  # Cast the whole thing to polygon
  paca_proc_matched_dropped <- sf::st_cast(paca_proc_matched_dropped, "POLYGON")
  
  paca_proc_clean <- bind_rows(paca_proc_clean, paca_proc_matched_dropped)
  
  #Optional: write layer to gpkg for checking
  st_write(paca_proc_clean, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "proclaimed_clean", append = FALSE)

# ---- 5. Identify clusters --------------------------------------------------------------
  
  # Clustering of protected areas assists in cleaning up overlaps
  # It is also useful for applying effectiveness, and for confirming presences of species in PAs
  
  # Get sparse list of polygons that are within the distance threshold of each other
  # This is the fastest way to do it but still takes a long time
  # This process could be done much faster in QGIS/Arc if time is tight/processing power is limited
  # The model identify-pa-clusters reproduces the code here
  
  neighbors <- st_is_within_distance(paca_proc_clean, paca_proc_clean, dist = distance_threshold)
  
  # Optimized build of edges from neighbour list
  edge_list <- lapply(seq_along(neighbors), function(i) {
    j <- neighbors[[i]]
    j <- j[j > i]  # avoid duplicate edges
    if (length(j) == 0) return(NULL)
    cbind(i, j)
  })
  
  # Remove NULL entries
  edge_list <- edge_list[!sapply(edge_list, is.null)]
  
  # Now combine
  edges <- do.call(rbind, edge_list)
  
  # Create igraph to find connected polygons
  g <- graph_from_edgelist(edges, directed = FALSE)
  
  # Assign cluster ids from graph connections
  paca_proc_clean$cluster_id <- components(g)$membership
  
  # ---- 5.1 Optional: write layer to gpkg for checking ----------------------------
  st_write(paca_proc_clean, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "proclaimed_clean", append = FALSE)
  
  # Alternative:
  # Write the proclaimed layer to geopackage
  st_write(paca_proc_clean, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "proclaimed_clean", append = FALSE)
  
  # Run the model identify-pa-clusters in QGIS
  # Read the results back in
  paca_proc_clean <- st_read(file.path(map_output_path, "paca_processing.gpkg"), 
                             layer = "proclaimed_clean_clusters") #Or whatever you saved it as
  
# ---- 6. Clean up proclaimed PAs ----------------------------------
 
  # Make a list of PAs
  # This is a combination of PA IDs, names, types, management authorities, and location
  # It does not work to just group by ID or name, because there are multiple PAs with the same name
  # some PAs are partially managed by different authorities, which means they need to be kept separate
  # for effectiveness assessment. We therefore assign an assessment specific PA ID here, to facilitate PA
  # data processing. The WDPAID is maintained to enable cross linking between different versions of PACA.
  # Max proclamation date helps to identify the current name and designation of a PA where there are overlaps
  # resulting from PAs changing names, type, extent, and management authorities over time
  
  pa_list <- st_drop_geometry(paca_proc_clean) %>% 
                        group_by(cluster_id, WDPAID, CUR_NME, SITE_TYPE, MANAGEMENT) %>% 
                        summarise(max_decl = max(year_decl)) %>% ungroup()
  pa_list <- rowid_to_column(pa_list, "pa_id")
  
  # Join pa IDs to paca_proc_clean
  paca_proc_clean <- left_join(paca_proc_clean, pa_list, 
                      by = c("cluster_id", "WDPAID", "CUR_NME", "SITE_TYPE", "MANAGEMENT")) %>% 
                      select(pa_id, cluster_id, WDPAID, CUR_NME, SITE_TYPE, MANAGEMENT, max_decl)
  
  # Loop through each PA ID in paca_proc_clean
  # Clean up polygons:
  #  - snap geometries
  #  - do a clean dissolve (dropping out holes left by small gaps) 
  #  - convert to single part (to get PA sections) and drop any polygons smaller than 1 ha
  
  pa_processed <- process_protected_areas(paca_proc_clean, pa_id) #This takes long
  pa_processed <- rowid_to_column(pa_processed, "sec_id")
  
   st_write(pa_processed, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "pa_processed", append = FALSE)
  
# ---- 7. Eliminate overlaps ----------------------------------
  
  # The biggest challenge with eliminating overlaps is that edges do not line up
  # So if you erase or union you get INSANE slivers
  # The approach taken here is to union slivers of older PAs with the newest PA where edges do
  # not align. Older PAs are kept only where a substantial part does not overlap with the newest version
  # PAs and WHS are processed separately - WHS are only included where they do not overlap with any other
  # type of PA
  
  # Step 1: split out WHS
  whs <- pa_processed %>% filter(SITE_TYPE == "World Heritage Site")
  pa <- pa_processed %>% filter(SITE_TYPE != "World Heritage Site")
  
  # Step 2: Find the clusters with multiple polygons in need of processing
  # Count the number of polygons in each cluster - if 1, then don't need processing
  cluster_counts <- st_drop_geometry(pa) %>% group_by(cluster_id) %>% summarise(count = n())
  cluster_count_1 <- cluster_counts %>% filter(count == 1)
  cluster_count_more <- cluster_counts %>% filter(count != 1)
  
  pa_processing <- pa %>% filter(cluster_id %in% cluster_count_more$cluster_id)
  pa <- pa %>% filter(cluster_id %in% cluster_count_1$cluster_id)
  
  # Step 3: Find clusters with overlapping polygons
  # Find overlapping polygons:
  # pattern = "T********" finds all polygons that partially or wholly overlap, but not the ones
  # that are just touching. It is more inclusive than st_overlaps
  overlap_clusters <- st_relate(pa_processing, pattern = "T********") 
  
  # Convert matches to df, add in data needed for processing
  overlap_list <- data.frame(
    id = rep(seq_along(overlap_clusters), lengths(overlap_clusters)),
    overlap_id = unlist(overlap_clusters)
  ) %>%
    mutate(
      cluster_id = pa_processing$cluster_id[id],
      pa_id = pa_processing$pa_id[id],
      sec_id = pa_processing$sec_id[id],
      overlap_pa_id = pa_processing$pa_id[overlap_id],
      overlap_sec_id = pa_processing$sec_id[overlap_id],
      pa_decl = pa_processing$max_decl[id],
      overlap_decl = pa_processing$max_decl[overlap_id]
      ) %>% 
    select(cluster_id, pa_id, sec_id, pa_decl, overlap_pa_id, overlap_sec_id, overlap_decl) %>% 
    filter(sec_id != overlap_sec_id)
  
  # Find non-overlapping pas and move them to pa
  non_overlapping_pa <- pa_processing %>% filter(!(pa_id %in% overlap_list$pa_id))
  pa <- rbind(pa, non_overlapping_pa)
  
  # Keep only overlapping PAs for processing
  pa_processing <- pa_processing %>%  filter(pa_id %in% overlap_list$pa_id)
  
  # Filter the overlaps where pa is newer than overlap pa
  # Sometimes adjacent PAs with the same declaration date overlap slightly due to digitisation errors
  # We also want to remove those overlaps, so including them here (that's why it is >=)
  overlap_list <- overlap_list %>% filter(pa_decl >= overlap_decl)
  # Sort by decl date from newest to oldest
  overlap_list <- overlap_list %>% dplyr::arrange(desc(pa_decl), desc(overlap_decl))
  
  # Step 4: Loop through the list of overlap pairs and process each one individually
  for (i in seq_len(nrow(overlap_list))) {
    
    id1 <- overlap_list$sec_id[i]
    id2 <- overlap_list$overlap_sec_id[i]
    
    idx1 <- match(id1, pa_processing$sec_id)
    idx2 <- match(id2, pa_processing$sec_id)
    
    # 1. Skip if either missing
    if (is.na(idx1) || is.na(idx2)) next
    
    geom1 <- make_valid_safe(st_geometry(pa_processing)[idx1])
    geom2 <- make_valid_safe(st_geometry(pa_processing)[idx2])
    
    if (is.null(geom1) || is.null(geom2)) next
    
    # Snap to improve accuracy of erasing, then make valid again
    geom1 <- st_snap(geom1, geom2, tolerance = 10)
    geom1 <- make_valid_safe(geom1)
    geom2 <- st_snap(geom2, geom1, tolerance = 10)
    geom2 <- make_valid_safe(geom2)
    
    # 3. Original area
    orig_area <- as.numeric(st_area(geom2))
    
    # 4. Erase
    erased <- st_difference(geom2, geom1)
    erased <- make_valid_safe(erased)
    
    # Check if erase completely eliminated polygon
    if (is.null(erased) || length(erased) == 0 || st_is_empty(erased)) {
      
      # 5. Drop overlap polygon
      pa_processing <- pa_processing[-idx2, ]
      next
    }
    
    # 6. Split into polygons
    # Make sure we only keep polygons and drop lines/points
    parts <- st_collection_extract(erased, "POLYGON")
    parts_area <- as.numeric(st_area(parts))
    pct <- parts_area / orig_area
    
    small_idx <- which(pct < 0.10)
    large_idx <- which(pct >= 0.10)
    
    # 7. Merge small pieces into sec_id geometry
    if (length(small_idx) > 0) {
      
      small_geom <- st_union(parts[small_idx])
      small_geom <- make_valid_safe(small_geom)
      new_geom1 <- st_union(geom1, small_geom)
      new_geom1 <- st_make_valid(new_geom1)
        
      if (length(new_geom1) != 1){
      new_geom1 <- st_cast(new_geom1, "MULTIPOLYGON") 
      }
        
        # Update dataset
        st_geometry(pa_processing)[idx1] <- new_geom1
      }

    # Remaining pieces logic
    if (length(large_idx) == 0) {
      
      # Everything merged into sec_id
      pa_processing <- pa_processing[-idx2, ]
      
    } else if (length(large_idx) == 1) {
      
      # 8. Single remaining polygon
      st_geometry(pa_processing)[idx2] <- parts[large_idx]
      
    } else {
      
      # 9. Multiple large polygons remain → turn into a multipolygon and update geometry
      new_geom2 <- st_union(parts[large_idx])
      new_geom2 <- make_valid_safe(new_geom2)
      
      if (length(new_geom2) != 1){
        new_geom2 <- st_cast(new_geom2, "MULTIPOLYGON") 
      }
      
      # Update dataset
      st_geometry(pa_processing)[idx2] <- new_geom2
      
    }
  }
  
  # Now cast back to single polygons and do basic checks for errors
  pa_processing <- st_make_valid(pa_processing)
  pa_processing <- st_collection_extract(pa_processing, "POLYGON")
  pa_processing <- st_cast(pa_processing, "POLYGON")
  
  # Drop little polygons that may be left over after erase
  pa_processing <- pa_processing %>% 
    dplyr::mutate(area_m = as.numeric(st_area(st_geometry(pa_processing)))) %>% 
    filter(area_m >= 10000)
 
  # Rejoin processed pas to other unprocessed pas
  pa <- rbind(pa, pa_processing)
  
  # Do a second run of the processing function (cleans up weird duplicates
  # showing up after the erase loop). Drop the area_m attribute otherwise it duplicates
  pa <- pa %>% select(-area_m)
  pa <- process_protected_areas(pa, pa_id) # This takes long
  
  # Reset section ids to be unique
  pa <- pa %>% select(-sec_id)
  pa <- rowid_to_column(pa, "sec_id")
  
  # This process cleans up MOST overlaps. Remaining errors are at the edges
  # of adjacent PAs where geometries are beyond the snapping tolerance
  # We just use simple erases to eliminate these overlaps, as most of the heavy
  # processing to preserve PA shapes and names has been done by now
  
  pa_clean <- pa
  intersects_list <- st_intersects(pa)
  
  for (i in seq_len(nrow(pa_clean))) {
    # Only look at previously processed overlaps
    overlapping_ids <- setdiff(intersects_list[[i]], i)
    overlapping_ids <- overlapping_ids[overlapping_ids < i]  
    
    if(length(overlapping_ids) > 0) {
      overlap_geom <- st_union(st_geometry(pa_clean)[overlapping_ids])
      new_geom <- st_difference(st_geometry(pa_clean)[i], overlap_geom)
      
      # Ensure new_geom is an sfc object
      new_geom <- st_sfc(new_geom, crs = st_crs(pa_clean))
      
      # Handle completely overlapped polygon
      if (length(new_geom) == 0 || st_is_empty(new_geom)) {
        st_geometry(pa_clean)[i] <- st_geometrycollection()
      } else {
        st_geometry(pa_clean)[i] <- new_geom
      }
    }
  }
  
  # Drop empty geometries
  pa_clean <- pa_clean[!st_is_empty(pa_clean), ]
  
  # Fix the geometry again
  pa_clean<- st_make_valid(pa_clean)
  pa_clean <- st_collection_extract(pa_clean, "POLYGON")
  pa_clean <- st_cast(pa_clean, "POLYGON")
  
  # Drop really small bits
  pa_clean <- pa_clean %>% 
    dplyr::mutate(area_m = as.numeric(st_area(st_geometry(pa_clean)))) %>% 
    filter(area_m >= 10000)
 
  pa_clean <- pa_clean %>% select(-sec_id)
  pa_clean <- rowid_to_column(pa_clean, "sec_id")
  
  
  # Write to gpkg to check that everything worked ok
  st_write(pa_clean, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "pa_clean", append = FALSE)
  
  st_write(whs, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "whs", append = FALSE)
  
  # Suggested cleanups to run in GIS:
  #  - fix geometries
  #  - multipart to singlepart, delete very small bits
  #  - snap geometries
  
  # Use the model clean-up-polygons to run all these in one go
  
  # Read the data back in if you did any manual GIS cleanups
  pa_clean <- st_read(file.path(map_output_path, "paca_processing.gpkg"), 
                      layer = "pa_clean")
  
  whs <- st_read(file.path(map_output_path, "paca_processing.gpkg"), 
                 layer = "whs")
  
# ---- 8. Insert WHS ----------------------------------
  
  # The principle is to map WHS only where they do not overlap with PA
  # Therefore only after cleaning up and flattening the PA layer do
  # we insert the WHS through an erase
  
  pa_union <- clean_dissolve(pa_clean) # This takes super long
  pa_union <- st_combine(pa_union)
  pa_union <- st_union(pa_union)
  pa_union <- st_sf(geometry = pa_union)
  
  whs <- whs %>% mutate(area_m = as.numeric(st_area(st_geometry(whs))))
  whs_erased <- st_difference(whs, pa_union)
  
  # Extract polygons
  whs_erased <- st_collection_extract(whs_erased, "POLYGON")
  whs_erased <- st_cast(whs_erased, "POLYGON")
  
  # Drop little bits
  whs_erased <- whs_erased %>% mutate(remaining_area = as.numeric(st_area(st_geometry(whs_erased))),
                                      remaining_perc = (remaining_area/area_m)*100)
  whs_erased <- whs_erased %>% filter(remaining_perc > 0.1)
  whs_erased <- whs_erased %>% filter(remaining_area > 10000)
  
  # Make sure that both layers have the same attributes
  pa_clean <- pa_clean %>%
                  select(pa_id, cluster_id, WDPAID, CUR_NME, SITE_TYPE, MANAGEMENT)
  
  whs_erased <- whs_erased %>% 
                    select(pa_id, cluster_id, WDPAID, CUR_NME, SITE_TYPE, MANAGEMENT)
  
  # Make sure both layers' geometry columns have the same name and type
  pa_clean <- pa_clean %>% sf::st_set_geometry("geometry")
  pa_clean <- st_cast(pa_clean, "POLYGON")
  whs_erased <- whs_erased %>% sf::st_set_geometry("geometry")
  whs_erased <- st_cast(whs_erased, "POLYGON")
  
  pa <- dplyr::bind_rows(pa_clean, whs_erased)
  pa <- rowid_to_column(pa, "sec_id")
  
  
# ---- 9. Create output ----------------------------------  
  
  gpkg_name <- paste0("pa-time-series-", map_type, "-paca-", paca_version, ".gpkg" )
  layer_name <- paste0("pas-", map_year)
  
  st_write(pa, file.path(map_output_path, gpkg_name), 
           layer = layer_name, append = FALSE)
  
  
  