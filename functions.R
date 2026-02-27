# Functions for frequently used spatial processing steps in building a protected areas snapshot
library(sf)
library(dplyr)
library(lwgeom)


# ----1. Function to clean up small gaps between adjacent polygons -------------
# This is very important to get right if you want to ensure continuous PAs without tiny gaps when you union/dissolve
# multiple polygons, or when you want to avoid tiny leftover slivers if you are using multiple polygons to erase.

fix_polygon_gaps <- function(x, snap_tolerance) {
  
  if (!inherits(x, "sf")) stop("Input must be an sf object")
  if (!all(st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Geometries must be POLYGON or MULTIPOLYGON")
  }
  
  x <- st_make_valid(x)
  x$..orig_id <- seq_len(nrow(x))
  
  # 1 Snap geometries to shared boundary fabric
  snapped <- st_snap(x, st_union(st_geometry(x)), tolerance = snap_tolerance)
  
  # 2 Build boundary network
  edges <- st_boundary(snapped) |>
    st_union() |>
    st_node()
  
  # 3 Polygonize into smallest planar pieces
  pieces <- st_polygonize(edges) |>
    st_collection_extract("POLYGON") |>
    st_as_sf()
  
  pieces$piece_id <- seq_len(nrow(pieces))
  
  # 4 Intersect pieces with originals (preserves overlaps)
  intersections <- st_intersection(
    pieces,
    snapped |> select(..orig_id)
  )
  
  # Keep only geometry + ID for safe summarising
  intersections <- intersections[, "..orig_id"]
  
  # 5 Reassemble cleaned geometry per original feature
  result_geom <- intersections |>
    group_by(..orig_id) |>
    summarise(do_union = TRUE)
  
  # 6 Attach original attributes back
  result <- result_geom |>
    left_join(st_drop_geometry(x), by = "..orig_id") |>
    select(-..orig_id)
  
  return(result)
}

# ----2. Function for clean erasing -------------
# Using any kind of erase where polygon edges do not line up cause horrible slivers
# This function uses a flexible tolerance level to decide whether or not to keep bits or
# any of what is leftover after the erase

erase_with_tolerance <- function(input_geom, erase_geom, tolerance = 1) {
  
  # ---- Safety checks ----
  if (!inherits(input_geom, "sfc") && !inherits(input_geom, "sf")) {
    stop("input_geom must be an sf or sfc geometry")
  }
  if (!inherits(erase_geom, "sfc") && !inherits(erase_geom, "sf")) {
    stop("erase_geom must be an sf or sfc geometry")
  }
  if (!is.numeric(tolerance) || tolerance < 0 || tolerance > 100) {
    stop("tolerance must be a number between 0 and 100")
  }
  
  input_geom <- st_geometry(input_geom)
  erase_geom <- st_geometry(erase_geom)
  
  # ---- 1. Make geometries valid ----
  input_geom <- st_make_valid(input_geom)
  erase_geom <- st_make_valid(erase_geom)
  
  # ---- 2. Original area ----
  original_area <- as.numeric(st_area(input_geom))
  
  if (length(original_area) == 0 || original_area == 0) {
    empty_geom <- st_sfc(st_multipolygon(), crs = st_crs(input_geom))
    return(empty_geom)
    
  }
  
  # ---- 3. Erase ----
  result_geom <- tryCatch(
    st_difference(input_geom, erase_geom),
    error = function(e) st_geometrycollection(crs = st_crs(input_geom))
  )
  
  if (length(result_geom) == 0 || all(st_is_empty(result_geom))) {
    empty_geom <- st_sfc(st_multipolygon(), crs = st_crs(input_geom))
    return(empty_geom)
    
  }
  
  # Extract polygons only (drop lines/points if produced)
  result_polys <- st_collection_extract(result_geom, "POLYGON")
  
  if (length(result_polys) == 0) {
    empty_geom <- st_sfc(st_multipolygon(), crs = st_crs(input_geom))
    return(empty_geom)
    
  }
  
  # ---- 4. Total remaining area check ----
  total_remaining_area <- sum(as.numeric(st_area(result_polys)))
  total_remaining_pct  <- (total_remaining_area / original_area) * 100
  
  if (total_remaining_pct < tolerance) {
    empty_geom <- st_sfc(st_multipolygon(), crs = st_crs(input_geom))
    return(empty_geom)
    
  }
  
  # ---- 5. Remove small fragments ----
  piece_areas <- as.numeric(st_area(result_polys))
  piece_pct   <- (piece_areas / original_area) * 100
  
  keep_idx <- piece_pct >= tolerance
  
  if (!any(keep_idx)) {
    empty_geom <- st_sfc(st_multipolygon(), crs = st_crs(input_geom))
    return(empty_geom)
    
  }
  
  kept_polys <- result_polys[keep_idx]
  
  # ---- 6. Return as MULTIPOLYGON ----
  combined <- st_union(kept_polys)
  combined <- st_cast(combined, "MULTIPOLYGON")
  
  combined
}

# ---- 3. Function for clean dissolving ------
# PA geometries often do not dissolve cleanly because geometries are not snapped, there are small gaps, etc.
# This function unions polygons and then fills small gaps by looking at whether a gap_threshold buffer makes the gap
# disappear or not. If it does, the hole is dropped, if not it stays as is.

clean_dissolve <- function(input_geom, gap_threshold = 100) {
  
  if (!inherits(input_geom, "sfc") && !inherits(input_geom, "sf")) {
    stop("input_geom must be an sf or sfc geometry")
  }
  
  # ---- Prepare geometry ----
  geom <- st_geometry(input_geom)
  geom <- st_make_valid(geom)
  snap_geom <- st_snap(geom, geom, tolerance = 10)
  snap_geom <- st_make_valid(snap_geom)
  dissolved <- st_union(snap_geom)
  dissolved <- st_cast(dissolved, "MULTIPOLYGON")
  
  # Step 2: Break into polygons to examine holes
  polygons_list <- st_cast(dissolved, "POLYGON")
  
  processed_polygons <- lapply(polygons_list, function(poly) {
    
    rings <- st_geometry(poly)[[1]]  # first = exterior, rest = holes
    exterior <- rings[[1]]
    
    if (length(rings) > 1) {
      interiors <- rings[-1]
      
      # Keep only holes that are wider than threshold
      keep_holes <- lapply(interiors, function(hole) {
        hole_poly <- st_polygon(list(hole))
        # Use negative buffer to check if hole is narrower than threshold
        if (st_is_empty(st_buffer(hole_poly, -distance_threshold))) {
          return(NULL)  # hole too thin, remove
        } else {
          return(hole)
        }
      })
      
      keep_holes <- keep_holes[!sapply(keep_holes, is.null)]
      new_poly <- st_polygon(c(list(exterior), keep_holes))
      
    } else {
      new_poly <- poly
    }
    
    return(new_poly)
  })
  
  # Step 3: Combine polygons back into a MULTIPOLYGON safely
  result <- st_sfc(processed_polygons, crs = st_crs(input_geom))
  result <- st_cast(result, "MULTIPOLYGON")
  
  return(result)
}

# ---- 4. Make safe geometry ------

# Erasing and other spatial processing can sometimes cause weird things like empty (but not null)
# geometries. This function converts empty geometries to true nulls (so that they can be detected through
# logical arguments) and also makes sure the geometry is valid

make_valid_safe <- function(g) {
  if (is.null(g)) return(NULL)
  g <- try(st_make_valid(g), silent = TRUE)
  if (inherits(g, "try-error")) return(NULL)
  g
}

# ---- 5. Process protected areas -----

# This code is designed to cleanly union different sections of a protected area into polygons
# where a PA has multiple sections, it creates multiple polygons

process_protected_areas <- function(x, id_col){
          
  # Check that input is an sf object
  if (!inherits(x, "sf")) stop("Input must be an sf object")
  if (!all(st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Geometries must be POLYGON or MULTIPOLYGON")
  }
  
  input_crs <- st_crs(x)
  # Create an empty spatial object to collect processing results
  pa <- NULL
  
  # Make a list of ID values to loop through
  id_list <- x %>% dplyr::pull({{ id_col }}) %>% unique()
  
  # Set aside the attributes for rejoining after processing
  attributes <- x %>% 
    st_drop_geometry() %>% 
    dplyr::distinct({{ id_col }}, .keep_all = TRUE)
  
  # Loop
  for (i in id_list){
    
    input_shape <- x %>% filter({{ id_col }} == i)
    clean_geom <- clean_dissolve(input_shape)
    
    # Convert back to an sf object (not just geometry)
    new_geom <- st_geometry(clean_geom)
    
    new_row <- st_sf(
      geometry = new_geom
    ) |>
      dplyr::mutate({{ id_col }} := i)
    
    st_crs(new_row) <- input_crs   # <- enforce CRS

    # Drop tiny bits (<1 ha)
    new_row <- st_cast(new_row, "POLYGON")
    new_row <- new_row %>% 
      dplyr::mutate(area_m = as.numeric(st_area(geometry))) %>% 
      filter(area_m >= 10000)
    
    
    # Join to output
    pa <- dplyr::bind_rows(pa, new_row)
  }
  
  # Join the attributes back in
  pa <- left_join(pa, attributes, by = rlang::as_name(rlang::ensym(id_col)))
  
  st_crs(pa) <- input_crs   # <- enforce CRS on output
  
  return(pa)
  
}
