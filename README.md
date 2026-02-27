# Preparing protected areas for protection level assessments

The Department of Forestry, Fisheries and the Environment maintains South Africa's [official register of protected and conserved areas](https://www.dffe.gov.za/index.php/egis). The data is contained in the PACA (Protected and Conserved Areas) geodatabase, which stores the spatial boundaries and attributes of legally declared protected areas (national parks, nature reserves, marine protected areas, etc.) as well as other conservation areas that do not have formal legal protection but are managed for biodiversity conservation, such as RAMSAR sites or Biosphere Reserves.

This database is the source of protected area network time series data used in protection level indicators in the National Biodiversity Assessment. However, constructing such a time series takes a considerable amount of processing of PACA data, which is made reproducible through the R code in this repository. The aim of the script `processing-paca.R` is to create a flat (non-overlapping) polygon layer representing the extent, correct name, and management authorities of protected areas present in the national protected area network at a particular point in time.

## Setup

The script creates a protected area network snapshot for a particular year. To create a time series, re-run the script for each time point. The construction of the time series is controlled by variables in the setup section of the script. For each run, it is important to review and adjust these variables to ensure correct outputs.

Setup requires the following inputs:

1.  **A local copy of the PACA database in .gdb format**. Specify the path to the PACA database in the variable `paca_path`. Also specify the PACA version used - this can just be the month-year of when the copy was made - in the variable `paca_version`. This ensures that time series constructed from different versions of PACA are stored in separate output geopackage files. The script assumes that PACA has the following attributes:

| Attribute | Definition and reason for need in processing |
|------------------------------------|------------------------------------|
| WDPAID | Unique identification number assigned by UNEP–WCMA for use in the World Database of Protected Areas. It can be used to distinguish different protected areas with the same name. |
| CUR_NME | The official or current legal name of the PA or CA. |
| MAJ_TYPE | Differentiates protected and conserved areas. |
| SITE_TYPE | Major category of protected area, used to differentiate terrestrial and marine protected areas. |
| SITE_STYPE | Protected area category subtype - used to differentiate World Heritage Site core areas (included) from buffer areas (excluded). |
| D_DCLARP | Parcel declaration date - used to identify parcels proclaimed at the time of the snapshot. |
| LEGAL_STAT | Current legal status of the area - only "declared" areas are included in the map. |
| D_UNDCLAP | Date of parcel deproclamation - parcels deproclaimed at the time of the snapshot are removed from the map. |
| MANAGEMENT | Entity responsible for the conservation management of the area. This data is used primarily to facilitate data collection on protected area effectiveness. |
| PROVINCE | Provincial location of the parcel. This field is used mainly to place offshore islands with marine protected areas data, rather than with terrestrial. |

2.  **Indicate the year for which the snapshot should be constructed**. To create different time points from the same version of PACA, change this variable (`map_year`) and run the script again. All the snapshots will be stored in the same output geopackage file.
3.  **Indicate whether you want to create a terrestrial or marine PA time series**. Terrestrial and marine PAs are processed separately. Add either "terrestrial" or "marine" to the variable `map_type`.
4.  **Specify the path for script outputs**. Using the variable `map_output_path`. The default (included in this setup is "outputs".
5.  **Specify the desired projection for the spatial data**. PACA is in geographic (with Hartbeesthoek94 datum). For spatial processing it is best to reproject to an equal area projection. The crs specified in `pa_crs` is the standardised Albers Equal Area projection used for NBA maps.
6.  **Specify a maximum distance (in meters) for protected areas to be considered part of the same cluster.** Clustering is used to confirm species' presence across large clusters of adjacent protected areas where there may be continuous habitat. The default value is 1000m.

## functions.R

A number of custom spatial processing functions used repeatedly in the script are stored in `functions.R`. For these functions to work, make sure `functions.R` is in the same folder as `processing-paca.R` and run `source("functions.R")`.

## QGIS models

The folder `models` contains two QGIS models which can be used for some parts of the data processing. Some spatial functions that are applied to the entire protected area network can be very slow in R. Defining protected area clusters (section 5 in the code) can take extremely long. An alternative approach is to write the spatial data to gpkg format and do the processing in QGIS using the model `identify-pa-clusters` (which takes only a few seconds) and then read the QGIS output back into R to continue processing. Relevant code for writing and reading spatial data is provided at relevant points in the code.

Partially processed data are written to a geopackage (`paca_processing.gpkg`) stored in the output folder. These outputs can be useful for inspecting the outputs of intermediate steps for troubleshooting processing errors.

> [!CAUTION] 
> Do not run all of `processing-paca.R` in one go - there are sections of the script that can take a long time (noted in comments) and some parts (sections 5 and 7) have optional processing steps outside of R. It is recommended that each of the numbered sections are run individually and the outputs in `paca_processing.gpkg` inspected for processing errors.
