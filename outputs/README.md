# Outputs

This folder is set up to receive the outputs of `processing-paca.R`. For each protected area time series, a geopackage with the following naming pattern will be created:

"pa-time-series" + `map_type` + "paca" + `paca_version`

`map_type` is specified as "terrestrial" or "marine" in `processing-paca.R` setup.

`paca_version` is also defined in `processing-paca.R` setup.

Each time point will be stored as a separate layer inside the output geopackage, with each layer being named "pas-" + `map_year`.

Intermediary processing steps of `processing-paca.R` are stored in a geopackage named `paca_processing.gpkg` which is also stored here.
