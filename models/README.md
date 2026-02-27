# QGIS models for spatial data processing

The models in this folder can be used to speed up some of the spatial data processing of this workflow, but it requires that spatial data is exported to a geopackage, processed inside QGIS, the results saved, and then reimported back into R for further processing.

## Connecting to model files from QGIS

1.  In QGIS's Processing Toolbox, click on the icon for models, and then select Add model to Toolbox.

2.  Navigate to the local folder of this project and click open.

3.  The model will then become available in the Processing Toolbox under the grouping Protection level.

4.  Models are run in a similar way to other geoprocessing tools in the QGIS Processing Toolbox.

## Workflow for processing spatial data outside R

1.  Write an sf object as a layer to geopackage. In this workflow, intermediary sf objects are all written to a geopackage called `paca_processing.gpkg`. It is useful for keeping the intermediary outputs together, and prevents them from getting mixed up with final outputs. Generic code for writing an sf object called x to `paca_processing.gpkg`:

```         
st_write(x, file.path(map_output_path, "paca_processing.gpkg"), 
           layer = "layername", append = FALSE)
```

`append = FALSE` ensures that you can overwrite an existing layer with a new version.

2.  Open the output layer in QGIS, and run the model on that layer.
3.  Save the model output as a new layer to `paca_processing.gpkg` (or overwrite the input layer).
4.  Read the output back into R for further processing. Generic code for reading a layer called "output" back into R:

```         
new_sf_object <- st_read(file.path(map_output_path, "paca_processing.gpkg"), 
                      layer = "output")
```

## Models in this folder

### clean-up-polygons

A multifunctional process for fixing problematic vector data:

-   topology fixes

-   converting multiparts to single parts

-   dropping very small processing artefacts (anything smaller than 1 ha)

-   snapping misaligned vertices causing gaps/overlaps

QGIS has a more sophisticated snapping algorithm than sf, and this model can be useful when R-based topology fixing or snapping processes causes errors. It is highly recommended that this model is run during the final processing of output protected area data in step 7 of `processing-paca.R.`

**identify-pa-clusters**

Step 5 in `processing-paca.R` identifies clusters of adjacent protected areas for assigning cluster IDs. There is code in step 5 that can achieve this in R, but it can take up to 30 minutes to run. In contrast, this model achieves the same outcome in about 5 seconds. If you are in a hurry, it is highly recommended that step 5 is replaced with this model. Code for writing and re-reading the spatial data is already set up in section 1.5.
