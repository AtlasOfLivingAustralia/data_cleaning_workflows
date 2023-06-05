## Creating shapefiles from RDS files

## Load libraries
x <- c("rgdal", "gdalUtils", "rgeos", "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)

## Species with polygons saved a list of Spatial Polygons Data Frames
species_polys <- readRDS(file.path(output_dir, "Species_alpha_hull_polygons.rds"))

## Associated species names
all_species <- names(species_polys)

## Specify directory for writing the shapefiles
shapefile_dir <- getwd()

## To write shapefiles in a for loop
for (species_name in all_species){
  writeOGR(species_polys[[species_name]], 
           dsn = shapefile_dir, 
           layer = species_name, 
           driver = "ESRI Shapefile", 
           overwrite_layer = TRUE)
}

## To write shapefiles in parallel
registerDoMC(future::availableCores())
system.time(foreach::foreach(species_name = all_species,
                             .combine = rbind,
                             .errorhandling = "pass",
                             .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                               
                               writeOGR(species_polys[[species_name]], 
                                        dsn = shapefile_dir, 
                                        layer = species_name, 
                                        driver = "ESRI Shapefile", 
                                        overwrite_layer = TRUE)
                             })
