## Aquatic overlap for polygons - job

## Set working environment ####
rm(list = ls())
gc()

x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos", "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)

bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
aqua_dir = file.path(bugs_dir, "aquatics")
output_dir = file.path(bugs_dir, "outputs")


## Subset data ####
aqualist <- fread(file.path(aqua_dir, "Aquatic_species.csv"))
aqualist <- aqualist[spfile != ""]
aqualist <- aqualist[-98,]

data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
aqua_data <- data[spfile %in% aqualist$spfile]

## Polygon overlaps
shapefile_dir = file.path(output_dir,  "slugrisk", "species_shapefiles")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}

overlap_dir = file.path(output_dir, "slugrisk", "slug_polygon_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/polygon_slugfire_overlap.R")

## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)
polygon_list <- polygon_list[polygon_list %in% aqualist$spfile]

csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
polygon_list <- polygon_list[!polygon_list %in% csvnames]

## >> Load in fire severity raster
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)

## >> Slugrisk raster
slug_severity <- raster(file.path(output_dir, "slugrisk", "slugrisk_NA.tif"))
slug_vals <- slug_severity[]
slug_classes <- sort(unique(na.omit(slug_vals)))

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-1)
system.time(log <- foreach(polys = polygon_list,
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             polygon_slugfire_overlap(species_name = polys,
                                                      species_poly = species_maps[[polys]],
                                                      shapefile_dir = shapefile_dir,
                                                      fire_res = fire_res, 
                                                      fire_crs = fire_crs, 
                                                      fire_extent = fire_extent, 
                                                      fire_vals = fire_vals,
                                                      fire_classes = fire_classes,
                                                      slug_vals = slug_vals,
                                                      slug_classes = slug_classes,
                                                      outdir = overlap_dir)
                           })