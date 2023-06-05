## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos", "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_data = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
shapefile_dir = file.path(output_dir, "species_shapefiles")
overlap_dir = file.path(output_dir, "polygon_overlap")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/polygon_paa_overlap.R")


## Fire overlap for species WITH EOO polygons  ####
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps) 

# ## To only run overlaps for species with > 0 points in PAA
# paa_species <- fread(file.path(output_dir, "PAA_in_species.csv"))$x
# polygon_list <- polygon_list[polygon_list %in% paa_species]

## >> Find missing species from outputs (if any) ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
species_list <- polygon_list[!polygon_list %in% csvnames]

## >> Load in fire severity raster and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-1)
system.time(foreach(polys = species_list,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{

                      polygon_paa_overlap(species_name = polys, # polys = species_list[335]
                                      species_poly = species_maps[[polys]],
                                      shapefile_dir = shapefile_dir,
                                      fire_res = fire_res,
                                      fire_crs = fire_crs,
                                      fire_extent = fire_extent,
                                      fire_vals = fire_vals,
                                      fire_classes = fire_classes,
                                      outdir = overlap_dir)
                    })



# i <- length(polygon_list) - length(csvnames)
# 
# while (i > 10) {
# 
#   ## Number of species
#   print(paste0(">>>> Species runs left: ", i, " <<<<< "))
# 
#   ## Run overlaps
#   foreach(polys = species_list,
#           .combine = rbind,
#           .errorhandling = "pass",
#           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
# 
#             polygon_paa_overlap(species_name = polys,
#                                 species_poly = species_maps[[polys]],
#                                 shapefile_dir = shapefile_dir,
#                                 fire_res = fire_res,
#                                 fire_crs = fire_crs,
#                                 fire_extent = fire_extent,
#                                 fire_vals = fire_vals,
#                                 fire_classes = fire_classes,
#                                 outdir = overlap_dir)
#           }
# 
#   ## Output files created
#   csvfiles <- list.files(overlap_dir, pattern = ".csv$",
#                          full.names = TRUE, all.files = TRUE)
#   csvnames <- tools::file_path_sans_ext(basename(csvfiles))
#   species_list <- polygon_list[!polygon_list %in% csvnames]
# 
#   ## Species without output
#   i <- length(polygon_list) - length(csvnames)
# }

## Errors seem to be an artefact of the system rather than problem with data/code
