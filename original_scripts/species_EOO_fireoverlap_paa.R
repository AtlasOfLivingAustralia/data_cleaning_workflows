## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin

## Polygon overlap for species with 3+ records


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
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

shapefile_dir = file.path(output_dir, "species_shapefiles")
overlap_dir = file.path(output_dir, "polygon_overlap")
# ## Remove exisitng overlap and shapefiles folder
# unlink(shapefile_dir, recursive = TRUE, force = TRUE)
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

## Load spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)
message(cat("Number of species with polygons: "),
        length(polygon_list))

# ## To only run overlaps for species with > 0 points in PAA
# paa_species <- fread(file.path(output_dir, "PAA_in_species.csv"))$x
# polygon_list <- polygon_list[polygon_list %in% paa_species]
# message(cat("Number of species with polygons and > 0 points in PAA: "),
#         length(polygon_list))

## Run overlap analysis in parallel: doMC ####
job_script <- file.path("/tempdata/workdir/nesp_bugs/", 
                        "scripts", "species_EOO_fireoverlap_paa_job.R")
rstudioapi::jobRunScript(job_script, encoding = "unknown", 
                         workingDir = "/tempdata/workdir/nesp_bugs",
                         importEnv = FALSE, exportEnv = "")


## Error reruns for individual species runs if required ####
## Check files
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))
length(polygon_list) - length(csvfiles)

## Find missing species from outputs
csvnames <- basename(tools::file_path_sans_ext(csvfiles))
error_list <- polygon_list[!polygon_list %in% csvnames]
message(cat("Number of species in error list: "),
        length(error_list))

## Run polygon_paa_overlap.R stepwise
polys = error_list[2]
species_name = polys
species_poly = species_maps[[polys]]
outdir = overlap_dir

## Check output is created
x <- "..."
fread(grep(x, csvfiles, value = TRUE))


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
names(out)[1] <- "spfile"
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap.csv"), row.names = FALSE)

## Checks
## >> Total overlapped area should be <= Species Polygon
message(cat("Check if area overlapping with fire is always <= Total polygon area for species: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3, Fire_Class_4, Fire_Class_5)]) <= out$Species_Polygon))

## >> Look for NAs in table - shoudln't be any
message(cat("Check for NAs: "),
        sum(is.na(out)))
# which(is.na(out), arr.ind=TRUE)
# error_list <- csvnames[!csvnames %in% out$Species]
# error_files <- csvfiles[!csvnames %in% out$Species]

## >> Look for Species_Polygon = 0 in table
message(cat("Number of species with Species_Polygon area = 0: "),
        sum(out$Species_Polygon == 0))
naidx <- which(out$Species_Polygon == 0)
out[naidx]

## >> Check individual species
polys = "tropidotasia_femoralis_13942"
species_name = polys
species_poly = species_maps[[polys]]
outdir = overlap_dir

## >> Create shapefiles and import in QGIS on top of aus_mask: auslands_aea137
writeOGR(species_poly, dsn = shapefile_dir, layer = species_name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
## We see that these species lie on islands that fall off the specified extent of the species rasetr (tif)
## Extent is specified as per the the fire map (see notes in polygon_paa_overlap.R)
## Therefore, the species raster created for these species is all 0 and hence polygon calculatyed for these species = 0
## If we use the alternate function to create species raster (see notes at the end of polygon_overlap.R),
## we get raster values with 0 and 1, but these cannot be overlaid with the fire map oif lesser extent anyway
## So we will have to discard these species; species list was checd by JM

## >> Get list of species names with Species_Polygon == 0
x <- out[naidx]$spfile
write.csv(x, file.path(output_dir, "species_offextent.csv"), row.names = FALSE)

# ## Remove species with Species_Polygon == 0
# message(cat("Number of species off PAA extent (removed from outputs): "),
#         length(x))
# out[(spfile %in% x)]
# dim(out); out <- out[!(spfile %in% x)]; dim(out)


## Add percentage overlap columns
out$Overlap_Polygons_Fire345_GEEBAM2_as_unburnt <- 
  ((out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Species_Polygon - out$Fire_Class_1)) * 100

out$Overlap_Polygons_Fire2345_GEEBAM2_as_burnt <- 
  ((out$Fire_Class_2 + out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Species_Polygon - out$Fire_Class_1)) * 100

out$Overlap_Polygons_Severe_Fire45 <- 
  ((out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Species_Polygon - out$Fire_Class_1)) * 100


sum(is.na(out$Overlap_Polygons_Fire345_GEEBAM2_as_unburnt))
sum(is.na(out$Overlap_Polygons_Fire2345_GEEBAM2_as_burnt))
sum(is.na(out$Overlap_Polygons_Severe_Fire45))

# ## If there are NAs, check if NAs are same in all three columns
# all(which(is.na(out$Overlap_Polygons_Fire345_GEEBAM2_as_unburnt)) == 
#       which(is.na(out$Overlap_Polygons_Fire2345_GEEBAM2_as_burnt)))
# all(which(is.na(out$Overlap_Polygons_Fire2345_GEEBAM2_as_burnt)) == 
#       which(is.na(out$Overlap_Polygons_Severe_Fire45)))



## Final formatting ####
names(out)[grep("Fire_Class_", names(out))] <- paste0(names(out)[grep("Fire_Class_", names(out))], "_Area")
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap.csv"), 
          row.names = FALSE)



## Summarize outputs ####
message(cat("NA in scientificName: "),
        length(which(is.na(out$scientificName))))

message(cat("Total number of species: "),
        nrow(out))