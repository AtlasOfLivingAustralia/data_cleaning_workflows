## Region overlap analysis: foreach{}
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
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")

source("/tempdata/workdir/nesp_bugs/scripts/region_overlap.R")

## Load species data rds files
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
length(spfiles)




## I. Bushfire recovery regions overlap ####
## ---------------------------------------------

## >> Load bushfire recovery regions layer
## Source: DAWE, Fiona Woods
region <- raster(file.path(output_dir, "regions","bushfire_recovery_p.tif"))
region_vals <- region[]
region_classes <- sort(unique(na.omit(region_vals)))
region_classes

## >> Specify overlap folder
overlap_dir = file.path(output_dir, "regions_overlap")
# ## Remove existing overlap folder
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             region_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            region_raster = region,
                                            region_classes = region_classes,
                                            outdir = overlap_dir)
                           })



## Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "bushfire_recregions_names.csv"))
names(out) <- c("spfile", "NA", gsub(" ", "_", tolower(reg_names$name)))

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_by_bushfireregions.csv"), row.names = FALSE)

# ## Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)


## II. States overlap ####
## ---------------------------------------------

## >> Load state boundaries layer
## Source: https://data.gov.au/data/dataset/geodata-coast-100k-2004
## See data processinf decision tree doc for steps involved...
region <- raster(file.path(output_dir, "regions", "auslands_wgs84_p.tif"))
region_vals <- region[]
region_classes <- sort(unique(na.omit(region_vals)))
region_classes

## Specify overlap folder ####
overlap_dir = file.path(output_dir, "states_overlap")
# ## Remove existing overlap folder
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             region_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            region_raster = region,
                                            region_classes = region_classes,
                                            outdir = overlap_dir)
                           })



## Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "state_names.csv"))
reg_names$name[1] <- "NA"
names(out) <- c("spfile", reg_names$name)

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_by_states.csv"), row.names = FALSE)




## III. States overlap - Clipped by PAA ####
## ---------------------------------------------

## >> Load state boundaries layer
## Source: https://data.gov.au/data/dataset/geodata-coast-100k-2004
## See data processinf decision tree doc for steps involved...
region <- raster(file.path(output_dir, "regions", "auslands_wgs84_p_paa.tif"))
region_vals <- region[]
region_classes <- sort(unique(na.omit(region_vals)))
region_classes

## Specify overlap folder ####
overlap_dir = file.path(output_dir, "states_paa_overlap")
# ## Remove existing overlap folder
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             region_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            region_raster = region,
                                            region_classes = region_classes,
                                            outdir = overlap_dir)
                           })



## Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "state_paa_names.csv"))
reg_names$name[1] <- "NA"
names(out) <- c("spfile", reg_names$name)

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_by_states_paa.csv"), row.names = FALSE)

# ## Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)

## List of species in/out of PAA
out$PAA_Points <- rowSums(out[, 3:ncol(out)])
message(cat("Number of species with 0 records inside PAA: "),
        nrow(out[PAA_Points == 0]))
message(cat("Number of species with at least 1 record inside PAA: "),
        nrow(out[PAA_Points != 0]))

write.csv(out[PAA_Points == 0]$spfile, 
          file = file.path(output_dir, "PAA_out_species.csv"),
          row.names = FALSE)

write.csv(out[PAA_Points > 0]$spfile, 
          file = file.path(output_dir, "PAA_in_species.csv"),
          row.names = FALSE)

