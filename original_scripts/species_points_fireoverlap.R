## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin

## Points overlap for all species


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

overlap_dir = file.path(output_dir, "points_overlap")
## Remove existing overlap folder
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")


  # ## Fire overlap only for species WITHOUT EOO polygons  ####
  # ## >> Load species  names without EOOs
  # points_list <- fread(file.path(output_dir, paste0("species_ahullnoEOO.csv")))
  # points_list <- points_list$x
  # length(points_list)
  # 
  # ## >> Find species data rds files
  # spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
  # x <- basename(tools::file_path_sans_ext(spfiles))
  # spfiles <- spfiles[x %in% points_list]

## Fire overlap for ALL species  ####
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
length(spfiles)

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores())
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             points_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            fire_severity = fire_severity,
                                            fire_classes = fire_classes,
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
## >> Merge csv files ####
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
names(out)[1] <- "spfile"
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"), row.names = FALSE)

## >> Checks ####
## >> Total overlapped points should be <= Total Occurrence points
message(cat("Check if #points overlapping with fire is always <= Total # points for species: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3, Fire_Class_4, Fire_Class_5)], na.rm = TRUE) <= out$Occurrence_Points))

## >> Look for NAs in table - shouldn't be any
message(cat("Check for NAs: "),
        sum(is.na(out)))

## >> Look for Occurrence_Points = 0 in table - shouldn't be any
message(cat("Number of species with Occurrence_Points = 0: "),
        sum(out$Occurrence_Points == 0))


## >> Add percentage overlap columns ####
out$Overlap_Points_Fire345_GEEBAM2_as_unburnt <- 
  ((out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Occurrence_Points - out$Fire_Class_1)) * 100

out$Overlap_Points_Fire2345_GEEBAM2_as_burnt <- 
  ((out$Fire_Class_2 + out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Occurrence_Points - out$Fire_Class_1)) * 100

out$Overlap_Points_Severe_Fire45 <- 
  ((out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Occurrence_Points - out$Fire_Class_1)) * 100

sum(is.na(out$Overlap_Points_Fire345_GEEBAM2_as_unburnt))
sum(is.na(out$Overlap_Points_Fire2345_GEEBAM2_as_burnt))
sum(is.na(out$Overlap_Points_Severe_Fire45))

## Check if NAs are same in all three columns
all(which(is.na(out$Overlap_Points_Fire345_GEEBAM2_as_unburnt)) == 
      which(is.na(out$Overlap_Points_Fire2345_GEEBAM2_as_burnt)))
all(which(is.na(out$Overlap_Points_Fire2345_GEEBAM2_as_burnt)) == 
      which(is.na(out$Overlap_Points_Severe_Fire45)))

## Look at rows with NAs: happens because we get a 0 in the denominator
naidx <- which(is.na(out$Overlap_Points_Severe_Fire45))
out[naidx]
  ## 0/0 divisions
out[naidx]$spfile


## >> Add class/family information to output table ####
tax <- fread(file = file.path(output_dir, "data_ALAnonALA_wgs84_corrected_taxinfo.csv"))

## Checks
all(out$spfile %in% tax$spfile)
dim(out); length(unique(out$spfile))
dim(tax); length(unique(tax$spfile)); length(unique(tax$scientificName))

out <- merge(out, tax, by = "spfile")
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"), 
          row.names = FALSE)


# >> Add order information for species in data from AFD list ####
## Extract infor for specxies in outputs from AFD list
afd <- fread(file.path(output_dir, "afd_species_clean.csv"))
setDT(afd, key = "VALID_NAME")
afd <- afd[VALID_NAME %in% unique(out$scientificName)][,.(VALID_NAME, CLASS, ORDER, 
                                                           FAMILY, GENUS, SUB_GENUS, 
                                                           SPECIES, SUB_SPECIES)]
names(afd) <- tolower(names(afd))
afd$class <- tolower(afd$class)
afd$order <- tolower(afd$order)
afd$family <- tolower(afd$family)
names(afd)[1] <- "scientificName"

length(unique(out$scientificName))
nrow(afd)
sum(duplicated(afd))
sum(duplicated(afd$scientificName))

afd[duplicated(afd)]
dim(afd); afd <- afd[!duplicated(afd)]; dim(afd)
afd <- setDT(afd, key = "scientificName")

## Add info to output data table
setDT(out, key = "scientificName")
for (sp in afd$scientificName){
  if(is.na(out[scientificName == sp]$order)){
    out[scientificName == sp]$order = afd[scientificName == sp]$order
  }
}

sum(is.na(out$class))
sum(is.na(out$order))
sum(is.na(out$family))

setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"), 
          row.names = FALSE)

## Save taxonomic info table
rm(tax)
tax <- setDT(out, key = "spfile")[, .SD[1L] ,.(scientificName, class, order, family, spfile)]
tax <- tax[,.(scientificName, class, order, family, spfile)]
dim(tax)
write.csv(tax, file = file.path(output_dir, "data_ALAnonALA_wgs84_corrected_taxinfo.csv"),
          row.names = FALSE)

## >> Add EOO and AOO information from species_EOO_AOO_ahullareas.csv ####
polyareas <- fread(file.path(output_dir, "species_EOO_AOO_ahullareas.csv"))
polyareas <- setDT(polyareas, key = "spfile")[spfile %in% out$spfile][, .(spfile, EOO, AOO, Nbe_unique_occ., scientificName)]

dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(polyareas); length(unique(polyareas$spfile)); length(unique(polyareas$scientificName))

## Check
out2 <- merge(out, polyareas, by = "spfile")
all(out2$scientificName.x == out2$scientificName.y)
which(!(out2$scientificName.x == out2$scientificName.y))
sum(!(out2$scientificName.x == out2$scientificName.y))
out2[which(!(out2$scientificName.x == out2$scientificName.y))][,.(scientificName.x, scientificName.y)]
  ## out2$scientificName.x & out2$scientificName.y are the same
rm(out2)

polyareas[, scientificName := NULL]
out <- merge(out, polyareas, by = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"),
          row.names = FALSE)


## >> Add regional data to output table ####
## Combine state & bushfire recovery regions tables
region1 <- fread(file.path(output_dir, "species_by_states.csv"))
names(region1)[-1] <- paste0("state_", names(region1)[-1])

region2 <- fread(file.path(output_dir, "species_by_states_paa.csv"))
names(region2)[-1] <- paste0("state.paa_", names(region2)[-1])

region3 <- fread(file.path(output_dir, "species_by_bushfireregions.csv"))
names(region3)[-1] <- paste0("fire.rec.reg_", names(region3)[-1])

dim(region1); length(unique(region1$spfile))
dim(region2); length(unique(region2$spfile))
dim(region3); length(unique(region3$spfile))

setDT(region1, key = "spfile")
setDT(region2, key = "spfile")
setDT(region3, key = "spfile")
region <- merge(region1, region2, by = "spfile")
region <- merge(region, region3, by = "spfile")
setDT(region, key = "spfile")
dim(region)

## Merge information with output table
region <- region[spfile %in% out$spfile]
dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(region); length(unique(region$spfile))
out <- merge(out, region, by = "spfile")

setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"),
          row.names = FALSE)


## Final formatting ####
## Column names
names(out)[which(names(out) == "Nbe_unique_occ.")] <- "Nbe_unique_occ"
names(out)[grep("Fire_Class_", names(out))] <- paste0(names(out)[grep("Fire_Class_", names(out))], "_Points")

## Column order
names(out)[c(1, 11, 12, 13, 14, 2:6, 8:10, 7, 16, 15, 17:46)]
out <- out[, c(1, 11, 12, 13, 14, 2:6, 8:10, 7, 16, 15, 17:46)]

## Check
all(out$Occurrence_Points == out$Nbe_unique_occ)
out[, Nbe_unique_occ := NULL]

## Save table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"),
          row.names = FALSE)


## Summarize outputs ####
out <- fread(file.path(output_dir, "species_points_fireoverlap.csv"))
message(cat("NA in scientificName: "),
        length(which(is.na(out$scientificName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("Number of species showing overlap: "))
out[, .N, by = Occurrence_Points]

## Unique class-family summaries in 100% overlap
message(cat("Unique classes shwowing 100% overlap:"))
out[Percent_Overlap == 100][, .SD[1L] ,.(class)][,.(class)][, .N, class]$class

out[Percent_Overlap == 100][, .SD[1L] ,.(family)][,.(family)][, .N, family]
out[Percent_Overlap == 100][, .SD[1L] ,.(class, family)][,.(class,family)]

## High severity overlaps
fire3_overlap <- (out$Fire_Class_4 + out$Fire_Class_5)/out$Occurrence_Points
message(cat("Number of species with high fire severity impact on all (n = 2) recorded data points: "),
        length(which(fire3_overlap == 1)))




