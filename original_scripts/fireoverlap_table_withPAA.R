## Combinign fire overlap results


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "readxl") 
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")


## Combine output tables ####
poly <- fread(file.path(output_dir, "species_polygon_fireoverlap.csv")); dim(poly)
point <- fread(file.path(output_dir, "species_points_fireoverlap.csv")); dim(point)

## Merge rows for species with polygon overlap innfo (n = 29029)
out1 <- merge(point, poly, by = "spfile")
dim(out1)

## Get rows for species without polygon informatiom (n = 29948)
sum(!(point$spfile %in% out1$spfile))
out2 <- point[!(point$spfile %in% out1$spfile)]
dim(out2)

## Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(poly)[-1]))), names(poly)[-1])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)

## Check
dim(out1)[1] + dim(out2)[1] == dim(point)[1]
ncol(out1) == ncol(out2)

## Combine both tables
out <- rbind(out1, out2); dim(out)
rm(out1, out2, point, poly)
setDT(out, key = "spfile")

## Total records within PAA
grep("paa", names(out), value = TRUE)
num <- grep("paa", names(out))[-1]
out$PAA_Points <- rowSums(out[, ..num])
message(cat("Number of species with 0 records inside PAA: "),
        nrow(out[PAA_Points == 0]))
message(cat("Number of species with at least 1 record inside PAA: "),
        nrow(out[PAA_Points != 0]))

## Total number of points overlaping GEEBAM
grep("_Points$", names(out), value = TRUE)[1:5]
num <- grep("_Points$", names(out), value = TRUE)[1:5]
out$Total_fire_points <- rowSums(out[, ..num])
nrow(out[Total_fire_points == 0])

## Total polygon area overlaping GEEBAM
grep("_Area$", names(out), value = TRUE)[1:5]
num <- grep("_Area$", names(out), value = TRUE)[1:5]
out$Total_fire_polygon <- rowSums(out[, ..num])
nrow(out[Total_fire_polygon == 0])


## Checks ####
## >> Check if total points in PAA = 0, the fire overlap points = 0 ####
nrow(out[PAA_Points == 0])
all(out[PAA_Points == 0]$Total_fire_points == 0)
  ## All TRUE


## >> Check if total points in PAA = 0, then polygon fire overlap = 0 ####
nrow(out[PAA_Points == 0 & !is.na(Total_fire_polygon)])
all(out[PAA_Points == 0]$Total_fire_polygon == 0, na.rm = TRUE)

out[PAA_Points == 0 & Total_fire_polygon != 0][,.(spfile, Total_fire_points, Occurrence_Points, Total_fire_polygon, Species_Polygon, EOO, PAA_Points)]


## >> Check if total points in PAA = 0, species polygon (raster) is also = 0 ####
## Species polygon (raster, cipped to PAA) 
## This can be FALSE because species polygon clipped to PAA may or may not overlap with fire within PAA
all(out[PAA_Points == 0 & !is.na(Total_fire_polygon)]$Species_Polygon != 0)
nrow(out[PAA_Points == 0 & Species_Polygon != 0])

out[PAA_Points == 0 & Species_Polygon != 0][,.(spfile, Total_fire_points, Occurrence_Points, Total_fire_polygon, Species_Polygon, EOO, PAA_Points)]


## >> Check if polygon fire overlap > species polygon clipped to PAA ####
## This will always be FALSE
## Area of polygon fire overlap will always be LESS THAN area of species polygon clipped to PAA
out[Total_fire_polygon > Species_Polygon][,.(spfile, Total_fire_points, Occurrence_Points, Total_fire_polygon, Species_Polygon, EOO, PAA_Points)]


## >> Check if Species_Polygon (raster) > EOO (shapefile) ####
## Area of species polygon raster clipped to PAA compared to area of species polygon shapefile from ConR
out[Species_Polygon > EOO][,.(spfile, Total_fire_points, Occurrence_Points, Total_fire_polygon, Species_Polygon, EOO, PAA_Points)]
  ## Because of decimal values, not useful.

plot(density(out$Species_Polygon/out$EOO, na.rm = TRUE))
  ## 0 values because Species_Polygons are zero because:
  ## We see that these species lie on islands that fall off the specified extent of the species rasetr (tif)
  ## Extent is specified as per the the fire map (see notes in polygon_paa_overlap.R)
  ## Therefore, the species raster created for these species is all 0 and hence polygon calculatyed for these species = 0

range(out[Species_Polygon > 0, Species_Polygon/EOO])
plot(density(out[Species_Polygon > 0, Species_Polygon/EOO], na.rm = TRUE))
  ## some values > 1
  ## This might be a problem because of species raster (Species_Polygon) ...

## Check for Species_Polygon/EOO > 1 
nrow(out[Species_Polygon/EOO > 1])
out[Species_Polygon/EOO > 1][,.(spfile, Total_fire_points, Occurrence_Points, Total_fire_polygon, Species_Polygon, EOO, PAA_Points)]


## >> Checks to test species with and without polygons ####
sum(is.na(out$Species_Polygon))
sum(!is.na(out$Species_Polygon))


## Save output table ####
## Reorder columns
names(out)[c(1:13, 56, 55, 14, 46:50, 52:54, 57, 51, 16, 15, 17:45)]
out <- out[, c(1:13, 56, 55, 14, 46:50, 52:54, 57, 51, 16, 15, 17:45)]

setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, paste0("invert_overlap_", Sys.Date(), ".csv")), 
          row.names = FALSE)



## Add JW's columns ####
tab <- fread(file.path(bugs_dir, "table_components", "invert_overlap_PAAonly_2021-06-11_jw2.csv"))
tab <- tab[,.(spfile, `preliminary priority sp.`, threatened)]
setDT(tab, key = "spfile")

## >> Find species not found in outputs
all(tab$spfile %in% out$spfile)
sum(!tab$spfile %in% out$spfile)
tab$spfile[which(!tab$spfile %in% out$spfile)]

delete_sp <- fread(file.path(bugs_dir, "data_corrections", "delete_species_fromdata.csv"))
tab$spfile[which(!tab$spfile %in% out$spfile)][tab$spfile[which(!tab$spfile %in% out$spfile)] %in% delete_sp$x]
  ## Species were either deleted or iupdated
  ## See nesp_bugs_data/table_components/old_new_comparison1.xlsx

## >> Subset to species found in outputs
tab <- tab[spfile %in% out$spfile]

## >> Combine output tables
## Merge rows for species with polygon overlap innfo (n = 29029)
out1 <- merge(out, tab, by = "spfile")
dim(out1)

## Get rows for species without polygon informatiom (n = 29948)
sum(!(out$spfile %in% out1$spfile))
out2 <- out[!(out$spfile %in% out1$spfile)]
dim(out2)

## Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(tab)[-1]))), names(tab)[-1])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)

## Check
dim(out1)[1] + dim(out2)[1] == dim(out)[1]
ncol(out1) == ncol(out2)

## Combine both tables
out <- rbind(out1, out2); dim(out)
rm(out1, out2, tab)
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, paste0("invert_overlap_", Sys.Date(), ".csv")), 
          row.names = FALSE)
## invert_overlap_2021-06-24.csv


## Save table truncated to species with > 0 records in PAA ####
## Check species with Species_Polygon == 0
message(cat("Number of species with 0 points inside PAA: "),
        nrow(out[PAA_Points == 0]))
dim(out); out1 <- out[PAA_Points > 0]; dim(out1)
setDT(out1, key = "spfile")
write.csv(out1, file = file.path(output_dir, 
                                 paste0("invert_overlap_PAAonly_", 
                                        Sys.Date(), ".csv")), 
          row.names = FALSE)
## invert_overlap_PAAonly_2021-06-24.csv



## Combine trait table: Fire_impacted_invert_traits_09.06.csv ####
## Read in trait table
trait <- as.data.table(fread(file.path(bugs_dir, "JM_traits", "Trait_RAWdata.csv")))
setDT(trait, key = "spfile")

# out <- fread(file.path(output_dir, "invert_overlap_2021-06-24.csv"))

## Move spfile column to the front
grep("spfile", names(trait))
# x <- grep("spfile", names(trait))
# idx <- c(11, 1:(x-1), (x+1):ncol(trait)) ## doesn't work
trait <- trait[, c(11, 1:10, 12:186)]

## Check duplicates in spfile
trait_sp <- unique(trait$spfile)
length(trait_sp)
sum(duplicated(trait))
sum(duplicated(trait$spfile))

nrow(trait) == length(trait_sp) + sum(duplicated(trait$spfile))

trait[duplicated(trait$spfile)]$spfile
nrow(trait[grep("#N/A", trait$spfile)])
  ## NAs found need to be reatined and stiched at end of the table
trait[grep("#N/A", trait$spfile)]$scientificName
trait[grep("#N/A", trait$spfile)]$scientificName %in% out$scientificName
  ## The corresponding scientific name isn't foud in our output table


## Check duplicates in scientificName
length(unique(trait$scientificName))
sum(duplicated(trait$scientificName))
# trait[duplicated(trait$scientificName)]$scientificName

## Check spfile not found
length(trait_sp)
all(trait_sp %in% out$spfile)
trait_sp[which(!(trait_sp %in% out$spfile))]
  ## Only NAs, these will need to be added at the end of merging

  # ## Check against data, shows the same
  # data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
  # data_sp <- unique(data$spfile)
  # length(data_sp)
  # all(trait_sp %in% data_sp)
  # trait_sp[which(!(trait_sp %in% data_sp))]

## Combine output and trait table
dim(out)
dim(trait)

trait1 <- merge(trait, out, by = "spfile"); dim(trait1)

## Get rows from trait table where spfile = "#N/A" (23 rows)
trait2 <- trait[grep("#N/A", trait$spfile)]; dim(trait2)

## Add empty columns to trait2
dt <- setNames(data.table(matrix(nrow = nrow(trait2), ncol = length(names(out)[-1]))), names(out)[-1])
trait2 <- cbind(trait2, dt)
dim(trait2)
rm(dt)

## Check
dim(trait1)[1] + dim(trait2)[1] == dim(trait)[1]
ncol(trait1) == ncol(trait2)

## Combine both tables
names(trait1) == names(trait2)
names(trait1)[!names(trait1) == names(trait2)]
names(trait2)[!names(trait1) == names(trait2)]
names(trait2) = names(trait1)

trait <- rbind(trait1, trait2); dim(trait)
setDT(trait, key = "spfile")

## Save table
names(trait)[grep("\\.x", names(trait))] <- 
  gsub("\\.x$", "_OLD", names(trait)[grep("\\.x", names(trait))])
names(trait)[grep("\\.y", names(trait))] <- 
  gsub("\\.y$", "_NEW", names(trait)[grep("\\.y", names(trait))])
write.csv(trait, file = file.path(output_dir, 
                                paste0("invert_overlap_traits_", 
                                       Sys.Date(), ".csv")), 
          row.names = FALSE)
## invert_overlap_traits_2021-06-24.csv

  # ## Checks
  # names(out)[grep("_OLD|_NEW", names(out))]
  # 
  # all(out$scientificName_NEW == out$scientificName_OLD)
  # which(out$scientificName_NEW != out$scientificName_OLD)
  # out[out$scientificName_NEW != out$scientificName_OLD][, .(scientificName_OLD, scientificName_NEW)]
  # 
  # all(out$order_NEW == out$order_OLD)
  # sum(out$order_NEW != out$order_OLD, na.rm = TRUE)
  # out[out$order_NEW != out$order_OLD][, .(order_OLD, order_NEW)]
  # 
  # ## Save taxonomic information table
  # out <- fread(file.path(output_dir, "invert_overlap_traits_2021-06-11.csv"))
  # names(out)[grep("_OLD|_NEW", names(out))]
  # 
  # x <- out[!duplicated(data[,.(spfile, scientificName, class, order, family)])][, .(spfile, scientificName, class, order, family)]


## Compare inclusion_set species in invert_overlap_PAAonly_2021-06-24_jw.xlsx to 1077 species in Trait_RAWdata.csv
jw_tab <- fread(file.path(bugs_dir, "table_components", "invert_overlap_PAAonly_2021-06-24_jw.csv"))
jw_tab <- jw_tab[,.(spfile, scientificName, inclusion_set)]
sum(is.na(jw_tab$spfile))
sum(is.na(jw_tab$scientificName))
sum(is.na(jw_tab$inclusion_set))
unique(jw_tab$inclusion_set)
jw_tab[, .N, inclusion_set]
jw_tab <- jw_tab[inclusion_set != ""]

trait_sp <- fread(file.path(output_dir, "invert_overlap_traits_2021-06-24.csv"))$spfile
length(unique(trait_sp)) 
trait_sp <- trait_sp[trait_sp !="#N/A"] ## removing the NA species (see notes above)

insp <- jw_tab$spfile[jw_tab$spfile %in% trait_sp]
outsp <- jw_tab$spfile[!jw_tab$spfile %in% trait_sp]
length(insp)+length(outsp) == length(jw_tab$spfile)

write.csv(insp, file = file.path(output_dir, "SAMEsp_JW_inclusionset_Trait_RAWdata.csv"),
          row.names = FALSE)
write.csv(outsp, file = file.path(output_dir, "NEWsp_JW_inclusionset_Trait_RAWdata.csv"),
          row.names = FALSE)



## Stichinng sheets: Traits_master copy.xlsx & invert_overlap_PAAonly_2021-06-24_jw.csv ####
traits <- fread(file.path(bugs_dir, "JM_traits", "Traits_master_copy.csv"))
overlap <- fread(file.path(bugs_dir, "table_components", "invert_overlap_PAAonly_2021-06-24_jw.csv"))
columns <- fread(file.path(bugs_dir, "table_components", "ColumnsStitch_v2_TraitedSpp.csv"))
columns[, Sheet := NULL]

## Checks 
## #VALUE! in overlap read in as NaN 
## data.table treats is.na and is.nan similarly
## https://www.r-bloggers.com/2012/08/difference-between-na-and-nan-in-r/
sum(!is.nan(overlap$avge_severe_pts_polygons))
sum(!is.na(overlap$avge_severe_pts_polygons))

dim(traits)
dim(overlap)

names(traits)
names(overlap)

## Get column names for each sheet
overlap_cols <- columns[File == "invert_overlap_PAAonly_2021-06-24_jw"]$Columns
overlap_cols <- c("spfile", "PAA_Points", "Species_Polygon", overlap_cols); length(overlap_cols)
overlap_cols <- overlap_cols[c(1,4,5,7,8,9,10,6,2,11:12,3,13:43)]; length(overlap_cols)

traits_cols <- columns[File == "Traits_master copy"]$Columns
traits_cols <- c("spfile", "scientificName", "order", "family", traits_cols); length(traits_cols)

## Subset sheets by column names
dim(overlap); overlap <- overlap[,..overlap_cols]; dim(overlap)
dim(traits); traits <- traits[,..traits_cols]; dim(traits)

## Tables for spfile in and out of overlap table
setDT(overlap, key = "spfile")
setDT(traits, key = "spfile")

all(traits$spfile %in% overlap$spfile)
sum(traits$spfile %in% overlap$spfile)
sum(!traits$spfile %in% overlap$spfile)
traits$spfile[which(!traits$spfile %in% overlap$spfile)]

traits1 <- merge(traits, overlap, by = "spfile"); dim(traits1)
traits2 <- traits[grep("#N/A", traits$spfile)]; dim(traits2)

## Add empty columns to trait2
dt <- setNames(data.table(matrix(nrow = nrow(traits2), 
                                 ncol = sum(!names(traits1)%in% names(traits2)))), 
               names(traits1)[!names(traits1)%in% names(traits2)]); dim(dt)
names(dt)
traits2 <- cbind(traits2, dt); dim(traits2)


## Check
dim(traits1)[1] + dim(traits2)[1] == dim(traits)[1]
ncol(traits1) == ncol(traits2)

## Combine both tables
names(traits1) == names(traits2)
traits_new <- rbind(traits2, traits1)
dim(traits_new); dim(traits)
setDT(traits_new, key = "spfile")

## Save table
names(traits_new)
names(traits_new)[c(1:4, 169:174, 209:210, 175:208, 5:168)]
traits_new <- traits_new[, c(1:4, 169:174, 209:210, 175:208, 5:168)]
write.csv(traits_new, file = file.path(output_dir, 
                                  paste0("invert_overlap_traits_UPDATED_", 
                                         Sys.Date(), ".csv")), 
          row.names = FALSE)
