## Integrating ALA and nonALA data for species found in non ALA data

## >>>>>> PREVIOUS SCRIPT <<<<<<< ####
file.edit("/tempdata/workdir/nesp_bugs/scripts/nonala_processing.R")


## Set working environment ####
## _______________________________________________

rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", 
       "sp", "raster", "sf", "lubridate", 
       "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)


## Server paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
nonala_dir = file.path(bugs_dir, "nonALA")
output_dir = file.path(bugs_dir, "outputs")



## Load datasets ####
## _______________________________________________

## >> Non ALA cleaned data: clean1_nonala_
nonala_data <- fread(list.files(output_dir,
                                pattern = "clean1_nonala*.*csv$",
                                full.names = TRUE))
setDT(nonala_data)
dim(nonala_data)
length(unique(nonala_data$scientificName))
length(unique(nonala_data$spfile))
  ## There are duplicates in spfile for distinct scientificName.
  ## This problem arises due to simplification of species name for the
  ## improper names found in state/museum data.
  ## To be fixed further down...

sum(is.na(nonala_data$year))
sum(nonala_data$year == "", na.rm = TRUE)


## >> ALA data cleaned data: clean2_ala_
ala_data <- fread(list.files(output_dir,
                      pattern = "clean2_ala*.*csv$",
                      full.names = TRUE))
setDT(ala_data)
dim(ala_data)
length(unique(ala_data$scientificName))
length(unique(ala_data$spfile))


## Extract year for ALA data ####
names(ala_data)[grep("Date", names(ala_data))]

sum(is.na(ala_data$eventDate))
sum(is.na(ala_data$year))
sum(ala_data$year == "", na.rm = TRUE)
nrow(ala_data[is.na(eventDate) & is.na(year)])
range(ala_data$year, na.rm = TRUE)

## >> Find records for which year can be extracted
sum(is.na(ala_data$year) & !is.na(ala_data$eventDate))
id <- which(is.na(ala_data$year) & !is.na(ala_data$eventDate))

## >> Specify year based in eventDate for records found
ala_data$eventDate[id]
range(year(ymd(ala_data$eventDate[id])))
ala_data$year[id]
ala_data$year[id] <- year(ymd(ala_data$eventDate[id]))
ala_data$year[id]
sum(is.na(ala_data$year))

message(cat("Proportion of ALA records without evenDate info: "),
        nrow(ala_data[is.na(eventDate)])/nrow(ala_data))
message(cat("Proportion of ALA records without year info: "),
        nrow(ala_data[is.na(year)])/nrow(ala_data))

# message(cat("Number of records with collection Date listed as invalid: "),
#         sum(ala_data$invalidCollectionDate))
# sum(is.na(ala_data$verbatimEventDate)) ## "" instead of NAs

## Format ALA data table ####
names(ala_data)
ala_data <- ala_data[ , .(id, class, family, scientificName, latitude, longitude, year, spfile)]
ala_data$data_source <- rep("ALA", nrow(ala_data))
ala_data$sensitive <- rep(0, nrow(ala_data))
ala_data$habitat <- rep("NA", nrow(ala_data))
ala_data <- ala_data[,c(9,1:7,10,11,8)]
names(ala_data) <- names(nonala_data)


## Find species in non ALA data also found in ALA ####
## _______________________________________________________________

nonala_sp <- unique(nonala_data$scientificName)
message(cat("Number of species in non ALA data: "),
        length(nonala_sp))

ala_sp <- unique(ala_data$scientificName)
message(cat("Number of species in ALA data (some repetitions here*): "),
        length(ala_sp))
## * see spfile section

message(cat("Number of non ALA species found in ALA data: "),
        length(nonala_sp[nonala_sp %in% ala_sp]))

message(cat("Number of non ALA species NOT found in ALA data: "),
        length(nonala_sp[!(nonala_sp %in% ala_sp)]))

message(cat("Number of ALA species NOT found in non ALA data: "),
        length(ala_sp[!(ala_sp %in% nonala_sp)]))


  # ## Split ALA data  -- NOT NEEDED
  # ## >> ALA data for non ALA species
  # ala_sub1 <- ala_data[spfile %in% nonala_sp[nonala_sp %in% ala_sp]]
  # ala_sub1 <- setDT(ala_sub1)
  # dim(ala_sub1)
  # length(unique(ala_sub1$spfile))
  # length(unique(ala_sub1$scientificName))
  # 
  # ## >> ALA data remaining
  # ala_sub2 <- ala_data[spfile %in% ala_sp[!(ala_sp %in% nonala_sp)]]
  # ala_sub2 <- setDT(ala_sub2)
  # dim(ala_sub2)
  # length(unique(ala_sub2$spfile))
  # length(unique(ala_sub2$scientificName))


## Combine ALA and non ALA for species found in both datasets ####
dat <- rbind(ala_data, nonala_data)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))
  ## To be fixed further down...



## Mask data ####
## _______________________________________________

## >> Load mask in WGS ####
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # ## Load mask in AEA137 - NOT REQUIRED
  # mask_file <- file.path(output_dir, "masks", "ausmask_noaa_250mAlbersEA_NA.tif")
  # ausmask <- raster::raster(mask_file)
  # wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # aea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  # 
  # ## Reproject data to AEA137 - NOT REQUIRED
  # sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
  #                     proj4string = CRS(wgs_crs))
  # sp_reproj <- sp::spTransform(sp, CRS(aea_crs))
  # str(sp_reproj@coords)
  # 
  # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
  # plot(sp_reproj, add = TRUE, pch = 17, col = "tomato3", cex = 0.5)
  # 
  # dat$latitude <- sp_reproj@coords[,"latitude"]
  # dat$longitude <- sp_reproj@coords[,"longitude"]


## >> Check for out-of-extent lat/longs ####
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])


## >> Remove points falling off extent (if any) ####
n <- dim(dat)[1]
dat <- dat[which(longitude >= ausmask@extent@xmin)]
dat <- dat[which(longitude <= ausmask@extent@xmax)]
dat <- dat[which(latitude >= ausmask@extent@ymin)]
dat <- dat[which(latitude <= ausmask@extent@ymax)]

message(cat("Proportion of records lost by clipping to mask extent: "),
        (n-dim(dat)[1])/n)


## >> Clip data/occurrence points if they fall outside mask polygon(s) ####
sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
                    proj4string = CRS(wgs_crs))
  # sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
  #                     proj4string = CRS(aea_crs))
grd.pts <-extract(ausmask, sp)

dat0 <- dat[is.na(grd.pts),]
dat <- dat[!is.na(grd.pts),]

message(cat("Proportion of records lost due to falling on NAs within the mask: "),
        dim(dat0)[1]/dim(dat)[1])


## >> Precise plotting
points1 <- sp::SpatialPoints(dat[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
points0 <- sp::SpatialPoints(dat0[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
plot(points1, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)
plot(points0, add = TRUE, pch = 18, col = "green", cex = 0.5)



## Find duplicates and prioritise ALA records for removal ####
## _______________________________________________________________

message(cat("Proportion of data duplicated: "),
        sum(duplicated(dat[,c("scientificName", 
                               "latitude", 
                               "longitude")]))/dim(dat)[1])
dat <- setDT(dat)[order(-data_source), .SD[1L] ,.(scientificName, latitude, longitude)]
  ## How this works: 
  ## > setDT - set as data.table
  ## > order in decreasing order by data_source (i.e. so that alphabetically ALA comes last)
  ##    this is same as setorder(dat, -data_source)
  ## > .SD[1L] get the first observation for each group by .(scientificName, latitude, longitude)
  ## > .(scientificName, latitude, longitude) defines the groups
  ## Ref: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-sd-usage.html
  ## Ref: https://stackoverflow.com/questions/8508482/what-does-sd-stand-for-in-data-table-in-r



## Apply year filter ####
## ___________________________________________________

## >> Find records without year information
plot(dat[!is.na(year)][, .N, year], xaxp = c(1630, 2020, 10), pch = 20)
range(dat$year, na.rm = TRUE)

message(cat("Proportion of records with year = NA: "),
        sum(is.na(dat$year))/nrow(dat))
sum(is.na(dat$year))
sum(dat$year == "")

message(cat("Proportion of records with year < 1990: "),
        nrow(dat[year < 1990])/nrow(dat))

message(cat("Proportion of records lost if applying year filer on records with year=NA or year < 1990: "),
        nrow(dat[is.na(year) | year < 1990])/nrow(dat))


## >> Number of records lost with year filter
t1 <- dat[, .N, scientificName]
t2 <- dat[!is.na(year) & year >= 1990][, .N, scientificName]
t <- merge(t1, t2, by = "scientificName", all.x = TRUE)
dim(t)
names(t)[2:3] <- c("n.all", "n.sub")
t[which(is.na(n.sub))]$n.sub = 0
sum(is.na(t$n.sub))
setorder(t, n.sub)
# t$p.lost <- (t$n.all - t$n.sub)/t$n.all

message(cat("Number of species with < 3 records to begin with: "),
        nrow(t[n.all < 3]))
message(cat("Number of species with < 3 records after filter for 1990 and NAs: "),
        nrow(t[n.sub < 3])); nrow(t[n.sub < 3])/nrow(t)
message(cat("Number of species with >= 3 records after filter for 1990 and NAs: "),
        nrow(t[n.sub >= 3])); nrow(t[n.sub >= 3])/nrow(t)
message(cat("Number of species with < 3 records after filter but which has >=3 records before filter: "),
        nrow(t[n.sub < 3 & n.all >=3]))


## >> Remove records based on filter rule: 
##  if >= 3 records after filter, remove NA and <1990
##  if < 3 records after filter, keep NA and <1990
sp_applyfilter <- t[n.sub >= 3]$scientificName
length(sp_applyfilter)

dim(dat)
dat0 <- dat[scientificName %in% sp_applyfilter][!is.na(year) & year >= 1990]
dat1 <- dat[!(scientificName %in% sp_applyfilter)]

  ## Checks
  sum(dat0[, .N, scientificName]$N < 3)
  dim(dat0)[1] + dim(dat1)[1]
  length(unique(dat0$scientificName)) + 
    length(unique(dat1$scientificName)) == length(unique(dat$scientificName))

n <- dim(dat)[1]
dat <- rbind(dat0, dat1)
points(dat[!is.na(year)][, .N, year], xaxp = c(1630, 2020, 10), pch = 20, col = "tomato3")

message(cat("Number of records lost with year-filer: "),
        n-dim(dat)[1])
message(cat("Proportion of data lost with year-filer: "),
        (n-dim(dat)[1])/n)
message(cat("Number of unique species in data: "),
        length(unique(dat$scientificName)))




## Complete taxonomic information (if possible) ####
## >> >> >> Add order << << << ####
## ___________________________________________________

## >> List records with incomplete class/family columns ####
message(cat("Number of records with NA class: "),
        nrow(dat[is.na(class)]))
message(cat("Number of records with NA family: "),
        nrow(dat[is.na(family)]))
message(cat("Species with NA class: "))
unique(dat[is.na(class)]$scientificName)
  ## No class information listed in ALA

message(cat("Number of records with blank string for family: "),
        nrow(dat[family == ""]))
message(cat("Number of records with blank string for class: "),
        nrow(dat[class == ""]))
message(cat("Number of records with blank string for class & family: "),
        nrow(dat[class == "" & family == ""]))

dat[class == "" & family == ""][, .N, data_source]
unique(dat[class == "" & family == ""]$scientificName)
  # x <- unique(dat[class == "" & family == ""]$scientificName)
  # write.csv(x, file.path(output_dir, "incomplete_taxinfo.csv"), row.names = FALSE)


## >> Delete records as per incomplete_taxinfo_JRM.csv ####
taxinfo <- fread(file.path(output_dir, "incomplete_taxinfo_JRM.csv"))
dropspecies <- taxinfo[exclude != ""]$species
taxinfo <- taxinfo[exclude == ""]

nrow(dat[class == "" & family == "" & scientificName %in% dropspecies])
nrow(dat[scientificName %in% dropspecies])
nrow(dat) - nrow(dat[!(scientificName %in% dropspecies)])

dat <- dat[!(scientificName %in% dropspecies)]


## >> Populate class/family columns as per incomplete_taxinfo_JRM.csv ####
for (sp in taxinfo$species){
  dat[class == "" & family == "" & scientificName %in% sp]$class = taxinfo[species == sp]$class
  dat[class == "" & family == "" & scientificName %in% sp]$family = taxinfo[species == sp]$family
}


## >> Add taxonominc info for anic ####
## Coenobiodes, Insecta, Tortricidae : provided by JM
dat[class == "" & family == ""][, .N, data_source]
dat[class == "" & family == ""]
dat[class == "" & family == ""]$class <- "Insecta"
dat[class == "" & family == ""]$family <- "Tortricidae"

message(cat("Number of records with blank string for class AND family: "),
        nrow(dat[class == "" & family == ""]))
dat[family == "" | class == ""][, .N, data_source]
unique(dat[family == "" & data_source != "ALA"]$scientificName)
  # temp <- unique(dat[family == "" & data_source != "ALA"]$scientificName)
  # write.csv(temp, file.path(output_dir, "naclass_nonala.csv"), row.names = FALSE)


## >> Populate class/family columns as per naclass_nonala_JRM.csv ####
taxinfo <- fread(file.path(output_dir, "naclass_nonala_JRM.csv"))
for (sp in taxinfo$species){
  dat[(class == "" | family == "") & scientificName %in% sp]$class = taxinfo[species == sp]$class
  dat[(class == "" | family == "") & scientificName %in% sp]$family = taxinfo[species == sp]$family
}

dat[family == "" | class == ""][, .N, data_source]

dat[(family == "" | class == "") & data_source == "anic_moths"]
dat[(family == "" | class == "") & data_source == "anic_moths"]$family <- "Tortricidae"

dat[(family == "" | class == "") & data_source == "SA_Churchett_Q26508-3"]
dat[(family == "" | class == "") & data_source == "SA_Churchett_Q26508-3"]$family <- "Formicidae"
 
message(cat("Number of records with blank string for family OR class: "),
        nrow(dat[family == "" | class == ""]))

dat[family == "" | class == ""][, .N, data_source]
unique(dat[family == "" | class == ""]$scientificName)
temp <- dat[family == "" | class == ""]
temp <- temp[,.(scientificName, class, family)]
temp <- temp[!duplicated(temp)]
write.csv(temp, file.path(output_dir, "naclassfamily_ala.csv"), row.names = FALSE)


## >> Populate class/family columns as per naclassfamily_ala_JRM.csv ####
taxinfo <- fread(file.path(output_dir, "naclassfamily_ala_JRM.csv"))
dropspecies <- taxinfo[exclude != ""]$species
taxinfo <- taxinfo[exclude == ""]
dat <- dat[!(scientificName %in% dropspecies)]

for (sp in taxinfo$species){
  dat[(class == "" | family == "") & scientificName %in% sp]$class = taxinfo[species == sp]$class
  dat[(class == "" | family == "") & scientificName %in% sp]$family = taxinfo[species == sp]$family
}

message(cat("Number of records with blank string for family OR class: "),
        nrow(dat[family == "" | class == ""]))
message(cat("Number of species with blank string for family OR class: "),
        length(unique(dat[family == "" | class == ""]$scientificName)))
  ## leftover species do not have class or family values assigned yet

## >> Formatting class & family columns ####
head(dat)
dat$class <- tolower(dat$class)
dat$family <- tolower(dat$family)



## Add sensitive species information as per JM's list ####
## ____________________________________________________________________

sensitive_sp <- fread(file.path(bugs_dir, "aus_listed", "Aus_listed_spp.csv"))$Species

## Fix funny formattig issues in data sheet
grep('\\\\', sensitive_sp)
sensitive_sp[92] <- 'Charopidae "Skemps"'
sensitive_sp[118] <- "Enchymus sp.nov."
sensitive_sp[164] <- "Lissotes menalcas"
sensitive_sp[172] <- "Miselaoma weldii"
grep('\\\\', sensitive_sp)

sensitive_sp <- stringi::stri_enc_toutf8(sensitive_sp, validate = TRUE) 
sensitive_sp[1:3] <- gsub("_", "", sensitive_sp[1:3])
sensitive_sp <- trimws(sensitive_sp)

dat_sp <- unique(dat$scientificName)
length(sensitive_sp)
sum(sensitive_sp %in% dat_sp)
sum(!(sensitive_sp %in% dat_sp))

## Look again for species not found...
for (sp in sensitive_sp[!(sensitive_sp %in% dat_sp)]) {
  print(paste0("Lookign for speceis: ", sp, " ..."))
  if (length(grep(sp, dat$scientificName)) == 0) {
    print("NOT FOUND")
  } else{
    print(grep(sp, dat$scientificName))
  }
}
sensitive_sp1 <- sensitive_sp[sensitive_sp %in% dat_sp]

  # temp <- sensitive_sp[!(sensitive_sp %in% dat_sp)]
  # write.csv(temp, file.path(bugs_dir, "aus_listed",, "aus_listed_notindata.csv"))

## See aus_listed_notindata_JRM.csv
sensitive_sp2 <- fread(file.path(bugs_dir, "aus_listed", "aus_listed_notindata_JRM.csv"))
dropspecies <- sensitive_sp2[exclude != ""]$species
dat <- dat[!(scientificName %in% dropspecies)] ## none found

sensitive_sp2 <- sensitive_sp2[exclude == ""]
sensitive_sp2[name_in_ala != "", species := name_in_ala]
sensitive_sp2 <- sensitive_sp2$species

sensitive_sp <- c(sensitive_sp1, sensitive_sp2)

for (sp in sensitive_sp){
  dat[scientificName == sp]$sensitive  <- 1
}



## Remove non natives ####
## _____________________________________________________________
## see data_corrections/exotics_alanonala.csv for list generated below
exotics <- fread(file.path(bugs_dir, "ALA", 
                           "GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))

dat_sp <- unique(dat$scientificName)
sum(dat_sp %in% exotics$`Supplied Name`)
sum(dat_sp %in% exotics$scientificName)
exotic1 <- dat_sp[dat_sp %in% exotics$`Supplied Name`]
exotic2 <- dat_sp[dat_sp %in% exotics$scientificName]

exotics <- c(exotic1, exotic2)
exotics <- exotics[!duplicated(exotics)]

dim(dat[(scientificName %in% exotics)])
dat <- dat[!(scientificName %in% exotics)]



## Final data checks ####
## _____________________________________________________________
message(cat("Number of records in cleaned data: "),
        dim(dat)[1])
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))

message(cat("Number of records with blank string for family OR class: "),
        nrow(dat[family == "" | class == ""]))
message(cat("Number of species with blank string for family OR class: "),
        length(unique(dat[family == "" | class == ""]$scientificName)))

message(cat("Duplicated records: "),
        sum(duplicated(dat[,c("scientificName", "latitude", "longitude")])))

message(cat("Records without year info: "),
        sum(is.na(dat$year)))



## Create new column to give unique ID by scientificName ####
## _____________________________________________________________
setDT(dat)[, new_id := .GRP, by = c("scientificName", "class", "family")]
length(unique(dat$new_id))
range(unique(dat$new_id))
length(unique(dat$scientificName))

  # temp <- c()
  # for (sp in unique(dat$scientificName)){
  #   if(length(unique(dat[scientificName %in% sp]$new_id)) > 1){
  #     temp <- c(temp, sp)
  #   }
  # }
  # temp
  # ## a number of species with uninfomrative names
  # x <- "`indet.`"
  # unique(grep(x, dat$scientificName, value = TRUE))
  # dat[grep(x, dat$scientificName)]

## Merge spfile and new_ID (overwrite existing spfile column)
dat$spfile <- paste0(dat$spfile, "_", dat$new_id)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))
dat[,new_id := NULL]




## Save combined data table - WGS 84 ####
## _____________________________________________________________
## This dataset contains all nonALA data and a subset of the ALA data 
## i.e., subset of species in ALA found in nonALA data.
setorder(dat, scientificName)
write.csv(dat, file = file.path(output_dir, "data_ALAnonALA_wgs84.csv"), 
          row.names = FALSE)

## Summarise
print(setorder(dat[, .N, data_source], -N))
x <- dat[, .N, data_source == "ALA"]
message(cat("proportion of data from ALA: "),
        round(x[data_source == TRUE]$N/dim(dat)[1], 2))
message(cat("proportion of data from non-ALA sources: "),
        round(x[data_source == FALSE]$N/dim(dat)[1], 2))

x <- setorder(dat[, .N, data_source], -N)
sum(x[grep("WA", x$data_source),]$N)


## >>>>>> NEXT SCRIPT <<<<<<< ####
# data_correction_speciesname.R
file.edit("/tempdata/workdir/nesp_bugs/scripts/data_corrections_speciesnames.R")