## Clean downloaded ALA data and summarise
## Notes: Each occurrence record has a record id, we can use `occurrence_details` to get additional information if you need something extra later

## >>>>>> PREVIOUS SCRIPT <<<<<<< ####
file.edit("")


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
output_dir = file.path(getwd(), "nesp_bugs", "outputs")
source(file.path(getwd(),"nesp_bugs", "scripts/remove_improper_names.R"))
source(file.path(getwd(),"nesp_bugs", "scripts/get_AFDsynonyms.R"))
source(file.path(getwd(),"nesp_bugs", "scripts/get_source.R"))

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# source(file.path(getwd(), "scripts/remove_improper_names.R"))
# source(file.path(getwd(), "scripts/get_AFDsynonyms.R"))
# source(file.path(getwd(), "scripts/get_source.R"))

## Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- sort(unique(afd_taxonomy$VALID_NAME))
message(cat("Number of species in AFD checklist: "),
        length(afd_species))

## Load ALA data
ala_raw <- readRDS(file.path(output_dir, "merged_ala_2020-10-02.rds"))
ala_species <- sort(unique(ala_raw$scientificName))
message(cat("Number of unique species in ALA data: "),
        length(ala_species))
message(cat("NAs in ALA species list: "),
        sum(is.na(ala_species)))



## ALA data cleaning ####
## Find improper & incomplete names in ALA data ####
species_record <- remove_improper_names(as.character(ala_species),
                                        allow.higher.taxa = FALSE,
                                        allow.subspecies = TRUE)
message(cat("# Improper species names found in ALA data: "),
        length(species_record$improper_species))
# message("Improper species names found in ALA data: ")
# species_record$improper_species
message(cat("# Incomplete species names found in ALA data: "),
        length(species_record$incomplete_species))
message(cat("NAs in cleaned ALA species list: "),
        sum(is.na(species_record$updated_list)))
message(cat("Duplicates in cleaned ALA species list: "),
        length(species_record$updated_list[duplicated(species_record$updated_list)]))

saveRDS(species_record, file = file.path(output_dir, "improper_names.rds"))

## Remove improper & incomplete names from ALA data ####
ala_species <- as.character(na.omit(species_record$updated_list))
ala_dat <- ala_raw[which(ala_raw$scientificName %in% ala_species),]

message(cat("Number of unique species in ALA data: "),
        length(ala_species))
message(cat("Number of records in raw ALA data: "),
        nrow(ala_raw))
message(cat("Number of records in cleaned ALA data: "),
        nrow(ala_dat))
message(cat("Prop of records lost in cleaning ALA data: "),
        (nrow(ala_raw) - nrow(ala_dat))/nrow(ala_raw))
message(cat("Proprotion of species removed: "),
        (length(unique(ala_raw$scientificName))-
           length(ala_species))/length(unique(ala_raw$scientificName)))

## Checks
length(ala_species) == length(unique(ala_dat$scientificName))
rje::is.subset(ala_species, unique(ala_dat$scientificName))
rje::is.subset(unique(ala_dat$scientificName), ala_species)
sum(ala_species==sort(unique(ala_dat$scientificName)))


## Find NZ and marine species in ALA ####
ala_species_all <- ala_species <- sort(unique(ala_dat$scientificName))

# matched_data <- suppressWarnings(get_source(ala_species))
# saveRDS(matched_data, file = file.path(output_dir, "matched_data.rds"))
matched_data <- readRDS(file.path(output_dir, "matched_data.rds"))

## >> Idenitfy source of species name as recorded in ALA database ####
unique(matched_data[,name_source])
matched_data[,.N, by = name_source]
matched_data[name_source == "APC"]$sp_names ## list APC species
matched_data[name_source == "CoL"]$sp_names ## list CoL species

## Remove species 
sources <- as.data.table(read.csv(file.path(output_dir, "name_source.csv")))
message(cat("Excluding name sources: \n"),
        paste0(sources[exclude == 1]$name_source, sep = "\n"))

sources <- sources[exclude == 1]$name_source
message(cat("Number of species removed: "),
        nrow(matched_data[name_source %in% sources]))
message(cat("Number of species retained: "),
        nrow(matched_data[name_source %!in% sources]))

ala_species <- matched_data[name_source %!in% sources]$sp_names

## Checks
length(ala_species)
sum(sort(ala_species_all[ala_species_all %!in% ala_species]) == sort(matched_data[name_source %in% sources]$sp_names))


## >> Identify (marine) habitat for species as listed in CAAB: http://www.cmar.csiro.au/caab/ ####
unique(matched_data[,caab_habitat]) 
matched_data[,.N, by = caab_habitat]

## Remove species 
marine <- as.data.table(read.csv(file.path(output_dir, "caab_habitat.csv")))
message(cat("Excluding name sources: \n"),
        paste0(marine[exclude == 1]$caab_habitat, sep = "\n"))

marine <- marine[exclude == 1]$caab_habitat
message(cat("Number of species removed: "),
        nrow(matched_data[caab_habitat %in% marine]))
message(cat("Number of species retained: "),
        nrow(matched_data[caab_habitat %!in% marine]))

ala_species <- matched_data[caab_habitat %!in% marine]$sp_names


## Remove NZ and marine species from ALA data ####
ala_species <- sort(ala_species)
message(cat("Number of unique species retained: "),
        length(ala_species))

ala_dat <- ala_raw[which(ala_raw$scientificName %in% ala_species),]
message(cat("Number of records in updated ALA data: "),
        nrow(ala_dat))

message(cat("Prop of records lost: "),
        (1949710 - nrow(ala_dat)) / 1949710)
message(cat("Proprotion of species removed: "),
        (62539 - length(ala_species)) / 62539)


## Get rid of unusable long lat vals ####
message(cat("Number of records with unusable lat-long:"),
        nrow(ala_dat[longitude < -180 |
                       longitude > 180 |
                       latitude < -90 |
                       latitude > 90, ]))


ala_dat <- ala_dat[longitude >= -180 &
                     longitude <= 180 &
                     latitude >= -90 &
                     latitude <= 90, ]

## Checks
range(ala_dat$latitude)
range(ala_dat$longitude)
length(ala_dat[longitude < -180]$longitude)
length(ala_dat[longitude > 180]$longitude)
length(ala_dat[latitude < -90]$latitude)
length(ala_dat[latitude > 90]$latitude)


## Identify ALA species not found in AFD checklist ####
## ALA scientificName compared against VALID_NAME and SYNONYMS in AFD checklist
## Species names categorised by number of words in the name for comparisons  
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Are all ALA species contained in the AFD checklist: "),
        rje::is.subset(ala_species, afd_species))


## >> ALA species: 4-word names ####
ala_species <- sort(unique(ala_dat$scientificName))
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 4

length(ala_species[which(sp_words == n)])
message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])

temp <- ala_dat[scientificName %in% ala_names]
write.csv(temp, file.path(output_dir, "names4_records.csv"))
rm(temp)

## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")

## Save outputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")), row.names = FALSE)

## Remove or mark unmatched and partially matched species from dataset
## Species list reviewed by JM: see names2_JRM.csv
## all species were found to either be marine or from NZ
message(cat("Number of records to be removed: "),
        length(which(ala_dat$scientificName %in% ala_names)))

## Remove records
dim(ala_dat)
ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_names),]
dim(ala_dat)


## >> ALA species: 3-word names ####
ala_species <- sort(unique(ala_dat$scientificName))
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 3

length(ala_species[which(sp_words == n)])
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])

message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
message(cat("Number of ALA records associated with species found: "),
        length(which(ala_dat$scientificName %in% ala_names)))

## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")
names(matches[!is.na(sapply(matches, "[[", 1))])

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")
names(matches[!is.na(sapply(matches, "[[", 2))])

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")
names(matches[!is.na(sapply(matches, "[[", 3))])

## Save outputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")), row.names = FALSE)

## Remove or mark unmatched and partially matched species from dataset
## Species list reviewed by JM: see names3_JRM.csv
## marine, NZ or exotic species idenntified were removed
ala_names <- fread(file.path(output_dir, paste0("names", n, "_JRM.csv")))
names3_out <- ala_names[exclude == 1]$ala_names
names3_in <- ala_names[exclude == 0]$ala_names

message(cat("Total number of species checked: "),
        length(ala_names))

message(cat("Number of species to be removed: "),
        length(names3_out))
message(cat("Number of records to be removed: "),
        length(which(ala_dat$scientificName %in% names3_out)))

message(cat("Number of species to be marked: "),
        length(names3_in))
message(cat("Number of records to be marked for checking: "),
        length(which(ala_dat$scientificName %in% names3_in)))

## Remove records
dim(ala_dat)
ala_dat <- ala_dat[which(ala_dat$scientificName %!in% names3_out),]
dim(ala_dat)

## Mark records
ala_dat <- cbind(ala_dat, rep(NA, nrow(ala_dat)))
names(ala_dat)[ncol(ala_dat)] <- paste0("names", n)

setkey(ala_dat, scientificName)
ala_dat[names3_in, paste0("names", n) := 1]
ala_dat[scientificName %!in% names3_in, paste0("names", n) := 0]

## Checks
length(names3_in) == sum(sort(unique(ala_dat[names3_in]$scientificName)) == sort(names3_in))
nrow(ala_dat[names3 == TRUE]) == length(which(ala_dat$scientificName %in% names3_in))


## >> ALA species: 2-word names ####
ala_species <- sort(unique(ala_dat$scientificName))
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 2

length(ala_species[which(sp_words == n)])
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])

message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
message(cat("Number of ALA records associated with species found: "),
        length(which(ala_dat$scientificName %in% ala_names)))

## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")
names(matches[!is.na(sapply(matches, "[[", 1))])

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")
names(matches[!is.na(sapply(matches, "[[", 2))])

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")
names(matches[!is.na(sapply(matches, "[[", 3))])

## Save outputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")), row.names = FALSE)

## Record and keep species in dataset: Unmacthed & partially matched species
message(cat("Number of records to be marked for checking: "),
        length(which(ala_dat$scientificName %in% ala_names)))

ala_dat <- cbind(ala_dat, rep(NA, nrow(ala_dat)))
names(ala_dat)[ncol(ala_dat)] <- paste0("names", n)

setkey(ala_dat, scientificName)
ala_dat[ala_names, paste0("names", n) := 1]
ala_dat[scientificName %!in% ala_names, paste0("names", n) := 0]

## Check
nrow(ala_dat[names2 == TRUE]) == length(which(ala_dat$scientificName %in% ala_names))


## Save cleaned ALA data ####
saveRDS(ala_dat, file = file.path(output_dir, paste0("clean1_ala_", Sys.Date(),".rds")))
write.csv(ala_dat, file = file.path(output_dir, paste0("clean1_ala_", Sys.Date(),".csv")))

message(cat("Number of records in data: "),
        nrow(ala_dat))


## Drop qa columns without values (i.e. all FALSE values) ####
qa <- as.data.frame(read.csv(file.path(output_dir, "qa_assertions.csv")))
x <- qa$name

drop_cols <- c()
for(i in x){
        if(eval(parse(text = paste0("sum(ala_dat$", i, ")"))) == 0){
                drop_cols <- c(drop_cols, i)
        }
}

message("Columns being dropped:")
drop_cols
message(cat("Number of columns being dropped: "),
        length(drop_cols))
ala_dat[, (drop_cols) := NULL]
message(cat("Number of columns in updated data: "),
        dim(ala_dat)[2])


## Drop records with issues ####
## Issues as commented by JM
exclude <- qa[which(qa$exclude == 1),]$name
out <- c()

## Find number of records with issues 
for(i in exclude){
        message(cat(paste0("Number of records with issue - ", i, ": ")),
                eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
        out <- c(out, eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}

## Remove records
for(i in 1: length(exclude)){
        if(any(out[i] > 0)){
                ala_dat <- eval(parse(text = paste0("ala_dat[", exclude[i], " == TRUE]")))
        } else {
                message(cat("No records found with issue: "),
                        exclude[i])
        }
}


## Create species file names & add to data as column ####
## NOTE: This step revelas duplicates in species names in the data;
## These are corrected in the data, not dropped
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of species with data: "),
        length(ala_species))
message(cat("Duplicates found in ala_species: "),
        sum(duplicated(ala_species)))

## Text modification
spfilename <- stringr::str_replace_all(ala_species, " ", "00xx00")
message(cat("Duplicates found in spfilename: "),
        sum(duplicated(spfilename)))

spfilename <- str_replace_all(spfilename, "[^[:alnum:]]", "")
message(cat("Duplicates found in spfilename: "),
        sum(duplicated(spfilename)))

spfilename <- tolower(gsub("00xx00", "_", spfilename))
message(cat("Duplicates found in spfilename: "),
        sum(duplicated(spfilename)))
spfilename <- unique(spfilename)
length(unique(spfilename))

## Identify duplicates in species name in data
spfilename[duplicated(spfilename)]
grep("incertae", spfilename)
ala_species[grep("incertae", spfilename)]
temp <- ala_species[grep("incertae", spfilename)]
dim(ala_dat[scientificName == temp[1]])
dim(ala_dat[scientificName == temp[2]])

## Modify data to correct duplicates in species name; no records dropped
ala_dat[scientificName == temp[2], scientificName := temp[1]]
dim(ala_dat[scientificName == temp[1]]) ## check
dim(ala_dat[scientificName == temp[2]]) ## check

## Add species file names column to data
y <- ala_dat$scientificName
y <- str_replace_all(y, " ", "00xx00")
y <- str_replace_all(y, "[^[:alnum:]]", "")
y <- tolower(gsub("00xx00", "_", y))
x <- sort(unique(y))
sum(x == sort(spfilename)) ## Check
ala_dat[, spfile := y]
dim(ala_dat)

## Display fields used to identify duplicates & check for NAs within ####
grep("ID|id", names(ala_dat), value = TRUE)
grep("catalogue", names(ala_dat), value = TRUE)
sum(is.na(ala_dat$collectionID)) ## https://dwc.tdwg.org/list/#dwc_collectionID
sum(is.na(ala_dat$institutionID))
sum(is.na(ala_dat$catalogueNumber))

## Check 'duplicateStatus' fields - NOT USEFUL
## This field is not used to subset data
grep("dup", names(ala_dat), value = TRUE)
unique(ala_dat$duplicateStatus) ## https://github.com/AtlasOfLivingAustralia/ala-dataquality/wiki/duplicate_status
ala_dat[,.N,by = duplicateStatus]

## Summary of cleaned data ####
message(cat("Number of records in cleaned data: "),
        nrow(ala_dat))
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of unique species in cleaned data: "),
        length(ala_species))

## Save updated data (all) ####
saveRDS(ala_dat, file = file.path(output_dir, paste0("clean2_ala_", Sys.Date(),".rds")))
write.csv(ala_dat, file = file.path(output_dir, paste0("clean2_ala_", Sys.Date(),".csv")))


## >>>>>> NEXT SCRIPT <<<<<<< ####
file.edit("/tempdata/workdir/nesp_bugs/scripts/nonala_processing.R")
