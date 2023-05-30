## Summarise ALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
output_dir = file.path(getwd(), "nesp_bugs", "outputs")
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_taxondata.R"))

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# source(file.path(getwd(), "scripts/get_ala_taxondata.R"))

## Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- sort(unique(afd_taxonomy$VALID_NAME))
message(cat("Number of species in AFD checklist: "),
        length(afd_species))

## Load ALA data
ala_raw <- readRDS(file.path(output_dir, "merged_ala_2020-10-02.rds"))
ala_dat <- list.files(output_dir, 
                      pattern = "clean2_ala*.*.csv$", 
                      full.names = TRUE)
ala_dat <- fread(ala_dat)
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of unique species in ALA data: "),
        length(ala_species))


## Data cleaning summary ####
## Raw ALA data overview
message(cat("Number of records in raw ALA data: "),
        dim(ala_raw)[1])
message(cat("Number of species in raw ALA data: "),
        length(unique(ala_raw$scientificName)))
message(cat("Raw ALA data file: \n"),
        paste0(list.files(output_dir, 
                          pattern = "merged_ala", 
                          full.names = TRUE), sep = "\n"))

## Cleaned ALA data overview
message(cat("Number of records in cleaned ALA data: "),
        dim(ala_dat)[1])
message(cat("Number of species in cleaned ALA data: "),
        length(unique(ala_dat$scientificName)))
message(cat("Cleaned ALA data file: \n"),
        paste0(list.files(output_dir, 
                          pattern = "clean2_ala", 
                          full.names = TRUE), sep = "\n"))

## Data lost in cleaning
message(cat("Proportion of records lost in cleaning ALA data: "),
        (dim(ala_raw)[1] - dim(ala_dat)[1])/(dim(ala_raw)[1]))
message(cat("Proportion of species lost in cleaning ALA data: "),
        (length(unique(ala_raw$scientificName)) - 
           length(unique(ala_dat$scientificName)))/
          (length(unique(ala_raw$scientificName))))

## Number of species in AFD and ALA
message(cat("Number of species in AFD checklist: "),
        length(afd_species))
message(cat("Number of unique species in ALA data: "),
        length(ala_species))


## AFD species with data in ALA
message(cat("Number of AFD species found in ALA: "),
        sum(afd_species %in% ala_species))
message(cat("Proportion of AFD species found in ALA: "),
        sum(afd_species %in% ala_species)/length(afd_species))


## AFD species without data in ALA
message(cat("Number of AFD species not found in ALA: "),
        sum(afd_species %!in% ala_species))
message(cat("Proportion of AFD species not found in ALA: "),
        sum(afd_species %!in% ala_species)/length(afd_species))


## Cleaned ALA data summary ####
## Species with 0 records (i.e. AFD species not found in ALA)
count0 <- afd_species[afd_species %!in% ala_species]
message(cat("Number of AFD species without records in cleaned ALA data: "),
        length(count0))
write.csv(count0, file = file.path(output_dir, "nodata_AFDspecies.csv"), row.names = FALSE)

## ALA species by number of records (> 0)
counts <- ala_dat[,.N,by = scientificName]
nrow(counts) == length(ala_species)
write.csv(counts, file = file.path(output_dir, "datacounts_ALAspecies.csv"), row.names = FALSE)

count1 <- counts[which(counts$N == 1)]
message(cat("Number of species with 1 record in cleaned ALA data: "),
        nrow(count1))

countLTE20 <- counts[which(counts$N > 1 & counts$N <= 20) , ]
message(cat("Number of species with more than 1 and less than or equal to 20 records in cleaned ALA data: "),
        nrow(countLTE20))

countMT20 <- counts[which(counts$N > 20)]
message(cat("Number of species with more than 20 records in cleaned ALA data: "),
        nrow(countMT20))

nrow(count1) + nrow(countLTE20) + nrow(countMT20) == nrow(counts)
nrow(count1) + nrow(countLTE20) + nrow(countMT20) == length(afd_species)
## because we have additional species in ALA compared to AFD checklist
## see names3.csv and names2.csv

## List for ALA mapping (Oct 2020)
countMTE5 <- counts[which(counts$N >= 5)]
countMTE5 <- countMTE5[order(N)]
range(countMTE5$N)
write.csv(countMTE5, file = file.path(output_dir, "countMTE5.csv"), row.names = FALSE)


## List for DAWE
## List of 98904 species from the cleaned ALA data that have 0 or less than 20 records
tab1 <- data.table(scientificName = count0)
tab1[,N := rep(0, length(count0))]
tab1 <- rbind(tab1, count1, countLTE20[order(countLTE20$N), ])
write.csv(tab1, file = file.path(output_dir, "ALAsp_sparsedata.csv"), row.names = FALSE)

## ALA species by year
age <- ala_dat[,.N, by = as.numeric(format(ala_dat$eventDate,'%Y'))]
names(age) <- c("year", "N")
age <- age[order(year)]
write.csv(age, file = file.path(output_dir, "yearcounts_ALAspecies.csv"), row.names = FALSE)

message(cat("Number of records without date in cleaned ALA data: "),
        age[is.na(year)]$N)

message(cat("Number of old records (before 1990) in cleaned ALA data: "),
        sum(age[year < 1990]$N))

message(cat("Number of recent records (after 1990) in cleaned ALA data: "),
        sum(age[year >= 1990]$N))

age[is.na(year)]$N + sum(age[year < 1990]$N) + sum(age[year >= 1990]$N) == sum(age$N)
sum(age$N) == dim(ala_dat)[1]


## Obervation versus specimen data in ALA database ####
## Get counts from ALA database
# typecounts <- lapply(afd_species, get_ala_taxondata, 
#                         get_counts_only = TRUE, 
#                         specimens_only = TRUE)
# 
# typecounts <- t(as.data.frame(typecounts))
# typecounts <- cbind(afd_species, typecounts)
# typecounts <- typecounts[ order(typecounts$species), ]
# typecounts <- as.data.table(typecounts)
# colnames(typecounts)[1] <- "species"
# saveRDS(typecounts, file.path(output_dir, "typecounts.rds"))
# write.csv(typecounts, file.path(output_dir, "typecounts.csv"), row.names = FALSE)
typecounts <- as.data.table(readRDS(file.path(output_dir, "typecounts.rds")))

## Summarise: All data
message(cat("Number of species with 0 records in ALA db: "),
        nrow(typecounts[all == 0]))
message(cat("Number of species with 1 record in ALA db: "),
        nrow(typecounts[all == 1]))
message(cat("Number of species with more than 1 and less than or equal to 20 records in ALA db: "),
        nrow(typecounts[all > 1 & all <= 20]))
message(cat("Number of species with more than 20 records in ALA db: "),
        nrow(typecounts[all > 20]))

## Summarise: Observation data
message(cat("Number of species with 0 observation records in ALA db: "),
        nrow(typecounts[observation == 0]))
message(cat("Number of species with 1 observation record in ALA db: "),
        nrow(typecounts[observation == 1]))
message(cat("Number of species with more than 1 and less than or equal to 20 observation records in ALA db: "),
        nrow(typecounts[observation > 1 & observation <= 20]))
message(cat("Number of species with more than 20 observation records in ALA db: "),
        nrow(typecounts[observation > 20]))

## Summarise: Specimen data
message(cat("Number of species with 0 specimen records in ALA db: "),
        nrow(typecounts[specimen == 0]))
message(cat("Number of species with 1 specimen record in ALA db: "),
        nrow(typecounts[specimen == 1]))
message(cat("Number of species with more than 1 and less than or equal to 20 specimen records in ALA db: "),
        nrow(typecounts[specimen > 1 & specimen <= 20]))
message(cat("Number of species with more than 20 specimen records in ALA db: "),
        nrow(typecounts[specimen > 20]))


## Explore data issues ####
## >> Load list of data issues
qa <- as.data.frame(read.csv(file.path(output_dir, "qa_assertions.csv")))

## >> Check for unusable long lat vals #### 
message(cat("Number of records with unusable lat-long:"),
        nrow(ala_dat[longitude < -180 |
                       longitude > 180 |
                       latitude < -90 |
                       latitude > 90, ]))

## >> Check for records with geospatial issues ####
x <- grep("geo|Geo" , qa$name, value = TRUE)
for(i in x){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}


## >> Check for records without date ####
message(cat("Number of records without date: "),
        sum(is.na(ala_dat$eventDate)))
x <- grep("date|Date" , qa$name, value = TRUE)
for(i in x){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}


## >> Sensitive species ####
## TO BE RESOLVED LATER BY SPECIES
grep("General|sensitive", names(ala_dat), value = TRUE)

ala_dat[,.N,by = sensitive]

unique(ala_dat$dataAreGeneralised)
sum(ala_dat$dataAreGeneralised)
ala_dat[,.N,by = dataAreGeneralised]

unique(ala_dat$dataGeneralizationsOriginal)
ala_dat[,.N,by = dataGeneralizationsOriginal]
sum(grepl("Coordinate precision generalised", ala_dat$dataGeneralizationsOriginal))

# ## Remove generalised data (as much as possible)/Sensitive
# if(any(grepl("dataGeneralizations", names(ala_df)))) {
#   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
# }

## >> Summary of by QA assertions ####
## Issues as commented by JM
exclude <- qa[which(qa$exclude == 1),]$name
for(i in exclude){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}

keep <- qa[which(qa$keep == 1),]$name
for(i in keep){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}

drop_qa <- qa[which(qa$not_relevant == 1),]$name
for(i in drop_qa){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}

## List all issues with corresponding number of records
x <- qa$name
for(i in x){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}

## Only list issues with > 0 records associated
for(i in x){
  if(eval(parse(text = paste0("sum(ala_dat$", i, ")"))) > 0){
    message(cat(paste0("Number of records with issue - ", i, ": ")),
            eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
  }
}