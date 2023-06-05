## Data corrections for ALA-nonALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "future", "future.apply", "parallel",
       "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)


## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
corr_dir = file.path(bugs_dir, "data_corrections")



## Species names A - L ####
temp <- fread(file.path(corr_dir, "invert_fireoverlap_ALnameissues_JRM.csv"))
setDT(temp, key = "spfile")
dim(temp)


## >> Subset table to A - L (upto 32981 when arranged by spfile) ####
temp[32981]$spfile
temp[32982]$spfile

temp <- temp[1:32981, ]
dim(temp)


## >> Quality checks ####
temp[, .N, delete_sp]
temp[, .N, name_issues]
temp[, .N, spell_variants]
nrow(temp[!is.na(name_corrections)])
  ## spell_variants and name_corrections should be equal
  ## i.e., for a record if spell_variants = 1, text string should be provided in name_corrections


## >> Find NA in name_corrections ####
message(cat("Number of name_corrections records to be resolved: "),
        nrow(temp[!is.na(spell_variants)
                  & is.na(name_corrections)
                  & is.na(delete_sp)]))


## >> Find species with 'group/complex/etc.' in name ####
grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName, value = TRUE)
  # x <- temp[grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JM_names_correction1.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JM_names_correction1_JRM.csv
x <- fread(file.path(corr_dir, "JM_names_correction1_JRM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Find species with '?' in name ####
  # x <- temp[grep("\\?", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JM_names_correction2.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JM_names_correction2_JRM.csv
x <- fread(file.path(corr_dir, "JM_names_correction2_JRM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Save data table ####
temp1 <- data.table::copy(temp)
rm(temp)



## Species names M - Z  ####
temp <- fread(file.path(corr_dir, "invert_fireoverlap_MZnameissues_JW_JM.csv"))
setDT(temp, key = "spfile")
dim(temp)

## >> Subset table to M - Z (from 32982 when arranged by spfile) ####
temp[32981]$spfile
temp[32982]$spfile

temp <- temp[32982:nrow(temp), ]
dim(temp)

## >> Correct delete_sp in JW sheet ####
temp[, .N, delete_sp]
temp[, .N, `JW notes`]
temp[, .N, `JM notes`]
temp[`JM notes` == "remove"]$delete_sp 
  ## JM already added 1 to delete_sp inn data sheet
temp[, `JW notes` := NULL]
temp[, `JM notes` := NULL]

## >> Quality checks ####
temp[, .N, name_issues]
temp[, .N, spell_variants]
nrow(temp[!is.na(name_corrections)])
  ## spell_variants and name_corrections should be equal
  ## i.e., for a record if spell_variants = 1, text string should be provided in name_corrections


## >> Find NA in name_corrections ####
message(cat("Number of name_corrections records to be resolved: "),
        nrow(temp[!is.na(spell_variants)
                  & is.na(name_corrections)
                  & is.na(delete_sp)]))
  # x <- temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_1spellvariants_NAnamecorrections.csv"), row.names = FALSE)

  # grep("mandjelia_myg438_wanjarri_1969", temp$spfile)
  # temp[grep("mandjelia_myg438_wanjarri_1969", temp$spfile)]
  # grep("mandjelia_myg438_wanjarri_1969", temp$name_corrections)
  # temp[grep("mandjelia_myg438_wanjarri_1969", temp$name_corrections)]

## >> Correct NAs in name_corrections ####
## NOTE: These are records with the correct species name, 
##  so name_corrections column can be populated with corresponding spfile;
##  associated records with the incorrect species inndicated in name_correctionns already
temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)]$name_corrections = temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)]$spfile
message(cat("Number of name_corrections records to be resolved: "),
        nrow(temp[!is.na(spell_variants)
                  & is.na(name_corrections)
                  & is.na(delete_sp)]))

## >> Find species with 'group/complex/etc.' in name ####
  # x <- temp[grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_names_correction1.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JW_names_correction1_JM.csv
x <- fread(file.path(corr_dir, "JW_names_correction1_JM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Find species with '?' in name ####
  # x <- temp[grep("\\?", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_names_correction2.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JW_names_correction2_JM.csv
x <- fread(file.path(corr_dir, "JW_names_correction2_JM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Save data table
temp2 <- data.table::copy(temp)
rm(temp)



## Combine tables ####
temp <- rbind(temp1, temp2)
setDT(temp, key = "spfile")
rm(temp1, temp2)

temp1 <- data.table::copy(temp)


## Check table and correct values ####
temp[, .N, delete_sp]
temp[is.na(delete_sp)][, .N, name_issues]
temp[is.na(delete_sp)][, .N, spell_variants]
temp[is.na(delete_sp)][, .N, name_corrections]

## Assign spell_variants as NA if delete_sp = 1
temp[!is.na(delete_sp)][!is.na(spell_variants)]$spell_variants = NA

## Assign spell_variants as 1 if !is.na(name_corrections) and delete_sp != 1
temp[is.na(delete_sp)][!is.na(name_corrections)]$spell_variants = 1

## Checks
nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])

message(cat("Number of records for which name_corrections are provided but they are not marked as spell_variants: "),
        nrow(temp[is.na(delete_sp)][is.na(spell_variants)][!is.na(name_corrections)]))

message(cat("Number of species marked as spell_variants but for which name_corrections are not provided: "),
        nrow(temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]))

temp[!is.na(delete_sp)][, .N, spell_variants]
temp[!is.na(delete_sp)][, .N, name_corrections]


## Correct values for individual species
grep("kwonkan_goongarriensis_39854", temp$name_corrections)
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]$delete_sp = NA
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]$spell_variants = 1

grep("kwonkan_goongarriensis_39854", temp$spfile)
temp[grep("kwonkan_goongarriensis_39854", temp$spfile)]


## Assign spell_variants as 1 for all name_corrections found in spfile
nrow(temp[spell_variants == 1])
for (i in unique(temp$name_corrections)){
  temp[spfile == i]$spell_variants = 1
}
nrow(temp[spell_variants == 1])

## Update name_corrections for new records found form last step
nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])
temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]

temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]$name_corrections =
  temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]$spfile

nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])
temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]


## Final checks
temp[, .N, delete_sp]
temp[is.na(delete_sp)][, .N, name_issues]
temp[is.na(delete_sp)][, .N, spell_variants]
temp[is.na(delete_sp)][, .N, name_corrections]

nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])

nrow(temp[is.na(delete_sp)][is.na(spell_variants)][!is.na(name_corrections)])
nrow(temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)])

temp[!is.na(delete_sp)][, .N, spell_variants]
temp[!is.na(delete_sp)][, .N, name_corrections]



## Add marine species information ####
sp1 <- fread(file.path(corr_dir, "invert_fireoverlap_JRM_marine.csv"))
setDT(sp1, key = "spfile")
sp1[, .N, marine]
message(cat("All marine species found in data: "),
        all(temp[spfile %in% sp1[marine == 1]$spfile]$spfile == 
              sp1[marine == 1]$spfile))
temp[spfile %in% sp1[marine == 1]$spfile]$marine = 1
rm(sp1)


## Save table ####
write.csv(temp, file.path(corr_dir, "invert_fireoverlap_corrected.csv"), 
          row.names = FALSE)


## Species list to update/remove ####
## >> Create list of species to update ####
x <- temp[spell_variants == 1]
length(unique(x$spfile))
length(unique(x$name_corrections))

x <- x[, .(spfile, scientificName, class, order, family, marine, delete_sp, name_issues, spell_variants, name_corrections)]

## Checks
unique(x$marine)
unique(x$delete_sp)
length(x$spfile) == length(unique(x$spfile))

write.csv(x, file.path(corr_dir, "update_species.csv"), row.names = FALSE)


## >> Create list of species to remove from data_ALAnonALA_wgs84.csv ####
s <- temp[marine == 1 | delete_sp == 1]$spfile
length(s)
length(unique(s))
write.csv(s, file.path(corr_dir, "delete_species_fromdata.csv"), 
          row.names = FALSE)

## >> Create list of species to remove from output table ####
s <- c(temp[marine == 1 | delete_sp == 1]$spfile, x$spfile)
length(s)
length(unique(s))
write.csv(s, file.path(corr_dir, "delete_species_fromoutputs.csv"), 
          row.names = FALSE)
rm(s, x, temp)



## Update data_ALAnonALA_wgs84.csv ####
## >> Correct data_ALAnonALA_wgs84.csv with updated species names ####
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84.csv"))
new_sp <- fread(file.path(corr_dir, "update_species.csv"))

length(new_sp$spfile)
length(unique(new_sp$name_corrections))

## Check all species are found in data
all(sort(unique(data[spfile %in% new_sp$spfile]$spfile)) == sort(new_sp$spfile))

## Add new columns to data table
data$order <- rep(character(), nrow(data))

## Change spfile in data according to name_corrections in new_sp
for (i in unique(new_sp$name_corrections)){
  data[spfile %in% new_sp[name_corrections == i]$spfile]$spfile = i
}

## Update scientificName, class, order, family information according to new_sp
for (i in unique(new_sp$name_corrections)){
  data[spfile %in% i]$scientificName = new_sp[spfile == i]$scientificName
  data[spfile %in% i]$class = new_sp[spfile == i]$class
  data[spfile %in% i]$order = new_sp[spfile == i]$order
  data[spfile %in% i]$family = new_sp[spfile == i]$family
}

## Check 
length(unique(data[spfile %in% new_sp$spfile]$spfile)) == length(unique(new_sp$name_corrections))
all(sort(length(unique(data[spfile %in% new_sp$spfile]$spfile))) == sort(length(unique(new_sp$name_corrections))))
length(unique(data[!duplicated(data[, .(spfile, class, order, family)])][spfile %in% new_sp$spfile]$spfile)) == length(unique(new_sp$name_corrections))
nrow(data[!duplicated(data[, .(spfile, class, order, family)])]) == length(unique(data$spfile))

rm(new_sp, i)

## >> Remove species from data_ALAnonALA_wgs84.csv - Part I ####
delete_sp <- fread(file.path(corr_dir, "delete_species_fromdata.csv"))$x
length(delete_sp)

length(unique(data$spfile)) - length(delete_sp)
length(unique(data[!spfile %in% delete_sp]$spfile))

dim(data); data <- data[!spfile %in% delete_sp]; dim(data)

rm(delete_sp)

## >> Remove exotic & marine species - Part II ####
exotic <- fread(file.path(corr_dir, "exotics_JM.csv"))$spfile
marine <- fread(file.path(corr_dir, "invert_fireoverlap_v02_marine_JM.csv"))
marine <- marine[, .(spfile, Marine)][Marine == 1]$spfile

dim(data); data <- data[!spfile %in% c(exotic, marine)]; dim(data)

message(cat("Number of unique species in updated data: "),
        length(unique(data$spfile)))

rm(exotic, marine)

## >> Remove duplicates ####
message(cat("Number of duplicate records: "),
        sum(duplicated(data[,c("scientificName", 
                               "latitude", 
                               "longitude")])))
data <- setDT(data)[order(-data_source), .SD[1L] ,.(scientificName, latitude, longitude)]

message(cat("Number of unique species in updated data: "),
        length(unique(data$spfile)))

## >> Apply year filter ####
## >> >> Number of records lost with year filter
t1 <- data[, .N, spfile]
t2 <- data[!is.na(year) & year >= 1990][, .N, spfile]
t <- merge(t1, t2, by = "spfile", all.x = TRUE)
dim(t)
names(t)[2:3] <- c("n.all", "n.sub")
t[which(is.na(n.sub))]$n.sub = 0
sum(is.na(t$n.sub))
setorder(t, n.sub)

## >> >> Remove records based on filter rule: 
##  if >= 3 records after filter, remove NA and <1990
##  if < 3 records after filter, keep NA and <1990
sp_applyfilter <- t[n.sub >= 3]$spfile
length(sp_applyfilter)

dim(data)
dat0 <- data[spfile %in% sp_applyfilter][!is.na(year) & year >= 1990]
dat1 <- data[!(spfile %in% sp_applyfilter)]

## Checks
sum(dat0[, .N, spfile]$N < 3)
dim(dat0)[1] + dim(dat1)[1]
length(unique(dat0$spfile)) + 
  length(unique(dat1$spfile)) == length(unique(data$spfile))

data <- rbind(dat0, dat1)
length(unique(data$spfile))
rm(t1, t2, t, dat0, dat1, sp_applyfilter)


## >> Save updated data table ####
names(data)
data <- data[, c(1:6, 12, 7:11)]
setDT(data, key = "spfile")
write.csv(data, file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"), 
          row.names = FALSE)

message(cat("Number of unique species in updated data: "),
        length(unique(data$spfile)))
message(cat("Total number of records in updated data: "),
        nrow(data))




## >> Save rds files for species ####
## ------------------------------------------------------------- ##

save_spdata2 <- function(species_uid, data, data_dir){
  
  temp <- dat[spfile == species_uid]
  spfile <- unique(temp$spfile)
  
  if (length(spfile) > 1){
    stop("Error: More than 1 unique spfile for naming species file...")
  }
  
  saveRDS(as.data.table(temp),
          file = file.path(data_dir, paste0(spfile, ".rds")))
}


## Run function in parallel
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")
if (!dir.exists(spdata_dir)) {dir.create(spdata_dir)}

errorlog <- paste0(output_dir, "/errorlog_data_ALAnonALA_corrections_", gsub("-", "", Sys.Date()), ".txt")
writeLines(c(""), errorlog)

# data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
dat <- data
all_species <- unique(dat$spfile)

plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf)

system.time(invisible(future.apply::future_lapply(all_species,
                                                  function(x){
                                                    tmp <- tryCatch(expr = save_spdata2(species_uid = x, 
                                                                                        data = dat, 
                                                                                        data_dir = spdata_dir),
                                                                    error = function(e){ 
                                                                      print(paste("\nError: More than 1 unique spfile for naming species file for...", x))
                                                                      cat(paste(x, "\n"),
                                                                          file = errorlog, 
                                                                          append = TRUE)
                                                                    })
                                                  })))

## Check files
length(list.files(spdata_dir, pattern = ".rds$"))
length(all_species)
rm(dat, save_spdata2)




## Data for sharing publicly ####
## For NESP report: avaibale at https://doi.org/10.5281/zenodo.5091296
## Renamed as ALA_invertebrate_data_clean_2020-10-28.rds
## Not yet masked, therefore many more records than in data_ALAnonALA_wgs84_corrected.csv
y <- fread(file.path(output_dir, "clean2_ala_2020-10-28.csv"))

## Subset csv to ALA data only and species not marked as sensitive (for sharing)
x <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
x <- x[sensitive != 1]
unique(x$data_source)
x <- x[data_source == "ALA"]
dim(x)
write.csv(x, file.path(output_dir, "data_invertebrates_ALAonly.csv"), 
          row.names = FALSE)

## Data summary ####
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))


## >> Summarize data by species and data source - I
sources <- fread(file.path(output_dir, "data_sources.csv")) ## file created manually based on unique data_source in data
unique(sources$description)
sources[, `original file` := NULL]

out <- data[, .N, by = data_source]
out$data_source

out <- merge(out, sources, by = "data_source")

out[, data_source := NULL]
out <- out[,c(3,2,1)]

out <- out[, .(N=sum(N)), by=description]
out <- out[order(-N)]
write.csv(out, file = file.path(output_dir, "data_bysources.csv"),
          row.names = FALSE)

## >> Summarize data by species and data source [SIMPLIFIED] - II
sources <- fread(file.path(output_dir, "data_sources.csv"))
out <- data[, .N, by = data_source]

out$data_type <- rep(character(), nrow(out))
out[data_source %in% sources[type == "state"]$data_source]$data_type = "state"
out[data_source %in% sources[type == "museum"]$data_source]$data_type = "museum"
out[data_source %in% sources[type == "ala"]$data_source]$data_type = "ala"
out[data_source %in% sources[type == "private"]$data_source]$data_type = "private"

out[, .(N=sum(N)), by=data_type]

## Note no private data type shows up, because the 37 in 'private data collection' 
## are labelled as 'state' under data_type. 
## This is because they were to be uploaded into BDBSA.


## >> Summarize data by mnumber of species, class, family, order
length(unique(data$spfile))
length(unique(data$class))

classtab <- data[, .N, class]
classtab <- classtab[order(-N)]
write.csv(classtab, file = file.path(output_dir, "data_byclass.csv"),
          row.names = FALSE)

ordertab <- data[, .N, order]
ordertab <- ordertab[order(-N)]
write.csv(ordertab, file = file.path(output_dir, "data_byorder.csv"),
          row.names = FALSE)
length(unique(data$family))

## >> Summarize data by number of records for species
counts <- data[, .N, spfile]
counts.f <- as.data.table(table(counts$N))
counts.f$V1 <- as.integer((counts.f$V1))
names(counts.f) <- c("N_records", "N_species")

counts.f$bins <- rep(character(), nrow(counts.f))
counts.f[N_records == 1]$bins = "1"
counts.f[N_records == 2]$bins = "2"
counts.f[N_records == 3]$bins = "3"
counts.f[N_records == 4]$bins = "4"
counts.f[N_records == 5]$bins = "5"
counts.f[N_records > 5 & N_records <= 10]$bins = "6-10"
counts.f[N_records > 10 & N_records <= 20]$bins = "11-20"
counts.f[N_records > 20 & N_records <= 50]$bins = "21-50"
counts.f[N_records > 50 & N_records <= 100]$bins = "51-100"
counts.f[N_records > 100 & N_records <= 500]$bins = "101-500"
counts.f[N_records > 500 ]$bins = ">500"

counts.ff <- counts.f[, .(N=sum(N_species)), by=bins]

p <- barplot(counts.ff$N, ylim = c(0, 15000), names.arg = counts.ff$bins, 
             space = 0, axes = FALSE,
             xlab = "Number of records", 
             ylab = "Number of species")
xval = seq(0, 16000, 2000)
axis(side = 2, at = xval, labels = FALSE, xpd = TRUE)
axis(side = 2, at = xval, tick = FALSE, labels = xval, xpd = TRUE)
text(p, counts.ff$N + 500*sign(counts.ff$N), labels = counts.ff$N, xpd = TRUE)

# ## For presentation
# p <- barplot(counts.ff$N, ylim = c(0, 15000), names.arg = counts.ff$bins, 
#              space = 0, axes = FALSE,
#              cex.names = 1.5, cex.lab = 1.5,
#              xlab = "Number of records", 
#              ylab = "Number of species")
# xval = seq(0, 16000, 4000)
# axis(side = 2, at = xval, labels = FALSE, xpd = TRUE)
# axis(side = 2, at = xval, tick = FALSE, labels = xval, xpd = TRUE, cex.axis = 1.5)
# text(p, counts.ff$N + 500*sign(counts.ff$N), labels = counts.ff$N, xpd = TRUE, cex = 1.5)

count1 <- counts[which(counts$N == 1)]
message(cat("Number of species with 1 record in cleaned ALA data: "),
        nrow(count1))

countLTE20 <- counts[which(counts$N > 1 & counts$N <= 20) , ]
message(cat("Number of species with more than 1 and less than or equal to 20 records in cleaned ALA data: "),
        nrow(countLTE20))

countMT20 <- counts[which(counts$N > 20)]
message(cat("Number of species with more than 20 records in cleaned ALA data: "),
        nrow(countMT20))

plot(density(counts$N))
plot(density(counts[N > 20 & N < 200, N]))
