# install.packages("pacmac")
pacman::p_load(data.table, galah, dplyr, tictoc)

galah_config(email = "fonti.kar@gmail.com", 
             atlas = "Australia",
             download_reason_id = 10 # testing reason
)

afd_taxonomy <- fread("output/afd_splist_full.csv")
afd_taxon <- unique(afd_taxonomy$PHYLUM) 

# Current workaround to get taxa in AFD list
afd_taxon_phylums <- afd_taxon[-which(afd_taxon == "ACANTHOCEPHALA")]

specimen_only <- c("PRESERVED_SPECIMEN", "LIVING_SPECIMEN", 
                   "MACHINE_OBSERVATION", "MATERIAL_SAMPLE")

# Fields that IA want
IA_fields<- c("id","dataResourceUid","dataResourceName",
              "institutionID","institutionName",
              "collectionID", "collectionUid","collectionName",
              "contentTypes", 
              "license", 
              "taxonConceptID",
              "raw_scientificName" ,"raw_vernacularName", 
              "scientificName", 
              "vernacularName", 
              "taxonRank", 
              "kingdom","phylum","class","order", 
              "family","genus","species","subspecies",
              "institutionCode","collectionCode",
              "locality", 
              "raw_geodeticDatum", 
              "raw_decimalLatitude","raw_decimalLongitude", 
              "decimalLatitude","decimalLongitude",
              "coordinatePrecision","coordinateUncertaintyInMeters",
              "country","stateProvince", 
              "cl959","cl21","cl1048",
              "minimumElevationInMeters", "maximumElevationInMeters", 
              "minimumDepthInMeters", "maximumDepthInMeters",
              "individualCount",
              "recordedBy",
              "eventDate",
              "year","month",
              "verbatimEventDate",
              "basisOfRecord","raw_basisOfRecord",
              "occurrenceStatus",
              "raw_sex", "sex",
              "preparations",
              "outlierLayer",
              "spatiallyValid", "catalogNumber") 

length(IA_fields)

# Split into 2 parts
IA_fields_split <- split(IA_fields, ceiling(seq_along(IA_fields)/29))

# We want to eventually join by recordID so need to add this to the 2-4 parts
IA_fields_split$`2` <- c("id", IA_fields_split$`2`)

IA_fields_split$`2`[1:20] 




tic("Full invertebrates galah download")
galah_call() %>% 
  galah_identify(afd_taxon_phylums) %>% 
  galah_identify("https://biodiversity.org.au/afd/taxa/3cbb537e-ab39-4d85-864e-76cd6b6d6572", search = FALSE) %>% 
  galah_filter(basisOfRecord == specimen_only) %>% 
  galah_select(all_of(IA_fields_split$`1`)) %>%
  atlas_occurrences() -> invert_p1

saveRDS(invert_p1, "output/rds/invert_p1")

galah_call() %>% 
  galah_identify(afd_taxon_phylums) %>% 
  galah_identify("https://biodiversity.org.au/afd/taxa/3cbb537e-ab39-4d85-864e-76cd6b6d6572", search = FALSE) %>% 
  galah_filter(basisOfRecord == specimen_only) %>% 
galah_select(all_of(IA_fields_split$`2`[1:20])) %>% 
  atlas_occurrences() -> invert_p2

saveRDS(invert_p1, "output/rds/invert_p2")

galah_call() %>% 
  galah_identify(afd_taxon_phylums) %>% 
  galah_identify("https://biodiversity.org.au/afd/taxa/3cbb537e-ab39-4d85-864e-76cd6b6d6572", search = FALSE) %>% 
  galah_filter(basisOfRecord == specimen_only) %>% 
  galah_select(all_of(c("id", IA_fields_split$`2`[21:length(IA_fields_split$`2`)]))) %>% 
  atlas_occurrences() -> invert_p3

saveRDS(invert_p1, "output/rds/invert_p3")
toc()
