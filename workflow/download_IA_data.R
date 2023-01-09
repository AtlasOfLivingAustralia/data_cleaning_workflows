pacman::p_load(galah, dplyr, arrow, purrr)

galah_config(email = "fonti.kar@gmail.com", 
             atlas = "Australia",
             download_reason_id = 10 # testing reason
)

# Fields that IA want
y <- tibble(name = 
              c("id",
                "dataResourceUid",
                "dataResourceName",
                "institutionID",
                "institutionName",
                "collectionID", 
                "collectionUid",
                "collectionName",
                "contentTypes", 
                "license", 
                "taxonConceptID",
                "raw_scientificName",
                "raw_vernacularName", 
                "scientificName", 
                "vernacularName", 
                "taxonRank", 
                "kingdom",
                "phylum",
                "class",
                "order",
                "family",
                "genus",
                "species",
                "subspecies",
                "institutionCode",
                "collectionCode",
                "locality", 
                "raw_geodeticDatum", 
                "raw_decimalLatitude",
                "raw_decimalLongitude", 
                "decimalLatitude",
                "decimalLongitude",
                "coordinatePrecision",
                "coordinateUncertaintyInMeters",
                "country",
                "stateProvince", 
                "cl959",
                "cl21",
                "cl1048",
                "minimumElevationInMeters", 
                "maximumElevationInMeters", 
                "minimumDepthInMeters",
                "maximumDepthInMeters",
                "individualCount",
                "recordedBy",
                "eventDate",
                "year",
                "month",
                "verbatimEventDate",
                "basisOfRecord",
                "raw_basisOfRecord",
                "occurrenceStatus",
                "raw_sex", 
                "sex",
                "preparations",
                "outlierLayer",
                "spatiallyValid", 
                "catalogNumber"),
            type = "field") 

# Set the call as galah_select
attr(y, "call") <- "galah_select"

# Split fields into 3 parts for download
y_ls <- split(y, f = ceiling(seq_along(y$name)/20))

# Add id in 2nd and 3rd list so output can all be joined later by id
y_ls$`2` <- bind_rows(tibble(name = "id",
                             type = "field"),
                      y_ls$`2`)

y_ls$`3` <- bind_rows(tibble(name = "id",
                             type = "field"),
                      y_ls$`3`)

# To get all the assertions
y_ls$`4` <- bind_rows(tibble(name = "id",
                             type = "field"),
                      galah_select(group = "assertions"))

# The rest of the galah_call as a function
get_occ <- function(y){
  
  # Phylums
  afd_taxon_phylums <- c("ANNELIDA",
                         "ARTHROPODA",
                         "BRACHIOPODA",
                         "BRYOZOA",
                         "CHAETOGNATHA",
                         "GASTROTRICHA",
                         "GNATHOSTOMULIDA",
                         "MOLLUSCA",       
                         "NEMATODA",        
                         "NEMATOMORPHA",    
                         "NEMERTEA",       
                         "ONYCHOPHORA",    
                         "PLATYHELMINTHES",
                         "PORIFERA",        
                         "ROTIFERA",        
                         "TARDIGRADA")
  
  # basisOfRecord filters
  specimen_only <- c("PRESERVED_SPECIMEN", "LIVING_SPECIMEN", 
                     "MACHINE_OBSERVATION", "MATERIAL_SAMPLE")
  x <- 
    galah_call() |>
    galah_identify(afd_taxon_phylums) %>% 
    galah_identify("https://biodiversity.org.au/afd/taxa/3cbb537e-ab39-4d85-864e-76cd6b6d6572", 
                   search = FALSE) |>
    galah_filter(basisOfRecord == specimen_only, 
                 identificationIncorrect == FALSE)
  
  x$select <- y
  
  x |> 
    atlas_occurrences() |> 
    arrow::write_parquet(sink = paste0("data/galah/IA_", Sys.time()))
  
  Sys.sleep(600)
}


map(y_ls,
    get_occ)

