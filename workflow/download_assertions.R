pacman::p_load(galah, dplyr, arrow, purrr)

galah_config(email = "fonti.kar@gmail.com", 
             atlas = "Australia",
             download_reason_id = 10 # testing reason
)

y <- bind_rows(tibble(name = "id",
                      type = "field"),
               galah_select(group = "assertions"))

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
