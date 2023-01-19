pacman::p_load(tidyverse, worrms, stringr)

afd_taxonomy <- read_csv("output/afd_species_clean.csv")              

## Format the data to use thier webtool for taxon match https://www.marinespecies.org/aphia.php?p=match
# This solution is preferred as you can query at much larger scales and all we need is a species list
# Instructions here: https://www.marinespecies.org/tutorial_taxonmatch.php

names(afd_taxonomy)

afd_taxonomy |> 
  select(FAMILY, GENUS, SPECIES, VALID_NAME, AUTHOR) |> 
  mutate(full_name = paste0(GENUS, " ", SPECIES)) -> afd_worms

# When VALID NAME doesn't match full species name 
bad_match <- afd_worms$VALID_NAME == afd_worms$full_name
afd_worms[which(bad_match == FALSE), ] -> bad_match_afd

# Species with numerous words in name or brackets in name
str_count(bad_match_afd$VALID_NAME, pattern = "\\S+") |> janitor::tabyl()
str_count(bad_match_afd$VALID_NAME, pattern = "\\(") |> janitor::tabyl()

# Just follow guidelines for now
afd_taxonomy |> 
  select(FAMILY, GENUS, SPECIES) |> 
  mutate(ScientificName = paste0(GENUS, " ", SPECIES))-> worms_taxmatch


n <- 1500
nr <- nrow(worms_taxmatch)
ind <- rep(1:ceiling(nr/n), each=n, length.out=nr)
ind |> janitor::tabyl()
split(worms_taxmatch, rep(1:ceiling(nr/n), each=n, length.out=nr)) -> split_worms

# Save each as a .CSV
names(split_worms) <- paste0("afd", "_", names(split_worms), ".csv")

for(i in 1:length(split_worms)){
  # write.table(split_worms[[i]], paste0("output/", names(split_worms)[[i]]), 
  #           row.names = FALSE, 
  #           quote = FALSE, 
  #           eol = "\r\n",
  #           sep = ",")
  write.csv(split_worms[[i]], paste0("output/", names(split_worms)[[i]]),
            row.names = FALSE, 
            quote = FALSE
  )
}



## Code for for loop to query their API - slow, avoid
afd_species <- unique(afd_taxonomy$VALID_NAME) 

afd_species_chunks <- split(afd_species, ceiling(seq_along(afd_species)/120))

for (i in 1:length(afd_species_chunks)) {
  
message(cat("Working on", names(afd_species_chunks)[i], "/", length(afd_species_chunks)))
  
try(worrms::wm_records_names(afd_species_chunks[[i]]) -> tmp)
    
    tmp |> bind_rows() -> afd_marine
    
    if(nrow(afd_marine) > 0){
    filenm <- paste0("output/rds/afd_marine", "_", names(afd_species_chunks)[i], ".rds")
  
    saveRDS(afd_marine, filenm)
  }
  
}

# Working on 39, 40, 41, 43, 44, 47, 50, 51, 53, 54, 55, 56
# Error: (204) No Content - AphiaRecordsByNames 
#https://www.marinespecies.org/rest/  API link 204 = nothing found

