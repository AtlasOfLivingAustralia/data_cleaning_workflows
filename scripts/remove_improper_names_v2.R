#' Remove improper names using stringr
#'
#' @param name_vector vector of species names
#' @param allow.higher.taxa logical, if TRUE species with single word names are excluded as they are likely within species suffix
#' @param improper.species.list logical, if TRUE a list of improper species will be returned
#' @details This function 1)  removes trailing white spaces, 2) flags and removes specis with specific taxonomic modifiers, 
#' text, symbol, formatting patterns in their name (improper species), 3) flags and removes species that one have one word in their name (incomplete species) 
#' 
#' @return list containing improper species, updated species list excluding improper species and incomplete species list

remove_improper_names_v2 <- function(name_vector,
                                     allow.higher.taxa = FALSE,
                                     allow.subspecies = TRUE,
                                     improper.species.list = TRUE){
  
  message("Cleaning checklist for improper species names...")
  
  ## Check if name_vector is in expected format
  if (!is.character(name_vector)){
    stop("name_vector is not a character vector")
  }
  
  ## Clean trailing and double spaces
  name_vector_raw <- name_vector <- stringr::str_squish(name_vector) # I notice name_vector_raw is being made but not used in flagging improper names
  
  originalN <- length(name_vector)
  
  ## Tax modifiers, text, symbol, formatting patterns that we want to flag/remove from final list
  regex_pattern <- c("Unplaced", 
                     "\\?",
                     "\\sex\\s",
                     "sp\\.", 
                     "Sp\\.", 
                     "spp\\.",
                     "aff\\.",
                     "cf\\.", 
                     "indet\\.", 
                     "indet\\s", 
                     "\\ssp$",
                     "aff\\s",
                     "\\ssp\\s",
                     "cf\\s", 
                     "\\sor\\s", 
                     "\\[", 
                     "sensu",
                     "species$",
                     "sens\\.", 
                     "-$", 
                     "\\s-\\s", 
                     "/", 
                     "s\\.", 
                     "etc\\.",
                     "\\sx$", 
                     "Unidentifi",
                     "spec\\.", 
                     "affin\\.", 
                     "species\\s",
                     "taxon", 
                     "spec\\.nov\\.",
                     "cf\\,", 
                     "\\sand\\s",
                     "\\swith\\s")
  
  ## Record improper species names as identified by taxa modifiers or particular text patterns
  if (improper.species.list){
    improper_species <- stringr::str_subset(string = name_vector, 
                                            pattern = stringr::regex(paste(regex_pattern, collapse = "|"))) 
  }
  
  ##  Remove all names with taxa modifiers (this list keeps subspecies and variety)
  name_vector_proper <- stringr::str_subset(string = name_vector, 
                                            pattern = stringr::regex(paste(regex_pattern, collapse = "|")),
                                            negate = TRUE) # Setting this as true it will return non-matching names
  
  ## Record names with less than or equal to one word
  incomplete_nms <- stringr::str_count(string = name_vector_proper, pattern = "\\w+") <= 1
  
  sp_incomplete <- name_vector_proper[incomplete_nms]
  
  ## [Optional] Remove single word names (likely higher taxa without species suffix)
  if (! allow.higher.taxa){
    ##  set all names without a space to NA, which should work because we fixed double and trailing space
    # name_vector[grep(" ", name_vector, fixed = TRUE, invert = TRUE)] <- NA
    
    ##  set names with less than or equal to one word as NA
    name_vector_proper[-incomplete_nms] 
  }
  
  ## record number of records with less than or equal to one word
  sp_incomplete_n <- length(sp_incomplete)
  
  if (length(improper_species) != 0){
    message(cat("# Improper names (indicated by NAs) in updated species list: "),
            length(improper_species))
    
    ## Print messages on screen
    message(cat("Original number of species: "),
            originalN)
    message(cat("Number of species removed: "),
            length(improper_species) + sp_incomplete_n)
    message(cat("Number of species retained: "),
            length(name_vector_proper))
    message(cat("Proprotion of species removed: "),
            (length(improper_species) + sp_incomplete_n)/originalN)
    message(cat("Is #species retained = #species in raw list - #species removed? : "),
            length(name_vector_proper) == (length(name_vector_raw) -
                                             (length(improper_species)+ sp_incomplete_n)))
    
  }else {
    message("No improper species found in checklist!")
  }
  
  return(list(updated_list = name_vector_proper,
              improper_species = improper_species,
              incomplete_species = sp_incomplete))
}