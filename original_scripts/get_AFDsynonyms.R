#' Get synonyms for species names from AFD checklist
#'
#' @author Payal Bal
#' 
#' @param species Scientific name of species, can inlcude subspecies
#' @param checklist AFD taxonomic cheklist 
#' @return list
#'
#' @examples

# test <- ...



get_AFDsynonyms <- function(species, checklist) {
  
  ## Define output object
  out <- list()
  
  for (i in species) {
    
    message("Looking for ", i, "in AFD checklist...")
    
    ## Look for entire name in synonyms
    z <- checklist[grep(i, checklist$SYNONYMS)]$SYNONYMS
    if(length(z)==0){
      message("Not found in AFD checklist")
      fullname_synonyms <- NA
    } else{
      message("****** Full name found in AFD checklist *******")
      fullname_synonyms <- paste(z, sep="", collapse="; ")
    }
    
    ## Look at synonyms for genus only
    x <- grep(word(i,-1), checklist[grep(word(i,1), checklist$VALID_NAME)]$SYNONYMS, value = TRUE)
    
    if (length(x) == 0) {
      message(cat("Genus", word(i,1), "not found in AFD synonyms"))
      genus_synonyms <- NA
    } else {
      message(cat("Genus", word(i,1), "found in AFD synonyms *******"))
      genus_synonyms <- paste(x, sep="", collapse="; ")
    }
    
    ## Look at synonyms for species only
    y <- grep(word(i,1), checklist[grep(word(i,-1), checklist$SYNONYMS)]$SYNONYMS, value = TRUE)
    
    if (length(y) == 0) {
      message(cat("Species", word(i,-1), "not found in AFD synonyms"))
      species_synonyms <- NA
    } else {
      message(cat("Species", word(i,-1), "found in AFD synonyms *******"))
      species_synonyms <- paste(y, sep="", collapse="; ")
    }
    temp <- list(fullname_synonyms = fullname_synonyms, 
                 genus_synonyms = genus_synonyms, 
                 species_synonyms = species_synonyms)
    
    out[[i]] <- temp
  }
  
  return(out)
}