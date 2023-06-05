#' Load ALA data
#'
#' @author Payal Bal
#' 
#' @param taxon
#' @param get_counts_only only get number of data point, do bot download data
#' @param fields ...
#' @param include_assertions see: ala_fields("assertions",as_is=TRUE)
#' @param BoR_filters basis of record filters - https://dwc.tdwg.org/list/#dwc_basisOfRecord + categories listed in https://biocache.ala.org.au/#tab_advanceSearch
#' @param dst
#' @param email
#' @return
#' @export
#'
#' @examples

# testALA <- ALA4R::occurrences(taxon = "text:\"...\"", download_reason_id = 5, email = "...")


get_ala_taxondata <- function(taxon,
                              get_counts_only = FALSE,
                              fields,
                              include_assertions = FALSE,
                              specimens_only = TRUE,
                              dst = NULL,
                              email #needed for ALA4R 'offline' download
                              # generate_doi = TRUE
){
  
  
  ## Define function parameters ####
  ## Specimen only filters
  BoRfilters <-  c("PreservedSpecimen", "LivingSpecimen", 
                   "MachineObservation", "EnvironmentalDNA",
                   "GenomicDNA")
  
  ## Extra DWC fields for download
  ## https://biocache.ala.org.au/ws/index/fields
  ##  Note: qa = “all" preferred to extra = "assertions" 
  ##  because latter only lists one assertion, even if the record has multiple
  dwc_fields <- c("el1055", "el1056", #endemism
                  "el682", #migratory
                  "disposition", 
                  "assertions","assertions_unchecked", 
                  "raw_data_generalizations",
                  "duplicate_record", "duplicate_status",
                  "sensitive","taxonomic_issue")
  
  ## Remove trailing whitespace
  taxon <- stringr::str_squish(taxon)
  
  
  ## Count records in ALA ####
  n.all <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon), #includes synonyms
                              record_count_only = TRUE)
  
  n.obs <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon),
                              record_count_only = TRUE,
                              fq = "basis_of_record:HumanObservation")
  
  
  n.spec <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon),
                               record_count_only = TRUE,
                               fq = paste0("basis_of_record:", 
                                           BoRfilters, collapse = " OR "))
  
  n.ala <- c(all = n.all, observation = n.obs,  specimen =  n.spec)
  
  ## Download records from ALA ####
  if (get_counts_only == FALSE) {
    
    rm(n.ala)
    
    ## Data destination directory
    if(is.null(dst)){
      stop("Warning: Destination directory for ALA data not specified...\n")
    } else {
      if(!dir.exists(dst)){
        dir.create(dst)
      } 
    }
    
    if (specimens_only == TRUE) {
      
      if (include_assertions == TRUE) {
        occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon),
                                      download_reason_id = 5, 
                                      email = email,
                                      fq = paste0("basis_of_record:", 
                                                  BoRfilters, collapse = " OR "),
                                      qa = "all",
                                      fields = fields,
                                      extra = dwc_fields)$data
      } else {
        occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon),
                                      download_reason_id = 5, 
                                      email = email,
                                      fq = paste0("basis_of_record:", 
                                                  BoRfilters, collapse = " OR "),
                                      qa = "none",
                                      fields = fields,
                                      extra = dwc_fields)$data
      }
      
    } else {
      if (include_assertions == TRUE) {
        occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon),
                                      download_reason_id = 5, 
                                      email = email,
                                      qa = "all",
                                      fields = fields,
                                      extra = dwc_fields)$data
      } else {
        occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', taxon),
                                      download_reason_id = 5, 
                                      email = email,
                                      qa = "none",
                                      fields = fields,
                                      extra = dwc_fields)$data
      }
    }
    
    ## If no data is returned from ALA, terminate function
    if(nrow(occ_ala) == 0){
      stop(paste("\nNot run: no records found for", taxon, "in ALA"))
    }
    
    ## Convert eventDate to a date object
    if(nrow(occ_ala) > 0){
      occ_ala$eventDate <- lubridate::as_date(occ_ala$eventDate)
    }
    
    ## Processindg downloaded data ####
    occ_ala <- data.table::as.data.table(occ_ala)
    
    ## Convert lat long colums to numeric
    occ_ala$longitude <- as.numeric(occ_ala$longitude)
    occ_ala$latitude <- as.numeric(occ_ala$latitude)
    
    ## Get rid of missing or incomplete long and lats
    occ_ala <- na.omit(occ_ala, cols = c("latitude", "longitude"))
    
    ## Check if any record left
    if(nrow(occ_ala) == 0){
      stop(paste("Not run: No data with legitimate coordinates found for", taxon))
    }
    
    ## Count records in cleaned data
    n.clean <- nrow(occ_ala)
    n.ala <- c(all = n.all, observation = n.obs,  specimen =  n.spec, cleaned = n.clean)
    
    ## Save results ####
    saveRDS((list(data = occ_ala,
                  counts = n.ala)),
            file = file.path(dst, 
                             paste0(gsub(" ", "_", tolower(taxon)), ".rds")))
    
    ## Clear workspace
    rm(n.ala, n.clean, occ_ala)
    
  } else {
    
    ## Save results ####
    return(list(counts = n.ala))
    
    ## Clear workspace
    rm(n.ala)
    
  }
  
}


## Default ALA fields: 
# fields = c("id","data_resource_uid","data_resource",
#            "institution_uid","institution_name",
#            "collection_uid","collection_name",
#            "license","catalogue_number",
#            "taxon_concept_lsid",
#            "raw_taxon_name","raw_common_name",
#            "taxon_name","common_name","rank",
#            "kingdom","phylum","class","order",
#            "family","genus","species","subspecies",
#            "institution_code","collection_code",
#            "raw_locality","raw_datum",
#            "raw_latitude","raw_longitude",
#            "latitude","longitude",
#            "coordinate_precision","coordinate_uncertainty",
#            "country","state","cl959","cl21","cl1048",
#            "min_elevation_d","max_elevation_d",
#            "min_depth_d","max_depth_d",
#            "individual_count","collector",
#            "occurrence_date","year","month",
#            "verbatim_event_date",
#            "basis_of_record","raw_basis_of_record",
#            "occurrence_status",
#            "raw_sex","preparations",
#            "outlier_layer",
#            "taxonomic_kosher","geospatial_kosher")