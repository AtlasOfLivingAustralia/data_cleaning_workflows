## Fire overlap analysis function for species points

points_overlap <- function(data_rds, crs_org, crs_new, fire_severity, fire_classes, outdir){
  
  ## Load species data (cleaned/masked ALA data)                          
  points_dat <- readRDS(data_rds)
  
  # ## Drop columns - not needed for ALA-nonALA combined data workflow
  # ## Note: Here, include fields required to identify senstivie status, uncertainty, other...
  # points_dat <- points_dat[, c("scientificName", "longitude", "latitude",
  #                            "phylum", "class", "order", "family", 
  #                            "genus" , "eventDate", 
  #                            "coordinateUncertaintyInMetres", "assertions", 
  #                            "dataGeneralizationsOriginal", "sensitive", 
  #                            "spfile", "id")]
  # 
  # 
  # ## Remove duplicates from data by name-lat-long (same as ConR)
  # points_dat <- points_dat[!duplicated(points_dat[ , c("scientificName", "longitude", "latitude")]), ]
  points_dat <- as.data.frame(points_dat)
  
  ## Project data points in equal area projection
  spdf <- SpatialPointsDataFrame(coords = points_dat[c("longitude", "latitude")], data =points_dat, proj4string = CRS(crs_org))
  spdf <- spTransform(spdf, CRSobj = CRS(crs_new))
  
  ## Extract fire severity values for data points
  points_dat$FireClass <- raster::extract(fire_severity, spdf)
  
  ## Create output table with number of points within each fire class
  points_dat <- as.data.table(points_dat)
  spname <- unique(points_dat$spfile)
  
  df <- data.frame(matrix(ncol = length(fire_classes) + 2))
  colnames(df) <- c("Species", paste0("Fire_Class_", fire_classes), 
                    "Occurrence_Points")
  df[ , 1] <- spname
  df[ , 2:(ncol(df)-1)] <- sapply(fire_classes, FUN = function(x) points_dat[FireClass == x, length(FireClass)])
  df[ , ncol(df)] <- nrow(points_dat)

  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, spname), ".csv"), row.names = FALSE)
  
}


## Previously...
# df <- data.frame(matrix(ncol = length(fire_classes) + 4))
# df[ , 1] <- spname
# df[ , -c(1, (ncol(df)-2):ncol(df))] <- sapply(fire_classes, FUN = function(x) points_dat[FireClass == x, length(FireClass)])
# df[ , ncol(df)-2] <- rowSums(df[, -c(1, (ncol(df)-2):ncol(df))])
# df[ , ncol(df)-1] <- nrow(points_dat)
# df[ , ncol(df)] <- (df[ , ncol(df)-2]/df[ , ncol(df)-1])*100
# colnames(df) <- c("Species", paste0("Fire_Class_", fire_classes), 
#                   "Total_Overlap", "Occurrence_Points", "Percent_Overlap")