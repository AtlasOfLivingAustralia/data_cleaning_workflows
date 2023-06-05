## Slugfire AND Fire overlap analysis function for species points

points_slugfire_overlap <- function(data_rds, crs_org, crs_new, fire_severity, 
                                    fire_classes, slug_severity, slug_classes,
                                    outdir){
  
  ## Load species data (cleaned/masked ALA data)                          
  points_dat <- as.data.frame(readRDS(data_rds))
  
  ## Project data points in equal area projection
  spdf <- SpatialPointsDataFrame(coords = points_dat[c("longitude", "latitude")], data =points_dat, proj4string = CRS(crs_org))
  spdf <- spTransform(spdf, CRSobj = CRS(crs_new))
  
  ## Extract fire severity values for data points
  points_dat$FireClass <- raster::extract(fire_severity, spdf)
  
  ## Extract slug severity values for data points
  points_dat$SlugClass <- raster::extract(slug_severity, spdf)
  
  ## Create output table with number of points within each fire class
  points_dat <- as.data.table(points_dat)
  spname <- unique(points_dat$spfile)
  
  df <- data.frame(matrix(ncol = (length(fire_classes) + length(slug_classes) + 
                                    length(fire_classes) * length(slug_classes)) + 2))
  y <- expand.grid(slug_classes, fire_classes)
  colnames(df) <- c("Species", paste0("Slug_Class_", slug_classes),
                    paste0("Fire_Class_", fire_classes),
                    paste0("SlugFire_Class_", sort(paste0(y[,1], y[,2]))),
                    "Occurrence_Points")
  df[ , 1] <- spname
  df[ , 2:3] <- sapply(slug_classes, FUN = function(x) points_dat[SlugClass == x, length(SlugClass)])
  df[ , 4:8] <- sapply(fire_classes, FUN = function(x) points_dat[FireClass == x, length(FireClass)])
  df[ , 9:13] <- sapply(fire_classes, 
                        FUN = function(x) points_dat[SlugClass == 1 & FireClass == x, length(FireClass)])
  df[ , 14:18] <- sapply(fire_classes, 
                         FUN = function(x) points_dat[SlugClass == 2 & FireClass == x, length(FireClass)])
  df[ , ncol(df)] <- nrow(points_dat)
  
  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, spname), ".csv"), row.names = FALSE)
  
}
