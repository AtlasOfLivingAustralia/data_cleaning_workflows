## Regional overlap function for species data points
## Collaborator: Casey Visintin

region_overlap <- function(data_rds, crs_org, crs_new, region_raster, region_classes, outdir){
  
  ## Load species data (cleaned/masked ALA data)                          
  points_dat <- readRDS(data_rds)
  points_dat <- as.data.frame(points_dat)
  
  ## Project data points in equal area projection
  spdf <- SpatialPointsDataFrame(coords = points_dat[c("longitude", "latitude")], data =points_dat, proj4string = CRS(crs_org))
  spdf <- spTransform(spdf, CRSobj = CRS(crs_new))
  
  ## Extract region values for data points
  points_dat$RegionClass <- raster::extract(region_raster, spdf)
  
  ## Create output table with number of points within each region class
  points_dat <- as.data.table(points_dat)
  spname <- unique(points_dat$spfile)
  
  df <- data.frame(matrix(ncol = length(region_classes) + 1))
  df[ , 1] <- spname
  df[, 2:(ncol(df))] <- sapply(region_classes, FUN = function(x) points_dat[RegionClass == x, length(RegionClass)])
  colnames(df) <- c("Species", paste0("Region_Class_", region_classes))
  
  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, spname), ".csv"), row.names = FALSE)
  
}




# ## If with fire...
# dt <- data.table("species_map" = species_map[],
#                  "boundary_map" = region_map[],
#                  "fire_severity" = fire_vals)
# 
# df <- data.frame(matrix(ncol = length(fire_classes) + 4))
# df[ , 1] <- species_name
# df[ , -c(1, (ncol(df)-2):ncol(df))] <- sapply(fire_classes, FUN = function(x) dt[species_map == 1 & boundary_map == 1 & fire_severity == x, length(fire_severity) * 250 * 250 / 1000000])
# df[ , ncol(df)-2] <- rowSums(df[, -c(1, (ncol(df)-2):ncol(df))])
# df[ , ncol(df)-1] <- dt[species_map == 1, length(species_map)* 250 * 250 / 1000000]
# df[ , ncol(df)] <- (df[ , ncol(df)-2]/df[ , ncol(df)-1])*100
# colnames(df) <- c("Species", paste0("Fire_Class_", fire_classes), "Total_Overlap", "Species_Polygon", "Percent_Overlap")