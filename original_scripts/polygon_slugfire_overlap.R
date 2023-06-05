## Slugfire AND Fire overlap analysis function for species polygons

polygon_slugfire_overlap <- function(species_name, species_poly, shapefile_dir,
                                     fire_res, fire_crs, fire_extent,
                                     fire_vals, fire_classes, 
                                     slug_vals, slug_classes,
                                     outdir){
  
  ## Write spatial data to disk if coming from RDS file
  writeOGR(species_poly, dsn = shapefile_dir, layer = species_name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  ## Reproject species boundaries to match fire severity raster
  system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr .0025 .0025 -l ",
                species_name, " ",
                file.path(shapefile_dir, species_name), ".shp ",
                file.path(shapefile_dir, species_name), ".tif"))
  
  system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
                paste(fire_res, collapse = " "), " -te ", 
                paste(fire_extent[1], fire_extent[3], 
                      fire_extent[2], fire_extent[4]), 
                " -s_srs 'EPSG:4326' -t_srs '", fire_crs, "' ",
                file.path(shapefile_dir, species_name), ".tif ",
                file.path(shapefile_dir, species_name), "_p.tif"))
  ## extent clips out islands, but we need to do this because fire map has this extent!
  
  ## Create table of areas within each fire class
  species_map <- raster(paste0(file.path(shapefile_dir, species_name), "_p.tif"))
  
  dt <- data.table("species_map" = species_map[],
                   "fire_severity" = fire_vals,
                   "slug_severity" = slug_vals)
  
  df <- data.frame(matrix(ncol = (length(fire_classes) + length(slug_classes) + 
                                    length(fire_classes) * length(slug_classes)) + 2))
  y <- expand.grid(slug_classes, fire_classes)
  colnames(df) <- c("Species", paste0("Slug_Class_", slug_classes),
                    paste0("Fire_Class_", fire_classes),
                    paste0("SlugFire_Class_", sort(paste0(y[,1], y[,2]))),
                    "Species_Polygon")
  
  df[, 1] <- species_name
  df[, 2:3] <- sapply(slug_classes, 
                      FUN = function(x) dt[species_map == 1 & 
                                             slug_severity == x, 
                                           length(slug_severity) * 250 * 250 / 1000000])
  df[ , 4:8] <- sapply(fire_classes,
                       FUN = function(x) dt[species_map == 1 & 
                                              fire_severity == x, 
                                            length(fire_severity) * 250 * 250 / 1000000])
  df[ , 9:13] <- sapply(fire_classes, 
                        FUN = function(x) dt[species_map == 1 & 
                                               slug_severity == 1 & 
                                               fire_severity == x, 
                                             length(fire_severity) * 250 * 250 / 1000000])
  
  df[ , 14:18] <- sapply(fire_classes, 
                         FUN = function(x) dt[species_map == 1 & 
                                                slug_severity == 2 & 
                                                fire_severity == x, 
                                              length(fire_severity) * 250 * 250 / 1000000])
  
  df[, ncol(df)] <- dt[species_map == 1, length(species_map)* 250 * 250 / 1000000]
  
  ## Remove files
  file.remove(file.path(shapefile_dir, dir(path = shapefile_dir)))
  
  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, species_name), ".csv"), row.names = FALSE)
  
}


# ## To include species that are on islands, and lie off the fire map
# ## To use this we woudl need to extend the extent of the fire map
# system(paste0("gdalwarp -overwrite -ot Byte -te -4493436.0 -6839453.5 3234148.7 -955558.6 -tr 250 250 -s_srs 'EPSG:4326' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ",
#               file.path(shapefile_dir, species_name), ".tif ",
#               file.path(shapefile_dir, species_name), "_p.tif"))
# ## extent including islands, but we cannot use this because fire map will have different extent


