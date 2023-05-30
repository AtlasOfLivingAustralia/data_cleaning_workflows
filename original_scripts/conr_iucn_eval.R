## ConR::IUCN.eval function for parallel processing

conr_iucn_eval <- function(species_filename, 
                           hull.method,
                           exclude.by.map = TRUE,
                           basemap_path, working_dir, iucn_outpath) {
  
  ## Set wd for IUCN-eval function
  setwd(working_dir)
  
  ## Read species data
  dat <- as.data.table(readRDS(species_filename))
  spname <- unique(dat$spfile)
  dat <- dat[ , .(latitude, longitude, scientificName, family, year)]
  names(dat) <- c("ddlat", "ddlon", "tax", "family", "coly")
  
  ## Load basemap
  basemap <- readOGR(basemap_path, verbose = FALSE)
  
  ## Run ConR function
  out <- IUCN.eval(dat, 
                   method.range = hull.method,
                   alpha = 2, 
                   Cell_size_AOO = 2,
                   Cell_size_locations = 2,
                   country_map = basemap,
                   exclude.area = exclude.by.map,
                   write_file_option = "csv", 
                   file_name = spname,
                   export_shp = TRUE, ## to get SpatialPolygonsDataFrame in output
                   write_results = TRUE,
                   write_shp = FALSE, ## to write shapefile files to folder
                   SubPop = FALSE,
                   DrawMap = FALSE,
                   showWarnings = FALSE)
  
  ## Save output per species
  saveRDS(out, file = paste0(iucn_outpath, "/", spname, ".rds"))
  
  # if (write_shp == TRUE) {
  #   if(!is.null(out[[1]][[2]])) {
  #     ## Write shapefiles
  #     ## Note: Functionality in IUCN.eval(..., write_shp = TRUE) gives errors
  #     writeOGR(out[[1]][[2]], dsn = file.path(working_dir, "shapesIUCN"),
  #              layer= spname, driver="ESRI Shapefile")
  #   }
  # }
  
  
  ## Clear objects
  rm(dat, spname, out)
}
