## Processing for regional layers - to run once only


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos")
lapply(x, require, character.only = TRUE)
rm(x)

bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

if(!dir.exists(file.path(bugs_dir, "outputs", "regions"))){
  dir.create(file.path(bugs_dir, "outputs", "regions"))
}


## Bushfire recovery region polygons ####
## Source: Fiona Woods, DAWE
## Create id field by row_number in QGIS
recovery_poly <- readOGR(file.path(bugs_dir, 
                                   "Bushfire_recovery_regions", 
                                   "regions.shp"),
                         layer = "regions")

## Reproject regions to match fire severity raster
## NOTE: Cannot do this in one step with gdal_rasterize (see commeted section below)
infile <- file.path(bugs_dir, "Bushfire_recovery_regions", "regions.shp")
outfile <- file.path(output_dir, "regions", "bushfire_recovery.tif")
system(paste0("gdal_rasterize -at -a id -ot Byte -tr .0025 .0025 -l regions ",
              infile, " ", outfile))

infile <- outfile
outfile <- gsub(".tif", "_p.tif", outfile)
  ##  !! BEWARE OF HARDCODED EXTENT, RES I& CRS N SYSTEM CALL !!
system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -s_srs 'EPSG:4326' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ",
              infile, " ", outfile))
gdalinfo(outfile)
sort(unique(raster(outfile)[]))

raster(outfile)
region_vals <- raster(outfile)[]
region_classes <- sort(unique(na.omit(region_vals)))

region_names <-   data.frame(matrix(nrow = length(region_classes), ncol = 2))
names(region_names) <- c("code", "name")
region_names$code <- 1:7
region_names$name <- c("Australian Alps", "East Gippsland", 
                       "Greater Blue Mountains", "Kangaroo Island", 
                       "North Coast and Tablelands", "NSW South Coast", 
                       "South East Queensland")
write.csv(region_names, 
          file = file.path(output_dir, "bushfire_recregions_names.csv"),
          row.names = FALSE)

  ##  !! BEWARE OF HARDCODED EXTENT, RES I& CRS N SYSTEM CALL !!
# system(paste0("gdal_rasterize -at -a id -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -a_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -l regions ",
#               infile, " ", outfile))
# ## rdal_rasterise page says: Note that on the fly reprojection of vector data to the coordinate system of the raster data is only supported since GDAL 2.1.0.


## State polygons ####
## Source: GEOCOAST...
## Create id field by unique(STATE) in QGIS
state_poly <- readOGR(file.path(output_dir, "masks", "auslands_wgs84.shp"), 
                      layer = "auslands_wgs84") 

## Reproject regions to match fire severity raster
## NOTE: Cannot do this in one step eith gdal_rasterize (see commneted section)
infile <- file.path(output_dir, "masks", "auslands_wgs84.shp")
outfile <- file.path(output_dir, "regions", "auslands_wgs84.tif")
system(paste0("gdal_rasterize -at -a id_state -ot Byte -tr .0025 .0025 -l auslands_wgs84 ",
              infile, " ", outfile))

infile <- outfile
outfile <- gsub(".tif", "_p.tif", outfile)

  ##  !! BEWARE OF HARDCODED EXTENT, RES I& CRS N SYSTEM CALL !!
system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -s_srs 'EPSG:4326' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ",
              infile, " ", outfile))

raster(outfile)
region_vals <- raster(outfile)[]
region_classes <- sort(unique(na.omit(region_vals)))

region_names <-   data.frame(matrix(nrow = length(region_classes), ncol = 2))
names(region_names) <- c("code", "name")
region_names$code <- region_classes
region_names$name <- c('N/A', 'ACT', 'NSW', 'VIC', 
                       'NT', 'QLD', 'SA', 'TAS', 
                       'WA', 'AET', 'JBT')
write.csv(region_names, 
          file = file.path(output_dir, "state_names.csv"), 
          row.names = FALSE)

## Clip regions to Preliminary analysis area
## Note: See firmap.R for PAA processing
infile <- outfile
outfile <- gsub(".tif", "_paa.tif", outfile)
maskfile <- file.path(output_dir, "fire", "prelim_analysis_areas_dissolve_eqar.shp") 

system(paste0("gdalwarp -of GTiff -cutline ", maskfile, " -cl prelim_analysis_areas_dissolve_eqar -crop_to_cutline ", infile, " ", outfile))

gdalinfo(outfile)
sort(unique(raster(outfile)[]))
region_classes <- sort(unique(na.omit(region_vals)))

region_names <-   data.frame(matrix(nrow = length(region_classes), ncol = 2))
names(region_names) <- c("code", "name")
region_names$code <- region_classes
region_names$name <- c('N/A', 'ACT', 'NSW', 'VIC', 
                       'QLD', 'SA', 'TAS', 
                       'WA', 'AET', 'JBT')
write.csv(region_names, 
          file = file.path(output_dir, "state_paa_names.csv"), 
          row.names = FALSE)
