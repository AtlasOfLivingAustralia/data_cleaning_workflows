## Reclassified fire map

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rgdal", "gdalUtils", "rgeos", "usethis")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

source(file.path("/tempdata/workdir/nesp_bugs/", "scripts", "gdal_calc.R"))  # by jgarber

if(!dir.exists(file.path(bugs_dir, "outputs", "fire"))){
  dir.create(file.path(bugs_dir, "outputs", "fire"))
}

if(!dir.exists(file.path(bugs_dir, "outputs", "native_vegetation"))){
  dir.create(file.path(bugs_dir, "outputs", "native_vegetation"))
}

## Load mask
ausmask <- raster(file.path(output_dir, "masks", "ausmask_noaa_250mAlbersEA_NA.tif"))
aus.crs <- crs(ausmask)
aus.res <- res(ausmask) ## 250m res



## Preliminnary analysis area ####
## Source: https://www.environment.gov.au/system/files/pages/a8d10ce5-6a49-4fc2-b94d-575d6d11c547/files/preliminary-analysis-area-19-jan-2020.pdf
paa <- file.path(bugs_dir, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
paa <- rgdal::readOGR(paa)

## Reproject PAA to equal area
paa <- sp::spTransform(paa, CRSobj = aus.crs)
writeOGR(paa2, dsn = file.path(output_dir, "fire"), layer = "prelim_analysis_areas_dissolve_eqar", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
rm(paa)



## Vegetation layer: NVIS ####
## Source: https://www.environment.gov.au/land/native-vegetation/national-vegetation-information-system/data-products

## Recalssify NVIS data to include native vegetation only & convert to tiff
infile <- file.path(bugs_dir, "native_vegetation", "GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf")
outfile <- file.path(output_dir, "native_vegetation", "nvis_v6.tif")

gdalUtils::gdalinfo(infile)
system(paste0("gdal_calc.py -A ", infile,
              " --calc='((A==1) + (A==2) + (A==3) + (A==4) + (A==5) + (A==6) + (A==7) + (A==8) + (A==9) + (A==10) + (A==11) + (A==12) + (A==13) + (A==14) + (A==15) + (A==16) + (A==17) + (A==18) + (A==19) + (A==20) + (A==21) + (A==22) + (A==23) + (A==24) + (A==26) + (A==27) + (A==29) + (A==30) + (A==31) + (A==32))*1 + ((A==25) + (A==28) + (A==99))*0' --NoDataValue=0",
              " --outfile=", outfile))
gdalUtils::gdalinfo(outfile)

raster(outfile)
sort(unique(na.omit(temp)))

## Reproject according to Australia mask
# r <- raster(file.path(bugs_dir, "native_vegetation", "GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf"))
infile <- outfile
outfile <- gsub(".tif", "_reclass.tif", infile)

system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
              paste(aus.res, collapse = " "),
              " -t_srs '", aus.crs, "' ",
              infile, " ", outfile))
gdalUtils::gdalinfo(outfile)

raster(outfile)
sort(unique(na.omit(temp)))

nvis.extent <- extent(raster(outfile))



## Fire severity layer - GEEBAM #####
## Source: http://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08%7D

## Reproject according to Australia mask
infile <- file.path(bugs_dir, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
outfile <- file.path(output_dir, "fire", "severity5_eqar250.tif")

gdalUtils::gdalinfo(infile)
system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
              paste(aus.res, collapse = " "), " -te ", 
              paste(nvis.extent[1], nvis.extent[3], 
                    nvis.extent[2], nvis.extent[4]), 
              " -t_srs '", aus.crs, "' ",
              infile, " ", outfile))
gdalUtils::gdalinfo(outfile)
raster(outfile)

# # Reclassify values in raster
# infile <- outfile
# outfile <- file.path(output_dir, "fire", "severity3_eqar250.tif")
# system(paste0("gdal_calc.py -A ", infile,
#               " --calc='(A==1)*1 + ((A==2)+(A==3))*2 + ((A==4)+(A==5))*3' --NoDataValue=0",
#               " --outfile=", outfile))
# gdalUtils::gdalinfo(outfile)

## Clip fire severity raster with reclassified NVIS layer (i.e. native vegetation only)
## >>>>> TAKE NOTE: Modify gdal_calc.R <<<<<
##  > Uncomment L163-165 : When NoDataValue == 0
##  > Comment L158-160: When NoDataValue == -9999
infile <- outfile
outfile <- gsub(".tif", "_native.tif", infile)

native_file <- file.path(output_dir, "native_vegetation", "nvis_v6_reclass.tif")
gdalUtils::gdalinfo(native_file)

source(file.path(getwd(), "scripts/gdal_calc.R"))
gdalmask(infile = infile, mask = native_file, outfile = outfile, output_Raster = FALSE, overwrite=TRUE, verbose=TRUE)
gdalUtils::gdalinfo(outfile)

## Clip fire severity raster with Preliminary analysis area shapefile
infile <- outfile
outfile <- gsub(".tif", "_paa.tif", infile)

paa_file <- file.path(output_dir, "fire", "prelim_analysis_areas_dissolve_eqar.shp") 

system(paste0("gdalwarp -overwrite ", "-te ", 
              paste(nvis.extent[1], nvis.extent[3], 
                    nvis.extent[2], nvis.extent[4]), 
              " -of GTiff -cutline ", paa_file, 
              " -cl prelim_analysis_areas_dissolve_eqar ",
              infile, " ", outfile))

gdalinfo(outfile)
raster(outfile)
sort(unique(raster(outfile)[]))




## NIAFED vs GEEBAM ####
## Find areas not included in fire severity (GEEBAM) but in fire extent (NIAFED)

## >> Convert GEEBAN severity raster data to shapefile ####
## >>>> GEEBAM fire severity 5 class into 1/0 raster
infile <- file.path(output_dir, "fire", "severity5_eqar250.tif")
outfile <- file.path(output_dir, "fire", "severity1_eqar250.tif")
# gdalUtils::gdalinfo(file.path(bugs_dir, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif"))
system(paste0("gdal_calc.py -A ", infile,
              " --calc='((A==1) + (A==2) + (A==3) + (A==4) + (A==5))*1' --NoDataValue=0",
              " --outfile=", outfile))
gdalUtils::gdalinfo(outfile)
sort(unique(raster(outfile)[]))

## >>>> Convert GEEBAM fire severity 1/0 raster to shapefile
infile <- outfile
outfile <- gsub(".tif",".shp", infile)
system(paste0("gdal_polygonize.py ", infile, " ", outfile, " -f 'ESRI Shapefile'"))
## check values in QGIS

## >>> Dissolve
infile <- outfile
outfile <- gsub(".shp","_dissolve.shp", infile)
system(paste0("ogr2ogr ", outfile, " ", infile, " -dialect sqlite -sql 'SELECT ST_Union(geometry), DN FROM severity1_eqar250 GROUP BY DN'"))
# ## Can also be written as this because there is only one value in the attribute table:
# system(paste0("ogr2ogr ", outfile, " ", infile, " -dialect sqlite -sql 'SELECT ST_Union(geometry) AS geometry FROM severity1_eqar250'"))

## Fire extent data: NIAFED
## Source http://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B9ACDCB09-0364-4FE8-9459-2A56C792C743%7D
fseverity <- rgdal::readOGR(outfile)
fextent <- rgdal::readOGR(file.path(bugs_dir, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))

## >> Reproject NIAFED extent data to Albers Euqal Atra
infile <- file.path(bugs_dir, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp")
outfile <- file.path(output_dir, "fire", "fire_extent_EA.shp")
system(paste0("ogr2ogr -f 'ESRI Shapefile' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -s_srs EPSG:OLD_EPSG_NUMBER ", outfile, " ", infile))

# 1. Erase severity from extent = residual polygons - ArcGIS Erase tool - QGIS tool? 
## https://www.r-bloggers.com/2015/09/clipping-polygons-in-r/
gIntersection(regions, lads, byid = TRUE, drop_lower_td = TRUE)
# 2. Total area of the leftover polygons: 

# 3. First dissolve on state boundaries 

# 4. sf_intersects: on the output run locator_buffer$areakm2 <-raster::area(locator_buffer)/1000000 units??







# ## >> Reclassify severity ####
# ## From 5 to 3 fire classes (if using buffer, leave buffer as a separate class)
# infile <- file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
# gdalUtils::gdalinfo(infile)
# outfile <- file.path(output_dir, "fire", "severity3.tif")
# system(paste0("gdal_calc.py -A ", infile,
#               " --calc='(A==1)*1 + ((A==2)+(A==3))*2 + ((A==4)+(A==5))*3' --NoDataValue=0",
#               " --outfile=", outfile))
# gdalUtils::gdalinfo(outfile)
# raster(outfile) ## values 0, 255 > 255 implies no data value
# 
# ## Using the raster package - VERY SLOW
# reclassMatrix <- matrix(c(1,1,
#                           2,2,
#                           3,2,
#                           4,3,
#                           5,3),5,2,byrow = TRUE)
# out <- raster::reclassify(fireseverity, rcl = reclassMatrix)
# writeRaster(out, filename = file.path(output_dir, "fire", "severity3_raster.tif"))
# 
# ## >> Reproject to Equal area ####
# ## See Proj4 in https://spatialreference.org/ref/sr-org/australia-albers-equal-area-conic-134/
# infile <- file.path(output_dir, "fire", "severity3_raster.tif")
# outfile <- file.path(output_dir, "fire", "severity3_eqar.tif")
# wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# # crs(fireseverity)
# new_crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# gdalUtils::gdalwarp(infile, outfile, s_srs = wgs_crs, t_srs = new_crs)
# 
# ## >> Resample to 250m resolution (from ~40m resolution) ####
# ## https://gis.stackexchange.com/questions/255150/using-resample-vs-aggregate-extend-in-r-to-have-rasters-of-matching-resolutio
# infile <- outfile
# outfile <- file.path(output_dir, "fire", "severity3_eqar250_near.tif")
# gdalUtils::gdalwarp(infile, outfile, tr = c(250,250), r = "near")
# raster(outfile)
# sampleRandom(raster(outfile), size = 50, ext = extent(raster(outfile)) , na.rm = TRUE, sp=FALSE, asRaster=FALSE) 
# 
# ## >> Convert to polygons ####
# ## In QGIS (see steps data processing document)
# ## In R - not trialled yet
# infile <- file.path(output_dir, "fire", "severity3_eqar_250avg.tif")
# outfile <- file.path(output_dir, "fire", "severity3_eqar_250avg_polygon2.shp")
# system(paste0("gdal_polygonize.py ", infile, " ", outfile, " -b 1 -f \"ESRI Shapefile\" severity3_eqar_250avg_polygon fireclass"))
# system(paste0("gdal_polygonize.py ", infile, " ", outfile, " -b 1 -f 'ESRI Shapefile' severity3_eqar_250avg_polygons fire_class"))
# # gdal_polygonize.py /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg.tif /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg_polygon.shp -b 1 -f "ESRI Shapefile" severity3_eqar_250avg_polygon fireclass
# # 
# # system("gdal_polygonize.py /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg.tif /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg_polygon.shp -b 1 -f \"ESRI Shapefile\" severity3_eqar_250avg_polygon fireclass")
# OR...??
# system("source ~/environments/gsdms_env/bin/activate") ??
# OR...??
# source("/tempdata/workdir/nesp_bugs/scripts/gdal_polygonizeR.R")
# system.time(p <- gdal_polygonizeR(outfile))

