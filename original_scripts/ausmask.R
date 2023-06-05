## Create australia coastline shapefile

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

# devtools::install_github('smwindecker/gdaltools', force = TRUE)
# devtools::install_github('skiptoniam/sense', force = TRUE)

x <- c("sp", "raster", "gdaltools", "rgdal", "gdalUtils", 
       "rnaturalearth", "devtools", "usethis", "sense","quickPlot")
lapply(x, require, character.only = TRUE)


## Server paths
bugs_data = file.path("/tempdata/research-cifs/uom_data/nesp_bugs_data")
mask_data = file.path(bugs_data, "masks")
output_dir = file.path(bugs_data, "outputs", "masks")

# install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "binary")
# source("/Users/payalb/Dropbox/Projects/discovery_trade/analyses/regSSP_fraclu/scripts/gdal_raster_functions.R")
source(file.path(getwd(), "nesp_bugs/scripts/gdal_calc.R"))



## NOAA mask with islands + terriroties ---- ####
  ## File: Topo250kv3AMBIS3islBlnNoaa is a blend on AU jursidiction data with NOAA coastline
  ## Auhtor: Keith Hayes, Data61, CSIRO
  ## Sources: 
  ## AMSIS http://www.ga.gov.au/scientific-topics/marine/jurisdiction/amsis
  ## NOAA https://www.ngdc.noaa.gov/mgg/shorelines/ 
  ## See data doc for  QGIS processing on file Topo250kv3AMBIS3islBlnNoaa > aus_lands.gpkg
auslands <- rgdal::readOGR(file.path(mask_data, "aus_lands.gpkg"))
unique(auslands$FEATTYPE)
plot(auslands)

## >> Save as shapefile in Albers Equal Area ####
aea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
auslands <- spTransform(auslands, aea_crs)
plot(auslands)
writeOGR(auslands, dsn = output_dir, layer = "auslands_aea137", driver="ESRI Shapefile")


## >> Save as shapefile in WGS 84 - wgs84.shp ####
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
auslands_wgs84 <- spTransform(auslands, wgs_crs)
writeOGR(auslands_wgs84, dsn = output_dir, layer = "auslands_wgs84", driver="ESRI Shapefile")
out <- rgeos::gUnionCascaded(auslands_wgs84)
crs(out)
shapefile(out, filename = file.path(output_dir, "auslands_1poly_wgs84.shp"))


## >> NOAA raster mask in WGS ####
## Convert to raster - 1km.tif
ausmask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
infile <- file.path(mask_data, "aus_lands.gpkg")
outfile <- file.path(output_dir, "ausmask_noaa_1km.tif")
rasterize_shp(infile, outfile, res = res(ausmask)[1], ext = extent(auslands)[c(1,2,3,4)])

## Reproject to WGS84 - 1kmWGS.tif
infile <- outfile
outfile <- file.path(output_dir, "ausmask_noaa_1kmWGS.tif")
source_crs <- crs(raster(infile))
new_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
new_res <- res(ausmask)
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = new_crs, tr = new_res, verbose=TRUE)
unique(getValues(raster(outfile)))

## Set 0 as NAs - 1kmWGS_NA.tif
outfile1 <- outfile
outfile2 <- gsub(".tif", "_NA.tif", outfile)
gdalcalc(calc="A==1", infile = outfile1, outfile = outfile2,
         NoDataValue=0, overwrite=TRUE)
unique(getValues(raster(outfile2)))
plot(raster(outfile2), col = "peru", axes = FALSE, box = FALSE, legend = FALSE)

## Save as shapefile - 1kmWGS_NA.shp
writeOGR(raster(outfile2), dsn = output_dir, layer = "ausmask_noaa_1kmWGS_NA", driver="ESRI Shapefile")


## >> NOAA  mask in Equal area ####
## >>>> Reproject to Albers - 250mEA.tif ####
## See Proj4 in https://spatialreference.org/ref/epsg/3577/
infile <- file.path(output_dir, "ausmask_noaa_1kmWGS.tif")
outfile <- file.path(output_dir, "ausmask_noaa_250mAlbers.tif")
source_crs <- crs(raster(infile))
equalarea_proj <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
new_res <-c(250, 250) 
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = equalarea_proj, tr = new_res, verbose=TRUE)

## >>>> Reproject to Albers EQUAL AREA- 250mEA.tif ####
## See Proj4 in https://spatialreference.org/ref/sr-org/australia-albers-equal-area-conic-134/
infile <- file.path(output_dir, "ausmask_noaa_1kmWGS.tif")
outfile <- file.path(output_dir, "ausmask_noaa_250mAlbersEA.tif")
source_crs <- crs(raster(infile))
equalarea_proj <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
new_res <-c(250, 250) 
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = equalarea_proj, tr = new_res, verbose=TRUE)

## Set 0s as NA - 250mEA_NA.tif
outfile1 <- outfile
outfile2 <- gsub(".tif", "_NA.tif", outfile)
gdalcalc(calc="A==1", infile = outfile1, outfile = outfile2,
         NoDataValue=0, overwrite=TRUE)
# plot(raster(outfile2), col = "peru", axes = FALSE, box = FALSE, legend = FALSE)

# Random sample of raster to check for NAs
sampleRandom(raster(outfile1), size = 50, ext = extent(raster(outfile1)) , na.rm = FALSE, sp=FALSE, asRaster=FALSE) 
sampleRandom(raster(outfile2), size = 50, ext = extent(raster(outfile2)) , na.rm = FALSE, sp=FALSE, asRaster=FALSE) 

## Clip to min extent of non NA values .... ?
e <- extent(raster(infile))
e <- as(e, "SpatialPolygons")
sp::proj4string(e) <- crs(raster(infile))
e_EA <- sp::spTransform(e, CRSobj = equalarea_proj)
e_EA <- as.vector(extent(e_EA))
e_EA <- e_EA[c(1,3,2,4)] #swap xmax and ymin values to use for te in gdalwarp

infile <- outfile
outfile <- file.path(output_dir, "ausmask_noaa_250mEA_clip.tif")
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, te = e_EA, te_srs = equalarea_proj)



## Quickpolotting ####
r <- raster(file)
clearPlot()
quickPlot::Plot(r,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                # addTo = "ausmask",
                new = TRUE)





## EXTRA ----- ####
## Coarse mask using rnaturaleath package ####
plot(sf::st_geometry(rnaturalearth::ne_countries(country = "australia",
                                                 returnclass = "sf")))
## coastline more detailed in ne_states compared to ne_country
aus.mask <- rnaturalearth::ne_states("australia",
                                     returnclass = "sf")
# ## Reproject to Australian Albers
# aus.mask <- sf::st_transform(aus.mask,
#                                crs = 3577)
# plot(sf::st_geometry(aus.mask))

aus.mask <- sf::st_union(aus.mask)
aus.mask <- as(aus.mask, "Spatial")
# ext <-  c(113.338953078, 153.569469029, -43.6345972634, -10.6681857235) #xmin, xmax, ymin,ymax
aus.mask <- rasterize(aus.mask, raster(ext=extent(aus.mask), res =  0.008333333), field = 1) ## 1km2: 0.008333333
## Reproject to Australian Albers
newproj <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
aus.mask.wgs <- aus.mask
aus.mask <- projectRaster(aus.mask, crs= newproj)
plot(aus.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
writeRaster(aus.mask, "./output/ausmask_3577.tif", format = "GTiff")
writeRaster(aus.mask.wgs, "./output/ausmask_WGS.tif", format = "GTiff")

# ## Coarse res mask for pdf plot - 100km2
# aus.mask <- rnaturalearth::ne_countries(country = "australia",
#                                         returnclass = "sf")
# aus.mask <- as(aus.mask, "Spatial")
# aus.mask <- rasterize(aus.mask, raster(ext=extent(aus.mask), 
#                                        res =  0.01), field = 1) ## 1km2: 0.008333333
# plot(aus.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
# writeRaster(aus.mask, 
#             file = file.path(output_dir, "ausmask_WGS10.tif"), format = "GTiff")


## Mask using GEODATA COAST 100K 2004####
## Projection: Projection/datum: Geographical coordinates using the Geocentric Datum of Australia 1994 (GDA94)
# aus_geodata_poly <- sf::st_read(file.path(mask_data, "GEODATA COAST 100K 2004/61395_shp/australia/cstauscd_r.shp"))
aus_geodata <- rgdal::readOGR(file.path(mask_data, "GEODATA COAST 100K 2004/61395_shp/australia/cstauscd_r.shp"))
unique(aus_geodata$FEAT_CODE)

## Convert to raster
aus_all <- rgeos::gUnionCascaded(aus_geodata)
shapefile(x = aus_geodata, file = file.path(output_dir, "aus_all.shp"))
ausmask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
infile <- file.path(output_dir, "aus_all.shp")
outfile <- file.path(output_dir, "aus_all.tif")
rasterize_shp(infile, outfile, res = res(ausmask)[1], ext = extent(aus_geodata)[c(1,2,3,4)])

## Reproject to WGS84
infile <- outfile
outfile <- file.path(output_dir, "aus_all_WGS.tif")
source_crs <- crs(raster(infile))
new_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
new_res <- res(ausmask)
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = new_crs, tr = new_res, verbose=TRUE)

plot(raster(outfile))
unique(raster(outfile)[])


## Mainland only mask ####
aus_mainland <- aus_geodata[aus_geodata$FEAT_CODE == "mainland",]

## Convert to raster
aus_mainland <- rgeos::gUnionCascaded(aus_mainland)
shapefile(x = aus_mainland, file = file.path(output_dir, "aus_mainland.shp"))
ausmask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
infile <- file.path(output_dir, "aus_mainland.shp")
outfile <- file.path(output_dir, "aus_mainland.tif")
rasterize_shp(infile, outfile, res = res(ausmask)[1], ext = extent(aus_geodata)[c(1,2,3,4)])

## Reproject to WGS84
infile <- outfile
outfile <- file.path(output_dir, "aus_mainland_WGS.tif")
source_crs <- crs(raster(infile))
new_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
new_res <- res(ausmask)
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = new_crs, tr = new_res, verbose=TRUE)
plot(raster(outfile))
unique(raster(outfile)[])


## Island only mask ####
aus_island <- aus_geodata[aus_geodata$FEAT_CODE == "island",]

## Convert to raster
aus_island <- rgeos::gUnionCascaded(aus_island)
shapefile(x = aus_island, file = file.path(output_dir, "aus_island.shp"))
ausmask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
infile <- file.path(output_dir, "aus_island.shp")
outfile <- file.path(output_dir, "aus_island.tif")
rasterize_shp(infile, outfile, res = res(ausmask)[1], ext = extent(aus_geodata)[c(1,2,3,4)])

## Reproject to WGS84
infile <- outfile
outfile <- file.path(output_dir, "aus_island_WGS.tif")
source_crs <- crs(raster(infile))
new_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
new_res <- res(ausmask)
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = new_crs, tr = new_res, verbose=TRUE)
plot(raster(outfile))
unique(raster(outfile)[])


## Sea only mask ####
aus_sea <- aus_geodata[aus_geodata$FEAT_CODE == "sea",]

## Convert to raster
aus_sea <- rgeos::gUnionCascaded(aus_sea)
shapefile(x = aus_sea, file = file.path(output_dir, "aus_sea.shp"))
ausmask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
infile <- file.path(output_dir, "aus_sea.shp")
outfile <- file.path(output_dir, "aus_sea.tif")
rasterize_shp(infile, outfile, res = res(ausmask)[1], ext = extent(aus_geodata)[c(1,2,3,4)])

## Reproject to WGS84
infile <- outfile
outfile <- file.path(output_dir, "aus_sea_WGS.tif")
source_crs <- crs(raster(infile))
new_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
new_res <- res(ausmask)
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = new_crs, tr = new_res, verbose=TRUE)
plot(raster(outfile))


## Mask using GADM ####
reg_mask <- getData("GADM", country = region, level = 0, path = rdata_path)
reg_mask <- gSimplify(reg_mask, tol = 0.00833)
mask_template <- raster(
  nrow = 4091,
  ncol = 4990,
  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
  ext = extent(c(112.4667, 154.05,-44.04167,-9.95))
)
mask_template[] <- 1
reg_mask <- mask(mask_template, reg_mask)
reg_mask <- aggregate(reg_mask, fact = 10)


## Mask using NOAA data ####
## Layer provided by Skip W
## Includes offshore territoties and Islands
## Needs to be clipped down by extent/boundinng box INCLUDING Australia + territories EXCLUDING Antractica
aus_noaa <- rgdal::readOGR("/Volumes/uom_data/nesp_bugs_data/masks/Topo250kv3AMBIS3islBlnNoaa/Topo250kv3AMBIS3islBlnNoaa.shp")
raster::extent(aus_noaa@bbox)

## EXTRA
# aus.mask  <- sf::st_read("./output/aus_tileedge.shp")
# plot(sf::st_geometry(aus.mask))
# 
# aus.mask <- concaveman(aus.mask) # concave polygon from lines
# aus.mask <- sf::st_zm(aus.mask) # drop M
# plot(sf::st_geometry(aus.mask))
# 
# temp <- sf::st_polygonize(aus.mask)
# temp <- sf::st_cast(aus_tileedge, "POLYGON")
# temp2 <- fasterize::fasterize(temp, raster::raster())
