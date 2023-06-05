## Authors: Jonathan Garber
## Ref: https://gitlab.unimelb.edu.au/garberj/gdalutilsaddons

library(gdalUtils)
library(rgdal)
library(devtools)
require("raster")
library(assertthat)





gdalcalc = function(calc,
                    infile,
                    outfile,
                    bands = 1,
                    NoDataValue,
                    type,
                    format,
                    creation,
                    co,
                    allBands,
                    overwrite,
                    debug,
                    quiet,
                    # Additional parameters
                    output_Raster = FALSE,
                    verbose = FALSE,
                    ignore.full_scan = TRUE){
  
  
  if (output_Raster && (!requireNamespace("raster") || !requireNamespace("rgdal"))) {
    warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
    return(NULL)
  }
  
  parameter_values <- as.list(environment())
  
  band_vec = paste(c(LETTERS), "_band", sep="")
  
  if(is.string(parameter_values$infile)){
    parameter_values$A = parameter_values$infile
    num_vars = 1
  }else if ( is.list(parameter_values$infile)){
    parameter_values = append(parameter_values, parameter_values$infile)
    num_vars = length(parameter_values$infile)
    parameter_values$infile = NULL
    
  }else if(is.array(parameter_values$infile)){
    v_list = setNames( as.list(parameter_values$infile),as.list(LETTERS[1:length(parameter_values$infile)]))
    num_vars = length(v_list)
    parameter_values = append(parameter_values, v_list)
    parameter_values$infile = NULL
  }
  
  #now make sure the bands get sorted out ok
  bands = parameter_values$bands
  if (is.integer(bands)){
    # this means all rasters get the same band
    
    v_list = setNames( as.list(rep(bands, length(num_vars)),as.list(band_vec[1:num_vars])))
    parameter_values = append(parameter_values, v_list)
    parameter_values$bands = NULL
    
  } else if( is.list(bands)){
    parameter_values = append(parameter_values, parameter_values$bands)
    parameter_values$bands = NULL
    
  }else if(is.array(bands)){
    v_list = setNames( as.list(bands),as.list(band_vec[1:length(bands)]))
    parameter_values = append(parameter_values, v_list)
    parameter_values$bands = NULL
  }
  
  
  # if (verbose)
  #   message("Checking gdal_installation...")
  # gdal_setInstallation(ignore.full_scan = ignore.full_scan,
  #                      verbose = verbose)
  # if (is.null(getOption("gdalUtils_gdalPath"))){
  #   message(character(is.null(getOption("gdalUtils_gdalPath"))))
  #   warning('cannot find gdal installation')
  #   return(list('didnt get past utils gdalPath', getOption('gdalUtils_gdalPath')))
  # }
  
  
  
  executable = 'gdal_calc.py'
  
  parameter_order = c('calc', c(LETTERS),c(band_vec), c('outfile', 'NoDataValue', 'type', 'format', 'creation',
                                                        'co', 'allBands', 'overwrite', 'debug', 'quiet'))
  
  parameter_variables = list(logical= list(varnames = c('overwrite', 'debug', 'quiet')),
                             scalar= list(varnames=c(c(band_vec), 'NoDataValue')),
                             character= list(varnames=c('calc', c(LETTERS), c ('outfile',
                                                                               'type', 'format', 'creation',
                                                                               'co', 'allBands')))
  )
  parameter_noflags = c()
  parameter_double_dash = c('calc',c(band_vec), 'outfile', 'NoDataValue', 'type', 'format', 'creation',
                            'co', 'allBands', 'overwrite', 'debug', 'quiet')
  
  # parameter_values = list(A='fra_ 15 .tif', calc = 'A+100', outfile = '/home/garberj/tests_scripts/fra_15_plus100.tif')
  
  cmd = gdal_cmd_builder(executable, parameter_variables = parameter_variables,
                         parameter_values = parameter_values,
                         parameter_order = parameter_order,
                         parameter_noflags = parameter_noflags,
                         parameter_doubledash = parameter_double_dash,
                         parameter_noquotes = c(),
                         gdal_installation_id = 1,
                         python_util = TRUE,
                         verbose = FALSE)
  
  if (verbose)
    message(paste("GDAL command being used:", cmd))
  cmd_output <- system(cmd, intern = TRUE)
  
  if (output_Raster) {
    return(brick(outfile))
  }
  else {
    return(NULL)
  }
  
}



gdalmask = function(infile, mask, outfile, output_Raster = FALSE, overwrite=TRUE,verbose=FALSE, ignore.full_scan = TRUE) {
  
  if (output_Raster && (!requireNamespace("raster") || !requireNamespace("rgdal"))) {
    warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
    return(NULL)
  }
  
  # if (verbose) {
  #   message("Checking gdal_installation...")
  # }
  #
  # gdal_setInstallation(ignore.full_scan = ignore.full_scan,
  #                      verbose = verbose)
  #
  # if (is.null(getOption("gdalUtils_gdalPath"))){
  #   message(character(is.null(getOption("gdalUtils_gdalPath"))))
  #   warning('cannot find gdal installation')
  #   return(list('didnt get past utils gdalPath', getOption('gdalUtils_gdalPath')))
  # }
  
  
  # tempfile = 'temp.tif'
  # gdalcalc(calc= 'A*(A>-9999)', infile = infile, outfile=tempfile,
  #          NoDataValue = -9999, verbose=verbose)
  # '(B*A) + (B<1)*(-9999)'
  
  
  # ## UNCOMMENT : When NoDataValue == -9999
  # gdalcalc(calc= '((B==1)*A)+(-9999*(B!=1))', infile = list(A=infile, B=mask), outfile=outfile,
  #          NoDataValue = -9999,verbose=verbose)

  
  ## UNCOMMENT : When NoDataValue == 0
  gdalcalc(calc= '((B==1)*A)+(0*(B!=1))', infile = list(A=infile, B=mask), outfile=outfile,
           NoDataValue = 0,verbose=verbose)
  
  # system(paste0('rm ',tempfile))
  if (isTRUE(output_Raster)) {
    outraster <- raster::raster(outfile)
    return(outraster)
  }
}




gdalfillnodata = function(srcfile,
                          dstfile,
                          nomask,
                          mask,
                          md,
                          si,
                          of,
                          b,
                          q,
                          mask_val=1,
                          output_Raster = FALSE,
                          verbose=FALSE,
                          ignore.full_scan=TRUE
                          # o_name Currently none are supported
){
  
  if (output_Raster && (!requireNamespace("raster") || !requireNamespace("rgdal"))) {
    warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
    return(NULL)
  }
  
  parameter_values <- as.list(environment())
  
  # if (verbose)
  #   message("Checking gdal_installation...")
  # gdal_setInstallation(ignore.full_scan = ignore.full_scan,
  #                      verbose = verbose)
  # if (is.null(getOption("gdalUtils_gdalPath"))){
  #   message(character(is.null(getOption("gdalUtils_gdalPath"))))
  #   warning('cannot find gdal installation')
  #   return(list('didnt get past utils gdalPath', getOption('gdalUtils_gdalPath')))
  # }
  
  
  
  
  executable = 'gdal_fillnodata.py'
  
  if ("mask" %in% names(parameter_values)){
    na = NAvalue(raster(parameter_values$srcfile))
    temp_mask = 'temp.tif'
    mask = parameter_values$mask
    mask_dat = parameter_values$mask_val[[1]]
    calc = paste('(A!=', as.character(mask_dat),")&(B!=",as.character(na),")" )
    gdalcalc(calc=calc,infile=list(A=mask, B= parameter_values$srcfile),
             outfile=temp_mask, verbose=verbose)
    parameter_values$mask = temp_mask
    plot(raster(temp_mask), main='inverted_mask')
  }
  
  #  [-q] [-md max_distance] [-si smooth_iterations]
  # [-o name=value] [-b band]
  # srcfile [-nomask] [-mask filename] [-of format] [dstfile]
  
  parameter_order = c('q','md','si','b','srcfile','nomask','mask','of', 'dstfile')
  parameter_variables = list(logical= list(varnames = c('nomask', 'q')),
                             scalar= list(varnames=c('b', 'md', 'si')),
                             character= list(varnames=c('srcfile', 'dstfile','of', 'mask')))
  parameter_noflags = c('srcfile', 'dstfile')
  parameter_double_dash = c()
  
  # parameter_values = list(A='fra_ 15 .tif', calc = 'A+100', outfile = '/home/garberj/tests_scripts/fra_15_plus100.tif')
  
  cmd = gdal_cmd_builder(executable, parameter_variables = parameter_variables,
                         parameter_values = parameter_values,
                         parameter_order = parameter_order,
                         parameter_noflags = parameter_noflags,
                         parameter_doubledash = parameter_double_dash,
                         parameter_noquotes = c(),
                         gdal_installation_id = 1,
                         python_util = TRUE,
                         verbose = FALSE)
  
  if (verbose)
    message(paste("GDAL command being used:", cmd))
  cmd_output <- system(cmd, intern = TRUE)
  
  if ("mask" %in% names(parameter_values)){
    system(paste('rm ', temp_mask))
  }
  
  if (output_Raster) {
    return(brick(outfile))
  }
  else {
    return(NULL)
  }
}





# Leaflet_plot('france_mask_big.tif','france_interpolated.tif')

#  Archived garbage will deal with it later
#plot(fra)
# gdal_rasterize()
#
# plot(out_fr_1p5min)
#
# plot(out_fr_1p5min2)
#
# class(out_fr_1p5)
# parameter_values$infile = parameter_values$A
#
# parameter_values$A = NULL
# parameter_values
# plot(raster('fra_ 15 .tif'))
#
# test_func=function(one=1) print(as.list(environment()))
# getwd()
# test_func()
#
# system('gdalwarp -h')
#
#
# extent = c(0,50,0,50)
# resolution = c(10, 10)
# inpath = 'dist_calc_wgs.tif'
# outpath = 'test2.tif'
# test_com = sprintf('gdalwarp  -of gtiff -te %f %f %f %f -tr %f %f %s %s',
#                    extent[1],extent[3],extent[2],extent[4], resolution[1], resolution[2], inpath, outpath)
#
# system2(test_com, stdout=TRUE)
# test_com
# test = list(string1 = 'string', int1 = 3, float1 = )
#
# type_list = list()
# for(name in names(test)){
#
#
# }
#
# #Idea just bring in GdalUtils command builder to build commands, will have to change a bit what
# #include a list of all the -- and - flags,
# #
#
# gdal_cmd_builder()


