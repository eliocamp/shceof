#' @export
print.nc_file <- function(x, ...) {
   cat("NetCDF file.\nLocation:",
       as.character(x), "\n\nDetails:\n")
   print(metR::GlanceNetCDF(x))
}

#' Data locations
#'
#' Functions that return the location of different datasets. They check that
#' relevant file exist and then return it with a class that has a neat print method.
#'
#'
#' @param type character indicating the type of data. The resulting base will be type_data
#' @param ... characters to be contactenadted with [file.path()]
#' @export
#' @rdname data_locations
data_path <- function(type = c("raw", "derived"), ...) {
   here::here("analysis", "data", paste0(type[1], "_data"), ...)
}


data_nc_fun <- function(file, download) {
   force(file)

   if (is.character(download)) {
      download <- simple_download(download)
   }

   function(force_download = FALSE) {
      if (force_download || !file.exists(file)) {
         file <- download(file)
      }

      checkmate::assert_access(file, access = "r")
      class(file) <- c("nc_file", class(file))
      return(file)
   }
}


#' @param force_download Logical indicating wether to force download of data.
#' @export
#' @rdname data_locations
ERA5 <- data_nc_fun(file = data_path("raw", "era5.mon.mean.nc"),
                    download = download_cds(request_era5))


#' @param force_download Logical indicating wether to force download of data.
#' @export
#' @rdname data_locations
ERA5_BE <- data_nc_fun(file = data_path("raw", "era5be.mon.mean.nc"),
                       download = download_cds(request_era5be))


#' @export
#' @rdname data_locations
ERA5_SST <- data_nc_fun(file = data_path("raw", "era5.sst.mon.mean.nc"),
                        download = download_cds(request_era5_sst))

#' @export
#' @rdname data_locations
ERA20C <- data_nc_fun(file = data_path("raw", "era20c.mon.mean.nc"),
                      download = download_webapi(request_era20c))


#' @export
#' @rdname data_locations
NCEP_PSI <- data_nc_fun(file = data_path("raw", "psi.mon.mean.nc"),
                        download = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/vor.mon.mean.nc")

#' @export
#' @rdname data_locations
NCEP_VOR <- data_nc_fun(file =  data_path("raw", "vor.mon.mean.nc"),
                        download = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/vor.mon.mean.nc")


#' @export
#' @rdname data_locations
HADSST <- data_nc_fun(file = data_path("raw", "hadsst.mon.mean.nc"),
                      download = "https://www.metoffice.gov.uk/hadobs/hadsst4/data/netcdf/HadSST.4.0.0.0_median.nc")

#' @export
#' @rdname data_locations
CMAP <- data_nc_fun(file = data_path("raw", "cmap.mon.mean.nc"),
                    download = "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc" )

