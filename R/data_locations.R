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


data_nc_fun <- function(file, download, citation = NULL) {
   # force(file)

   if (is.character(download)) {
      download <- simple_download(download)
   }

   function(force_download = FALSE) {
      if (force_download || !file.exists(file)) {
         file <- download(file)
         if (!is.null(citation)) {
            write_citation(citation)
         }

      }

      checkmate::assert_access(file, access = "r")
      class(file) <- c("nc_file", class(file))
      return(file)
   }
}


data_rds_fun <- function(file, download, citation = NULL) {
   # force(file)

   if (is.character(download)) {
      download <- simple_download(download)
      if (!is.null(citation)) {
         write_citation(citation)
      }
   }

   function(force_download = FALSE) {
      if (force_download || !file.exists(file)) {
         file <- download(file)
      }
      return(file)
   }
}



#' @param force_download Logical indicating wether to force download of data.
#' @export
#' @rdname data_locations
ERA5_TOC <- data_nc_fun(file = data_path("raw", "era5.toc.mon.mean.nc"),
                        download = download_cds(request_toc),
                        citation = citation_o3_era5)

#' @export
#' @rdname data_locations
ERA5_geopotential <- data_nc_fun(file = data_path("raw", "era5.z.mon.mean.nc"),
                       download = download_cds(request_geopotential),
                       citation = citation_era5)

#' @export
#' @rdname data_locations
ERA5_geopotential_all <- data_nc_fun(file = data_path("raw", "era5.z-all.mon.mean.nc"),
                                 download = download_cds(request_geopotential_all),
                                 citation = citation_era5)


#' @export
#' @rdname data_locations
ERA5_temperature <- data_nc_fun(file = data_path("raw", "era5.air.mon.mean.nc"),
                                 download = download_cds(request_temperature),
                                 citation = citation_era5)

#' @export
#' @rdname data_locations
ERA5_vorticity <- data_nc_fun(file = data_path("raw", "era5.vor.mon.mean.nc"),
                                download = download_cds(request_vorticity),
                                citation = citation_era5)

#' @export
#' @rdname data_locations
ERA5_ozone <- data_nc_fun(file = data_path("raw", "era5.o3.mon.mean.nc"),
                              download = download_cds(request_ozone),
                              citation = citation_era5)




#' @param force_download Logical indicating wether to force download of data.
#' @export
#' @rdname data_locations
ERA5_BE <- data_nc_fun(file = data_path("raw", "era5be.mon.mean.nc"),
                       download = download_cds(request_era5be),
                       citation = citation_era5be)


#' @export
#' @rdname data_locations
ERA5_T2M <- data_nc_fun(file = data_path("raw", "era5.2mt.mon.mean.nc"),
                        download = download_cds(request_era5_2mt))


#' @export
#' @rdname data_locations
NCEP_PSI <- data_nc_fun(file = data_path("raw", "psi.mon.mean.nc"),
                        download = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/psi.mon.mean.nc")

#' @export
#' @rdname data_locations
NCEP_VOR <- data_nc_fun(file =  data_path("raw", "vor.mon.mean.nc"),
                        download = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/vor.mon.mean.nc")

#' @export
#' @rdname data_locations
CMAP <- data_nc_fun(file = data_path("raw", "cmap.mon.mean.nc"),
                    download = "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc",
                    citation = citation_cmap)



#' @export
#' @rdname data_locations
ERSST <- data_rds_fun(file = data_path("raw", "ersst.Rds"),
                      download = download_ersst)
