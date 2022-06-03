#' Data locations
#'
#' Functions that return the location of different datasets. They check that
#' relevant file exist and then return it with a class that has a neat print method.
#'
#'
#' @param type character indicating the type of data. The resulting base will be type_data
#' @param ... characters to be concatenated with [file.path()]
#'
#' @export
#' @rdname data_locations
data_path <- function(type = c("raw", "derived"), ...) {
  here::here("analysis", "data", paste0(type[1], "_data"), ...)
}


download_from_source <- function(data) {
  file <- data$source(data$file)
  if (!is.null(data$citation)) {
    write_citation(data$citation)
  }

  file
}

download_from_zenodo <- function(data) {
  utils::download.file(data$zenodo_url, data$file)
}


data_function <- function(name,
                          file,
                          source,
                          zenodo_url,
                          zenodo_md5,
                          citation) {
  if (is.character(source)) {
    source <- simple_download(source)
  }

  data <- list(
    name = name,
    file = file,
    source = source,
    zenodo_url = zenodo_url,
    zenodo_md5 = zenodo_md5,
    citation = citation
  )

  function(from_source = FALSE) {
    will_download <- from_source == TRUE || !file.exists(data$file)

    if (will_download) {
      if (from_source || is.null(data$zenodo_url)) {
        message("Downloading dataset ", data$name, " from remote source.")
        download_from_source(data)
      } else {
        message("Downloading dataset ", data$name, " from zenodo repository.")
        download_from_zenodo(data)
      }
    }

    if (!is.null(data$zenodo_md5)) {
      md5 <- digest::digest(data$file, "md5", file = TRUE)
      if (data$zenodo_md5 != md5) {
        warning("File for dataset ", data$name, " has incorrect checksum compared to the Zenodo reference.")
      }
    }

    data$file
  }

}


#' @param from_source Logical indicating whether to force download of data from the
#' remote source instead of the Zeonodo repository.
#'
#' @details
#' Each dataset has two potential sources, either the original source or the
#' Zenodo repository associated with this paper. The default behaviour is to
#' download the file from Zenodo if the file does not exist. This will
#' get the exact files that where used for the paper.
#' If `from_source = TRUE`, then the function will fetch the data from the
#' primary source (e.g. Climate Data Store) and save the file where it should be.
#'
#' In either case, the file hash will be compared with a reference hash and warn
#' if there are differences.
#'
#' @export
#' @rdname data_locations
ERA5_TOC <- data_function(
  name = "era5-total_column_ozone",
  file = data_path("raw", "era5.toc.mon.mean.nc"),
  source = download_cds(request_toc),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5.toc.mon.mean.nc?download=1",
  zenodo_md5 = "71c56bc40022026da943c35fb4983941",
  citation = citation_o3_era5
)

#' @export
#' @rdname data_locations
ERA5_geopotential <- data_function(
  name = "era5-geopotential",
  file = data_path("raw", "era5.z.mon.mean.nc"),
  source = download_cds(request_geopotential),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5.z.mon.mean.nc?download=1",
  zenodo_md5 = "78cb847b37f05536da0a8fa7422a7c6f",
  citation = citation_era5
)

#' @export
#' @rdname data_locations
ERA5_geopotential_all <- data_function(
  name = "era5-geopotential-all",
  file = data_path("raw", "era5-all.z.mon.mean.nc"),
  source = download_cds(request_geopotential_all),
  zenodo_url = NULL,
  zenodo_md5 = NULL,
  citation = citation_era5
)



#' @export
#' @rdname data_locations
ERA5_temperature <- data_function(
  name = "era5-air_temperature",
  file = data_path("raw", "era5.air.mon.mean.nc"),
  source = download_cds(request_temperature),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5.air.mon.mean.nc?download=1",
  zenodo_md5 = "ebabdb3760c5999f4e7360f1d9c93b9e",
  citation = citation_era5
)

#' @export
#' @rdname data_locations
ERA5_vorticity <- data_function(
  name = "era5-vorticity",
  file = data_path("raw", "era5.vor.mon.mean.nc"),
  source = download_cds(request_vorticity),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5.vor.mon.mean.nc?download=1",
  zenodo_md5 = "ba65b25c763ed6541db9d8e89bf68a05",
  citation = citation_era5
)

#' @export
#' @rdname data_locations
ERA5_ozone <- data_function(
  name = "era5-ozone",
  file = data_path("raw", "era5.o3.mon.mean.nc"),
  source = download_cds(request_ozone),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5.o3.mon.mean.nc?download=1",
  zenodo_md5 = "0055f304426c700d91be68be38df0b83",
  citation = citation_era5
)

#' @export
#' @rdname data_locations
ERA5_BE <- data_function(
  name = "era5-back_extended-geopotential",
  file = data_path("raw", "era5be.mon.mean.nc"),
  source = download_cds(request_era5be),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5be.mon.mean.nc?download=1",
  zenodo_md5 = "7bebebf781bcdb045fc2a7735ef90629",
  citation = citation_era5be
)


#' @export
#' @rdname data_locations
ERA5_T2M <- data_function(
  name = "era5-2_metre_temperature",
  file = data_path("raw", "era5.2mt.mon.mean.nc"),
  zenodo_url = "https://zenodo.org/record/6612324/files/era5.2mt.mon.mean.nc?download=1",
  zenodo_md5 = "bc6cb57caee814847374e47ca2f22fe2",
  source = download_cds(request_era5_2mt),
  citation = citation_era5
)


#' @export
#' @rdname data_locations
NCEP_PSI <- data_function(
  name = "ncep-streamfunction",
  file = data_path("raw", "psi.mon.mean.nc"),
  zenodo_url = NULL,
  zenodo_md5 = NULL,
  source = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/psi.mon.mean.nc",
  citation = NULL
)

#' #' @export
#' #' @rdname data_locations
#' NCEP_VOR <- data_nc_fun(file =  data_path("raw", "vor.mon.mean.nc"),
#'                         download = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/vor.mon.mean.nc")

#' @export
#' @rdname data_locations
CMAP <- data_function(
  name = "cmap-precipitation",
  file = data_path("raw", "cmap.mon.mean.nc"),
  source = "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc",
  zenodo_url = "https://zenodo.org/record/6612324/files/cmap.mon.mean.nc?download=1",
  zenodo_md5 = "c003b8bb4d277418a5e7fa235828949b",
  citation = citation_cmap
)



#' @export
#' @rdname data_locations
ERSST <- data_function(
  name = "ERSST",
  file = data_path("raw", "ersst.csv"),
  source = download_ersst,
  zenodo_url = "https://zenodo.org/record/6612324/files/ersst.csv?download=1",
  zenodo_md5 = "fae3de4b4a0128b2a4b738d5cebd7094",
  citation = NULL
)

#' @export
#' @rdname data_locations
ENSO <- data_function(
  name  = "ENSO-oceanic_enso_index",
  file = data_path("raw", "oni.csv"),
  source = function(file) {
    data <- rsoi::download_oni(use_cache = FALSE)
    data.table::fwrite(
      data.frame(
        time = lubridate::as_datetime(data$Date),
        oni = data$ONI
      ),
      file = file
    )
    file
  },
  zenodo_url = "https://zenodo.org/record/6612324/files/oni.csv?download=1",
  zenodo_md5 = "98d31a4984f1bac522717e4d303c890c",
  citation = NULL
)

#' @export
#' @rdname data_locations
DMI <- data_function(
  name  = "DMI",
  file = data_path("raw", "dmi.csv"),
  source = function(file) {
    data <- rsoi::download_dmi(use_cache = FALSE)

    data.table::fwrite(
      data.frame(
        time = lubridate::as_datetime(data$Date),
        dmi = data$DMI
      ),
      file = file
    )

    file
  },
  zenodo_url = "https://zenodo.org/record/6612324/files/dmi.csv?download=1",
  zenodo_md5 = "9e09ae2d9b6a260c78d76c80d3b4b9bf",
  citation = NULL
)

#' @export
#' @rdname data_locations
SAM <- data_function(
  name = "sam",
  file = data_path("derived", "sam.csv"),
  source = function(file) {
    check_asymsam <- requireNamespace("asymsam", quietly = TRUE)
    if (!check_asymsam) {
      stop("Computing the Asymmetric and Symmetric SAM requires package asymsam.\n",
           "Install it with remotes::install_github(\"eliocamp/asymsam\").")
    }

    sams <- ERA5_geopotential_all() %>%
      metR::ReadNetCDF(vars = c("hgt" = "z"),
                       subset = list(latitude = c(-90, -20))) %>%
      normalise_coords() %>%
      .[, hgt := hgt - mean(hgt), by = .(lon, lat, lev, month(time))] %>%
      .[, asymsam::eof_asym(hgt, lon, lat, time), by = lev] %>%
      rm_singleton()

    data.table::fwrite(sams, file)
    file
  },
  zenodo_url = "https://zenodo.org/record/6612324/files/sam.csv?download=1",
  zenodo_md5 = "c46a278527ba97686172ca2f1a7e4469",
  citation = NULL
)
