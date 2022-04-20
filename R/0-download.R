southern_hemisphere <- c(20, 0, -90, 360)

download_cds <- function(request) {
  force(request)
  function(file) {
    user <- Sys.getenv("CDSUSER", NA)
    key <- Sys.getenv("CDSKEY", NA)
    filename <- basename(file)

    if (is.na(user) | is.na(key)) {
      stop("Please set the environmental variables CDSUSER and CDSKEY")
    }

    request$target <- filename

    ecmwfr::wf_set_key(user = user,
                       key = key,
                       service = "cds")

    files <- ecmwfr::wf_request(request = request,
                                user = user,
                                time_out = 5*3600,
                                path = dirname(file))
    file
  }
}


download_ozone <- function(request) {
  force(request)
  function(file) {
    user <- Sys.getenv("CDSUSER", NA)
    key <- Sys.getenv("CDSKEY", NA)
    filename <- basename(file)

    if (is.na(user) | is.na(key)) {
      stop("Please set the environmental variables CDSUSER and CDSKEY")
    }


    ecmwfr::wf_set_key(user = user,
                       key = key,
                       service = "cds")

    file_zip <- ecmwfr::wf_request(request = request,
                                user = user,
                                time_out = 5*3600,
                                path = tempdir())


    files <- utils::unzip(file_zip, exdir = tempdir())

    data <- do.call(rbind, lapply(files, metR::ReadNetCDF))
    data <- normalise_coords(data)
    data[, lon := metR::ConvertLongitude(lon)]

    saveRDS(data, file)
    file
  }
}

download_webapi <- function(request) {
  force(request)
  function(file) {
    user <- Sys.getenv("WEBAPIUSER", NA)
    key <- Sys.getenv("WEBAPIKEY", NA)
    filename <- basename(file)

    if (is.na(user) | is.na(key)) {
      stop("Please set the environmental variables CDSUSER and CDSKEY")
    }

    request$target <- filename

    ecmwfr::wf_set_key(user = user,
                       key = key,
                       service = "webapi")

    files <- ecmwfr::wf_request(request = request,
                                user = user,
                                time_out = 5*3600,
                                path = dirname(file))
    file
  }
}


request_era5be <- list(
  format = "netcdf",
  product_type = "reanalysis-monthly-means-of-daily-means",
  variable = c("geopotential"),
  pressure_level = c("50", "200"),
  year = as.character(1950:1978),
  month = c("09", "10", "11"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  area = southern_hemisphere,
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means-preliminary-back-extension"
)


request_geopotential <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("geopotential"),
  pressure_level = c("50", "200", "500", "850"),
  year = as.character(1979:2020),
  month = formatC(1:12, width = 2, flag = 0),   # Need all months to compute PSA
  time = "00:00",
  grid = c("2.5", "2.5"),
  area = southern_hemisphere,
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)


request_geopotential_all <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("geopotential"),
  year = as.character(1979:2020),
  month = formatC(1:12, width = 2, flag = 0),   # Need all months to compute PSA
  time = "00:00",
  grid = c("2.5", "2.5"),
  area = southern_hemisphere,
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)




request_vorticity <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("vorticity"),
  pressure_level = c("200"),
  year = as.character(1979:2020),
  month = formatC(9:11, width = 2, flag = 0),
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)


request_temperature <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("temperature"),
  pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
                     "70", "100", "125", "150", "175", "200", "225",
                     "250", "300", "350", "400", "450", "500", "550",
                     "600", "650", "700", "750", "775", "800", "825",
                     "850", "875", "900", "925", "950", "975", "1000"),
  year = as.character(1979:2020),
  month = formatC(9:11, width = 2, flag = 0),
  time = "00:00",
  grid = c("2.5", "2.5"),
  area = southern_hemisphere,
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)

request_ozone <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("ozone_mass_mixing_ratio"),
  pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
                     "70", "100", "125", "150", "175", "200", "225",
                     "250", "300", "350", "400", "450", "500", "550",
                     "600", "650", "700", "750", "775", "800", "825",
                     "850", "875", "900", "925", "950", "975", "1000"),
  year = as.character(1979:2020),
  month = formatC(9:11, width = 2, flag = 0),
  time = "00:00",
  grid = c("2.5", "2.5"),
  area = southern_hemisphere,
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)

request_toc <- list(
  format = "netcdf",
  variable = "total_column_ozone",
  product_type = "monthly_averaged_reanalysis",
  year = as.character(1979:2020),
  month = formatC(9:11, width = 2, flag = 0),
  time = "00:00",
  area = southern_hemisphere,
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
  target = "ozone.era5.nc"
)


request_era5_2mt <- list(
  format = "netcdf",
  variable = c("2m_temperature"),
  product_type = "monthly_averaged_reanalysis",
  year = as.character(1979:2020),
  month =  c("09", "10", "11"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  area = southern_hemisphere,
  dataset_short_name = "reanalysis-era5-single-levels-monthly-means"
)



simple_download <- function(url) {
  force(url)
  function(file){
    utils::download.file(url, destfile = file)
    file
  }
}


download_ersst <- function(file) {
  base_url <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5."

  years <- 1979:2020
  months <- formatC(9:11, width = 2, flag = "0")
  dates <- data.table::CJ(years, months)[, paste0(years, months)]

  urls <- paste0(base_url, dates, ".nc")

  files <- vapply(urls, function(url) {
    file <- tempfile(fileext = ".nc")
    utils::download.file(url, file)
    file
  }, character(1))

  data <- lapply(files, function(file) metR::ReadNetCDF(file, vars = c(t = "sst")))
  data <- data.table::rbindlist(data)

  saveRDS(data, file)
  file
}
