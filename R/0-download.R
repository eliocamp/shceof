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
  variable = c("geopotential", "temperature", "vorticity"),
  pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50", "70", "100", "125", "150", "175", "200", "225", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "775", "800", "825", "850", "875", "900", "925", "950", "975", "1000"),
  year = as.character(1950:1978),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means-preliminary-back-extension"
)


request_era5 <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("geopotential", "temperature", "vorticity"),
  pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
                     "70", "100", "125", "150", "175", "200", "225",
                     "250", "300", "350", "400", "450", "500", "550",
                     "600", "650", "700", "750", "775", "800", "825",
                     "850", "875", "900", "925", "950", "975", "1000"),
  year = as.character(1979:2019),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)

request_era5_sst <- list(
  format = "netcdf",
  variable = "sea_surface_temperature",
  product_type = "monthly_averaged_reanalysis",
  year = as.character(1979:2019),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-single-levels-monthly-means"
)



dates <- seq(as.Date("1900-01-01"), as.Date("2010-12-01"), by = "1 month")
dates <- paste0(gsub("-", "", as.character(dates)), collapse = "/")

request_era20c <- list(
  class    = "e2",
  dataset  = "era20c",
  date     = dates,
  expver   = "1",
  levelist = "50/200",
  levtype  = "pl",
  param    = "129.128",
  grid     =  c("2.5", "2.5"),
  stream   = "moda",
  type     = "an",
  format   = "netcdf"
)

simple_download <- function(url) {
  force(url)
  function(file){
    download.file(url, destfile = file)
    file
  }
}
