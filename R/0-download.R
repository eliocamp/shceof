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


    files <- unzip(file_zip, exdir = tempdir())

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
  variable = c("geopotential", "temperature", "vorticity"),
  pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50", "70", "100", "125", "150", "175", "200", "225", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "775", "800", "825", "850", "875", "900", "925", "950", "975", "1000"),
  year = as.character(1950:1978),
  month = c("09", "10", "11"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means-preliminary-back-extension"
)


request_era5 <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("geopotential", "temperature", "vorticity", "ozone_mass_mixing_ratio"),
  pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
                     "70", "100", "125", "150", "175", "200", "225",
                     "250", "300", "350", "400", "450", "500", "550",
                     "600", "650", "700", "750", "775", "800", "825",
                     "850", "875", "900", "925", "950", "975", "1000"),
  year = as.character(1979:2020),
  month = formatC(1:12, width = 2, flag = 0),   # Need all months to compute PSA
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
)

request_o3_era5 <- list(
  format = "netcdf",
  variable = "total_column_ozone",
  product_type = "monthly_averaged_reanalysis",
  year = as.character(1979:2020),
  month = formatC(9:11, width = 2, flag = 0),
  time = "00:00",
  area = c(0, -180, -90, 180),
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
  target = "ozone.era5.nc"
)


request_era5_sst <- list(
  format = "netcdf",
  variable = "sea_surface_temperature",
  product_type = "monthly_averaged_reanalysis",
  year = as.character(1979:2020),
  month =  c("09", "10", "11"),
  time = "00:00",
  grid = c("2.5", "2.5"),
  dataset_short_name = "reanalysis-era5-single-levels-monthly-means"
)




dates <- seq(as.Date("1900-01-01"), as.Date("2010-12-01"), by = "1 month")
dates <- dates[data.table::month(dates) %in% c(9:11)]
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

rm(dates)

request_o3 <- list(
  format = "zip",
  processing_level = "level_4",
  variable = "atmosphere_mole_content_of_ozone",
  vertical_aggregation = "total_column",
  sensor = "msr",
  year = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
  version = c("v0020", "v0021", "v0022"),
  month = c("09", "10", "11"),
  dataset_short_name = "satellite-ozone-v1",
  target = "ozone.zip"
)



simple_download <- function(url) {
  force(url)
  function(file){
    download.file(url, destfile = file)
    file
  }
}


