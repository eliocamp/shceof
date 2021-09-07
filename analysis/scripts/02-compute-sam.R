sam_file <- data_path("derived", "sam.Rds")

if (file.exists(sam_file)) {
  message("Sam file already exists. Exiting.")
} else {
  check_asymsam <- requireNamespace("asymsam", quietly = TRUE)
  if (!check_asymsam) {
    stop("Computing the Asymmetric and Symmetric SAM requires package asymsam.\n",
         "Install it with remotes::install_github(\"eliocamp/asymsam\").")
  }

  library(shceof)
  library(metR)

  sams <- ERA5_geopotential_all() %>%
    ReadNetCDF(vars = c("hgt" = "z"),
               subset = list(latitude = c(-90, -20))) %>%
    normalise_coords() %>%
    .[, hgt := Anomaly(hgt), by = .(lon, lat, lev, month(time))] %>%
    .[, asymsam::eof_asym(hgt, lon, lat, time), by = lev] %>%
    rm_singleton()

  saveRDS(sams, sam_file)
}


