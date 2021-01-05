subset = list(lat = -90:10)
levels = c(50, 200, 500)

years <- seq(1979, 2018)

uv <- data.table::rbindlist(lapply(years, function(y) {
  cat("Processing year ", y, "\r")
  ufile <- paste0("datos/NCEP Reanalysis/daily/uwnd.", y, ".nc")
  vfile <- paste0("datos/NCEP Reanalysis/daily/vwnd.", y, ".nc")
  
  wnd <- metR::ReadNetCDF(ufile, "uwnd", subset = subset)
  wnd[, vwnd := metR::ReadNetCDF(vfile, "vwnd", subset = subset, out = "vector")[[1]]]
  wnd <- wnd[level %in% levels]
  
  wnd[, year := year(time[1]), by = time]
  wnd[, season := metR::season(time[1]), by = time]
  
  wnd[, .(uv = cov(uwnd, vwnd)), by = .(level, lon, lat, year, season)]
}))

data.table::setnames(uv, "level", "lev")
uv[, dataset := "ncep"]
cat("Saving data.")
saveRDS(uv, "datos/uv.Rds")

