#' Downloads DMI
#'
#' From https://github.com/boshek/rsoi/pull/43
#'
#' @export
download_dmi <-  function() {
  dmi_link = "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data"

  res = tryCatch(
    rsoi:::check_response(dmi_link),
    error = function(e) {
      message(e)
      return(invisible(NULL))
    }
  )

  years = strsplit(trimws(readLines(res, n = 1)), " ")[[1]]
  rows = diff(as.numeric(years)) + 1
  months = month.abb

  dmi =  read.csv(rsoi:::check_response(dmi_link),    # reset connection
                  header = FALSE,
                  col.names = c("Year", months),
                  nrows = rows,
                  skip = 1,
                  na.strings = "-9999.000",
                  sep = "",
                  stringsAsFactors = FALSE)

  grid = expand.grid(Year = dmi$Year, Month = months)
  grid$DMI = c(as.matrix(dmi[, months]))
  grid$Month = factor(grid$Month, levels = months)
  grid$Date = as.Date(paste0(grid$Year, "-", as.numeric(grid$Month) ,"-01"), "%Y-%m-%d")

  grid <- grid[, c("Year", "Month", "Date", "DMI")]
  class(grid) = c("tbl_df", "tbl", "data.frame")

  return(grid)
}
