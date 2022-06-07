# https://cliflo.niwa.co.nz/pls/niwp/wgenf.genform1


file <- data_path("raw", "wgenf.genform1_proc.txt")


stations <- fread(file, skip = 1, nrows = 17, na.strings = "N/A",
                  col.names = c("name", "agent_number", "network_number", "lat", "lon", "height", "pos", "auth")) %>%
  .[, lon := ConvertLongitude(lon)]

variable_codes <-  fread(file, skip = 24, nrow = 4)


data <- fread(file, skip = 34, na.strings = "-", nrow = 1251 - 36) %>%
  .[!(Stats_Code %in%  c("83", "83_date"))] %>%
  .[, -"Annual"] %>%
  melt(id.vars = c("Station", "Stats_Code", "Year")) %>%
  .[, month := setNames(1:12, month.abb)[variable]] %>%
  .[, time := lubridate::make_datetime(Year, month)] %>%
  .[, .(agent_number = Station, code = Stats_Code, time, value)]


uv2 <- fread(data_path("raw", "NILU-UV_2000-01-01_2010-12-31_020321203210.txt"), skip = 2,
             col.names = c("time", "uv_marambio", "uvmax_marambio", "uv_ushuaia", "uvmax_ushuaia")) %>%
  melt(id.vars = "time") %>%
  tidyr::separate(variable, c("variable", "station")) %>%
  tidyr::complete(variable, station, time = seq(min(time), max(time), "1 day")) %>%
  as.data.table() %>%
  .[!is.finite(value), value := NA]






data[code == "84"] %>%
  ggplot(aes(time, value)) +
  geom_line() +
  facet_wrap(~agent_number)


uv <- data[code == "84"] %>%
  tidyr::complete(agent_number, code, time = seq(min(time), max(time), "1 month")) %>%
  as.data.table() %>%
  .[, .(value = mean(value, na.rm = TRUE),
        n = sum(!is.na(value))), by = .(agent_number, code, time = seasonally(time))] %>%
  .[n > 2] %>%
  .[season(time) == "SON"] %>%
  .[, n := .N, by = .(code, agent_number)] %>%
  .[n > 20]

uv_eof <- ceof$eof[[1]]$left %>%
  .[uv, on = "time", allow.cartesian = TRUE] %>%
  na.omit()


sep_ReIm() %>%
  .[, correlate(value, hgt), by = .(agent_number, PC, part)] %>%
  stations[., on = "agent_number"] %>%
  ggplot(aes(lon, lat)) +
  geom_point(aes(color = estimate, size = estimate^2)) +
  geom_point(shape = 21, size = 3, data= ~.x[p.value < 0.05]) +
  geom_qmap(~.x %>% add_continent() %>% .[continent == "oceania"]) +
  facet_grid(PC ~ part)



angles <- seq(-pi, pi, by = .5*pi/180)

rotations_uv <- lapply(angles, function(a) {
  uv_eof %>%
    copy() %>%
    .[, hgt := rotate(hgt, a)] %>%
    sep_ReIm() %>%
    .[, correlate(value, hgt), by = .(agent_number, PC, part)] %>%
    .[, rotation := a]
}) %>%
  rbindlist()


rotations_uv %>%
  ggplot(aes(rotation, p.value)) +
  geom_line(aes(color = part)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0.05) +
  facet_grid(PC ~ agent_number)


uv2 %>%
  .[, .(value = mean(value, na.rm = TRUE),
        n = mean(is.na(value))), by = .(variable, station, time = lubridate::round_date(time, "1 month"))] %>%
  .[n > 0.1, value := NA] %>%
  .[ceof$eof[[1]]$left, on = "time", allow.cartesian = TRUE] %>%
  na.omit() %>%
  sep_ReIm() %>%
  .[, correlate(value, hgt), by = .(station, variable, PC, part)]



uv_eof <- uv2 %>%
  .[, .(value = mean(value, na.rm = TRUE),
        n = mean(is.na(value))), by = .(variable, station, time = seasonally(time))] %>%
  .[n > 0.1, value := NA] %>%
  .[ceof$eof[[1]]$left, on = "time", allow.cartesian = TRUE] %>%
  na.omit()





angles <- seq(-pi, pi, by = .5*pi/180)

rotations_uv <- lapply(angles, function(a) {
  uv_eof %>%
    copy() %>%
    .[, hgt := rotate(hgt, a)] %>%
    sep_ReIm() %>%
    .[, correlate(value, hgt), by = .(station, variable, PC , part)] %>%
    .[, rotation := a]
}) %>%
  rbindlist()





rotations_uv %>%
  .[variable == "uv"] %>%
  ggplot(aes(rotation, estimate)) +
  geom_line(aes(color = part)) +
  geom_vline(xintercept = 0) +
  # geom_hline(yintercept = 0.05) +
  facet_grid(PC ~ station)




lag_correlation <- function(x, y, lags) {

  lapply(lags, function(lag) {
    y_shifted <- shift(y, lag)

    as.data.table(correlate(x, y_shifted))[, lag := ..lag]
  }) %>%
    rbindlist()

}



uv_month <- uv2 %>%
  .[, .(value = mean(value, na.rm = TRUE),
        n = mean(is.na(value))), by = .(variable, station, time = lubridate::round_date(time, "1 month"))] %>%
  .[n > 0.1, value := NA] %>%
  .[, value_a := Anomaly(value, na.rm = TRUE), by = .(variable, station, month(time))]


ceof$eof[[1]]$left %>% copy() %>%
  .[, year := year(time)] %>%
  .[uv_month, on = "year", allow.cartesian = TRUE]



uv_month[ceof$eof[[1]]$left, on = "time", allow.cartesian = TRUE] %>%
  na.omit()

lags <- -10:10

angles <- seq(-pi, pi, by = 5*pi/180)
lagged <- lapply(angles, function(a) {
  lapply(lags, function(lag) {

    uv_month %>%
      copy() %>%
      .[, value := shift(value, lag), by = .(station, variable)] %>%
      .[ceof$eof[[1]]$left, on = "time", allow.cartesian = TRUE] %>%
      na.omit() %>%
      .[, hgt := rotate(hgt, a)] %>%
      sep_ReIm() %>%
      .[, correlate(value, hgt), by = .(variable, station, PC, part)] %>%
      .[, lag := ..lag] %>%
      .[, rotation := ..a]


  }) %>%
    rbindlist()
}) %>%
  rbindlist()


lagged %>%
  .[variable == "uv"] %>%
  .[station == "ushuaia"] %>%
  .[part == "Real"] %>%
  ggplot(aes(rotation, lag)) +
  geom_contour_fill(aes(z = estimate^2)) +
  geom_contour_pval(aes(z = p.value), p.value = 0.05) +
  facet_grid(part ~ PC)



lagged %>%
  .[variable == "uv"] %>%
  .[station == "ushuaia"] %>%
  .[rotation == 0] %>%
  ggplot(aes(lag, estimate)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = low, ymax = hig, color = station), alpha = 0.3) +
  geom_line(aes(color = station)) +
  facet_grid(part ~ PC)



lagged %>%
  .[variable == "uv"] %>%
  .[station == "marambio"] %>%
  .[part == "Real"] %>%
  ggplot(aes(rotation, lag)) +
  geom_contour_fill(aes(z = estimate^2)) +
  geom_contour_pval(aes(z = p.value), p.value = 0.05) +
  facet_grid(part ~ PC)



lagged %>%
  .[variable == "uv"] %>%
  .[station == "marambio"] %>%
  .[rotation == 0] %>%
  ggplot(aes(lag, estimate)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = low, ymax = hig, color = station), alpha = 0.3) +
  geom_line(aes(color = station)) +
  facet_grid(part ~ PC)



angles <- seq(-pi, pi, by = 5*pi/180)
lags <- -12:12
lagged_mag <-  lapply(lags, function(lag) {
  uv_month %>%
    copy() %>%
    .[, value_a := shift(value_a, lag), by = .(station, variable)] %>%
    .[, time_orginal := shift(time, lag), by = .(station, variable)] %>%
    .[ceof$eof[[1]]$left, on = "time", allow.cartesian = TRUE] %>%
    .[is.finite(value_a)] %>%
    .[, month := month(time_orginal)] %>%
    .[, correlate(value_a, Mod(hgt)), by = .(variable, station, PC, month)] %>%
    .[, lag := ..lag]


}) %>%
  rbindlist()



lagged_mag %>%
  .[variable == "uv"] %>%
  # .[station == "marambio"] %>%
  ggplot(aes(lag, estimate)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = low, ymax = hig, color = station), alpha = 0.3) +
  geom_line(aes(color = station)) +
  facet_grid(station ~ PC)

