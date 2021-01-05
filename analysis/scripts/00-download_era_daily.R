request <- list(
  class    = "ei",
  dataset  = "interim",
  date     = "1979-01-01/to/2018-12-31",
  expver   = "1",
  grid     = "2.5/2.5",
  levelist = "500",
  levtype  = "pl",
  param    = "131.128/132.128",
  step     = "0",
  stream   = "oper",
  time     = "00:00:00/06:00:00/12:00:00/18:00:00",
  type     = "an",
  target   = "u_v_daily.nc",
  format   = "netcdf"
)

ecmwfr::wf_request(request, job_name = "era_daily")
