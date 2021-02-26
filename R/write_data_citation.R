write_citation <- function(citation) {
  date <- format(Sys.Date(), "%d-%m-%Y")
  text <- citation(date)

  lines <- strsplit(text, "\n")[[1]]
  line_start <- lines[1]
  line_end <- rev(lines)[1]

  file <- here::here("analysis", "paper", "data.bib")

  if (!file.exists(file)) {
    file.create(file)
  }

  lines <- readLines(file)
  start <- which(lines == line_start)
  end <- which(lines == line_end)

  if (length(end) != 0 & length(start) != 0) {
    lines <- lines[-seq(start, end)]
  }

  lines <- c(lines, text)
  lines <- lines[nchar(lines) != 0]

  unlink(file)
  file.create(file)
  writeLines(lines, file)
  return(invisible(file))
}


citation_era5be <- function(date) {
  paste0(
r"(--start ERA5BE
@article{era5be,
  title = {{{ERA5}} Monthly Averaged Data on Pressure Levels from 1950 to 1978 (Preliminary Version)},
  author = {Bell, B. and Hersbach, H. and  Berrisford, P. and  Dahlgren, P. and Horányi, A. and Muñoz Sabater, J. and Nicolas, J. and Radu, R. and Schepers, D. and Simmons, A. and Soci, C. and Thépaut, J-N.},
  year = {2020},
  journal = {{{Copernicus Climate Change Service}} ({{C3S}}) {{Climate Data Store}} ({{CDS}})},
  volume = {(Accessed on {$<$})", date, r"({$>$}), https://cds.climate.copernicus.eu/cdsapp\#!/dataset/reanalysis-era5-pressure-levels-monthly-means-preliminary-back-extension?tab=overview}
}
--end ERA5BE
)"
  )
}

citation_era5 <- function(date) {
  paste0(
r"(--start ERA5
@article{era5,
    title = {{{ERA5}} monthly averaged data on pressure levels from 1979 to present},
    author = {Hersbach, H. and Bell, B. and Berrisford, P. and Biavati, G. and Horányi, A. and Muñoz Sabater, J. and Nicolas, J. and Peubey, C. and Radu, R. and Rozum, I. and Schepers, D. and Simmons, A. and Soci, C. and Dee, D. and Thépaut, J-N.},
    year = {2019},
    journal = {{{Copernicus Climate Change Service}} ({{C3S}}) {{Climate Data Store}} ({{CDS}})},
    volume = {(Accessed on {$<$})", date, r"({$>$})},
    doi = {10.24381/cds.6860a573}
    }
--end ERA5
)"
  )
}

citation_cmap <- function() {
  paste0(
r"(--start CMAP
@article{cmap,
    title = {Global {{Precipitation}}: {{A}} 17-{{Year Monthly Analysis Based}} on {{Gauge Observations}}, {{Satellite Estimates}}, and {{Numerical Model Outputs}}},
    shorttitle = {Global {{Precipitation}}},
    author = {Xie, Pingping and Arkin, Phillip A.},
    year = {1997},
    month = nov,
    volume = {78},
    pages = {2539--2558},
    publisher = {{American Meteorological Society}},
    issn = {0003-0007},
    doi = {10.1175/1520-0477(1997)078<2539:GPAYMA>2.0.CO;2},
    file = {/home/elio/Dropbox/Papers/Xie et al_Global Precipitation_Global Precipitation.pdf},
    journal = {Bull. Amer. Meteor. Soc.},
    language = {en},
    number = {11}
}
--end CMAP
)"
  )
}

