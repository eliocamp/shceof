# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/rstudio:4.1.3

# required
MAINTAINER Elio Campitelli <elio.campitelli@cima.fcen.uba.ar>

WORKDIR /home/rstudio/shceof

# from sysreqs::sysreq_commands('DESCRIPTION', 'linux-x86_64-ubuntu-gcc')
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update  \
  && apt-get --no-install-recommends install -y \
  libssl-dev zlib1g-dev pandoc pandoc-citeproc libgeos-dev \
  libgeos++-dev make libmagic-dev gdal-bin libnetcdf-dev libxml2-dev \
  libmagick++-dev libproj-dev libgdal-dev libsodium-dev libicu-dev \
  imagemagick libudunits2-dev libcurl4-openssl-dev libpng-dev \
  libsecret-1-dev git-core \
  libmagick++-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Get and install R packages to local library
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN chown -R rstudio . \
  && sudo -u rstudio R -e 'renv::restore()'
