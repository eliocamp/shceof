# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/rstudio:4.1.3

# required
MAINTAINER Elio Campitelli <elio.campitelli@cima.fcen.uba.ar>

# Get and install system dependencies
RUN R -e "install.packages('remotes')" \
  && R -e "remotes::install_github('r-hub/sysreqs')" \
  && R -e "install.packages('tinytex'); tinytex::install_tinytex()"  # Install LaTeX

WORKDIR /home/rstudio/shceof

COPY DESCRIPTION DESCRIPTION
RUN R -e "system(sysreqs::sysreq_commands('DESCRIPTION', 'linux-x86_64-ubuntu-gcc'))" \
  && apt-get install --no-install-recommends -y libmagick++-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Get and install R packages to local library
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
RUN chown -R rstudio . \
  && sudo -u rstudio R -e 'renv::restore()'

