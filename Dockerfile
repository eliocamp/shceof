# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/rstudio:4.1.1 AS base

# required
MAINTAINER Elio Campitelli <elio.campitelli@cima.fcen.uba.ar>

RUN R -e "install.packages('remotes')" \
  && R -e "remotes::install_github('r-hub/sysreqs')"

WORKDIR /home/rstudio/shceof
COPY DESCRIPTION DESCRIPTION
RUN sudo apt update \
  && R -e "system(sysreqs::sysreq_commands('DESCRIPTION', 'linux-x86_64-ubuntu-gcc'))" \
  && apt install -y libmagick++-dev \
  && sudo -u rstudio R -e 'writeLines(.libPaths()[2], "~/lib")'

COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
RUN chown -R rstudio . \
  && sudo -u rstudio R -e 'renv::restore()'

COPY analysis/data analysis/data
