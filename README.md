# shceof

This repository contains data, code and other source files associated with a paper (title TBD)

Clone this repo. If using RStudio, open the project. 

This project uses the [renv](https://rstudio.github.io/renv/) package to manage a reproducible environment. If opening this project with RStudio or starting R from the command line from the root directory, renv should automatically install and load itself. 

To recreate the environment then run

```r
renv::restore()
```

This should install all the package dependencies needed to install the package and compile the document. 

Then install this package with

```r
if (!require("devtools")) {
   install.packages("devtools")
}
devtools::install()
```

The main manuscript is located at `analysis/paper/paper.Rmd`. Knitting it should work, including automatically downloading the relevant datasets. However, bear in mind that this could literally take hours, since many datasets are downloaded from the [Climate Data Service](https://cds.climate.copernicus.eu/), which does some conversion. 
