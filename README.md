# shceof

This repository contains data, code and other source files associated with a paper (title TBD)

Clone this repo. If using RStudio, open the project. 

This project uses the [renv](https://rstudio.github.io/renv/) package to manage a reproducible environment. If opening this project with RStudio or starting R from the command line from the root directory, renv should automatically install and load itself. 

To recreate the environment then run

```r
renv::restore()
```

This should install all the package dependencies needed to install the package and compile the document. Depending on your operating system, this could take a while. 

Then install this package with

```r
if (!require("devtools")) {
   install.packages("devtools")
}
devtools::install()
```

The main manuscript is located at `analysis/paper/paper.Rmd`. Knitting it should work, including automatically downloading the relevant datasets. However, bear in mind that this could literally take hours, since many datasets are downloaded from the [Climate Data Service](https://cds.climate.copernicus.eu/), which does some conversion. 

To get the data you'll need to set create a user and set up your credentials on your .Renviron (you can use `usethis::edit_r_environ()` to open the relevant file). On your [user page](https://cds.climate.copernicus.eu/user/) copy your UID and API Key and set them in the environmental variables CDSKEY and CDSUSER. By the end your .Renviron should have lines similar to these:

```
CDSKEY = "xxxxxxx-xxxxxx-xxxx-xxx-xxxxxxxx"
CDSUSER = "xxxx"
```

