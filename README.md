# shceof

This repository contains data, code and other source files associated with a paper (title TBD)

First clone or download this repository with

```bash
git clone --depth 1 https://github.com/eliocamp/shceof.git
```

(This will download only the lasted version, without all the git history, which can get somehwat bloated.)


## Runing with docker

The easiest way to run this code is to use the docker environment, which comes not only with all the packages and system dependencies preinstalled, but also with the data already downloaded. 

[Install Docker](https://docs.docker.com/engine/install/) if you haven't already. Then, go to the folder in which you cloned the repository and run this line:

```bash
docker run --rm -p 8787:8787 -e DISABLE_AUTH=true -v $(pwd):/home/rstudio/shceof -v /home/rstudio/shceof/renv -v /home/rstudio/shceof/analysis/data eliocamp/shceof
```

Open your web browser to [localhost:8787](http://127.0.0.1:8787/) and you'll be welcomed by an RStudio session with a shceof folder with all that you need. And you can move [to the next section](#compiling-the-manuscript).

## Running locally

If you can't or don't want to use Docker, then you need to install all the requires packages. 

### Installing system dependencies

If running on linux, you'll probably need to install a bunch of system dependencies. On Ubuntu, this should be enough: 

```bash 
sudo apt-get -y update && sudo apt-get install -y libssl-dev zlib1g-dev libv8-dev pandoc pandoc-citeproc libgeos-dev libgeos++-dev default-jre-headless make gdal-bin libjq-dev libnetcdf-dev libxml2-dev libproj-dev libgdal-dev libsodium-dev protobuf-compiler libprotoc-dev imagemagick libicu-dev libudunits2-dev libcurl4-openssl-dev libpng-dev libsecret-1-dev git-core libprotobuf-dev libmagick++-dev
```

Otherwise, the easiest way to do it is with the sysreqs package. Open an R session in the repository folder and run this

```r
## install.packages('remotes')
remotes::install_github('r-hub/sysreqs')
sysreqs::sysreq_commands('DESCRIPTION')
```

This will print a command that you need to run in order to install all the system dependencies. (You might need to add libmagick++-dev to the list, which is not detected as a dependency for some reason.)

### Installing R packages

This project uses the [renv](https://rstudio.github.io/renv/) package to manage a reproducible environment. If opening this project with RStudio or starting R from the command line from the root directory, renv should automagically install and load itself. 

To recreate the environment then run

```r
renv::restore()
```

This should install all the package dependencies needed to install the package and compile the document. Depending on your operating system, this could take a while!


### Getting the data

To get the data you'll need to set create a user [here](https://cds.climate.copernicus.eu/user/register?destination=/). Once you have your user ready, to to your [user page](https://cds.climate.copernicus.eu/user/) copy your UID and API Key and set them in the environmental variables CDSKEY and CDSUSER. You can do this by creating a file called `.Renviron` on the root folder of this project with 

```
CDSUSER = "xxxx"
CDSKEY = "xxxxxxx-xxxxxx-xxxx-xxx-xxxxxxxx"
```

Note that your USER is **NOT** the user you use to authenticate on the website, but the 5 or 6 digit number you see on the "API key" section on [your user page](https://cds.climate.copernicus.eu/user/)

After that, the relevant datasets will be downlaoded the first time you compile the manuscript. However, bear in mind that this could literally take hours, since many datasets are downloaded from the [Climate Data Service](https://cds.climate.copernicus.eu/), which does some conversion.



## Compiling the manuscript

To compile this manuscript you need to install the associated package with 

```r
remotes::install_local()
```

The main manuscript is located at `analysis/paper/shceof.Rmd`. Knit it to compile de PDF.



