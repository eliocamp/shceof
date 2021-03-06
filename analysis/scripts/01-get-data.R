library(shceof)

force <- FALSE

# Download CMAP -----------------------------------------------------------
CMAP(force)

# Download HadSST -----------------------------------------------------------
HADSST(force)

# Download Streamfunction NCEP ----------------------------------------------------
NCEP_PSI(force)

# Download Vorticity NCEP ----------------------------------------------------
NCEP_VOR(force)

# Download ERA5 -----------------------------------------------------------
ERA5(force)
ERA5_BE(force)
ERSST(force)

# Download ERA20C -----------------------------------------------------------
ERA20C(force)

# Download O3 --------------------------------
O3(force)
