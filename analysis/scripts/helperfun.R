## Archivo con funciones

#Librerías
# library(ggplot2)
# library(ggforce)
# library(stringi)
# # library(ggthemes)
# library(magrittr)
# library(data.table)
# library(lubridate)
# library(akima)

# library(RColorBrewer)
library(compiler)
enableJIT(0)

# source(here::here("scripts/eof_methods.R"))

`%b%` <- function(x, y) data.table::`%between%`(x, y) 

# Mapa
BuildMap <- function(res = 1, smooth = 1, pm = 180,
                     countries = FALSE, ...) {
   # Caraga datos de mapas y cambia el meridiano principal, suaviza y cambia
   # resolución para plotear más rápido.
   # Entra:
   #   res: resolución (cuantos puntos se eliminan entre los que quedan)
   #   smooth: factor de suavifzado (ventana del promedio corrido)
   #   pm: longitud del meridiano central
   #   countries: ¿datos a nivel país?
   # Sale:
   #   un data.table con las coordenadas de cada polígono y su grupo
   library(data.table)
   if (countries) {
      library(rworldxtra)
      data("countriesLow")
      m <- countriesLow
   } else {
      library(rworldmap)
      data(coastsCoarse)
      m <- coastsCoarse
   }
   
   m <- as.data.table(fortify(m))
   m[, group := as.numeric(group)]
   m[, id := as.numeric(id)]
   
   # Cambio el prime meridian.
   m2 <- copy(m)
   m2[, long := long + 360]
   m2[, group := group + max(group) + 1]
   m <- rbind(m, m2)
   m <- m[long >= pm - 180 & long <= 180 + pm]
   
   m[, MO := max(order), by = group]
   
   # Suavizo.
   if (smooth > 1) {
      cut <- max(smooth, res)
      notsmooth <- m[, .N, by = group][N < cut + 4, group]
      m <- m[!(group %in% notsmooth)]    # saco los grupos muy chicos
      
      m[order != 1 & order != MO,
        `:=`(long = zoo::rollmean(long, smooth, fill = "extend"),
             lat = zoo::rollmean(lat, smooth, fill = "extend")),
        by = group]
   }
   # Bajo la resolución.
   suppressWarnings(m[, keep := c(1, rep(0, res - 1)), by = group])
   m <- m[order == 1 | order == MO | keep == 1, .SD, by = group]
   
   return(m)
}

geom_map2 <- function(map, size = 0.2, color = "gray50") {
   # Un geom_path con defaults copados para agregar mapas
   g <- geom_path(data = map, aes(long, lat, group = group),
                  inherit.aes = F, color = color, size = size)
   return(g)
}


# Repetir vector en negativo
fill_neg <- function(vector) {
   c(-vector[order(-vector)], vector)
}



# Nombres de los meses en español
month.abb_sp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
names(month.abb_sp) <- as.character(1:12)

names(month.abb) <- as.character(1:12)


# # Para interpolación en data table
# Interpolate.DT <- function(z, x, y, yo = unique(y), xo = unique(x), ...){
#    na <- is.na(z)
#    int <- akima::interp(x = x[!na], y = y[!na], z = z[!na], yo = yo, xo = xo, ...)
#    names <- c(deparse(substitute(x)),
#               deparse(substitute(y)),
#               deparse(substitute(z)))    # muy feo, sí
#    r <- with(int, {
#       grid <- expand.grid(x, y)
#       r <- list(grid[,1], grid[, 2], c(z))
#       names(r) <- names
#       return(r)
#    })
# }

# Función que hace autocorrelograma y su test según Anderson o large lag.
acf.sig <- function(x, lag.max=0.3*length(x), alpha = 0.05,
                    method=c("anderson","large.lag", "salas"), sided="one") {
   autocor <- acf(x, lag.max=lag.max, plot = F)$acf
   N <- length(x)
   e <- -1/(N-1)
   if (method[1]=="anderson"){
      var <- (N-2)/(N-1)^2
   } else if (method[1]=="large.lag"){
      var <- vector()
      for (i in 1:length(autocor)){
         v <- ifelse(i==1,1/N, 1/N * (1+2*sum(autocor[1:i-1]^2)))
         var<- c(var, v)
      }
   } else if (method[1]=="salas"){
      var <- vector()
      
      for (i in 1:length(autocor)){
         v <- (N-1-i)/(N-i)^2
         var<- c(var, v)
         e <- -1/(N-i)
      }
   }
   if (sided=="one"){
      a <- alpha
      q <- qnorm(a, lower.tail=F)
      sigupp <- e + sqrt(var)*q
      ret <- data.table(lag=0:lag.max, acf=autocor, sig.cut=sigupp)
   } else if (sided == "two"){
      a <- alpha/2
      q <- qnorm(a, lower.tail=F)
      sigupp <- e+sqrt(var)*q
      siginf <- e-sqrt(var)*q
      ret <- data.table(lag=0:lag.max, acf=autocor, upp.sig.cut=sigupp, low.sig.cut=siginf)
   }
   ret
}



# Convierte la salida de la función fft en un formato
# legible por humanos.

convert.fft <- function(cs, sample.rate=1, full=T) {
   distance.center <- function(c) Mod(c)
   angle <- function(c) Arg(c)
   is.even <- function(x) ceiling(x/2) == x/2
   N <- length(cs)
   if (full==T){
      nyq <- ifelse(is.even(N), N/2+1, (N+1)/2)
      cs <- cs[2:nyq]
   }
   NP <- length(cs)
   cs <- cs / N # normalize
   
   df <- data.frame(cycle    = 1:(NP),
                    freq     = 1:(NP) * sample.rate / N,
                    per      = N/(1:(NP) * sample.rate),
                    ampl = sapply(cs, Mod),
                    delay    = sapply(cs, angle),
                    spect    = sqrt(sapply(cs, Mod)),
                    comp = cs)
   
   non.unique <- ifelse(is.even(NP), NP-1, NP)
   df$ampl [1:non.unique] <- df$ampl[1:non.unique]*2
   df
}

# ¿por qué está esto acá? Es una función que convierte una columna con n factores
# en n columnas con 0 o 1. ¿No es un dcast? Que la pedo que estoy...
factor2cols <- function(x, column, factors) {
   column <- deparse(substitute(column))
   for(h in factors){
      x[, (h) := ifelse(get(column) == h, 1, 0)]
   }
   x[is.na(get(column)), (factors) := 0]
}

# convierte una fecha en formato AAAA-MM-DD en mes con factor y ordenado según estaciones
date_month2factor <- function(x) {
   factor(as.numeric(stringi::stri_sub(x, 6, 7)), levels = c(12, 1:11), ordered = T)
}

WaveFlux <- function(psi, p = 250, a = 6371000) {
   k <- p*100/(a^2*2000)
   psi <- copy(psi)
   psi[, c("psi.dlon", "psi.dlat") := Derivate(psi.z ~ lon + lat,
                                               cyclical = c(TRUE, FALSE))] %>%
      .[, psi.ddlon := Derivate(psi.z ~ lon, cyclical = TRUE, order = 2),
        by = lat] %>%
      .[, psi.dlondlat := Derivate(psi.dlon ~ lat),
        by = lon] %>%
      .[, `:=`(f.lon = k/cos(lat*pi/180)*(psi.dlon^2 - psi.z*psi.ddlon),
               f.lat = k*(psi.dlon*psi.dlat - psi.z*psi.dlondlat))]
   list(f.lon = psi$f.lon, f.lat = psi$f.lat)
}

guide_colorstrip_bottom <- function(width = 25, height = 0.5, ...) {
   guide_colorstrip(title.position = "top", title.hjust = 0.5,
                    barheight = height,
                    barwidth = width, ...)
}

scale_x_longitude <- function(ticks = 60, xwrap = c(0, 360), ...) {
   b <- seq(min(xwrap), max(xwrap), by = ticks)
   metR::scale_x_longitude(ticks = ticks, breaks = b, labels = LonLabel(b),...)
}
scale_s_map <- function(ylim = c(-90, -15), xlim = c(0, 360)) {
   list(scale_y_latitude(limits = ylim),
        scale_x_longitude(xwrap = xlim)) 
} 

ggplot <- function(...) {
   ggplot2::ggplot(...)# + scale_linetype(guide = "none")
}

AddSuffix <- function(suffix = "") {
   force(suffix)
   function(string) {
      paste0(string, suffix)
   }
}

AddPreffix <- function(preffix = "") {
   force(preffix)
   function(string) {
      paste0(preffix, string)
   }
}

lev.lab <- AddSuffix(" hPa")
qs.lab <- AddPreffix("QS ")


yearmonth <- function(date, day = 1) {
   months <- lubridate::month(date)
   years <- lubridate::year(date)
   lubridate::ymd(paste(years, months, day, sep = "-"))
}

yearly <- function(date, day = 182) {
   years <- lubridate::year(date)
   d <- lubridate::ymd(paste(years, "01", "01", sep = "-"))
   lubridate::yday(d) <- day
   d
}

geom_index.region <- function(data, color = "black") {
   geom_rect(data = data, aes(xmin = latmin, xmax = latmax,
                              ymin = levmin, ymax = levmax),
             inherit.aes = F, linetype = 3, color = color, fill = NA)
}


PeriodicWavelet <- function(x, k, normalize = FALSE) {
   period <- length(x)/k
   x1 <- rep(x, 3)
   keep <- (length(x)+1):(2*length(x))
   res <- list()
   if (isFALSE(normalize)) {
      mult <- sd(x) 
   } else {
      mult <- 1
   }
   for (p in seq_along(period)) {
      w <- WaveletComp::WaveletTransform(x1, dt = 1, upperPeriod = period[p],
                                         lowerPeriod = period[p])
      
      res[[paste0("k", ".", k[p])]] <- w$Ampl[keep]*mult
      
   }
   return(res)
}

ReconstructWavelet <- function(x, k) {
   period <- length(x)/k
   x1 <- rep(x, 3)
   keep <- (length(x)+1):(2*length(x))
   w <- WaveletComp::analyze.wavelet(data.frame(x1), make.pval = F,
                                     loess.span = 0, verbose = F)
   r <- WaveletComp::reconstruct(w, sel.period = period,
                                 plot.rec = F, verbose = F)$series$x1.r
   r[keep]
}


greater <- function(x, N) {
   r <- frank(-x, ties.method = "first")
   r <= N
}


decade <- function(year) {
   substr(year, 3, 4)
}

fsign <- function(x) {
   f <- sign(x)
   factor(ifelse(f == 0, NA, f))
}



ifelse2 <- function(x, expression, yes = NA, no = x) {
   e <- eval(parse(text = deparse(substitute(expression))), envir = environment())
   yes <- eval(parse(text = deparse(substitute(yes))), envir = environment())
   no <- eval(parse(text = deparse(substitute(no))), envir = environment())
   ifelse(e, yes, no)
}

geom_label_contour2 <- function(...) {
   list(geom_label_contour(fill = "white", label.r = unit(0, "lines"),
                           label.padding = unit(0.06, "lines"), color = NA, ...),
        geom_text_contour(..., rotate = FALSE))
}


cache.file <- function(file, expression, verbose = interactive()) {
   
   if (file.exists(file)) {
      if (verbose) message("Reading data from file.")
      return(readRDS(file))
   } else {
      if (verbose) message("Evaluating expression.")
      r <- eval(expression)
      if (verbose) message("Saving data to file.")
      saveRDS(r, file = file)
      return(r)
   }
}

mode.circular <- function(x, limits = c(0, 2/3*pi)) {
   if (length(x) > 1) {
      x1 <- c(x - limits[2], x, x + limits[2])
      keep <- (length(x) + 1):(2*length(x))
      d <- density(x1)
      y <- d$y[d$x %b% limits]
      x2 <- d$x[d$x %b% limits]
      x2[which.max(y)]
   } else {
      x
   }
}

# "Estaciones" en base a la amplitud y fase de la onda 3.
qs.sem <- function(month) {
   if (metR:::.is.somedate(month)) month <- lubridate::month(month)
   
   qs.seasons <- factor(c(rep("DJFMA", 4),
                          rep("MJJASON", 7),
                          rep("DJFMA", 1)))
   
   return(factor(qs.seasons[month], levels = c("DJFMA", "MJJASON")))
}



# "Estaciones" en base a la amplitud y fase de la onda 3.
qs.season <- function(month) {
   if (metR:::.is.somedate(month)) month <- lubridate::month(month)
   
   qs.seasons <- factor(c(rep("DJFM", 3),
                          rep("A", 1),
                          rep("MJJ", 3),
                          rep("ASO", 3),
                          rep("N", 1),
                          rep("DJFM", 1)))
   
   return(factor(qs.seasons[month], levels = c("DJFM", "A", "MJJ",
                                               "ASO", "N")))
}


# Trimestres
qs.trim <- function(month) {
   if (metR:::.is.somedate(month)) month <- lubridate::month(month)
   
   qs.seasons <- factor(c(rep("JFM", 3),
                          rep("AMJ", 3),
                          rep("JAS", 3),
                          rep("OND", 3)))
   
   return(factor(qs.seasons[month], levels = c("JFM", "AMJ",
                                               "JAS", "OND")))
}

qs.sem <- qs.trim


geom_contour_back <- function(..., color = "black", size = 0.2, alpha = 0.5) {
   geom_contour(..., color = color, size = size, alpha = alpha)
}
geom_label_contour_back <- function(...) {
   geom_label_contour2(..., alpha = 0.5, size = 3)
}

geom_contour_fine <- function(...) geom_contour(..., size = 0.4)
stat_contour_fine <- function(...) stat_contour(..., size = 0.4)

geom_cross <- function(x = 0, y = 0, ...) {
   list(geom_vline(xintercept = x, ...),
        geom_hline(yintercept = y, ...))
}

labeller.date <- function(sep = " - ") {
   function(s) {
      s <- as.Date(s)
      m <- month(s)
      y <- year(s)
      paste0(month.abb_sp[m], sep, y)
   }
}

no.zero_ <- function(x) {
   if (x == 0) return(".0")
   if (abs(x) < 1) {
      s <- ifelse(x < 0, "-", "")
      paste0(s, substr(abs(x), 2, nchar(x)))
   } else {
      x
   }
}

no.zero <- function(x) {
   sapply(seq_along(x), function(i) no.zero_(x[i]))
}


StatContour3 <- ggplot2::ggproto("StatContour3", metR:::StatContour2,
                                 default_aes = aes(order = stat(level), linetype = factor(-sign(stat(level))))
)

geom_contour3 <- function(mapping = NULL, data = NULL,
                          stat = "contour3", position = "identity",
                          ...,
                          color = "black",
                          size = 0.3,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 1,
                          breaks = scales::fullseq,
                          bins = NULL,
                          binwidth = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
   layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomContour2,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
         lineend = lineend,
         linejoin = linejoin,
         linemitre = linemitre,
         breaks = breaks,
         bins = bins,
         binwidth = binwidth,
         na.rm = na.rm,
         color = color,
         size = size,
         ...
      )
   )
}

PermTest <- function(y, ..., N = 10) {
   original <- FitLm(y, ..., se = FALSE)
   regressor <- original$regressor
   estimate <- original$estimate
   f <- rep(0, length(estimate))
   n <- length(y)
   p <- seq_len(n)
   set.seed(42)
   for (i in seq_len(N)) {
      y <- y[sample(p, n, replace = FALSE)]
      e <- FitLm(y, ..., se = FALSE)$estimate
      f <- f + as.numeric(abs(e) >= abs(estimate))
   }
   f <- f/N
   return(append(original, list(p.value = f)))
}

Detrend <- function(y, x) {
   nas <- is.na(y)
   m <- mean(y, na.rm = TRUE)
   if (!hasArg(x)) x <- seq_along(y)
   y[!nas] <- .lm.fit(cbind(1, x[!nas]), y[!nas])$residuals
   return(y + m)
}

Jump <- function(x, by = 1) {
   keep <- JumpBy(unique(x), by = by)
   x[!(x %in% keep)] <- NA
   x
}

shift2 <- function(x, n = 1L, fill = NA, give.names = FALSE) {
   type <- ifelse(n > 0, "lead", "lag")
   data.table::shift(x, abs(n), fill, type, give.names)
}

shiftcor <- function(x, y, lags) {
   cors <- vapply(lags, function(i) cor(x, shift2(y, i), use = "complete.obs"), 1)
   return(list(cor = cors, lag = lags))
}

shiftregr <- function(x, y, lags, ...) {
   regr <- vapply(lags, function(i) {
      FitLm(x, y = shift2(y, i), ...)$estimate[[2]]
   }, 1)
   return(list(regr = regr, lag = lags))
}

BuildEOF <- function(formula, value.var = NULL, data = NULL, n = 1, 
                     rotate = FALSE) {
   
   if (!is.null(value.var)) {
      if (is.null(data)) stop("data must not be NULL if value.var is NULL",
                              .call = FALSE)
      data <- copy(data)
      f <- as.character(formula)
      f <- stringr::str_replace(f, "~", "\\|")
      formula <- Formula::as.Formula(paste0(value.var, " ~ ", f))
   }
   
   if (is.null(data)) {
      formula <- Formula::as.Formula(formula)
      data <- as.data.table(eval(quote(model.frame(formula, data  = data))))
   }
   
   f <- as.character(formula)
   f <- stringr::str_split(f,"~", n = 2)[[1]]
   dcast.formula <- stringr::str_squish(f[stringr::str_detect(f, "\\|")])
   dcast.formula <- as.formula(stringr::str_replace(dcast.formula, "\\|", "~"))
   
   value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])
   
   g <- metR:::.tidy2matrix(data, dcast.formula, value.var)
   
   if (is.null(n)) n <- seq_len(min(ncol(g$matrix), nrow(g$matrix)))
   
   if (requireNamespace("irlba", quietly = TRUE) &
       max(n) < 0.5 *  min(ncol(g$matrix), nrow(g$matrix))) {
      set.seed(42)
      eof <- irlba::irlba(g$matrix, nv = max(n), nu = max(n), rng = runif)
   } else {
      eof <- svd(g$matrix, nu = max(n), nv = max(n))
      eof$d <- eof$d[1:max(n)]
   }
   eof$D <- diag(eof$d, ncol = max(n), nrow = max(n))
   
   if (rotate == TRUE & max(n) > 1) {
      # Rotation
      loadings <- t(with(eof, D%*%t(v)))
      scores <- eof$u
      R <- varimax(loadings, normalize = FALSE)
      eof$u <- eof$u%*%R$rotmat
      
      # Recover rotated V and D matrixs
      loadings <- R$loadings
      class(loadings) <- "matrix"
      eof$d <- sqrt(apply(loadings, 2, function(x) sum(x^2)))
      eof$v <- t(diag(1/eof$d)%*%t(loadings))
   }
   
   c(with(eof, u%*%D%*%t(v)))
}



stat_contour4 <- function(mapping = NULL, data = NULL,
                          geom = "contour", position = "identity",
                          ...,
                          breaks = scales::fullseq,
                          bins = NULL,
                          binwidth = NULL,
                          na.rm = FALSE,
                          circular = NULL,
                          show.legend = NA,
                          inherit.aes = TRUE) {
   layer(
      data = data,
      mapping = mapping,
      stat = StatContour4,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
         na.rm = na.rm,
         breaks = breaks,
         bins = bins,
         binwidth = binwidth,
         circular = circular,
         ...
      )
   )
}

#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
StatContour4 <- ggplot2::ggproto("StatContour4", Stat,
                                 required_aes = c("x", "y", "z"),
                                 default_aes = ggplot2::aes(order = ..level..),
                                 setup_params = function(data, params) {
                                    # Check is.null(breaks) for backwards compatibility
                                    if (is.null(params$breaks)) {
                                       params$breaks <- scales::fullseq
                                    }
                                    
                                    if (is.function(params$breaks)) {
                                       # If no parameters set, use pretty bins to calculate binwidth
                                       if (is.null(params$bins) && is.null(params$binwidth)) {
                                          params$binwidth <- diff(pretty(range(data$z), 10))[1]
                                       }
                                       # If provided, use bins to calculate binwidth
                                       if (!is.null(params$bins)) {
                                          params$binwidth <- diff(range(data$z)) / params$bins
                                       }
                                       
                                       params$breaks <- params$breaks(range(data$z), params$binwidth)
                                    }
                                    return(params)
                                    
                                 },
                                 compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                          breaks = scales::fullseq, complete = FALSE,
                                                          na.rm = FALSE, circular = NULL) {
                                    
                                    if (!is.null(circular)) {
                                       # M <- max(data[[circular]]) + resolution(data[[circular]])
                                       data <- RepeatCircular(data, circular)
                                    }
                                    
                                    contours <- as.data.table(.contour_lines(data, breaks, complete = complete))
                                    
                                    # contours <- .order_contour(contours, setDT(data))
                                    
                                    return(contours)
                                 }
)


.contour_lines <- function(data, breaks, complete = FALSE) {
   
   cl <- setDT(contoureR::getContourLines(
      x = data$x, y = data$y, z = data$z, levels = breaks))
   
   if (length(cl) == 0) {
      warning("Not possible to generate contour data", call. = FALSE)
      return(data.frame())
   }
   setnames(cl, c("z", "Group", "PID"), c("level", "group", "piece"))
   return(cl)
}


Smooth2D <- function(formula, x.out = 64, y.out = 64, data = NULL, ...) {
   dep.names <- formula.tools::lhs.vars(formula)
   if (length(dep.names) == 0) stop("LHS of formula must have at least one variable")
   
   ind.names <- formula.tools::rhs.vars(formula)
   if (length(ind.names) > 2) {
      stop("RHS of formula must be of the form x + y")
   }
   
   formula <- Formula::as.Formula(formula)
   data <- as.data.table(eval(quote(model.frame(formula, data = data,
                                                na.action = NULL))))
   
   loc <- setDF(data)[, ind.names]
   # for (v in seq_along(dep.names)) {
   value.var <- dep.names[1]
   
   sm <- fields::smooth.2d(data[[value.var]], loc, 
                           nrow = x.out, ncol = y.out, 
                           ...)
   if (!is.finite(diff(range(sm$z)))) {
      stop(paste0("smoothing failed for ", value.var, " use a bigger smooth parameter"))
   }
   
   dimnames(sm$z) <- with(sm, setNames(list(x, y), ind.names))
   
   z <- setDT(melt(sm$z, value.name = value.var))
   # set(loc, NULL, value.var, z)
   # }
   return(z)
}


seq_range <- function(x, ...) {
   r <- range(x)
   seq(r[1], r[2], ...)
}

fft2 <- function(x, k) {
   f <- fft(x)/length(x)
   f[-1] <- f[-1]*2
   return(list(R = Re(f[k + 1]), 
               I = Im(f[k + 1])))
}


StatRasa <- ggplot2::ggproto("StatRasa", Stat,
                             compute_group = function(data, scales, fun, fun.args) {
                                args <- formals(fun)
                                
                                for (i in seq_along(fun.args)) {
                                   if (names(fun.args[i]) %in% names(fun.args)) {
                                      args[[names(fun.args[i])]] <- fun.args[[i]]
                                   }
                                }
                                
                                formals(fun) <- args
                                fun(data)
                             })

stat_rasa <- function(mapping = NULL, data = NULL,
                      geom = "point", 
                      position = "identity",
                      fun = NULL,
                      ...,
                      show.legend = NA,
                      inherit.aes = TRUE) {
   if (!is.function(fun)) stop("fun must be a function")
   
   fun.args <- match.call(expand.dots = FALSE)$`...`
   layer(
      data = data,
      mapping = mapping,
      stat = StatRasa,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      check.aes = FALSE,
      check.param = FALSE,
      params = list(
         fun = fun, 
         fun.args = fun.args,
         na.rm = FALSE,
         ...
      )
   )
}


SmoothContour <- function(data, nx = 64, ny = 64, breaks, 
                          smooth = 0.05) {
   data <- data[complete.cases(data), ]
   M <- max(diff(range(data$x)), diff(range(data$y)))
   theta <- smooth*M
   sm <- setDF(Smooth2D(z ~ x + y, data = data, x.out = nx, y.out = ny,
                        theta = theta))
   if (is.function(breaks)) {
      breaks <- breaks(sm$z)      
   }
   
   contours <- as.data.table(.contour_lines(sm, breaks,
                                            complete = FALSE))
   
   if (length(contours) == 0) {
      warning("Not possible to generate contour data", call. = FALSE)
      return(data.frame())
   }
   
   contours <- metR:::.order_contour(contours, setDT(sm))
   return(contours)
}

RunMean <- function(data, weights = NULL, k) {
   data$y <- with(data, RcppRoll::roll_mean(y, k, fill = NA))
   data
}


notify <- function(title = "title", text = NULL, time = 2) {
   time <- time*1000
   system(paste0('notify-send "', title, '" "', text, '" -t ', time, ' -a rstudio'))
}

notify_after <- function(expression, ...) {
   expression <- eval(expression)
   notify(title = "Run\\ ended", ...)
   return(expression)
}

# Plus-minus functions
pm <- function(x) {
   x <- abs(x)
   x <- x[!duplicated(x)]
   x <- sort(x)
   c(-x[x != 0], x)
}


`%pm%` <- function(x, y) {
   c(x - y, x + y)
}


theilsen <- function(formula, data, repeated = TRUE, weights = NULL) {
   mblm::mblm(formula, dataframe = data, repeated = repeated)
}

prob.t <- function(estimate, se, df) {
   pt(abs(estimate)/se, df, lower.tail = FALSE)
}

cut_obs <- function(x, n, keep = "first") {
   n_groups <- floor(length(x)/n)
   groups <- rep(seq_len(n_groups), each = n)
   nas_length <- length(x) - length(groups)
   
   if (keep == "first") {
      groups <- c(groups, rep(NA, nas_length))
   } else {
      groups <- c(rep(NA, nas_length), groups)
   }
   return(groups)
}

MakeCircle <- function(r, x0 = 0, y0 = 0, n = 40) {
   data <- data.table(r = r, x0 = x0, y0 = y0)
   theta <- seq(0, 360, length.out = n)
   data[, .(x = r*cos(theta*pi/180) + x0,
            y = r*sin(theta*pi/180) + y0), by = .(r)]
}

MakeLine <- function(angle, r = 1) {
   data <- data.table(angle = angle, x = 0, y = 0)
   data[, `:=`(xend = r[1]*cos(angle*pi/180),
               yend = r[1]*sin(angle*pi/180))]
}

angle <- function(a, b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

angle2 <- function(M, N){
   atan2(N[2], N[1]) - atan2(M[2], M[1]) 
}



dervangle <- function(x, y) {
   N <- length(x)
   a_prev <- atan2(y[c(N, 1:(N-1))],  x[c(N, 1:(N-1))])
   a_next <- atan2(y[c(2:N, 1)], x[c(2:N, 1)])
   
   dxdy <- (a_next - a_prev)
   dxdy[dxdy > pi] <- dxdy[dxdy > pi] - 2*pi
   dxdy[dxdy < -pi] <- dxdy[dxdy < -pi] + 2*pi
   dxdy <- dxdy/2
   dxdy[c(1, N)] <- NA
   dxdy
}


ReIm <- function(complex) {
   list(R = Re(complex), I = Im(complex))
}

clusters <- function(data, k) {
   kmeans(data, k)$cluster
}


xy2lonlat <- function(x, y) {
   data <- data.table(x, y)
   
   data[, y1 := as.numeric(-y + max(y))]
   data[, y1 := - 3950*1000 + 25000*y1]
   data[, x1 := - 3950*1000 + 25000*x]
   
   datau <- unique(data)
   
   proj <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
   datau[, c("lon", "lat") := proj4::project(list(x1, y1), proj = proj, 
                                             inverse = TRUE)]
   datau[, lon := ConvertLongitude(lon, from = 180)]
   
   as.list(datau[data, on = c("x", "y")][, .(lon, lat)])
}

meanfun <- function(x, group, fun, ...) {
   dt <- data.table(x, group)
   dt[, group2 := seq_len(.N), by = group]
   
   mf <- dt[, .(f = fun(x, ...)), by = group][, .(mf = mean(f, na.rm = TRUE))]$mf
   fm <- dt[, .(m = mean(x, na.rm = TRUE)), by = group2][, .(fm = fun(m, ...))]$fm
   
   return(list(mean.fun = mf, fun.mean = fm))
}

qs.index <- function(gh, lat, lev, k = 3, lats.index =  c(-65, -40), levs.index = c(100, 700)) {
   dt <- data.table(gh, lat, lev)
   dt[lat %between% lats.index & 
         lev %between% levs.index] %>% 
      .[, FitWave(gh, k), by = .(lat, lev)] %>% 
      .[, phase := circular(phase*k, modulo = "2pi")] %>% 
      .[, .(amplitude = mean(amplitude), phase = as.numeric(mean.circular(phase/k)))]
}


qs3.index <- function(...) {
   qs.index(k = 3, ...)
}

slide_apply <- function (data, window, step = 1, fun) {
   fun <- match.fun(fun)
   total <- nrow(data)
   window <- abs(window)
   spots <- seq(from = 1, to = (total - window + 1), by = abs(step))
   result <- rep(NA, length(spots))
   for (i in 1:length(spots)) {
      result[window + i - 1] <- fun(data[spots[i]:(spots[i] + 
                                                      window - 1), ])
   }
   return(result)
}

cor.wave <- function(phi1, phi2, k = 3) {
   a1 <- -phi1*k 
   a2 <- -phi2*k 
   
   cos(a1 - a2)
}

sum.wave <- function(amplitudes, phases, k = 1) {
   phases <- -k[1]*phases + pi/2
   
   R <- sum(amplitudes*cos(phases))
   I <- sum(amplitudes*sin(phases))
   phase <- -(atan2(I, R) - pi/2)/k[1]
   amplitude <- sqrt(R^2 + I^2)
   
   return(list(amplitude = amplitude,
               phase = phase,
               k = k[1]))
}

mean.wave <- function(amplitudes, phases, k = 3) {
   wave <- sum.wave(amplitudes, phases, k)
   wave$amplitude <- wave$amplitude/length(amplitudes)
   wave
}

mean.phase <- function(amplitudes, phases, k = 3) {
   phases <- -k[1]*phases + pi/2
   R <- sum(amplitudes*cos(phases))
   I <- sum(amplitudes*sin(phases))
   -(atan2(I, R) - pi/2)/k[1]
   
}

# stationarity.wave <- function(waves, phi.s = NULL, method = c("amoma", "avar")) {
#    # waves es una lista con amplitudes, phases, y kes
#    # method AM/MA
#    waves <- transpose(waves)
#    names(waves) <- c("amplitude", "phase", "k")
#    
#    if (is.null(phi.s)) {
#       phi.s <- with(waves, mean.wave(amplitude, phase, k[1]))$phase
#    }
#    
#    w <- with(waves, amplitude/sum(amplitude))
#    if (method[1] == "amoma") {
#       s <- weighted.mean(cor.wave(waves$phase, phi.s, waves$k), w)
#    }
#    
#    if (method[1] == "avar") {
#       # waves$phase <- circular::circular(waves$phase*waves$k, modulo = "2pi")
#       # phi.s <- circular::circular(phi.s*waves$k[1], modulo = "2pi")
#       
#       s <- mean(w*(acos(cos(waves$phase - phi.s))^2))/waves$k[1]
#       if (!is.finite(s)) s <- 0
#    }
#    return(s)
# }

stationarity.wave <- function(waves, group = NULL, method = c("amoma", "avar")) {
   waves <- transpose(waves)
   names(waves) <- c("amplitude", "phase", "k")
   if (is.null(group)) {
      waves$phi.s <- with(waves, mean.phase(amplitude, phase, k[1]))
   } else {
      group <- deparse(substitute(group))
      waves[, phi.s := mean.phase(amplitude, phase, k[1]), by = group]
   }
   
   if (method[1] == "amoma") {
      dif <- with(waves, cos(k*(phase - phi.s)))
   }
   
   if (method[1] == "avar") {
      dif <- with(waves, acos(cos(k*(phase - phi.s)))^2)
   }
   s <- weighted.mean(dif, waves[["amplitude"]])
   return(s)
}


stationarity.wave2 <- function(waves, method = c("amoma", "avar")) {
   # waves es una lista con amplitudes, phases, y kes
   # method AM/MA
   waves <- transpose(waves)
   if (length(waves) == 3) {
      names(waves) <- c("amplitude", "phase", "k")
      waves$phi.s <- with(waves, mean.phase(amplitude, phase, k[1]))
   } else {
      names(waves) <- c("amplitude", "phase", "k", "phi.s")
   }
   
   # setDT(waves)
   
   if (method[1] == "amoma") {
      dif <- with(waves, cos(k[1]*(phase - phi.s)))
   }
   
   if (method[1] == "avar") {
      dif <- with(waves, acos(cos(k[1]*(phase - phi.s)))^2)
   }
   s <- weighted.mean(dif, waves[["amplitude"]])
   return(s)
}


as.wave <- function(amplitude, phase, k, ...) {
   data.table::transpose(list(amplitude = amplitude, phase = phase, k = k, ...))
}

dtapply <- function(x, width, FUN = NULL, by = 1, fill = NA, ...) {
   FUN <- match.fun(FUN)
   if (is.null(by)) by <- width
   if (width %% 2 == 0) stop("width must be odd")
   
   if (is.data.frame(x)) {
      lenX <- nrow(x)
   } else if (is.list(x)) {
      lenX <- length(x[[1]])
   } else {
      lenX <- length(x)
   }
   
   if (lenX < width) {
      warning("width is longer than length(x), returning NA")
      return(NA)
   }
   
   SEQ1 <- seq(1, lenX - width + 1, by = by)
   SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
   
   OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
   OUT <- base:::simplify2array(OUT, higher = TRUE)
   fill <- rep(fill[1], (width - 1)/2)
   return(c(fill, OUT, fill))
}


listapply <- function(x, width, FUN = NULL, by = 1, fill = NA, ...) {
   FUN <- match.fun(FUN)
   if (is.null(by)) by <- width
   if (width %% 2 == 0) stop("width must be odd")
   
   lenX <- length(x)
   
   if (lenX < width) {
      warning("width is longer than length(x), returning NA")
      return(NA)
   }
   
   SEQ1 <- seq(1, lenX - width + 1, by = by)
   SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
   
   OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
   OUT <- base:::simplify2array(OUT, higher = TRUE)
   fill <- rep(fill, (width -1 )/2)
   return(c(fill, OUT, fill))
}

tanh_trans <- function() {
   scales::trans_new("tanh", 
                     transform = tanh,
                     inverse = atanh)
}

expand.grid <- function(...) {
   as.data.table(base::expand.grid(...))
}

median.dist <- function(x) {
   m <- as.vector(dist(x))
   M <- mean(Mag(x[, 1], x[, 2]))
   mean(m^2/M^2)
}


interactivity <- function() {
   structure(list(), class = "add_plotly")
}

ggplot_add.add_plotly <- function(object, plot, object_name) {
   plotly::ggplotly(plot)
}

seq_centered <- function(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
                         length.out = NULL, along.with = NULL, sign = -1, ...) {
   s <- seq(from, to, by, ...)
   d <- s[2] - s[1]
   s + d*sign/2
}

no <- function(object, i) {
   UseMethod("no")
}

no.default <- function(object, i) {
   i <- ifelse(i < 0, length(object) + i + 1, i)
   object[-i]
}

no.data.frame <- function(object, i) {
   i <- ifelse(i < 0, nrow(object) + i + 1, i)
   object[-i, , drop = FALSE]
}

no_last <- function(object) {
   no(object, i = -1)
}

compute_group = function(data, scales, bw = "nrd0", adjust = 1, kernel = "gaussian",
                         n = 512, trim = FALSE, na.rm = FALSE, 
                         circular = FALSE) {
   if (trim) {
      range <- range(data$x, na.rm = TRUE)
   } else {
      range <- scales$x$dimension()
   }
   
   compute_density(data$x, data$weight, from = range[1], to = range[2],
                   bw = bw, adjust = adjust, kernel = kernel, n = n,
                   circular = circular)
}

compute_density <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian", n = 512,
                            circular = FALSE) {
   nx <- length(x)
   
   if (is.null(w)) {
      w <- rep(1 / nx, nx)
   }
   
   w <- w/sum(w)
   
   # if less than 2 points return data frame of NAs and a warning
   if (nx < 2) {
      warning("Groups with fewer than two data points have been dropped.", call. = FALSE)
      return(data.frame(
         x = NA_real_,
         density = NA_real_,
         scaled = NA_real_,
         ndensity = NA_real_,
         count = NA_real_,
         n = NA_integer_
      ))
   }
   
   if (isFALSE(circular)) {
      dens <- stats::density(x, weights = w, bw = bw, adjust = adjust,
                             kernel = kernel, n = n, from = from, to = to)
   } else {
      if (isTRUE(circular)) {
         x <- suppressWarnings(circular::as.circular(x))
      } else {
         x <- do.call(circular::as.circular, c(list(x = x), circular))
      }
      
      if (!is.numeric(bw)) {
         bw <- suppressWarnings(bw.nrd.circular(x))
      }
      if (kernel != "wrappednormal") kernel <- "vonmises"
      dens <- suppressWarnings(density.circular(x, bw = bw, adjust = adjust, kernel = kernel,
                                                from = from, to = to, n = n, weights = w))
   }
   
   data.frame(
      x = dens$x,
      density = dens$y,
      scaled =  dens$y / max(dens$y, na.rm = TRUE),
      ndensity = dens$y / max(dens$y, na.rm = TRUE),
      count =   dens$y * nx,
      n = nx
   )
}

StatDensity$compute_group <- compute_group

StatDensity$default_aes <- c(StatDensity$default_aes, 
                             list(weight = 1))

library(circular)
density.circular <- function(x, z = NULL, bw = bw.nrd.circular,
                             weights = NULL,
                             adjust = 1, type = c("K", "L"), 
                             kernel = c("vonmises", "wrappednormal"), 
                             na.rm = FALSE, from = circular(0), 
                             to = circular(2*pi), n = 512, K = NULL, min.k = 10,
                             control.circular = list(), ...) {
   name <- deparse(substitute(x))
   data <- x
   if (!is.numeric(from)) 
      stop("argument 'from' must be numeric")
   if (!is.numeric(to)) 
      stop("argument 'to' must be numeric")
   if (!is.finite(from)) 
      stop("non-finite `from'")
   if (!is.finite(to)) 
      stop("non-finite `to'")
   if (!is.numeric(n)) 
      stop("argument 'n' must be numeric")
   n <- round(n)
   if (n <= 0) 
      stop("argument 'n' must be integer and positive")
   if (!is.numeric(x)) 
      stop("argument 'x' must be numeric")
   if (!is.null(z) && is.circular(z)) {
      datacircularp <- circularp(z)
   }
   else if (is.circular(x)) 
      datacircularp <- circularp(x)
   else {
      datacircularp <- list(type = "angles", units = "radians", 
                            template = "none", modulo = "asis", zero = 0, rotation = "counter")
   }
   dc <- control.circular
   if (is.null(dc$type)) 
      dc$type <- datacircularp$type
   if (is.null(dc$units)) 
      dc$units <- datacircularp$units
   if (is.null(dc$template)) 
      dc$template <- datacircularp$template
   if (is.null(dc$modulo)) 
      dc$modulo <- datacircularp$modulo
   if (is.null(dc$zero)) 
      dc$zero <- datacircularp$zero
   if (is.null(dc$rotation)) 
      dc$rotation <- datacircularp$rotation
   if (dc$modulo == "pi") 
      stop("The function does not work yet for modulo='pi'")
   
   if (!is.numeric(bw)) {
      bw <- match.fun(bw)
      bw <- bw(x)
   }
   
   x <- conversion.circular(x, units = "radians", zero = 0, 
                            rotation = "counter")
   attr(x, "class") <- attr(x, "circularp") <- NULL
   from <- conversion.circular(from, units = "radians", zero = 0, 
                               rotation = "counter")
   attr(from, "class") <- attr(from, "circularp") <- NULL
   to <- conversion.circular(to, units = "radians", zero = 0, 
                             rotation = "counter")
   attr(to, "class") <- attr(to, "circularp") <- NULL
   kernel <- match.arg(kernel)
   x <- as.vector(x)
   x.na <- is.na(x)
   if (any(x.na)) {
      if (na.rm) 
         x <- x[!x.na]
      else stop("x contains missing values")
   }
   x.finite <- is.finite(x)
   if (any(!x.finite)) {
      x <- x[x.finite]
   }
   nx <- length(x)
   if (is.null(z)) {
      z <- circular(seq(from = from, to = to, length = n))
   }
   else {
      if (!is.numeric(z)) 
         stop("argument 'z' must be numeric")
      namez <- deparse(substitute(z))
      z.na <- is.na(z)
      if (any(z.na)) {
         if (na.rm) {
            z <- z[!z.na]
         }
         else {
            stop("z contains missing values")
         }
      }
      z.finite <- is.finite(z)
      if (any(!z.finite)) {
         z <- z[z.finite]
      }
   }
   zz <- conversion.circular(z, dc$units, dc$type, dc$template, 
                             dc$modulo, dc$zero, dc$rotation)
   z <- conversion.circular(z, units = "radians", zero = 0, 
                            rotation = "counter")
   attr(z, "class") <- attr(z, "circularp") <- NULL
   z <- as.vector(z)
   
   bw <- adjust * bw
   if (!is.numeric(bw)) 
      stop("argument 'bw' and 'adjust' must be numeric")
   if (!is.finite(bw)) 
      stop("non-finite `bw'")
   if (bw <= 0) 
      stop("`bw' is not positive.")
   
   y <- DensityCircularRad(x = x, z = z, bw = bw, kernel = kernel, 
                           K = K, min.k = min.k, weights = weights)
   structure(list(data = data, x = zz, y = y, bw = bw, n = nx, 
                  kernel = kernel, call = match.call(), data.name = name, 
                  has.na = FALSE), class = "density.circular")
}


DensityCircularRad <- function (x, z, bw, kernel, K = NULL, min.k = 10, 
                                weights = NULL) {
   if (is.null(weights)) {
      weights <- rep(1, length(x))
   }
   
   # weights <- weights/sum(weights)
   nx <- length(x)
   if (kernel == "vonmises") {
      y <- sapply(z, circular:::DvonmisesRad, mu = x, kappa = bw, log = FALSE)
   }
   else if (kernel == "wrappednormal") {
      rho <- exp(-bw^2/2)
      y <- sapply(z, circular:::DwrappednormalRad, mu = x, rho = rho, 
                  K = K, min.k = min.k)
   }
   else {
      stop("other kernels not implemented yet")
   }
   y <- apply(y, 2, weighted.mean, w = weights)
   return(y)
}


as.data.frame.density.circular <- function(x, row.names = NULL, optional = FALSE, ...) {
   df <- with(x, data.frame(x = x, y = y))
   df[order(df$x), ]
}


as.degrees <- function(x) {
   x*180/pi
}

as.radians <- function(x) {
   x*pi/180
}

degrees_trans <- function() {
   scales::trans_new("degrees",
                     function(x) x*180/pi,
                     function(x) x*pi/180)
}

LabDegrees <- function(x) {
   as.character(as.numeric(x)*180/pi)
}


as.data.table.analyze.wavelet <- function(object) {
   df <- with(object, Ampl)
   
   dimnames(df) <- list(period = object$Period,
                        location = seq(0, object$nc - 1)*object$dt + 1)
   
   df <- data.table::setDT(data.table::melt(df, value.name = "amplitude"))
   if (!is.null(object$series$date)) {
      df[, date := lubridate::as_datetime(object$series$date), by = period]
   }
   df[, phase := c(object$Phase)]
   df[, power := c(object$Power)]
   if (!is.null(object$Power.pval)) df[, p.value := c(object$Power.pval)]
   df <- df[, ridge := c(object$Ridge)]
   
   coi <- get_coi(object)
   data.table::setattr(df, "coi", coi)
   
   return(df)
}

fortify.analyze.wavelet <- function(model, object, ...) {
   as.data.table(model)
}

get_coi <- function(object) {
   UseMethod("get_coi")
}

get_coi.data.table <- function(object) {
   attr(object, "coi")
}

get_coi.analyze.wavelet <- function(object) {
   coi <- with(object, data.table(location = coi.1, 
                                  period = 2^coi.2))
   coi <- coi[location %between% range(object$axis.1)]
   if (!is.null(object$series$date)) {
      coi[, date := lubridate::as_datetime(object$series$date)]
   }
   coi <- coi[period %between% range(object$Period)]
   # coi <- coi[coi < min(object$Period), coi := min(object$Period)]
   coi
}

geom_coi <- function(object, alpha = 0.2, fill = "white", 
                     color = "white", size = 0.1, ...) {
   if (!is.data.frame(object)) object <- get_coi(object)
   # coi <- get_coi(object) 
   if (is.null(object$time)) object$time <- object$location
   geom_ribbon(data = object, aes(x = time, ymax = Inf, ymin = period),
               inherit.aes = F, alpha = alpha, fill = fill, 
               color = color, size = size, ...) 
}

log2_breaks <- function(range) {
   breaks <- 2^seq(floor(log2(range[1])), ceiling(log2(range[2])))
   # breaks <- breaks[breaks %between% range]
}

autoplot.analyze.wavelet <- function(object, p.val = 0.01, 
                                     geom = c("contour_fill", "raster", "tanaka"),
                                     ridge = FALSE) {
   df <- as.data.table(object)
   coi <- get_coi(df)
   
   r <- range(df$period)
   
   breaks <- 2^seq(floor(log2(r[1])), ceiling(log2(r[2])))
   breaks <- breaks[breaks %between% r]
   
   g <- ggplot(df, aes(location, period))
   
   if (geom[1] == "contour_fill") {
      g <- g +  geom_contour_fill(aes(z = power)) 
   } else if (geom[1] == "raster") {
      g <- g + geom_raster(aes(fill = power)) 
   } else if (geom[1] == "tanaka") {
      g <- g +  
         geom_contour_fill(aes(z = power)) +
         geom_contour_tanaka(aes(z = power))
   }
   
   if (isTRUE(ridge)) {
      g <- g + stat_subset(aes(subset = ridge == 1), size = 0.1)
   }
   
   if (!is.null(p.val)) {
      g <- g +
         geom_contour(aes(z = p.value), breaks = p.val, 
                      color = "white", size = 0.3) 
   }
   
   g <- g +
      geom_ribbon(data = coi, aes(x = location, ymax = Inf, ymin = period),
                  inherit.aes = F, alpha = 0.2, fill = "white", 
                  color = "white", size = 0.1) +
      scale_y_continuous("Period", expand = c(0, 0),
                         breaks = breaks,
                         trans = scales::log2_trans()) +
      scale_fill_viridis_c()
   
   if (!is.null(object$series$date)) {
      dates <- object$series$date
      breaklabs <- function(x) {
         as.character(dates[as.numeric(x)])
      }
      g + scale_x_continuous(labels = breaklabs, expand = c(0, 0))
   } else {
      g + scale_x_continuous(expand = c(0, 0))
   }
   
}



Wavelets <- function(..., seed = 42) {
   set.seed(seed)
   invisible(capture.output(w <- invisible(WaveletComp::analyze.wavelet(..., verbose = FALSE))))
   return(w)
}

harmonics <- function(x, n = 4) {
   n <- as.numeric(seq(1, by = 1, length.out = n))
   h <- c(vapply(x, function(i) i/n, n))
   sort(h)
}


addkey <- function(x, ..., verbose=getOption("datatable.verbose"), physical = TRUE) {
   cols = as.character(substitute(list(...))[-1L])
   if (!length(cols)) {
      cols = colnames(x)
   }
   else if (identical(cols, "NULL")) 
      cols = NULL
   prev_keys <- key(x)
   
   setkeyv(x, c(prev_keys, cols), verbose = verbose, physical = physical)
}


range_overlap <- function(x, y) {
   rx <- range(x)
   ry <- range(y)
   
   c(max(c(rx[1], ry[1])), min(c(rx[2], ry[2])))
}

nsign <- function(x, ref = 0) {
   factor(-sign(x - ref))
}



smooth.loess <- function(formula, span = 0.75, degree = 1, ...) {
   predict(loess(formula, span = span, degree = degree, ...))
}


.daymonth <- function(x) {
   paste0(formatC(lubridate::month(x), width = 2, flag = "0"), "-", 
          formatC(lubridate::day(x), width = 2, flag = "0"))
}
.daymonth.levels <- .daymonth(seq.Date(as.Date("2000-01-01"), 
                                       as.Date("2000-12-31"), by = "1 day"))

daymonth <- function(x) {
   factor(paste0(formatC(month(x), width = 2, flag = "0"), "-", 
                 formatC(lubridate::day(x), width = 2, flag = "0")),
          levels = .daymonth.levels)
}



.signal.random <- function() {
   x <- rnorm(12*(2018-1948 +1))
   r.s <- RcppRoll::roll_mean(x, 12*10, fill = NA)
   r.s[!is.na(r.s)]
}

wavelet.boot <- function(null.fun, probs = 0.95, B = 10, seed = 42) {
   set.seed(seed)
   null <- match.fun(null.fun)
   
   list <- lapply(seq_len(B), function(b) {
      x <- null()
      dt <- c(WaveletComp::WaveletTransform(x)$Ampl)
   })
   
   data.table::transpose(lapply(data.table::transpose(list), quantile, probs = probs, names = FALSE))
}

WaveEnvelope <- function(y, k = "all") {
   N <- length(y)
   x_hat <- fft(y)/N
   
   if (k[1] == "all") {
      k <- 1:ceiling(N/2)
   }
   # browser
   x_hat[-(k+1)] <- 0
   Mod(fft(x_hat, inverse = T))*2
}


common_range <- function(x, groups) {
   group_order <- grouping(groups)
   group_bounds <- c(0, attr(group_order, "ends"))
   
   x_ordered <- x[group_order]
   common_range <- c(-Inf, Inf)
   for (i in seq_along(group_bounds)[-1]) {
      new_range <- range(x_ordered[(group_bounds[i-1] + 1):group_bounds[i]])
      common_range <- c(max(common_range[1], new_range[1]), 
                        min(common_range[2], new_range[2]))
      
   }
   common_range
}



FFTSmooth <- function(formula, data, weights, fc, BW = 0, n = 3) {
   browser()
   pred <- Re(spectral::filter.fft(data$y, data$x, fc = fc, BW = BW, n = n))
   
   model <- list(x = data$x, pred = pred)
   class(model) <- "my_smooth"
   return(model)
}

predictdf.my_smooth <- function(model, xseq, se, level) {
   data.frame(x = model$x, y = model$pred)
}



loess_fixed <- function (formula, data, weights, subset, model = FALSE, 
                         span, degree = 2L) {
   data <- model.frame(formula, data = data)
   data <- data[complete.cases(data), ]
   
   model <- list(data = data, 
                 span = span,
                 degree = degree)
   class(model) <- "loess_fixed"
   model
}

predictdf.loess_fixed <- function(model, xseq, se, level) {
   # browser()
   
   data <- model$data
   half_span <- model$span/2 
   sd <- y.out <- rep(NA, length(xseq))
   
   for (i in seq_along(xseq)) {
      sub <- data$x >= (xseq[i] - half_span) & data$x <= (xseq[i] + half_span)
      X <-  cbind(1, data$x[sub], data$x[sub]^2)
      fit <- stats::lm.fit(x = X,
                           y = data$y[sub])
      # sigma <- sum(fit$residuals^2)/(sum(sub) - 2)
      # se <- sqrt(diag(chol2inv(chol(t(X)%*%X)))*sigma)
      sd[i] <- stats::sd(fit$residuals)*sqrt(1/sum(sub) + (xseq[i] - mean(x[sub]))^2/sum((x[sub] - mean(x[sub]))^2)/(sum(sub) - 2))
      y.out[i] <- t(fit$coefficients) %*% c(1, xseq[i], xseq[i]^2)
   }
   # browser()
   
   if (se) {
      ci <-  stats::qt(level / 2 + .5, fit$df.residual)*se
      data.frame(x = xseq, y = y.out, ymin = y.out - ci, ymax = y.out + ci, se = sd)   
   } else {
      data.frame(x = xseq, y = y.out)
   }
   
}


position_relative <- function(x, y, range = c(0, 1)) {
   force(range)
   force(x)
   force(y)
   ggproto(NULL, Position, 
           compute_panel = function(self, data, params, scales) {
              # browser()
              data$x <- scales::rescale(x, from = range, to = scales$x$get_limits())
              data$y <- scales::rescale(y, from = range, to = scales$y$get_limits())
              data
           }
   ) 
}


hilbert <- function(x) {
   x1 <- rep(x, 3)
   keep <- (length(x)+1):(2*length(x))
   
   x1 <- spectral::analyticFunction(x1)
   x1[keep]
}
