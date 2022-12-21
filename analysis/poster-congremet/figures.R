base_dir <- here::here("analysis", "poster-congremet")

source(file.path(base_dir, "data.R"))

library(kableExtra)
library(ggplot2)
library(ggperiodic)
library(tagger)
sf::sf_use_s2(FALSE)
# mgp <- latex2exp::TeX('Geopotential height ($m^2s^{-1}$)')
mpg <- 'Altura geopotencial ($m^2s^{-1}$)'

theme_set(theme_shceof(base_size = 20) +
            theme(legend.box.spacing = grid::unit(-.5, "lines")) +
            theme(plot.margin = margin(0, 0, 0, 0, "cm")))
panel_background <- theme(panel.background = element_rect(fill = "#fbfbfb", color = NA),
                          panel.ontop = FALSE,
                          tagger.panel.tag.background = ggplot2::element_rect(color = NA,
                                                                              fill = "#fbfbfb"))
one_less <- function(x) {
  n <- length(x)
  if (n < 4) return(x)
  x <- round(x, 5)

  c( rev(JumpBy(rev(x[x < 0]), 2, fill = "")),
     JumpBy(x[x > 0], 2, fill = ""))
}
no_grid <- theme(panel.grid = element_blank())
guide_colorstrip_bottom <- function(width = 15, height = 0.5,
                                    title.position = "top",...) {
  guide_colorstrip(title.position = title.position, title.hjust = 0.5,
                   barheight = height,
                   barwidth = width, ...)
}

f <- formals(geom_contour_tanaka)
f$range <-  c(0.01, 0.2)
formals(geom_contour_tanaka) <- f

guide_colorsteps_bottom <- function(width = 15, height = 0.5, even.steps = FALSE,
                                    title.position = "top", show.limits = TRUE, ticks = TRUE,
                                    ...) {
  guide_colorsteps(show.limits = show.limits, even.steps = even.steps,
                   title.position = title.position, title.hjust = 0.5,
                   barheight = height, ticks = ticks,
                   ticks.colour = "black",
                   barwidth = width, frame.colour = "black", ...)
}

grid_y <- theme(panel.grid.major.y = element_line(color = "gray60", size = 0.1))

geom_coords <- function() {

  list(
    no_grid,
    annotate("segment",
             y = seq(-90, 0, by = 15),
             yend = seq(-90, 0, by = 15),
             x = 0,
             xend = 360,
             size = 0.1, alpha = 0.5),

    annotate("segment",
             x = seq(0, 360 - 30, by = 30),
             xend =  seq(0, 360 - 30, by = 30),
             y = -90 + 15,
             yend = Inf,
             size = 0.1, alpha = 0.5),
    shadowtext::geom_shadowtext(data = data.frame(x = 0, y = seq(-90 + 15, 0,
                                                                 by = 15)),
                                aes(x, y, label = LatLabel(y)), size = 1.5,
                                alpha = 0.7,
                                colour = "black",
                                bg.colour = "white")

  )
}

pattern_dots <- ggplot2::ggproto("GeomDots", ggpattern::GeomPolygonPattern)
ggplot2::update_geom_defaults(pattern_dots,
                              list(pattern = "circle",
                                   colour = NA,
                                   pattern_colour = NA,
                                   pattern_fill = "black",
                                   pattern_density = 0.3,
                                   pattern_alpha = 1,
                                   pattern_spacing = 0.02,
                                   fill = NA
                              ))
update_geom_defaults(GeomSmooth, list(colour = "#0d52bf"))


coord_polar <- function(ymax = -20, ...) {

  x <- c(seq(0, 360, length.out = 40),
         seq(360, 0, length.out = 40),
         0)
  y <- c(rep(ymax, length.out = 40),
         rep(60, length.out = 40),
         ymax)

  cbind(x, y) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "+proj=latlong") -> white

  list(
    geom_sf(data = white, inherit.aes = FALSE,
            fill = "white",
            colour = "white", size = 2),
    coord_sf(ylim = c(-90, ymax),
             lims_method = "box",
             crs = "+proj=laea +lat_0=-90",
             default_crs = "+proj=longlat",
             label_axes =  "----", ...)
  )
}

geom_contour_pval <- function(mapping, p.value = 0.01, size = 0.1, ...) {
  mapping2 <- mapping
  mapping2$fill <- aes(fill = NA)$fill
  list(
    stat_contour_filled(mapping2, breaks = c(0, p.value), fill = NA, geom = pattern_dots, ...),
    geom_contour2(mapping, breaks = p.value, size = size, ...)
  )
}

spoke <- function(data, coords) {
  radius <-  grid::unit(.15, "snpc")

  outer_circle <-  grid::circleGrob(x = 0,
                                    y = 0,
                                    r = radius*1.2)

  xcentre <- grid::unit(0, "npc") + grid:::grobHeight(outer_circle)/2
  ycentre <-  grid::unit(0, "npc") + grid:::grobWidth(outer_circle)/2


  circle <- grid::circleGrob(x = xcentre,
                             y = ycentre,
                             r = radius,
                             gp = grid::gpar(fill = "white",
                                             alpha = 0.7))
  # browser()
  dx <- cos(coords$angle[1]*pi/180)*grid::grobHeight(circle)/2
  dy <- sin(coords$angle[1]*pi/180)*grid::grobWidth(circle)/2


  line <- grid::segmentsGrob(x0 = xcentre, y0 = ycentre,
                             x1 = xcentre + dx,
                             y1 = ycentre + dy,
                             gp = grid::gpar(fill = "black"),
                             arrow = grid::arrow(angle = 13,
                                                 length =  grid::unit(.15/2, "snpc"),
                                                 type = "closed")
  )

  grid::gTree(children = grid::gList(circle, line))


}

lab_lev <- AddSuffix(" hPa")
term_lab <- c("Real" = "0º (~PSA2)",
              "Imaginary" = "90º (~PSA1)")
# Plots -------------------------------------------------------------------



gdata <- ceof[, eof[[1]]$right, by = .(season)] %>%
  copy() %>%
  .[, hgt := hgt/sd(abs(hgt))]

var <- ceof[, eof[[1]]$sdev, by = .(season)] %>%
  .[, setNames(paste0(cEOF, " (", scales::percent(r2), ")"),
               cEOF)]

gdata %>%
  ggperiodic::periodic(lon = c(0, 360)) %>%
  ggplot(aes(lon, lat)) +

  geom_contour_fill(aes(z = Re(hgt), fill = ..level..),
                    breaks = AnchorBreaks(0, 1, exclude = 0)) +
  geom_contour2(aes(z = Im(hgt),  linetype = factor(sign(..level..))),
                breaks = AnchorBreaks(0, 1, exclude = 0)) +

  scale_fill_divergent_discretised(guide = guide_colorsteps_bottom(width = 30)) +
  geom_qmap() +
  scale_x_longitude(labels = NULL) +
  scale_y_latitude(labels = NULL,
                   minor_breaks = NULL) +
  scale_linetype_manual(values = c("1" = 1, "-1" = 2),
                        labels = c("1" = "+", "-1" = "-"),
                        guide = "none") +
  facet_grid(lev  ~  cEOF,
             labeller = labeller(lev = lab_lev,
                                 cEOF = var)) +
  theme(legend.title = element_blank()) +
  geom_coords() +
  coord_polar() +
  tag_facets(tag = "cr")


# ceofs -------------------------------------------------------------------
w <- 170
h <- 196
ggsave(file.path(base_dir, "ceofs.svg"),
       width = w,
       height = h, units = "mm", bg = "white")


# R2 ----------------------------------------------------------------------

breaks <- seq(.2, 1, by = .1)

rbind(Precipitation = pp_regr,
      `2-metre\nTemperature` = t2m_regr, idcol = "var") %>%
  .[angle == 0] %>%
  .[is.finite(r.squared)] %>%
  # .[cEOF == "cEOF2"] %>%
  periodic(lon = c(0, 360)) %>%
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = r.squared, fill = ..level..), breaks = breaks) +
  geom_contour_tanaka(aes(z = r.squared), breaks = breaks) +
  # geom_text_contour(aes(z = r.squared,
  #                       label = ifelse(..level.. > 0.2, scales::percent(..level..), NA)),
  #                   breaks = breaks, size = 2,
  #                   skip = 1, stroke = 0.15, stroke.colour = "white") +
  geom_qmap() +
  scale_fill_divergent_discretised(name = "Varianza expliacda", midpoint = 0,
                                   labels = scales::percent_format(),
                                   guide = guide_colorsteps_bottom(40, show.limits = TRUE)) +

  scale_x_continuous(NULL, expand = c(0, 0),
                     breaks = seq(0, 360, by = 60),
                     labels = LonLabel) +
  scale_y_continuous(NULL, breaks  = seq(-90, 0, by = 30), expand = c(0, 0)) +

  facet_grid(var ~ cEOF) +
  # coord_quickmap(ylim = c(NA, 10)) +
  coord_sf(ylim = c(NA, 10),
           default_crs = "+proj=longlat +lon_wrap=180 +over") +
  theme(legend.box = "horizontal",
        legend.text = element_text(size = rel(0.7)),
        legend.box.spacing = grid::unit(0, "lines") ) +
  tag_facets("rc")  -> g



ggsave(file.path(base_dir, "r2.svg"),
       plot = g,
       width = 323,
       height = 172.6,
       units = "mm",
       bg = NULL)



# sam ---------------------------------------------------------------------

time <- ceof[, eof[[1]]$left, by = .(season)] %>%
  sep_ReIm() %>%
  .[, hgt := scale(hgt), by = .(cEOF, part)]

sam_r2 <- sams %>%
  .[time, on = .NATURAL, allow.cartesian = TRUE] %>%
  na.omit() %>%
  .[, correlate(hgt, estimate), by = .(lev, term, part, cEOF)]

max_r2 <- sam_r2[part == "Imaginary"] %>%
  .[, .SD[which.max(estimate^2)], by = .(cEOF, term)]

sam_r2 %>%
  .[, p.value_a := p.adjust(p.value, "fdr")] %>%
  ggplot(aes(lev, estimate^2)) +
  geom_vline(xintercept = c(50, 200),  size = 0.2, color = "gray50") +
  # geom_point(data = ~.x[p.adjust(p.value, "fdr") < 0.01], aes(color = term)) +
  geom_line(data = ~copy(.x)[p.value_a > 0.01, estimate := NA],
            aes(colour = stage(term, after_scale = scales::alpha(colour, 0.5))),
            size = 2) +
  geom_line(aes(color = term, group = term)) +

  scale_x_level(NULL, minor_breaks = NULL,
                sec.axis = sa_height_axis(breaks = seq(0, 50, by = 10),
                                          labels = function(x) paste0("~", x, " km"))) +
  scale_y_continuous("r2", limits = c(0, 1),
                     guide = guide_axis(check.overlap = TRUE)) +
  scale_size_manual("p-value", values = c("TRUE" = 1,
                                          "FALSE" = 0.5),
                    guide = "none") +
  scale_color_brewer(NULL, palette = "Dark2", labels = c(full = "SAM",
                                                         asym = "A-SAM",
                                                         sym = "S-SAM")) +
  facet_grid(part ~ cEOF, labeller = labeller(part = term_lab)) +
  coord_flip() +
  tag_facets("rc") +
  panel_background +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.placement = "outside") -> g

ggsave(file.path(base_dir, "sam.svg"),
       plot = g,
       width = 250,
       height = 243,
       units = "mm",
       bg = NULL)


# PSA ---------------------------------------------------------------------

psa2 <- psa$left[PC == "PSA2"][season(time) == "SON"]
cEOF2 <- ceof$eof[[1]]$left[cEOF == "cEOF2"] %>%
  copy() %>%
  .[, cEOF := NULL] %>%
  .[]

rotations_psa <- lapply(angles, function(a) {
  cEOF2 %>%
    copy() %>%
    .[, hgt := rotate(hgt, a)] %>%
    .[psa2, on = "time"] %>%
    .[, cor(value, Re(hgt))]
})

psa_angle <- angles[which.max(unlist(rotations_psa))]

make_numeric <- function(x) {
  x <- strsplit(x, "\ ")
  vapply(x, function(x) as.numeric(x[[1]]), numeric(1))
}

background_palette <- grDevices::colorRampPalette(c(scales::muted("blue"),
                                                    "white",
                                                    scales::muted("red")),
                                                  space = "Lab")

table <- cEOF2[psa_time, on = "time"] %>%
  .[PC != "SAM"] %>%
  copy() %>%
  sep_ReIm() %>%
  .[, correlate(value, hgt), by = .(PC, part)]

p.val <- table %>%
  dcast(PC ~ part, value.var = "p.value")

w <- 250*1.5
r <- 187*1.5
table %>%
  copy() %>%
  .[, part := term_lab[part]] %>%
  dcast(PC ~ part, value.var = "text") %>%
  kbl2(booktabs = TRUE) %>%
  column_spec2(2:3, bold = p.val[[.col]] < 0.01) %>%
  add_header_above(c("", "cEOF2" = 2)) %>%
  kable_classic() %>%
  save_kable(file.path(base_dir, "PSA.pdf"), vwidth = w, vheight = r, density = 96)


# regresiones -------------------------------------------------------------


sst_regr <- sst_regr[cEOF == "cEOF2"]
psi_regr <-  psi_regr[cEOF == "cEOF2"]
sst_gdata <- sst_regr[cEOF == "cEOF2"] %>%
  # .[cEOF == "cEOF2"] %>%
  .[lat %between% c(-90, 10)] %>%
  .[, p_val := Pvaluate(estimate, std.error, df, "fdr"),
    by = .(cEOF, term, angle)] %>%
  .[, var := "SST"]

rsst <- max(range(abs(sst_regr[!is.na(cEOF), ]$estimate), na.rm = TRUE))
breaks_sst <- AnchorBreaks(0, bins = 20, exclude = 0)(c(-rsst, rsst))


stream_gdata <- psi_regr[cEOF == "cEOF2"] %>%
  # .[cEOF == "cEOF2"] %>%
  # .[abs(estimate) > 1, estimate := NA] %>%
  .[lat %between% c(-90, 10)] %>%
  .[, p_val := Pvaluate(estimate, std.error, df, "fdr"), by = .(cEOF, angle, term)] %>%
  .[, var := "Streamfunction"]

rstream <- max(range(abs(psi_regr[cEOF == "cEOF2"]$estimate),
                     na.rm = TRUE))*1e-7
breaks_stream <- AnchorBreaks(0, bins = 20, exclude = 0)(c(-rstream, rstream))


pp_regr <- pp_regr[cEOF == "cEOF2"]
t2m_regr <- t2m_regr[cEOF == "cEOF2"]
hgt850_regr <- hgt850_regr[cEOF == "cEOF2"]
lats <- c(-55, 10)
breaks_pp <- AnchorBreaks(exclude = 0)(c(-.9, .9))


pp_regr <- pp_regr %>%
  copy() %>%
  .[, var := "Precipitation"] %>%
  .[, p.value := Pvaluate(estimate, std.error, df, "none"),
    by = .(cEOF, angle)]

t2m_regr <- t2m_regr %>%
  copy() %>%
  .[, var := "2m Temperature"] %>%
  .[, p.value := Pvaluate(estimate, std.error, df, "none"), by = .(cEOF, angle)]

var_lab <- c("2m Temperature" = "Temperatura a 2m",
             "Precipitation" = "Precipitación",
             "Streamfunction" = "Función Corriente")

g <- pp_regr %>%
  periodic(lon = c(0, 360)) %>%
  ggplot(aes(lon, lat)) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "#d4d4d4", color = NA
  ) +

  geom_contour_fill(aes(z = estimate, fill = ..level..),
                    breaks = breaks_pp) +
  geom_contour_tanaka(aes(z = estimate), breaks = breaks_pp) +
  scale_fill_divergent_discretised("Precipitación (correlación)",
                                   # limits = c(-1, 1),
                                   low = "#8E521C",
                                   high = "#00665E",
                                   guide = guide_colorsteps_bottom(25, order = 99)) +
  geom_contour_pval(aes(z = p.value)) +

  ggnewscale::new_scale_fill() +

  geom_contour_fill(data = t2m_regr,
                    aes(z = estimate, fill = ..level..),
                    breaks = AnchorBreaks(exclude = 0)) +
  geom_contour_tanaka(data = t2m_regr,
                      aes(z = estimate),
                      breaks = AnchorBreaks(exclude = 0)) +
  scale_fill_divergent_discretised("Temperatura a 2m (K)",
                                   # limits = c(-1, 1),
                                   guide = guide_colorsteps_bottom(25, order = 1)) +
  geom_contour_pval(data = t2m_regr, aes(z = p.value)) +
  geom_contour2(data = copy(hgt850_regr)[, var := "2m Temperature"],
                aes(z = estimate, linetype = factor(-sign(..level..))),
                size = 0.2, breaks = AnchorBreaks(0, exclude = 0)) +

  ggnewscale::new_scale_fill() +
  geom_contour_fill(aes(z = estimate*1e-7, fill = ..level..),
                    breaks = breaks_stream, data = stream_gdata) +
  geom_contour_pval(aes(z = p_val), data = stream_gdata) +
  geom_streamline(aes(dx = fx, dy = fy), min.L = 3, L = 10,
                  size = .5, res = 2, nx = 22, ny = 12,
                  arrow.angle = 20, arrow.length = 0.2, data = stream_gdata) +
  # geom_vector(aes(dx = fx, dy = fy), skip = 3, min.mag = 0.002,
  #             size = 0.1) +
  # scale_mag(max_size = .5, guide = "none") +

  scale_fill_divergent_discretised("Función Corriente (m^2s^-1x10^-7)",
                                   labels = one_less,
                                   low = scales::muted("red"),
                                   high = scales::muted("blue"),
                                   guide = guide_colorsteps_bottom(25, show.limits = FALSE,
                                                                   order = 99)) +
  geom_qmap() +

  grid_panel(spoke, aes(angle = -angle, var = "Streamfunction"),
             inherit.aes = FALSE) +

  scale_x_continuous("", expand = c(0, 0), breaks = seq(0, 360, by = 60),
                     labels = LonLabel) +
  scale_y_continuous("", expand = c(0, 0), breaks = seq(-90, 0, by = 30),
                     labels = LatLabel) +
  # scale_y_latitude(ticks = 15, labels = LatLabel) +
  scale_linetype_discrete(guide = "none") +
  facet_grid(angle ~ var, labeller = labeller(angle = angles_labs,
                                              var = var_lab)) +
  # axis_labs_smol +
  tag_facets("rc") +
  coord_sf(ylim = c(NA, 10),
           default_crs = "+proj=longlat +over") +
  theme(panel.spacing = grid::unit(1, "lines")) +
  theme(legend.box = "horizontal",
        legend.text = element_text(size = rel(0.7)),
        legend.box.spacing = grid::unit(0, "lines") ) +
  theme(strip.text.x = element_text(size = rel(1.5)))



ggsave(file.path(base_dir, "regresiones.svg"),
       plot = g,
       width = 530,
       height = 301,
       units = "mm",
       bg = NULL)



# enso --------------------------------------------------------------------

gdata <- ceof[, eof[[1]]$left, by = .(season)] %>%
  .[cEOF == "cEOF2"] %>%
  # .[, hgt := Anomaly(hgt)] %>%
  .[, arg := Arg(hgt)] %>%
  .[, mag := Mod(hgt)] %>%
  .[enso, on = "time"] %>%
  na.omit() %>%
  .[, q := ifelse(mag >= quantile(mag, 0.5), "|cEOF2| > 50%", "|cEOF2| < 50%")]

cor_enso_mag <- gdata[, correlate(mag, abs(oni))]

cor_enso_spearman <- gdata[, cor.test(mag, abs(oni), method = "spearman")]

cor_enso_mag_outlier <- gdata[oni < 1.5][, correlate(mag, abs(oni))]

cor_enso_mag_quantile <- gdata[mag >= quantile(mag, 0.5)] %>%
  .[oni < 1.5] %>%
  .[, correlate(mag, abs(oni))]

cos_model <- gdata %>%
  .[, lm(oni ~ I(sin(arg)) - 1, weights = mag )]

equation <- bquote(ONI  * {phantom() == phantom()} * .(signif(coef(cos_model)[1], 2)) * sin(Phase) )


gdata %>%
  ggplot(aes(arg, oni)) +
  geom_hline(yintercept = c(-0.5, 0, 0.5), size = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, size = 0.2, color = "gray50") +
  annotate(shadowtext:::GeomShadowText,
           label = c("- Real", "- Imaginary", "+ Real", "+ Imaginary", "- Real"),
           color = "gray60", bg.colour = "white", bg.r = 0.2,
           size = 6,
           y = -Inf, x = c(-pi, -pi/2, 0, pi/2, pi), angle = 90, hjust = 0) +

  geom_smooth(data = ~qwrap(.x, arg = c(-pi, pi) ~ c(-2*pi, 2*pi)),
              method = "lm", aes(weight = mag),
              formula = y ~ I(sin(x)) -1,
              se = FALSE, color = "black", size = 0.5) +
  geom_point(aes(color = q,
                 shape  = q),
             size = 5) +
  ggrepel::geom_text_repel(data = ~.x[order(-oni)][1:3],
                           aes(label = year(time)), size = 5) +

  scale_color_brewer("", palette = "Dark2") +
  scale_shape_manual("", values = c(20, 18)) +
  scale_y_continuous("ONI") +
  scale_x_continuous("Fase",
                     breaks = seq(-pi, pi, by = 45*pi/180),
                     labels = function(x) paste0(round(x*180/pi, 5), "º")) +
  coord_cartesian(xlim = c(-pi, pi)) +
  panel_background -> g

ggsave(file.path(base_dir, "enso.svg"),
       plot = g,
       width = 250,
       height = 143,
       units = "mm",
       bg = NULL)

