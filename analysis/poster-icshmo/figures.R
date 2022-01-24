base_dir <- here::here("analysis", "poster-icshmo")

source(file.path(base_dir, "data.R"))


library(ggplot2)
library(ggperiodic)
library(tagger)
sf::sf_use_s2(FALSE)
mgp <- latex2exp::TeX('Geopotential height ($m^2s^{-1}$)')

theme_set(theme_shceof(base_size = 10) +
            theme(legend.box.spacing = grid::unit(-.5, "lines")))

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

term_lab <- c("Real" = "0º (~PSA2)",
              "Imaginary" = "90º (~PSA1)")
g <- regr %>%
  .[cEOF == "cEOF2" & lev == 200] %>%
  # .[term == "I" & lev == 50] %>%
  .[, p.value := Pvaluate(estimate, std.error, df, "fdr"), by = .(lev, cEOF, term)] %>%
  periodic(lon = c(0, 360)) %>%
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = estimate), breaks = AnchorBreaks(exclude = 0),
                    global.breaks = FALSE) +
  geom_contour_tanaka(aes(z = estimate),
                      breaks = AnchorBreaks(exclude = 0), global.breaks = FALSE) +
  scale_fill_divergent(mgp,
                       breaks = AnchorBreaks(0, 200, exclude = 0),
                       guide = guide_colorsteps_bottom(15, even.steps = TRUE)) +

  # geom_contour_pval(aes(z = p.value)) +

  geom_qmap() +

  scale_x_longitude(labels = NULL) +
  scale_y_latitude(labels = NULL,
                   minor_breaks = NULL) +
  grid_y +
  facet_wrap(~term, labeller = labeller(term = term_lab)) +
  geom_coords() +
  coord_polar() +
  tag_facets("panel")

ggsave(file.path(base_dir, "ceofs.png"), width = 1000, height = 700, units = "px", bg = "white")

plot_sst_psi <- function(sst_regr, psi_regr) {
  sst_gdata <- sst_regr %>%
    # .[cEOF == "cEOF2"] %>%
    .[lat %between% c(-90, 10)] %>%
    .[, p_val := Pvaluate(estimate, std.error, df, "fdr"),
      by = .(cEOF, term, angle)] %>%
    .[, var := "SST"]

  rsst <- max(range(abs(sst_regr[!is.na(cEOF), ]$estimate), na.rm = TRUE))
  breaks_sst <- AnchorBreaks(0, bins = 20, exclude = 0)(c(-rsst, rsst))


  stream_gdata <- psi_regr %>%
    # .[cEOF == "cEOF2"] %>%
    # .[abs(estimate) > 1, estimate := NA] %>%
    .[lat %between% c(-90, 10)] %>%
    .[, p_val := Pvaluate(estimate, std.error, df, "fdr"), by = .(cEOF, angle, term)] %>%
    .[, var := "Streamfunction"]

  rstream <- max(range(abs(psi_regr$estimate),
                       na.rm = TRUE))*1e-7
  breaks_stream <- AnchorBreaks(0, bins = 20, exclude = 0)(c(-rstream, rstream))


  # browser()
  stream_gdata %>%
    periodic(lon = c(0, 360)) %>%
    ggplot(aes(lon, lat)) +

    geom_contour_fill(data = sst_gdata,
                      aes(z = estimate, fill = ..level..),
                      breaks = breaks_sst) +
    geom_contour_pval(data = sst_gdata, aes(z = p_val)) +

    scale_fill_divergent_discretised("SST (K)",
                                     labels = one_less,
                                     guide = guide_colorsteps_bottom(width = 15,
                                                                     show.limits = FALSE, ticks = TRUE,
                                                                     order = 1)) +
    ggnewscale::new_scale_fill() +
    geom_contour_fill(aes(z = estimate*1e-7, fill = ..level..),
                      breaks = breaks_stream) +
    geom_contour_pval(aes(z = p_val)) +
    # geom_streamline(aes(dx = fx, dy = fy), min.L = 3, L = 10,
    #                 size = 0.1, res = 2, nx = 22, ny = 12,
    #                 arrow.angle = 14, arrow.length = 0.2) +
    geom_vector(aes(dx = fx, dy = fy), skip = 3, min.mag = 0.002,
                size = 0.1) +
    scale_mag(max_size = .5, guide = "none") +
    geom_qmap(keep = .015) +
    scale_fill_divergent_discretised(latex2exp::TeX("Streamfunction ($m^2s^{-1}\\times 1e^{-7}$)"),
                                     labels = one_less,
                                     low = scales::muted("red"),
                                     high = scales::muted("blue"),
                                     guide = guide_colorsteps_bottom(15, show.limits = FALSE,
                                                                     order = 99)) +
    grid_panel(spoke, aes(angle = -angle, var = "SST"), inherit.aes = FALSE) +
    scale_x_continuous(NULL, expand = c(0, 0),
                       breaks = seq(0, 360, by = 60),
                       labels = LonLabel) +

    scale_y_continuous(NULL, breaks  = seq(-90, 0, by = 30), expand = c(0, 0)) +

    facet_grid(angle ~ var, labeller = labeller(angle = angles_labs)) +
    # coord_quickmap(ylim = c(NA, 10)) +
    coord_sf(ylim = c(-89, 10), expand = FALSE,
             # crs = "+proj=longlat +pm=180 +over",
             default_crs = "+proj=longlat +lon_wrap=180 +over") +
    theme(legend.box = "horizontal",
          legend.text = element_text(size = rel(0.7)),
          legend.box.spacing = grid::unit(0, "lines") ) +
    tag_facets("rc") +
    theme(strip.text.x = element_text(size = rel(1.5)),
          strip.text.y = element_text(size = rel(1.5)))
}


g <- plot_sst_psi(sst_regr[cEOF == "cEOF2"], psi_regr[cEOF == "cEOF2"])
ggsave(file.path(base_dir, "sst-psi.png"), width = 1980, height = 1650, units = "px", bg = "white")


plot_pp <- function(pp_regr, t2m_regr, hgt850_regr) {
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


  pp_regr %>%
    # .[lat %between% lats] %>%

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


    scale_fill_divergent_discretised("Precipitation (correlation)",
                                     # limits = c(-1, 1),
                                     low = "#8E521C",
                                     high = "#00665E",
                                     guide = guide_colorsteps_bottom(15, order = 99)) +
    geom_contour_pval(aes(z = p.value)) +

    ggnewscale::new_scale_fill() +


    geom_contour_fill(data = t2m_regr,
                      aes(z = estimate, fill = ..level..),
                      breaks = AnchorBreaks(exclude = 0)) +
    geom_contour_tanaka(data = t2m_regr,
                        aes(z = estimate),
                        breaks = AnchorBreaks(exclude = 0)) +

    scale_fill_divergent_discretised("Temperature",
                                     # limits = c(-1, 1),
                                     guide = guide_colorsteps_bottom(15, order = 1)) +
    geom_contour_pval(data = t2m_regr, aes(z = p.value)) +

    geom_contour2(data = copy(hgt850_regr)[, var := "2m Temperature"],
                  aes(z = estimate, linetype = factor(-sign(..level..))),
                  size = 0.2, breaks = AnchorBreaks(0, exclude = 0)) +

    geom_qmap() +

    grid_panel(spoke, aes(angle = -angle, var = "SST"), inherit.aes = FALSE) +

    scale_x_continuous("", expand = c(0, 0), breaks = seq(0, 360, by = 60),
                       labels = LonLabel) +
    scale_y_continuous("", expand = c(0, 0), breaks = seq(-90, 0, by = 30),
                       labels = LatLabel) +
    # scale_y_latitude(ticks = 15, labels = LatLabel) +
    scale_linetype_discrete(guide = "none") +
    facet_grid(angle ~ var, labeller = labeller(angle = angles_labs)) +
    # axis_labs_smol +
    tag_facets("rc") +
    coord_sf(ylim = c(NA, 10),
             default_crs = "+proj=longlat +over") +
    theme(panel.spacing = grid::unit(1, "lines")) +
    theme(legend.box = "horizontal",
          legend.text = element_text(size = rel(0.7)),
          legend.box.spacing = grid::unit(0, "lines") ) +
    theme(strip.text.x = element_text(size = rel(1.5)))
}



g <- plot_pp(pp_regr[cEOF == "cEOF2"], t2m_regr[cEOF == "cEOF2"], hgt850_regr[cEOF == "cEOF2"])
ggsave(file.path(base_dir, "pp-temp.png"), width = 1980, height = 1650,
       units = "px")
