pp <- pp_regr[cEOF == "cEOF2"] %>%
  .[, land := MaskLand(lon, lat)] %>%
  .[land == TRUE] %>%
  .[, pval := Pvaluate(estimate, std.error, df, "fdr"), by = .(cEOF, angle)] %>%
  .[lat %between% c(-65, 10) & lon %between% c(155, 345)]

sst_regr[cEOF == "cEOF2"] %>%
  .[lat %between% c(-65, 10) & lon %between% c(155, 345)] %>%
  ggplot(aes(lon, lat)) +

  geom_contour_fill(aes(z = estimate, fill = ..level..), breaks = AnchorBreaks(0, exclude = 0)) +

  scale_fill_divergent_discretised("SST (K)",
                                   # labels = one_less,
                                   guide = guide_colorsteps_bottom(width = 15,
                                                                   show.limits = FALSE, ticks = TRUE,
                                                                   order = 1)) +

  scale_linetype(guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_contour_fill(data = pp, aes(z = estimate, fill = ..level..),
                    breaks = AnchorBreaks(0, exclude = 0)) +
  geom_contour_pval(data = pp, aes(z = pval)) +
  # stat_subset(data = pp, aes(subset = pval <= 0.05)) +
  scale_fill_divergent_discretised("Precipitation (correlation)",
                                   # limits = c(-1, 1),
                                   low = "#8E521C",
                                   high = "#00665E",
                                   guide = guide_colorsteps_bottom(15, order = 99)) +
  geom_contour2(data = psi_regr[cEOF == "cEOF2"],
                aes(z = estimate, linetype = factor(sign(..level..))),
                size = 0.3) +
  geom_qmap() +
  grid_panel(spoke, aes(angle = -angle, var = "SST"), inherit.aes = FALSE) +
  scale_x_continuous(NULL, expand = c(0, 0),
                     breaks = seq(0, 360, by = 60),
                     labels = LonLabel) +

  scale_y_continuous(NULL, breaks  = seq(-90, 0, by = 30), expand = c(0, 0)) +

  facet_wrap(angle ~ ., labeller = labeller(angle = angles_labs)) +
  # coord_quickmap(ylim = c(NA, 10)) +
  coord_sf(ylim = c(-65, 10),
           xlim = c(155, 345),
           expand = FALSE,
           # crs = "+proj=longlat +pm=180 +over",
           default_crs = "+proj=longlat +lon_wrap=180 +over") +
  theme(legend.box = "horizontal",
        legend.text = element_text(size = rel(0.7)),
        legend.box.spacing = grid::unit(0, "lines") )

ggsave(here::here("analysis/figures/ppcongremet.png"), bg = "white",
       width = 15*1.3, height = 9.5*1.3, units = "cm")

