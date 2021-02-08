#' Main theme used in the paper
#'
#' Based on [ggplot2::theme_minimal()] but with minimal (wink) modifications.
#'
#'
#' @param base_size base font size, given in pts
#'
#' @export
theme_shceof <- function(base_size = 16) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # strip.background = ggplot2::element_rect(fill = NA, color = "gray30"),
      # text = element_text(family = font_rc),
      legend.position = "bottom", legend.box = "vertical",
      tagger.panel.tag.background = ggplot2::element_rect(color = NA),
      strip.text = ggplot2::element_text(size = ggplot2::rel(12/16)),
      # panel.spacing.y = grid::unit(5, "mm"),
      # panel.spacing.x = grid::unit(5, "mm"),
      # legend.spacing = grid::unit(2, "mm"),
      # panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      # plot.margin = grid::unit(rep(3, 4), "mm"),
      # # legend.title = element_blank(),
      # legend.box.spacing = grid::unit(3, "mm"),
      # legend.margin = ggplot2::margin(t = -5),
      panel.grid = ggplot2::element_blank(),
      # panel.grid = ggplot2::element_line(color = "gray10", size = 0.4, linetype = 3),
      panel.ontop = TRUE)
}
