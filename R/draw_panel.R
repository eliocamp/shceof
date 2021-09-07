
draw_grid_panel <- function(data, panel_params, coord, grob, debug) {
  if (!is.null(debug)) {
    coords <- coord$transform(data, panel_params)
    debug(data, coords)
  }
  if (grid::is.grob(grob)) {
    grob
  } else {
    if (!is.function(grob))
      stop("Invalid grob in GeomPanel")
    coords <- coord$transform(data, panel_params)
    grob(data, coords)
  }
}


GeomGridPanel <- ggplot2::ggproto("GeomGridPanel", ggplot2::Geom,
                         ## No required_aes
                         ## No default_aes
                         ## No draw_key
                         draw_panel = draw_grid_panel)

## Provide 'stat', 'position', etc because they may be useful
## modifications to the 'data'
#' @export
grid_panel <- function(grob=grid::nullGrob(),
                       mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", inherit.aes = TRUE,
                       debug = NULL, ...) {
  if (!grid::is.grob(grob) && !is.function(grob))
    stop("Invalid 'grob' argument;  must be grob or function")
  if (!is.null(debug) && !is.function(debug))
    stop("Invalid 'debug' argument:  must be NULL or function")
  ggplot2::layer(geom = GeomGridPanel,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        inherit.aes = inherit.aes,
        show.legend = FALSE,
        params = list(grob = grob, debug = debug, ...))
}


