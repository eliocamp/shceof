#' Draws a violin plot representing a T-Student distribution
#'
#'
#'
#' @export
geom_tstudent <- function (mapping = NULL, data = NULL, stat = StatTStudent, position = "dodge",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, orientation = NA,
                           df = 30,...)
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, df = df, orientation = orientation, ...))
}


#' @export
StatTStudent <- ggproto("StatTStudent", Stat,
                        required_aes = c("x|y", "mean", "sd"),
                        optional_aes = c("df"),
                        extra_params = c("na.rm", "orientation"),
                        setup_params = function(data, params) {
                          params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE,
                                                                         main_is_continuous = TRUE)
                          has_x <- !(is.null(data$x) && is.null(params$x))
                          has_y <- !(is.null(data$y) && is.null(params$y))
                          if (!has_x && !has_y) {
                            stop("stat_tstudent() requires an x or y aesthetic.")
                          }

                          params
                        },
                        compute_group = function(self, data, scales, df = 30, confidence = .95, flipped_aes = FALSE) {

                          if (is.null(data$df)) {
                            data$df <- df
                          }

                          alpha <- (1-confidence)/2
                          q <- qt(c(alpha, 1-alpha), data$df)
                          q <- seq(min(q), max(q), length.out = 40)

                          density <- dt(q, data$df)

                          data.frame(y = q*data$sd + data$mean, x = data$x,
                                     density = density/data$sd,
                                     scaled = density/max(density))


                        },
                        compute_panel = function(self, data, scales, df = 30, confidence = .95, flipped_aes = FALSE) {

                          data <- ggplot2::flip_data(data, flipped_aes)
                          data <- ggproto_parent(Stat, self)$compute_panel(
                            data, scales, df = df, confidence = confidence, flipped_aes = flipped_aes
                          )

                          # choose how violins are scaled relative to each other
                          data$violinwidth <- data$density / max(data$density)
                          data$flipped_aes <- flipped_aes
                          ggplot2::flip_data(data, flipped_aes)
                        }



)

