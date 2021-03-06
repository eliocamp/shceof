library(grid)
library(gtable)
#' @title Layout panels in a grid with nested strips
#'
#' @description \code{facet_nest()} form a matrix of panels defined by row and
#'   column faceting variables and nests grouped facets.
#'
#' @param
#' rows,cols,scales,space,shrink,labeller,as.table,switch,drop,margins,facets
#' see \code{\link[ggplot2]{facet_grid}}
#' @param nest_line a \code{logical} vector of length 1, indicating whether to
#'   draw a nesting line to indicate the nesting of variables.
#' @param resect  a \code{unit} vector of length 1, indicating how much the
#'   nesting line should be shortened.
#' @param bleed a \code{logical} vector of length 1, indicating wether merging
#'   of lower-level variables is allowed when the higher-level variables are
#'   seperate. See details.
#'
#' @details Unlike \code{facet_grid()}, this function only automatically expands
#'   missing variables when they have no variables in that direction, to allow
#'   for unnested variables. It still requires at least one layer to have all
#'   faceting variables.
#'
#'   Hierarchies are inferred from the order of variables supplied to
#'   \code{rows} or \code{cols}. The first variable is interpreted to be the
#'   outermost variable, while the last variable is interpreted to be the
#'   innermost variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. Strips are
#'   automatically grouped when they span a nested variable.
#'
#'   The \code{bleed} argument controls wether lower-level variables are allowed
#'   to be merged when higher-level are different, i.e. they can bleed over
#'   hierarchies. Suppose the \code{facet_grid()} behaviour would be the
#'   following:
#'
#'   \code{[_1_][_2_][_2_]} \cr \code{[_3_][_3_][_4_]}
#'
#'   In such case, the default \code{bleed = FALSE} argument would result in the
#'   following:
#'
#'   \code{[_1_][___2____]} \cr \code{[_3_][_3_][_4_]}
#'
#'   Whereas \code{bleed = TRUE} would allow the following:
#'
#'   \code{[_1_][___2____]} \cr \code{[___3____][_4_]}
#'
#' @export
#'
#' @seealso See \code{\link[ggplot2]{facet_grid}} for descriptions of the
#'   original arguments. See \code{\link[grid]{unit}} for the construction of a
#'   \code{unit} vector.
#'
#' @references https://stackoverflow.com/questions/40316169/nested-facets-in-ggplot2-spanning-groups
#' @examples
#' df <- iris
#' df$nester <- ifelse(df$Species == "setosa",
#'                     "Short Leaves",
#'                     "Long Leaves")
#'
#' ggplot(df, aes(Sepal.Length, Petal.Length)) +
#'   geom_point() +
#'   facet_nested(~ nester + Species)
#'
#' @import ggplot2
facet_nested <- function(rows = NULL, cols = NULL, scales = "fixed", space = "fixed",
                         shrink = TRUE, labeller = "label_value", as.table = TRUE,
                         switch = NULL, drop = TRUE, margins = FALSE, facets = NULL,
                         nest_line = FALSE, resect = unit(0, "mm"), bleed = FALSE)
{
  if (!is.null(facets)) {
    rows <- facets
  }
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(x = any(scales %in% c("free_x", "free")),
               y = any(scales %in% c("free_y", "free")))
  
  space <- match.arg(space, c("fixed","free_x","free_y","free"))
  space_free <- list(x = any(space %in% c("free_x", "free")),
                     y = any(space %in% c("free_y", "free")))
  
  if (!is.null(switch) && !switch %in% c("both","x","y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }
  
  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)
  n <- length(facets_list)
  if (n > 2L) {
    stop("A grid facet specification can't have more than two dimensions",
         .call = FALSE)
  }
  if (n == 1L) {
    rows <- quos()
    cols <- facets_list[[1]]
  } else {
    rows <- facets_list[[1]]
    cols <- facets_list[[2]]
  }
  labeller <- ggplot2:::check_labeller(labeller)
  ggproto(NULL, FacetNested, shrink = shrink,
          params = list(
            rows = rows,
            cols = cols,
            margins = margins,
            free = free,
            space_free = space_free,
            labeller = labeller,
            as.table = as.table,
            switch = switch,
            drop = drop,
            nest_line = nest_line,
            resect = resect,
            bleed = bleed
          ))
}

#' @rdname facet_nested
#' @format NULL
#' @usage NULL
#' @export
FacetNested <- ggplot2::ggproto(
  "FacetNested", ggplot2::
    FacetGrid,
  map_data = function(data, layout, params) {
    # Handle empty data
    if (ggplot2:::empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    # Setup variables
    rows <- params$rows
    cols <- params$cols
    
    vars <- c(names(rows), names(cols))
    margin_vars <- list(intersect(names(rows), names(data)),
                        intersect(names(cols), names(data)))
    
    # Add variables
    data <- reshape2::add_margins(data, margin_vars, params$margins)
    facet_vals <- ggplot2:::eval_facets(c(rows, cols), data, params$plot$env)
    
    # Only set as missing if it has no variable in that direction
    missing_facets <- character(0)
    if (!any(names(rows) %in% names(facet_vals))){
      missing_facets <- c(missing_facets, setdiff(names(rows), names(facet_vals)))
    }
    if (!any(names(cols) %in% names(facet_vals))){
      missing_facets <- c(missing_facets, setdiff(names(cols), names(facet_vals)))
    }
    
    # Fill in missing values
    if (length(missing_facets) > 0) {
      to_add <- unique(layout[missing_facets])
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))
      data <- plyr::unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- plyr::unrowname(
        cbind(facet_vals[data_rep, , drop = FALSE],
              to_add[facet_rep, , drop = FALSE])
      )
    }
    
    # Match columns to facets
    if (nrow(facet_vals) == 0) {
      data$PANEL <- NO_PANEL
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      keys <- plyr::join.keys(facet_vals, layout, by = vars[vars %in% names(facet_vals)])
      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },
  compute_layout = function(data, params)
  {
    rows <- params$rows
    cols <- params$cols
    dups <- intersect(names(rows), names(cols))
    if (length(dups) > 0) {
      stop("Facetting variables can only appear in row or cols, not both.\n",
           "Problems: ", paste0(dups, collapse = "'"), call. = FALSE)
    }
    base_rows <- combine_nested_vars(data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ggplot2:::ulevels(x)))
    }
    base_cols <- combine_nested_vars(data, params$plot_env, cols, drop = params$drop)
    base <- ggplot2:::df.grid(base_rows, base_cols)
    base <- reshape2::add_margins(base, list(names(rows), names(cols)), params$margins)
    base <- unique(base)
    panel <- plyr::id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))
    rows <- if (!length(names(rows))) {
      1L
    } else {
      plyr::id(base[names(rows)], drop = TRUE)
    }
    cols <- if (!length(names(cols))) {
      1L
    } else {
      plyr::id(base[names(cols)], drop = TRUE)
    }
    panels <- data.frame(PANEL = panel, ROW = rows, COL = cols,
                         base, check.names = FALSE, stringsAsFactors = FALSE)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL
    panels$SCALE_X <- if (params$free$x) {
      panels$COL
    } else {
      1L
    }
    panels$SCALE_Y <- if (params$free$y) {
      panels$ROW
    } else {
      1L
    }
    panels
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params)
  {
    panel_table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales,
                                         ranges, coord, data, theme, params)
    
    # Setup strips
    col_vars  <- unique(layout[names(params$cols)])
    row_vars  <- unique(layout[names(params$rows)])
    attr(col_vars, "type")  <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type")  <- "rows"
    attr(row_vars, "facet") <- "grid"
    
    # Build strips
    strips <- render_strips(col_vars, row_vars, params$labeller, theme)
    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
    
    # Merging strips
    merge_cols <- apply(col_vars, 2, function(x) any(rle(x)$lengths > 1))
    merge_rows <- apply(row_vars, 2, function(x) any(rle(x)$lengths > 1))
    
    if (any(merge_cols)) {
      if (switch_x) {
        panel_table <- merge_strips(panel_table, strips$x$bottom,
                                    col_vars, switch_x, params, theme, "x")
      } else {
        panel_table <- merge_strips(panel_table, strips$x$top,
                                    col_vars, switch_x, params, theme, "x")
      }
    }
    
    if (any(merge_rows)) {
      if (switch_y) {
        panel_table <- merge_strips(panel_table, strips$y$left,
                                    row_vars, switch_y, params, theme, "y")
      } else {
        panel_table <- merge_strips(panel_table, strips$y$right,
                                    row_vars, switch_y, params, theme, "y")
      }
    }
    panel_table
  }
)

# Helper functions -----------------------------------------------

combine_nested_vars <- function(data, env = emptyenv(), vars = NULL, drop = TRUE) {
  if (length(vars) == 0)
    return(data.frame())
  values <- ggplot2:::compact(plyr::llply(data, ggplot2:::eval_facets, facets = vars))
  has_all <- unlist(lapply(values, length)) == length(vars)
  if (!any(has_all)) {
    missing <- lapply(values, function(x) setdiff(names(vars), names(x)))
    missing_txt <- vapply(missing, var_list, character(1))
    name <- c("Plot", paste0("Layer ", seq_len(length(data) - 1)))
    stop("At least one lyaer must contain all faceting variables: ",
         var_list(names(vars)), ".\n", paste0("* ", name, " is missing",
                                              missing_txt, collapse = "\n"),
         call. = FALSE)
  }
  base <- unique(plyr::ldply(values[has_all]))
  if (!drop) {
    base <- ggplot2:::unique_combs(base)
  }
  for (value in values[!has_all]) {
    if (ggplot2:::empty(value))
      next
    old <- base[setdiff(names(base), names(value))]
    new <- unique(value[intersect(names(base), names(value))])
    if (drop) {
      new <- ggplot2:::unique_combs(new)
    }
    old[setdiff(names(base), names(value))] <- rep("", nrow(old))
    base <- rbind(base, ggplot2:::df.grid(old, new))
  }
  if (ggplot2:::empty(base)) {
    stop("Facetting variables must have at least one value",
         call. = FALSE)
  }
  base
}


merge_strips <- function(panel_table, strip, vars, switch, params, theme, orient = c("x","y"))
{
  if (is.null(strip)) {
    return(panel_table)
  }
  n_levels <- nrow(strip[[1]]$layout)
  splitstrip <- lapply(seq_len(n_levels), function(i) {
    switch(orient,
           x = lapply(strip, function(x) x[i, ]),
           y = lapply(strip, function(x) x[, i]))
    
  })
  
  if (params$bleed) {
    merge <- apply(vars, 2, function(x) any(rle(x)$lengths > 1))
  } else {
    merge <- sapply(1:ncol(vars), function(i){
      x <- apply(subset.data.frame(vars, select = seq(i)), 1, paste0, collapse = "")
      return(any(rle(x)$lengths > 1))
    })
  }
  
  if (orient == "y" && !switch) {
    vars <- rev(vars)
    merge <- rev(merge)
  }
  if (orient == "x" && switch) {
    vars <- rev(vars)
    merge <- rev(merge)
    splitstrip <- rev(splitstrip)
  }
  
  sizes <- switch(orient,
                  x = do.call(unit.c, lapply(splitstrip, max_height)),
                  y = do.call(unit.c, lapply(splitstrip, max_width)))
  
  assign("panel_table", panel_table, 1)
  
  grabwhat <- switch(orient,
                     x = grepl("strip-t|strip-b", panel_table$layout$name),
                     y = grepl("strip-r|strip-l", panel_table$layout$name))
  
  pos_y <- unique(panel_table$layout$t[grabwhat])
  pos_x <- unique(panel_table$layout$l[grabwhat])
  panel_pos <- find_panel(panel_table)
  
  if (orient == "x") {
    nudge <- if (pos_y < panel_pos$t) -1 else -1
    panel_table <- panel_table[-pos_y,]
    panel_table <- gtable_add_rows(panel_table, sizes, pos_y + nudge)
    
  } else {
    nudge <- if (pos_x < panel_pos$l) -1 else 0
    panel_table <- panel_table[, -pos_x]
    panel_table <- gtable_add_cols(panel_table, sizes, pos_x + nudge)
  }
  
  for(i in seq_len(n_levels)) {
    if (!merge[i]) {
      panel_table <- gtable_add_grob(
        panel_table, splitstrip[[i]],
        t = pos_y + switch(orient, x = i + nudge, y = 0),
        l = pos_x + switch(orient, x = 0, y = i + nudge),
        z = 2, clip = "on",
        name = paste0("strip-", orient, "-", seq_along(splitstrip[[i]]))
      )
    } else {
      j <- as.numeric(as.factor(vars[,i]))
      ends <- cumsum(rle(j)$lengths)
      starts <- c(1, which(diff(j) != 0) + 1)
      panel_table <- gtable_add_grob(
        panel_table, splitstrip[[i]][starts],
        t = switch(orient, x = pos_y + i + nudge, y = pos_y[starts]),
        b = switch(orient, x = pos_y + i + nudge, y = pos_y[ends]),
        l = switch(orient, x = pos_x[starts], y = pos_x + i + nudge),
        r = switch(orient, x = pos_x[ends],   y = pos_x + i + nudge),
        z = 2, clip = "on",
        name = paste0("strip-", orient, "-", seq_along(splitstrip[[i]][starts]))
      )
      
      if(params$nest_line && any(starts != ends)) {
        insert_here <- which(starts != ends)
        indicator <- linesGrob(
          x = switch(orient,
                     x = unit(c(0, 1), "npc") + c(1, -1) * params$resect,
                     y = if (switch) c(1, 1) else c(0, 0)),
          y = switch(orient,
                     x = if (switch) c(1, 1) else c(0, 0),
                     y = unit(c(0, 1), "npc") + c(1, -1) * params$resect),
          gp = grid::gpar(col = theme$line$colour,
                          lty = theme$line$linetype,
                          lwd = theme$line$size * .pt,
                          lineend = theme$line$lineend))
        panel_table <- gtable_add_grob(
          panel_table, lapply(seq_along(insert_here), function(x) indicator),
          t = switch(orient, x = pos_y + i + nudge,
                     y = pos_y[starts[insert_here]]),
          b = switch(orient, x = pos_y + i + nudge,
                     y = pos_y[ends[insert_here]]),
          l = switch(orient, x = pos_x[starts[insert_here]],
                     y = pos_x + i + nudge),
          r = switch(orient, x = pos_x[ends[insert_here]],
                     y = pos_x + i + nudge),
          z = 3, clip = "on",
          name = "nesting-indicator"
        )
      }
    }
  }
  panel_table
}
