#' Propagate mapping and parameters
#'
#' @param expr A ggplot-like expression consisting on
#'
#' @export
ditto <- function(expr) {
  plus_fun <- function(e1, e2) {
    if (is.list(e1)) {
      c(e1, list(e2))
    } else {
      list(e1, e2)
    }

  }

  geom_list <- eval(substitute(expr), envir = list("+.gg" = plus_fun), enclos = parent.frame())
  e1 <- geom_list[[1]]
  for (i in seq_along(geom_list)[-1]) {
    e2 <- geom_list[[i]]

    if (inherits(e2, "Scale")) {
      geom_list[[i]] <- stomp(e2, c(e1$geom_params, e1$aes_parama,  e1$stat_params))
    } else {
      geom_list[[i]]$geom_params <- stomp(e2$geom_params,
                                          e1$geom_params)
      geom_list[[i]]$aes_params <- stomp(e2$aes_params,
                                         e1$aes_params)
      geom_list[[i]]$stat_params <- stomp(e2$stat_params,
                                          e1$stat_params)
      geom_list[[i]]$mapping <- e1$mapping

    }


  }

  geom_list
}

#' @export
ditto_params <- function(expr) {
  plus_fun <- function(e1, e2) {
    if (is.list(e1)) {
      c(e1, list(e2))
    } else {
      list(e1, e2)
    }

  }

  geom_list <- eval(substitute(expr), envir = list("+.gg" = plus_fun), enclos = parent.frame())
  e1 <- geom_list[[1]]
  for (i in seq_along(geom_list)[-1]) {
    e2 <- geom_list[[i]]

    if (inherits(e2, "Scale")) {
      geom_list[[i]] <- stomp(e2, c(e1$geom_params, e1$aes_parama,  e1$stat_params))
    } else {
      geom_list[[i]]$geom_params <- stomp(e2$geom_params,
                                          e1$geom_params)
      geom_list[[i]]$aes_params <- stomp(e2$aes_params,
                                         e1$aes_params)
      geom_list[[i]]$stat_params <- stomp(e2$stat_params,
                                          e1$stat_params)
    }


  }

  geom_list


}

stomp <- function(original, new) {
  names <- names(new)
  for (name in names) {
    original[[name]] <- new[[name]]
  }
  original
}
