#' @title stat_jjboxplot
#' @name stat_jjboxplot
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param geom geom
#' @param position position
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @import tidyverse
#'
#' @export
stat_jjboxplot <- function(mapping = NULL, data = NULL, geom = "jjboxplot",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatJjboxplot,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatJjboxplot <- ggplot2::ggproto("StatJjboxplot", ggplot2::Stat,
                                  # compute data
                                  compute_group = function(data, scales) {
                                    probs <- c(0, 0.25, 0.5, 0.75, 1)
                                    qq <- quantile(data$y, probs, na.rm = TRUE)
                                    names(qq) <- NULL
                                    out <- data.frame("ymin" = qq[1], "lower" = qq[2], "middle" = qq[3],
                                                      "upper" = qq[4], "ymax" = qq[5])


                                    # include second max/min value
                                    val <- unique(data$y)
                                    val <- val[order(val)]

                                    out <- out %>% dplyr::mutate(iqr = upper - lower) %>%
                                      dplyr::mutate(ymin = ifelse(lower - iqr*1.5 <= ymin,
                                                                  ymin,
                                                                  val[2]),
                                                    ymax = ifelse(upper + iqr*1.5 >= ymax,
                                                                  ymax,
                                                                  val[length(val)-1]))

                                    # filter outliers
                                    # outlier <- data$y > out$upper + out$iqr*1.5 | data$y < out$lower - out$iqr*1.5
                                    data$y > out$ymax | data$y < out$ymin

                                    # save as list
                                    out$outlier_data <- list(data$y[outlier])

                                    # add x
                                    out$x <- data$x[1]
                                    out
                                  },
                                  required_aes = c("x", "y")
)
