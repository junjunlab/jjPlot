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
                                    out <- qq %>% as.list %>% data.frame
                                    names(out) <- c("ymin", "lower", "middle",
                                                    "upper", "ymax")

                                    # include second max/min value
                                    out <- out %>% dplyr::mutate(iqr = upper - lower) %>%
                                      dplyr::mutate(ymin = ifelse(lower - iqr*1.5 <= ymin,
                                                                  ymin,
                                                                  lower - iqr*1.5),
                                                    ymax = ifelse(upper + iqr*1.5 >= ymax,
                                                                  ymax,
                                                                  upper + iqr*1.5))

                                    # filter outliers
                                    outlier <- data$y > out$upper + out$iqr*1.5 | data$y < out$lower - out$iqr*1.5

                                    # save as list
                                    out$outlier_data <- list(data$y[outlier])

                                    # add x
                                    out$x <- data$x[1]
                                    out
                                  },
                                  required_aes = c("x", "y")
)
