#' @title stat_jjPointPie
#' @name stat_jjPointPie
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param geom geom
#' @param position position
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#' @export
stat_jjPointPie <- function(mapping = NULL, data = NULL, geom = "jjPointPie",
                            position = "identity", show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatJjPointPie,
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
# stat
StatJjPointPie <- ggplot2::ggproto("StatJjPointPie", ggplot2::Stat,
                                   # compute data
                                   compute_group = function(data, scales) {

                                     data
                                   },
                                   required_aes = c("x", "y", "pievar")
)
