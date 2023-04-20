#' @title stat_jjpie
#' @name stat_jjpie
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
#' @export
stat_jjpie <- function(mapping = NULL, data = NULL, geom = "jjpie",
                       position = "identity", show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatJjpie,
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
StatJjpie <- ggplot2::ggproto("StatJjpie", Stat,
                              # compute data
                              compute_panel = function(data, scales) {

                                data
                              },
                              required_aes = c("x", "y","piefill")
)
