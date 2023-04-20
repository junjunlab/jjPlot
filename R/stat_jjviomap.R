#' @title stat_jjviomap
#' @name stat_jjviomap
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param geom geom
#' @param position position
#' @param show.legend show.legend
#' @param trim trim
#' @param bw bw
#' @param adjust adjust
#' @param kernel kernel
#' @param n n
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @export
stat_jjviomap <- function(mapping = NULL, data = NULL, geom = "jjviomap",
                          position = "identity", show.legend = NA,
                          trim = TRUE,
                          bw = "nrd0",
                          adjust = 1,
                          kernel = "gaussian",
                          n = 512,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatJjviomap,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trim = trim,
                  bw = bw,
                  adjust = adjust,
                  kernel = kernel,
                  n = n,
                  ...)
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatJjviomap <- ggplot2::ggproto("StatJjviomap", ggplot2::Stat,
                                 # compute data
                                 compute_group = function(data, scales,
                                                          trim = TRUE,
                                                          bw = "nrd0",
                                                          adjust = 1,
                                                          kernel = "gaussian",
                                                          n = 512) {

                                   # calculate density
                                   density_data <- stats::density(data$val,
                                                                  bw = bw, adjust = adjust,
                                                                  kernel = kernel,
                                                                  n = n)

                                   # data range
                                   range_data <- range(data$val)

                                   # whether tirm head and tail
                                   if(trim == TRUE){
                                     # data frame
                                     new_daframe <- data.frame(vio_y = density_data$x,
                                                               vio_x = density_data$y)

                                     # trim head and tail
                                     new_daframe$vio_y <- dplyr::case_when(
                                       new_daframe$vio_y < range_data[1] ~ range_data[1],
                                       new_daframe$vio_y > range_data[2] ~ range_data[2],
                                       TRUE ~ new_daframe$vio_y)

                                   }else{
                                     # data frame
                                     new_daframe <- data.frame(vio_y = density_data$x,
                                                               vio_x = density_data$y)
                                   }

                                   # add x y
                                   new_daframe$x <- data$x[1]
                                   new_daframe$y <- data$y[1]
                                   new_daframe

                                 },
                                 required_aes = c("x", "y","val")
)
