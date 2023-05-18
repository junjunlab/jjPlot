#' @title geom_jjtriangle
#' @name geom_jjtriangle
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param type type
#' @param rect rect
#' @param ... ...
#'
#' @export
geom_jjtriangle <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            type = "ul",
                            rect = FALSE,
                            ...) {
  ggplot2::layer(
    geom = GeomJjtriangle,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  type = type,
                  rect = rect,
                  ...)
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomJjtriangle <- ggplot2::ggproto("GeomJjtriangle", ggplot2::Geom,
                                   required_aes = c("x", "y"),
                                   default_aes = aes(colour = "black",
                                                     fill = "grey80",
                                                     width = 1,
                                                     size = 0.1,
                                                     linetype = 1,
                                                     alpha = 1),

                                   draw_key = draw_key_polygon,

                                   draw_group = function(data, panel_scales, coord,
                                                         type = "ul",
                                                         rect = FALSE) {

                                     # calculate xmin xmax ymin ymax
                                     data <- data %>%
                                       dplyr::mutate(x0 = x - width/2,
                                                     x1 = x + width/2,
                                                     y0 = y - width/2,
                                                     y1 = y + width/2)

                                     # Transform the data
                                     coords <- coord$transform(data, panel_scales) %>%
                                       dplyr::mutate(
                                         # x0 = scales::rescale(x0,from = panel_scales$x.range),
                                         x1 = scales::rescale(x1,from = panel_scales$x.range),
                                         # y0 = scales::rescale(y0,from = panel_scales$y.range),
                                         y1 = scales::rescale(y1,from = panel_scales$y.range))

                                     # define coordinates
                                     if(rect == FALSE){
                                       if(type == "ul"){
                                         poly_x = c(coords$x0,coords$x0,coords$x1)
                                         poly_y = c(coords$y0,coords$y1,coords$y1)
                                       }else if(type == "bl"){
                                         poly_x = c(coords$x0,coords$x0,coords$x1)
                                         poly_y = c(coords$y0,coords$y1,coords$y0)
                                       }else if(type == "ur"){
                                         poly_x = c(coords$x0,coords$x1,coords$x1)
                                         poly_y = c(coords$y1,coords$y1,coords$y0)
                                       }else if(type == "br"){
                                         poly_x = c(coords$x0,coords$x1,coords$x1)
                                         poly_y = c(coords$y0,coords$y1,coords$y0)
                                       }else if(type == "mu"){
                                         poly_x = c(coords$x,coords$x0,coords$x1)
                                         poly_y = c(coords$y,coords$y1,coords$y1)
                                       }else if(type == "mb"){
                                         poly_x = c(coords$x,coords$x0,coords$x1)
                                         poly_y = c(coords$y,coords$y0,coords$y0)
                                       }else if(type == "ml"){
                                         poly_x = c(coords$x,coords$x0,coords$x0)
                                         poly_y = c(coords$y,coords$y0,coords$y1)
                                       }else if(type == "mr"){
                                         poly_x = c(coords$x,coords$x1,coords$x1)
                                         poly_y = c(coords$y,coords$y0,coords$y1)
                                       }else if(type == "centri"){
                                         poly_x = c(coords$x0,coords$x,coords$x1)
                                         poly_y = c(coords$y0,coords$y1,coords$y0)
                                       }else{
                                         print('please supply the correct type!')
                                       }
                                     }else{
                                       # draw rect
                                       if(type == "upper"){
                                         poly_x = c(coords$x0,coords$x0,coords$x1,coords$x1)
                                         poly_y = c(coords$y,coords$y1,coords$y1,coords$y)
                                       }else if(type == "bottom"){
                                         poly_x = c(coords$x0,coords$x0,coords$x1,coords$x1)
                                         poly_y = c(coords$y0,coords$y,coords$y,coords$y0)
                                       }else{
                                         print('please supply the correct type!')
                                       }
                                     }

                                     # Construct grob
                                     polygengrob <- grid::polygonGrob(x = poly_x,
                                                                      y = poly_y,
                                                                      gp = grid::gpar(col = ggplot2::alpha(coords$colour,coords$alpha),
                                                                                      fill = ggplot2::alpha(coords$fill,coords$alpha),
                                                                                      lwd = coords$size * .pt,
                                                                                      lty = coords$linetype))

                                   })
