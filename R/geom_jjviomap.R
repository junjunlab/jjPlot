#' @title geom_jjviomap
#' @name geom_jjviomap
#' @author Junjun Lao
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param show.legend show.legend
#' @param na.rm na.rm
#' @param inherit.aes inherit.aes
#' @param ... ...
#' @export
geom_jjviomap <- function(mapping = NULL, data = NULL, stat = "jjviomap",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE,
                          inherit.aes = TRUE,
                          ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJjviomap,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomJjviomap <- ggplot2::ggproto("GeomJjviomap", ggplot2::Geom,
                                 required_aes = c("x", "y", "vio_x", "vio_y"),
                                 default_aes = aes(lwd = 0.5,
                                                   width = 1,
                                                   colour = 'black',
                                                   fill = 'grey80',
                                                   alpha = 1,
                                                   linetype = 1,
                                                   angle = 0),

                                 # transform data
                                 draw_group = function(data, panel_scales, coord) {

                                   # scale violin width
                                   data <- data %>%
                                     dplyr::mutate(vio_x = scales::rescale(vio_x,to = c(0,0.5))) %>%
                                     dplyr::mutate(vio_y = scales::rescale(vio_y,to = c(0,1)))

                                   n <- nrow(data)
                                   if (n == 1) return(zeroGrob())

                                   # transform data
                                   coords <- coord$transform(data, panel_scales) %>%
                                     dplyr::mutate(x.width = scales::rescale(width, from = panel_scales$x.range)) %>%
                                     dplyr::mutate(y.width = scales::rescale(width, from = panel_scales$y.range))

                                   # get groups
                                   first_idx <- !duplicated(coords$group)
                                   first_rows <- coords[first_idx, ]

                                   # polygen vp
                                   vp <- grid::viewport(x = unique(coords$x),
                                                        y = unique(coords$y),
                                                        width = unique(coords$x.width),
                                                        height = unique(coords$y.width),
                                                        angle = unique(coords$angle),
                                                        just = c("center", "center"),
                                                        default.units = "native")

                                   # part to show
                                   polygen_xpos = c(0.5 - coords$vio_x,rev(coords$vio_x) + 0.5)
                                   polygen_ypos = c(coords$vio_y,rev(coords$vio_y))

                                   # polygen
                                   polygen <- grid::polygonGrob(x = polygen_xpos,
                                                                y = polygen_ypos,
                                                                vp = vp,
                                                                name = unique(first_rows$group),
                                                                gp = grid::gpar(col = ggplot2::alpha(first_rows$colour %||% "black", first_rows$alpha),
                                                                                fill = ggplot2::alpha(first_rows$fill %||% "grey30", first_rows$alpha),
                                                                                lwd = first_rows$size,
                                                                                lty = first_rows$linetype))

                                   grobs = list(grid::gTree(children = grid::gList(polygen)))
                                   class(grobs) <- "gList"
                                   ggplot2:::ggname("geom_jjviomap",grid::gTree(children = grobs))
                                 },

                                 # plot legend
                                 # draw_key = draw_key_violinFull
                                 draw_key = draw_key_polygon
)


###############################
#' This is a test data for this package
#' test data describtion
#'
#' @name exp.long
#' @docType data
#' @author Junjun Lao
"exp.long"
