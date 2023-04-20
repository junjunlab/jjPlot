# ========================================
# draw legend function

# full legend
# #' @export
# draw_key_violinFull <- function(data, params, size) {
#   if (is.null(data$size)) {
#     data$size <- 0.5
#   }
#
#   lwd <- min(data$size, min(size) / 4)
#
#   d <- stats::density(rep(0,20))
#   grid::grid.polygon(y = c(scales::rescale(d$x,to = c(0,1)),
#                            rev(scales::rescale(d$x,to = c(0,1)))),
#                      x = c(0.5 - scales::rescale(d$y,to = c(0,0.5)),
#                            rev(scales::rescale(d$y,to = c(0,0.5)) + 0.5)),
#                      gp = grid::gpar(
#                        col = ggplot2::alpha(data$colour %||% "black", data$alpha),
#                        fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
#                        lty = data$linetype %||% 1,
#                        lwd = lwd * .pt))
# }

# split legend
# #' @export
# draw_key_violinSplit <- function(data, params, size) {
#   if (is.null(data$size)) {
#     data$size <- 0.5
#   }
#
#   lwd <- min(data$size, min(size) / 4)
#
#   d <- stats::density(rep(0,20))
#   left <- grid::grid.polygon(y = scales::rescale(d$x,to = c(0,1)),
#                              x = 0.5 - scales::rescale(d$y,to = c(0,0.5)) - 0.08,
#                              gp = grid::gpar(
#                                col = ggplot2::alpha(data$colour %||% "black", data$alpha),
#                                fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
#                                lty = data$linetype %||% 1,
#                                lwd = lwd * .pt))
#
#   right <- grid::grid.polygon(y = rev(scales::rescale(d$x,to = c(0,1))),
#                               x = rev(scales::rescale(d$y,to = c(0,0.5)) + 0.5 + 0.08),
#                               gp = grid::gpar(
#                                 col = ggplot2::alpha(data$colour %||% "black", data$alpha),
#                                 fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
#                                 lty = data$linetype %||% 1,
#                                 lwd = lwd * .pt))
#   # combine
#   grid::gTree(children = grid::gList(left,right))
# }

# left legend
# #' @export
# draw_key_violinLeft <- function(data, params, size) {
#   if (is.null(data$size)) {
#     data$size <- 0.5
#   }
#
#   lwd <- min(data$size, min(size) / 4)
#
#   d <- stats::density(rep(0,20))
#   grid::grid.polygon(y = scales::rescale(d$x,to = c(0,1)),
#                      x = 0.5 - scales::rescale(d$y,to = c(0,0.5)),
#                      gp = grid::gpar(
#                        col = ggplot2::alpha(data$colour %||% "black", data$alpha),
#                        fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
#                        lty = data$linetype %||% 1,
#                        lwd = lwd * .pt))
# }

# right legend
# #' @export
# draw_key_violinRight <- function(data, params, size) {
#   if (is.null(data$size)) {
#     data$size <- 0.5
#   }
#
#   lwd <- min(data$size, min(size) / 4)
#
#   d <- stats::density(rep(0,20))
#   grid::grid.polygon(y = rev(scales::rescale(d$x,to = c(0,1))),
#                      x = rev(scales::rescale(d$y,to = c(0,0.5)) + 0.5),
#                      gp = grid::gpar(
#                        col = ggplot2::alpha(data$colour %||% "black", data$alpha),
#                        fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
#                        lty = data$linetype %||% 1,
#                        lwd = lwd * .pt))
# }

# ==============================================================================
#' @title geom_jjviolin
#' @name geom_jjviolin
#' @author Junjun Lao
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param show.legend show.legend
#' @param na.rm na.rm
#' @param inherit.aes inherit.aes
#' @param shift shift
#' @param ... ...
#' @export
geom_jjviolin <- function(mapping = NULL, data = NULL, stat = "jjviolin",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE,
                          inherit.aes = TRUE,
                          # type = "full",
                          shift = 0,
                          ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJjviolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  # type = type,
                  shift = shift,
                  ...)
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomJjviolin <- ggplot2::ggproto("GeomJjviolin", ggplot2::Geom,
                                 required_aes = c("x", "vio_x", "vio_y"),
                                 default_aes = aes(lwd = 0.5,
                                                   width = 0.5,
                                                   colour = 'black',
                                                   fill = 'grey80',
                                                   alpha = 1,
                                                   linetype = 1,
                                                   type = "full"),

                                 # transform data
                                 draw_group = function(data, panel_scales, coord,
                                                       # type = "full",
                                                       shift = 0) {

                                   # scale violin width
                                   data <- data %>%
                                     dplyr::mutate(vio_x = scales::rescale(vio_x,
                                                                           to = c(0,unique(data$width/4))
                                     ))

                                   n <- nrow(data)
                                   if (n == 1) return(zeroGrob())

                                   # transform data
                                   coords <- coord$transform(data, panel_scales) %>%
                                     dplyr::mutate(width = scales::rescale(width, from = panel_scales$x.range),
                                                   vio_y = scales::rescale(vio_y,from = panel_scales$y.range))

                                   # get groups
                                   first_idx <- !duplicated(coords$group)
                                   first_rows <- coords[first_idx, ]

                                   # graph type
                                   aesType <- unique(coords$type)

                                   # part to show
                                   if(aesType == 'full'){
                                     polygen_xpos = c(coords$x - coords$vio_x,rev(coords$x + coords$vio_x))
                                     polygen_ypos = c(coords$vio_y,rev(coords$vio_y))
                                   }else if(aesType == 'split'){
                                     polygen_xpos = list(coords$x - coords$vio_x,rev(coords$x + coords$vio_x))
                                     polygen_ypos = list(coords$vio_y,rev(coords$vio_y))
                                   }else if(aesType == 'left'){
                                     polygen_xpos = coords$x - coords$vio_x - shift
                                     polygen_ypos = coords$vio_y
                                   }else if(aesType == 'right'){
                                     polygen_xpos = rev(coords$x + coords$vio_x) + shift
                                     polygen_ypos = rev(coords$vio_y)
                                   }else{
                                     print('Please supply correct params!')
                                   }

                                   # polygen
                                   if(aesType == 'split'){
                                     polygen_left <- grid::polygonGrob(x = polygen_xpos[[1]] - shift,
                                                                       y = polygen_ypos[[1]],
                                                                       gp = grid::gpar(col = ggplot2::alpha(first_rows$colour %||% "black", first_rows$alpha),
                                                                                       fill = ggplot2::alpha(first_rows$fill %||% "grey30", first_rows$alpha),
                                                                                       lwd = first_rows$size,
                                                                                       lty = first_rows$linetype))

                                     polygen_right <- grid::polygonGrob(x = polygen_xpos[[2]] + shift,
                                                                        y = polygen_ypos[[2]],
                                                                        gp = grid::gpar(col = ggplot2::alpha(first_rows$colour %||% "black", first_rows$alpha),
                                                                                        fill = ggplot2::alpha(first_rows$fill %||% "grey30", first_rows$alpha),
                                                                                        lwd = first_rows$size,
                                                                                        lty = first_rows$linetype))

                                     # combine
                                     grid::gTree(children = grid::gList(polygen_left,polygen_right))
                                   }else{
                                     polygen <- grid::polygonGrob(x = polygen_xpos,
                                                                  y = polygen_ypos,
                                                                  gp = grid::gpar(col = ggplot2::alpha(first_rows$colour %||% "black", first_rows$alpha),
                                                                                  fill = ggplot2::alpha(first_rows$fill %||% "grey30", first_rows$alpha),
                                                                                  lwd = first_rows$size,
                                                                                  lty = first_rows$linetype))
                                   }
                                 },

                                 # plot legend
                                 # draw_key = draw_key_violinFull
                                 draw_key = ggplot2::draw_key_polygon
)


