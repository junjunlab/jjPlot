globalVariables(c("%||%",".pt"))

# ========================================
# draw legend function

# full legend
# #' @export
# draw_key_boxFull <- function(data, params, size) {
#   grid::grobTree(
#     grid::linesGrob(c(0.4,0.6), 0.1),
#     grid::linesGrob(0.5, c(0.1, 0.25)),
#
#     grid::linesGrob(c(0.4,0.6), 0.9),
#     grid::linesGrob(0.5, c(0.75, 0.9)),
#
#     grid::rectGrob(height = 0.5, width = 0.75),
#     grid::linesGrob(c(0.125, 0.875), 0.5),
#     gp = grid::gpar(
#       col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
#       fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
#       lwd = 0.5 * .pt,
#       lty = data$linetype %||% 1
#     )
#   )
# }

# left legend
# #' @export
# draw_key_boxLeft <- function(data, params, size) {
#   grid::grobTree(
#     grid::linesGrob(c(0.4,0.5), 0.1),
#     grid::linesGrob(0.5, c(0.1, 0.25)),
#
#     grid::linesGrob(c(0.4,0.5), 0.9),
#     grid::linesGrob(0.5, c(0.75, 0.9)),
#
#     grid::rectGrob(x = 0.3125,height = 0.5, width = 0.375),
#     grid::linesGrob(c(0.125, 0.5), 0.5),
#     gp = grid::gpar(
#       col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
#       fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
#       lwd = 0.5 * .pt,
#       lty = data$linetype %||% 1
#     )
#   )
# }

# right legend
# #' @export
# draw_key_boxRight <- function(data, params, size) {
#   grid::grobTree(
#     grid::linesGrob(c(0.5,0.6), 0.1),
#     grid::linesGrob(0.5, c(0.1, 0.25)),
#
#     grid::linesGrob(c(0.5,0.6), 0.9),
#     grid::linesGrob(0.5, c(0.75, 0.9)),
#
#     grid::rectGrob(x = 0.6875,height = 0.5, width = 0.375),
#     grid::linesGrob(c(0.5, 0.875), 0.5),
#     gp = grid::gpar(
#       col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
#       fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
#       lwd = 0.5 * .pt,
#       lty = data$linetype %||% 1
#     )
#   )
# }

# split legend
# #' @export
# draw_key_boxSplit <- function(data, params, size) {
#   grid::grobTree(
#     grid::linesGrob(c(0.3,0.4), 0.1),
#     grid::linesGrob(c(0.3,0.4), 0.9),
#     grid::linesGrob(0.4, c(0.1, 0.25)),
#     grid::linesGrob(0.4, c(0.75, 0.9)),
#     grid::rectGrob(x = 0.2125,height = 0.5, width = 0.375),
#     grid::linesGrob(c(0.025, 0.4), 0.5),
#
#     grid::linesGrob(c(0.6,0.7), 0.1),
#     grid::linesGrob(c(0.6,0.7), 0.9),
#     grid::linesGrob(0.6, c(0.1, 0.25)),
#     grid::linesGrob(0.6, c(0.75, 0.9)),
#     grid::rectGrob(x = 0.7875,height = 0.5, width = 0.375),
#     grid::linesGrob(c(0.6, 0.975), 0.5),
#
#     gp = grid::gpar(
#       col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
#       fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
#       lwd = 0.5 * .pt,
#       lty = data$linetype %||% 1
#     )
#   )
# }

# ==============================================================================

#' @title geom_jjboxplot
#' @name geom_jjboxplot
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param show.legend show.legend
#' @param na.rm na.rm
#' @param inherit.aes inherit.aes
#' @param outlier.show outlier.show
#' @param outlier.colour outlier.colour
#' @param outlier.color outlier.color
#' @param outlier.fill outlier.fill
#' @param outlier.shape outlier.shape
#' @param outlier.size outlier.size
#' @param outlier.stroke outlier.stroke
#' @param outlier.alpha outlier.alpha
#' @param shift shift
#' @param split.shift split.shift
#' @param ... ...
#'
#' @export
geom_jjboxplot <- function(mapping = NULL, data = NULL, stat = "jjboxplot",
                           position = "identity", show.legend = NA,
                           na.rm = FALSE,
                           inherit.aes = TRUE,
                           outlier.show = TRUE,
                           outlier.colour = "grey50",
                           outlier.color = "grey50",
                           outlier.fill = "grey50",
                           outlier.shape = 19,
                           outlier.size = 1.5,
                           outlier.stroke = 0.5,
                           outlier.alpha = 1,
                           # type = "full",
                           shift = 0,
                           split.shift = 0,
                           ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJjboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  outlier.show = outlier.show,
                  outlier.colour = outlier.color %||% outlier.colour,
                  outlier.fill = outlier.fill,
                  outlier.shape = outlier.shape,
                  outlier.size = outlier.size,
                  outlier.stroke = outlier.stroke,
                  outlier.alpha = outlier.alpha,
                  # type = type,
                  shift = shift,
                  split.shift = split.shift,
                  ...)
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomJjboxplot <- ggplot2::ggproto("GeomJjboxplot", ggplot2::Geom,
                                  required_aes = c("x", "ymin", "lower", "middle","upper", "ymax"),
                                  default_aes = ggplot2::aes(shape = 19,
                                                             lwd = 2,
                                                             width = 0.9,
                                                             alpha = 1,
                                                             colour = 'black',
                                                             fill = 'grey80',
                                                             linetype = 1,
                                                             iqrLty = 1,
                                                             type = "full"),

                                  # plot legend
                                  # draw_key = draw_key_boxFull,
                                  draw_key = ggplot2::draw_key_boxplot,

                                  # draw_panel
                                  draw_group = function(data, panel_scales, coord,
                                                        outlier.show = TRUE,
                                                        outlier.colour = 'grey50',
                                                        outlier.fill = 'grey50',
                                                        outlier.shape = 19,
                                                        outlier.size = 1.5,
                                                        outlier.stroke = 0.5,
                                                        outlier.alpha = 1,
                                                        # type = "full",
                                                        shift = 0,
                                                        split.shift = 0) {
                                    # trnasform data
                                    coords <- coord$transform(data, panel_scales) %>%
                                      mutate(lower = scales::rescale(lower, from = panel_scales$y.range),
                                             upper = scales::rescale(upper, from = panel_scales$y.range),
                                             middle = scales::rescale(middle, from = panel_scales$y.range),
                                             width = scales::rescale(width, from = panel_scales$x.range),
                                             iqr = scales::rescale(iqr, from = panel_scales$y.range))

                                    # extarct outlier data
                                    # lapply(1:nrow(coords), function(x){
                                    #   tmp <- coords[x,]
                                    #   # find outlier points
                                    #   if(length(unlist(tmp$outlier_data)) != 0){
                                    #     outlier_y = scales::rescale(unlist(tmp$outlier_data),from = panel_scales$y.range)
                                    #     outlier_df <- data.frame(outlier_y = outlier_y,x = tmp$x)
                                    #   }else{ }
                                    # }) %>% Reduce('rbind',.) -> outlier_point_df

                                    # extarct outlier data
                                    if(length(unlist(coords$outlier_data)) != 0){
                                      outlier_y = scales::rescale(unlist(coords$outlier_data),from = panel_scales$y.range)
                                      outlier_point_df <- data.frame(outlier_y = outlier_y,x = coords$x)
                                    }else{
                                      outlier_point_df <- data.frame(outlier_y = NA,x = NA)
                                    }

                                    # outlier plot
                                    outlierGrob <- grid::pointsGrob(x = outlier_point_df$x + shift,
                                                                    y = outlier_point_df$outlier_y,
                                                                    pch = outlier.shape,
                                                                    gp = grid::gpar(col = ggplot2::alpha(outlier.colour,outlier.alpha),
                                                                                    fill = ggplot2::alpha(outlier.fill,outlier.alpha),
                                                                                    fontsize = outlier.size * .pt,
                                                                                    lwd = outlier.stroke))


                                    # ======================================
                                    # graph type
                                    aesType <- unique(coords$type)

                                    # split grobs
                                    if(aesType == "full"){
                                      rectgrob_x <- coords$x
                                      rectgrob_width = coords$width

                                      meadia_x0 = coords$x - coords$width/2
                                      meadia_x1 = coords$x + coords$width/2

                                      hl_x0 = coords$x - coords$width/8
                                      hl_x1 = coords$x + coords$width/8

                                      vl_x = coords$x
                                    }else if(aesType == "left"){
                                      rectgrob_x <- coords$x - coords$width/4
                                      rectgrob_width = coords$width/2

                                      meadia_x0 = coords$x - coords$width/2
                                      meadia_x1 = coords$x

                                      hl_x0 = coords$x - coords$width/8
                                      hl_x1 = coords$x

                                      vl_x = coords$x
                                    }else if(aesType == "right"){
                                      rectgrob_x <- coords$x + coords$width/4
                                      rectgrob_width = coords$width/2

                                      meadia_x0 = coords$x
                                      meadia_x1 = coords$x + coords$width/2

                                      hl_x0 = coords$x
                                      hl_x1 = coords$x + coords$width/8

                                      vl_x = coords$x
                                    }else if(aesType == "split"){
                                      rectgrob_x <- c(coords$x - coords$width/4 - split.shift/2,coords$x + coords$width/4 + split.shift/2)
                                      rectgrob_width = c(coords$width/2,coords$width/2)

                                      meadia_x0 = c(coords$x - coords$width/2 - split.shift/2,coords$x + split.shift/2)
                                      meadia_x1 = c(coords$x - split.shift/2,coords$x + coords$width/2 + split.shift/2)

                                      hl_x0 = c(coords$x - coords$width/8 - split.shift/2,coords$x + split.shift/2)
                                      hl_x1 = c(coords$x - split.shift/2,coords$x + coords$width/8 + split.shift/2)

                                      vl_x = c(coords$x - split.shift/2,coords$x + split.shift/2)
                                    }

                                    # lower segment
                                    lower <- grid::segmentsGrob(x0 = vl_x + shift,
                                                                x1 = vl_x + shift,
                                                                y0 = coords$ymin,
                                                                y1 = coords$lower,
                                                                gp = grid::gpar(lwd = coords$size,
                                                                                col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                                lty = coords$iqrLty))

                                    # upper segment
                                    upper <- grid::segmentsGrob(x0 = vl_x + shift,
                                                                x1 = vl_x + shift,
                                                                y0 = coords$upper,
                                                                y1 = coords$ymax,
                                                                gp = grid::gpar(lwd = coords$size,
                                                                                col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                                lty = coords$iqrLty))

                                    # rect plot
                                    med <- grid::rectGrob(x = rectgrob_x + shift,
                                                          y = (coords$upper + coords$lower)/2,
                                                          width = rectgrob_width,
                                                          height = coords$upper - coords$lower,
                                                          gp = grid::gpar(col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                          fill = ggplot2::alpha(coords$fill, coords$alpha),
                                                                          lwd = coords$size,
                                                                          lty = coords$linetype))

                                    # media line
                                    medialine <- grid::segmentsGrob(x0 = meadia_x0 + shift,
                                                                    x1 = meadia_x1 + shift,
                                                                    y0 = coords$middle,
                                                                    y1 = coords$middle,
                                                                    gp = grid::gpar(lwd = coords$size + 1.5,
                                                                                    col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                                    lty = coords$linetype))

                                    # lower hline
                                    lowhline <- grid::segmentsGrob(x0 = hl_x0 + shift,
                                                                   x1 = hl_x1 + shift,
                                                                   y0 = coords$ymin,
                                                                   y1 = coords$ymin,
                                                                   gp = grid::gpar(lwd = coords$size,
                                                                                   col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                                   lty = coords$linetype))

                                    # upper hline
                                    uphline <- grid::segmentsGrob(x0 = hl_x0 + shift,
                                                                  x1 = hl_x1 + shift,
                                                                  y0 = coords$ymax,
                                                                  y1 = coords$ymax,
                                                                  gp = grid::gpar(lwd = coords$size,
                                                                                  col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                                  lty = coords$linetype))

                                    # combine
                                    if(outlier.show == TRUE){
                                      grid::gTree(children = grid::gList(med, lower, upper,medialine,lowhline,uphline,outlierGrob))
                                    }else{
                                      grid::gTree(children = grid::gList(med, lower, upper,medialine,lowhline,uphline))
                                    }
                                  }
)



