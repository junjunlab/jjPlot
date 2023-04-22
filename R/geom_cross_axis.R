#' Cross Axis Geom
#' @author Jun Zhang
#'
#' The geom_cross_axis() function allows you to add cross-axis lines and labels
#' to a ggplot2 plot.
#'
#' @inheritParams ggplot2::layer
#' @param bk.len The length of the tick marks extending from the axis, as a
#' proportion of the range of the data. Default is 0.02.
#' @param label.size The font size of the labels for the tick marks. Default is 3.
#' @param label.shift The distance between the tick mark and its label. Default
#' is 0.015.
#' @param lineend The line ending style for the cross-axis lines. Possible values
#' are "round", "butt", or "square". Default is "butt".
#' @param linejoin The line joining style for the cross-axis lines. Possible
#' values are "round", "mitre", or "bevel". Default is "round".
#' @param arrow.fill The fill color for any arrowheads on the cross-axis lines.
#' Default is "black".
#' @param colour The color of the cross-axis lines. Default is "black".
#' @param arrow A logical value indicating whether or not arrowheads should be
#' drawn at the end of the cross-axis lines. Default is NULL.
#' @param fontface The font face for the tick mark labels. Possible values are
#' "plain", "italic", "bold", or "bold.italic". Default is "plain".
#' @param draw.minor.bk A logical value indicating whether or not to draw minor
#' tick marks. Default is FALSE.
#' @param xline.pos A vector of values representing the positions of the vertical
#' cross-axis lines.
#' @param yline.pos A vector of values representing the positions of the
#' horizontal cross-axis lines.
#' @param na.rm description
#' @param ... description
#'
#' @examples
#' library(ggplot2)
#'
#' # Create a simple plot with cross-axis lines
#' p <- ggplot(iris, aes(Sepal.Width, Petal.Length)) +
#'   geom_point()
#' p + geom_cross_axis()
#'
#' @export
geom_cross_axis <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE,
                            bk.len = 0.02,label.size = 3,label.shift = 0.015,
                            lineend = "butt",linejoin = "round",
                            arrow.fill = "black",colour = "black",arrow = NULL,
                            fontface = "plain",draw.minor.bk = FALSE,
                            xline.pos = NULL,yline.pos = NULL,
                            ...) {
  # construct layer
  ggplot2::layer(
    geom = GeomCrossAxis, mapping = mapping,
    data = data,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  bk.len = bk.len,
                  label.size = label.size,
                  label.shift = label.shift,
                  lineend = lineend,
                  linejoin = linejoin,
                  arrow.fill = arrow.fill,
                  arrow = arrow,
                  colour = colour,
                  fontface = fontface,
                  draw.minor.bk = draw.minor.bk,
                  xline.pos = xline.pos,yline.pos = yline.pos,
                  ...)
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCrossAxis <- ggplot2::ggproto("GeomCrossAxis", ggplot2::Geom,
                                  required_aes = c("x", "y"),
                                  # non_missing_aes = c("linetype", "linewidth", "shape"),
                                  default_aes = ggplot2::aes(linewidth = 0.5, linetype = 1),
                                  draw_key = draw_key_path,
                                  draw_panel = function(data, panel_scales, coord,
                                                        bk.len = 0.02,draw.minor.bk = FALSE,
                                                        arrow.fill = "black",colour = "black",
                                                        arrow = NULL,label.size = 3,label.shift = 0.015,
                                                        lineend = "butt", linejoin = "round",
                                                        fontface = "plain",
                                                        xline.pos = NULL,yline.pos = NULL) {

                                    # Transform the data first
                                    coords <- coord$transform(data, panel_scales)

                                    # process X axis data
                                    bk_x <- panel_scales$x$breaks
                                    if(is.null(yline.pos)){
                                      xline_y <- (panel_scales$y.range[1] + panel_scales$y.range[2])*0.5
                                    }else{
                                      xline_y <- yline.pos
                                    }

                                    # rescale data
                                    bk_x = scales::rescale(bk_x,from = panel_scales$x.range)
                                    xline_y <- scales::rescale(xline_y,from = panel_scales$y.range)
                                    print(xline_y)

                                    # process Y axis data
                                    bk_y <- panel_scales$y$breaks
                                    if(is.null(xline.pos)){
                                      xline_x <- (panel_scales$x.range[1] + panel_scales$x.range[2])*0.5
                                    }else{
                                      xline_x <- xline.pos
                                    }

                                    # rescale data
                                    bk_y = scales::rescale(bk_y,from = panel_scales$y.range)
                                    xline_x = scales::rescale(xline_x,from = panel_scales$x.range)

                                    # minor breaks
                                    minor_x_bk <- setdiff(panel_scales$x$minor_breaks,panel_scales$x$breaks)
                                    minor_x_bk <- scales::rescale(minor_x_bk,from = panel_scales$x.range)

                                    minor_y_bk <- setdiff(panel_scales$y$minor_breaks,panel_scales$y$breaks)
                                    minor_y_bk <- scales::rescale(minor_y_bk,from = panel_scales$y.range)

                                    # ==========================================
                                    # x axis grob
                                    x_axis <- segmentsGrob(x0 = 0,
                                                           x1 = 1,
                                                           y0 = unique(xline_y),
                                                           y1 = unique(xline_y),
                                                           gp = grid::gpar(
                                                             col = colour,
                                                             fill = arrow.fill,
                                                             lwd = coords$linewidth * .pt,
                                                             lty = coords$linetype,
                                                             lineend = lineend,
                                                             linejoin = linejoin
                                                           ),
                                                           default.units = "native",
                                                           arrow = arrow)

                                    x_axis_bk <- segmentsGrob(x0 = bk_x,
                                                              x1 = bk_x,
                                                              y0 = bk.len + unique(xline_y),
                                                              y1 = unique(xline_y),
                                                              gp = grid::gpar(col = colour,
                                                                              lwd = coords$linewidth * .pt,
                                                                              lty = coords$linetype),
                                                              default.units = "native",
                                                              arrow = NULL)

                                    x_axis_bk_minor <- segmentsGrob(x0 = minor_x_bk,
                                                                    x1 = minor_x_bk,
                                                                    y0 = bk.len*0.5 + unique(xline_y),
                                                                    y1 = unique(xline_y),
                                                                    gp = grid::gpar(col = colour,
                                                                                    lwd = coords$linewidth * .pt,
                                                                                    lty = coords$linetype),
                                                                    default.units = "native",
                                                                    arrow = NULL)

                                    x_axis_label <- textGrob(label = panel_scales$x$get_labels(),
                                                             x = bk_x,
                                                             y = bk.len + unique(xline_y) + label.shift,
                                                             gp = grid::gpar(col = colour,
                                                                             fontsize = label.size*.pt,
                                                                             fontface = fontface),
                                                             default.units = "native")
                                    # ==========================================
                                    # y axis grob
                                    y_axis <- segmentsGrob(x0 = unique(xline_x),
                                                           x1 = unique(xline_x),
                                                           y0 = 0,
                                                           y1 = 1,
                                                           gp = grid::gpar(
                                                             col = colour,
                                                             fill = arrow.fill,
                                                             lwd = coords$linewidth * .pt,
                                                             lty = coords$linetype,
                                                             lineend = lineend,
                                                             linejoin = linejoin
                                                           ),
                                                           default.units = "native",
                                                           arrow = arrow)

                                    y_axis_bk <- segmentsGrob(x0 = unique(xline_x),
                                                              x1 = bk.len + unique(xline_x),
                                                              y0 = bk_y,
                                                              y1 = bk_y,
                                                              gp = grid::gpar(col = colour,
                                                                              lwd = coords$linewidth * .pt,
                                                                              lty = coords$linetype),
                                                              default.units = "native",
                                                              arrow = NULL)

                                    y_axis_bk_minor <- segmentsGrob(x0 = unique(xline_x),
                                                                    x1 = bk.len*0.5 + unique(xline_x),
                                                                    y0 = minor_y_bk,
                                                                    y1 = minor_y_bk,
                                                                    gp = grid::gpar(col = colour,
                                                                                    lwd = coords$linewidth * .pt,
                                                                                    lty = coords$linetype),
                                                                    default.units = "native",
                                                                    arrow = NULL)

                                    y_axis_label <- textGrob(label = panel_scales$y$get_labels(),
                                                             x = bk.len + unique(xline_x) + label.shift,
                                                             y = bk_y,
                                                             gp = grid::gpar(col = colour,
                                                                             fontsize = label.size*.pt,
                                                                             fontface = fontface),
                                                             default.units = "native")

                                    # ==========================================
                                    # return
                                    if(draw.minor.bk == TRUE){

                                      grobs = list(grid::gTree(children =
                                                                 grid::gList(x_axis_bk_minor,
                                                                             x_axis,x_axis_bk,x_axis_label,
                                                                             y_axis,y_axis_bk,y_axis_label,
                                                                             y_axis_bk_minor)))
                                    }else{
                                      grobs = list(grid::gTree(children =
                                                                 grid::gList(x_axis,x_axis_bk,x_axis_label,
                                                                             y_axis,y_axis_bk,y_axis_label)))
                                    }

                                    class(grobs) <- "gList"
                                    ggplot2:::ggname("geom_cross_axis",grid::gTree(children = grobs))
                                  })
