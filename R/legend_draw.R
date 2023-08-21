#' Key glyphs for legends
#'
#' Each geom has an associated function that draws the key when
#' the geom needs to be displayed in a legend. These functions are
#' called  draw_key_*(), where * stands for the name of the
#' respective key glyph.
#' see examples below.
#'
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @name draw_key
NULL

#' @rdname draw_key
#' @export
draw_key_boxFull <- function(data, params, size) {
  grid::grobTree(
    grid::linesGrob(c(0.4,0.6), 0.1),
    grid::linesGrob(0.5, c(0.1, 0.25)),

    grid::linesGrob(c(0.4,0.6), 0.9),
    grid::linesGrob(0.5, c(0.75, 0.9)),

    grid::rectGrob(height = 0.5, width = 0.75),
    grid::linesGrob(c(0.125, 0.875), 0.5),
    gp = grid::gpar(
      col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
      fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
      lwd = 0.5 * .pt,
      lty = data$linetype %||% 1
    )
  )
}

#' @rdname draw_key
#' @export
draw_key_boxLeft <- function(data, params, size) {
  grid::grobTree(
    grid::linesGrob(c(0.4,0.5), 0.1),
    grid::linesGrob(0.5, c(0.1, 0.25)),

    grid::linesGrob(c(0.4,0.5), 0.9),
    grid::linesGrob(0.5, c(0.75, 0.9)),

    grid::rectGrob(x = 0.3125,height = 0.5, width = 0.375),
    grid::linesGrob(c(0.125, 0.5), 0.5),
    gp = grid::gpar(
      col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
      fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
      lwd = 0.5 * .pt,
      lty = data$linetype %||% 1
    )
  )
}


#' @rdname draw_key
#' @export
draw_key_boxRight <- function(data, params, size) {
  grid::grobTree(
    grid::linesGrob(c(0.5,0.6), 0.1),
    grid::linesGrob(0.5, c(0.1, 0.25)),

    grid::linesGrob(c(0.5,0.6), 0.9),
    grid::linesGrob(0.5, c(0.75, 0.9)),

    grid::rectGrob(x = 0.6875,height = 0.5, width = 0.375),
    grid::linesGrob(c(0.5, 0.875), 0.5),
    gp = grid::gpar(
      col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
      fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
      lwd = 0.5 * .pt,
      lty = data$linetype %||% 1
    )
  )
}


#' @rdname draw_key
#' @export
draw_key_boxSplit <- function(data, params, size) {
  grid::grobTree(
    grid::linesGrob(c(0.3,0.4), 0.1),
    grid::linesGrob(c(0.3,0.4), 0.9),
    grid::linesGrob(0.4, c(0.1, 0.25)),
    grid::linesGrob(0.4, c(0.75, 0.9)),
    grid::rectGrob(x = 0.2125,height = 0.5, width = 0.375),
    grid::linesGrob(c(0.025, 0.4), 0.5),

    grid::linesGrob(c(0.6,0.7), 0.1),
    grid::linesGrob(c(0.6,0.7), 0.9),
    grid::linesGrob(0.6, c(0.1, 0.25)),
    grid::linesGrob(0.6, c(0.75, 0.9)),
    grid::rectGrob(x = 0.7875,height = 0.5, width = 0.375),
    grid::linesGrob(c(0.6, 0.975), 0.5),

    gp = grid::gpar(
      col = ggplot2::alpha(data$colour %||% "grey20", data$alpha),
      fill = ggplot2::alpha(data$fill %||% "white", data$alpha),
      lwd = 0.5 * .pt,
      lty = data$linetype %||% 1
    )
  )
}


#' @rdname draw_key
#' @export
draw_key_violinFull <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }

  lwd <- min(data$size, min(size) / 4)

  d <- stats::density(rep(0,20))
  grid::grid.polygon(y = c(scales::rescale(d$x,to = c(0,1)),
                           rev(scales::rescale(d$x,to = c(0,1)))),
                     x = c(0.5 - scales::rescale(d$y,to = c(0,0.5)),
                           rev(scales::rescale(d$y,to = c(0,0.5)) + 0.5)),
                     gp = grid::gpar(
                       col = ggplot2::alpha(data$colour %||% "black", data$alpha),
                       fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
                       lty = data$linetype %||% 1,
                       lwd = lwd * .pt))
}


#' @rdname draw_key
#' @export
draw_key_violinSplit <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }

  lwd <- min(data$size, min(size) / 4)

  d <- stats::density(rep(0,20))
  left <- grid::grid.polygon(y = scales::rescale(d$x,to = c(0,1)),
                             x = 0.5 - scales::rescale(d$y,to = c(0,0.5)) - 0.08,
                             gp = grid::gpar(
                               col = ggplot2::alpha(data$colour %||% "black", data$alpha),
                               fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
                               lty = data$linetype %||% 1,
                               lwd = lwd * .pt))

  right <- grid::grid.polygon(y = rev(scales::rescale(d$x,to = c(0,1))),
                              x = rev(scales::rescale(d$y,to = c(0,0.5)) + 0.5 + 0.08),
                              gp = grid::gpar(
                                col = ggplot2::alpha(data$colour %||% "black", data$alpha),
                                fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
                                lty = data$linetype %||% 1,
                                lwd = lwd * .pt))
  # combine
  grid::gTree(children = grid::gList(left,right))
}


#' @rdname draw_key
#' @export
draw_key_violinLeft <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }

  lwd <- min(data$size, min(size) / 4)

  d <- stats::density(rep(0,20))
  grid::grid.polygon(y = scales::rescale(d$x,to = c(0,1)),
                     x = 0.5 - scales::rescale(d$y,to = c(0,0.5)),
                     gp = grid::gpar(
                       col = ggplot2::alpha(data$colour %||% "black", data$alpha),
                       fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
                       lty = data$linetype %||% 1,
                       lwd = lwd * .pt))
}


#' @rdname draw_key
#' @export
draw_key_violinRight <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }

  lwd <- min(data$size, min(size) / 4)

  d <- stats::density(rep(0,20))
  grid::grid.polygon(y = rev(scales::rescale(d$x,to = c(0,1))),
                     x = rev(scales::rescale(d$y,to = c(0,0.5)) + 0.5),
                     gp = grid::gpar(
                       col = ggplot2::alpha(data$colour %||% "black", data$alpha),
                       fill = ggplot2::alpha(data$fill %||% "grey30", data$alpha),
                       lty = data$linetype %||% 1,
                       lwd = lwd * .pt))
}

#' @rdname draw_key
#' @export
draw_number_circle <- function(data,params,size){
  grobTree(pointsGrob(x = 0.5,y = 0.5,
                      size = unit(2,"char"),
                      pch = 16,
                      gp = gpar(
                        col = alpha(data$colour %||% "grey50",data$alpha),
                        fill = alpha(data$fill %||% "grey50",data$alpha),
                        lwd = (data$linewidth %||% 0.5)* .pt,
                        lty = data$linetype %||% 1)),
           textGrob(label = data$label,
                    x = rep(0.5,3),y = rep(0.5,3),
                    gp = gpar(col = "black"))
  )

}
