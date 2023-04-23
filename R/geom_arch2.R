#' Draw an Arch2 shape
#'
#' @author Jun Zhang
#'
#' An Arch2 geom is a curved line with arrowheads at both ends.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link[ggplot2]{aes}}.
#' @param data A dataset to use for the plot.
#' @param stat The statistical transformation to use on the data.
#' @param position The position adjustment to use for overlapping points.
#' @param arrow The type of arrowhead to use for the ends of the curve.
#' @param arrow.fill The fill color for the arrowheads.
#' @param lineend The type of line end to use for the curve.
#' @param ndivisions The number of divisions used to create the curve.
#' @param na.rm If \code{TRUE}, remove missing values from the data before plotting.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If \code{TRUE}, inherit aesthetics from parent plot object.
#' @param ... description
#'
#' @return A layer that can be added to a ggplot object using the + operator.
#'
#' @export
geom_arch2 <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       arrow = NULL,
                       arrow.fill = "black",
                       lineend = "butt",
                       ndivisions = 200,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArch2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      ndivisions = ndivisions,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomArch2 <- ggplot2::ggproto("GeomArch2", ggplot2::Geom,
                              required_aes = c("x", "xend","y","yend"),
                              default_aes = aes(colour = "black", arrow.fill = "black",
                                                linewidth = 0.5, linetype = 1, alpha = NA),
                              draw_panel = function(data, panel_params, coord,
                                                    arrow = NULL, arrow.fill = NULL,
                                                    lineend = "butt", na.rm = FALSE,
                                                    ndivisions = 200) {

                                if (!coord$is_linear()) {
                                  cli::cli_warn("{.fn geom_arch2} is not implemented for non-linear coordinates")
                                }

                                trans <- coord$transform(data, panel_params)

                                arrow.fill <- arrow.fill %||% trans$colour

                                lapply(1:nrow(trans),function(x){
                                  tmp <- trans[x,]
                                  xy_coord <- data.frame(
                                    DescTools::DrawBezier(
                                      x = c(tmp$x,0.5*(tmp$x + tmp$xend),tmp$xend),
                                      y = c(tmp$y,tmp$yend,tmp$y),
                                      plot = FALSE,nv = ndivisions))
                                  colnames(xy_coord) <- c("x1","y1")
                                  xy_coord$y1 <- scales::rescale(xy_coord$y1,
                                                                 to = c(range(xy_coord$y1)[1],
                                                                        tmp$yend))

                                  tmp1 <- do.call("rbind", replicate(ndivisions, tmp, simplify = FALSE))
                                  tmp1$x1 <- xy_coord$x1
                                  tmp1$y1 <- xy_coord$y1
                                  tmp1$id <- x

                                  rownames(tmp1) <- NULL

                                  return(tmp1)
                                }) %>% do.call("rbind",.) %>% data.frame() -> trans

                                # grobs
                                line_grob <-
                                  grid::polylineGrob(x = trans$x1,y = trans$y1,
                                                     gp = grid::gpar(
                                                       col = alpha(unique(trans$colour), unique(trans$alpha)),
                                                       fill = alpha(arrow.fill, trans$alpha),
                                                       lwd = trans$linewidth * .pt,
                                                       lty = trans$linetype,
                                                       lineend = lineend),
                                                     id = trans$id,
                                                     arrow = arrow,
                                                     default.units = "native")

                                # return
                                grobs <- grid::grobTree(line_grob)
                                grid::gTree(children = grid::gList(grobs))
                              }
)
