#' @title geom_jjtile
#' @name geom_jjtile
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param ... ...
#' @param linejoin linejoin
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#'
#' @export
geom_jjtile <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        linejoin = "mitre",
                        # rotate = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomjjTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      # rotate = rotate,
      na.rm = na.rm,
      ...
    ))
}

#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomjjTile <- ggplot2::ggproto("GeomjjTile", ggplot2::Geom,
                      extra_params = c("na.rm"),
                      default_aes = aes(fill = "grey20",
                                        colour = NA,
                                        size = 0.1,
                                        linetype = 1,
                                        alpha = NA,
                                        width = 1,
                                        # height = 1,
                                        angle = 0),

                      required_aes = c("x", "y"),

                      draw_panel = function(data, panel_scales, coord,
                                            linejoin = "mitre") {

                        data <- data %>%
                          dplyr::mutate(xmin = x - width / 2,  xmax = x + width / 2,
                                 ymin = y - width / 2, ymax = y + width / 2)

                        # Transform the data
                        coords <- coord$transform(data,panel_scales)

                        grobs <- lapply(1:nrow(coords), function(x){
                          tmp <- coords[x,]
                          tmp$group <- x

                          # Construct grob
                          vp <- grid::viewport(x = tmp$x,
                                               y = tmp$y,
                                               width = tmp$xmax - tmp$xmin,
                                               height = tmp$ymax - tmp$ymin,
                                               angle = tmp$angle,
                                               just = c("center", "center"),
                                               default.units = "native")

                          # rectGrob
                          grid::rectGrob(vp = vp,
                                   name = tmp$group,
                                   # default.units = "native",
                                   gp = grid::gpar(
                                     col = tmp$colour,
                                     fill = alpha(tmp$fill, tmp$alpha),
                                     lwd = tmp$size * .pt,
                                     lty = tmp$linetype,
                                     linejoin = linejoin,
                                     lineend = if (identical(linejoin, "round")) "round" else "square"
                                   ))
                        })

                        class(grobs) <- "gList"
                        ggplot2:::ggname("geom_jjtile",grid::gTree(children = grobs))

                      },

                      draw_key = draw_key_polygon
)
