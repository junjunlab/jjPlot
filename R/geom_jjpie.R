#' @title geom_jjpie
#' @name geom_jjpie
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param ... ...
#' @param value_max value_max
#' @param add.circle add.circle
#' @param circle.color circle.color
#' @param circle.fill circle.fill
#' @param pie.theta pie.theta
#' @param add.rect add.rect
#' @param rect.width rect.width
#' @param rect.height rect.height
#' @param rect.col rect.col
#' @param rect.fill rect.fill
#' @param circle.radius circle.radius
#' @param hollow.fill hollow.fill
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#'
#' @export
# geom
geom_jjpie <- function(mapping = NULL, data = NULL,
                       stat = "jjpie",
                       position = "identity",
                       ...,
                       value_max = 1,
                       add.circle = TRUE,
                       circle.color = 'black',
                       circle.fill = 'white',
                       pie.theta = NULL,
                       add.rect = FALSE,
                       rect.width = 1,
                       rect.height = 1,
                       rect.col = 'black',
                       rect.fill = 'white',
                       circle.radius = 0,
                       hollow.fill = 'white',
                       na.rm = FALSE,
                       show.legend = TRUE,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJjpie,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      value_max = value_max,
      add.circle = add.circle,
      circle.color = circle.color,
      circle.fill = circle.fill,
      pie.theta = pie.theta,
      add.rect = add.rect,
      rect.width = rect.width,
      rect.height = rect.height,
      rect.col = rect.col,
      rect.fill = rect.fill,
      circle.radius = circle.radius,
      hollow.fill = hollow.fill,
      ...
    )
  )
}


#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomJjpie <- ggplot2::ggproto("GeomJjpie", ggplot2::Geom,
                              required_aes = c("x", "y", "piefill"),

                              default_aes = aes(fill = "#339933",
                                                colour = "black",
                                                size = 0.1,
                                                linetype = 1,
                                                alpha = 1,
                                                width = 1),

                              draw_group = function(data, panel_scales, coord,
                                                    value_max = 1,
                                                    add.circle = TRUE,
                                                    circle.colour = 'black',
                                                    circle.fill = 'white',
                                                    pie.theta = NULL,
                                                    add.rect = FALSE,
                                                    rect.width = 1,
                                                    rect.height = 1,
                                                    rect.col = 'black',
                                                    rect.fill = 'white',
                                                    circle.radius = 0,
                                                    hollow.fill = 'white') {

                                # pie.dat returned
                                # refer to corrplot package: https://github.com/taiyun/corrplot/blob/master/R/corrplot.R
                                pie.dat = function(theta, length = 100) {
                                  k = seq(pi / 2, pi / 2 - theta, length = 0.5 * length * abs(theta) / pi)
                                  x = c(0, cos(k) / 2, 0)
                                  y = c(0, sin(k) / 2, 0)
                                  cbind(x, y)
                                }

                                # fill type
                                if(is.numeric(data$piefill)){

                                  # add percent ratio
                                  data <- data %>% dplyr::mutate(fill_val_per = abs(piefill/value_max))

                                  if(is.null(pie.theta)){
                                    data <- data %>% dplyr::mutate(theta = fill_val_per*360)
                                  }else{
                                    data <- data %>% dplyr::mutate(theta = pie.theta)
                                  }


                                  # calculate pos and add to data
                                  map_df(1:nrow(data),function(x){
                                    tmp <- data[x,]
                                    tmp <- tmp %>%
                                      dplyr::mutate(pielist = list(data.frame(pie.dat(pi*(theta/360)*2))))
                                  }) -> data

                                }else{
                                  print("Please supply numeric values to fill colors!")
                                }

                                # Transform the data
                                coords <- coord$transform(data, panel_scales)

                                coords <- coords %>% dplyr::mutate(width = scales::rescale(width,from = panel_scales$x.range),
                                                                   rect.width = scales::rescale(rect.width,from = panel_scales$x.range),
                                                                   rect.height = scales::rescale(rect.height,from = panel_scales$y.range))

                                # # rescale pie pos
                                # map_df(1:nrow(coords),function(x){
                                #   tmp <- coords[x,]
                                #
                                #   # calculate each group pie coordinates
                                #   if(tmp$theta <= 90){
                                #     tmp$pielist[[1]]["x"] <- scales::rescale(tmp$pielist[[1]][["x"]],
                                #                                              to = c(0,sin((tmp$theta/180)*pi)*tmp$width/2))
                                #     tmp$pielist[[1]]["y"] <- scales::rescale(tmp$pielist[[1]][["y"]],
                                #                                              to = c(0,tmp$width/2))
                                #   }else if(tmp$theta <= 180 & tmp$theta > 90){
                                #     tmp$pielist[[1]]["x"] <- scales::rescale(tmp$pielist[[1]][["x"]],
                                #                                              to = c(0,tmp$width/2))
                                #     tmp$pielist[[1]]["y"] <- scales::rescale(tmp$pielist[[1]][["y"]],
                                #                                              to = c(cos((tmp$theta/180)*pi)*tmp$width/2,tmp$width/2))
                                #   }else if(tmp$theta <= 270 & tmp$theta > 180){
                                #     tmp$pielist[[1]]["x"] <- scales::rescale(tmp$pielist[[1]][["x"]],
                                #                                              to = c(sin((tmp$theta/180)*pi)*tmp$width/2,tmp$width/2))
                                #     tmp$pielist[[1]]["y"] <- scales::rescale(tmp$pielist[[1]][["y"]],
                                #                                              to = c(-tmp$width/2,tmp$width/2))
                                #   }else{
                                #     # 270-360 degree
                                #     tmp$pielist[[1]]["x"] <- scales::rescale(tmp$pielist[[1]][["x"]],
                                #                                              to = c(-tmp$width/2,tmp$width/2))
                                #     tmp$pielist[[1]]["y"] <- scales::rescale(tmp$pielist[[1]][["y"]],
                                #                                              to = c(-tmp$width/2,tmp$width/2))
                                #   }
                                #
                                #   return(tmp)
                                # }) -> coords_new

                                coords_new <- coords

                                # Construct a grid grob
                                rect <- grid::rectGrob(x = coords_new$x,
                                                       y = coords_new$y,
                                                       width = coords_new$rect.width,
                                                       height = coords_new$rect.height,
                                                       default.units = "native",
                                                       just = "centre",
                                                       gp = grid::gpar(col = rect.col,
                                                                       fill = rect.fill,
                                                                       lwd = coords_new$size * .pt,
                                                                       lty = coords_new$linetype))

                                # # calculate ratio
                                # ratio <- sum(panel_scales$y.range)/sum(panel_scales$x.range)
                                #
                                # # ajust pie radius
                                # if(is.null(shift)){
                                #   if(ratio > 1){
                                #     pie.shift = 0.5 + 1/ratio
                                #   }else{
                                #     pie.shift = 0.5 + ratio
                                #   }
                                # }else{
                                #   pie.shift = shift
                                # }
                                #
                                # # ratio type
                                # if(ratio > 1){
                                #   pie_x = coords_new$x + coords_new$pielist[[1]][["x"]]*ratio
                                #   pie_y = coords_new$y + coords_new$pielist[[1]][["y"]]*pie.shift
                                #
                                #   ratio <- ratio
                                # }else if(ratio < 1){
                                #   pie_x = coords_new$x + coords_new$pielist[[1]][["x"]]*pie.shift
                                #   pie_y = coords_new$y + coords_new$pielist[[1]][["y"]]*(1/ratio)
                                #
                                #   ratio <- 1/ratio
                                # }else{
                                #   pie_x = coords_new$x + coords_new$pielist[[1]][["x"]]
                                #   pie_y = coords_new$y + coords_new$pielist[[1]][["y"]]
                                #
                                #   ratio <- 1/ratio
                                # }

                                # pie viewport
                                vp <- grid::viewport(x = coords_new$x,
                                                     y = coords_new$y,
                                                     width = grid::unit(coords_new$width,"snpc"),
                                                     height = grid::unit(coords_new$width,"snpc"),
                                                     # angle = coords_new$angle,
                                                     just = c("center", "center"),
                                                     default.units = "native")

                                # circle grob
                                circle <- grid::circleGrob(x = coords_new$x,
                                                           y = coords_new$y,
                                                           r = coords_new$width/2,
                                                           gp = grid::gpar(col = circle.colour,
                                                                           fill = circle.fill,
                                                                           lwd = coords_new$size * .pt))

                                # pie grob  + coords_new$width/2
                                pie <- grid::polygonGrob(x = coords_new$pielist[[1]][["x"]] + 0.5,
                                                         y = coords_new$pielist[[1]][["y"]] + 0.5,
                                                         vp = vp,
                                                         name = coords_new$group,
                                                         gp = grid::gpar(col = ggplot2::alpha(coords_new$colour,coords_new$alpha),
                                                                         fill= ggplot2::alpha(coords_new$fill,coords_new$alpha),
                                                                         lty = coords_new$linetype,
                                                                         lwd = coords_new$size * .pt))

                                # center circle
                                hollow <- grid::circleGrob(x = coords_new$x,
                                                           y = coords_new$y,
                                                           r = circle.radius/50,
                                                           gp = grid::gpar(col = circle.colour,
                                                                           fill = hollow.fill))

                                # combine
                                if(add.rect == FALSE){
                                  if(add.circle == TRUE){
                                    grid::gTree(children = grid::gList(circle,pie,hollow))
                                  }else{
                                    grid::gTree(children = grid::gList(pie,hollow))
                                  }
                                }else{
                                  if(add.circle == TRUE){
                                    grid::gTree(children = grid::gList(rect,circle,pie,hollow))
                                  }else{
                                    grid::gTree(children = grid::gList(rect,pie,hollow))
                                  }
                                }

                              },

                              draw_key = draw_key_polygon
)

