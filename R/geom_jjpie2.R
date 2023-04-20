#' @title geom_jjpie2
#' @name geom_jjpie2
#' @author Junjun Lao
#'
#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
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
geom_jjpie2 <- function(mapping = NULL, data = NULL,
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
                        shift = NULL,
                        na.rm = FALSE,
                        show.legend = TRUE,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJjpie2,
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
      shift = shift,
      ...
    )
  )
}

GeomJjpie2 <- ggplot2::ggproto("GeomJjpie2", ggplot2::Geom,
                               required_aes = c("x", "y", "piefill"),

                               default_aes = aes(fill = "#339933",
                                                 colour = "black",
                                                 size = 0.1,
                                                 linetype = 1,
                                                 alpha = 1,
                                                 width = 1,
                                                 angle = 0),

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
                                                     hollow.fill = 'white',
                                                     shift = NULL) {

                                 # pie.dat returned
                                 pie.dat <-  function(theta, length = 100) {
                                   k = seq(pi / 2, pi / 2 - theta, length = 0.5 * length * abs(theta) / pi)
                                   x = c(0, cos(k) / 2, 0)
                                   y = c(0, sin(k) / 2, 0)
                                   cbind(x, y)
                                 }

                                 # fill type
                                 if(is.numeric(data$piefill)){
                                   # add percent ratio
                                   data <- data %>% dplyr::mutate(fill_val_per = abs(piefill/value_max),
                                                                  width = width*0.1)

                                   # specified pe theta
                                   if(is.null(pie.theta)){
                                     data <- data %>% dplyr::mutate(theta = fill_val_per*360)
                                   }else{
                                     data <- data %>% dplyr::mutate(theta = pie.theta)
                                   }
                                 }else{
                                   print("Please supply numeric values to fill colors!")
                                 }

                                 # draw pie grob function
                                 pieGrob <- function(theta,
                                                     fill = "grey50",col = NULL,
                                                     lty = NULL,lwd = NULL,
                                                     vp = NULL, name = NULL,...) {
                                   pie = pie.dat(pi*(theta/360)*2)
                                   # grob
                                   grid::polygonGrob(x = pie[,'x'] + 0.5,
                                                     y = pie[,'y'] + 0.5,
                                                     name = name,
                                                     vp = vp,
                                                     gp = grid::gpar(fill = fill,
                                                                     col = col,
                                                                     lty = lty,
                                                                     lwd = lwd))
                                 }

                                 # Transform the data
                                 coords <- coord$transform(data, panel_scales)

                                 coords <- coords %>% dplyr::mutate(rect.width = scales::rescale(rect.width,from = panel_scales$x.range),
                                                                    rect.height = scales::rescale(rect.height,from = panel_scales$y.range))

                                 # calculate ratio
                                 ratio <- (sum(panel_scales$y.range) - 1)/(sum(panel_scales$x.range) - 1)

                                 # # ajust pie radius
                                 # if(is.null(shift)){
                                 #   if(ratio > 1){
                                 #     pie.shift = ratio
                                 #   }else{
                                 #     pie.shift = 1/ratio
                                 #   }
                                 # }else{
                                 #   pie.shift = shift*10
                                 # }
                                 #
                                 # # pie x width and height
                                 # if(ratio > 1){
                                 #   vp_width = coords$width*pie.shift
                                 #   vp_height = coords$width
                                 #
                                 #   cirle_width = coords$width/2*pie.shift
                                 # }else if(ratio < 1){
                                 #   vp_width = coords$width
                                 #   vp_height = coords$width*pie.shift
                                 #
                                 #   cirle_width = coords$width/2*pie.shift
                                 # }else{
                                 #   vp_width = coords$width
                                 #   vp_height = coords$width
                                 #
                                 #   cirle_width = coords$width/2
                                 # }


                                 # produce grobs
                                 vp <- grid::viewport(x = coords$x,
                                                      y = coords$y,
                                                      width = grid::unit(coords$width,"snpc"),
                                                      height = grid::unit(coords$width,"snpc"),
                                                      angle = coords$angle,
                                                      just = c("center", "center"),
                                                      default.units = "native")

                                 # rect grob
                                 rect <- grid::rectGrob(x = coords$x,
                                                        y = coords$y,
                                                        width = coords$rect.width,
                                                        height = coords$rect.height,
                                                        default.units = "native",
                                                        just = "centre",
                                                        gp = grid::gpar(col = rect.col,
                                                                        fill = rect.fill,
                                                                        lwd = coords$size * .pt,
                                                                        lty = coords$linetype))

                                 # circle grob
                                 circle <- grid::circleGrob(x = coords$x,
                                                            y = coords$y,
                                                            r = coords$width/2,
                                                            gp = grid::gpar(col = circle.colour,
                                                                            fill = circle.fill,
                                                                            lwd = coords$size * .pt))

                                 # pie grob
                                 pie <- pieGrob(theta = coords$theta,
                                                vp = vp,
                                                name = coords$group,
                                                col = alpha(coords$colour,coords$alpha),
                                                fill= alpha(coords$fill,coords$alpha),
                                                lty = coords$linetype,
                                                lwd = coords$size * .pt)

                                 # center circle
                                 hollow <- grid::circleGrob(x = coords$x,
                                                            y = coords$y,
                                                            r = circle.radius/50,
                                                            gp = grid::gpar(col = circle.colour,
                                                                            fill = hollow.fill))

                                 # combine
                                 if(add.rect == FALSE){
                                   if(add.circle == TRUE){
                                     grobs = list(grid::gTree(children = grid::gList(circle,pie,hollow)))
                                   }else{
                                     grobs = list(grid::gTree(children = grid::gList(pie,hollow)))
                                   }
                                 }else{
                                   if(add.circle == TRUE){
                                     grobs = list(grid::gTree(children = grid::gList(rect,circle,pie,hollow)))
                                   }else{
                                     grobs = list(grid::gTree(children = grid::gList(rect,pie,hollow)))
                                   }
                                 }

                                 class(grobs) <- "gList"
                                 ggplot2:::ggname("geom_jjpie",grid::gTree(children = grobs))
                               },

                               draw_key = draw_key_polygon
)
