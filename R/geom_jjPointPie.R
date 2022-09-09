#' @title geom_jjPointPie
#' @name geom_jjPointPie
#' @author Junjun Lao
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param ... ...
#' @param explode.dist explode.dist
#' @param add.circle add.circle
#' @param circle.rev circle.rev
#' @param circle.colour circle.colour
#' @param circle.fill circle.fill
#' @param circle.radius circle.radius
#' @param add.text add.text
#' @param text.all text.all
#' @param text.dist text.dist
#' @param text.size text.size
#' @param ratio.digit ratio.digit
#' @param text.hjust text.hjust
#' @param text.vjust text.vjust
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param seq.length seq.length
#'
#' @import dplyr
#'
#' @export
geom_jjPointPie <- function(mapping = NULL, data = NULL,
                            stat = "jjPointPie",
                            position = "identity",
                            ...,
                            explode.dist = 0.1,
                            add.circle = FALSE,
                            circle.rev = FALSE,
                            circle.colour = 'black',
                            circle.fill = 'white',
                            circle.radius = 0.05,
                            add.text = FALSE,
                            text.all = TRUE,
                            text.dist = 0.1,
                            text.size = 5,
                            ratio.digit = 2,
                            text.hjust = 0.5,
                            text.vjust = 0.5,
                            seq.length = 500,
                            na.rm = FALSE,
                            show.legend = TRUE,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJjPointPie,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      explode.dist = explode.dist,
      add.circle = add.circle,
      circle.rev = circle.rev,
      circle.colour = circle.colour,
      circle.fill = circle.fill,
      circle.radius = circle.radius,
      add.text = add.text,
      text.all = text.all,
      text.dist = text.dist,
      text.size = text.size,
      ratio.digit = ratio.digit,
      text.hjust = text.hjust,
      text.vjust = text.vjust,
      seq.length = seq.length,
      ...
    )
  )
}

#' @rdname jjPlot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomJjPointPie <- ggplot2::ggproto("GeomJjPointPie", ggplot2::Geom,
                                   required_aes = c("x", "y", "pievar"),

                                   default_aes = aes(fill = NA, # "#339933"
                                                     filltype = NULL,
                                                     explode = NULL,
                                                     colour = "black",
                                                     size = 0.1,
                                                     line.size = 0.1,
                                                     linetype = 1,
                                                     alpha = 1,
                                                     width = 1,
                                                     angle = 0,
                                                     text.color = 'black',
                                                     text.rot = 0),

                                   draw_group = function(data, panel_scales, coord,
                                                         explode.dist = 0.1,
                                                         add.circle = FALSE,
                                                         circle.rev = FALSE,
                                                         circle.colour = 'black',
                                                         circle.fill = 'white',
                                                         circle.radius = 0.05,
                                                         add.text = FALSE,
                                                         text.all = TRUE,
                                                         text.dist = 0.1,
                                                         text.size = 5,
                                                         ratio.digit = 2,
                                                         text.hjust = 0.5,
                                                         text.vjust = 0.5,
                                                         seq.length = 500) {

                                     # pie.dat function
                                     pie.dat <-  function(theta, start, length = seq.length) {
                                       k = seq(pi / 2 - start, pi / 2 - theta - start, length = 0.5 * length * abs(theta) / pi)
                                       x = c(0, cos(k) / 2, 0)
                                       y = c(0, sin(k) / 2, 0)
                                       cbind(x, y)
                                     }

                                     # pie.dat.explode function
                                     pie.dat.explode <- function(theta, start, length = seq.length) {
                                       k = seq(pi / 2 - start, pi / 2 - theta - start, length = 0.25 * length * abs(theta) / pi)
                                       shift.y = explode.dist*cos((theta + start*2)*0.5)
                                       shift.x = explode.dist*sin((theta + start*2)*0.5)

                                       x = c(shift.x, cos(k) / 2 + shift.x, shift.x)
                                       y = c(shift.y, sin(k) / 2 + shift.y, shift.y)
                                       cbind(x, y)
                                     }

                                     # fill type
                                     sum.val = sum(abs(data$pievar))

                                     # add percent ratio
                                     data <- data %>% dplyr::mutate(per = abs(pievar)/sum.val) %>%
                                       # add theta
                                       dplyr::mutate(theta = per*360) %>%
                                       dplyr::mutate(width = width/10)

                                     # cusum theta
                                     data$theta.cum <- c(0,cumsum(data$theta)[1:(nrow(data) - 1)])

                                     # draw pie grob function
                                     pieGrob <- function(theta,
                                                         start,
                                                         fill = "grey50",col = NULL,
                                                         lty = NULL,lwd = NULL,
                                                         vp = NULL, name = NULL,...) {
                                       # pie coordinates
                                       pie = pie.dat(theta = pi*(theta/360)*2,
                                                     start = pi*(start/360)*2)
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

                                     # draw pieGrob.explode grob function
                                     pieGrob.explode <- function(theta,
                                                                 start,
                                                                 fill = "grey50",col = NULL,
                                                                 lty = NULL,lwd = NULL,
                                                                 vp = NULL, name = NULL,...) {
                                       # pie coordinates
                                       pie = pie.dat.explode(theta = pi*(theta/360)*2,
                                                             start = pi*(start/360)*2)
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

                                     # pie grob
                                     pie.grobs <- lapply(1:nrow(coords), function(x){
                                       tmp = coords[x,]

                                       vp <- grid::viewport(x = tmp$x,
                                                            y = tmp$y,
                                                            width = grid::unit(tmp$width,"snpc"),
                                                            height = grid::unit(tmp$width,"snpc"),
                                                            angle = tmp$angle,
                                                            just = c("center", "center"),
                                                            default.units = "native")

                                       # colnames
                                       col.name <- colnames(tmp)

                                       # seprate part of pie
                                       if("explode" %in% col.name && tmp$explode == tmp$filltype){
                                         # pie grob
                                         pie.grob <- pieGrob.explode(theta = tmp$theta,
                                                                     start = tmp$theta.cum,
                                                                     vp = vp,
                                                                     name = tmp$theta,
                                                                     col = alpha(tmp$colour,tmp$alpha),
                                                                     fill= alpha(tmp$fill,tmp$alpha),
                                                                     lty = tmp$linetype,
                                                                     lwd = tmp$line.size * .pt)

                                       }else{
                                         # pie grob
                                         pie.grob <- pieGrob(theta = tmp$theta,
                                                             start = tmp$theta.cum,
                                                             vp = vp,
                                                             name = tmp$theta,
                                                             col = alpha(tmp$colour,tmp$alpha),
                                                             fill= alpha(tmp$fill,tmp$alpha),
                                                             lty = tmp$linetype,
                                                             lwd = tmp$line.size * .pt)
                                       }

                                       # whether add percent ratio
                                       if(add.text == TRUE){
                                         # whether supply text.hjust/text.vjust
                                         if(is.null(text.hjust) & is.null(text.vjust)){
                                           hjust = ifelse((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180)) > pi,0,1)
                                           vjust = ifelse((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180)) < pi/2 | (pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180)) > 1.5*pi,1,0)
                                         }else{
                                           if(is.null(text.vjust)){
                                             hjust = text.hjust
                                             vjust = ifelse((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180)) < pi/2 | (pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180)) > 1.5*pi,1,0)
                                           }else if(is.null(text.hjust)){
                                             vjust = text.vjust
                                             hjust = ifelse((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180)) > pi,0,1)
                                           }else{
                                             hjust = text.hjust
                                             vjust = text.vjust
                                           }
                                         }

                                         # add part of percent label
                                         if("explode" %in% col.name && tmp$explode == tmp$filltype){
                                           # textGrob
                                           text.grob <- grid::textGrob(label = paste(round(tmp$per*100,digits = ratio.digit),"%",sep = ' '),
                                                                       x = text.dist*sin((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180))) + 0.5,
                                                                       y = text.dist*cos((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180))) + 0.5,
                                                                       hjust = hjust,
                                                                       vjust = vjust,
                                                                       check.overlap = TRUE,
                                                                       rot = tmp$text.rot,
                                                                       # name = tmp$theta,
                                                                       vp = vp,
                                                                       gp = grid::gpar(col = tmp$text.color,
                                                                                       fontsize = text.size*.pt))
                                         }else{
                                           if(text.all == TRUE){
                                             # textGrob
                                             text.grob <- grid::textGrob(label = paste(round(tmp$per*100,digits = ratio.digit),"%",sep = ' '),
                                                                         x = text.dist*sin((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180))) + 0.5,
                                                                         y = text.dist*cos((pi*(tmp$theta/180)/2 + pi*(tmp$theta.cum/180))) + 0.5,
                                                                         hjust = hjust,
                                                                         vjust = vjust,
                                                                         check.overlap = TRUE,
                                                                         rot = tmp$text.rot,
                                                                         # name = tmp$theta,
                                                                         vp = vp,
                                                                         gp = grid::gpar(col = tmp$text.color,
                                                                                         fontsize = text.size*.pt))
                                           }else{
                                             text.grob <- ggplot2::zeroGrob()
                                           }
                                         }
                                         return(grid::grobTree(childrenvp = vp,pie.grob,text.grob))
                                       }else{
                                         return(grid::grobTree(pie.grob))
                                       }
                                     })

                                     # add attr
                                     class(pie.grobs) <- "gList"

                                     # whether add circle to pie
                                     if(add.circle == TRUE){
                                       # circle grob
                                       circle <- grid::circleGrob(x = unique(coords$x),
                                                                  y = unique(coords$y),
                                                                  r = circle.radius,
                                                                  gp = grid::gpar(col = circle.colour,
                                                                                  fill = circle.fill,
                                                                                  lty = unique(coords$linetype),
                                                                                  lwd = unique(coords$line.size) * .pt))

                                       # whether reverse circle
                                       if(circle.rev == TRUE){
                                         grid::gTree(children = grid::gList(pie.grobs,circle))
                                       }else{
                                         grid::gTree(children = grid::gList(circle,pie.grobs))
                                       }

                                     }else{
                                       grid::gTree(children = grid::gList(pie.grobs))
                                     }
                                   },
                                   draw_key = draw_key_polygon
)
