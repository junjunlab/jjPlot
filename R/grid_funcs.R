#' grid.xaxis2 function
#' @author Jun Zhang
#'
#' This function creates a x-axis in a graphical grid.
#'
#' @param at numeric vector specifying the locations of tick marks on the axis.
#' @param breaks integer specifying the number of breaks on the axis.
#' @param labels character vector providing custom labels for the tick marks.
#' @param tick.len numeric specifying the length of the tick marks.
#' @param label.space numeric specifying the space between the tick marks and labels.
#' @param side character specifying the side to place the axis ("bottom" or "top").
#' @param rot numeric specifying the rotation angle for the tick mark labels.
#' @param label.size numeric specifying the axis label text size, default 12.
#'
#' @importFrom grid grid.pretty
#' @importFrom grid grid.segments
#' @importFrom grid grid.text
#' @importFrom grid current.viewport
#' @importFrom grid unit
#' @importFrom grid gpar
#'
#' @export
grid.xaxis2 <- function(at = NULL,
                        breaks = 5,
                        labels = NULL,
                        tick.len = 0.5,
                        label.space = 0.5,
                        side = c("bottom","top"),
                        rot = 0,
                        label.size = 12){
  # labels and ticks
  if(is.null(at) || is.null(labels)){
    at <- grid.pretty(current.viewport()$xscale,n = breaks)
    labels <- as.character(at)
  }else{
    at <- at
    labels <- as.character(labels)
  }

  # axis position
  side <- match.arg(side,c("bottom","top"))
  if(side == "bottom"){
    tck.y0 = unit(0, "npc")
    tck.y1 = unit(-tick.len, "lines")
    text.y = unit(-tick.len - label.space,"lines")
  }else{
    tck.y0 = unit(1, "npc")
    tck.y1 = unit(1, "npc") + unit(tick.len, "lines")
    text.y = unit(abs(tick.len) + abs(label.space),"lines") + unit(1, "npc")
  }

  grid.segments(x0 = 0,x1 = 1,y0 = 0,y1 = 0)
  grid.segments(x0 = unit(at, "native"),
                x1 = unit(at, "native"),
                y0 = tck.y0,
                y1 = tck.y1)

  grid.text(label = labels,
            x = unit(at, "native"),
            y = text.y,
            rot = rot,
            gp = gpar(fontsize = label.size))
}


#' grid.yaxis2 function
#' @author Jun Zhang
#'
#' This function creates a y-axis in a graphical grid.
#'
#' @param at numeric vector specifying the locations of tick marks on the axis.
#' @param breaks integer specifying the number of breaks on the axis.
#' @param labels character vector providing custom labels for the tick marks.
#' @param tick.len numeric specifying the length of the tick marks.
#' @param label.space numeric specifying the space between the tick marks and labels.
#' @param side character specifying the side to place the axis ("left" or "right").
#' @param rot numeric specifying the rotation angle for the tick mark labels.
#' @param label.size numeric specifying the axis label text size, default 12.
#'
#' @importFrom grid grid.pretty
#' @importFrom grid grid.segments
#' @importFrom grid grid.text
#' @importFrom grid current.viewport
#' @importFrom grid unit
#' @importFrom grid gpar
#'
#' @export
grid.yaxis2 <- function(at = NULL,
                        breaks = 5,
                        labels = NULL,
                        tick.len = 0.5,
                        label.space = 0.25,
                        side = c("left","right"),
                        rot = 0,
                        label.size = 12){
  # labels and ticks
  if(is.null(at) || is.null(labels)){
    at <- grid.pretty(current.viewport()$yscale,n = breaks)
    labels <- as.character(at)
  }else{
    at <- at
    labels <- as.character(labels)
  }

  # axis position
  side <- match.arg(side,c("left","right"))
  if(side == "left"){
    tck.x0 = unit(0, "npc")
    tck.x1 = unit(-tick.len, "lines")
    text.x = unit(-tick.len - label.space,"lines")
    text.just = "right"
  }else{
    tck.x0 = unit(1, "npc")
    tck.x1 = unit(1, "npc") + unit(tick.len, "lines")
    text.x = unit(abs(tick.len) + abs(label.space),"lines") + unit(1, "npc")
    text.just = "left"
  }

  grid.segments(x0 = 0,x1 = 0,y0 = 0,y1 = 1)
  grid.segments(y0 = unit(at, "native"),
                y1 = unit(at, "native"),
                x0 = tck.x0,
                x1 = tck.x1)

  grid.text(label = labels,
            y = unit(at, "native"),
            x = text.x,
            rot = rot,
            just = text.just,
            gp = gpar(fontsize = label.size))
}


#' Create a Color Key Grid
#'
#' This function creates a color key grid using the grid graphics system.
#'
#' @param x Numeric vector specifying the range for the x-axis.
#' @param color Vector of color values to be used for the color key.
#' @param color.n Number of colors to generate.
#' @param ticks.side Side of the color key to display tick marks
#' ("left", "right", "top", or "bottom").
#' @param pos Position of the color key ("h" for horizontal, "v" for vertical).
#' @return NULL
#'
#' @importFrom grid grid.rect gpar
#' @importFrom grDevices colorRampPalette
#'
#' @export
grid.colorkey <- function(x = NULL,
                          color = NULL,
                          color.n = 100,
                          ticks.side = c("left","right","top","bottom"),
                          pos = c("h","v")){
  pos <- match.arg(pos,c("h","v"))
  ticks.side <- match.arg(ticks.side,c("left","right","top","bottom"))

  # check position
  if(pos == "v"){
    x_scale <- c(0,1)
    y_scale <- range(as.numeric(x))

    xpos <- 0.5
    ypos <- seq(0,1, length = color.n)

    r_width = unit(1, "npc")
    r_height = 1/(color.n - 1)
  }else{
    y_scale <- c(0,1)
    x_scale <- range(as.numeric(x))

    ypos <- 0.5
    xpos <- seq(0,1, length = color.n)

    r_width = 1/(color.n - 1)
    r_height = unit(1, "npc")
  }

  # canvas
  pushViewport(viewport(angle = 0,
                        yscale = y_scale,
                        xscale = x_scale))
  # assign colors
  if(is.null(color)){
    cols <- c("blue","white","red")
  }else{
    cols <- color
  }

  col_p <- colorRampPalette(cols)(color.n)

  # just
  if(pos == "v"){
    just <- c("bottom",rep("centre",color.n-2),"top")

    # loop to create color
    for (i in 1:color.n) {
      grid.rect(x = xpos,
                y = ypos[i],
                height = r_height,
                width = r_width,
                just = just[i],
                gp = gpar(col = col_p[i], fill = col_p[i]))
    }
  }else{
    just <- c("left",rep("centre",color.n-2),"right")

    # loop to create color
    for (i in 1:color.n) {
      grid.rect(x = xpos[i],
                y = ypos,
                height = r_height,
                width = r_width,
                just = just[i],
                gp = gpar(col = col_p[i], fill = col_p[i]))
    }
  }

  grid.rect(gp = gpar(fill = NA))

  # add axis
  if(pos == "h"){
    jjPlot::grid.xaxis2(side = ticks.side,tick.len = 0.25)
  }else{
    jjPlot::grid.yaxis2(side = ticks.side,tick.len = 0.25)
  }
  popViewport()
}
