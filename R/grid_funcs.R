#' grid.xaxis2 function
#' @author Jun Zhang
#'
#' This function creates a secondary x-axis in a graphical grid.
#'
#' @param at numeric vector specifying the locations of tick marks on the axis.
#' @param breaks integer specifying the number of breaks on the axis.
#' @param labels character vector providing custom labels for the tick marks.
#' @param tick.len numeric specifying the length of the tick marks.
#' @param label.space numeric specifying the space between the tick marks and labels.
#' @param side character specifying the side to place the axis ("bottom" or "top").
#' @param rot numeric specifying the rotation angle for the tick mark labels.
#'
#' @importFrom grid grid.pretty
#' @importFrom grid grid.segments
#' @importFrom grid grid.text
#' @importFrom grid current.viewport
#' @importFrom grid unit
#'
#' @export
grid.xaxis2 <- function(at = NULL,
                        breaks = 5,
                        labels = NULL,
                        tick.len = 0.5,
                        label.space = 0.5,
                        side = c("bottom","top"),
                        rot = 0){
  # labels and ticks
  if(is.null(at) || is.null(labels)){
    at <- grid.pretty(current.viewport()$xscale,n = 5)
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

  grid.segments(x0 = unit(at, "native"),
                x1 = unit(at, "native"),
                y0 = tck.y0,
                y1 = tck.y1)

  grid.text(label = labels,
            x = unit(at, "native"),
            y = text.y,
            rot = rot)
}


#' grid.yaxis2 function
#' @author Jun Zhang
#'
#' This function creates a secondary y-axis in a graphical grid.
#'
#' @param at numeric vector specifying the locations of tick marks on the axis.
#' @param breaks integer specifying the number of breaks on the axis.
#' @param labels character vector providing custom labels for the tick marks.
#' @param tick.len numeric specifying the length of the tick marks.
#' @param label.space numeric specifying the space between the tick marks and labels.
#' @param side character specifying the side to place the axis ("left" or "right").
#' @param rot numeric specifying the rotation angle for the tick mark labels.
#'
#' @importFrom grid grid.pretty
#' @importFrom grid grid.segments
#' @importFrom grid grid.text
#' @importFrom grid current.viewport
#' @importFrom grid unit
#'
#' @export
grid.yaxis2 <- function(at = NULL,
                        breaks = 5,
                        labels = NULL,
                        tick.len = 0.5,
                        label.space = 0.25,
                        side = c("left","right"),
                        rot = 0){
  # labels and ticks
  if(is.null(at) || is.null(labels)){
    at <- grid.pretty(current.viewport()$yscale,n = 5)
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

  grid.segments(y0 = unit(at, "native"),
                y1 = unit(at, "native"),
                x0 = tck.x0,
                x1 = tck.x1)

  grid.text(label = labels,
            y = unit(at, "native"),
            x = text.x,
            rot = rot,
            just = text.just)
}
