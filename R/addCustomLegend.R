#' Title
#' Create a custom legend for a leaflet webmap
#' @param map the leaflet map to add the legend to
#' @param colors the colors, ideally as hex codes
#' @param labels the character labels
#' @param sizes the sizes, as a numeric vector
#' @param shapes the character shapes
#' @param borders the border colours, ideally as hex codes
#' @param opacity the opacity, ranging from 0 (transparent) to 1 (opaque)
#' @export
#' @examples
#' colors <- c("#E69F00", "#56B4E9", "#009E73")
#' labels <- c("One", "Two", "Three")
#' sizes <- c(10, 5, 7.5)
#' shapes <- c("square", "circle", "square")
#' borders <- c("#E69F00", "#56B4E9", "#009E73")
#' leaflet::leaflet() %>%
#'   addCustomLegend(colors, labels, sizes, shapes, borders)

addCustomLegend <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){

  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ",
           sizes, "px;margin-top: 4px;line-height: ",
           sizes, "px;'>", labels, "</div>")
  }

  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)

  return(leaflet::addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
}
