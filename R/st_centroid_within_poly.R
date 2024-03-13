#' Function for finding a centroid and ensuring that it is within the polygon for irregularly shaped polygons
#' This function returns an sf point object.
#' @param poly the  sf object containing polygons that you want centroids for
#' @export
#' @examples
#' nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc_centroid = st_centroid_within_poly(nc)

st_centroid_within_poly <- function(poly) {
  ctrd <- sf::st_centroid(poly, of_largest_polygon = TRUE)
  in_poly <- diag(sf::st_within(ctrd, poly, sparse = F))
  sf::st_geometry(ctrd[!in_poly,]) <- sf::st_geometry(sf::st_point_on_surface(poly[!in_poly,]))
  ctrd
}
