#' Negation of `%in%`
#'
#' This function returns the negation of the `%in%` operator.
#'
#' @return A logical vector indicating if a match was not found.
#' @export
#' @name `%nin%`
#' @examples
#' 1 %nin% c(2, 3, 4)
#' "a" %nin% c("b", "c", "d")
`%nin%` <- Negate(`%in%`)
