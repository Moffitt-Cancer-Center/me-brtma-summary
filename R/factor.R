
#' Find min/max ordinal level in vector
#'
#' @description Returns the minimum (or maximum) ordinal level present in the vector
#'
#' @details Factors generally are numerical values representing specific strings that
#' are expected to be repeated in a vector (see iris$Species for instance). Ordinal factors
#' are factors that have an ordering imposed by the levels specified in the factor (see
#' [levels()]).
#'
#' This function determines, for a vector, what the smallest (or largest) ordinal factor
#' is. In a vector it is not required that all ordinal values are present. This function
#' will return the factor level of the smallest value (or largest value) in the vector.
#'
#' @param x A vector representing ordinal values
#' @param f A function (min, max) to use against ordinal values
#'
#' @returns A character vector representing the max/min ordinal value in `x`
#' @export
#'
#' @seealso fct_min(), fct_max()
#' @examples
#' \dontrun{
#' find_level(iris$Species)
#' # setosa
#' find_level(iris$Species, f = max)
#' # virginica
#' }
find_level <- function(x, f = min) {
  v <- na.omit(x)

  if ( length(v) == 0) return(NA)

  levels(x)[f(as.numeric(v))]

}


#' Find min ordinal level in vector
#'
#' @description Returns the minimum ordinal level present in the vector
#'
#' @inherit find_level
#' @export
fct_min <- function(x) find_level(x, f=min)

#' Find max ordinal level in vector
#'
#' @description Returns the maximum ordinal level present in the vector
#' @inherit find_level
#' @export
fct_max <- function(x) find_level(x, f=max)

