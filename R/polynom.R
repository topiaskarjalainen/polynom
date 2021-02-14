#' Create polynom
#'
#' Create polynom
#' @param x a vector or list
#' @value Returns a language object
#' @export
polynom <- function(x) {
  UseMethod("polynom")
}

#' Make polynom out of a vector
#'
#' Makes a polynom out of vector x.
#' @param x a vector
#' @return a language object
#' @export
polynom.vector <- function(x) {
  ee <- list()
  for (i in seq_along(x)) {
    ee[[i]] <- bquote(.(x[i])*x^.(i-1))
  }
  .callFromList(ee)
}

#' Makes a call out of a list
#'
#' internal function
.callFromList <- function(.list) {
  a <- length(.list)
  if (a == 1) return(.list[[1]])
  q <- bquote(.(.list[[a]])+.(.list[[a-1]]))
  l <- .list[1:a-1]
  l[[a-1]] <- q
  .callFromList(l)
}

(ll <- polynom.vector(c(1,2,3)))


polynom.list <- function(x) {
  1:32
}


expression()
