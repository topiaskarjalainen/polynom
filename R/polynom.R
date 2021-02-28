#' Create polynom
#'
#' Create polynom
#' @param x an argument vector or list
#' @param ... write like "'3' = 2", and so on like in polynom.list
#' @return  Returns a language object
#' @export
polynom <- function(x, ...) {
  UseMethod("polynom")
}

#' Make polynom out of a vector
#'
#' Makes a polynom out of vector x.
#' @param x a vector
#' @return a language object
.polynom.vector <- function(x) {
  ee <- list()
  for (i in seq_along(x)) {
    ee[[i]] <- bquote(.(x[i])*x^.(i-1))
  }
  .callFromList(ee)
}

#' Makes a call out of a list
#'
#' internal function
#' @param .list  a list
.callFromList <- function(.list) {
  a <- length(.list)
  if (a == 1) return(.list[[1]])
  .list[[a-1]] <- bquote(.(.list[[a]])+.(.list[[a-1]]))
  .callFromList(.list[1:a-1])
}


#' Construct from list
#'
#' @param x a list where the names are the powers and values are the coeficcients
#' @param ... additional arguments
#' @export
polynom.list <- function(x, ...) {
  ee <- lapply(seq_along(x), function(e, n, i) {
    bquote(.(e[[i]]) * x^.(as.numeric(n[i])))
    },
    n=names(x),
    e=x
  )
  .callFromList(ee)
}

#' Make a polynom
#'
#' @param x an argument vector or list
#' @param ... write like "'3' = 2", and so on like in polynom.list
#' @export
polynom.default <- function(x = NULL, ...) {
  if(is.vector(x)) return(.polynom.vector(x))
  ee <- list(...)
  polynom.list(ee)
}
