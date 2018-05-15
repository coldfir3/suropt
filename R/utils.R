#' A description of the selection functions
#'
#' @rdname sel_funs
#' @title sel_funs: Selection Functions
#' @param x input data to be subeseted
#' @param ... aditional parameters
#' @export
.SEL <- function(x, ...){
  UseMethod(".SEL")
}

#' @rdname sel_funs
#' @export
.X <- function(x){
  UseMethod(".X")
}
#' @rdname sel_funs
#' @export
.X.data.frame <- function(x) dplyr::select(x, dplyr::contains('X'))
#' @rdname sel_funs
#' @export
.X.list <- function(x) x[grepl('X', names(x))]
#' @rdname sel_funs
#' @export
.X.default <- function(x, ...) {
  NULL
}

#' @rdname sel_funs
#' @export
.Y <- function(x){
  UseMethod(".Y")
}
#' @rdname sel_funs
#' @export
.Y.data.frame <- function(x) dplyr::select(x, dplyr::contains('Y'))
#' @rdname sel_funs
#' @export
.Y.list <- function(x) x[grepl('Y', names(x))]
#' @rdname sel_funs
#' @export
.Y.default <- function(x, ...) {
  NULL
}

#' @rdname sel_funs
#' @export
.G <- function(x){
  UseMethod(".G")
}
#' @rdname sel_funs
#' @export
.G.data.frame <- function(x) dplyr::select(x, dplyr::contains('G'))
#' @rdname sel_funs
#' @export
.G.list <- function(x) x[grepl('G', names(x))]
#' @rdname sel_funs
#' @export
.G.default <- function(x, ...) {
  NULL
}
