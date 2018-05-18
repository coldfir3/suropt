#' Selection functions
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



#' Seting functions
#'
#' @rdname set_funs
#' @title set_funs: Setting Functions
#' @param x input data to be subeseted
#' @param value value to be set to x
#' @param ... aditional parameters
#' @export
.SET <- function(x, value){
  UseMethod(".SET")
}

#' @rdname set_funs
#' @export
`.Y<-` <- function(x, value){

  if(ncol(value) == 1)
    nam <- 'Y'
  else
    nam <- paste('Y', 1:ncol(value), sep = '.')

  if(is.data.frame(x)){
    value <- value %>% unname() %>% as.data.frame() %>% `names<-`(nam)
    x <- x %>%
      dplyr::select(-dplyr::contains('Y')) %>%
      dplyr::bind_cols(value) %>%
      dplyr::select(dplyr::contains('X'), dplyr::contains('Y'), dplyr::everything())
  }
  else if(is.list(x)){
    value <- value %>% as.list %>% `names<-`(nam)
    x <- x[-grepl('Y', names(x))]
    x <- c(value, x)
  }
  x
}

#' @rdname set_funs
#' @export
`.G<-` <- function(x, value){

  if(ncol(value) == 1)
    nam <- 'G'
  else
    nam <- paste('G', 1:ncol(value), sep = '.')

  if(is.data.frame(x)){
    value <- value %>% unname() %>% as.data.frame() %>% `names<-`(nam)
    x <- x %>%
      dplyr::select(-dplyr::contains('G')) %>%
      dplyr::bind_cols(value) %>%
      dplyr::select(dplyr::contains('X'), dplyr::contains('G'), dplyr::everything())
  }
  else if(is.list(x)){
    value <- value %>% as.list %>% `names<-`(nam)
    x <- x[-grepl('G', names(x))]
    x <- c(value, x)
  }
  x
}
