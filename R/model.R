check.surmodel <- function(object){
  errors <- character()

  if (length(errors) == 0) TRUE else errors
}

#' The \code{surmodel} class
#'
#' @slot data a data.frame with the model data
#' @slot sur a list with the surogates
#' @slot fn a function used as high-fidelity model
#' @export
setClass('surmodel', representation(
  data = 'data.frame',
  sur = 'list',
  fn = 'function'
), validity = check.surmodel
)

## TODO, add control

#' Build an surmodel object
#'
#' @param fn high fidelity function (fun must return a list of vectors with the
#'   format list(y = c(y1, y2, y3), g = c(g1, g2))).
#'   Alternatively, it can e a data.frame with names such as X.1, X.2, Y.1, G.1, G.2, G3
#' @param n_in,d_in integer number of observations and dimension of the input variables
#' @param doe_type string that defines the doe generation scheme (list valid schemes)
#' @param sur_type string that defines the surogate type (list valid types)
#' @param pre_process string vector defining the pre processing functions
#' @param post_process string vector defining the post processing
#'
#'
#'
#' @return surogate model
#'
#' @export
#' @examples
#'
#' fn <- function(x) list(y = x^2)
#' model <- build_surmodel(fn, 20, 1)
#' plot(model)
#'
#' fn <- function(x) list(y = DiceKriging::branin(x))
#' model <- build_surmodel(fn, 20, 2)
#'
#' fn <- function(x) list(y = DiceKriging::branin(x), g = 0.2 - prod(x))
#' model <- build_surmodel(fn, 20, 2)
#'
#' fn <- shaffer2
#' model <- build_surmodel(fn, 20, 1)
#'
#' fn <- binh
#' model <- build_surmodel(fn, 20, 2)
#' plot(model)
#'
#' data <- data.frame(X.1 = runif(5), X.2 = runif(5), Y.1 = runif(5))
#' model <- build_surmodel(data)
#'
#' data <- data.frame(X.1 = runif(5), X.2 = runif(5), Y.1 = runif(5), G.1 = rnorm(5))
#' model <- build_surmodel(data)
build_surmodel <- function(fn, n_in, d_in, doe_type = 'rlhs', sur_type = 'mkm', pre_process = NULL, post_process = NULL){

  if(is.data.frame(fn)){
    fn <- fn %>% tibble::as.tibble()
    X <- .X(fn)
    Y <- .Y(fn)
    G <- .G(fn)
    fn <- function(x) rep(NA, ncol(dplyr::bind_cols(Y, G)))
    n_in <- nrow(X)
    d_in <- ncol(X)
    source = 'DATA'
  }
  else{
    if(doe_type == 'rlhs')
      X <- lhs::randomLHS(n_in, d_in) %>% tibble::as.tibble()
    else if(doe_type == 'olhs')
      X <- lhs::optimumLHS(n_in, d_in) %>% tibble::as.tibble()
    else
      stop('doe_type not available, check ?build_surmodel for further help.')
    if(ncol(X) > 1)
      names(X) <- paste0('X.', 1:ncol(X))
    else
      names(X) <- "X"

    cat('Evaluating fn on DOE...\n')
    pb <- utils::txtProgressBar(min = 0, max = n_in, width = n_in, style = 3)
    YG <- list(x = split(X, 1:n_in), i = 1:n_in) %>%
      purrr::pmap(safe_fn, fn = fn, pb = pb) %>%
      purrr::transpose() %>%
      purrr::map(purrr::reduce, rbind) %>%
      purrr::map(tibble::as.tibble)
    Y <- YG$y %>% stats::setNames(paste0('Y.', 1:ncol(.)))
    if(is.null(YG$g))
      G <- dplyr::select(X, dplyr::contains('G'))
    else
      G <- YG$g %>% stats::setNames(paste0('G.', 1:ncol(.)))
    source = 'DOE'
  }

  if(ncol(G) > 0){
    data <- dplyr::bind_cols(X, Y, G) %>% dplyr::mutate(is.feasible = apply(G < 0, 1, all), source = source)
    d_obj <- ncol(Y)
    d_cons <- ncol(G)
    d_out <- d_obj + d_cons
    }
  else{
    data <- dplyr::bind_cols(X, Y, G) %>% dplyr::mutate(is.feasible = TRUE, source = source)
    d_obj <- ncol(Y)
    d_out <- d_obj
  }

  cat('\nBuilding surogate models...\n')
  pb <- utils::txtProgressBar(min = 0, max = d_out, width = d_out, style = 3)
  if(sur_type == 'mkm')
    sur <- purrr::pmap(list(response = dplyr::bind_cols(Y, G), i = 1:d_out), quiet_km, design = X, pb = pb)

  model <- methods::new('surmodel')
  model@data <- data
  model@sur <- sur
  model@fn <- fn

  methods::validObject(model)

  cat('\n')

  model
}

# #' @describeIn surmodel show method
# #' @param object \code{surmodel} object
#' Custom method for showing model objects
#' @param object \code{surmodel} object
#' @export
show.surmodel <- function(object){

  model <- object

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))

  cat('Surogate Model:', d_in, 'inputs,', d_obj, 'objectives,', d_cons, 'constrains')
}
# #' @describeIn surmodel show method
# #' @param object \code{surmodel} object
# #' @export
# setMethod("show", signature(object = "surmodel"), show_surmodel)

#' @export
plot.surmodel <- function(x, y, ...){

  model <- x

  x <- .X(model@data)
  y <- .Y(model@data)

  if(ncol(x) == 2 & ncol(y) == 2){
    data <- rbind(
      cbind(type = 'design',
            unname(x),
            is.feasible = model@data$is.feasible,
            source = model@data$source),
      cbind(type = 'objective',
            unname(y),
            is.feasible = model@data$is.feasible,
            source = model@data$source)
    )
    names(data)[2:3] <- c('x', 'y')
  }
  else if(ncol(x) == 1 & ncol(y) == 1){
    data <- cbind(type = '', x = unname(x), y = unname(y),
                  is.feasible = model@data$is.feasible,
                  source = model@data$source)

  }
  else
    stop('plot function not implemented for this strucuture of surmodel')

  data$source <- as.factor(data$source)
  data$is.feasible <- factor(data$is.feasible, levels = c(TRUE, FALSE))

  p <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = 'x', y = 'y', shape = 'source', col = 'is.feasible')) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_wrap(~type,  scales = "free")

  p <- p + ggplot2::scale_color_manual('Is Feasible', values = c('black', 'red')) +
    ggplot2::scale_shape_discrete('Type') +
    ggplot2::theme_minimal()

  p

}

