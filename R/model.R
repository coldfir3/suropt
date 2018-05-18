check.surmodel <- function(object){
  errors <- character()

  if (length(errors) == 0) TRUE else errors
}

#' The \code{surmodel} class
#'
#' @slot data bla
#' @slot sur bla
#' @slot fn bla
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
#'   format list(y = c(y1, y2, y3), g = c(g1, g2)))
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
build_surmodel <- function(fn, n_in, d_in, doe_type = 'rlhs', sur_type = 'mkm', pre_process = NULL, post_process = NULL){

  if(doe_type == 'rlhs')
    X <- lhs::randomLHS(n_in, d_in)
  else
    stop('doe_type not available, check ?build_surmodel for further help.')

  cat('Evaluating fn on DOE...\n')
  pb <- utils::txtProgressBar(min = 0, max = n_in, width = n_in, style = 3)
  YG <- list(x = split(X, 1:n_in), i = 1:n_in) %>%
    purrr::pmap(safe_fn, fn = fn, pb = pb) %>%
    purrr::transpose() %>%
    purrr::map(purrr::reduce, rbind) %>%
    purrr::map(unname)

  if(is.null(YG$g)){
    FS <- rep(TRUE, n_in)
    data <- data.frame(X = X, Y = YG$y, is.feasible = FS) %>% tibble::as.tibble() %>% dplyr::mutate(source = 'DOE')
    d_obj <- ncol(YG$y)
    d_out <- d_obj
    }
  else{
    FS <- apply(YG$g < 0, 1, all)
    data <- data.frame(X = X, Y = YG$y, G = YG$g, is.feasible = FS) %>% tibble::as.tibble() %>% dplyr::mutate(source = 'DOE')
    d_obj <- ncol(YG$y)
    d_cons <- ncol(YG$g)
    d_out <- d_obj + d_cons
  }

  cat('\nBuilding surogate models...\n')
  pb <- utils::txtProgressBar(min = 0, max = d_out, width = d_out, style = 3)
  if(sur_type == 'mkm')
    sur <- purrr::pmap(list(response = cbind(.Y(data), .G(data)), i = 1:d_out), quiet_km, design = .X(data), pb = pb)

  model <- methods::new('surmodel')
  model@data <- data
  model@sur <- sur
  model@fn <- fn

  methods::validObject(model)

  cat('\n')

  model
}

show_surmodel <- function(object){

  model <- object

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))

  cat('Surogate Model:', d_in, 'inputs,', d_obj, 'objectives,', d_cons, 'constrains')
}
#' @describeIn surmodel show method
#' @param object \code{surmodel} object
#' @export
setMethod("show", signature(object = "surmodel"), show_surmodel)

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

