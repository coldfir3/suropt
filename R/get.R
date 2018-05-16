#' Get prediction dafa of models
#'
#' Bla
#'
#' @name get
#' @rdname get
#' @title get: get prediction data of models
#' @param model suropt model
#' @param newdata input data where predictions will be done
#' @param slot character indicating slot, must be 'y', 'g' or 'all' (default)
#' @param stats character indicating statistics to be taken, must be one or some of
#'  'mean', 'sd', 'lower95', 'upper95' or 'trend'
NULL

#' @rdname get
#' @export
get_feasibility <- function(model, newdata){

  sd <- 'cran_check_hack'

  if(is.null(nrow(newdata)))
    newdata <- matrix(newdata, nrow = 1)

  if(length(.G(model@sur)) > 0)
    model %>% get_stats(newdata, 'g') %>%
      with(stats::pnorm(-mean/sd)) %>%
      apply(1, prod)
  else
    rep(1, nrow(newdata))

}

K_entropy = log(2 * pi * exp(1))/2

#' @rdname get
#' @export
get_entropy <- function(model, newdata){

  if(is.null(nrow(newdata)))
    newdata <- matrix(newdata, nrow = 1)

  K_entropy + log(apply(get_stats(model, newdata, 'y')$sd, 1, prod)^2)/2
}

#' @rdname get
#' @export
get_stats <- function(model, newdata, slot = 'all', stats = c('mean','sd')){

  if(is.null(nrow(newdata)))
    newdata <- matrix(newdata, nrow = 1)

  model.y <- .Y(model@sur)
  model.g <- .G(model@sur)

  if(slot == 'all')
    x <- purrr::map(c(model.y,model.g), DiceKriging::predict, newdata = as.data.frame(newdata), type = 'UK', checkNames = FALSE)
  else if(slot == 'g')
    x <- purrr::map(model.g, DiceKriging::predict, newdata = as.data.frame(newdata), type = 'UK', checkNames = FALSE)
  else if(slot == 'y')
    x <- purrr::map(model.y, DiceKriging::predict, newdata = as.data.frame(newdata), type = 'UK', checkNames = FALSE)
  else
    stop('slot must be one of "g", "y" or "all"')

  x %>%
    purrr::transpose() %>%
    `[`(stats) %>%
    purrr::map(function(x) do.call(cbind, x))
}



