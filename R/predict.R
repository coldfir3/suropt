predict_surmodel <- function(object, newdata = NULL){

  model <- object

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))

  if(is.null(newdata))
    res <- predict_front(model)
  else
    stop('functionality not yet implemented')


}

#' Predictor surogate model
#'
#' This functions performs predictions for an surogate model.
#'
#' @param object An object of class surmodel
#' @param newdata An opttional vector, matrix or data.frame containing the
#'   points where to perfom predictions. If not provided the predicted optima
#'   will be outputed.
#'
#' @aliases predict
#' @export
setMethod("predict", c("surmodel"), predict_surmodel)

predict_front <- function(model, control = NULL){

  d_in <- ncol(.X(model@data))
  d_obj <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))

  lower <- rep(0, d_in)
  upper <- rep(1, d_in)

  if (is.null(control$popsize))
    control$popsize <- 200
  if (is.null(control$generations))
    control$generations <- 30
  if (is.null(control$cprob))
    control$cprob <- 1
  if (is.null(control$cdist))
    control$cdist <- 1/d_in
  if (is.null(control$mprob))
    control$mprob <- 15
  if (is.null(control$mdist))
    control$mdist <- 20

  model.y <- .Y(model@sur)
  model.g <- .G(model@sur)

  fn <- function(x){
    purrr::map(model.y, DiceKriging::predict, newdata = as.data.frame(x), type = 'UK', checkNames = FALSE) %>%
      purrr::map('mean') %>%
      as.data.frame() %>% t()

  }
  if(d_cons){
    cfn <- function(x){
      purrr::map(model.g, DiceKriging::predict, newdata = as.data.frame(x), type = 'UK', checkNames = FALSE) %>%
        purrr::map('mean') %>%
        as.data.frame() %>%
        t() %>%
        `*`(-1)
    }
  }
  else{
    cfn <- NULL
  }
  mco::nsga2(fn, d_in, d_obj,
                    constraints = cfn,
                    cdim = d_cons,
                    lower.bounds = lower,
                    upper.bounds = upper,
                    popsize = control$popsize,
                    generations = control$generations,
                    cprob = control$cprob,
                    cdist = control$cdist,
                    mprob = control$mprob,
                    mdist = control$mdist,
                    vectorized = TRUE
  )#[c('par', 'value')]

}
