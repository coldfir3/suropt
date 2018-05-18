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

  fn <- function(x){get_stats(model, x, 'y')$mean %>% t()}

  if(d_cons){
    cfn <- function(x){ -get_stats(model, x, 'g')$mean %>% t()}
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
  )
}


plot_predict <- function(model){

  pred <- predict_front(model)

  x <- pred$par[pred$pareto.optimal,]
  y <- pred$value[pred$pareto.optimal,]
  p <- get_feasibility(model, x)

  if(ncol(x) == 2 & ncol(y) == 2){
    data <- rbind(
      cbind(type = 'design',
            unname(as.data.frame(x)),
            feasibility = p,
            source = 'predicted'),
      cbind(type = 'objective',
            unname(as.data.frame(y)),
            feasibility = p,
            source = 'predicted')
    )
    data <- rbind(
      data,
      cbind(type = 'design',
            unname(.X(model@data)),
            feasibility = model@data$is.feasible,
            source = model@data$source),
      cbind(type = 'objective',
            unname(.Y(model@data)),
            feasibility = model@data$is.feasible,
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

  p <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = 'x', y = 'y', shape = 'source', col = 'feasibility')) +
    ggplot2::geom_point(size = 2, alpha = 1) +
    ggplot2::facet_wrap(~type,  scales = "free")

  p <- p + ggplot2::scale_colour_gradient('Feasibility', low = 'red', high = 'black', limits = c(0, 1)) +
    ggplot2::scale_shape_discrete('Type') +
    ggplot2::theme_minimal()

  p

}
