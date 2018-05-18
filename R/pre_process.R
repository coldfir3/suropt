#' Pre-Processing of models
#'
#' @param data input data.frame
#' @param operations string vector with the operation names in order
#'  to be applied
#' @param control list of control parameters for the operations
#' @export
#' @examples
#' fn <- binh
#' model <- build_surmodel(fn, 20, 2)
#' data <- model@data
pre_process <- function(data, operations = NULL, control = NULL){ #allow for partial operation match

  for (operation in operations){

#    if (operation == 'range'){
#      model$y <- range2(model$y)
#    }

#    if (operation == 'rangex'){
#      model$x <- range2(model$x)
#    }

    if (operation == 'r_asf'){
      if(is.null(control$N))
        control$N <- 10
      w <- sample(1:control$N, ncol(.Y(data)))
      w <- w/sqrt(sum(w^2))
      control$w <- rbind(control$w, w)
      .Y(data) <- asf(.Y(data), w)
    }

#    if (operation == 's_asf'){
#      if (is.null(pre_pars$w))
#        w <- rbind(1, rep(0, ncol(model$y) - 1)) + 1e-3
#      else
#        w <- matrix(c(cos(0.5*pi/pre_pars$N), sin(0.5*pi/pre_pars$N),
#                      -sin(0.5*pi/pre_pars$N), cos(0.5*pi/pre_pars$N)), nrow = 2) %*% t(tail(pre_pars$w, 1))
#      pre_pars$w <- rbind(pre_pars$w, as.numeric(abs(w)))
#      model$y <- cbind(asf(model$y, tail(pre_pars$w, 1), pre_pars$ref))
#    }
  }

  return(list(data = data, control = control))
}

range2 <- function(x){
  rn <- function(x){
    ran <- base::range(x)
    (x - ran[1])/(ran[2] - ran[1])
  }
  x %>% as.data.frame() %>% purrr::map(rn) %>% as.data.frame()
}

asf <- function(y, w = NULL, ref = NULL){

  y <- range2(y)

  if (is.null(ref))
    ref <- rep(0, ncol(y))
  else if (length(w) != ncol(y))
    stop('length(w) must be equal to ncol(y)')

  w <- w/sqrt(sum(w^2))
  res <- matrix(apply(y, 1, function(f) max((f - ref)/w)), ncol = 1)

  return(res)
}
