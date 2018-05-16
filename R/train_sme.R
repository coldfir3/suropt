#' Trainer for a surmodel object based on the SME algorithm
#'
#' @param model surmodel object to be trained
#' @param niter integer indicating number of iterations
#'
#' @export
#' @examples
#' fn <- shaffer2
#' model <- build_surmodel(fn, 20, 1) %>% train_sme(5)
#'
#' fn <- binh
#' model <- build_surmodel(fn, 20, 2) %>% train_sme(5)
train_sme <- function(model, niter){

  cat('Running SME algorithm\n\n')

  fn <- model@fn

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))
  d_out <- d_obj + d_cons

  for (i in 1:niter){

    cat('\n## iteration ', i, ' of ', niter, '\n', sep = '')

    front <- predict(model)

    cat('\nfinding infill design ...\n')
    sd <- .Y(model@sur) %>%
      purrr::map(DiceKriging::predict, newdata = as.data.frame(front$par), type = 'UK', checkNames = FALSE) %>%
      purrr::map('sd') %>%
      as.data.frame() %>%
      apply(1, prod)
    entropy <- d_obj * K_entropy + log(sd^2)/2
    x_star <- front$par[which.max(entropy),]
    cat('  found:', round(x_star,3), '\n')

    cat('calculating responses at infill point ...')
    res_star <- safe_fn(fn, x = x_star)
    cat(' got values of \n  objectives:', res_star$y)
    cat(' (', dominates(rbind(res_star$y), .Y(model@data)), ')\n', sep ='')
    cat('  constraints:', res_star$g)
    cat(' (', ifelse(is.null(res_star$g), 'feasible', ifelse(all(res_star$g < 0), 'feasible', 'un-feasible')), ')\n', sep ='')


    if(!is.null(res_star$g))
      new_data <- data.frame(
        X = matrix(unname(x_star), nrow = 1),
        Y = matrix(res_star$y, nrow = 1),
        G = matrix(res_star$g, nrow = 1),
        is.feasible = all(res_star$g < 0),
        source = 'SME',
        stringsAsFactors = FALSE) %>%
        dplyr::bind_rows(model@data, .)
    else
      new_data <- data.frame(
        X = matrix(unname(x_star), nrow = 1),
        Y = matrix(res_star$y, nrow = 1),
        is.feasible = all(res_star$g < 0),
        source = 'SME',
        stringsAsFactors = FALSE) %>%
      dplyr::bind_rows(model@data, .)

    new_sur <- purrr::pmap(list(response = cbind(.Y(new_data), .G(new_data)), i = 1:d_out), quiet_km, design = .X(new_data))

    model@data <- new_data
    model@sur <- new_sur

    cat('\n')
  }

  return(model)
}

K_entropy = log(2 * pi * exp(1))/2

