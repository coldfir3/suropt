#' Trainer for a surmodel object based on the SME algorithm
#'
#' @param model surmodel object to be trained
#' @param niter integer indicating number of iterations
#' @param optimizer character, only working for nsga2 by now
#'
#' @export
#' @examples
#' fn <- shaffer2
#' model <- build_surmodel(fn, 10, 1) %>% train_sme(5)
#'
#' fn <- binh
#' model <- build_surmodel(fn, 10, 2) %>% train_sme(5)
#' plot(model)
#' suropt:::plot_predict(model)
train_sme <- function(model, niter, optimizer = 'nsga2'){

  cat('Running SME algorithm on', niter, 'iterations...\n')
  pb <- utils::txtProgressBar(min = 0, max = niter, width = niter, style = 3)

  fn <- model@fn

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))
  d_out <- d_obj + d_cons

  for (i in 1:niter){

    if(optimizer == 'nsga2')
      front <- predict(model)
    else
      stop('Only the "nsga2" optimizer is implemented.')

    x_star <- front$par[which.max(get_entropy(model, front$par)),]
    res_star <- safe_fn(fn, x = x_star)

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

    utils::setTxtProgressBar(pb, i)

  }

  cat('\n')

  model
}




