#' Trainer for a surmodel object based on the MEGO algorithm
#'
#' @param model surmodel object to be trained
#' @param niter integer indicating number of iterations
#' @param optimizer string one of: "gen" (genetic, default),
#'  "sa" (simulated annealing) or "ps" (particle swarm)
#'
#' @export
#' @examples
#' fn <- binh
#' model <- build_surmodel(fn, 5, 2) %>% train_mego(1, 'sa')
#' plot(model)
train_mego <- function(model, niter, optimizer = 'gen'){

  cat('Running MEGO algorithm on', niter, 'iterations...\n')
  pb <- utils::txtProgressBar(min = 0, max = niter, width = niter, style = 3)

  fn <- model@fn

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))
  d_out  <- d_obj + d_cons
  d_out_sc <- 1 + d_cons

  data <- model@data

  for (i in 1:niter){

    scalar_data <- pre_process(data, 'r_asf')$data
    sur_scalarized <- purrr::pmap(list(response = cbind(.Y(scalar_data), .G(scalar_data)), i = 1:d_out_sc), quiet_km, design = .X(scalar_data))

    scalar_model <- methods::new('surmodel')
    scalar_model@data <- scalar_data
    scalar_model@sur <- sur_scalarized

    if(optimizer == 'gen'){
      opt <- quiet_genoud(EI,
                          nvars = d_in,
                          max = TRUE,
                          pop.size = 100,
                          max.generations = 20,
                          wait.generations = 3,
                          boundary.enforcement = 2,
                          Domains = cbind(rep(0, d_in), rep(1, d_in)),
                          solution.tolerance = 0.01,
                          print.level = 0,
                          model = scalar_model
      )$result
    }
    else if(optimizer == 'sa'){
      opt <- GenSA::GenSA(par = rep(0, d_in),
                          fn = (function(x) -EI(x, scalar_model)),
                          lower = rep(0, d_in),
                          upper = rep(1, d_in),
                          control = list(
                            max.call = 1e3,
                            nb.stop.improvement = 10,
                            verbose = FALSE
                          )
      )
      opt$value <- -opt$value
    }
    else if(optimizer == 'ps'){
      opt <- pso::psoptim(par = rep(0, d_in),
                          fn = EI,
                          lower = rep(0, d_in),
                          upper = rep(1, d_in),
                          control = list(
                            fnscale = -1,
                            maxf = 1e3,
                            vectorize = TRUE
                          ),
                          model = scalar_model
      )
      opt$value <- -opt$value
    }
    else
      stop('Optimizer must be one of: "gen", "sa" or "ps"')

    x_star <- opt$par
    res_star <- safe_fn(fn, x = x_star)

    if(is.null(res_star$g))
      new_data <- data.frame(
          X = matrix(unname(x_star), nrow = 1),
          Y = matrix(res_star$y, nrow = 1),
          is.feasible = all(res_star$g < 0),
          source = 'MEGO',
          stringsAsFactors = FALSE)
    else
      new_data <- data.frame(
          X = matrix(unname(x_star), nrow = 1),
          Y = matrix(res_star$y, nrow = 1),
          G = matrix(res_star$g, nrow = 1),
          is.feasible = all(res_star$g < 0),
          source = 'MEGO',
          stringsAsFactors = FALSE)

    utils::setTxtProgressBar(pb, i)

  }

  new_sur <- purrr::pmap(list(object = model@sur, newy = cbind(.Y(new_data), .G(new_data))), quiet_update, newX = .X(new_data))

  model@data <- dplyr::bind_rows(data, new_data)
  model@sur <- new_sur %>% purrr::map('result')

  cat('\n')

  model
}

EI <- function(x, model){

  model.y <- .Y(model@sur)[[1]]
  model.g <- .G(model@sur)

  if (!is.null(model.g))
    quiet_crit_EFI(x, model.y, model.g)$result
  else
    quiet_EI(x, model.y)$result

}
