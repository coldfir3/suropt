#' Trainer for a surmodel object based on the HEGO algorithm
#'
#' @param model surmodel object to be trained
#' @param niter integer indicating number of iterations
#' @param optimizer string one of: "gen" (genetic, default),
#'  "sa" (simulated annealing) or "ps" (particle swarm)
#'
#' @export
#' @examples
#' fn <- binh
#' model <- build_surmodel(fn, 5, 2) %>% train_hego(1, 'sa')
train_hego <- function(model, niter, optimizer = 'gen'){

  cat('Running HEGO algorithm on', niter, 'iterations...\n')
  pb <- utils::txtProgressBar(min = 0, max = niter, width = niter, style = 3)

  fn <- model@fn

  d_in   <- ncol(.X(model@data))
  d_obj  <- ncol(.Y(model@data))
  d_cons <- ncol(.G(model@data))
  d_out <- d_obj + d_cons

  for (i in 1:niter){

    pf <- .Y(model@data) %>% t() %>% emoa::nondominated_points() %>% t()

    pf_range <- apply(pf, 2, range)
    pf_rp <- matrix(pf_range[2, ] + pmax(1, (pf_range[2, ] - pf_range[1, ]) * 0.2), 1, d_obj)

    if(optimizer == 'gen'){
      opt <- quiet_genoud(EHVI,
                          nvars = d_in,
                          max = TRUE,
                          pop.size = 100,
                          max.generations = 20,
                          wait.generations = 3,
                          boundary.enforcement = 2,
                          Domains = cbind(rep(0, d_in), rep(1, d_in)),
                          solution.tolerance = 0.01,
                          print.level = 0,
                          model = model,
                          paretoFront = pf,
                          refPoint = pf_rp
      )$result
    }
    else if(optimizer == 'sa'){
      opt <- GenSA::GenSA(par = rep(0, d_in),
                          fn = (function(x) -EHVI(x, model, pf, pf_rp)),
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
                          fn = EHVI,
                          lower = rep(0, d_in),
                          upper = rep(1, d_in),
                          control = list(
                            fnscale = -1,
                            maxf = 1e3,
                            vectorize = TRUE
                            ),
                          model = model,
                          paretoFront = pf,
                          refPoint = pf_rp
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
        source = 'HEGO',
        stringsAsFactors = FALSE)
    else
      new_data <- data.frame(
        X = matrix(unname(x_star), nrow = 1),
        Y = matrix(res_star$y, nrow = 1),
        G = matrix(res_star$g, nrow = 1),
        is.feasible = all(res_star$g < 0),
        source = 'HEGO',
        stringsAsFactors = FALSE)

    new_sur <- purrr::pmap(list(object = model@sur, newy = cbind(.Y(new_data), .G(new_data))), quiet_update, newX = .X(new_data))

    model@data <- dplyr::bind_rows(model@data, new_data)
    model@sur <- new_sur %>% purrr::map('result')

    utils::setTxtProgressBar(pb, i)

  }

  cat('\n')

  model
}

EHVI <- function(x, model, paretoFront = NULL, refPoint = NULL){

  feasibility <- get_feasibility(model, x)
  ehvi <- quiet_crit_EHI(x, .Y(model@sur), paretoFront, list(refPoint = refPoint))$result

  ehvi * feasibility
}


