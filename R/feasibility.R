get_feasibility <- function(model, newdata){

  if(is.null(nrow(newdata)))
    newdata <- matrix(newdata, nrow = 1)

  g <- .G(model@sur)
  if(length(g) > 0)
    g %>%
    purrr::map(DiceKriging::predict, newdata = as.data.frame(newdata), type = 'UK', checkNames = FALSE) %>%
      purrr::transpose() %>%
      `[`(c('mean', 'sd')) %>%
      purrr::map(function(x) do.call(cbind, x)) %>%
      with(pnorm(-mean/sd)) %>%
      apply(1, prod)
  else
    rep(1, nrow(newdata))

}
