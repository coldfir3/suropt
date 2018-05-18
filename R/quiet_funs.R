quiet_km <- function(..., i = 1, pb = NULL){
  model <- purrr::quietly(DiceKriging::km)(...)$result
  if(!is.null(pb))
    utils::setTxtProgressBar(pb, i)
  model
}
quiet_crit_EFI <- purrr::quietly(DiceOptim::crit_EFI)
quiet_EI <- purrr::quietly(DiceOptim::EI)
quiet_crit_EHI <- purrr::quietly(GPareto::crit_EHI)
quiet_genoud <- purrr::quietly(rgenoud::genoud)
