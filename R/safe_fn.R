safe_fn <- function(fn, ..., i = 1, pb = NULL){
  res <- purrr::safely(fn, otherwise = NA)(...)
  if(!is.null(pb))
    utils::setTxtProgressBar(pb, i)

  if(any(is.na(res$result)))
    print(paste0('fn error on sample ', i, ', error messages: ', res$error$message))

  res$result
}
