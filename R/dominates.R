dominates <- function(x, y, verbose = TRUE){

  xdim <- ncol(x)
  ydim <- ncol(y)

  if(xdim != ydim)
    stop('ncol(x) not euqal to ncol(y)')

  if(ydim == 1){
    if(x < min(y)){
      if(verbose)
        return('new local optima')
      else
        return(3)
    }
    else{
      if(verbose)
        return('')
      else
        return(0)
    }
    }

  dom <- y %>% apply(1, '>', x) %>% cbind() %>% apply(2, sum)

  if(any(dom > 1)){
    if(verbose)
      return('dominates')
    else
      return(2)
  }

  if(all(dom > 0)){
    if(verbose)
      return('enhances')
    else
      return(1)
  }
  else{
    if(verbose)
      return('')
    else
      return(0)
  }
}
