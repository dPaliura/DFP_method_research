source("Methods/ODS/Interval/Sven.R", echo=FALSE)

DSK_Powell <- function(f, x0, eps=1e-2, h=1, maxiters=5000){
  sven_res <- sven(f, x0, h)
  if (!is.list(sven_res)){
    cat('Can not find initial interval using Sven method.\n')
    return(NA)
  }

  x1 <- sven_res$a
  x3 <- sven_res$b
  x2 <- (x3+x1)/2
  dx <- x3-x2
  
  f1 <- sven_res$fa
  f2 <- f(x2)
  f3 <- sven_res$fb
  
  x_apr <- x2 + dx*(f1-f3)/(f1-2*f2+f3)/2
  f_apr <- f(x_apr)
  
  
  if(!is.finite(x_apr)){
    cat("Non-finite values generated in DSK-Powell method. Returned NA\n")
    return(NA)
  }
  if (abs(x2-x_apr) < eps){
    return(list('minimum'=x_apr, 'objective'=f_apr))
  }
  
  if (f2<f_apr){
    if (x2<x_apr) {
      x3 <- x_apr
      f3 <- f_apr
    }
    else {
      x1 <- x_apr
      f1 <- f_apr
      }
  }
  else{
    if (x2<x_apr){
      x1 <- x2
      f1 <- f2
      x2 <- x_apr
      f2 <- f_apr
    }
    else{
      x3 <- x2
      f3 <- f2
      x2 <- x_apr
      f2 <- f_apr
    }
  }
  
  for (i in 1:maxiters){
    a1 = (f2-f1)/(x2-x1)
    a2 = ((f3-f1)/(x3-x1) - (f2-f1)/(x2-x1))/(x3-x2)
    x_apr <- (x1+x2-a1/a2)/2
    
    if(!is.finite(x_apr)){
      cat("Non-finite values generated in DSK-Powell method. Returned NA\n")
      return(NA)
    }
    f_apr <- f(x_apr)
    
    if (abs(x2-x_apr) < eps){
      return(list('minimum'=x_apr, 'objective'=f_apr))
    }
    
    if (f2<f_apr){
      if (x2<x_apr) {
        if(abs(x_apr-x3) < eps){
          x1 <- x2
          f1 <- f2
          x2 <- (x3+x1)/2
          f2 <- f(x2)
        }
        else {
          x3 <- x_apr
          f3 <- f_apr
        }
      }
      else {
        if(abs(x_apr-x1) < eps){
          x3 <- x2
          f3 <- f2
          x2 <- (x3+x1)/2
          f2 <- f(x2)
        }
        else{
          x1 <- x_apr
          f1 <- f_apr
        }
      }
    }
    else{
      if (x2<x_apr){
        x1 <- x2
        f1 <- f2
        x2 <- x_apr
        f2 <- f_apr
      }
      else{
        x3 <- x2
        f3 <- f2
        x2 <- x_apr
        f2 <- f_apr
      }
    }
  }
  cat("DSK-Powell method reached max number of iterations: ", maxiters,". Returned NA\n", sep='')
  return(NA)
}