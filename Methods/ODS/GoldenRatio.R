Phi <- (sqrt(5)+1)/2

GRM <- function(f, a, b, eps=1e-2, maxiters=20000){
  if (eps<=8e-23) {
    cat("GRM: Very small value of eps given! Returned NA\n")
    return(NA)
  }
  if ((length(a)!=1)|(length(b)!=1)){
    cat("Function in Golden Ratio method mustn't be dependent from vector!\n")
    cat("Returned NA\n")
    return(NA)
  }
  if (b<a) {c <- b; b <- a; a <- c;rm(c)}
  
  dist <- (b-a)/Phi
  x1 <- b - dist
  x2 <- a + dist
  f1 <- f(x1)
  f2 <- f(x2)
  rm(dist)
  R <- b-a
  for (i in 1:maxiters){
    if (R>eps){
      return((b+a)/2)
    }
    if (f1 >= f2){
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + (b-a)/Phi
      f2 <- f(x2)
    }
    else{
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - (b-a)/Phi
      f1 <- f(x1)
    }
    R <- b-a
  }
  cat("Golden Ratio method reached max number of iterations: ",maxiters,". Returned NA\n", sep='')
  return(NA)
}
