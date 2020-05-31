sven <- function(f, x0, lmbd=0.1, maxiters=1000){
  x_cur <- x0
  f_cur <- f(x_cur)
  x_next <- x_cur+lmbd
  f_next <- f(x_next)
  if (FALSE %in% is.finite(c(f_next, f_cur))){
    cat("Got non-finite value in Sven method. Returned NA\n")
    return(NA)
  }
  if (f_next > f_cur) {
    f_x1 <- f_next
    lmbd <- -lmbd
    x_next <- x_cur+lmbd
    f_next <- f(x_next)
    rm(f_x1)
  }
  for (i in 1:maxiters){
    x_cur <- x_next
    f_cur <- f_next
    x_next <- x_cur + lmbd*(2^i)
    f_next <- f(x_next)
    if (FALSE %in% is.finite(c(f_next, f_cur))){
      cat("Got non-finite value in Sven method. Returned NA\n")
      return(NA)
    }
    if (f_next >= f_cur){
      if (lmbd>0) return(list('a'=x_cur, 'b'=x_next, 'fa'=f_cur, 'fb'=f_next))
      else return(list('a'=x_next, 'b'=x_cur, 'fa'=f_next, 'fb'=f_cur))
    }
  }
  cat("Sven method reached max number of iterations: ", maxiters,". Returned NA\n", sep='')
  return(NA)
}



