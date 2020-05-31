deriv1 <- function(f, x, h=0.1, eps=1e-3, doublestep=FALSE, maxiters = 1000){
  
  if (doublestep){
    f_1l <- f(x-h)
    f_1r <- f(x+h)
    f_2l <- f(x-2*h)
    f_2r <- f(x+2*h)
    df <- ((f_2l-f_2r) + 8*(f_1r-f_1l))/(12*h)
    
    f_2l_half <- f_1l
    f_2r_half <- f_1r
    half_h <- h/2
    
    for (i in 1:maxiters){
      f_1l_half <- f(x-half_h)
      f_1r_half <- f(x+half_h)
      df_half <- ((f_2l_half-f_2r_half) + 8*(f_1r_half-f_1l_half))/(12*half_h)

      if (abs(df - df_half)/15 < eps) return(df_half)
      
      f_2l_half <- f_1l_half
      f_2r_half <- f_1r_half
      half_h <- half_h/2
      df <- df_half
    }
  }
  else{
    f_1l <- f(x-h)
    f_1r <- f(x+h)
    df <- (f_1r-f_1l)/(2*h)
    half_h <- h/2
    for (i in 1:maxiters){
      f_1l_half <- f(x-half_h)
      f_1r_half <- f(x+half_h)
      df_half <- (f_1r_half-f_1l_half)/(2*half_h)

      if (abs(df - df_half)/3 < eps) return(df_half)
      
      half_h <- half_h/2
      df <- df_half
    }
  }
  if (i == maxiters){
    print("deriv1: Max iterations reached without reaching needed accuracy. Returned NA\n")
    return(NA)
  }
}

grad <- function(f, v, h=0.1, eps=1e-3, doublestep=FALSE, .ndims=NULL){
  if (!is.numeric(v)){
    cat("deriv1: No-numeric object given as v. Returned NA\n")
    return (NA)
  }
  if (FALSE %in% is.finite(v)){
    cat("deriv1: Vector must be numeric and include only finite values. Returned NA\n")
    return(NA)
  }
  n <- ifelse(test = is.null(.ndims), yes=length(v), no=.ndims)
  
  v_tmp <- v
  result <- rep(NA, n)
  
  for (i in 1:n){
    
    result[i] <- deriv1(function(x){
                          v_tmp[i]<-x;
                          res <- f(v_tmp);
                          v_tmp[i] <- v[i];
                          res}, 
                        v[i], 
                        h, 
                        eps, 
                        doublestep)
  }
  if (FALSE %in% is.finite(result)){
    cat("grad: non-finite values got. Returned NA\n")
    return(NA)
  }
  result
}