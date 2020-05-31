source("Methods/Norm/Norm.R", echo=FALSE)
source("Methods/NumDeriv/Deriv1.R", echo=FALSE)
source("Methods/ODS/DSK_Powell.R", echo=FALSE)
source("Methods/ODS/GoldenRatio.R", echo=FALSE)


# Methods for calculating derivative in DFP function.
# deriv_single_step uses formula for accuracy O(h^2)
# while method deriv_double_step uses formula for accuracy O(h^4).
# (!!!) One of this values must be used as parameter der_method of function DFP (!!!)
deriv_single_step = FALSE
deriv_double_step = TRUE

# Methods of one-dimesional optimisation to be used in DFP function. 
# onedim_GRM stands for Golden Ratio Method.
# onedim_DSKP is for DSK-Powell method (Devis-Sven-Kempi).
# (!!!) One of this values must be used as parameter onedim_method of function DFP (!!!)
onedim_GRM = FALSE
onedim_DSKP = TRUE

# One of this constants used for defining stop criteria of DFP method.
# stop_absolute is used for comparison absolute values with accuracy value eps
# while stop_gradient is used for comparison gradient norm with accuracy value eps
# (!!!) One of this values must be used as parameter stop_criteria of function DFP (!!!)
stop_absolute = FALSE
stop_gradient = TRUE

# One of this constants used for defining if DFP method uses restarts (dicarding 
# matrix D to unar).
# (!!!) One of this values must be used as parameter with_restart of function DFP (!!!)
restart = TRUE
norestart = FALSE


# DFP is functional realisation of Davidon-Fletcher-Powell method
# parameters of function:
# f - target function, it must be dependent of single parameter x, wich must be numeric vector
#     also f must return single number. Mathematically f:R^n -> R, where R is set of real numbers
# x0 - initial point of algorithm, must be numerical vector of finite numbers
# eps - accuracy of DFP method, mustn't be less than 1e-20
# der_h - step used in calculating of derivatives, mustn't be less than 1e-20
# der_method - method on calculating derivatives, must be single logical value
# onedim_method - method for one-dimesional optimisation to be used in DFP algorithm,
#     must be single logical value
# onedim_eps - accuracy of calculating within chosen one-dimensional method,
#     mustn't be less than 1e-20
# sven_lmbd - parameter lambda (initial step) of Sven algorithm for evaluating uncertainty interval,
#     mustn't be less than 1e-20
# stop_criteria - criteria of DFP algorithm stop, must be single logical value
# DFP function returns (if valid parameters and no critical situation ocurred) named list of 
#     "minimum" - founded point where target fuction reaches it's minimum with specified accuracy
#     "objective" - value of function in this point
DFP <- function(f, x0, 
                eps, 
                der_h, der_method, 
                onedim_method, onedim_eps, sven_alpha, 
                stop_criteria, 
                with_restart = TRUE,
                trace = FALSE,
                maxiters=30000){
{
  if (!is.function(f)){
    cat("Parameter f must be function! Returned NA\n")
    return(NA)
  }
  
  if (!is.numeric(x0)){
    cat("vector x0 must be numeric vector! Returned NA\n")
    return(NA)
  }
  if (FALSE %in% is.finite(x0)){
    cat("vector x0 must be vector of finite numbers! Returned NA\n")
    return(NA)
  }
  
  f_next <- f(x0)
  if (!is.numeric(f_next)){
    cat("Function f must return numeric values! DFP returned NA\n")
    return(NA)
  }
  if (FALSE %in% is.finite(f_next)){
    cat("Function f must return finite values (excepting it's problem points)! DFP returned NA\n")
    return(NA)
  }
  if (length(f_next) != 1){
    cat("Function f mustn't return vectors, only single numbers! DFP returned NA\n")
    return(NA)
  }
  
  secondary_params <- c(eps, der_h, onedim_eps, sven_alpha)
  if (!is.numeric(secondary_params)){
    cat("der_h, onedim_eps, sven_lmbd must be numeric values! Returned NA\n")
    return(NA)
  }
  if (FALSE %in% is.finite(secondary_params)){
    cat("der_h, onedim_eps, sven_lmbd must be finite values! Returned NA\n")
    return(NA)
  }
  if (TRUE %in% (secondary_params<1e-20)){
    cat("der_h, onedim_eps, sven_lmbd mustn't be less than 1e-20! Returned NA\n")
    return(NA)
  }
  rm(secondary_params)
  
  if (!is.logical(der_method)){
    cat("Parameter der_method must be logical!\n")
    cat("Use constant deriv_single_step or deriv_double_step!\n")
    cat("Returned NA\n")
    return(NA)
  }
  if (!is.logical(onedim_method)){
    cat("Parameter onedim_method must be logical!\n")
    cat("Use constant onedim_GRM or onedim_DSKP!\n")
    cat("Returned NA\n")
    return(NA)
  }
  if (!is.logical(stop_criteria)){
    cat("Parameter stop_criteria must be logical!\n")
    cat("Use constant stop_absolute or stop_gradient!\n")
    cat("Returned NA\n")
    return(NA)
  }
  
  n <- length(x0)
  der_eps <- eps/(2*sqrt(n))
  
  D0 <- matrix(0, nrow=n, ncol=n)
  diag(D0) <- 1
  D <- D0
  x_prev <- x0
  x_next <- x0 + 2*eps
  
  last_count <- 0
}
  trc <- NULL
  if (trace) trc <- rbind(trc, x0)
  gradf_prev <- grad(f, x_prev, der_h, der_eps, der_method, .ndims=n)
  if (TRUE %in% is.na(gradf_prev)){
    cat("Got NA as result of grad function in DFP. Returned NA\n")
    return(NA)
  }
  
  if (stop_criteria){
    if (vnorm(gradf_prev) < (eps-der_eps)){
      cat("Probably x0 given as searched minimum.\n")
      cat("Result returned without main part of DFP algorithm\n")
      return(list('minimum'=x_prev, 'objective'=f(x_prev), 'trace' = trc))
    }
  }
  
  j<-1
  for (i in 1:maxiters){
######## STEP 1
    d <- -D%*%gradf_prev
    
    g <- function(lm) f(as.numeric(x_prev+lm*d))
    x_norm <- vnorm(x_prev)
    step <- sven_alpha
    
    if (onedim_method){
      dsk <- DSK_Powell(g, 0, onedim_eps, step)
      if(!is.list(dsk)){
        cat("DSK-Powell method broken. DFP returned NA\n")
        return(NA)
      }
      l <- dsk$minimum
    }
    else{
      
      svn <- sven(g, 0, step)
      if(!is.list(svn)){
        cat("Sven method broken. DFP returned NA\n")
        return(NA)
      }
      
      l <- GRM(g, svn$a, svn$b, onedim_eps)
      if (is.na(l)){
        cat("Golden Ratio method broken. DFP returned NA\n")
        return(NA)
      }
    }
    
    x_next <- x_prev + l*d
    if (trace) trc <- rbind(trc, c(x_next))
    if (with_restart & (j%%n==0))D <- D0
######## STEP 2
    gradf_next <- grad(f, x_next, der_h, der_eps, der_method, .ndims=n)
    if (TRUE %in% is.na(gradf_next)){
      cat("Got NA as result of grad function in DFP. Returned NA\n")
      return(NA)
    }
    
    p <- l*d
    q <- gradf_next - gradf_prev

    D_p <- (p%*%t(p))/as.numeric(t(p)%*%q) 
    D_m <- (D%*%q%*%t(D%*%q))/as.numeric(t(D%*%q)%*%q)
    D <- D + D_p - D_m
    
    j <- j+1
######## CRITERIA
    if (stop_criteria){
      if (vnorm(gradf_next) < (eps-der_eps)){
        return(list('minimum'=x_next, 'objective'=f(x_next), 'trace' = trc))
      }
    }
    else{
      if (vnorm(x_next-x_prev)<(eps)){
        f_prev <- f(x_prev)
        f_next <- f(x_next)
        if (abs(f_next-f_prev)<eps)
          return(list('minimum'=x_next, 'objective'=f_next, 'trace' = trc))
      }
    }
    gradf_prev <-gradf_next
    x_prev <- x_next
  }
  cat("DFP reached max number of iterations",maxiters,'\n')
  cat("DFP returned NA\n")
  return(NA)
}


