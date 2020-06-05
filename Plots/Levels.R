levels <- function(levvals, step=1e-2, xlim=c(0,2), ylim=c(0,2), lcol='blue',lwd=1,...){
  rosenbrock <- function(x) 100*(x[2]-x[1]^2)^2 + (1-x[1])^2
  
  line_part <- function(cnst, x){
    if (cnst < (1-x)^2) return(NA)
    return(sqrt((cnst-(1-x)^2))/10)
  }
  
  X <- list()
  X_pts <- seq(from=xlim[1], to=xlim[2], by=step)
  i=1
  
  plot(NULL,xlim=xlim, ylim=ylim, ...)
  
  for (lv in levvals){
    X_cur <- NULL
    Y_pos <- NULL
    Y_neg <- NULL
    
    for(x in X_pts){
      y <- line_part(lv, x)
      if (is.na(y)){
        if (!is.null(X_cur)){
          lines(X_cur,Y_pos, col=lcol, lwd=lwd)
          lines(X_cur,Y_neg, col=lcol, lwd=lwd)
          X_cur <- NULL
          Y_pos <- NULL
          Y_neg <- NULL
          i <- i+2
        }
      }
      else {
        X_cur <- c(X_cur, x)
        Y_pos <- c(Y_pos, y+x^2)
        Y_neg <- c(Y_neg, -y+x^2)
        i <- i+2
      }
    }
    if (!is.null(X_cur)){
      lines(X_cur,Y_pos, col=lcol, lwd=lwd)
      lines(X_cur,Y_neg, col=lcol, lwd=lwd)
      i <- i+2
    }
  }
  
  lines(1,1,type='p', col='red', pch=16)
}