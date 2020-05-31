cnt <- 0
increment <- function(){ cnt <<- cnt+1}
discharge <- function() {
  ret <- cnt
  cnt <<- 0
  return(ret)
}
