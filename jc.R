jc <- function(X) 
{
      x1 <- X[1]
      x2 <- X[2]
      y1 <- X[3]
      y2 <- X[4]
      a <- abs( (x1+x2)/(1+abs(x1+x2)) - (y1+y2)/(1+abs(y1+y2)) )
      b <- abs(x1/(1+abs(x1)) - y1/(1+abs(y1))) + abs(x2/(1+abs(x2)) - y2/(1+abs(y2)))
      c(a,b)
}