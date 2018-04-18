jc0 = function(n){
      x = runif(n,-pi/2,pi/2)
      x = tan(x)
      x = tapply(x, 1:n, f0)
      xx = numeric(n)
      for (i in 1:n) {
            xx[i] = mean(head(x,i))
      }
      plot(xx,type="l")
      tail(xx,10)
}

jc1 = function(n){
      x = runif(n,-pi/2,0)
      x = tan(x)
      x = f(x)
      xx = numeric(n)
      for (i in 1:n) {
            xx[i] = mean(head(x,i))
      }
      plot(xx,type="l")
      tail(xx,10)
}

jc2 = function(n){
      x = rnorm(n,6,1)
      x = tapply(x, 1:n, g)
      xx = numeric(n)
      for (i in 1:n) {
            xx[i] = mean(head(x,i))
      }
      plot(xx,type="l")
      tail(xx,10)
}

jc3 = function(n){
      x = rnorm(n,0,1)
      x = tapply(x, 1:n, h)
      xx = numeric(9e2)
      for (i in 1:9e2) {
            xx[i] = mean(head(x,n+i-9e2))
      }
      plot(xx,type="l")
      tail(xx,10)
}

jc33 = function(n,k){
      x = numeric(k)
      s = 0
      d = n-k
      for(i in 1:n){
            if(i %% 1e5 == 0) print(sprintf("%5.1f%1s", 100*i/n,'%'))
            z = rnorm(1,0,1)
            if(z < 0) z = exp(6*z-18)
            else z = 0
            s = s + z
            if(i > d) x[i-d] = s/i
      }
      plot(x,type="l")
      tail(x,20)
}

jc4 = function(n){
      x = rnorm(n,0,1)
      x = tapply(x, 1:n, k)
      xx = numeric(n)
      for (i in 1:n) {
            xx[i] = mean(head(x,i))
      }
      plot(xx,type="l")
      return(xx)
}

f0 = function(x){
      if(x<0)
            return( exp(-(x-6)^2/2)/sqrt(2*pi) )
      else
            return(0)
}

n61 = function(x){
      return( exp(-(x-6)^2/2)/sqrt(2*pi) )
}

f = function(x){
      return( exp(-x^2/2+6*x-18)/sqrt(2*pi) )
}

g = function(x){
      if(x<0){
            return(x)      
      }
      else{
            return(0)
      }
}

h = function(x){
      if(x<0){
            return(exp(6*x-18))      
      }
      else{
            return(0)
      }
}

k = function(x){
      if(x<0){
            return(exp(6*(x-1)))      
      }
      else{
            return(0)
      }
}