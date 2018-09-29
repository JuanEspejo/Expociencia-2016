E_P = function(n,k){
    x = numeric(k)
    s = 0
    d = n - k
    for(i in 1:n){
        z = g(rnorm(1,0,1))
        s = s + z
        if(i > d) x[i-d] = s/i
    }
    plot(x,type="l")
    tail(x,1)
}

E_Q = function(n,k){
    x = numeric(k)
    s = 0
    d = n - k
    for(i in 1:n){
        z = f(rnorm(1,6,1))
        s = s + z
        if(i > d) x[i-d] = s/i
    }
    plot(x,type="l")
    tail(x,1)
}

g = function(x){
      if(x < 0) return(exp(6*x-18))      
      else return(0)
}

f = function(x){
      if(x < 0) return(x)      
      else return(0)
}
