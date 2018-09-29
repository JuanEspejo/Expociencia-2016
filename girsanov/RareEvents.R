E_Q = function(n,k){
    x = numeric(k)
    s = 0
    d = n - k
    for(i in 1:n){
        #if(i %% 1e5 == 0) print(sprintf("%5.1f%1s", 100*i/n,'%'))
        z = rnorm(1,0,1)
        if(z < 0) z = exp(6*z-18)
        else z = 0
        s = s + z
        if(i > d) x[i-d] = s/i
    }
    plot(x,type="l")
    tail(x,20)
}

f = function(x){
      if(x < 0) return(x)      
      else return(0)
}
