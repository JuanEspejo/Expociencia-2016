ev = function(S,u)
{
      while (l2(u - S%*%u/l2(S%*%u))>1e-15)
            u = S%*%u/l2(S%*%u)
      return(u)
}
      