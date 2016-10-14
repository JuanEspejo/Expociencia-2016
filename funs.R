fun = function (t,a)
{
      y = 0
      for (n in -9:9)
            y = y + Re(a[n+10])*cos(2*pi*n*t/23) - Im(a[n+10]*sin(2*pi*n*t/23))
      return (y)
}


tinf = function (t,a,m,b)
{
      y = 0
      for (n in -9:9)
            y = y + Re(a[n+10])*cos(2*pi*n*t/23) - Im(a[n+10]*sin(2*pi*n*t/23))
      return (y + t*m + b)
      #return (y)
}


ifun = function (t,a)
{
      y = 0
      for (n in -9:9)
            y = y + Re(a[n+10])*sin(2*pi*n*t/23) + Im(a[n+10]*cos(2*pi*n*t/23))
      return (y)
}
