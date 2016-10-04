temp = function ( dfib = 4e-03, dairet = 2e-02, 
                  daire = 3.9e-02, dpiedra = 4.1e-02, 
                  dtriplay = 1e-02, dpared = 5.1e-02,
                  lint = 4.9e-01, h = 2.68e-01, lambda = 12,
                  irra = 200, tinf = 30, tsuelo = 25, theta = 5e-01 )

{
      tinfK = tinf + 273.15
      tsueloK = tsuelo + 273.15
      delta = lint*tan(lambda*pi/180)
      
      R = 8.31434
      alpha = 7.7e-01
      paire = 1.205
      M = 2.8e-02
      hint = 20
      hext = 25
      sigma = 5.6703e-08
      epsilon = 8e-01
      
      kfib = 3.46e-01
      kaire = 2.624e-02
      kpiedra = 3.5
      ktriplay = 1.4e-01
      kpared = 6.25e-01
      
      dtecho = dfib + dairet
      dsuelo = daire + dpiedra + dtriplay
      
      ktecho = dtecho / (2*dfib/kfib + dairet/kaire)
      ksuelo = dsuelo / (daire/kaire + dpiedra/kpiedra + dtriplay/ktriplay)      
      
      htecho = 1/(dtecho/ktecho + 1/hint)
      hlateral = 1/(1/hint + 1/hext + dpared/kpared)
      hsuelo = 1/(dsuelo/ksuelo + 1/hint)
      
      tt = c(-hext*tinfK - irra, hext, 0, 0, sigma*epsilon)
      tt = polyroot(tt)
      ttecho = Re(tt[1])
      
      asuelo = lint*lint
      atecho = lint*lint/cos(lambda*pi/180)
      alateral = 2*lint*(2*h + delta)
      
      vcasa = asuelo*(h + 0.5*delta)
      n = paire*vcasa/M
      
      a = ((1-theta)*atecho*htecho + alateral*hlateral + asuelo*hsuelo) / (5/2*n*R)
      b = (alpha*theta*irra*atecho + (1-theta)*atecho*htecho*ttecho + alateral*hlateral*tinfK + asuelo*hsuelo*tsueloK) / (5/2*n*R)      

      print(a)
      print(b)
      plot.new()
      curve((tinfK - b/a)*exp(-60*a*x) + b/a - 273.15, from=0, to=30, , xlab="tiempo(min.)", ylab="T(°C)")
}    
    
    
