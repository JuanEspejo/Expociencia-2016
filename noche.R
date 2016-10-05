temp2 = function ( dfib = 4e-03, daire = 2e-02, 
                  dairesuelo = 3.9e-02, dpiedra = 4.1e-02, 
                  dtriplay = 1e-02, dpared = 5.1e-02,
                  lint = 4.9e-01, h = 2.68e-01, lambda = 12,
                  irra = 200, tinf = 10, tsuelo = 15, t0 = 30, theta = 5e-01 )
      
{
      t0K = t0 + 273.15
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
      
      dtecho = dfib + daire
      dsuelo = dairesuelo + dpiedra + dtriplay
      
      ksuelo = dsuelo / (dairesuelo/kaire + dpiedra/kpiedra + dtriplay/ktriplay)      
      
      htecho = 1 / ( (2*dfib/kfib + daire/kaire)*(1/(1-theta)) + (1/3.9 + dfib/kfib)*(1/theta) + 1/hint + 1/hext )
      hlateral = 1/(1/hint + 1/hext + dpared/kpared)
      hsuelo = 1/(dsuelo/ksuelo + 1/hint)
      
      asuelo = lint*lint
      atecho = lint*lint/cos(lambda*pi/180)
      alateral = 2*lint*(2*h + delta)
      
      vcasa = asuelo*(h + 0.5*delta)
      n = paire*vcasa/M
      
      a = (atecho*htecho + alateral*hlateral + asuelo*hsuelo) / (5/2*n*R)
      b = (atecho*htecho*tinfK + alateral*hlateral*tinfK + asuelo*hsuelo*tsueloK) / (5/2*n*R)      
      
      print(a)
      print(b)
      plot.new()
      curve((t0K - b/a)*exp(-60*a*x) + b/a - 273.15, from=0, to=30, , xlab="tiempo(min.)", ylab="T(°C)")
}
