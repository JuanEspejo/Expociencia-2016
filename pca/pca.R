pca = function()
{
      ###### libraries and functions to use ######
      
      library("RNetCDF")
      
      l2 = function(x) 
            sqrt(sum(x^2))
      
      ev = function(S,u)
      {
            while (l2(u - S%*%u/l2(S%*%u))>1e-15)
                  u = S%*%u/l2(S%*%u)
            return(u)
      }
      
      # returns TRUE if year is leap and FALSE otherwise
      leap = function(year)
      {
            if (year%%4 != 0) return(FALSE)
            else if (year%%100 != 0) return(TRUE)
            else if (year%%400 == 0) return(TRUE)
            else return(FALSE)
      }
      
      ############################################
      
      
      
      
      ########### downloading data ###############
      
      nc = open.nc("/home/juanca/Downloads/sst_xyt_dy_2byte.cdf")
      X = read.nc(nc)$sst
      m = dim(X)[3]
      
      s = 0
      for(i in 1:m) s = s + is.na(X[,,i])
      
      index = (s < 4800)
      n = sum(index)
      time = matrix(NA,m,3)
      A = matrix(NA,m,n)
      
      # for february
      fe = function()
      {
            if ( (leap(year) && day==29) || (!leap(year) && day==28) )
            {
                  month <<- 3
                  day <<- 1
            } else day <<- day + 1
      }
      
      # for a month of thirty days 
      ty = function()
      {
            if (day == 30)
            {
                  month <<- month + 1
                  day <<- 1
            } else day <<- day + 1
      }      
      
      # for a month of thirty one days
      ty1 = function()
      {
            if (day == 31 && month == 12)
            {
                  year <<- year + 1
                  month <<- 1
                  day <<- 1
            } else if (day == 31)
            {
                  month <<- month + 1
                  day <<- 1
            } else day <<- day + 1
      }      
      
      # creating time and cloud of points matrices
      year = 1994
      month = 1
      day = 1
      i = 1
      while(i <= m)
      {
            time[i,] = c(year,month,day)
            A[i,] = X[,,i][index]
            switch(month,ty1(),fe(),ty1(),ty(),ty1(),ty(),ty1(),ty1(),ty(),ty1(),ty(),ty1()) 
            i = i + 1
      }
      
      # closing connection
      close.nc(nc)      

      
      #####################################################
      
      
      
      
      ############ setting data for analysis ##############
      
      # for february
      t9 = function()
      {
            if (day == 29)
            {
                  month <<- 3
                  day <<- 1
            } else day <<- day + 1
      }
      
      # for a month of thirty days 
      t0 = function()
      {
            if (day == 30)
            {
                  month <<- month + 1
                  day <<- 1
            } else day <<- day + 1
      }      
      
      # for a month of thirty one days
      t1 = function()
      {
            if (day == 31)
            {
                  month <<- month + 1
                  day <<- 1
            } else day <<- day + 1
      }      
      
      # creating template matrix
      template = matrix(NA,366,2+n)
      month = 1
      day = 1
      i = 1
      while(i <= 366)
      {
            template[i,1:2] = c(month,day)
            ind = time[,2]==month & time[,3]==day
            template[i,3:(2+n)] = colMeans(A[ind,],na.rm=TRUE)
            switch(month,t1(),t9(),t1(),t0(),t1(),t0(),t1(),t1(),t0(),t1(),t0(),t1()) 
            i = i + 1
      }
      
      # filling cloud of points matrix
      for(i in 1:m)
      {   
            for(j in 1:n)
            {
                  if(is.na(A[i,j]))
                        A[i,j] = template[template[,1]==time[i,2] & template[,2]==time[i,3]][2+j]
                  j = j + 1
            }
            print(i)
            i = i + 1
      }
      
      ###################################################
      
      

            
      ########## applying our pca methodology ###########

      z = colMeans(A)
      A = A - matrix(1,m,1)%*%z
      
      # calculating covariance matrix
      S = t(A)%*%A 
      
      # calculating first principal component
      u = matrix(0,n,1)
      u[1] = 1
      u = ev(S,u)
      
      # calculating second principal component
      v = matrix(0,n,1)
      if (sum(u==0) == 0)
      {
            v[1] = -u[2]
            v[2] = u[1]
      } else
      {
            for (i in 1:n)
                  if (u[i] == 0) v[i] = 1
      }
      v = v/l2(v)
      U = diag(n) - u%*%t(u)
      S = U%*%S%*%U
      v = -ev(S,v)

      # day points
      dpoint = cbind(A%*%u,A%*%v)

      
      
      ############## representing cloud of points ####################      

      
      plot.new()
      #title(xlab="Primer eje principal", ylab="Segundo eje principal", main="Representación en el plano principal")
      title(main="sst - daily representation")
      plot.window(xlim=c(-20,20), ylim=c(-10,15))
      abline(h=0, v=0, lty=1)
      axis(1)
      axis(2)
      box()
      points(dpoint[,1], dpoint[,2], pch=19, col="blue", cex=0.10)

      
      plot.new()
      title(main="sst - monthly pattern")
      mlabel = c("January","February","March","April","May","June","July","August","September","October","November","December")
      mpoint = matrix(NA,13,2)
      plot.window(xlim=c(-8,10), ylim=c(-4,6))
      abline(h=0, v=0, lty=1)
      axis(1)
      axis(2)
      box()
      for(i in 1:12)
            mpoint[i,] = colMeans(dpoint[time[,2]==i,])
      mpoint[13,] = mpoint[1,]
      for(i in 1:12)
      {
            points(mpoint[i,1], mpoint[i,2], pch=1, col="darkgreen", cex=0.80)
            text(mpoint[i,1], mpoint[i,2], mlabel[i], pos=3, col="darkgreen", cex=0.70)
      }
      arrows(mpoint[1:12,1],mpoint[1:12,2],mpoint[2:13,1], mpoint[2:13,2], length=0.09, code=2, col="darkgreen")
      
      
      plot.new()
      title(main="sst - yearly representation")
      ylabel = c("1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
      yn = 2016-1993
      ypoint = matrix(NA,yn,2)
      plot.window(xlim=c(-6,8), ylim=c(-5,5))
      abline(h=0, v=0, lty=1)
      axis(1)
      axis(2)
      box()
      for(i in 1:yn)
            ypoint[i,] = colMeans(dpoint[time[,1]==(1993+i),])
      for(i in 1:yn)
      {
            points(ypoint[i,1], ypoint[i,2], pch=1, col="red", cex=0.50)
            text(ypoint[i,1], ypoint[i,2], ylabel[i], pos=3, col="red", cex=0.75)
      }
      arrows(ypoint[1:(yn-1),1],ypoint[1:(yn-1),2],ypoint[2:yn,1], ypoint[2:yn,2], length=0.05, code=2, col="red")
      
      
      ##################################################
}