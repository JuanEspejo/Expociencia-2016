library("rJava")
library("xlsxjars")
library("xlsx")
library("RNetCDF")

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

index = (s <= 3068)
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

years = integer(0)
months = years
days = years

# creating time and cloud of points matrices
#year = 1994
year = 1998
month = 1
day = 1
for(i in 1:m)
{
      time[i,] = c(year,month,day)
      years = c(years,year)
      months = c(months,month)
      days = c(days,day)
      A[i,] = X[,,i][index]
      switch(month,ty1(),fe(),ty1(),ty(),ty1(),ty(),ty1(),ty1(),ty(),ty1(),ty(),ty1()) 
}

# closing connection
close.nc(nc)


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
for(i in 1:366)
{
      template[i,1:2] = c(month,day)
      ind = time[,2]==month & time[,3]==day
      template[i,3:(2+n)] = colMeans(A[ind,],na.rm=TRUE)
      switch(month,t1(),t9(),t1(),t0(),t1(),t0(),t1(),t1(),t0(),t1(),t0(),t1()) 
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



AA = data.frame(years,months,days)
AA = cbind(AA,as.data.frame(A))


buoy = c("year","month","day")
for(j in 1:8)
{
      for(i in 1:11)
      {
            if(index[i,j])
            {
                  jj = switch(j,"8S","5S","2S","0N","2N","5N","8N","9N")
                  ii = switch(i,"137E","147E","156E","165E","180E","170W","155W","140W","125W","110W","95W")
                  buoy = c(buoy,paste0(jj,ii))      
            }
      }
}

names(AA) = buoy
