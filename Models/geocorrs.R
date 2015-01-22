setwd('GeoCorrs')

# load data
library(foreign)
d <- read.dta("Data//Trade//wtf_bilat//wtf_bilat.dta")
p=length(d[1,])

# mapping
library(maps)
library(mapdata)
library(rworldmap)
data(countriesCoarseLessIslands)

# Example : plot total world trade
#plot(1962:2000,d[1,5:p]);

# \phi_i = sum_{j\neq i}\phi^{(i,j)}
# same with algebric ?

# example : France -> Ireland
plot(1962:2000,d[19154,5:p],xlab='year',ylab='FR->UK')





#######################
## Core
#######################

#get countries list
countriesIDs = unique(d[1:201,3]);
countries = unique(d[,4])
# remove former countries or shitty elements
to_remove = c(1,9,32,37,39,41,50,53,55,67,73,75,78,81,82,84,85,87,88,92,95,97,104,107,111,112,114,120,125,130,142,143,151,159,166,167,168,172,174,176,189,190,191,192,196,199,200,201)
c=0
for(r in to_remove){
  countries<-countries[(-r+c)]
  c=c+1
}

countries=sort(countries)

N=length(countries)

# corresponding coordinates
all_coord=coordinates(countriesCoarseLessIslands)
coordbij=read.csv('./Data/bijCoordinates.csv',header=FALSE)
countriesCoords = all_coord[coordbij$V1,]

# test for correspondance of coordinates
#dd=data.frame(rownames(countriesCoords),countries)


# network drawing function
drawNetwork <- function(matrix,threshold){
  map()
  matrix = (matrix - min(matrix))/(max(matrix)-min(matrix))
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      if(i==49&&j==147){show(matrix[i,j])}
        
      if(matrix[i,j]>threshold){
         w = matrix[i,j]
         colindex <- floor(99 * w) + 1
         c <- colorRampPalette(c("yellow", "darkred"))(100)[colindex]
         segments(countriesCoords[i,1],countriesCoords[i,2],
                  countriesCoords[j,1],countriesCoords[j,2],
                  col=c,lwd=w*10)
      }
    }
  }
}

#test
drawNetwork(matrix(data=runif(N*N,-10,10),nrow=N,ncol=N),0.5)


# construct algebric flows matrix \bar{\phi}_{i,j}
flows = array(data=rep(0,N*N*(p-4)),dim=c(N,N,p-4));

for(i in 1:N){
  show(i)
  for(j in 1:N){
    f=d[which((d$exporter==countries[i])&(d$importer==countries[j])),5:p]
    if(length(f[,1])>0){
      for(k in 1:length(f[,1])){
        for(t in 1:length(f[1,])){
          if(!is.na(f[k,t])){flows[i,j,t]=flows[i,j,t]+f[k,t]}
        }
      }
    }
    f=d[which((d$exporter==countries[j])&(d$importer==countries[i])),5:p]
    if(length(f[,1])>0){
      for(k in 1:length(f[,1])){
        for(t in 1:length(f[1,])){
          if(!is.na(f[k,t])){flows[i,j,t]=flows[i,j,t]-f[k,t]}
        }
      }
    }
  }
}

# visualize trade NW (quantities ?)



# construct phi_i
cum_flows = matrix(nrow=N,ncol=(p-4))
for(i in 1:N){
  cum = rep(0,(p-4))
  for(j in 1:N){
    cum = cum + flows[i,j,]
  }
  cum_flows[i,]=cum
}

# construct returns by integration
LR = matrix(nrow=N,ncol=(p-4))
for(i in 1:N){
  LR[i,]=cum_flows[i,]/cumsum(cum_flows[i,])
}



# construct matrix of relative returns


# construct matrix of normalized returns


# construct normalized correlation matrix (3 times ?)


# visualize NW / NW measures ?



# that's all folks !