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
years = 1962:2000;
plot(1962:2000,d[1,5:p],xlab='year',ylab='World')





#######################
## Core
#######################

##########################
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

duplicates <- matrix(data=c("Russian Fed","Fm USSR","Germany","Fm German FR","Germany","Fm German DR"),ncol=2,byrow=TRUE)

N=length(countries)

##########################
# corresponding coordinates
all_coord=coordinates(countriesCoarseLessIslands)
coordbij=read.csv('./Data/bijCoordinates.csv',header=FALSE)
countriesCoords = all_coord[coordbij$V1,]

# test for correspondance of coordinates
#dd=data.frame(rownames(countriesCoords),countries)


##########################
# network drawing function
##########################
drawNetwork <- function(matrix,threshold,countries_inds){
  map()
  matrix = (matrix - min(matrix))/(max(matrix)-min(matrix))
  coords=countriesCoords[countries_inds,]
  for(i in 1:(length(matrix[,1])-1)){
    for(j in (i+1):(length(matrix[1,]))){
      if(i==49&&j==147){show(matrix[i,j])}
        
      if(matrix[i,j]>threshold){
         w = matrix[i,j]
         colindex <- floor(99 * (w-threshold)/(1-threshold)) + 1
         c <- colorRampPalette(c("blue", "red"))(100)[colindex]
         segments(coords[i,1],coords[i,2],
                  coords[j,1],coords[j,2],
                  col=c,lwd=w*10)
      }
    }
  }
}

#test
drawNetwork(matrix(data=runif(N*N,-10,10),nrow=N,ncol=N),0.5)

##########################


##########################
# construct algebric flows matrix \bar{\phi}_{i,j}
###########################
flows = array(data=rep(0,N*N*(p-4)),dim=c(N,N,p-4));

# dirty dirty because of duplicates ; no choice :/

for(i in 1:N){
  show(i)
  for(j in 1:N){
    inds = (d$exporter==countries[i])&(d$importer==countries[j]);
    if(length(which(duplicates[,1]==countries[i]))>0){
      for(k in which(duplicates[,1]==countries[i])){
        inds=inds|((d$exporter==duplicates[k,2])&(d$importer==countries[j]))
      }
    }
    if(length(which(duplicates[,1]==countries[j]))>0){
      for(k in which(duplicates[,1]==countries[i])){
         inds=inds|((d$exporter==countries[i])&(d$importer==duplicates[k,2]))
      }
    }
    if((length(which(duplicates[,1]==countries[i]))>0)&&(length(which(duplicates[,1]==countries[j]))>0)){
      for(k in which(duplicates[,1]==countries[i])){
        for(l in which(duplicates[,1]==countries[j])){
          inds=inds|((d$exporter==duplicates[k,2])&(d$importer==duplicates[l,2]))
        }
      }
    }
    
    f=d[which(inds),5:p]
    if(length(f[,1])>0){
      for(k in 1:length(f[,1])){
        for(t in 1:length(f[1,])){
          if(!is.na(f[k,t])){flows[i,j,t]=flows[i,j,t]+f[k,t]}
        }
      }
    }
       
    inds = (d$importer==countries[i])&(d$exporter==countries[j]);
    if(length(which(duplicates[,1]==countries[i]))>0){
      for(k in which(duplicates[,1]==countries[i])){
        inds=inds|((d$importer==duplicates[k,2])&(d$exporter==countries[j]))
      }
    }
    if(length(which(duplicates[,1]==countries[j]))>0){
      for(k in which(duplicates[,1]==countries[i])){
        inds=inds|((d$importer==countries[i])&(d$exporter==duplicates[k,2]))
      }
    }
    if((length(which(duplicates[,1]==countries[i]))>0)&&(length(which(duplicates[,1]==countries[j]))>0)){
      for(k in which(duplicates[,1]==countries[i])){
        for(l in which(duplicates[,1]==countries[j])){
          inds=inds|((d$importer==duplicates[k,2])&(d$exporter==duplicates[l,2]))
        }
      }
    }

    f=d[inds,5:p]
    if(length(f[,1])>0){
      for(k in 1:length(f[,1])){
        for(t in 1:length(f[1,])){
          if(!is.na(f[k,t])){flows[i,j,t]=flows[i,j,t]-f[k,t]}
        }
      }
    }
  }
}
##########################


##########################
# visualize trade NW (quantities ?)
##########################
drawNetwork(flows[,,1],0.55) # exporters
drawNetwork(flows[,,17],0.55)
drawNetwork(flows[,,27],0.55)
drawNetwork(flows[,,37],0.55)
drawNetwork(flows[,,47],0.55)

##########################
# construct phi_i
##########################
#
cum_flows = matrix(nrow=N,ncol=(p-4))
for(i in 1:N){
  cum = rep(0,(p-4))
  for(j in 1:N){
    cum = cum + flows[i,j,]
  }
  cum_flows[i,]=cum
}
##########################


##########################
# map cum flows : importer or exporter ?
##########################

map_cumflows <- function(year_index){
  # get year column
  f = cum_flows[,year_index]
  
  # join data to world map data
  # --> see help maps, mapdata
  r= rownames(countriesCoords)
  s = joinCountryData2Map(data.frame(r, f),joinCode='NAME',nameJoinColumn = 'r')
  mapCountryData( mapToPlot = joinCountryData2Map(data.frame(r, f),joinCode='NAME',nameJoinColumn = 'r'),nameColumnToPlot ='f',addLegend = FALSE,
                  mapTitle="")
}
##########################
map_cumflows(39)



##########################

cum_flows_rec=(cum_flows-2*min(cum_flows))

# construct returns by integration
LR = matrix(nrow=N,ncol=(p-5))
for(i in 1:N){
  #Ki=cumsum(cum_flows[i,]);
  LR[i,]=diff(log(cum_flows_rec[i,]))
}


##########################
# direct cor matrix
rho = cor(t(LR))

drawNetwork(abs(rho),0.7)

##########################
# construct matrix of relative returns

# pb with close-to-zero flows (then rho~1 as same value)
# -- method works only with same magnitude agents variables.

# visualize flows
plot(years,cf_rec[1,],type='l',col=1);
for(i in 2:length(cf_rec[,1])){
  points(years,cf_rec[i,],type='l',col=i)
}
#-> nothing visible.

# remove close to zeros elements ?
#construct threshold returns
subyears=10:20
s=c();for(i in 1:N){s=append(s,sum(cum_flows[i,subyears]))}

# most important countries
inds=which(abs(s)>(max(abs(s))/10))
countries_max = countries[inds]
cf_rec=cum_flows_rec[inds,]

# plot
plot(years,cf_rec[1,],type='l',col=1,main='rectified yearly balance');
for(i in 2:length(cf_rec[,1])){
  points(years,cf_rec[i,],type='l',col=i)
}


LR_th = matrix(nrow=length(inds),ncol=length(cf_rec[1,])-1)
for(i in 1:length(LR_th[,1])){
  LR_th[i,]=diff(log(cf_rec[i,]))
}

# try cor matrix with that ?
rho = cor(t(LR_th))
drawNetwork(abs(rho),0,inds)

##########################
# --> equal weights numeraire
rel_LR = matrix(nrow=length(inds),ncol=length(cf_rec[1,])-1)
for(i in 1:length(LR_th[,1])){
  for(t in 1:length(LR[1,])){
    rel_LR[i,t]=LR_th[i,t]-1/length(LR_th[,1])*sum(LR_th[,t]);
  }
}



##########################
# construct normalized correlation matrix (3 times ?)
rho = cor(t(rel_LR))
drawNetwork(abs(rho),0,inds)

##########################
# visualize NW / NW measures ?

#drawNetwork(abs(rho),0.97)
# ok done before

# --> PB ; indeed, close-to-zeros flows are then highly correlated ; no sense.
# :: back, threshold before ?
# -> reduce the number of countries, to have comparable flow
#  - redraw wn maps, then with.


##########################
# that's all folks !
##########################