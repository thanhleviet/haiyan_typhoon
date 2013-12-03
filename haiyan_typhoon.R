#Some pieces of code of this script was inherited from http://www.r-bloggers.com/r-mapping-super-typhoon-yolanda-haiyan-track/
#lethanhx2k@gmail.com
#Dec 03 2013
#Load the packages
library(RCurl)
library(ggplot2)
library(maptools)
library(OpenStreetMap)
library(ggmap)
library(scales)
library(animation)
library(grid) #To get arrow()
gpclibPermit()

#Import the data
temp_file <-getURL("https://raw.github.com/alstat/Analysis-with-Programming/master/2013/R/R-Mapping-Super-Typhoon-Yolanda-Haiyan-Track/TyDatYolanda2013.csv",ssl.verifypeer=0L,followlocation=1L)
writeLines(temp_file,"haiyan.csv")
tydat <- read.csv("haiyan.csv", header = TRUE)

#Clean mess
tydat$Time <- sub("\t","",tydat$Time)
tydat$dat <- sub("Nov. ","",tydat$Date)
tydat$month <- c(11)
tydat$year <- c(2013)
tydat$Category <- sub("\t","",tydat$Category)

#Create legend

legend <- data.frame(abbr=c("TD","TS","TY","STY"),
                     desc=c("Tropical Depression","Tropical Storm","Typhoon","Super Typhoon"),
                     X=c(0),
                     Y=seq(300000,0,length.out=4))


#Obtain the Map
map <- openmap(c(27.605671,103.256834),
               c(1.757537,150.4541),
               minNumTiles=4,type="bing",zoom = 6)
# 
# map <- openproj(map)
conic <- c("+proj=lcc +lat_1=-1 +lat_2=25 +lat_0=10 +lon_0=120 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#Project the map from lat/long to Lambert conformal conic in order to keep the right shape of each country in Southeast Asia region

conic_map <- openproj(map,projection=conic)
tydat$conicx <- project(cbind(tydat$CLongitude,tydat$CLatitude),conic)[,1]
tydat$conicy <- project(cbind(tydat$CLongitude,tydat$CLatitude),conic)[,2]

#Calculate symbol size of each time point based on wind speed
tydat$symbol <- with(tydat,MaxSpeed/max(MaxSpeed))


#Original position of the text speed and time
xr <- 1200000.0
yr <- 1100000.0

delta_x <- 500000.0
delta_y <- 300000.0
# delta_x_text <- delta_x - 200
delta_y_text <- 0 #delta_y - 150000
#Function to plot background map, typhoon data and day i
plotmap <- function (conic_map, data,i=1) {
  k <- i + 1
  typhoon <-data[i,] #sub data frame to plot the storm/day
  typhoon_path <-data[1:k,] #sub dataframe to plot track
  map0 <- autoplot(conic_map) +geom_path(data=typhoon_path,aes(x=conicx,y=conicy),size=1.6,alpha=0.6,colour="red",arrow=arrow(angle=15)) +
    geom_rect(aes(xmin=-1550000,xmax=-300000,ymin=-700000,ymax=-200000),fill="grey90",colour="grey80",alpha=0.3)
  map <- map0 + geom_point(data=typhoon,aes(x=conicx,y=conicy),alpha=0.3,colour="yellow",size=typhoon$symbol*40)+ 
    geom_point(data=typhoon,aes(x=conicx,y=conicy),alpha=0.5,colour="orange",size=typhoon$symbol*34) + 
    geom_text(data=typhoon,aes(x=conicx,y=conicy,label=Category,family="serif",fontface="bold"),colour="white",size=rel(4)) + 
    geom_text(data=typhoon,aes(x=xr,y=yr+delta_y_text,label = paste("Thời gian: ",Time," ngày ",dat,"/",month,"/",year,sep=""),family = "serif",fontface = "bold"),hjust=0,colour = "white",size = rel(5))+
    geom_text(data=typhoon,aes(x=xr,y=yr-120000,label = paste("Vận tốc gió: ",MaxSpeed," m/h",sep=""),family = "serif",fontface = "bold"),hjust=0,colour = "white",size = rel(5))+
    geom_text(data=legend,aes(x=(X-1500000),y=(Y-600000),label=paste(abbr,desc,sep=": ")),colour="white",hjust=0) +
    labs(title = "Siêu bão Hải Yến 3/11 - 11/11, 2013") +
    xlab("Kinh độ") + 
    ylab("Vi độ")  +
    theme(panel.background = element_rect(
      size = 1, 
      colour = "black",
      fill = "white"),
          axis.ticks = element_line(
            size = 1),
          axis.title.x = element_text(
            size = rel(1.2), 
            face = "bold"),
          axis.title.y = element_text(
            size = rel(1.2), 
            face = "bold"),
          plot.title = element_text(
            size = 20,
            face = "bold", 
            vjust = 1.5),
          legend.position = "bottom",
          legend.title = element_text(
            size=rel(1.2), 
            face="bold")) +
    scale_x_continuous(labels = comma)+
    scale_y_continuous(labels = comma)+
    coord_fixed()
  print(map)
}

#Function to plot all days
aniplot <- function(data){
  max <- nrow(data)
  for(i in(1:max)){
    plotmap(conic_map,data,i)
  }
}

#Function to export time series to movie
exportMovie <- function (tydat) {
  oopt = ani.options(interval=1,nmax=nrow(tydat),ani.width=900,ani.height=695,outdir=getwd())
  saveVideo(aniplot(tydat),video.name="typhoon_hayan.mp4",ffmpeg = "ffmpeg")
}

#Run after sourcing the script i.e source("hayan_typhoon.r")
exportMovie(tydat)
