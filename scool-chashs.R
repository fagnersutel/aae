#https://nycdatascience.com/blog/student-works/nyc-speed-camera-program-revenue-or-safety-well-get-you-up-to-speed-in-a-flash/

School.Loc = read.csv('NYC_School_Locations.csv', stringsAsFactors=F)
School.Loc$Address = paste(School.Loc$Address,", New York, NY", sep="", collapse = NULL)
School.LonLat = geocode(School.Loc$Address)
School.Loc = cbind(School.Loc, School.LonLat)

library(plyr)
library(dplyr)
Accidents = read.csv("NYPD_Motor_Vehicle_Collisions.csv")
Accidents = select(Accidents, NUMBER.OF.PEDESTRIANS.KILLED, NUMBER.OF.PEDESTRIANS.INJURED,
                   DATE, BOROUGH, LATITUDE, LONGITUDE)
Accidents = filter(Accidents, NUMBER.OF.PEDESTRIANS.KILLED >=1 | NUMBER.OF.PEDESTRIANS.INJURED >=1)
Accidents = Accidents[complete.cases(Accidents$LONGITUDE,Accidents$LATITUDE),]
Accidents$DATE = as.Date(as.character(Accidents$DATE), "%m/%d/%Y")
colnames(Accidents) = c('Peds.Killed', 'Peds.Injured', 'Date', 'Borough', 'Lat', 'Lon')


library(reshape2)
Speed.Cam = read.csv('Speed_Camera_Tickets.csv', stringsAsFactors=F)
Speed.Cam = select(Speed.Cam, Issue.Date, Street, Intersecting.Street)
Speed.Cam$Issue.Date = as.Date(as.character(Speed.Cam$Issue.Date), '%m/%d/%Y')
Speed.Cam$Address = paste(Speed.Cam$Street, Speed.Cam$Intersecting.Street, sep = "", collapse = NULL)
Speed.Cam = select(Speed.Cam, Issue.Date, Address)
range(Speed.Cam$Issue.Date)
Speed.Cam.Melt = melt(Speed.Cam, id='Address')
Cam = dcast(Speed.Cam.Melt, Address~., length)
colnames(Cam) = c('Address', 'count')
fix(Cam) 
Cam = ddply(Cam, 'Address', colwise(sum, ~count))
Cam.Lonlat = geocode(Cam$Address[1:10])
Cam = cbind(Cam[1:10,], Cam.Lonlat)



library(ggplot2)
library(ggmap)
ny <- get_map('New York City')
ggmap(ny, extent='device', legend = 'topleft')+
  geom_point(aes(x=lon, y=lat, size=count), color='red', data=Cam)+
  scale_size_continuous(range=c(2,8)) +
  labs(title = 'Map of Speed Camera Locations in NYC', size = 'Tickets/Camera')



Accidents.lastyear = Accidents[Accidents$Date >= as.Date("2013-01-16", "%Y-%m-%d") & 
                                 Accidents$Date <= as.Date("2013-06-20", "%Y-%m-%d"),]
Accidents.thisyear = Accidents[Accidents$Date >= as.Date("2014-01-16", "%Y-%m-%d") & 
                                 Accidents$Date <= as.Date("2014-06-20", "%Y-%m-%d"),]
ggmap(ny, extent='device', legend='topleft')+
  geom_point(aes(x=lon, y=lat, size=count), color='red', data=Cam)+ scale_size_continuous(range=c(2,8))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.title.x=element_blank(),axis.title.y=element_blank())+
  stat_density2d(aes(x=lon, y=lat, fill = ..level.., alpha=..level..), size = 5, bins=10, 
                 geom='polygon', data=School.Loc)+scale_alpha(range = c(0,0.35), guide=FALSE)+ 
  scale_fill_gradient(limits = c(1,40), low='yellow', high='orange')+
  labs(title = 'Map of speed camera locations in NYC overlayed with K - 12 school density',
       size='Tickets/Camera', fill = 'SchoolnDensity')






ggmap(ny, extent='device', legend='topleft')+
  geom_point(aes(x=lon, y=lat, size=count), color='red', data=Cam)+ scale_size_continuous(range=c(2,8))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.title.x=element_blank(),axis.title.y=element_blank())+
  stat_density2d(aes(x=Lon, y=Lat, fill = ..level.., alpha=..level..), size = 5, bins=10, 
                 geom='polygon', data=Accidents.lastyear)+ 
  scale_fill_gradient(limits = c(1,60), low='light blue', high = 'blue')+
  scale_alpha(range = c(0.3,0.75), guide=FALSE)+
  labs(title = 'Map of NYC speed camera locations overlayedn with accident density before camera deployment (Jan - June 2013)', 
       size='Tickets/Camera', fill = 'AccidentnDensity')



ggmap(get_map(location = c(lon = Cam[which.max(Cam$count), 'lon'], 
                           lat = Cam[which.max(Cam$count), 'lat']), zoom=15), legend='topleft')+ 
  geom_point(aes(x=lon, y=lat, size=count), color='red', data=Cam)+ scale_size_continuous(range=c(2,8))+
  geom_point(aes(x=lon, y =lat), size = 3, data = School.Loc)+ 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.title.x=element_blank(),axis.title.y=element_blank(), axis.ticks=element_blank())+
  labs(title = 'The most profitable speed camera and nearest school locations', size = 'Tickets/Camera')






library(plyr)
library(dplyr)
mapdist(from = c(lon = Cam[which.max(Cam$count), 'lon'], 
                 lat = Cam[which.max(Cam$count), 'lat']), to = maxtix.schools$Address)





chisq.test(x=c(nrow(Accidents.lastyear),nrow(Accidents.thisyear)))

ggplot(by.month, aes(x=as.Date(Cut),y=total))+geom_point()+geom_line()+geom_smooth(method='lm')+
  labs(title='NYC Accident Rate')+theme_bw()+xlab('')+ylab('Pedestrian/Vehicle Accidents')+ 
  scale_y_continuous(limits=c(590,1200))



camsum = function(x){
  y = data.frame()
  for (i in 1:length(x)){
    y[i,1] = length(grep(x[i], Cam$Address, ignore.case=T))
    y[i,2] = sum(Cam$count[grep(x[i], Cam$Address, ignore.case=T)])
  }
  y = cbind(y, x)
  return(y)
}

byBorough = camsum(c('BROOKLYN','STATEN ISLAND','QUEENS','NEW YORK','BRONX'))
colnames(byBorough) = c('Cameras','Tickets','Borough')
byBorough$Borough = as.character(byBorough$Borough)
byBorough$Borough[4] = 'MANHATTAN'

this.yr = ddply(Accidents.thisyear, .(Borough), summarize, 
                Peds.Injured = sum(Peds.Injured), Peds.Killed = sum(Peds.Killed))
last.yr = ddply(Accidents.lastyear, .(Borough), summarize, 
                Peds.Injured = sum(Peds.Injured), Peds.Killed = sum(Peds.Killed))

acc.by.borough = inner_join(this.yr, last.yr, by='Borough')
byBorough = inner_join(acc.by.borough, byBorough, by='Borough')

library(reshape2)
mBorough = melt(byBorough, id = c('Borough','Cameras','Tickets'))
mBorough$Year = rep(2014:2013, each=10)
mBorough$variable = rep(c('Killed', 'Injured'), each = 5)
mBorough$Cameras[11:20]=0
mBorough$Tickets[11:20]=1

table = dcast(mBorough, Borough~Year, sum)
table$Borough = NULL
chisq.test(table)



ggplot(mBorough[mBorough$variable == 'Killed',], aes(x=Borough, y=value, color=as.factor(Year)))+
  ylab('Number of Pedestrians Killed')+ geom_point(aes(y = value, group = variable, size = Tickets))+
  geom_line(aes(y=value,group=Year))+ theme_bw()+
  labs(title='Number of pedestrians deaths before and after camera installation', color='Year')+
  theme(axis.text.x=element_text(angle=50, hjust=1), axis.title.x=element_blank())



ggplot(mBorough[mBorough$variable == 'Injured',], aes(x=Borough, y=value, color=as.factor(Year)))+
  ylab('Number of Pedestrians Injured')+ geom_point(aes(y = value, group = variable, size = Tickets))+
  geom_line(aes(y=value,group=Year))+theme_bw()+
  labs(title='Number of pedestrians injured before and after camera installation', color='Year')+
  theme(axis.text.x=element_text(angle=50, hjust=1), axis.title.x=element_blank())



