library(ggplot2)
library(map)
library(ggmap)

# Download dataset, if it does not exist.
fileName <- 'mvt.csv';
if (!file.exists(fileName)) {
  # , method="curl"
  download.file(paste0('https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/', fileName), fileName)
}

mvt <- read.csv(fileName, stringsAsFactors=F)
mvt$Date <- strptime(mvt$Date, format='%m/%d/%y %H:%M')
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour

# Show records per day of the week.
table(mvt$Weekday)

WeekdayCounts <- as.data.frame(table(mvt$Weekday))

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

# ggplot plots the data in alphabetical order, but we want it in chronological order. So, add a factor and specify the levels for days.
WeekdayCounts$Var1 <- factor(WeekdayCounts$Var1, ordered=TRUE, levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab('Day of the week') + ylab('Total Motor Vehicle Thefts')

# Draw the line lighter in color.
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab('Day of the week') + ylab('Total Motor Vehicle Thefts')

# Plot crimes per hour and day of week.
DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
DayHourCounts <- NULL
DayHourCounts <- cbind(mvt$Weekday, mvt$Hour)
DayHourCounts <- as.data.frame(DayHourCounts)

ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

# Plot a heatmap for each hour and each day of the week.
# We can see a lot of crime happens at night (0 oclock), especially on weekends.
DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered=TRUE, levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name='Total MV Thefts', low='white', high='red') + theme(axis.title.y = element_blank())

# Plot crime on a map.
chicago <- get_map(location = 'chicago', zoom=11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))

LatLonCounts <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))

# Plot crime as a heatmap on top of the map.
###
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + 
  scale_color_gradient(low='yellow', high='red')

# Plot crime as an alpha-blended heatmap on top of the map, in red for hotspots of crime.
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill='red')

# Remove observations with a frequency of 0 (so water will not get plotted on).
LatLonCounts2 <- LatLonCounts[LatLonCounts$Freq > 0,]
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill='red')

# We've removed 952 rows.
nrow(LatLonCounts) - nrow(LatLonCounts2)
