#Inporting the cab_ride table
cab_ride <- read.csv("C:/Users/family/Desktop/Assignments/ALY 6040/Week2/Data set/uber&lyft/cab_ride.csv")
#coverting timestam into datetime formate
cab_ride$datetime <- as_datetime(cab_ride$time_stamp/1000, origin ="1970-02-07 09:05:10" )
#cab_ride$time_stamp<- as.POSIXct((cab_ride$time_stamp/1000), origin="1970-02-07")
str(cab_ride)

#making a column with values source + date + hour
#which will be used for merging the tables
cab_ride$merge_col <- paste(as.character(cab_ride$source), as.character(as.Date(cab_ride$datetime)),
                            as.character(hour(cab_ride$datetime)), sep = ' - ')


#Importing the weather table
weather <- read.csv("C:/Users/family/Desktop/Assignments/ALY 6040/Week2/Data set/uber&lyft/weather.csv")
#converting timestam into datetime formate
weather$datetime <- as_datetime(weather$time_stamp)
#weather$time_stamp<- as.POSIXct(weather$time_stamp, origin="1970-01-01")
str(weather)

#making a column with values location + date + hour
weather$merge_col <- paste(as.character(weather$location), as.character(as.Date(weather$datetime)),
                           as.character(hour(weather$datetime)), sep = ' - ')



#merging cab_ride and weather table
#both methods deliver the rsult
merged_df <- merge(cab_ride, weather, by = "merge_col")
#merged_df <- join(cab_ride, weather, by= "merge_col", type="inner") 