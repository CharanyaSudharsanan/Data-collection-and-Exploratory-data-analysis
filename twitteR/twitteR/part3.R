library(twitteR)
library(ggmap)
library(RJSONIO)
library(stringr)
library(sp)
library(maptools)
library(ggplot2)
library(maps)
library(plyr)

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

print("Group Members")
print("Prachi Jaydeep Shah (pshah8)")
print("Charanya Sudharsanan (csudhars)")

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


api_key <- "K5Pko61GE8SNrFGFJBw7OBiHD"
api_secret <- "qyKaZ8OBhJCEVgBXrbAhD9ci4UMe14AysjRMIHGln4tmtQpaLS"
token <- "1546700100-54n8lyF0jGWnOP9BxcC3oJMWF0KXQq00Xtf7zl8"
token_secret <- "wFORy3PkM4CLxMMonehjxhUBvHveMyTk0UfrcFGwbeorT"

setup_twitter_oauth(api_key, api_secret, token, token_secret)

flu_keywords <- "fluview OR #flu OR #influenza OR influenza OR influenza virus OR seasonal flu OR seasonal influenza OR #fluviruz OR #influenzavirus OR #fluepidemic OR #influenzaepidemic"
tweets <- searchTwitter(flu_keywords, n = 2000, lang = "en")
tweets.df <-twListToDF(tweets)
#write.csv(tweets.df, "Desktoptweets3.csv")

userInfo <- lookupUsers(tweets.df$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo) 

#write.csv(userFrame,"userData.csv")
userFrame <- read.csv("UserData.csv", header =T)
locatedUsers <- !(userFrame$location == "") & !is.na(userFrame$location)

locations <- geocode(as.character(userFrame$location[locatedUsers]))

filteredLoc <- subset.data.frame(locations, lon >= -161.75583 & lon <= -68.01197 & lat >= 19.50139 & lat <= 64.85694)
states <- latlong2state(filteredLoc)

filteredLoc['stateName'] <- states
filtered <- filteredLoc[!(filteredLoc$stateName == "district of columbia"),]
filtered <- subset(filtered, !is.na(filtered$stateName))

stateCount <- count(filtered, "stateName") #FOR MACOS
#stateCount <- as.data.frame(table(filtered$stateName)) #FOR WINDOWS

stateCount$region <- tolower(stateCount$stateName) #FOR MACOS
#stateCount$region <- tolower(stateCount$Var1) #FOR WINDOWS


states <- map_data("state")
map.df <- merge(states,stateCount, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=freq))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

# for windows
#ggplot(map.df, aes(x=long,y=lat,group=group))+
# geom_polygon(aes(fill=Freq))+
#geom_path()+ 
#scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
#coord_map()
#

