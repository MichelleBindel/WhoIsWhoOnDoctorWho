library(tidyverse)

# load all csv files
scriptsData <- read_csv("datasets/all-scripts.csv")

#dwguideData <- read_csv("datasets/dwguide.csv")

#detailEpisodeData <- read_csv("datasets/all-detailsepisodes.csv")

#imdbData <- read_csv("datasets/imdb_details.csv")


#remove scripts with doctors 1-8

scriptsData <- subset(scriptsData, doctorid>8)

#check if only doctors with id 9 and up are left
scriptsData[!duplicated(scriptsData$doctorid), ]

#remove details NA lines

scriptsData <- subset(scriptsData, type=="talk" | type=="location")

#create dateframes for each episode
#group by episode id
#episodeGroups <- scriptsData %>% group_by(episodeid)
#head(episodeGroups)

#split dataframe to episode-dataframes
scriptData_byEpisode <- split(scriptsData, scriptsData$episodeid)

#now we have 145 dataframes with the individual episodes inside

library(plyr)


#loop through episodes, for each episode do:
for (i in scriptData_byEpisode) {
  
  
  #split i (episode) on occurance of type=="location" into different scenes
  
  
}

#split episodes into scences by looping through the episodes list we just created
#desired_length <- 145 # 145 episodes...
#episodesList <- vector(mode = "list", length = desired_length)
#desired_length2 <- 1000 #we have to see how many scenes there are
#sceneList <- vector(mode = "list", length = desired_length2) #container for scenes added to episodelist

