library(tidyverse)

# load all csv files
scriptsData <- read_csv("datasets/all-scripts.csv")

dwguideData <- read_csv("datasets/dwguide.csv")

detailEpisodeData <- read_csv("datasets/all-detailsepisodes.csv")

imdbData <- read_csv("datasets/imdb_details.csv")


#remove scripts with doctors 1-8

scriptsDataNew <- subset(scriptsData, doctorid>8)

#check if only doctors with id 9 and up are left
scriptsDataNew[!duplicated(scriptsDataNew$doctorid), ]


#create dateframes for each episode
#group by episode id
episodeGroups <- scriptsDataNew %>% group_by(episodeid)
head(episodeGroups)

#split dataframe to episode-dataframes
scriptData_byEpisode <- split(episodeGroups, episodeGroups$episodeid)
#now we have 145 dataframes with the individual episodes inside


#split episodes into scences by looping through the episodes list we just created

desired_length <- 145 # 145 episodes...
episodesList <- vector(mode = "list", length = desired_length)
desired_length2 <- 1000 #we have to see how many scenes there are
sceneList <- vector(mode = "list", length = desired_length2) #container for scenes added to episodelist

