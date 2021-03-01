library(plyr)
library(dplyr)
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

#change all "Doctor"s to their corresponding numbered Doctors
for (row in 1:nrow(scriptsData)){
  doc <- scriptsData[row, "details"]
  if (!is.na(doc)){
    if (doc=="DOCTOR"){
      doctor <-c("Doctor")
      doctor <- c(doctor, scriptsData[row, "doctorid"])
      scriptsData[row, "details"]<-toString(doctor)
    }
  }
}
rm(doc); rm(doctor); rm(doctorB); rm(row)

#create non-duplicate list of all characters to create node file for Gephi
charDF = distinct(na.omit(as.data.frame(scriptsData[, "details"])))
id <- rownames(charDF)
charDF <- cbind(id=id, charDF)
write.csv(charDF,"nodes.csv", row.names = FALSE)


#create dateframes for each episode
#group by episode id
#episodeGroups <- scriptsData %>% group_by(episodeid)
#head(episodeGroups)

#split dataframe to episode-dataframes
scriptData_byEpisode <- split(scriptsData, scriptsData$episodeid)

#now we have 145 dataframes with the individual episodes inside

episodesList <- list()
speakersOverall <- c()

#loop through episodes, for each episode do:
for (i in scriptData_byEpisode) {
  
  sceneNum=0
  speakersInScene<-c(i[1, "details"])
  #y=0 #end of scene

  episodeID=i[1, "episodeid"]
  
  df<-data.frame("episodeidCol"=episodeID,"sceneIdCol"=sceneNum, "idxCol"=i[1, "idx"], "nameOfSpeaker"=i[1, "details"])
  
  #loop through all rows of episode i
  for (row in 1:nrow(i)){
    
    #print(i[row, ])
    if (i[row, "type"]=="location") { #increase scene counter
      sceneNum = sceneNum+1
    }
    else { 
      speaker=i[row, "details"]
      if(!(speaker %in% speakersInScene)){
      df = rbind(df, data.frame(episodeidCol=episodeID,sceneIdCol=sceneNum, idxCol=i[row, "idx"], nameOfSpeaker=speaker))
      speakersInScene<-c(speakersInScene, speaker)
      }
      }
  }
  #end of Episode,add df to episodes-list
 episodesList[[length(episodesList)+1]] <-df
 rm(speakersInScene)
}

rm(i);rm(df);rm(row);rm(sceneNum);rm(episodeID);rm(speaker) #remove unused variables

#exchange name of speaker on episodesList with ID from charDF(Vorschlag, verstehe den Fehler nicht)

for (i in episodesList) {
 for (nameOfSpeaker in 1:144) {
   if(nameOfSpeaker == scriptsData[, "details"](charDF)) {
     nameOfSpeaker <- id(charDF)}
   }
  }

# Fehlermeldung: Fehler in scriptsData[, "details"](charDF) : Versuch eine Nicht-Funktion anzuwenden


# TODO: - exchange name of speaker on episodesList with ID from charDF
#       - create combinations of all characters within a scene
#       - create DF with columns: source, target, type (undirected), weight (1?) with source/target for each combination of characters 
#       - create edges.csv from DF
