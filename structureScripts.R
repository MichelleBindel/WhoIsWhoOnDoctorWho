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

#remove lines that are neither talk nor location

scriptsData <- subset(scriptsData, type=="talk" | type=="location")




#compress speakers like Rose and Rose [OC] and Rose [on TV] to only one node of "ROSE"
#change all "Doctor"s to their corresponding numbered Doctors
for (row in 1:nrow(scriptsData)){
  doc <- scriptsData[row, "details"]
  if (!is.na(doc)){
    doc <- str_remove_all(toString(doc), ' \\[.+\\]| \\[.+\\}|\\+|\\d+|\\(.+\\)')
    scriptsData[row, "details"]<-doc
    if (doc=="DOCTOR"){
      doctor <- c("DOCTOR", scriptsData[row, "doctorid"])
      scriptsData[row, "details"]<-toString(doctor)
    }
  }
}
rm(doc); rm(doctor); rm(row)

#create non-duplicate list of all characters to create node file for Gephi
charDF = distinct(na.omit(as.data.frame(scriptsData[, "details"])))
id <- rownames(charDF)
charDF <- cbind(id=id, charDF)
write.csv(charDF,"nodes.csv", row.names = FALSE)


#exchange speaker names with IDs through a lookup table (named vector)
getSpeakerID <- charDF$id
names(getSpeakerID) <- charDF$details

for (row in 1:nrow(scriptsData)) {
  IDfromLUT <- scriptsData[row, "details"]
  scriptsData[row, "details"] <- getSpeakerID[toString(IDfromLUT)]
}

rm(IDfromLUT); rm(id); rm(getSpeakerID); rm(row)

#split dataframe to episode-dataframes
scriptData_byEpisode <- split(scriptsData, scriptsData$episodeid)

#now we have 145 dataframes with the individual episodes inside

episodesList <- list()

#loop through episodes, for each episode do:
for (i in scriptData_byEpisode) {
  
  sceneNum=0
  speakersInScene<-c(i[1, "details"])

  episodeID=i[1, "episodeid"]
  
  df<-data.frame("episodeidCol"=episodeID,"sceneIdCol"=sceneNum, "idxCol"=i[1, "idx"], "nameOfSpeaker"=i[1, "details"])
  
  #loop through all rows of episode i
  for (row in 1:nrow(i)){
    speakersInScene <- c()
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

rm(i);rm(df);rm(row);rm(sceneNum);rm(episodeID);rm(speaker); #remove unused variables
combinations <- data.frame("Var1"=0, "Var2"=0)


# loop all scenes and create combination of all characters within one scene, creating all edges needed for the edge.csv
for (i in episodesList){
  
  x <- split(i, i$sceneIdCol)

  for (j in x) {
    df <- expand.grid(j$details, j$details)
    df <- df[!duplicated(df), ]
    combinations <- rbind(combinations, df)
    
  }
  
}

rm(df); rm(i); rm(j); rm(x)

combinations <- combinations[order(combinations$Var1),]
combinations <- aggregate(cbind(combinations[0],"weight"=1), combinations, length) # count how often combinations are present
combinations <- combinations[combinations$weight > 5, ]
names(combinations)[1] <- "source"
names(combinations)[2] <- "target"

type <- rep("undirected", nrow(combinations))
combinations <- cbind(combinations, type)
write.csv(combinations,"edges.csv", row.names = FALSE)

