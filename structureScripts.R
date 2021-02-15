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
episodesList <- list()


#loop through episodes, for each episode do:
for (i in scriptData_byEpisode) {
  
  sceneNum=0
  #y=0 #end of scene

  episodeID=i[1, "episodeid"]
  
  df<-data.frame("episodeidCol"=episodeID,"sceneIdCol"=sceneNum, "idxCol"=i[1, "idx"], "nameOfSpeaker"=i[1, "details"])
  
  #loop through all rows of episode i
  for (row in 1:nrow(i)){
    
    #print(i[row, ])
    if (i[row, "type"]=="location") { #increase scene counter
      sceneNum = sceneNum+1
    }
    else { #Hier liegt das Problem! rbind scheint die neue Zeile der Szene nicht mit dem dataframe df zu binden. 
      rbind(df, data.frame(episodeidCol=episodeID,sceneIdCol=sceneNum, idxCol=i[row, "idx"], nameOfSpeaker=i[row, "details"]))
      }
  }
  #end of Episode,add df to episodes-list
 episodesList[[length(episodesList)+1]] <-df
  
}
