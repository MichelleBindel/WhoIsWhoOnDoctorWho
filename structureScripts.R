library(plyr)
library(dplyr)
library(tidyverse)

# load all csv files
scriptsData <- read_csv("datasets/all-scripts.csv")

#dwguideData <- read_csv("datasets/dwguide.csv")

#detailEpisodeData <- read_csv("datasets/all-detailsepisodes.csv")

imdbData <- read_csv("datasets/imdb_details.csv")

# normalize season number according to script data
imdbData$season[imdbData$season == 1] <- 27
imdbData$season[imdbData$season == 2] <- 28
imdbData$season[imdbData$season == 3] <- 29
imdbData$season[imdbData$season == 4] <- 30
imdbData$season[imdbData$season == 5] <- 31
imdbData$season[imdbData$season == 6] <- 32
imdbData$season[imdbData$season == 7] <- 33
imdbData$season[imdbData$season == 8] <- 34
imdbData$season[imdbData$season == 9] <- 35
imdbData$season[imdbData$season == 10] <- 36
imdbData$season[imdbData$season == 11] <- 37

imdbData <- unite(imdbData, episode, c(season, number), remove=TRUE, sep = "-")

imdbData = subset(imdbData, select = -c(title, nbr_votes, description))


#remove scripts with doctors 1-8

scriptsData <- subset(scriptsData, doctorid>8)

#check if only doctors with id 9 and up are left
scriptsData[!duplicated(scriptsData$doctorid), ]

scriptsData2 <- scriptsData[!duplicated(scriptsData$episodeid), ]


scriptsData2 <- scriptsData2[ scriptsData2$episodeid %in% imdbData$episode, ]
scriptsData <- scriptsData[ scriptsData$episodeid %in% imdbData$episode, ]

imdbData <- imdbData[ imdbData$episode %in% scriptsData2$episodeid, ]

rm(scriptsData2)

#scale ratings to a scale of 0 to 1
imdbData["rating"] <- (imdbData["rating"]-min(imdbData["rating"]))/(max(imdbData["rating"])-min(imdbData["rating"]))


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

names(imdbData)[1] <- "episodeid"
#double ratings so that low rated episodes lower the weight of the character combination and high rated episodes increase the weight
imdbData$rating <- imdbData$rating * 2


#add rating to scriptsData
scriptsData <- merge(scriptsData[, c("idx", "text", "type", "details", "episodeid", "doctorid")], imdbData, by="episodeid")

#create non-duplicate list of all characters to create node file for Gephi
charDF = distinct(na.omit(as.data.frame(scriptsData[, "details"])))
id <- rownames(charDF)
charDF <- cbind(id=id, charDF)
names(charDF)[2]<-paste("details")  # works
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
  
  df<-data.frame("episodeidCol"=episodeID,"sceneIdCol"=sceneNum, "idxCol"=i[1, "idx"], "nameOfSpeaker"=i[1, "details"], "rating"=i[1, "rating"])
  
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
        df = rbind(df, data.frame(episodeidCol=episodeID,sceneIdCol=sceneNum, idxCol=i[row, "idx"], nameOfSpeaker=speaker, rating=i[row, "rating"]))
        speakersInScene<-c(speakersInScene, speaker)
      }
    }
  }
  #end of Episode,add df to episodes-list
  episodesList[[length(episodesList)+1]] <-df
  rm(speakersInScene)
}

rm(i);rm(df);rm(row);rm(sceneNum);rm(episodeID);rm(speaker); #remove unused variables
rm(scriptData_byEpisode); rm(scriptsData)

combinations <- data.frame("Var1"=0, "Var2"=0)
ratingsOverview <- data.frame("endOfRating" = 0, "rating" = 0)

# loop all scenes and create combination of all characters within one scene, creaing all edges needed for the edge.csv
for (i in episodesList){ # loop episodes
  
  x <- split(i, i$sceneIdCol)
  
  for (j in x) { # loop scenes j of episode x
    df <- expand.grid(j$nameOfSpeaker, j$nameOfSpeaker)
    df <- df[!duplicated(df), ]
    combinations <- rbind(combinations, df)
    rm(df); rm(rating)
    
  }
  #save the ratings of the episodes for the combinations list
  ratingsOverview[nrow(ratingsOverview) +1,] <- c(nrow(combinations), j[nrow(j), ]$rating)
}

rm(df); rm(i); rm(j); rm(x); rm(rating);# rm(imdbData)

#combinations <- combinations[order(combinations$Var1),]
combinationsRating <- combinations
vectorOfRatings <- c(0)

#vectorOfRatings <- c(vectorOfRatings, rep(ratingsOverview[2, 2], ratingsOverview[2, 1]))
#vectorOfRatings <- c(vectorOfRatings, rep(ratingsOverview[3, 2], ratingsOverview[3, 1]-ratingsOverview[2, 1]))

row2 = ratingsOverview[1,]

for (i in 1:(nrow(ratingsOverview))) {
  
  row <- ratingsOverview[i,]
  vectorOfRatings <- c(vectorOfRatings, rep(row[[2]], (row[[1]]-row2[[1]])))
  row2<-row
}
rm(row); rm(row2)

vectorOfRatings = vectorOfRatings[-1]
combinationsRating <- cbind(combinationsRating, "ratings"=vectorOfRatings)

combinationsRating2 <- combinationsRating %>% group_by(Var1, Var2) %>% summarise(weight=sum(ratings))
combinationsRating2 = combinationsRating2[-1, ]
combinationsRating2 <- combinationsRating2[combinationsRating2$weight > 5,]
names(combinationsRating2)[1] <- "source"
names(combinationsRating2)[2] <- "target"

#combinationsWithout2 <- subset(combinations, select = -c(rating))
#combinationsWithout2 <- ddply(combinationsWithout2,.(Var1,Var2),nrow)
#combinationsWithout2 <- combinationsWithout2[combinationsWithout2$V1 > 5, ]

#combinationsWithout <- subset(combinations, select = -c(rating))
#591: 1-1 / 12693 obs ->
#combinationsWithout <- aggregate(cbind(combinationsWithout[0],"weight"=1), combinationsWithout, length) # count how often combinations are present
#combinationsWithout <- combinationsWithout[combinationsWithout$weight > 5, ]




# edges list without Ratings, just combined number of occurences
combinations <- aggregate(cbind(combinations[0],"weight"=1), combinations, length) # count how often combinations are present
combinations <- combinations[combinations$weight > 5, ]
names(combinations)[1] <- "source"
names(combinations)[2] <- "target"

type <- rep("undirected", nrow(combinations))
combinations <- cbind(combinations, type)
type <- rep("undirected", nrow(combinationsRating2))
combinationsRating2 <- cbind(combinationsRating2, type)
names(combinationsRating2)[4] <- "type"

write.csv(combinations,"edges.csv", row.names = FALSE)
write.csv(combinationsRating2,"edgesWithRatings.csv", row.names = FALSE)
