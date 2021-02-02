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

