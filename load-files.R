# ================ [0.0] load libraries ================ 
# install.packages("tidyverse")
# install.packages("magrittr") 
# install.packages("dplyr")
# install.packages("chron")
# install.packages("ggplot2")
# install.packages("ggpubr")

library(readxl)
library(magrittr) 
library(dplyr)
library(chron)
library(ggplot2)
library(ggpubr)
library(tidyverse)

# ================ [1.0] load files ================ 

# clear work space
rm(list = ls()) 

# set path to file source
home_path = rstudioapi::getActiveDocumentContext()$path 

# set working directory
setwd("C:/Users/lpxjgb/OneDrive - The University of Nottingham/Desktop/___iVideos/analysis-interactions/files")

# set path output
path_output <- ("C:/Users/lpxjgb/OneDrive - The University of Nottingham/Desktop/___iVideos/analysis-interactions/results")

# select files in folder
file.list <- list.files(pattern = "p1[0-9]{2}.xlsx", full.names = TRUE)

# load sample file 
sample.file <- list.files(pattern = "sample", full.names = TRUE)
modes.df <- as.data.frame(read_excel(sample.file, sheet = "modes"))

# frequency modes table
freqMode <- modes.df %>%
  group_by(mode) %>%
  summarise(total = length(p))

# set global participant variable
participants <- length(file.list)
#participants <- 10 # dummy to find the erronous speech data sets


# initialize empty lists objects
items.list <- vector("list", length(file.list))
head.list <- vector("list", length(file.list))
body.list <- vector("list", length(file.list))
speech.list <- vector("list", length(file.list))

# assign files to a list
for (i in 1:length(file.list)){
  items.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Items"))
  head.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Head Eye Face"))
  body.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Body Posture Hand"))
  speech.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Speech"))
}

# store lists into a data structure
df.lists <- list(items.list, head.list, body.list, speech.list)
lists_names <- c("items", "head", "body", "speech")

# select columns from data frames
for (l in 1:length(df.lists)){
  
  # select df
  this.list <- df.lists[[l]]
  
  for (p in 1:participants){
    
    # add participant column (+100 because participants are named from 101)
    this.list[[p]]$p <- rep(p+100, length(this.list[[p]][[1]]))
    # add mode column
    this.list[[p]]$mode <- modes.df$mode[match(this.list[[p]]$p, modes.df$p)]
  }
  
  # indicate the list
  if (l == 1){
    items.list <- this.list
  } else if (l == 2){
    head.list <- this.list
  } else if (l == 3) {
    body.list <- this.list
  } else if (l == 4) {
    speech.list <- this.list
  }
}

# fix speech.list column name (replace "action -verb" for "action - verb")
for (p in 1:participants){
  colnames(speech.list[[p]])[11] <- "Action word - verb"
  colnames(speech.list[[p]])[4] <- "Item OR between items"
  
  # fix p28
  if (p == 28){
    colnames(speech.list[[p]])[c(5,6)] <- colnames(speech.list[[1]])[c(5,6)]
  }  

}

# find elements that do not match between speech.list column names
# used to find the column names that did not match, no longer needed 
# for (p in 1:participants){
#   print(p)
#   print(setdiff(colnames(speech.list[[1]]), colnames(speech.list[[p]])))
#   }

# create concatenated df of reg and new
items.df <- do.call("rbind", items.list)
head.df <- do.call("rbind", head.list)
body.df <- do.call("rbind", body.list)
speech.df <- do.call("rbind", speech.list)

# fix time columns (transform excel to r format)
for (i in 1:2){
  # for items df
  items.df[[i]] <- as.POSIXct(items.df[[i]], format = "%Y-%m-%d %H:%M")
  items.df[[i]] <- times(strftime(items.df[[i]], format="%H:%M:%S"))
  # move positions to the right (i.e. convert hours to minutes and minutes to seconds)
  items.df[[i]] <- gsub(" ", "", paste("00:", as.character(items.df[[i]])))
  items.df[[i]] <- substr(items.df[[i]] , 1, 8)
  items.df[[i]] <- strptime(items.df[[i]], format = "%H:%M:%S")
  items.df[[i]] <- times(strftime(items.df[[i]], format="%H:%M:%S"))
  
  # for head df
  head.df[[i]] <- as.POSIXct(head.df[[i]], format = "%Y-%m-%d %H:%M")
  head.df[[i]] <- times(strftime(head.df[[i]], format="%H:%M:%S"))
  # move positions to the right (i.e. convert hours to minutes and minutes to seconds)
  head.df[[i]] <- gsub(" ", "", paste("00:", as.character(head.df[[i]])))
  head.df[[i]] <- substr(head.df[[i]] , 1, 8)
  head.df[[i]] <- strptime(head.df[[i]], format = "%H:%M:%S")
  head.df[[i]] <- times(strftime(head.df[[i]], format="%H:%M:%S"))
  
  # for body df
  body.df[[i]] <- as.POSIXct(body.df[[i]], format = "%Y-%m-%d %H:%M")
  body.df[[i]] <- times(strftime(body.df[[i]], format="%H:%M:%S"))
  # move positions to the right (i.e. convert hours to minutes and minutes to seconds)
  body.df[[i]] <- gsub(" ", "", paste("00:", as.character(body.df[[i]])))
  body.df[[i]] <- substr(body.df[[i]] , 1, 8)
  body.df[[i]] <- strptime(body.df[[i]], format = "%H:%M:%S")
  body.df[[i]] <- times(strftime(body.df[[i]], format="%H:%M:%S"))
  
  # for speech df
  speech.df[[i]] <- as.POSIXct(speech.df[[i]], format = "%Y-%m-%d %H:%M")
  speech.df[[i]] <- times(strftime(speech.df[[i]], format="%H:%M:%S"))
  # move positions to the right (i.e. convert hours to minutes and minutes to seconds)
  speech.df[[i]] <- gsub(" ", "", paste("00:", as.character(speech.df[[i]])))
  speech.df[[i]] <- substr(speech.df[[i]] , 1, 8)
  speech.df[[i]] <- strptime(speech.df[[i]], format = "%H:%M:%S")
  speech.df[[i]] <- times(strftime(speech.df[[i]], format="%H:%M:%S"))
}

# rename columns data frame (shorter names)
names(body.df)[1:7] <- c("start", "end", "bodyPosture", "handGesture", "postureObject", "gestureObject", "comments")
names(head.df)[1:5] <- c("start", "end", "headEye", "facialExpress", "comments")
names(items.df)[1:4] <- c("start", "end", "itemBetween", "comments")
names(speech.df)[1:12] <- c("start", "end", "speaker", "item", "speech", "speechMetrics", "taskRelated", "object", "direction", "location", "actionWord", "comments") 

# uppercase values data frame (fix values in lowercase)
# library(tidyverse)
str_sub(body.df$bodyPosture, 1, 1) <- str_sub(body.df$bodyPosture, 1, 1) %>% str_to_upper()
str_sub(body.df$handGesture, 1, 1) <- str_sub(body.df$handGesture, 1, 1) %>% str_to_upper()
# [TO DO IT LATER] need to fix conversation values (make data more homogeneous) for speech.df 


### GET duration of session and add that to column in modes.df

# add empty duration column
modes.df['duration'] <- as.times("00:00:00")

# array participants
pId <- as.array(modes.df[[1]])

for (i in pId){
  # filter p in body.df
  bodyId <- body.df %>% 
    filter(p == i)
  # filter p in head.df
  headId <- head.df %>% 
    filter(p == i)
  
  # get smallest start in body
  bodyStart <- (min(bodyId$start))
  # get largest start in body
  bodyEnd <- (max(bodyId$end))
  
  
  # get smallest start in head
  headStart <- (min(headId$start))
  # get largest start in body
  headEnd <- (max(headId$end))
  
  # get smallest Id
  idStart <- min(bodyStart, headStart) 
  # get largest Id
  idEnd <- max(bodyEnd, headEnd) 
  
  # idTime
  duration <- idEnd-idStart
  
  # add duration to modes.df
  modes.df[modes.df$p == i, "duration"] <- as.times(duration)
}
# transform time to seconds 
durTest <- strptime(modes.df$duration, format='%H:%M:%S')
modes.df$durSeconds <- durTest$hour * 3600 + durTest$min * 60 + durTest$sec


# identify and items to body.df entries

# add empty item column to body.df 
body.df$item <- NA
# add empty item column to body.df 
head.df$item <- NA

# function to add identify and add item to body.df entries
for (i in pId){
  
  # filter p in dfs
  pItems <- items.df %>%
    filter(p == i, itemBetween != "Between items")
  
  # iterate over items in filtered items.df
  for (item in pItems$itemBetween){
    # filter item
    pthisItem <- pItems %>%
      filter(itemBetween == item) 
    
    # get start and end time item
    startItem <- pthisItem$start
    endItem <- pthisItem$end
    
    # go through all the start times of an item
    for (j in 1:length(startItem)){
      
      ### add items to body.df 
      
      # get items that meet the condition [start item in time range]
      indStart <- (body.df$start >= startItem[j] &  body.df$start <= endItem[j] &  body.df$p == i)  # change to improve catching items in between times
      # get items that meet the condition [end item in time range]
      indEnd <- (body.df$end <= endItem[j] &  body.df$end >= startItem[j] &  body.df$p == i)  # catch entries END time in between range
      
      # identify index of TRUE cells
      indRepS <- which(indStart %in% TRUE) # start
      indRepE <- which(indEnd %in% TRUE) # end
      indBoth <- c(indRepS, indRepE)
      # add item to body.df
      body.df[indBoth, 10] <- item
      
      
      # add items to head.df 
      # get items that meet the condition [start item in time range]
      indStartH <- (head.df$start >= startItem[j] &  head.df$start <= endItem[j] &  head.df$p == i)  # change to improve catching items in between times
      # get items that meet the condition [end item in time range]
      indEndH <- (head.df$end <= endItem[j] &  head.df$end >= startItem[j] &  head.df$p == i)  # catch entries END time in between range
      
      # identify index of TRUE cells
      indRepSH <- which(indStartH %in% TRUE) # start
      indRepEH <- which(indEndH %in% TRUE) # end
      indBothH <- c(indRepSH, indRepEH)
      # add item to head.df
      head.df[indBothH, 8] <- item
    }
  }
}

# set the directory back to the script source
setwd("C:/Users/lpxjgb/OneDrive - The University of Nottingham/Desktop/___iVideos/analysis-interactions")