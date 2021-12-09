# ================ [0.0] load libraries ================ 
# install.packages("tidyverse")
# install.packages("magrittr") 
# install.packages("dplyr")
# install.packages("chron")
# install.packages("ggplot2")

library(readxl)
library(magrittr) 
library(dplyr)
library(chron)
library(ggplot2)

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


# set global participant variable
participants <- length(file.list)


# initialize empty lists objects
items.list <- vector("list", length(file.list))
head.list <- vector("list", length(file.list))
body.list <- vector("list", length(file.list))

# assign files to a list
for (i in 1:length(file.list)){
  items.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Items"))
  head.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Head Eye Face"))
  body.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = "Body Posture Hand"))
}

# store lists into a data structure
df.lists <- list(items.list, head.list, body.list)
lists_names <- c("items", "head", "body")

# select columns from data frames
for (l in 1:length(df.lists)){
  
  # select df
  this.list <- df.lists[[l]]
  
  for (p in 1:participants){
    
    # add participant column 
    this.list[[p]]$p <- rep(p+100, length(this.list[[p]][[1]]))
    # add mode column
    this.list[[p]]$mode <- modes.df$mode[match(this.list[[p]]$p, modes.df$p)]
  }
  
  # indicate the list
  if (l == 1){
    items.list <- this.list
  } else if (l == 2){
    head.list <- this.list
  }  else if (l == 3) {
    body.list <- this.list
  }
}

# create concatenated df of reg and new
items.df <- do.call("rbind", items.list)
head.df <- do.call("rbind", head.list)
body.df <- do.call("rbind", body.list)

# fix time columns (transform excel to r format)
for (i in 1:2){
  # for items df
  items.df[[i]] <- as.POSIXct(items.df[[i]], format = "%Y-%m-%d %H:%M")
  items.df[[i]] <- times(strftime(items.df[[i]], format="%H:%M:%S"))
  
  # for head df
  head.df[[i]] <- as.POSIXct(head.df[[i]], format = "%Y-%m-%d %H:%M")
  head.df[[i]] <- times(strftime(head.df[[i]], format="%H:%M:%S"))
  
  # for body df
  body.df[[i]] <- as.POSIXct(body.df[[i]], format = "%Y-%m-%d %H:%M")
  body.df[[i]] <- times(strftime(body.df[[i]], format="%H:%M:%S"))
}

# rename columns data frame (shorter names)
names(body.df)[1:7] <- c("start", "end", "bodyPosture", "handGesture", "postureObject", "gestureObject", "comments")
names(head.df)[1:5] <- c("start", "end", "headEye", "facialExpress", "comments")
names(items.df)[1:4] <- c("start", "end", "itemBetween", "comments")

# uppercase values data frame (fix values in lowercase)
library(tidyverse)
str_sub(body.df$bodyPosture, 1, 1) <- str_sub(body.df$bodyPosture, 1, 1) %>% str_to_upper()
str_sub(body.df$handGesture, 1, 1) <- str_sub(body.df$handGesture, 1, 1) %>% str_to_upper()

# set the directory back to the script source
setwd("C:/Users/lpxjgb/OneDrive - The University of Nottingham/Desktop/___iVideos/analysis-interactions")