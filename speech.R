### SCRIPT TO ANALYSE SPEECH 

# load files and data frames
# setwd("C:/Users/lpxjgb/OneDrive - The University of Nottingham/Desktop/___iVideos/analysis-interactions") # set working directory to load files
# rm(list = ls()) # to clear workspace
# source("./load-files.R") # un-comment to load files OR comment once files are loaded 

## Word count (error vs no error) ----------------------------------------------------------------

### COUNTING THE MOST FREQUENT WORDS
## frequency df - No Error
# get vector to feed wordCloud function
pNoError <- speech.df %>%
  #filter(error == 0 & mode > 1) # only considering mode 2 and 3 [errors happen]
  filter(error == 0 & mode > 1, speaker == "P") # only considering mode 2 and 3 [errors happen]
pNoError <- pNoError$speech
# call wordCloud function
wordClNoE <- wordFreq(pNoError) # return elements from wordCloud function
# get frequency table term-document matrix 
dfNoE <- wordClNoE[[2]] # frequency table
dtmNoE <- wordClNoE[[1]] # matrix

## frequency df - Error
# get vector to feed wordCloud function
pError <- speech.df %>%
  #filter(error == 1)
  filter(error == 1, speaker == "P") # only considering mode 2 and 3 [errors happen]
pError <- pError$speech
# call wordCloud function
wordClE <- wordFreq(pError) # return elements from wordCloud function
# get frequency table term-document matrix 
dfE <- wordClE[[2]] # frequency table
dtmE <- wordClE[[1]] # matrix

# frequency words

# define number of items for both No Error and Error
itemsCount <- (participants-16) * 6

# No Error
head(dfNoE, 10) # most frequent words 
sum(dfNoE$freq) # number of words
avWNoE <- ceiling(sum(dfNoE$freq)/ itemsCount) # average number of words per item

# Error
head(dfE, 10) # most frequent words 
sum(dfE$freq) # number of words
avWE <- ceiling(sum(dfE$freq)/ itemsCount) # average number of words per item

### test number of words per item error and no error 

# get number of words per cell (line)
speech.df$nWords <- str_count(speech.df$speech, '\\w+')

### COUNTING THE NUMBER OF WORDS 
# create df number of words per item & mode
pWordCount <- speech.df %>%
  filter(mode > 1) %>%
  select(p, mode, item, error, nWords) %>%
  filter(item %in% c(1:12)) %>%
  group_by(p, mode, item, error) %>% 
  summarise(count = sum(nWords))
pWordCount$item <- as.numeric(pWordCount$item)

# filter data 

# df NO ERROR sum of all items per participant 
pNoError <- pWordCount %>%
  filter(error == 0) %>%
  group_by(p, mode) %>%
  summarise(total = sum(count))

# df NO ERROR sum of all items per participant 
pError <- pWordCount %>%
  filter(error == 1) %>%
  group_by(p, mode) %>%
  summarise(total = sum(count))

# paired t-test
# summary by participant 
t.test(pError$total, pNoError$total, paired = TRUE, alternative = "two.sided")


# paired t-test
# summary by item
pWNoE <- pWordCount %>%
  filter(error == 0)
pWCE <- pWordCount %>%
  filter(error == 1)

t.test(pWCE$count, pWNoE$count, paired = FALSE, alternative = "two.sided")


## Duration (error vs no error) ----------------------------------------------------------------

### DURATION PER ITEM
speech.df = subset(speech.df, select = -c(comments) )


# get duration for each line of conversation
speech.df$duration <- speech.df$end - speech.df$start


# create df duration per item & mode
pDuration <- speech.df %>%
  filter(mode > 1) %>%
  select(p, mode, item, error, duration) %>%
  filter(item %in% c(1:12)) %>%
  group_by(p, mode, item, error) %>% 
  summarise(count = sum(duration))
pDuration$item <- as.numeric(pDuration$item)

# filter data 

#### CONTINUE HERE

# df NO ERROR sum of all items per participant 
pDurNE <- pWordCount %>%
  filter(error == 0) %>%
  group_by(p, mode) %>%
  summarise(total = sum(count))

# df NO ERROR sum of all items per participant 
pError <- pWordCount %>%
  filter(error == 1) %>%
  group_by(p, mode) %>%
  summarise(total = sum(count))

# paired t-test
# summary by participant 
t.test(pError$total, pNoError$total, paired = TRUE, alternative = "two.sided")


# paired t-test
# summary by item
pWNoE <- pWordCount %>%
  filter(error == 0)
pWCE <- pWordCount %>%
  filter(error == 1)

t.test(pWCE$count, pWNoE$count, paired = FALSE, alternative = "two.sided")
