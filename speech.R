### SCRIPT TO ANALYSE SPEECH 

# load files and data frames
# rm(list = ls()) # to clear workspace
# source("./load-files.R") # un-comment to load files OR comment once files are loaded 

## Entries between Items x OBJECT x SPEECH ----------------------------------------------------------------

### SPEECH

### frequency table - ITEMS x BODY POSTURE x OBJECT
pivotIS <- speech.df %>%
  filter(item %in% c(1, 5, 9)) %>%
  group_by(p, item, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(item, speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants, 1))

# frequency figure
ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = speechMetrics)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle("Speech Metrics (SPEECH) - Frequency Items x Object") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

### MODES

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIS <- speech.df %>%
  filter(item %in% c(1, 5, 9) & mode == 1) %>%
  group_by(p, item, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(item, speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m1 <- ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = speechMetrics)) +    # print bar chart
  lims(y = c(0, 10)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Speech Metrics (SPEECH) - Frequency Items x Object - MODE 1") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIS <- speech.df %>%
  filter(item %in% c(1, 5, 9) & mode == 2) %>%
  group_by(p, item, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(item, speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m2 <- ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = speechMetrics)) +    # print bar chart
  lims(y = c(0, 10)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Speech Metrics (SPEECH) - Frequency Items x Object - MODE 2") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIS <- speech.df %>%
  filter(item %in% c(1, 5, 9) & mode == 3) %>%
  group_by(p, item, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(item, speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m3 <- ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = speechMetrics)) +    # print bar chart
  lims(y = c(0, 10)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Speech Metrics (SPEECH) - Frequency Items x Object - MODE 3") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  
  scale_fill_discrete(name = "Items")

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

### MOST COMMON STATEMENTS IN ITEMS 9 BY DIFFERENT MODES 

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIS <- speech.df %>%
  filter(item == 9 & speechMetrics == "Statement" & mode == 1) %>%
  group_by(p, item, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(item, speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

## Word analysis in SPEECH data frame ----------------------------------------------------------------

### frequency words - ALL 

# create a vector containing only the text [for all modes]
pvText <- speech.df$speech

# call wordCloud function
wordCl <- wordFreq(pvText) # return elements from wordCloud function
# get frequency table term-document matrix 
df <- wordCl[[2]] # frequency table
dtm <- wordCl[[1]] # matrix


### frequency words x MODE

## frequency df - MODE 1
# get vector to feed wordCloud function
pv1 <- speech.df %>%
  filter(mode == 1)
pvText1 <- pv1$speech
# call wordCloud function
wordCl1 <- wordFreq(pvText1) # return elements from wordCloud function
# get frequency table term-document matrix 
df1 <- wordCl1[[2]] # frequency table
dtm1 <- wordCl1[[1]] # matrix

## frequency df - MODE 2
# get vector to feed wordCloud function
pv2 <- speech.df %>%
  filter(mode == 2)
pvText2 <- pv2$speech
# call wordCloud function
wordCl2 <- wordFreq(pvText2) # return elements from wordCloud function
# get frequency table term-document matrix 
df2 <- wordCl2[[2]] # frequency table
dtm2 <- wordCl2[[1]] # matrix

## frequency df - MODE 3
# get vector to feed wordCloud function
pv3 <- speech.df %>%
  filter(mode == 3)
pvText3 <- pv3$speech
# call wordCloud function
wordCl3 <- wordFreq(pvText3) # return elements from wordCloud function
# get frequency table term-document matrix 
df3 <- wordCl3[[2]] # frequency table
dtm3 <- wordCl3[[1]] # matrix

### Word association find words associate to a specific word

#ALL 
findAssocs(dtm, terms = "please", corlimit = 0.1)

# MODE 1 
asw1 <- as.data.frame(findAssocs(dtm1, terms = "please", corlimit = 0.1))
asw1$words <- rownames(asw1)
asw1$mode <- 1

# MODE 2
asw2 <- as.data.frame(findAssocs(dtm2, terms = "please", corlimit = 0.1))
asw2$words <- rownames(asw2)
asw2$mode <- 2

# MODE 3
asw3 <- as.data.frame(findAssocs(dtm3, terms = "please", corlimit = 0.1))
asw3$words <- rownames(asw3)
asw3$mode <- 3

# create dataframe with most common word associated to specific word
dfAssoc <- rbind(asw1, asw2, asw3)
row.names(dfAssoc) <- 1:length(dfAssoc[[1]])
names(dfAssoc)[1] <- "corr"
dfAssoc <- dfAssoc[, c(2, 1, 3)]

# print df
grid.table(dfAssoc, theme = ttheme_minimal())


### plot wordClouds

# ALL 
set.seed(1) # set for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = wes_palette("Zissou1", 8, type = "continuous"), scale=c(2.5, 0.25))

# MODE 1
set.seed(1) # set for reproducibility
df <- df1 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = wes_palette("Zissou1", 8, type = "continuous"),
          scale=c(2.5, 0.5))

# MODE 2
set.seed(1) # set for reproducibility
df <- df2 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = wes_palette("Zissou1", 8, type = "continuous"),
          scale=c(2.5, 0.5))

# MODE 3
set.seed(1) # set for reproducibility
df <- df3 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = wes_palette("Zissou1", 8, type = "continuous"),
          scale=c(2.5, 0.5))

## descriptive analysis frequency df

# MODE - 1
head(df1, 10) # most frequent words 
sum(df1$freq) # number of words
avW1 <- ceiling(sum(df1$freq)/16) # average number of words per session

# MODE - 2
head(df2, 10)
sum(df2$freq)
avW2 <- ceiling(sum(df2$freq)/13)

# MODE - 3
head(df3, 10)
sum(df3$freq)
avW3 <- ceiling(sum(df3$freq)/13)

### Barplot average number of words per mode and most common words per mode

# create data frame with average number of words for each mode per session
avgWords <- data.frame (mode = c(1:3),
                        avg = c(avW1, avW2, avW3))

# BARPLOT - Basic 
ggplot(data = avgWords, aes(x=mode, y=avg, fill=mode)) +
  geom_bar(stat="identity") + 
  ggtitle("Number of words (SPEECH) - Per session") +
  xlab("Mode") + ylab("Mean") +
  theme(legend.position="none")

# BARPLOT - MOST COMMON WORDS (TOP 10) - MODE 1
m1 <- ggplot(data = df1[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 450)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - MODE 1") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# BARPLOT - MOST COMMON WORDS (TOP 10) - MODE 2
m2 <- ggplot(data = df2[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 450)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - MODE 2") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# BARPLOT - MOST COMMON WORDS (TOP 10) - MODE 3
m3 <- ggplot(data = df3[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 450)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - MODE 3") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# put plots together 
ggarrange(m1, m2, m3,
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)


## WORD FREQUENCY X SPEECH METRIC

## frequency df - Speech metrics 
# ['Statement']
# get vector to feed wordCloud function
pvSM <- speech.df %>%
  filter(speechMetrics == 'Statement')
pvSM <- pvSM$speech
# call wordCloud function
wordClSM <- wordFreq(pvSM) # return elements from wordCloud function
# get frequency table term-document matrix 
dfSM <- wordClSM[[2]] # frequency table
dtmSM <- wordClSM[[1]] # matrix

# ['Question']
# get vector to feed wordCloud function
pvQ <- speech.df %>%
  filter(speechMetrics == 'Question')
pvQ <- pvQ$speech
# call wordCloud function
wordClQ <- wordFreq(pvQ) # return elements from wordCloud function
# get frequency table term-document matrix 
dfQ <- wordClQ[[2]] # frequency table
dtmQ <- wordClQ[[1]] # matrix

# ['Affirmation']
# get vector to feed wordCloud function
pvAf <- speech.df %>%
  filter(speechMetrics == 'Affirmation')
pvAf <- pvAf$speech
# call wordCloud function
wordClAf <- wordFreq(pvAf) # return elements from wordCloud function
# get frequency table term-document matrix 
dfAf <- wordClAf[[2]] # frequency table
dtmAf <- wordClAf[[1]] # matrix

# BARPLOT - MOST COMMON WORDS (TOP 10) - Statement
m1 <- ggplot(data = dfSM[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 550)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - 'Statement'") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# BARPLOT - MOST COMMON WORDS (TOP 10) - Question
m2 <- ggplot(data = dfQ[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 550)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - 'Question'") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# BARPLOT - MOST COMMON WORDS (TOP 10) - Affirmation
m3 <- ggplot(data = dfAf[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 550)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - 'Affirmation'") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# put plots together 
ggarrange(m1, m2, m3,
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

## WORD FREQUENCY X ITEM

## frequency df - Item

# [1]
# get vector to feed wordCloud function
pvI1 <- speech.df %>%
  filter(item == 1)
pvI1 <- pvI1$speech
# call wordCloud function
wordClI1 <- wordFreq(pvI1) # return elements from wordCloud function
# get frequency table term-document matrix 
df_I1 <- wordClI1[[2]] # frequency table
dtm_I1 <- wordClI1[[1]] # matrix

# [5]
# get vector to feed wordCloud function
pvI5 <- speech.df %>%
  filter(item == 5)
pvI5 <- pvI5$speech
# call wordCloud function
wordClI5 <- wordFreq(pvI5) # return elements from wordCloud function
# get frequency table term-document matrix 
df_I5 <- wordClI5[[2]] # frequency table
dtm_I5 <- wordClI5[[1]] # matrix

# [9]
# get vector to feed wordCloud function
pvI9 <- speech.df %>%
  filter(item == 9)
pvI9 <- pvI9$speech
# call wordCloud function
wordClI9 <- wordFreq(pvI9) # return elements from wordCloud function
# get frequency table term-document matrix 
df_I9 <- wordClI9[[2]] # frequency table
dtm_I9 <- wordClI9[[1]] # matrix


# BARPLOT - MOST COMMON WORDS (TOP 10) - Statement
m1 <- ggplot(data = df_I1[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 150)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - ITEM 1") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# BARPLOT - MOST COMMON WORDS (TOP 10) - Question
m2 <- ggplot(data = df_I5[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 150)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - ITEM 5") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# BARPLOT - MOST COMMON WORDS (TOP 10) - Affirmation
m3 <- ggplot(data = df_I9[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 150)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words all sessions (SPEECH) - ITEM 9") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# put plots together 
ggarrange(m1, m2, m3,
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)








## Word count (error vs no error) ----------------------------------------------------------------

### frequency words x MODE

## frequency df - No Error
# get vector to feed wordCloud function
pNoError <- speech.df %>%
  filter(error == 0)
pNoError <- pNoError$speech
# call wordCloud function
wordClNoE <- wordFreq(pNoError) # return elements from wordCloud function
# get frequency table term-document matrix 
dfNoE <- wordClNoE[[2]] # frequency table
dtmNoE <- wordClNoE[[1]] # matrix

## frequency df - Error
# get vector to feed wordCloud function
pError <- speech.df %>%
  filter(error == 0)
pError <- pError$speech
# call wordCloud function
wordClE <- wordFreq(pError) # return elements from wordCloud function
# get frequency table term-document matrix 
dfE <- wordClE[[2]] # frequency table
dtmE <- wordClE[[1]] # matrix

# frequency words 