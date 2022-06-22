# load files and data frames
# rm(list = ls()) # to clear workspace
# source("./load-files.R") # un-comment to load files OR comment once files are loaded 
## Duration sessions----------------------------------------------------------------

# BARPLOT - Duration session PARTICIPANTS
ggplot(data = modes.df, aes(x = p, y = durSeconds/60), fill = durSeconds/60) +
  geom_bar(stat="identity") + 
  ggtitle("Durations Sessions") +
  xlab("Participants") + ylab("Time (minutes)") 

# BARPLOT - Duration session PARTICIPANTS
ggplot(data = modes.df, aes(x = p, y = durSeconds/60, width = 0.9, fill = as.character(mode))) +
  geom_bar(stat="identity", alpha=.75) + 
  ggtitle("Durations Sessions") +
  xlab("Participants") + ylab("Time (minutes)") +
  labs(fill = "Mode") +
  theme(legend.position="bottom")

# BARPLOT - MEAN DURATION session and MODE 
ggplot(data = modes.df, aes(x = factor(mode), y = durSeconds/60, fill = as.character(mode))) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Durations Sessions") +
  xlab("Mode") + ylab("Time (minutes)") +
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  labs(fill = "Mode") +
  theme(legend.position="bottom")

## Entries per participant (session) ----------------------------------------------------------------

### BODY - descriptive statistics all participants

# number of entries per participant
pivotB <- body.df %>%
  dplyr::group_by(p, mode) %>%        # grouping variable
  dplyr::summarise(total = length(p)) %>%
  dplyr::group_by(mode) %>%    
  dplyr::summarise(mean = round(mean(total))) %>%
  dplyr::mutate(condition = "body")

# BARPLOT - Basic 
ggplot(data = pivotB, aes(x=mode, y=mean)) +
  geom_bar(stat="identity") + 
  ggtitle("Body Posture Hand - Entries per participant") +
  xlab("Mode") + ylab("Mean")

### HEAD - descriptive statistics all participants

# number of entries per participant
pivotH <- head.df %>%
  dplyr::group_by(p, mode) %>%        # grouping variable
  dplyr::summarise(total = length(p)) %>%
  dplyr::group_by(mode) %>%    
  dplyr::summarise(mean = round(mean(total))) %>%
  dplyr::mutate(condition = "head")

# BARPLOT - Basic 
ggplot(data = pivotH, aes(x=mode, y=mean)) +
  geom_bar(stat="identity") + 
  ggtitle("Head Eye Face - Entries per participant") +
  xlab("Mode") + ylab("Mean")

### SPEECH - descriptive statistics all participants [analysis 2022]

# number of entries per participant
pivotB <- speech.df %>%
  dplyr::group_by(p, mode) %>%        # grouping variable
  dplyr::summarise(total = length(p)) %>%
  dplyr::group_by(mode) %>%    
  dplyr::summarise(mean = round(mean(total))) %>%
  dplyr::mutate(condition = "body")

# BARPLOT - Basic 
ggplot(data = pivotB, aes(x=mode, y=mean)) +
  geom_bar(stat="identity") + 
  ggtitle("Speech - Entries per participant") +
  xlab("Mode") + ylab("Mean")


### BAR PLOT - BODY AND HEAD MEAN ENTRIES
pivotTotal <- rbind(pivotB, pivotH)

# compare BODY vs HEAD
ggplot(data = pivotTotal, aes(x = factor(mode), y = mean, fill = condition)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Body Posture Hand & Head Eye Face - Entries per participant") +
  xlab("Mode") + ylab("Mean") +
  scale_fill_discrete(name = "Condition", labels = c("BodyPostHand", "HeadEyeFace"))

## Frequency items ----------------------------------------------------------------

### BODY - frequency values per column

# frequency table - BODY POSTURE
pivotB <- body.df %>%
  group_by(p, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(bodyPosture), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# frequency table - HAND GESTURE
pivotB <- body.df %>%
  group_by(p, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(handGesture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(handGesture), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Hand Gesture (BPH) - Frequency Items") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


# frequency table - BODY POSTURE and MODE
pivotB <- body.df %>%
  group_by(p, mode, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(mode, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(mode), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Body Posture (BPH) - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


# frequency table - HAND GESTURE & MODE
pivotB <- body.df %>%
  group_by(p, mode, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(mode, handGesture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(mode), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Hand Gesture (BPH) - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### HEAD - frequency values per column [11 DEC]

# frequency table - HEAD EYE
pivotH <- head.df %>%
  group_by(p, headEye) %>%
  summarise(total = length(headEye)) %>%
  na.omit(headEye) %>%
  group_by(headEye) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1))

# frequency figure
ggplot(data = pivotH, aes(x = factor(headEye), y = mean, fill = headEye)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Head Eye Movements (HEF) - Frequency Items") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# frequency table - FACIAL EXPRESSIONS
pivotH <- head.df %>%
  group_by(p, facialExpress) %>%
  summarise(total = length(facialExpress)) %>%
  na.omit(facialExpress) %>%
  group_by(facialExpress) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1))

# frequency figure
ggplot(data = pivotH, aes(x = factor(facialExpress), y = mean, fill = facialExpress)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Facial Expressions (HEF) - Frequency Items") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# frequency table - HEAD EYE and MODE
pivotH <- head.df %>%
  group_by(p, mode, headEye) %>%
  summarise(total = length(headEye)) %>%
  na.omit(headEye) %>%
  group_by(mode, headEye) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotH, aes(x = factor(mode), y = mean, fill = headEye)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Head Eye Movements (HEF) - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

# frequency table - FACIAL EXPRESSIONS and MODE
pivotH <- head.df %>%
  group_by(p, mode, facialExpress) %>%
  summarise(total = length(facialExpress)) %>%
  na.omit(facialExpress) %>%
  group_by(mode, facialExpress) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotH, aes(x = factor(mode), y = mean, fill = facialExpress)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Facial Expressions (HEF) - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

### SPEECH - frequency values per column [analysis 2022]

# frequency table - SPEECH METRICS
pivotB <- speech.df %>%
  group_by(p, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(speechMetrics), y = mean, fill = speechMetrics)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Speech Metrics (SPEECH) - Frequency Items") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


# frequency table - TASK RELATED
pivotB <- speech.df %>%
  group_by(p, taskRelated) %>%
  summarise(total = length(taskRelated)) %>%
  na.omit(taskRelated) %>%
  group_by(taskRelated) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(taskRelated), y = mean, fill = taskRelated)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Task Related (SPEECH) - Frequency Items") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# frequency table - SPEECH METRICS POSTURE and MODE
pivotB <- speech.df %>%
  group_by(p, mode, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(mode, speechMetrics) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(mode), y = mean, fill = speechMetrics)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Speech Metrics (SPEECH) - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


# frequency table - SPEECH METRICS & TASK RELATED
pivotB <- speech.df %>%
  group_by(p, taskRelated, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(taskRelated, speechMetrics) %>% 
  summarise(freq = sum(total))

# frequency figure
ggplot(data = pivotB, aes(x = factor(taskRelated), y = freq, fill = speechMetrics)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Task Related & Speech Metrics (SPEECH) - Frequency Items") +
  xlab("Task Related") + ylab("Frequency") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

## Entries between Items [clothes] x BODY ----------------------------------------------------------------

### identify entries for each of the items

# frequency of items (all participants)

### frequency table - ITEMS x BODY POSTURE
pivotIB <- body.df %>%
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Body Posture - Entries per Item") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))
 

### frequency table - ITEMS x BODY x MODE
pivotIB <- body.df %>%
  group_by(p, mode, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(mode, item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1), item = as.numeric(item)) %>%
  arrange(item) %>%
  mutate(item = as.character(item))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(mode), y = mean, fill = item)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Body Posture - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### make a plot for each one of the modes 

### frequency table - ITEMS x BODY POSTURE
pivotIB <- body.df %>%
  filter(mode == 1) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
m1 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  lims(y = c(0, 8)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Body Posture - Entries per Item - Mode 1") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


### frequency table - ITEMS x BODY POSTURE
pivotIB <- body.df %>%
  filter(mode == 2) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
m2 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  lims(y = c(0, 8)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Body Posture - Entries per Item - Mode 2") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


### frequency table - ITEMS x BODY POSTURE
pivotIB <- body.df %>%
  filter(mode == 3) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
m3 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  lims(y = c(0, 8)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Body Posture - Entries per Item - Mode 3") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

## Entries between Items [clothes] x HEAD ----------------------------------------------------------------

### identify entries for each of the items

# frequency of items (all participants)

### frequency table - ITEMS x HEAD POSTURE
pivotIH <- head.df %>%
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
ggplot(data = pivotIH, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Head Eye Movements - Entries per Item") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


### frequency table -ITEMS x BODY x MODE
pivotIH <- head.df %>%
  group_by(p, mode, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(mode, item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1), item = as.numeric(item)) %>%
  arrange(item) %>%
  mutate(item = as.character(item))

# frequency figure
ggplot(data = pivotIH, aes(x = factor(mode), y = mean, fill = item)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Head Eye Movements - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### make a plot for each one of the modes 

### frequency table - ITEMS x BODY POSTURE
pivotIH <- head.df %>%
  filter(mode == 1) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
m1 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Head Eye Movements - Entries per Item - Mode 1") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


### frequency table - ITEMS x BODY POSTURE
pivotIH <- head.df %>%
  filter(mode == 2) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
m2 <- ggplot(data = pivotIH, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Head Eye Movements - Entries per Item - Mode 2") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


### frequency table - ITEMS x BODY POSTURE
pivotIB <- head.df %>%
  filter(mode == 3) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item)

# frequency figure
m3 <- ggplot(data = pivotIH, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Head Eye Movements - Entries per Item - Mode 3") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

## Entries between Items [clothes] x SPEECH ----------------------------------------------------------------
### identify entries for each of the items

# frequency of items [clothes] (all participants)

### frequency table - ITEMS x SPEECH [ALL ITEMS]
pivotIS <- speech.df %>%
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1)) %>%
  arrange(item) %>%
  subset(item != "2,3")# remove item 2,3 [only appears for one participant]

# frequency figure
ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("SPEECH - Entries per Item [All ITEMS]") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

### frequency table - ITEMS x SPEECH [ONLY NUMBERS]
pivotIS <- speech.df %>%
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  arrange(item) %>%
  subset(item < 13)# remove non numeric items

# frequency figure
ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("SPEECH - Entries per Item [Only NUMERIC]") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

### frequency table - ITEMS x BODY x MODE
pivotIS <- speech.df %>%
  group_by(p, mode, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(mode, item) %>% 
  mutate(item = as.numeric(item)) %>%
  subset(item < 13) %>% # remove non numeric items
  summarise(freq = sum(total)) %>%
  mutate(pMode = case_when(mode == 1 ~ 16,
                           mode > 1 ~ 13)) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1), item = as.numeric(item)) %>%
  arrange(item)  %>%
  mutate(item = as.character(item))

# frequency figure
ggplot(data = pivotIS, aes(x = factor(mode), y = mean, fill = item)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("SPEECH - Frequency Items") +
  xlab("Mode") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

### CONTINUE

### make a plot for each one of the modes 

### frequency table - ITEMS x BODY POSTURE
pivotIS <- speech.df %>%
  filter(mode == 1) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  subset(item < 13) %>% # remove non numeric items
  arrange(item)

# frequency figure
m1 <- ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("SPEECH - Mean entries per Item - Mode 1") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))


### frequency table - ITEMS x BODY POSTURE
pivotIS <- speech.df %>%
  filter(mode == 2) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  subset(item < 13) %>% # remove non numeric items
  arrange(item)

# frequency figure
m2 <- ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("SPEECH - Mean entries per Item - Mode 2") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

### frequency table - ITEMS x BODY POSTURE
pivotIS <- speech.df %>%
  filter(mode == 3) %>% 
  group_by(p, item) %>%
  summarise(total = length(item)) %>%
  na.omit(item) %>%
  group_by(item) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants,1), item = as.numeric(item)) %>%
  subset(item < 13) %>% # remove non numeric items
  arrange(item)

# frequency figure
m3 <- ggplot(data = pivotIS, aes(x = factor(item), y = mean, fill = mean)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("SPEECH - Mean entries per Item - Mode 2") +
  xlab("Items") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1))

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

## Entries between Items x OBJECT x BODY ----------------------------------------------------------------

### BODY POSTURE

### frequency table - ITEMS x BODY POSTURE x OBJECT
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9)) %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants, 1))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")

### MODES

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9) & mode == 1) %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m1 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object - MODE 1") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9) & mode == 2) %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m2 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object -  MODE 2") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9) & mode == 3) %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m3 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object -  MODE 3") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

### HAND GESTURE

### frequency table - ITEMS x HAND GESTURE x OBJECT
pivotIH <- body.df %>%
  filter(item %in% c(1, 5, 9)) %>%
  group_by(p, item, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(item, handGesture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(mean = round(freq/participants, 1))

# frequency figure
ggplot(data = pivotIH, aes(x = factor(item), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Hand Gesture (BPH) - Frequency Items x Object") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### MODES

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9) & mode == 1) %>%
  group_by(p, item, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(item, handGesture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m1 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Hand Gesture (BPH) - Frequency Items x Object - MODE 1") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9) & mode == 2) %>%
  group_by(p, item, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(item, handGesture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m2 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Hand Gesture (BPH) - Frequency Items x Object - MODE 2") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(1, 5, 9) & mode == 3) %>%
  group_by(p, item, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(item, handGesture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m3 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Hand Gesture (BPH) - Frequency Items x Object - MODE 3") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)

## Entries between Items x OBJECT x BODY x MODE [Specific items 5 and 9 for Mode 2]

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




## Survey data ----------------------------------------------------------------

# BARPLOT - Duration session PARTICIPANTS & NAME GIVEN TO THE ROBOT
ggplot(data = surveyDem.df, aes(x = p, y = durSeconds/60, width = 0.9, fill = as.character(mode))) +
  geom_bar(stat="identity", alpha=.75) + 
  ggtitle("Durations Sessions & Name Given to Robot") +
  lims(y = c(0, 20)) +
  xlab("Participants") + ylab("Time (minutes)") +
  labs(fill = "Mode") +
  theme(legend.position="bottom") +
  geom_text(aes(label = nameRobot), nudge_y = 0.5,  angle = 90, size = 2)


### COMPLETING TASK Average

# ALL MODES
pivotSDA <- surveyDem.df %>%
  dplyr::select(completing) %>%
  dplyr::group_by(completing) %>%
  dplyr::summarise(total = length(completing)) %>%
  ungroup() %>% 
  dplyr::mutate(mean = round((total/sum(total))*100, digits = 1))

# completing conditions
pivotSDA$completing <- as.numeric(pivotSDA$completing)
pivotSDA <- complete(pivotSDA, completing = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replacing numeric for character answers
pivotSDR <- replaceAnswer(pivotSDA, 1)

# plot COMPLETING TASK All participants 
ggplot(data = pivotSDR, aes(x = completing, y = mean, fill = completing, color = completing)) +    # print bar chart
  lims(y = c(0, 43)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Completing the task was easy? - ALL PARTICIPANTS") +
  xlab("Evaluation") + ylab("Mean") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
  # + coord_flip()


# for each MODE 
# get df with average for all three modes 
for (i in 1:3){
  # get df for each mode
  pivotSD <- surveyDem.df %>%
    dplyr::filter(mode == i) %>%
    dplyr::select(completing) %>%
    dplyr::group_by(completing) %>%
    dplyr::summarise(total = length(completing)) %>%
    ungroup() %>% 
    dplyr::mutate(mean = round((total/sum(total))*100, digits = 1), mode = i)
  
  # joint df
  if (i == 1){
    pivotCom <- pivotSD
  } else {
    pivotCom <- rbind(pivotCom, pivotSD)
  }
}

# complete pivotCom (add data to empty categories)
pivotCom$completing <- as.numeric(pivotCom$completing)
pivotCom <- complete(pivotCom, mode, completing = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replace numeric for characters answers in pivotCom
for (i in 1:lenAns){
  pivotCom$completing[pivotCom$completing == i] <-  charAns[i]
} 

# PLOT - COMPLETING TASK Average & MODE

# frequency figure
m1 <- pivotCom %>%
  filter(mode == 1) %>%
  ggplot(aes(x = completing, y = mean, fill = completing, color = completing)) +    # print bar chart
  lims(y = c(0, 55)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Completing the task was easy? - MODE 1") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
  # + coord_flip()

# frequency figure
m2 <- pivotCom %>%
  filter(mode == 2) %>%
  ggplot(aes(x = completing, y = mean, fill = completing, color = completing)) +    # print bar chart
  lims(y = c(0, 55)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Completing the task was easy? - MODE 2") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
  # + coord_flip()

# frequency figure
m3 <- pivotCom %>%
  filter(mode == 3) %>%
  ggplot(aes(x = completing, y = mean, fill = completing, color = completing)) +    # print bar chart
  lims(y = c(0, 55)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Completing the task was easy? - MODE 3") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1))
  # + coord_flip()

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)


### ROBOT UNDERSTOOD WHAT I EXPLAINED Average

# ALL MODES
pivotSDA <- surveyDem.df %>%
  dplyr::select(robotUnderstood) %>%
  dplyr::group_by(robotUnderstood) %>%
  dplyr::summarise(total = length(robotUnderstood)) %>%
  ungroup() %>% 
  dplyr::mutate(mean = round((total/sum(total))*100, digits = 1))

# completing conditions
pivotSDA$robotUnderstood <- as.numeric(pivotSDA$robotUnderstood)
pivotSDA <- complete(pivotSDA, robotUnderstood = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replacing numeric for character answers
pivotSDR <- replaceAnswer(pivotSDA, 1)

# plot COMPLETING TASK All participants 
ggplot(data = pivotSDR, aes(x = robotUnderstood, y = mean, fill = robotUnderstood, color = robotUnderstood)) +    # print bar chart
  lims(y = c(0, 43)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Understood - ALL PARTICIPANTS") +
  xlab("Evaluation") + ylab("Mean") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# For each MODE 

# get df with average for all three modes 
for (i in 1:3){
  # get df for each mode
  pivotSD <- surveyDem.df %>%
    dplyr::filter(mode == i) %>%
    dplyr::select(robotUnderstood) %>%
    dplyr::group_by(robotUnderstood) %>%
    dplyr::summarise(total = length(robotUnderstood)) %>%
    ungroup() %>% 
    dplyr::mutate(mean = round((total/sum(total))*100, digits = 1), mode = i)
  
  # joint df
  if (i == 1){
    pivotCom <- pivotSD
  } else {
    pivotCom <- rbind(pivotCom, pivotSD)
  }
}

# complete pivotCom (add data to empty categories)
pivotCom$robotUnderstood <- as.numeric(pivotCom$robotUnderstood)
pivotCom <- complete(pivotCom, mode, robotUnderstood = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replace numeric for characters answers in pivotCom
for (i in 1:lenAns){
  pivotCom$robotUnderstood[pivotCom$robotUnderstood == i] <-  charAns[i]
} 

# PLOT - Robot Understood TASK Average & MODE

# frequency figure
m1 <- pivotCom %>%
  filter(mode == 1) %>%
  ggplot(aes(x = robotUnderstood, y = mean, fill = robotUnderstood, color = robotUnderstood)) +    # print bar chart
  lims(y = c(0, 55)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Understood - MODE 1") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# frequency figure
m2 <- pivotCom %>%
  filter(mode == 2) %>%
  ggplot(aes(x = robotUnderstood, y = mean, fill = robotUnderstood, color = robotUnderstood)) +    # print bar chart
  lims(y = c(0, 55)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Understood - MODE 2") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# frequency figure
m3 <- pivotCom %>%
  filter(mode == 3) %>%
  ggplot(aes(x = robotUnderstood, y = mean, fill = robotUnderstood, color = robotUnderstood)) +    # print bar chart
  lims(y = c(0, 55)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Understood - MODE 3") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1))
# + coord_flip()

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)


### ROBOT REACTIVE Average

# ALL MODES
pivotSDA <- surveyDem.df %>%
  dplyr::select(robotReactive) %>%
  dplyr::group_by(robotReactive) %>%
  dplyr::summarise(total = length(robotReactive)) %>%
  ungroup() %>% 
  dplyr::mutate(mean = round((total/sum(total))*100, digits = 1))

# completing conditions
pivotSDA$robotReactive <- as.numeric(pivotSDA$robotReactive)
pivotSDA <- complete(pivotSDA, robotReactive = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replacing numeric for character answers
pivotSDR <- replaceAnswer(pivotSDA, 1)

# plot COMPLETING TASK All participants 
ggplot(data = pivotSDR, aes(x = robotReactive, y = mean, fill = robotReactive, color = robotReactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Reactive - ALL PARTICIPANTS") +
  xlab("Evaluation") + ylab("Mean") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# For each MODE 

# get df with average for all three modes 
for (i in 1:3){
  # get df for each mode
  pivotSD <- surveyDem.df %>%
    dplyr::filter(mode == i) %>%
    dplyr::select(robotReactive) %>%
    dplyr::group_by(robotReactive) %>%
    dplyr::summarise(total = length(robotReactive)) %>%
    ungroup() %>% 
    dplyr::mutate(mean = round((total/sum(total))*100, digits = 1), mode = i)
  
  # joint df
  if (i == 1){
    pivotCom <- pivotSD
  } else {
    pivotCom <- rbind(pivotCom, pivotSD)
  }
}

# complete pivotCom (add data to empty categories)
pivotCom$robotReactive <- as.numeric(pivotCom$robotReactive)
pivotCom <- complete(pivotCom, mode, robotReactive = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replace numeric for characters answers in pivotCom
for (i in 1:lenAns){
  pivotCom$robotReactive[pivotCom$robotReactive == i] <-  charAns[i]
} 

# PLOT - Robot Understood TASK Average & MODE

# frequency figure
m1 <- pivotCom %>%
  filter(mode == 1) %>%
  ggplot(aes(x = robotReactive, y = mean, fill = robotReactive, color = robotReactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Reactive - MODE 1") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# frequency figure
m2 <- pivotCom %>%
  filter(mode == 2) %>%
  ggplot(aes(x = robotReactive, y = mean, fill = robotReactive, color = robotReactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Reactive - MODE 2") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# frequency figure
m3 <- pivotCom %>%
  filter(mode == 3) %>%
  ggplot(aes(x = robotReactive, y = mean, fill = robotReactive, color = robotReactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Reactive - MODE 3") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1))
# + coord_flip()

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)


### ROBOT PROACTIVE Average

# ALL MODES
pivotSDA <- surveyDem.df %>%
  dplyr::select(robotProactive) %>%
  dplyr::group_by(robotProactive) %>%
  dplyr::summarise(total = length(robotProactive)) %>%
  ungroup() %>% 
  dplyr::mutate(mean = round((total/sum(total))*100, digits = 1))

# completing conditions
pivotSDA$robotProactive <- as.numeric(pivotSDA$robotProactive)
pivotSDA <- complete(pivotSDA, robotProactive = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replacing numeric for character answers
pivotSDR <- replaceAnswer(pivotSDA, 1)

# plot COMPLETING TASK All participants 
ggplot(data = pivotSDR, aes(x = robotProactive, y = mean, fill = robotProactive, color = robotProactive)) +    # print bar chart
  lims(y = c(0, 30)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Proactive - ALL PARTICIPANTS") +
  xlab("Evaluation") + ylab("Mean") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# For each MODE 

# get df with average for all three modes 
for (i in 1:3){
  # get df for each mode
  pivotSD <- surveyDem.df %>%
    dplyr::filter(mode == i) %>%
    dplyr::select(robotProactive) %>%
    dplyr::group_by(robotProactive) %>%
    dplyr::summarise(total = length(robotProactive)) %>%
    ungroup() %>% 
    dplyr::mutate(mean = round((total/sum(total))*100, digits = 1), mode = i)
  
  # joint df
  if (i == 1){
    pivotCom <- pivotSD
  } else {
    pivotCom <- rbind(pivotCom, pivotSD)
  }
}

# complete pivotCom (add data to empty categories)
pivotCom$robotProactive <- as.numeric(pivotCom$robotProactive)
pivotCom <- complete(pivotCom, mode, robotProactive = full_seq(fixAns, period = 1), fill = list(total = 0, mean = 0))

# replace numeric for characters answers in pivotCom
for (i in 1:lenAns){
  pivotCom$robotProactive[pivotCom$robotProactive == i] <-  charAns[i]
} 

# PLOT - Robot Understood TASK Average & MODE

# frequency figure
m1 <- pivotCom %>%
  filter(mode == 1) %>%
  ggplot(aes(x = robotProactive, y = mean, fill = robotProactive, color = robotProactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Proactive - MODE 1") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# frequency figure
m2 <- pivotCom %>%
  filter(mode == 2) %>%
  ggplot(aes(x = robotProactive, y = mean, fill = robotProactive, color = robotProactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Proactive - MODE 2") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 
# + coord_flip()

# frequency figure
m3 <- pivotCom %>%
  filter(mode == 3) %>%
  ggplot(aes(x = robotProactive, y = mean, fill = robotProactive, color = robotProactive)) +    # print bar chart
  lims(y = c(0, 40)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(legend.position = "none")  + 
  ggtitle("Robot Should be More Proactive - MODE 3") +
  xlab("Evaluation") + ylab("Mean") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, hjust=1))
# + coord_flip()

# put plots together 
ggarrange(m1, m2, m3, 
          labels = c("1", "2", "3"),
          ncol = 1, nrow = 3)
## Demographics data ----------------------------------------------------------------

install.packages("psych")
library(psych)

# descriptive statistics 

### AGE
# age - ALL
describe(surveyDem.df$age)

# age - MODE
for (i in 1:3){
  
  # filter data frame
  pivotD <- surveyDem.df %>%
    dplyr::filter(mode == i)
  
  # age by mode
  print(paste("MODE = ", i)) 
  print(describe(pivotD$age))
}

# ANOVA - AGE x MODE

# compute the analysis of variance
res.aov <- aov(age ~ mode, data = surveyDem.df)
# summary of the analysis
summary(res.aov)

# GENDER

# Frequency ALL

# Frequency MODE


## Integration of data types test ----------------------------------------------------------------

## body posture

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(2, 5) & p == 119) %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) # %>% 
  # summarise(freq = sum(total)) %>%
  # mutate(pMode = 16) %>% # Fix the number of participants per mode 
  # mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = total, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object - P119") +
  xlab("Item") + ylab("Frequency") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Metrics")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  filter(item %in% c(2, 5) & p == 119) %>%
  group_by(p, item, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(item, handGesture) # %>% 
# summarise(freq = sum(total)) %>%
# mutate(pMode = 16) %>% # Fix the number of participants per mode 
# mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = total, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Hand Gesture (BPH) - Frequency Items x Object - P119") +
  xlab("Item") + ylab("Frequency") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Metrics")


# need to complete table [add zeroes if items have no entries]

### frequency table - ITEMS x HEAD EYE FACIAL x OBJECT x MODES
pivotIB <- head.df %>%
  filter(item %in% c(2, 5) & p == 119) %>%
  group_by(p, item, headEye) %>%
  summarise(total = length(headEye)) %>%
  na.omit(headEye) %>%
  group_by(item, headEye) # %>% 
# summarise(freq = sum(total)) %>%
# mutate(pMode = 16) %>% # Fix the number of participants per mode 
# mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = total, fill = headEye)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Head Eye Movements (HEF) - Frequency Items x Object - P119") +
  xlab("Item") + ylab("Frequency") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Metrics")


### frequency table - ITEMS x HEAD EYE FACIAL x OBJECT x MODES
pivotIB <- head.df %>%
  filter(item %in% c(2, 5) & p == 119) %>%
  group_by(p, item, facialExpress) %>%
  summarise(total = length(facialExpress)) %>%
  na.omit(facialExpress) %>%
  group_by(item, facialExpress) # %>% 
# summarise(freq = sum(total)) %>%
# mutate(pMode = 16) %>% # Fix the number of participants per mode 
# mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = total, fill = facialExpress)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Facial Expressions (HEF) - Frequency Items x Object - P119") +
  xlab("Item") + ylab("Frequency") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Metrics")


### frequency table - ITEMS x SPEECH METRICS x OBJECT x MODES
pivotIS <- speech.df %>%
  filter(item %in% c(2, 5) & p == 119) %>%
  group_by(p, item, speechMetrics) %>%
  summarise(total = length(speechMetrics)) %>%
  na.omit(speechMetrics) %>%
  group_by(item, speechMetrics) # %>% 
  # summarise(freq = sum(total)) %>%
  # mutate(mean = round(freq/participants, 1))

# frequency figure
ggplot(data = pivotIS, aes(x = factor(item), y = total, fill = speechMetrics)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle("Speech Metrics (SPEECH) - Frequency Items x Object") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


# word frequency - ITEM 5
# get vector to feed wordCloud function
pv1 <- speech.df %>%
  filter(item == 5 & p == 119)
pvText1 <- pv1$speech
# call wordCloud function
wordCl1 <- wordFreq(pvText1) # return elements from wordCloud function
# get frequency table term-document matrix 
df1 <- wordCl1[[2]] # frequency table
# remove word 'laugh'
df1 <- df1[!(df1$word == "laugh"), ]
dtm1 <- wordCl1[[1]] # matrix

df_I5 <- df1
wordcloud(words = df_I5$word, freq = df_I5$freq, min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = wes_palette("Zissou1", 8, type = "continuous"),
          scale=c(2.5, 0.5))

#frequency table 
m1 <- ggplot(data = df_I5[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 30)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words (SPEECH) - Item 5 x P119") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# word frequency - ITEM 2
# get vector to feed wordCloud function
pv1 <- speech.df %>%
  filter(item == 2 & p == 119)
pvText1 <- pv1$speech
# call wordCloud function
wordCl1 <- wordFreq(pvText1) # return elements from wordCloud function
# get frequency table term-document matrix 
df1 <- wordCl1[[2]] # frequency table
# remove word 'laugh'
df1 <- df1[!(df1$word == "laugh"), ]
dtm1 <- wordCl1[[1]] # matrix

df_I2 <- df1
wordcloud(words = df_I2$word, freq = df_I2$freq, min.freq = 1, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = wes_palette("Zissou1", 8, type = "continuous"),
          scale=c(2.5, 0.5))

#frequency table 
m2 <- ggplot(data = df_I2[1:10, ], aes(x=reorder(word, -freq), y=freq, fill=freq)) +
  lims(y = c(0, 30)) +
  geom_bar(stat="identity") + 
  ggtitle("Most frequent words (SPEECH) - Item 2 x P119") +
  xlab("Mode") + ylab("Frequency") +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

# put plots together 
ggarrange(m2, m1,
          labels = c("1", "2"),
          ncol = 1, nrow = 2)

# duration Items
durItems <- items.df %>%
  filter(itemBetween %in% c(2, 5) & p == 119) %>%
  mutate(dur = (end - start))
durTest <- strptime(durItems$dur, format='%H:%M:%S')
timSec <- durTest$hour * 3600 + durTest$min * 60 + durTest$sec
durItems$dur[1] <- timSec[1]
durItems$dur[2] <- timSec[2]

# BARPLOT - Duration session PARTICIPANTS
ggplot(data = durItems, aes(x = itemBetween, y = dur/60, fill = itemBetween)) +
  geom_bar(stat="identity") + 
  ggtitle("Durations items") +
  theme(legend.position = "none")  + 
  xlab("Items") + ylab("Time (minutes)")


# duration Items - ALL PARTICIPANTS t-test
durI <- items.df %>%
  filter(itemBetween %in% c(2, 5), mode >1) %>%
  mutate(dur = (end - start)) %>%
  select(p, mode, dur, itemBetween) %>% 
  group_by(p, mode, itemBetween) %>% 
  summarise_all(.funs = sum, na.rm=T)
durIA <- strptime(durI$dur, format='%H:%M:%S')
tSecA <- durIA$hour * 3600 + durIA$min * 60 + durIA$sec

# replace data in data frame 
len <- length(tSecA)
for (i in 1:len){
  durI$dur[i] <- tSecA[i]  
}
durI2 <- durI %>%
  filter(itemBetween == 2)
durI5 <- durI %>%
  filter(itemBetween == 5)

# t test
t.test(durI2$dur, durI5$dur, paired = TRUE, alternative = "two.sided")
describe(durI2)


# Create a box-plot
bxp <- ggboxplot(
  durI, x = "itemBetween", y = "dur", 
  ylab = "Duration (seconds)", xlab = "Items", add = "jitter"
)


test <- durI %>%
  group_by(itemBetween) %>%
  summarize(mean_length = mean(dur),
            sd_length = sd(dur))