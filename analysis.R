# load files and data frames
# source("./load-files.R")
## Duration sessions----------------------------------------------------------------

# BARPLOT - Duration session
ggplot(data = modes.df, aes(x = p, y = durSeconds/60), fill = durSeconds/60) +
  geom_bar(stat="identity") + 
  ggtitle("Durations Sessions") +
  xlab("Participants") + ylab("Time (minutes)") 
  #test

# BARPLOT - DURATION session and MODE 
ggplot(data = modes.df, aes(x = factor(mode), y = durSeconds/60), fill = p) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ggtitle("Durations Sessions") +
  xlab("Mode") + ylab("Time (minutes)") +
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) 

## Entries per participant ----------------------------------------------------------------

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

## Entries between Items x BODY ----------------------------------------------------------------

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
 

### frequency table -ITEMS x BODY x MODE
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

## Entries between Items x HEAD ----------------------------------------------------------------

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

## Entries between Items x OBJECT x BODY x MODE [Specific items 5 and 9 for Mode 2] ----------------------------------------------------------------

### BODY POSTURE [MODE 2]

pivotIB <- body.df %>%
  filter(item %in% c(5, 9) & mode == 2 & bodyPosture != "Other body movement") %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object -  MODE 2") +
  xlab("Item(Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### BODY POSTURE [OBJECT 5]

### MODE EFFECT

### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  # filter(item %in% c(1, 5, 9) & mode == 1) %>%
  filter(item == 5 & mode == 1) %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 16) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m1 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  lims(y = c(0, 1.6)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object - MODE 1") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  # filter(item %in% c(5, 9) & mode == 2) %>%
  filter(item == 5 & mode == 2 & bodyPosture != "Other body movement") %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m2 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  lims(y = c(0, 1.6)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #theme(legend.position = "none")  + 
  ggtitle("Body Posture (BPH) - Frequency Items x Object -  MODE 2") +
  xlab("Item (Object)") + ylab("Mean") +
  theme(axis.text.x=element_text(angle = 50, hjust=1)) +
  scale_fill_discrete(name = "Items")


### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  #filter(item %in% c(1, 5, 9) & mode == 3) %>%
  filter(item == 5 & mode == 3, bodyPosture != "Other body movement") %>%
  group_by(p, item, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(item, bodyPosture) %>% 
  summarise(freq = sum(total)) %>%
  mutate(pMode = 13) %>% # Fix the number of participants per mode 
  mutate(mean = round(freq/pMode, 1))

# frequency figure
m3 <- ggplot(data = pivotIB, aes(x = factor(item), y = mean, fill = bodyPosture)) +    # print bar chart
  lims(y = c(0, 1.6)) +
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

### ANOVA
### frequency table - ITEMS x BODY POSTURE x OBJECT x MODES
pivotIB <- body.df %>%
  #filter(bodyPosture == "Move towards robot") %>%
  filter(bodyPosture == "Move towards robot", item == 5) %>%
  group_by(p, mode, bodyPosture) %>%
  summarise(total = length(bodyPosture))

ggplot(pivotIB) +
  aes(x = mode, y = total, color = mode) +
  geom_jitter() + 
  theme(legend.position = "none") +
  ggtitle("Body Posture (BPH) - Move towards robot (total) x MODE x Item 5") +
  scale_x_discrete(name ="Mode", 
                   limits=c("1","2","3")) +
  ylab("Total (Move towards robot)") +
  geom_text_repel(aes(mode, total, label = p), size = 2)


### P119

pivotIB <- body.df %>%
  filter(item %in% c(5) & mode == 2 & bodyPosture == "Move towards robot" & p == 119) 