# load files and data frames
# source("./load-files.R")

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
  geom_bar(stat="identity")


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
  geom_bar(stat="identity")

### BAR PLOT - BODY AND HEAD MEAN ENTRIES
pivotTotal <- rbind(pivotB, pivotH)

# compare BODY vs HEAD
ggplot(data = pivotTotal, aes(x = factor(mode), y = mean, fill = condition)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')


### BODY - frequency values per column

# frequency table - BODYPOSTURE
pivotB <- body.df %>%
  group_by(p, mode, bodyPosture) %>%
  summarise(total = length(bodyPosture)) %>%
  na.omit(bodyPosture) %>%
  group_by(mode, bodyPosture) %>% 
  summarise(freq = sum(total), ps = length(unique(p))) %>%
  mutate(mean = round(freq/ps,1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(mode), y = mean, fill = bodyPosture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')


# frequency table - HANDGESTURE
pivotB <- body.df %>%
  group_by(p, mode, handGesture) %>%
  summarise(total = length(handGesture)) %>%
  na.omit(handGesture) %>%
  group_by(mode, handGesture) %>% 
  summarise(freq = sum(total), ps = length(unique(p))) %>%
  mutate(mean = round(freq/ps,1))

# frequency figure
ggplot(data = pivotB, aes(x = factor(mode), y = mean, fill = handGesture)) +    # print bar chart
  geom_bar(stat = 'identity', position = 'dodge')