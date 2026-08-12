######################################
# Code to get categorical accuracy of map with four clusters
# This code generates the numbers presented in Table S2

######################################
library(dplyr)


setwd("//groups.geos.ed.ac.uk/landteam/people/lorena_benitez/UMAP_maps")


# Load data
# This has the predicted values for categorical maps generated at both 1km and 100m resolutions
# The extract occurs in the map vizualisation python script
data<-read.csv('test_extracted_categorical.csv')


# Reassign the cluster names
data <- data %>%
  mutate(cluster4 = recode(cluster4,
                           `0` = 'Miombo',
                           `1` = 'Savanna',
                           `2` = 'Androstachys',
                           `3` = 'Mopane'
                           
  ))
data <- data %>%
  mutate(cluster4_100m = recode(cluster4_100m,
                           `1` = 'Miombo',
                           `2` = 'Savanna',
                           `3` = 'Androstachys',
                           `4` = 'Mopane'
                           
  ))

data <- data %>%
  mutate(cluster4_1km = recode(cluster4_1km,
                                `1` = 'Miombo',
                                `2` = 'Savanna',
                                `3` = 'Androstachys',
                                `4` = 'Mopane'
                                
  ))

data <- data %>%
  mutate(cluster7 = recode(cluster7,
                           `0` = 'Mopane',
                           `1` = 'Cryptosepalum',
                           `2` = 'Baikiaea',
                           `3` = 'Combretum',
                           `4` = 'Uapaca',
                           `5` = 'Androstachys',
                           `6` = 'Julbernardia'
  ))

##############################
# Make everything a factor
data$cluster4_1km<-as.factor(data$cluster4_1km)
data$cluster4<-as.factor(data$cluster4)
data$cluster4_100m<-as.factor(data$cluster4_100m)

###################################################
# Get accuracy assessment
library(caret)

# For 1km map
confusionMatrix(data$cluster4_1km,data$cluster4)

# For 100m map
confusionMatrix(data$cluster4_100m,data$cluster4)



#######################################################
# Determine which plots are correct

data<-data%>%
  mutate(correct_100m=cluster4==cluster4_100m,
         correct_1km=cluster4==cluster4_1km)

# Get table of cluster 7 to corect assingments
table(data$cluster7,data$correct_100m)


summary_df <- data %>%
  group_by(cluster7) %>%
  summarise(
    n = n(),
    correct_n = sum(correct_100m, na.rm = TRUE),
    incorrect_n = sum(!correct_100m, na.rm = TRUE),
    pct_correct = correct_n / n * 100
  )

summary_df

summary_data <- data %>%
  group_by(cluster7) %>%
  summarise(
    n = n(),
    correct_n = sum(correct_1km, na.rm = TRUE),
    incorrect_n = sum(!correct_1km, na.rm = TRUE),
    pct_correct = correct_n / n * 100
  )
summary_data
