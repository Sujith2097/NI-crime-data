# Section-2
setwd("C:/Users/Dell/OneDrive/Documents/L00151024/NI Crime Data")
# A)
# combining all of the crime data from each csv file into one data set
# creating an empty dataframe to store the data after looping
final_crime_data <- data.frame()
#files_list contains all the csv files from NI Crime Data
files_list <- list.files(recursive = TRUE)
for(file_list in files_list){
  crime_data <- read.csv(file_list)
  final_crime_data <- rbind(final_crime_data, crime_data)
}
# Getting the current working directory
getwd()
setwd("NI Crime Data/")
# Saving the combined dataset into a csv file
write.csv(final_crime_data, "AllNICrimeData.csv")
AllNICrimeData <- read.csv("AllNICrimeData.csv")
str(AllNICrimeData)
# showing the number of rows
nrow(AllNICrimeData)
# B)
# Selecting required attribute and removing other attributes according to the given task and saving it in the new csv file
AllNICrimeData <- final_crime_data[c(2,5,6,7,10)]
str(AllNICrimeData)

# C) 
# Giving short forms to the crime type attribute
AllNICrimeData$Crime.type <- as.character(AllNICrimeData$Crime.type)
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Bicycle theft"] <- "BITH"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Burglary"] <- "BURG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "DRUGS"] <- "DRUG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Other Theft"] <- "OTTH"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Public order"] <- "PUBO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Robbery"] <- "ROBY"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Shoplifting"] <- "SHOP"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Theft from the person"] <- "THPR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Vehicle crime"] <- "VECR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Violence and sexual offences"] <- "VISO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Other crime"] <- "OTCR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Drugs"] <- "DRUG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Possession of weapons"] <- "POW"

# D)
# Plotting a bar graph of frequecy of crime type and setting the axe labels and bar colours
library(ggplot2)
ggplot(AllNICrimeData, aes(x=Crime.type)) + geom_bar(fill = "blue", color ="black")


# E)
#install.packages("stringr")
library(stringr)
# Removing the extra strings in the location attribute and only keeping the street names
AllNICrimeData$Location <- str_remove_all(AllNICrimeData$Location, 'On or near ')
# Assigning NA values for no location attribute value
AllNICrimeData$Location[AllNICrimeData$Location == "no location" | AllNICrimeData$Location == ""] <- NA



# F)
# Choosing 5000 random samples of crime data from AllNICrimeData
# setting the seed value to 100
set.seed(100)
# Where location attribute should not contain any NA values
random <- subset(AllNICrimeData[, ], !is.na(AllNICrimeData$Location))
# saving the data into the new file randowm_crime_data
random_crime_sample <- random[sample(1:nrow(random), 5000, replace = FALSE), ]

# changing the string to lower case to match the attribute
NIPOSTCODE$`Primary Thorfare` <- str_to_lower(NIPOSTCODE$`Primary Thorfare`)
random_crime_sample$Location <- str_to_lower(random_crime_sample$Location)

# creating a function to match to find correct town/city information for each location variable within the random_crime_sample dataset
find_a_town <- function(x,y){
  
  return(NIPOSTCODE$Town[match(x,y)])
}
# Saving the match attributes into the town column of the random_crime_sample
random_crime_sample$Town <- find_a_town(random_crime_sample$Location, NIPOSTCODE$`Primary Thorfare`)
str(random_crime_sample)

# G)
# Reading village data
village_data  <- read.csv("VillageList.csv")
View(village_data)
# Changing the string into upper case
village_data$ï..CITY.TOWN.VILLAGE  <- str_to_upper(village_data$ï..CITY.TOWN.VILLAGE)
village_data$ï..CITY.TOWN.VILLAGE  <- str_replace_all(village_data$ï..CITY.TOWN.VILLAGE, "DERRY", "LONDONDERRY")
# Creating a function to match the each crime data to relevant information in village list
add_town_data <- function(x,y){
  
  return(village_data$POPULATION[match(x,y)])
}
# storing it into the random_crime_sample
random_crime_sample$Population <- add_town_data(random_crime_sample$Town, village_data$ï..CITY.TOWN.VILLAGE)
str(random_crime_sample)

# H)
# changing the column name of town to city-town-village
colnames(random_crime_sample)[6] <- c("City-Town-Village")
# Saving the final output to the csv file
write.csv(random_crime_sample, "random_crime_sample.csv")

# I)
# Ploting the crimes in cities Belfast and Londonderry
random_crime_belfast <- subset(random_crime_sample, random_crime_sample$`City-Town-Village` == "BELFAST")
random_crime_derry <- subset(random_crime_sample, random_crime_sample$`City-Town-Village` == "LONDONDERRY")


library(dplyr)
library(ggplot2)
# Step 1
plot1 <- random_crime_belfast %>%
  #Step 2
  group_by(Crime.type) %>%
  #Step 3
  summarise(count_fre = n()) %>%
  #Step 4
  ggplot(aes(x = Crime.type, y = sort(count_fre, decreasing = TRUE), fill = Crime.type)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Crime.type",
    y = "Count",
    title = paste(
      "Crimes in Belfast"
    )
  )
plot2 <- random_crime_derry %>%
  #Step 2
  group_by(Crime.type) %>%
  #Step 3
  summarise(count_fre = n()) %>%
  #Step 4
  ggplot(aes(x = Crime.type, y = sort(count_fre, decreasing = TRUE), fill = Crime.type)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Crime.type",
    y = "Count",
    title = paste(
      "Crimes in Londonderry"
    )
  )

install.packages("gridExtra")
require(gridExtra)
grid.arrange(plot1,plot2,ncol=2)


