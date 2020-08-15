# Section 1
# A)
# Loading the CSV file by placing in the working directory
NIPOSTCODE = read.csv("NIPostcodes.csv", header = FALSE)

# Displaying the total number of rows
nrow(NIPOSTCODE)
# Structure of the data
str(NIPOSTCODE)
# Displaying the fisrt 10 rows of the data frame
head(NIPOSTCODE,10)

# B)
# Assigning the relevant title for each attribute of the data
col_names = c("Organisation Name", 
              "Sub-building Name", 
              "Building Name", 
              "Number", 
              "Primary Thorfare",
              "Alt Thorfare", 
              "Secondary Thorfare",
              "Locality", 
              "Townland",
              "Town", 
              "County", 
              "Postcode", 
              "x-coordinates",
              "y-coordinates", 
              "Primary Key (identifier)")
colnames(NIPOSTCODE) <- col_names
View(NIPOSTCODE)

# C)
# Checking the count of missing entries in each attribute
data.frame(lapply(NIPOSTCODE, function(x)sum(x =="")))

# Using library VIM
library(VIM)
# Plotting a graph to choose the best consideration for the missing entries 
missing_values <- aggr(NIPOSTCODE, prop= FALSE, numbers = TRUE)
summary(missing_values)

# D)
# Replacing the missing values with the NA
NIPOSTCODE[NIPOSTCODE == ""] <- NA
# To view the data frame
View(NIPOSTCODE)

# Checking the count of NA in each attribute
data.frame(sapply(NIPOSTCODE, function(x)sum(length(which(is.na(x))))))

# E)
# Changing the position of the primary key to the first
NIPOSTCODE <-NIPOSTCODE[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
# Displaying the first 10 rows
head(NIPOSTCODE,10)

# F)
# To store only the data within name containing limavady
attach(NIPOSTCODE)
limavady_data <- subset(NIPOSTCODE, Locality=="LIMAVADY"|Townland == "LIMAVADY"|Town=="LIMAVADY")
View(limavady_data)
# To check the rows in the Limavady data 
nrow(limavady_data)
# The modified data is loaded in to a csv file
write.csv(limavady_data, "Limavady.csv")

# G)
# Saving the modified data in new csv file
write.csv(NIPOSTCODE, "CleanNIPostCode.csv")



