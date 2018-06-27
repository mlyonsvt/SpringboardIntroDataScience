# Data Wrangling Exercise 2
# Room for improvement: working directory, 

# Set working directory to the desktop, the location of my file
setwd("C:/Users/mwlyo/Desktop")

# Import the file as a data frame called "titanic"
titanic <- read.csv("titanic_original.csv", header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE, fill = TRUE)

# Replace the missing "embarked" values as "S" for Southampton
titanic$embarked <- sub("^$", "S", titanic$embarked)

# For null age values, they are assigned the value of the mean age of the ages that are recorded
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)

# For people who did not make it to a boat, their null values were replaced with "NA"
titanic$boat <- sub("^$", "NA", titanic$boat)

# In case the fact that a person's cabin number was recorded is an important indicator of survival, a binary
# column was created to record whether or not a passenger's cabin number was recorded
titanic <- mutate(titanic, has_cabin_number = ifelse(titanic$cabin != "", 1, 0))

# This writes the final data frame "titanic" as a .csv file
write.csv(titanic, "titanic_clean.csv")