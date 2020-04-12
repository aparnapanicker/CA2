#All csv files required for this analysis are stored in the same folder of R project
#See the current working directory 
getwd()
ni_postcode <- read.csv("NIPostcodes.csv", header = FALSE)

# Step A
#Showing the class of data
class(ni_postcode)
#Showing the total number of rows avaiable in the data frame
nrow(ni_postcode)
#Showing the structure of the data frame
str(ni_postcode)
#Showing the first 10 rows of the data frame
head(ni_postcode, 10)


#Step B
#Creating attributre names
col_names <- c("Organisational_Name", "Sub-building_Name", "Building_Name", "Number", "Primary_Thorfare", "Alt_Thorfare", 
             "Secondary_Thorfare", "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary_Key")
#Assigning attribute names to data frame
colnames(ni_postcode) <- col_names
#Showing the structure of the data frame after assigning column name
str(ni_postcode)
#Showing the firts 10 rows of data frame with attribute names
head(ni_postcode, 10)



#Step C
#Replacing and Recoding all missing entries with `NA`
ni_postcode$Organisational_Name[ni_postcode$Organisational_Name == ""] <- NA
ni_postcode$`Sub-building_Name`[ni_postcode$`Sub-building_Name`== ""] <- NA
ni_postcode$Building_Name[ni_postcode$Building_Name == ""] <- NA
ni_postcode$Number[ni_postcode$Number == ""] <- NA
ni_postcode$Primary_Thorfare[ni_postcode$Primary_Thorfare == ""] <- NA
ni_postcode$Alt_Thorfare[ni_postcode$Alt_Thorfare == ""] <- NA
ni_postcode$Secondary_Thorfare[ni_postcode$Secondary_Thorfare == ""] <- NA
ni_postcode$Locality[ni_postcode$Locality == ""] <- NA
ni_postcode$Townland[ni_postcode$Townland == ""] <- NA
ni_postcode$Town[ni_postcode$Town == ""] <- NA
ni_postcode$County[ni_postcode$County == ""] <- NA
ni_postcode$Postcode[ni_postcode$Postcode == ""] <- NA
ni_postcode$`x-coordinates`[ni_postcode$`x-coordinates` == ""] <- NA
ni_postcode$`y-coordinates`[ni_postcode$`y-coordinates` == ""] <- NA
ni_postcode$Primary_Key[ni_postcode$Primary_Key == ""] <-NA


# Display the columns containing missing values
library(VIM)
missing_values <- aggr(ni_postcode, prop = FALSE, numbers = TRUE)
summary(missing_values)

#Step D
#Showing the total number of missing entries in the data frame
sum(is.na(ni_postcode))
#Displayng the count of missing entries based on attributes
colSums(is.na(ni_postcode))


#Step E
#Moving the attribute `Primary Key` to the start of the data set
ni_postcode <- ni_postcode[,c(15,1:ncol(ni_postcode)-1)]

#Step F
#Creating a new data set where the name of locality, town or townland is `Limeavady``
Limevady_data <- subset(ni_postcode, ni_postcode$Town == "LIMAVADY" | ni_postcode$Townland == "LIMAVADY" | ni_postcode$Locality == "LIMAVADY")
#Counting the number of rows of the new data set 
nrow(Limevady_data)
#Storing information in a new csv file
write.csv(Limevady_data,'Limavady.csv', row.names=FALSE)


#Step G
#Saving the modified dataset to the csv file CleanNIPostcodeData
write.csv(ni_postcode,"CleanNIPostcodeData.csv", row.names=FALSE)
