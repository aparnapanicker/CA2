#NI Crime Data
#The zip folder was downloaded, unzipped and saved into the same folder of R project
#The name of the unzipped folder is NI_Crime_Data
#Step A
#Viewing the current directory 
getwd()
#Setting the directory where the crime data folders are present as the current directory
setwd('NI_Crime_Data/')
#Storing the foldernames where the csv files are present into a variable
foldernames <- list.files(full.names=TRUE)
#Displaying the foldernames
foldernames
#Storing the filenames of csv files 
filenames <- list.files(foldernames, full.names=TRUE)
#Displaying the filenames
filenames
#Pushing data from files into variable ALLNI
AllNI <- lapply(filenames,function(i){read.csv(i)})
#Turning the dataset into dataframe
AllNICrimeData <- do.call(rbind.data.frame, AllNI)
#Displaying the top 10 lines of the dataframe
head(AllNICrimeData)
setwd('C:/Users/user/Documents/CA2/')
getwd()
#Saving the combined data into a new csv file
write.csv(AllNICrimeData,"AllNICrimeData.csv", row.names=FALSE)
#Displaying the number of rows
nrow(AllNICrimeData)

#Step B
#Reading ALLNICrimeData.csv file into a new data frame
AllNICrime <- read.csv("AllNICrimeData.csv")
#Showing the structure of AllNICrimeData.csv
str(AllNICrime)
#Removing the attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,last outcome and context
AllNICrime<- AllNICrime[, -c(1, 3:4, 8, 9, 11, 12)]
#show the structure of the modified file
str(AllNICrime)
#Saving the changes into the AllNICrimeData.csv
write.csv(AllNICrime,"AllNICrimeData.csv", row.names=FALSE)


#Step C
AllNICrime$Crime.type <- as.character(AllNICrime$Crime.type)
attach(AllNICrime)
AllNICrime$Crime.type[Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNICrime$Crime.type[Crime.type == "Bicycle theft"] <- "BITH"
AllNICrime$Crime.type[Crime.type == "Burglary"] <- "BURG"
AllNICrime$Crime.type[Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNICrime$Crime.type[Crime.type == "DRUGS"] <- "DRUG"
AllNICrime$Crime.type[Crime.type == "Other theft"] <- "OTTH"
AllNICrime$Crime.type[Crime.type == "Public order"] <- "PUBO"
AllNICrime$Crime.type[Crime.type == "Robbery"] <- "ROBY"
AllNICrime$Crime.type[Crime.type == "Shoplifting"] <- "SHOP"
AllNICrime$Crime.type[Crime.type == "Theft from the person"] <- "THPR"
AllNICrime$Crime.type[Crime.type == "Vehicle crime"] <- "VECR"
AllNICrime$Crime.type[Crime.type == "Violence and sexual offences"] <- "VISO"
AllNICrime$Crime.type[Crime.type == "Other crime"] <- "OTCR"
detach(AllNICrime)
write.csv(AllNICrime,"AllNICrimeData.csv", row.names=FALSE)
AllNICrime$Crime.type <- as.factor(AllNICrime$Crime.type)



#Step D
#Creating table to calculate the ferquency of each crimes
table(AllNICrime$Crime.type)
#calculating the proportions of crime frequencies
prop.table(table(AllNICrime$Crime.type))
#creating graphical display
attach(AllNICrime)
barplot(height = prop.table(table(AllNICrime$Crime.type)), type = "h", col.main="red", col.lab="blue", col.sub="black", main = "Frequency of crimes", xlab="Crime Type",
     ylab="Frequency")
detach(AllNICrime)


#Step E 
#Converting the location attribute to character
AllNICrime$Location <- as.character(AllNICrime$Location)
#Using the stringr library
library(stringr)
# Removing and storing the string On or near from every rows of the location attribute
AllNICrime$Location <- str_remove(AllNICrime$Location, "On or near ")
#Replacing the resultant blank fileds with NA
AllNICrime$Location[AllNICrime$Location == ""] <- NA
#Converting the location attribute back to factor
AllNICrime$Location <- as.factor(AllNICrime$Location)
#Displaying s sample of the changes made to the data
head(AllNICrime, 10)  
#Saving changes to ALLNICrimeData csv file
write.csv(AllNICrime,"AllNICrimeData.csv", row.names=FALSE)

#Step F
#Creating a new data set called cleanlocation that does not contain the NA identifier in location attribute
cleanlocation <- AllNICrime[complete.cases(AllNICrime$Location), ]
#Setting the seed value to 100
set.seed(100)
#Generating 5000 random samples and storing it in a new data frame random crime_sample
random_crime_sample <- cleanlocation[sample(1:nrow(cleanlocation), 5000, replace = TRUE),]


#Reading CleanNIPostcodeData.csv file
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv")
str(CleanNIPostcodeData)
CleanNIPostcodeData <- CleanNIPostcodeData[complete.cases(CleanNIPostcodeData$Primary_Thorfare), ]
#creating function find_a_town
find_a_town <- function(df) {
  CleanNIPostcodeData$Town[match(tolower(df), tolower(CleanNIPostcodeData$Primary_Thorfare))] 
}

#Calling the function
random_crime_sample$Town <- find_a_town(random_crime_sample$Location)


#Step G
#Reading the `VillageList.csv` file
VillageList <- read.csv("VillageList.csv")
#Creating function `add_town_data`
add_town_data <- function(df)
{
  VillageList$POPULATION[match(tolower(df),tolower(VillageList$ï..CITY.TOWN.VILLAGE))]
}
#Calling function `add_town_data`
random_crime_sample$Population <- add_town_data(random_crime_sample$Town)

#Step H
#Changing the column name of Town to City-Town-Village and Crime.type to Crime type
colnames(random_crime_sample)[6] <-  "City-Town-Village"
colnames(random_crime_sample)[5] <-  "Crime type"
#Writing the  changes to  random_crime_sample csv file
write.csv(random_crime_sample,"random_crime_sample.csv", row.names=FALSE)

#Step I
#Filtering and storing crimes that happened only in Belfast
belfastcrime <- filter(random_crime_sample,`City-Town-Village` == 'BELFAST')
#Counting the number of crimes that happened in Belfast
belfastcrimecount <- count(belfastcrime,Crime.type)
#Sorting crime according to count
sort(belfastcrimecount$n)
#Filtering and storing the crimes that happned only in Londenderry
londonderrycrime <- filter(random_crime_sample,`City-Town-Village` == 'LONDONDERRY')
#Counting the number of crimes that happened in Londenderry
londonderrycrimecount <- count(londonderrycrime,Crime.type)
#Sorting crime according to count
sort(londonderrycrimecount$n)
#Creating multi-panelled window for plotting
par(mfrow = c(2,2))
#Plotting the crimes happened in Blefast
plot(belfastcrimecount$Crime.type, belfastcrimecount$n, xlab = "Crime Type", ylab = "Crime Count", main = "Types of crime happened in Belfast with their count", cex.axis=0.35)
#Plotting the crimes happened in Londonderry
plot(londonderrycrimecount$Crime.type, londonderrycrimecount$n, xlab = "Crime Type", ylab = "Crime Count", main = "Types of crime happened in Londonderry with their count", cex.axis=0.35)
