
#packeges
install.packages("readxl") #install packages to read XLSX
install.packages("ggplot2") #install packages to biuld graphs 
install.packages('GGally')
# Libraries
library(readxl) 
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(summarytools)
library(stringr)
library(GGally)
library(gapminder)
library(ggExtra)
library(viridis) 
par(mar=c(2,2,2,2))
dev.off()

getwd()

# set the local work where we will use files
# setwd("E:\\Givaldo\\DATA SCIENCE\\MESTRADO CIT\\SUBJECTS\\R\\Assignment 1\\")

my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.path)
# List the worksheet on Excel file
excel_sheets('assignment1.xlsx')
# [1] "Sheet1" "Sheet2"


##########################################################################

# 1. Using the xlsx or readxl package or otherwise, read each sheet in the ”assignment1.xlsx” file into R.

# reading excel file sheet_1 and inserting in varible sheet_1
sheet_1 <- read_excel('assignment1.xlsx', sheet = "Sheet1", 
                      col_names = c("IDNumber","FirstName","Surname","Age","DoB","Height","Weight", #name of columns
                                    "EducationLevel", "Salary","CriminalRecord"),
                      skip = 1) # skip = 1 = skip the first line

sheet_2 <- read_excel('assignment1.xlsx', sheet = "Sheet2") #reading sheet_2 and inserting in varible sheet_2

#show sheet_1 
View(sheet_1)

##########################################################################
#before
class(sheet_1)# show class of sheet_1
str(sheet_1) #show information about dataframe sheet_1
# 2. Generate a data frame for each sheet in the file.

sheet_1<-as.data.frame(sheet_1) #converting variable sheet_1 (Classes ‘tbl_df’, ‘tbl’ and 'data.frame') into a data frame
sheet_2<-as.data.frame(sheet_2)

#after
class(sheet_1)# show class of sheet_1
str(sheet_1) #show information about dataframe sheet_1

##########################################################################

# 3. The dataset in the first sheet is a random selection from a larger dataset.
# You will never get access to the full dataset so you should regenerate a new 
# identification number for each subject in the dataset. This should be the row 
# number of each entry in Sheet 1. You do not need to do this for Sheet 2.

for(i in 1:length(sheet_1$IDNumber)){ # length(sheet_1$IDNumber = give the total row from IDNumber
  sheet_1$rownumber[i] <- i # add number range from 1 to total rows in sheet_1
  }

sheet_1$rownumber #print column rownumber

##########################################################################

# 4. It is also required to have an additional identifier which is the number you 
# have generated in (3) followed by the first letter of each subjects first name 
# and then followed by the first letter of each subject’s surname. You do not need 
# to do this for Sheet 2.

for(i in 1:length(sheet_1$rownumber)){ # length(sheet_1$rownumber = give the total row from rownumber
  
  A<- i # number range from 1 to total rownumber
  B<- str_sub(sheet_1$FirstName[i], end = 1) # take the first letter from FIRSTNAME in sheet1
  C<- str_sub(str_trim(sheet_1$Surname[i]), end = 1)# take the first letter from SURNAME in sheet1
  
  sheet_1$NewID[i] <- paste(A,B,C, sep="") #Add evereeything together in NewID variable in 
   
 
}

select(sheet_1, FirstName,Surname,NewID)# print columns using tidyverse to check if names and surname match

##########################################################################

#5 Although the data is not available for most subjects, some data highlighting subjects state of 
# health is available in Sheet 2. You should use the subjects ID number to match it and merge it 
# with the data in Sheet 1.

FinalDataSet <- merge(sheet_1,sheet_2, by=c("IDNumber","IDNumber")) #merging sheet_1 with sheet_2 by IDNumbers

head(FinalDataSet) # print some few lines to check is is right

##########################################################################

# 6. Not every subject has its ID number included in Sheet 2. 
# You should attempt to match the remaining subjects using their first and surnames. 
# This must be done using tidyverse in a robust manner. Your code for doing this should 
# work again in the case of a new sample of data being provided.

JoinDataSet <- function(dataset1,dataset2,a,b,c,d){ #create a function recieving (dataset1,dataset2,a,b,c,d) as arguments
  
  DataSet <- left_join(dataset1, dataset2, by = c(a,b,c,d)) #join the datasets using tidyverse/ insert in DataSet variable
  
  return(DataSet)# return DAtaSet variable (2 dataframes joined)

  }

FinalDataSet <- JoinDataSet(sheet_1,sheet_2,"FirstName", "FirstName", "Surname" , "Surname")# calling function with arguments

head(FinalDataSet)# print few lines to check  
str(FinalDataSet)# print information about Final dataframe

##########################################################################

# 7. You should add a column for age range. This should be

#this part of code filter rows ' Age < = 17' and add a label "0 - 17" in a new variable 'Age_Range' in sheet_1
# the same for each line for diferent ages
sheet_1$Age_Range[sheet_1$Age <= 17] <-"0 - 17"
sheet_1$Age_Range[sheet_1$Age > 17 & sheet_1$Age <= 35 ] <-"18 - 35"
sheet_1$Age_Range[sheet_1$Age > 35 & sheet_1$Age <= 54 ] <-"35 - 54"
sheet_1$Age_Range[sheet_1$Age > 54 & sheet_1$Age <= 74 ] <-"54 - 74"
sheet_1$Age_Range[sheet_1$Age > 74 ] <-"74 +"

table(sheet_1$Age_Range)# show the quantity of each range of age

ctable(sheet_1$Age_Range,sheet_1$CriminalRecord)# create a table crossing information from age range and criminal records

#filter information from age_range by respective label and insert it into a new variable
#these variable contain a subset from sheet_1
sheet_Age_Range17 <- sheet_1 %>% filter(Age_Range=="0 - 17")
sheet_Age_Range35 <- sheet_1 %>% filter(Age_Range=="18 - 35")
sheet_Age_Range54 <- sheet_1 %>% filter(Age_Range=="36 - 54")
sheet_Age_Range74 <- sheet_1 %>% filter(Age_Range=="55 - 74")
sheet_Age_Range90 <- sheet_1 %>% filter(Age_Range=="74 +")


#========================================================================

# this part of code creates a range of Height
sheet_1$Hei_Range[sheet_1$Height <= 1.700] <-"0-1.700"
sheet_1$Hei_Range[sheet_1$Height > 1.700 & sheet_1$Height <= 1.900 ]<-"1.700-1.900"
sheet_1$Hei_Range[sheet_1$Height > 1.900 ]<-"1.900+"

# show quantity from Height range
# we can see the proportion of each range
table(sheet_1$Hei_Range)
# 0-1.700 1.700-1.900      1.900+ 
#   51          49          60 

#========================================================================

# this part of code creates a range of Weight
sheet_1$Wei_Range[sheet_1$Weight <= 100] <-"0-100"
sheet_1$Wei_Range[sheet_1$Weight > 100 & sheet_1$Weight <= 150 ]<-"100-150"
sheet_1$Wei_Range[sheet_1$Weight > 150 ]<-"150+"

# show quantity from Weight range
# we can see the proportion of each range
table(sheet_1$Wei_Range)
# 0-100 100-150    150+ 
#   44      44      72

#========================================================================

#this line calculate the BMI formula (Weight/Height²)
sheet_1$BMI <- (sheet_1$Weight/(sheet_1$Height^2))

# this part of code creates a range of BMI
sheet_1$BMI_range[sheet_1$BMI <= 30] <- "0-30"
sheet_1$BMI_range[sheet_1$BMI > 30 & sheet_1$BMI <= 45 ] <- "30-45"
sheet_1$BMI_range[sheet_1$BMI > 45 ] <-"45+"

# show quantity from BMI range
# we can see the proportion of each range
table(sheet_1$BMI_range)
# 0-100 100-150    150+ 
#   44      44      72

# BMI
# between 18,5 e 24,9	normal
# between 25,0 e 29,9	overweight
# between 30,0 e 34,9	Obesity grade I.
# between 35,0	Obesity grade II.
# 40,0 and above	Obesity grade I. III e IV.


#========================================================================
# this part of code creates a range of Salary
sheet_1$SAL_Range[sheet_1$Salary <= 11440] <-"0-11440"
sheet_1$SAL_Range[sheet_1$Salary > 11440 & sheet_1$Salary <= 22639 ]<-"11440-22639"
sheet_1$SAL_Range[sheet_1$Salary > 22639 ]<-"22639+"


# show quantity from Salary range
# we can see the proportion of each range
table(sheet_1$SAL_Range)
# 0-11440   11440-22639      22639+ 
#   40          80             40 

#========================================================================
# this part of code creates a range of DoB - I suppose this means Date of Birthday

sheet_1$DoB_Range[sheet_1$DoB <= "1950-01-01"] <-"0-1950"
sheet_1$DoB_Range[sheet_1$DoB > "1950-01-01" & sheet_1$DoB <= "2000-01-01" ] <-"1950 - 2000"
#sheet_1$DoB_Range[sheet_1$DoB < "2000-01-01" ] <-"< 2000"
sheet_1$DoB_Range[sheet_1$DoB > "2000-01-01" ] <-"2000 +"

# show quantity from DoB range
# we can see the proportion of each range
table(sheet_1$DoB_Range)

#this part of code makes a filter in DoB_Range creating a subset from sheet_1 dataset
sheet_DoB_Range_1950 <- sheet_1 %>% filter(DoB_Range=="0-1950")
sheet_DoB_Range_2000 <- sheet_1 %>% filter(DoB_Range=="1950 - 2000")
sheet_DoB_Range_more_2000 <- sheet_1 %>% filter(DoB_Range=="2000 +")
#sheet_DoB_Range_less_2000 <- sheet_1 %>% filter(DoB_Range=="< 2000")



#########################################################################

#I need to call this function again to add the 'Range' columns in FinalDataSet
FinalDataSet <- JoinDataSet(sheet_1,sheet_2,"FirstName", "FirstName", "Surname" , "Surname")# calling function with arguments


#This code cross information from several variables to compare how one affect each other 
#---------------------------------------------------------------
ctable(sheet_1$CriminalRecord,sheet_1$DoB_Range)
ctable(sheet_1$EducationLevel,sheet_1$DoB_Range)
ctable(sheet_1$Age_Range,sheet_1$DoB_Range)
ctable(sheet_1$Hei_Range,sheet_1$DoB_Range)
ctable(sheet_1$Wei_Range,sheet_1$DoB_Range)
ctable(sheet_1$SAL_Range,sheet_1$DoB_Range)
ctable(FinalDataSet$Health,FinalDataSet$DoB_Range)
ctable(sheet_1$Hei_Range,sheet_1$Age_Range)
ctable(sheet_1$Wei_Range,sheet_1$Age_Range)
#---------------------------------------------------------------

#changing CriminalRecord and Health into factor variables
 sheet_1$CriminalRecord <- factor(sheet_1$CriminalRecord, levels = c(1,2),
                                 labels = c("One", "Two"))
 
 sheet_2$Health <- factor(sheet_2$Health, levels = c(1,2),
                         labels = c("One", "Two"))

#function for joining datasets sheet_1 and sheet_2 in one final dataset
FinalDataSet  <- JoinDataSet(sheet_1,sheet_2,"FirstName", "FirstName", "Surname" , "Surname")# calling function with arguments

##########################################################################

# 8. You should filter the data by each age category. 
# Generate a bar plot using ggplot2 for the criminal record variable.

#age  and criminal record 

# comparing differences between the range of age and criminal records
ggplot(data, aes(factor(age), fill = factor(gender))) +
  geom_bar(position = position_dodge2(preserve = "single"))

# creating a cross table between range of age and Criminal records
ctable(data$age,data$gender)

#----------------------------------------------------------------
# comparing Age range and Criminal record filtering by Salary range

# here we have just criminal record One 
Filter<-filter(sheet_1, SAL_Range=="0-11440")
ggplot(Filter, aes(factor(Age_Range), fill = factor(CriminalRecord))) +
  geom_bar(position = position_dodge2(preserve = "single"))

# here we have both criminal record, one and two
Filter<-filter(sheet_1, SAL_Range=="11440-22639")
ggplot(Filter, aes(factor(Age_Range), fill = factor(CriminalRecord))) +
  geom_bar(position = position_dodge2(preserve = "single"))

# here we have just criminal record two 
Filter<-filter(sheet_1, SAL_Range=="22639+")
ggplot(Filter, aes(factor(Age_Range), fill = factor(CriminalRecord))) +
  geom_bar(position = position_dodge2(preserve = "single"))



#---------------------------------------------------------------------------------

#comparing the criminal records with a salary range
# criminal record = 2 have the highest salary
ggplot(sheet_1, aes(factor(SAL_Range), fill = factor(CriminalRecord))) +
  geom_bar(position = position_dodge2(preserve = "single"))


# creating a linear correlation between Weight and Height
ggplot(data, aes(x = net_t, y =  gross_t)) +
  geom_point(aes(color = gender)) +
  geom_smooth(method = 'lm')# create the line of linear correlatio

# creating a linear correlation between Weight and Height
ggplot(data, aes(x = net_t, y =  net_t_v)) +
  geom_point(aes(color = gender)) +
  geom_smooth(method = 'lm')# create the line of linear correlatio

# creating a linear correlation between Weight and Height
ggplot(data, aes(x = net_t, y =  canceled_t_v)) +
  geom_point(aes(color = gender)) +
  geom_smooth(method = 'lm')# create the line of linear correlatio




#---------------------------------------------------------------------
#salary/Age vs criminal record 1

Filter<-filter(data, gender=="male")
ggplot(Filter, aes(x = Month, y =  net_t)) +
  geom_point(aes(color = age)) +
  geom_smooth(method = 'lm')# create the line of linear correlation

#salary/Age vs criminal record 2
Filter<-filter(sheet_1, CriminalRecord=="Two")
ggplot(Filter, aes(x = Age, y =  Salary)) +
  geom_point(aes(color = CriminalRecord)) +
  geom_smooth(method = 'lm')# create the line of linear correlation


#---------------------------------------------------------------------
#filter Salary range  different from the range  11440-22639
Filter<-filter(sheet_1, SAL_Range!="11440-22639") 
ggplot(Filter, aes(x = Age, y =  Salary)) +
  geom_point(aes(color = CriminalRecord)) +
  geom_smooth(method = 'lm')# create the line of linear correlation



#filter Salary range  different from the range  11440-22639
Filter<-filter(sheet_1, SAL_Range=="11440-22639")
ggplot(Filter, aes(x = Age, y =  Salary)) +
  geom_point(aes(color = CriminalRecord)) +
  geom_smooth(method = 'lm')# create the line of linear correlation


#-------------------------------------------------------------------

ggplot(data , aes(age, net_t)) +
  geom_boxplot(aes(colour = Month), varwidth = TRUE)

# bubble plot comparing Salary/Criminal record/ Age and Height
# we can see a clear separation between criminal record one and Two in Salary less and more than 1500 
data %>% # data set sheet_1
  arrange(desc(net_t)) %>% # Arrange variable Height 
  mutate(gender = factor(gender)) %>% #turn criminal record in factor and a new variable with mutate function
  ggplot(aes(x=net_t_v, y=age, size=net_t, color=gender)) + #plot graph with variables
  geom_point(alpha=0.5) + # level of transparence
  scale_size(range = c(.1, 12), name="net_t") #size of buble

##########################################################################


# 9. You should generate an appropriate visualisation examining the 
# relationships between height, weight, age and criminal records. Comment on this.
# 

# height, weight, age and criminal records

#these boxplot shows a increasing in weight and height as samples get older

ggplot(data = sheet_1, aes(Age_Range, Height)) + # select columns Age_range and Height from dataset sheet_1
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE) #insert colors according to the variable CriminalRecord


ggplot(data = sheet_1, aes(Age_Range, Weight)) + # select columns Age_range and Weight from dataset sheet_1
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE) #insert colors according to the variable CriminalRecord



# create a buble graph considering Height/Weight/Age and Criminal Record
ggplot(sheet_1, aes(x=Age, y=Height, size=Weight, color=CriminalRecord)) +
  geom_point(alpha=0.5) + # level of transparence
  scale_size(range = c(.1, 12), name="Weight(kg)") # size of buble is related with weight



# ------------------------------------------------------------------------------------
#greate a grid graph with several variable together
ggplot(data, aes(x =  net_t, y =net_t_v , color = gender)) +
  geom_point() + #insert the points
  facet_grid(~ age) # separete the grid on total of Age_range group

Filter<-filter(sheet_1,Weight < 80) #filter weight less than 80 in dataset sheet_1
ggplot(Filter, aes(x =  Height, y =Weight , color = CriminalRecord)) +
  geom_point
  facet_grid(~ Age_Range)

#------ grid separeted by Criminal record one and two
ggplot(sheet_1, aes(x =  Age, y =Weight , color = CriminalRecord)) + #determine the axes x and y, and also the color 
  geom_point() + #insert the points
  facet_grid(~ CriminalRecord) # separete the grid on total of Age_range group

#----------Hei_Range------------------------------------------------------------
#these graphs creates a grid with Hei_Range variable

ggplot(sheet_1, aes(x =  Age, y =Weight , color = CriminalRecord)) + #determine the axes x and y, and also the color
  geom_point() + #insert the points
  facet_grid(~ Hei_Range) # separete the grid on total of Hei_Range group

Filter<-filter(sheet_1,Weight > 200) #filter weight above 200kg
ggplot(Filter, aes(x =  Age, y =Weight , color = CriminalRecord)) +#determine the axes x and y, and also the color
  geom_point() + #insert the points
  facet_grid(~ Hei_Range)# separete the grid on total of Hei_Range group

#----------Weight------------------------------------------------------------
ggplot(sheet_1, aes(x =  Age, y =Height , color = CriminalRecord)) + #determine the axes x and y, and also the color
  geom_point() + #insert the points
  facet_grid(~ Wei_Range)# separete the grid on total of Wei_Range group

Filter<-filter(sheet_1,Weight >200)
ggplot(Filter, aes(x =  Age, y =Height , color = CriminalRecord)) + #determine the axes x and y, and also the color
  geom_point() + #insert the points
  facet_grid(~ Wei_Range)# separete the grid on total of Wei_Range group




#-------------------------------------------------------------
#comparing criminal record with variables (range of Height / range of Weight / range of Age)

#barplot
ggplot(sheet_1, aes(factor(Hei_Range), fill = factor(CriminalRecord))) +
  geom_bar(position = position_dodge2(preserve = "single"))

#barplot
ggplot(sheet_1, aes(factor(Wei_Range), fill = factor(CriminalRecord))) +
  geom_bar(position = position_dodge2(preserve = "single"))

#barplot
ggplot(sheet_1, aes(factor(CriminalRecord), fill = factor(Age_Range))) +
  geom_bar(position = position_dodge2(preserve = "single"))


#-----------------------------------------------------------

# linear regrassion considering Height and Age
ggplot(sheet_1, aes(x = Age, y =  Height)) + # graph with information from Age and Height from sheet_1
  geom_point(aes(color = CriminalRecord)) + # color of point separeted by One and Two
  geom_smooth(method = 'lm')# create the line of linear correlation



#-----------------Height--------------------------------------------
# correlation line to visualize difference in Height considering criminal record 1 or 2

# Filter by  CriminalRecord=="One"

Filter<-filter(sheet_1, CriminalRecord=="One")
#create graph with Age and Height
ggplot(Filter, aes(x = Age, y =  Height)) + # use Filte 'subset' from sheet_1 to create the graph
  geom_point(aes(color = CriminalRecord)) + #insert samples in respective position by Criminal Record/Age/Height
  geom_smooth(method = 'lm')# create the line of linear correlation

# Filter by  CriminalRecord=="Two"
Filter<-filter(sheet_1, CriminalRecord=="Two")
#create graph with Age and Height
ggplot(Filter, aes(x = Age, y =  Height)) +# use Filte 'subset' from sheet_1 to create the graph
  geom_point(aes(color = CriminalRecord)) + #insert samples in respective position by Criminal Record/Age/Height
  geom_smooth(method = 'lm')# create the line of linear correlation

#----------------------Weight---------------------------------------
# correlation line to visualize difference in Weight considering criminal record 1 or 2

# Filter by  CriminalRecord=="One"
Filter<-filter(data, gender=="male")
#create graph with Age and Weight
ggplot(Filter, aes(x = age, y = net_t)) +# use Filte 'subset' from sheet_1 to create the graph
  geom_point(aes(color = gender)) + #insert samples in respective position by Criminal Record/Age/Weight
  geom_smooth(method = 'lm')# create the line of linear correlation

# Filter by  CriminalRecord=="Two"
Filter<-filter(sheet_1, CriminalRecord=="Two")
#create graph with Age and Weight
ggplot(Filter, aes(x = Age, y = Weight)) +# use Filte 'subset' from sheet_1 to create the graph
  geom_point(aes(color = CriminalRecord)) +#insert samples in respective position by Criminal Record/Age/Weight
  geom_smooth(method = 'lm')# create the line of linear correlation




# the samples get Taller but not heavier

ggplot(data, aes(x = age, y =  net_t)) +#determine the axes x and y, and also the color
  geom_point(aes(color = gender)) + #determine the color
  geom_smooth(method = 'lm')# create the line of linear correlation

ggplot(sheet_1, aes(x = Age, y =  Weight)) +#determine the axes x and y, and also the color
  geom_point(aes(color = CriminalRecord)) + #determine the color
  geom_smooth(method = 'lm')# create the line of linear correlation


#######################################################################

# 10. Using filters, you should analyse if there are any interesting results in the dataset regarding 
# the relationships between height, weight and criminal record. Use appropriate visualisations.

#height, weight and criminal record.

# hist(sheet_1$BMI, breaks = 40)
# hist(sheet_1$BMI[sheet_1$BMI<45], breaks = 40)
# summary(sheet_1$BMI)
# 
# ctable(sheet_1$CriminalRecord,sheet_1$Hei_Range)
# ctable(sheet_1$CriminalRecord,sheet_1$Wei_Range)
# 
# ctable(sheet_1$Age_Range, sheet_1$Hei_Range)
# ctable(sheet_1$Age_Range, sheet_1$Wei_Range)
# ctable(sheet_1$Age_Range, sheet_1$SAL_Range)
# 
# ctable(sheet_1$EducationLevel,sheet_1$Age_Range)

#-------------------------------------------------------------------------------

ggplot(data, aes(x = age, y =  net_t)) +#determine the axes x and y, and also the color
  geom_point(aes(color = gender)) + #determine the color
  geom_smooth(method = 'lm')# create the line of linear correlation


#density graph comparing Weight and CriminalRecord
ggplot(data=sheet_1, aes(x=Weight, group=CriminalRecord, fill=CriminalRecord)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

#density graph comparing Height and CriminalRecord
ggplot(data=sheet_1, aes(x=Height, group=CriminalRecord, fill=CriminalRecord)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

#density graph comparing Salary and CriminalRecord
# there is a huge difference
# clear division beteween salaries
ggplot(data=sheet_1, aes(x=Salary, group=CriminalRecord, fill=CriminalRecord)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

#density graph comparing Age and CriminalRecord
ggplot(data=sheet_1, aes(x=Age, group=CriminalRecord, fill=CriminalRecord)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()



#BMI range

# samples smaller than 1700cm have the highest level of BMI
ggplot(data = data, aes(age, net_t_v)) +
  geom_boxplot(aes(colour = gender), varwidth = TRUE)
 
#comparing BMI / Weight range and Height range
ggplot(data = FinalDataSet, aes(Wei_Range, BMI)) +
  geom_boxplot(aes(colour = Hei_Range), varwidth = TRUE)

# selecting samples higher than 60 BMI
Filter<-filter(sheet_1, BMI>60)
#criminal record one in ages between 54 and 74 are the heaviest samples
ggplot(data = Filter, aes(Age_Range,Weight)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

# selecting samples higher than 60 BMI
Filter<-filter(sheet_1, BMI>60)
#samples in criminal record two between ages 18 and 35 have the highest level of BMI
ggplot(data = Filter, aes(Age_Range,BMI)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)


#Height range
ggplot(data = sheet_1, aes(BMI_range,Weight)) +
  geom_boxplot(aes(colour = Hei_Range), varwidth = TRUE)

#Age range
ggplot(data = sheet_1, aes(BMI_range,Weight)) +
  geom_boxplot(aes(colour = Age_Range), varwidth = TRUE)


#----Height range
ggplot(data = sheet_1, aes(BMI_range,Age)) +
  geom_boxplot(aes(colour = Hei_Range), varwidth = TRUE)

#filter age less than 60
Filter<-filter(sheet_1, Age<60)
#plot information about BMI and Age
ggplot(data = Filter, aes(BMI_range,Age)) +
  geom_boxplot(aes(colour = Hei_Range), varwidth = TRUE)

#BMI vs Salary > very distributed, we don't have an specific group with high level of BMI
ggplot(data = sheet_1, aes(BMI_range,Weight)) +
  geom_boxplot(aes(colour = SAL_Range), varwidth = TRUE)

#BMI vs CriminalRecord > very distributed, we don't have an specific group with high level of BMI
ggplot(data = sheet_1, aes(BMI_range,Weight)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

#BMI vs EducationLevel > 
ggplot(data = sheet_1, aes(BMI_range,Weight)) +
  geom_boxplot(aes(colour = EducationLevel), varwidth = TRUE)


#-------------------------------------------------------------------------------
# show information about criminal record / Height and Weight
data %>%
  arrange(desc(net_t)) %>%
  ggplot(aes(x=age, y=net_t_v, size=gross_t, color=gender)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 5), name='Weight')

ggplot(sheet_1, aes(x = Age, y = Height, colour = CriminalRecord, size = Weight)) +
  geom_point()

ggplot(sheet_1, aes(x = Weight, y = Height, colour = CriminalRecord, size = Age)) +
  geom_point()

#----------------------------------------------------------------------------------------
#information about Healt
# it show a grouped sample of health "one" onder than 60
ggplot(data, aes(x = age, y = net_t, colour = canceled_t, size = gross_t)) +
  geom_point()

#---------slicing by Health=="One" and "two"------------------------------------------------------------------------

colnames(data)

Filter<-filter(FinalDataSet, Health=="One")
#Health vs weight
ggplot(Filter, aes(x = Age, y = Height, colour = Health, size = Weight)) +
  geom_point()
Filter<-filter(FinalDataSet, Health=="Two")
#Health vs weight
ggplot(Filter, aes(x = Age, y = Height, colour = Health, size = Weight)) +
  geom_point()

#------------slicing by Health=="One" and "two" and Age>50---------------------------------------------


Filter<-filter(FinalDataSet, Health=="One" & Age>50)
#Health vs weight
ggplot(Filter, aes(x = Age, y = Height, colour = Health, size = Weight)) +
  geom_point()
Filter<-filter(FinalDataSet, Health=="Two" & Age>50)
#Health vs weight
ggplot(Filter, aes(x = Age, y = Height, colour = Health, size = Weight)) +
  geom_point()

#-----------------------------------------------------------------------------------



#CriminalRecord vs weight vs Height
#no correlation
ggplot(FinalDataSet, aes(x = Age, y = Height, colour = CriminalRecord, size = Weight)) +
  geom_point()


ggplot(sheet_1, aes(x = Weight, y = Height, color = CriminalRecord)) +
  geom_point() +
  facet_grid(~ CriminalRecord)

#linear model comparing weight and Height
# there is a smooth incresing / positive correlation / as taller the sample heavier it will be
ggplot(sheet_1, aes(x = Height  , y = Weight)) +
  geom_point(aes(color = CriminalRecord)) +
  geom_smooth(method = 'lm')

#boxplot about range of age and Height separeted by criminal record
ggplot(data = sheet_1, aes(Age_Range, Height)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

#separeting the dataset by criminal record = one
ggplot(data = sheet_1[sheet_1$CriminalRecord=='One',], aes(Age_Range, Height)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

#====FinalDataSet
#Criminal range / Weight and Age range
ggplot(data = FinalDataSet, aes(Age_Range, Weight)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

#Criminal range / Height and Age range
ggplot(data = FinalDataSet, aes(Age_Range, Height)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

#health / Weight and Age
ggplot(data = FinalDataSet, aes(Age_Range, Weight)) +
  geom_boxplot(aes(colour = Health), varwidth = TRUE)

#Health / Height and Age range
ggplot(data = FinalDataSet, aes(Age_Range, Height)) +
  geom_boxplot(aes(colour = Health), varwidth = TRUE)



#====sheet_1 Age_Range / CriminalRecord / Height
ggplot(data = sheet_1, aes(Age_Range, Height)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)

#====sheet_1 Age_Range / CriminalRecord / Height
ggplot(data = sheet_1, aes(Age_Range, Weight)) +
  geom_boxplot(aes(colour = CriminalRecord), varwidth = TRUE)



# Salary range / Height and Age_Range
# all samples from age between 54 - 74 have the same mean of Height

ggplot(data = sheet_1, aes(Age_Range,Height )) +
  geom_boxplot(aes(colour = SAL_Range), varwidth = TRUE)

# Salary range / Weight and Age_Range
#age more than 74 and salary between 0 - 11440 have a low mean of Weight
ggplot(data = sheet_1, aes(Age_Range,Weight)) +
  geom_boxplot(aes(colour = SAL_Range), varwidth = TRUE)

## Hei_Range / Weight and Age_Range
ggplot(data = sheet_1, aes(Age_Range,Weight)) +
  geom_boxplot(aes(colour = Hei_Range), varwidth = TRUE)


## EducationLevel / Height and Age_Range
ggplot(data = sheet_1, aes(Age_Range,Height)) +
  geom_boxplot(aes(colour = EducationLevel), varwidth = TRUE)

#=====EducationLevel = secondary/ Age_Range /Weight`
#filter just by EducationLevel=="FALSE"
Filter<-filter(sheet_1, EducationLevel=="FALSE")
ggplot(data = Filter, aes(Age_Range,Weight)) +
  geom_boxplot(aes(colour = EducationLevel), varwidth = TRUE)

#filter just by EducationLevel=="FALSE" / Weight
#negative correlation
ggplot(Filter, aes(x =Age, y =  Weight)) +
  geom_point(aes(color = EducationLevel)) +
  geom_smooth(method = 'lm')

# 
#filter just by EducationLevel=="FALSE" / Height
#almost no correlation
ggplot(data = Filter, aes(Age_Range,Height)) +
  geom_boxplot(aes(colour = EducationLevel), varwidth = TRUE)

#filter just by EducationLevel=="FALSE" / Height
#almost no correlation
ggplot(Filter, aes(x =Age, y =  Height)) +
  geom_point(aes(color = EducationLevel)) +
  geom_smooth(method = 'lm')

#=====EducationLevel = secondary/ Age_Range /Height
#positive correlation
Filter<-filter(sheet_1, EducationLevel=="secondary")
ggplot(data = Filter, aes(Age_Range,Height)) +
  geom_boxplot(aes(colour = EducationLevel), varwidth = TRUE)


ggplot(Filter, aes(x =Age, y =  Height)) +
  geom_point(aes(color = EducationLevel)) +
  geom_smooth(method = 'lm')

#boxplots EducationLevel

ggplot(data = sheet_1, aes(Age_Range,Height)) +
  geom_boxplot(aes(colour = EducationLevel), varwidth = TRUE)

#EducationLevel=="FALSE" >> negative correlation
ggplot(filter(sheet_1, EducationLevel=="FALSE"), aes(x =Age, y =  Weight)) +
  geom_point(aes(color = EducationLevel)) +
  geom_smooth(method = 'lm')



#CriminalRecord=="Two" >> positive correlation
ggplot(filter(sheet_1, CriminalRecord=="Two"), aes(x =Age, y =  Height)) +
  geom_point(aes(color = CriminalRecord)) +
  geom_smooth(method = 'lm')

#CriminalRecord=="One" >> no correlation
ggplot(filter(sheet_1, CriminalRecord=="One"), aes(x =Age, y =  Height)) +
  geom_point(aes(color = CriminalRecord)) +
  geom_smooth(method = 'lm')

#FinalDataSet  positive correlation
Filter<-filter(FinalDataSet, Health!="NA")
ggplot(Filter, aes(x =Weight, y =   Height)) +
  geom_point(aes(color = Health)) +
  geom_smooth(method = 'lm')


##########################################################################

# 11. Generate a smaller data frame for the subjects where health related data is available. 
# Examine if there is a relationship between the different states of health and height, weight 
# or age. Use appropriate visualisations. Note this should include a modelling type analysis such as regression. 
# (S. Weisberg. Applied Linear Regression. Wiley Series in Probability and Statistics, 2005. may be useful)
# # 

# bubble plot comparing Salary/Criminal record/ Age and Height
# we can see there is a concentration of criminal record two after age 60
# most of criminal record two are more than 60 years old
#---------------Salary------------------------
Filter<-filter(FinalDataSet, Health!="NA")
Filter %>% # data set Filter
  arrange(desc(Height)) %>% # Arrange variable Height 
  mutate(Health = factor(Health)) %>% #turn criminal record in factor and a new variable with mutate function
  ggplot(aes(x=Age, y=Salary, size=Height, color=Health)) + #plot graph with variables
  geom_point(alpha=0.5) + # level of transparence
  scale_size(range = c(.1, 12), name="Height(m)") #size of buble

#----------------Health--------------------------------
Filter<-filter(FinalDataSet, Health!="NA")
Filter %>% # data set Filter
  arrange(desc(Height)) %>% # Arrange variable Height 
  mutate(Health = factor(Health)) %>% #turn criminal record in factor and a new variable with mutate function
  ggplot(aes(x=Age, y=Height, size=Salary, color=Health)) + #plot graph with variables
  geom_point(alpha=0.5) + # level of transparence
  scale_size(range = c(.1, 12), name="Height(m)") #size of buble


#---------------------------------------------------------------------
#filter by Health different from NA
Filter<-filter(FinalDataSet, Health!="NA")
#Create a densety graph
ggplot(data=Filter, aes(x=Age, group=Health, fill=Health)) +
  geom_density(adjust=1.5, alpha=.4) + # level of transparence
  theme_ipsum()
#this graph shows health = 1 increasing with age and in the opposite direction health = 2 decreasing

#---------------------------------------------------------------------
#filter Health different from "NA"
Filter<-filter(FinalDataSet, Health!="NA")
#create a boxplot with information from Age and Health
ggplot(Filter, aes(y=Age)) + geom_boxplot(aes(fill=Health))

#filter Health different from "NA"
Filter<-filter(FinalDataSet, Health!="NA")
# create a graph comparing Health / Age_range / Height
ggplot(data = Filter, aes(Age_Range, Height)) +
  geom_boxplot(aes(colour = Health), varwidth = TRUE)

#filter Health different from "NA"
Filter<-filter(FinalDataSet, Health!="NA")
# create a graph comparing Health / Age_range / Weight
ggplot(data = Filter, aes(Age_Range, Weight)) +
  geom_boxplot(aes(colour = Health), varwidth = TRUE)

#filter Health different from "NA"
Filter<-filter(FinalDataSet, Health!="NA")
# create a graph comparing Health / Age_range / BMI
ggplot(data = Filter, aes(Age_Range, BMI)) +
  geom_boxplot(aes(colour = Health), varwidth = TRUE)



# health Age-------------------------------------------------------------------

Filter<-filter(FinalDataSet, Health!="NA")
ggplot(Filter, aes(x = Weight, y = Height, color = Health)) +
  geom_point() +
  facet_grid(~ Age_Range)

Filter<-filter(FinalDataSet, Health=="One")
ggplot(Filter, aes(x = Weight, y = Height, color = Health)) +
  geom_point() +
  facet_grid(~ Age_Range)

Filter<-filter(FinalDataSet, Health=="Two")
ggplot(Filter, aes(x = Weight, y = Height, color = Health)) +
  geom_point() +
  facet_grid(~ Age_Range)

# Scatter plot of Height(m) / Weight separated by Health in each Age range
Filter<-filter(FinalDataSet, Health!="NA")
ggplot(Filter, aes(x =Height, y = Weight , color = Health)) +
  geom_point() + #build the scatter plot
  facet_grid(~ Age_Range)# create a grid by age range


#Filter information about Weight more than 150 kg and Health different from "NA"
Filter<-filter(FinalDataSet,Weight > 150 & Health!="NA")
ggplot(Filter, aes(x = Height, y = Weight, color = Health)) +
  geom_point() +
  facet_grid(~ Age_Range)

#---------------------------------------------------------

#comparing information from CriminalRecord / Height / Weight
sheet_1 %>%
  arrange(desc(Height)) %>%
   ggplot(aes(x=Age, y=Height, size=Weight, color=CriminalRecord)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 5), name='Weight')

#filter samples Health different from NA
Filter<-filter(FinalDataSet, Health!="NA")
#plot graph about Age /  Health and Height
Filter %>%
  arrange(desc(Age)) %>%
  ggplot(aes(x=Height, y=Age, size=Weight, color=Health)) +
  geom_point(alpha=0.5) 
  scale_size(range = c(.1, 5), name='Weight')



#--------------------------------------------------------------------------------------------

# # comparing percentage in 35-54 ages about weigt (less than 150kg and more than 150kg)
# # slicing FinalDataSet to get just samples whit Weight between ages 35 to 54
# a<-FinalDataSet$Weight[FinalDataSet$Age_Range=="35 - 54" & FinalDataSet$Weight<=150 & FinalDataSet$Health=="healthy" ]
# b<-FinalDataSet$Weight[FinalDataSet$Age_Range=="35 - 54" & FinalDataSet$Weight>150 & FinalDataSet$Health=="healthy"]
# c<-FinalDataSet$Age_Range[FinalDataSet$Age_Range=="35 - 54"]
# 
# length(a)/length(c)*100
# length(b)/length(c)*100
# length(c)
# # percentage from samples less than 150 = 35.48
# # percentage from samples more than 150 = 38.70
# 
# d<-FinalDataSet$BMI[FinalDataSet$Age_Range=="35 - 54" & FinalDataSet$Weight<=150]
# e<-FinalDataSet$BMI[FinalDataSet$Age_Range=="35 - 54" & FinalDataSet$Weight>150]
# 
# mean(d)#  31.64732
# mean(e)#  59.7532

#--------------------------------------------------------------------------------------------


# Scatter plot of Height/Wei_Range and Health
ggplot(FinalDataSet, aes(x =Height, y = Weight , color = Health)) +
  geom_point() +
  facet_grid(~ Wei_Range)

#FinalDataSet Helth
#filter by health = "one"
Filter<-filter(FinalDataSet, Health=="One")

# plot of Weight/Wei_Range and Health
ggplot(Filter, aes(x =Height, y = Weight , color = Health)) +
  geom_point() +
  facet_grid(~ Wei_Range)

#filter by health = "two"
Filter<-filter(FinalDataSet, Health=="Two")
# plot of Weight/Wei_Range and Health
ggplot(Filter, aes(x =Height, y = Weight , color = Health)) +
  geom_point() +
  facet_grid(~ Wei_Range)



Filter<-filter(FinalDataSet, Health!="NA")
ggplot(Filter, aes(x =Height, y =  Weight)) +
  geom_point(aes(color = Health)) +
  geom_smooth(method = 'lm')



#comparing the age range with a height range
ggplot(sheet_1, aes(factor(Age_Range), fill = factor(Hei_Range))) +
  geom_bar(position = position_dodge2(preserve = "single"))

#comparing the age range with a weight range
ggplot(sheet_1, aes(factor(Age_Range), fill = factor(Wei_Range))) +
  geom_bar(position = position_dodge2(preserve = "single"))

#comparing the age range with a salary range
ggplot(sheet_1, aes(factor(Age_Range), fill = factor(SAL_Range))) +
  geom_bar(position = position_dodge2(preserve = "single"))


# compraring age range with DoB range
# there is a correlation because the age is related to the day of the birthday
ggplot(sheet_1, aes(factor(Age_Range), fill = factor(DoB_Range))) +
  geom_bar(position = position_dodge2(preserve = "single"))

#####################################################################################################
#####################################################################################################
#####################################################################################################



install.packages("aod")
library(aod)
library(ggplot2)

# reference
# https://stats.idre.ucla.edu/r/dae/logit-regression/

#slice the FinalDataSet and take only variables Age Height Weight Health
mydata<- FinalDataSet[,c(4,6,7,21)]
mydata<- filter(mydata, Health!="NA") # filter Health without NA values


#normallize data
xtabs(~Age + Health , data = mydata)

#change Health in factor
mydata$Health  <- factor(mydata$Health)

#create gml model with binomial distribution
mylogit <- glm(Health ~ Height + Weight + Age , data = mydata, family = "binomial")


#create a new data frame 
newdata1 <- with(mydata, data.frame(Age = mean(Age), Height = mean(Height), Weight = mean(Weight),rank = factor(4)))

# newdata1
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")


newdata2 <- with(mydata, data.frame(Age = rep(seq(from = 1, to = 90, length.out = 5), 4), 
                                    Height = mean(Height),
                                    Weight = mean(Weight),
                                    rank = factor(rep(1:4, each = 10))))

#combine all information
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})


#plot graph
ggplot(newdata3, aes(x = Age, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                      size = 1)






  
