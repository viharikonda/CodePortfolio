#week 2
#Creating a variable
dataset = data.frame(id = 1:8, 
                      sex = c("f", "m", "m", "m", "f", "f", "f", "m"), 
                      score = c(17, 29, 61, 42, 12, 109, 11, 107))

# mean of the gender variable
Gender_score = aggregate(score ~ sex, 
                          FUN = mean, 
                          data = dataset)

# paired t test
dataset.htest = t.test(score ~ sex, 
                       data = dataset)

# saving the file to current working directory
save(dataset, Gender_score, dataset.htest,
     file = "CodePortfolio.RData")

#saving image file to working directory
save.image(file = "CodePortfolioImage.RData")

#loading files back into workspace
load(file = "CodePortfolio.Rdata")
load(file = "CodePortfolioImage.RData")

#removing a dataset from workspace
rm(dataset)

#remove all objects 
#rm(list = ls())

#exporting file as tab delimited file
write.table(x = dataset,
            file = "test.txt",
            sep = "\t")

#reading the table - tab delimited, with headers and in working directory
'''mydata = read.table(file = 'test.txt',    
                     sep = '\t',                 
                     header = TRUE,   
                     stringsAsFactors = FALSE) '''

#reading from web
df1 = read.table(file = 'http://goo.gl/jTNf6P',
                 sep = '\t',
                 header = TRUE)

# Week 3
# EDA
mtcars #generic dataset
names(mtcars) = make.names(names(mtcars)) #column names of dataset
nrow(mtcars) #number of rows
ncol(mtcars) #number of cols
str(mtcars) #structure of the dataset - small descriptive
head(mtcars) #top 5 rows 
tail(mtcars) #bottom 5 rows
table(mtcars$mpg) #to check in general distinct values

# Filtering based on a condition
library(dplyr)
filter(mtcars, mpg == 14.7) %>%
  select(cyl, disp, hp, drat)

# Filtering based on multiple conditions
filter(mtcars, mpg > 11
       & drat > 3.90
       & hp > 110) %>%
  select(cyl, disp, hp, drat)

# how many distinct mpg are represented in the dataset
select(mtcars, mpg) %>% unique %>% nrow 

unique(mtcars$gear) # distinct values in the column
summary(mtcars$carb) # basic descriptives like min, max, median, mean, IQR
quantile(mtcars$carb) #IQR
quantile(mtcars$carb, seq(0, 1, 0.1)) #IQR with step size of 10%

# grouping by the combination of mpg and disp
ranking = group_by(mtcars, mpg, disp) %>%
  summarise(mtcars = mean(hp)) %>%
  as.data.frame %>%
  arrange(desc(mtcars))

head(ranking, 10) # top 10 rows of the ranking df
tail(ranking, 10) # bottom 10 rows of the ranking df

# how many average occurances at carb level
filter(mtcars, mpg > 11 & disp > 150) %>%
  group_by(disp) %>%
  summarise(mtcars = mean(carb))

#to remove the spaces in column names
names(US_EPA_data) <- make.names(names(US_EPA_data))
names(US_EPA_data) # to view the column names

#Summarizing the data 
summary(US_EPA_data)
str(US_EPA_data)
head(US_EPA_data[, c(9:11, 17)]) #to view top few rows of data, 
tail(US_EPA_data[, c(7:11, 17)]) #to view last few rows of data 

# to view data from any particular column
table(US_EPA_data$Pollutant.Standard)

select(US_EPA_data, State.Name) %>% unique %>% nrow #to check how many unique States are listed in the data
unique(US_EPA_data$State.Name) #to list distinct state names

#filter data based on presence of  Completeness.Indicator
library(dplyr)
EPA_Data<-filter(US_EPA_data, Completeness.Indicator == "Y"
                 & State.Name =='California') %>% 
  select(Parameter.Code, Parameter.Name, Pollutant.Standard, Method.Name, Datum,Observation.Count,Observation.Percent,State.Name, Arithmetic.Mean, City.Name, County.Name, CBSA.Name)
View(EPA_Data)

#To summarize any one column
summary(EPA_Data$Observation.Count)
summary(EPA_Data$Observation.Percent, seq(0, 1, 0.1))

#Question 1: Which California city is having more pollution levels
ranking <- group_by(EPA_Data, City.Name, County.Name) %>%
  summarize(EPA_Data = mean(Observation.Percent)) %>%
  as.data.frame %>%
  arrange(desc(EPA_Data))

head(ranking, 10) # to view top 10 california cities with more pollution levels
tail(ranking, 10) # last 10 california cities with less pollution levels

# no of cities from SFO Oakland area in California
filter(EPA_Data, CBSA.Name == "San Francisco-Oakland-Hayward, CA") %>% nrow
data<-filter(EPA_Data, CBSA.Name == "San Francisco-Oakland-Hayward, CA" & City.Name == "Livermore")
data

