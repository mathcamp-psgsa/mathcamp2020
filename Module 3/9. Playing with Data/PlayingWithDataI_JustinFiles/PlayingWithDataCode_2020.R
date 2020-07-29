##These are the packages that we need. 
#Step one is to make sure the are installed
install.packages("sjlablled")
install.packages("readstata13")
install.packages("openxlsx")
install.packages("tidyverse")
#Step two is to load them into R 
library(sjlabelled) #to load SPSS .sav files
library(readstata13)#to load STATA .dta files
library(openxlsx) #to lead Excell .xlsx files 
library(tidyverse) #an entire universe of packages great for data science, especially for data managing 

##We need load in our data. 
#The ANES data 
#Load in a .dta file with read.dta13 (This packaged is used for STATA files from STATA13 and newer)
ANESData=read.dta13("ANESShort.dta")

#We also have data in the form of .sav files. If you only have that in the futer here's how to load it: 
#Use read_spss
ANESData_sav=read_spss("ANESShort.sav")

#Add the state population data:
#This is an Excell spreadsheet: use the read.xlsx function
StatePop=read.xlsx("StatePop_2010-19.xlsx")
#Add the FIPS codes data 
#This is a csv file, one of the most common data forms, which we can open with read.csv
FIPSCodes=read.csv("US_State_FIPScodes.csv")

#Check the class of these objects: Should all be "data.frame" (df)
class(ANESData)
class(StatePop)
class(FIPSCodes)

#Familiarize yourself with the ANESData
View(ANESData) #Opens up the data in a new window
names(ANESData) #Prints the name of the variables
length(names(ANESData))#How many variables do we have?
ncol(ANESData) #More efficient way of doing the above
nrow(ANESData) #Number of observations in the data
dim(ANESData) #Prints output of nrow followed by ncol
head(ANESData)#First 6 rows of the data
head(ANESData, 10) #Change to first 10 rows instead
tail(ANESData) #Last 6 rows
summary(ANESData) #All of the variables with some descriptive stats 


View(StatePop)#View the data
names(StatePop)#Note: have more years here than in the ANESData
unique(StatePop$`State/Region`) # $ calls a specific variable. Unique returns all of the possible values 
#The lines below tiddys up our data to just what we can use/need
##Subset our data##
StatePop2=StatePop %>% #Dplyr's %>% operator lets us run multiple functions on the same data w/o having to call them all
    #Quicker and less code intensive
    select(`State/Region`, Census, Estimates.Base, `2016`) %>% #select tells R which variables to keep
    filter(! c(`State/Region`== "United States" |`State/Region`== "Northeast" |`State/Region`== "Midwest" |
                   `State/Region`== "South" |`State/Region`== "West" ))
    #This filter code is a bit more complex: we are telling R to drop the rows where the State/Region cells contain these 
    #Values. On the comuptational level, we are telling filter not (w/ the ! operator) State/Region is the United States, 
    # OR State/Region is the "Northest", etc etc.
View(StatePop2) #We have saved the data as a new object. This is good practice for data work as we can keep the old 
#data frame in case we've made mistakes or need data that we took out before 

View(FIPSCodes)
unique(ANESData$State)#This variable only has the FIPS codes; lets combine these dfs so we have state names in ANES
names(FIPSCodes) #Note the best names; also need to have FIPS code variable same as in ANES 
colnames(FIPSCodes)=c("StateName", "FIPSCode", "StateAbrv") #Change the variable (column) names
names(FIPSCodes)#Check our work! 

Full_DF=left_join(ANESData, FIPSCodes, by="FIPSCode") #This is the code needed to merge data; note that "by" specifies 
#the common comma
#Again, let's check our work:
View(Full_DF)
nrow(Full_DF)#should be the same as ANESData: nrow(ANESData)
is.na(ANESData$FIPSCode)#Did we have missing data in the ANESData state codes? 
#One of the easiest signs that a merge has failed is if there is an excess of NAs that did not exist before
#The above line produces 1180 FALSE lines: too much for R to print and annoying to check manually 
sum(is.na(ANESData$FIPSCode)) #This will "sum" the number of TRUE values in the rows 
sum(is.na(Full_DF$FIPSCode)) #Neither set should have any NAs


#######EXERCISE 1########
##Adding in State Pop 
names(StatePop2)
colnames(StatePop2)=c("StateName","Census","Estimates.Base", "2016") #only want to change 1 so we can copy and paste 
#the others
Full_DF=left_join(Full_DF,StatePop2, by="StateName")



########PART 2#########
Data$Survey="Time Series 2016" #This creates a new column all with the 
#Same value telling us that this is from the 2016 data if we wanted to 
#Add new ANES data later

unique(FullDF$Sex)#Check the current values of our dataset 
FullDF$Sex_Charac=ifelse(FullDF$Sex==1, "Male", "Female") #Create a new variable based on values of an existing variable 
#Using ifelse. Syntax goes ifelse(Condition, IfMet, IfElse/IfNotMet)
#We can check based on the count of each value in the two variables
#Table prints how many times each value appears 
table(FullDF$Sex)
table(FullDF$Sex_Charac)
#Same numbers. Recode successful!

##Can create a new variable when more than 2 values: Who_Inted_Votefor_2016 variable 
unique(FullDF$Who_Inted_Votefor_2016)#Check the values
#Need a nested ifelse statement 
FullDF$Intend2016_Vote=ifelse(FullDF$Who_Inted_Votefor_2016==1, "Hillary Clinton",
                            ifelse(FullDF$Who_Inted_Votefor_2016==2, "Donald Trump",
                                   ifelse(FullDF$Who_Inted_Votefor_2016==3, "Gary Johnson",
                                          ifelse(FullDF$Who_Inted_Votefor_2016==4, "Jill Stein",
                                                 ifelse(FullDF$Who_Inted_Votefor_2016==5, "Other",
                                                        "No Response")))))
#Once again, we can check our work 
table(FullDF$Who_Inted_Votefor_2016)
table(FullDF$Intend2016_Vote)
#Looks good!

##Nested ifelse like above can be a pain: long, big chance for error 
#Can instead use recode, which has a more efficient syntax 
#Let's recode the follow-up to the above, Who_Votefor_2016 
unique(FullDF$Who_Votefor_2016) #Check the values 
#recode's syntax goes recode(Varaible to Recode, assingment of new values separted by commas)
#recode works best when specify the values of the old variable and assigning the new value with =
#Even though we have numeric values in the old values we still need to inlcude them in ' ' 
#recode has a .default argument which we can use as a value for all others: in this case 
#We are telling recode that for any value not 1-5 put the value "No Response" in the new 
#variable 
FullDF$Vote2016=recode(FullDF$Who_Votefor_2016, '1'= "Hillary Clinton",
                      '2'="Donald Trump", "3"="Gary Johnson", '4'="Jill Stein",
                      '5'="Other", .default = "No Response")
#For both recode and if else, if we don't create a new variable, but assign the output of the 
#function into the old variable we would replace the value of the old variable 

########EXERCISE 2########
####2.1####
# Create new variable for Does intend to vote? 3 categories: Yes, No and No Response
unique(FullDF$DoesIntedtoVote)
#With ifelse 
FullDF$IntendToVote=ifelse(FullDF$DoesIntedtoVote==1, "Yes" ,
                           ifelse(FullDF$DoesIntedtoVote==2,"No", 
                                  "No Response"))
#With recode 
FullDF$IntendToVote2=recode(FullDF$DoesIntedtoVote, '1'="Yes", '2'="No", .default = "No Response")

#Check our work
table(FullDF$DoesIntedtoVote)
table(FullDF$IntendToVote)
table(FullDF$IntendToVote2)

####2.2####
#Create new variable for Who voted for in 2016 primary
unique(FullDF$Who_Votefor_2016_Primary)
#With ifelse 
FullDF$PrimaryVote=ifelse(FullDF$Who_Votefor_2016_Primary==1, "Hillary Clinton", 
                          ifelse(FullDF$Who_Votefor_2016_Primary==2, "Bernie Sanders", 
                                 ifelse(FullDF$Who_Votefor_2016_Primary==3, "Another Democrat",
                                        ifelse(FullDF$Who_Votefor_2016_Primary==4, "Donald Trump", 
                                               ifelse(FullDF$Who_Votefor_2016_Primary==5, "Ted Cruz",
                                                      ifelse(FullDF$Who_Votefor_2016_Primary==6, "John Kasich", 
                                                             ifelse(FullDF$Who_Votefor_2016_Primary==7, "Marco Rubio", 
                                                                    ifelse(FullDF$Who_Votefor_2016_Primary==8, "Another Republican",
                                                                           ifelse(FullDF$Who_Votefor_2016_Primary==9, "Another Not Democrat or Republicn",
                                                                                  "No Response")))))))))
#With recode 
FullDF$PrimaryVote2=recode(FullDF$Who_Votefor_2016_Primary, '1'="Hillary Clinton", '2'="Bernie Sanders", '3'="Another Democrat",
                          '4'="Donald Trump", '5'="Ted Cruz", '6'="John Kasich", '7'="Marco Rubio", '8'="Another Republican", 
                          '9'= "Another Not Democrat or Republican", .default = "No Response")
#Check our work 
table(FullDF$Who_Votefor_2016_Primary)
table(FullDF$PrimaryVote)
table(FullDF$PrimaryVote2)

########Part 3#########
#####Frequency Tables#####
#table is a kind of frequency table: shows how much each value appears 
table(FullDF$Vote2016)
#Simply shows the number of times each value appears

#We can make a nicer frequency table with freq in the summarytools package
#Make sure installed, then load
install.packages("summarytools") 
library(summarytools)
#Syntax is simple and relatively the same as above
freq(FullDF$Vote2016)
#Prints a nicer table, and includes more info like percentages.
#Can set the order of the values so that largest is first row, which is common for frequency tables
freq(FullDF$Vote2016, order="freq")
#Weird in this case because we have the high "No Response" value 
#Can control the sort using the "order" argument. 
#First must convert from class "character" to class "factor 
#Characters represent data that are string, where as factors represent categorical data with 
#Fixed responses (which we have here)
class(FullDF$Vote2016)#OG class character
FullDF$Vote2016=as.factor(FullDF$Vote2016)
class(FullDF$Vote2016)#now a factor
#Important part of factors are that they have "levels" or the set of possible responses 
levels(FullDF$Vote2016)
#We can manipulate the order of those levels so they appear how we would like
#In this case, we want the candidates who recieved the most votes first, then the "No Response
#Category 
FullDF$Vote2016=factor(FullDF$Vote2016, levels = c("Hillary Clinton", "Donald Trump", "Gary Johnson",
                                            "Jill Stein", "Other", "No Response"))
#Now can set order to "levels"
freq(FullDF$Vote2016, order="levels")
#We can "tidy up" our table to make it look nicer
#We can remove the NA rows because we do not have any NAs
freq(FullDF$Vote2016, order="levels", report.nas = FALSE)
#This also removes the %Total and %Total Cum columns as w/o NAs these don't contain useful info
#The %Valid columns name is changed to % as "Valid" ususally refers to w/o NAs (ditto for %Cum)
#We can also remove the % Cum column 
freq(FullDF$Vote2016, order="levels", report.nas = FALSE, cumul = FALSE)

###Bivariate###
#Can compare 2 vars
#Lets look at both the Vote2016, how respondent did vote in 2016, with how they intended 
#to vote prior, Intend2016_Vote
#Start by making the same changes to Intend2016_Vote as we did with Vote2016:
FullDF$Intend2016_Vote=as.factor(FullDF$Intend2016_Vote)
FullDF$Intend2016_Vote=factor(FullDF$Intend2016_Vote, levels = c("Hillary Clinton",
                    "Donald Trump", "Gary Johnson","Jill Stein", "Other", "No Response"))


table(FullDF$Intend2016_Vote, FullDF$Vote2016)
#This is an interesting look into how people voted vs intended to vote. 
#This table shows how many people inteded to vote for Donald Trump, and then 
#Voted for whomever in the column 
#A better display of joint frequency would be a crosstab: can us ctable from summarytools 
ctable(FullDF$Intend2016_Vote, FullDF$Vote2016)
#this is similar to table above, but it includes the percentages for each cell. Defaul is for percents to be in the rows
#We can change to columns 
ctable(FullDF$Intend2016_Vote, FullDF$Vote2016, prop="c")
#Crosstabs are most commonly associated with chi squared tests, which test the relationship between two variables 
ctable(FullDF$Intend2016_Vote, FullDF$Vote2016, prop="c", chisq = TRUE)
#As expected there is a statistically significant association between who respondents said they intended to vote for and
#who they actually voted for 

########EXERCISE 3########
####3.1####
#Create a frequency table for who intended to vote in 2016
unique(FullDF$IntendtoVote)
#We recoded this in the last exercise
#Check the codebook for the values
#Can create a frequency table with the numeric values 
freq(FullDF$IntendtoVote, order="freq", report.nas = FALSE, cumul = FALSE)

####3.2####
#Create a crosstab with the above variable and who did vote in 2016
#First recode did vote for consistency: 
FullDF$Voted=ifelse(FullDF$Did_Vote_2016_Presi==1, "Yes" ,
                           ifelse(FullDF$Did_Vote_2016_Presi==2,"No", 
                                  "No Response"))
ctable(FullDF$IntendtoVote, FullDF$Voted, chisq = TRUE)


########Part 4#########
#####Saving Data#####
#Since we've made changes to our dataset, we can re-save it so we can pick it up later 
#Most likely will want an excel or csv: 
#Excell: write.xlsx from openxlsx
write.xlsx(FullDF, "C:/Users/Justin Pierce/OneDrive/Math Camp 2020/FinalData_PlayingwithData.xlsx")
#First type the dataframe you want to save, then the file name you want to save it as
#Automatically saves to the current working directory. If want to save it to a different location
#re-set the working directory, or specificy the full file path in quotes before the file name 
#write.xlsx(FullDF, "FinalData_PlayingwithData.xlsx")
#If the file name is "unique" in that there is no file with that name, a new file will be created
#If the file name is the same as an already existing file, it will over-write that file 

#Saving in the other formats we covered is the same, just with different functions 
#csv: write.csv from utils 
write.csv(FullDF, "FinalData_PlayingwithData.csv")
#Make sure to change the file format or R will complain 

#write_spss with sjlabelled
write_spss(FullDF, "FinalData_PlayingwithData.sav")
#readstata13 has a slightly different naming convention for their function, but is otherwise the same
#save.dta13 with readstate13 
save.dta13(FullDF, "FinalData_PlayingwithData.dta")








