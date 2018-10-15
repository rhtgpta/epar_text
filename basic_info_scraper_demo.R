###################################################
# ACTIONS FOR USER ARE IN BLOCKS LIKE THIS 
###################################################

#clear workspace
rm(list = ls())

#getting packages installed/loaded
packages <- c("tm", "SnowballC", "reshape2", "gplots", "ggplot2", "plyr", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#load packages
library("tm")
library("SnowballC")
library("reshape2")
library("gplots")
library("ggplot2")
library("plyr")
library("stringr")

###################################################
# SET PATH TO GRANT DOCUMENTS  
###################################################
# To determine path: 
# 1. Go the location of your text files in Windows Explorer
# 2. Click on the top bar and copy the path 
# 3. Type "getwd()" in the console
# 4. Edit the front of the path from step 2 based off the result of step 3

#Example path: 
dest <- "\\\\netid.washington.edu/wfs/EvansEPAR/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/scraper/text files"

###################################################
# RUN THE WHOLE SCRIPT WITH CTRL + ALT + R 
###################################################

### LOAD TEXT FILES AND CLEAN 
mycorpus <- Corpus(DirSource(dest, pattern = "txt"))

#convert text to lowercase
mycorpus <- tm_map(mycorpus, content_transformer(tolower))

#eliminate extra white spaces
mycorpus <- tm_map(mycorpus, stripWhitespace)



### SAVE USEFUL INFO 
file_names <- c()
for(i in 1:length(mycorpus)) {
  fname = meta(mycorpus[[i]])['id']
  file_names[i] <- fname
}

proposal_titles <- c('Grant Challenges - Women and Girls', 'Empowering Girls to End Child Marriage', 'Promoting female empowerment')
org_legal_name <- c('4-H Ghana','Public Health Institute', 'College of Medicine, Unversity of Ibadan')
org_country <- c('Ghana', 'US','Nigeria')
country_of_intervention <- c('Ghana', 'Malawi', 'Nigeria')
region <- c('Sub-Saharan Africa','Sub-Saharan Africa','Sub-Saharan Africa')



### WRITE TO EXCEL FILE 
df <- cbind(file_names, proposal_titles,org_legal_name,org_country,country_of_intervention,region)
write.csv(df,'prefilled_coding_sheet.csv',append=FALSE)

###################################################
# GO TO CURRENT WORKING DIRECTORY TO FIND PREFILLED CODING SHEET 
###################################################