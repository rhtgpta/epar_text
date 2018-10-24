# clear workspace
rm(list = ls())

#getting packages installed/loaded
packages <- c("docxtractr", "tibble", "dplyr", "textreadr", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# load packages
library(docxtractr)
library(tibble)
library(dplyr)
library(textreadr)
library(stringr)

##################### input space ###########################

# set the directory of proposals
dest <- "R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/334/scraping_docs/"

# set a working directory
setwd("R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/334/")

#############################################################

# reading the list of all countries 
list_countries <- read.csv("country_names.csv")

#getting all docx files from the path provided
mydocxfiles <- list.files(path = dest, pattern = "docx",  full.names = FALSE)

# initialize the dataframe
final_df <- data.frame(Proposal_File_Name = character(),
                       Proposal_Title = character(), 
                       Investment_Duration = character(), 
                       Requested_Amount = character(), 
                       Total_Project_Cost = character(), 
                       Organization_Legal_Name = character(), 
                       Organization_Country = character(), 
                       Country_of_Intervention = character(),
                       Region = character(),
                       stringsAsFactors=FALSE)
col_names_df <- colnames(final_df)

# processing documents one-by-one
for (doc_file in mydocxfiles){
  
  # reading a single file
  doc <- docxtractr::read_docx(paste0(dest, doc_file))
  
  # extract all tables from the file
  tables_list <- docx_extract_all_tbls(doc, guess_header = TRUE, preserve = FALSE, trim = TRUE)
  
  # getting a dataframe from the returned object
  # identifying the relevant tables among a set of tables
  # assuming the first word in the relevant table is "Proposal Title"
  len_tables <- length(tables_list)
  idx_relevant_table <- 0
  for (i in 1:len_tables){
    curr_table <- tables_list[i]
    first_word <- curr_table[[1]][[1]][1]
    second_word <- curr_table[[1]][[1]][2]
    # if NA value, next loop iteration
    if (is.na(first_word) == TRUE | is.na(second_word) == TRUE){
      next
    }
    else if ((first_word == "Proposal Title" | second_word == "Proposal Title") == TRUE){
      idx_relevant_table <- i
      break
    }
  }
  # if no value found
  if ((idx_relevant_table == 0) == TRUE){
    print ("Couldn't find the relevant table in the document...")
  }
  
  # getting the dataframe with the relevant table
  df <- as.data.frame(tables_list[idx_relevant_table][[1]])
  
  # getting all relevant information
  # assuming that the answer is saved in the next element of the searched element
  # good assumption based on the structure of the underlying document
  indices <- which(df == 'Proposal Title', arr.ind = TRUE) 
  proposal_title <- df[[indices[2]+1]][indices[1]]
  indices <- which(df == 'Investment Duration (Months)', arr.ind = TRUE)
  investment_duration <- df[[indices[2]+1]][indices[1]]
  indices<- which(df == 'Requested Amount (U.S.$)', arr.ind = TRUE)
  requested_amount <- df[[indices[2]+1]][indices[1]]
  indices <- which(df == 'Total Project Cost (U.S.$)', arr.ind = TRUE)
  total_project_cost <- df[[indices[2]+1]][indices[1]]
  indices <- which(df == 'Organization Legal Name1', arr.ind = TRUE)
  organization_legal_name <- df[[indices[2]+1]][indices[1]]
  indices <- which(df == 'Country', arr.ind = TRUE)
  organization_country <- df[[indices[2]+1]][indices[1]]
  
  # country of intervention is based on the premise that it will be mentioned the most in a document
  # reading the docx file from the textreadr package
  all_words_doc <- textreadr::read_docx(paste0(dest, doc_file))
  
  # getting all words stored in a character vector
  length_vector <- length(all_words_doc)
  all_words_vector <- vector(mode = "character", length = 0)
  for (i in 1:length_vector){
    curr_elem <- all_words_doc[i]
    curr_elem <- strsplit(curr_elem, split = " ")
    # getting all seperate words in the main result vector
    length_single <- length(curr_elem[[1]])
    for (j in 1:length_single){
      curr_word <- curr_elem[[1]][j]
      all_words_vector <- append(all_words_vector, curr_word)
    }
  }
  
  # cleaning the corpus
  cleaned_corpus <- vector(mode = "character", length = 0)
  length_corpus <- length(all_words_vector)
  for (i in 1:length_corpus) {
    curr_elem <- all_words_vector[i]
    # getting only alphabetic characters from the string
    curr_elem <- gsub("[^A-z]","", curr_elem)
    # making all lowercase
    curr_elem <- str_to_lower(curr_elem)
    # trimming whitespaces
    curr_elem <- str_trim(curr_elem)
    # only non_empty characters added to final result
    if (str_length(curr_elem) > 0){
      cleaned_corpus <- append(cleaned_corpus, curr_elem)
    }
  }
  
  # getting the count of all individual words
  count_words <- as.data.frame(table(cleaned_corpus))
  
  # getting the country information
  # assigning an ID to every country
  list_countries$ID <- rownames(list_countries)
  # assigning a count to the occurences of every country
  length_country_df <- dim(list_countries)[1]
  freq_countries <- vector(mode = "numeric", length = 0)
  for (i in 1:length_country_df){
    curr_row <- list_countries[i,]
    # lower case 
    curr_country <- str_to_lower(curr_row$Country)
    # split (if multiple words)
    curr_country <- strsplit(curr_country, split = " ")
    # matching all individual words in the country name to the corpus
    length_country_name <- length(curr_country[[1]])
    freq_vector <- vector(mode = "numeric", length = 0)
    for (j in 1:length_country_name){
      curr_word <- curr_country[[1]][j]
      # trimming whitespaces
      curr_word <- str_trim(curr_word)
      freq_word <- count_words[count_words$cleaned_corpus == curr_word,]$Freq
      # if no count registered, assign zero
      if (length(freq_word) == 0){
        freq_word <- 0}
      freq_vector <- append(freq_vector, freq_word)
    }
    # number of common occurences b/w words of country 
    freq_country <- min(freq_vector)
    freq_countries <- append(freq_countries, freq_country)
  }
  
  # assigning the resultant frequencies to the dataframe
  list_countries$Freq <- freq_countries
  # sorting by frequency
  list_countries <- list_countries[order(-list_countries$Freq),] 
  
  # if topmost country has a non-zero frequency, and not United States
  # reliably found the intervention country (discretion required by the RA)
  top_row <- list_countries[1,]
  if (top_row$Freq != 0 && str_trim(as.character(top_row$Country)) != "United States"){
    intervention_country <- str_trim(as.character(top_row$Country))
    intervention_region <- str_trim(as.character(top_row$Region))
  } else{
    intervention_country <- ""
    intervention_region <- ""
  }
  
  # the final row for every document
  final_row <- c(doc_file, proposal_title, investment_duration, requested_amount, total_project_cost, organization_legal_name, 
                 organization_country, intervention_country, intervention_region)
  print (final_row)
  
  # appending data to the final dataframe
  final_df <- rbind(final_df, t(final_row))
}

# assigning column names to the final data frame 
colnames(final_df) <- col_names_df

# writing the information to a csv file
write.csv(final_df, "scraping_result.csv", row.names = FALSE)

##################################################

