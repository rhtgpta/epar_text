# Purpose of script: This script was developed by the Evans School Policy Analysis & Research Group (EPAR) to perform topic modeling 
# from grant proposals around initiatives by Bill & Melinda Gates Foundation. 

# references: https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

# Authors: Rohit Gupta
# Date: 15 March 2019

# Inputs: This script takes as input ---
# docx files that contain propsal documents
# Outputs: a csv files with results of topic modeling exercise

#clear workspace
rm(list = ls())

#getting packages installed/loaded
packages <- c("tm", "textreadr", "stringr", "dplyr", "SnowballC", "topicmodels", "openxlsx")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#load packages
library("tm")
library("textreadr")
library("stringr")
library("dplyr")
library("SnowballC")
library("topicmodels")
library("openxlsx")

###############################################

# reading all the documents
#define the path to grant text files
source <- "R:/Project/EPAR/Working Files/374 - SHG Portfolio Review/Grant Analysis by EPAR/Machine-Assisted Analyses/what_elements_or_pathways_matter/Proposals_Filtered_Supplements/"

#setting the work directory
setwd("R:/Project/EPAR/Working Files/RA Working Folders/Rohit/")

###############################################

# reading all the doc files
# getting all doc files from the path provided
mydocfiles <- list.files(path = source, pattern = "doc",  full.names = FALSE)

# removing temp docx files in the directory (if any) 
doc_files <- vector(mode = "character", length = 0)
for (i in 1:length(mydocfiles)){
  # name of the file read
  name_file <- mydocfiles[i]
  # does not contain tilde (~)
  if (grepl("~", name_file) == FALSE){
    doc_files <- append(doc_files, name_file)
  }
}

# updating the original file
mydocfiles <- doc_files

# words meant to be together
word_stitch <- c("community health projects")

# initialize the daraframe to hold all text data read from documents
corpus_df <- data.frame(doc_id = character(),
                        text = character(), 
                        stringsAsFactors=FALSE)

# reading all the docx files
for (i in 1:length(mydocfiles)){
  
  # detecting the extension
  ext <- strsplit(mydocfiles[i], split = '.', fixed = TRUE)[[1]][2]

  # reading a single file
  if (ext == 'doc'){
    doc <- textreadr::read_doc(paste0(source, mydocfiles[i]))
  }
  else if (ext == 'docx'){
    doc <- textreadr::read_docx(paste0(source, mydocfiles[i]))
  }
  else {
    print ('Unknown file extension!')
  }
  
  # document name
  doc_name <- mydocfiles[i]
  print (doc_name)
  
  # extract all strings from the list with at least 5 seperate words
  # assumption is that a meaningful sentence will have at least 5 words
  
  # looping through the resultant character list
  # initialize the empty vector to hold all the eligible strings
  sentence_str <- ""
  for (j in 1:length(doc)){
    # which element
    elem <- doc[[j]]
    # splitting the character vector on spaces
    len_element <- length(strsplit(elem, "\\s+")[[1]])
    # appending all eligible strings with length > 5
    if (len_element > 5){
      sentence_str <- paste(sentence_str, elem)
    }
    # converting to a character type
    sentence_str <- as.character(sentence_str)
  }
  
  # replacing the words (which need to be together)
  for (k in 1:length(word_stitch)){
    elem_stitch <- word_stitch
    elem_space_rem <- gsub(" ", "", elem_stitch, fixed = TRUE)
    sentence_str <- gsub(elem_stitch, elem_space_rem, sentence_str)
  }
  
  # appending to the corpus dataframe
  corpus_df <- rbind(corpus_df, data.frame(doc_id = doc_name, text = sentence_str))
}

# create corpus from vector
docs <- VCorpus(VectorSource(corpus_df$text))

# start preprocessing
# transform to lower case
docs <-tm_map(docs,content_transformer(tolower))

# remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, """)
docs <- tm_map(docs, toSpace, """)

# remove punctuation
docs <- tm_map(docs, removePunctuation)
# strip digits
docs <- tm_map(docs, removeNumbers)
# remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# remove whitespace
docs <- tm_map(docs, stripWhitespace)
# stem document
docs <- tm_map(docs, stemDocument)

#define and eliminate all custom stopwords
myStopwords <- c("can", "say","one","way","use",
                  "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think","'ve ",
                  "'re ","anoth","put","set","new","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "littl","ever","moreov","though","found","abl",
                  "enough","far","earli","away","achiev","draw",
                  "last","never","brief","bit","entir","brief",
                  "great","lot",
                  "project", "grant", "program", "objectives",
                  "group", "proposal", "purpose", "goal", "summary",
                  "scope", "approach", "narrative", "activiti", 
                  "descri")

docs <- tm_map(docs, removeWords, myStopwords)

# create document-term matrix
dtm <- DocumentTermMatrix(docs)
# convert rownames to filenames
rownames(dtm) <- mydocfiles
# collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
# length should be total number of terms
length(freq)
# create sort order (descending)
ord <- order(freq,decreasing=TRUE)
# list all terms in decreasing order of freq and write to disk
freq[ord]

# set parameters for Gibbs sampling
burnin <- 5000
iter <- 2000
thin <- 500
seed <-list(1,2,10,100,500)
nstart <- 5
best <- TRUE

# number of topics
min_topic_k <- 2
max_topic_k <- 10
topic_k <- seq(min_topic_k, max_topic_k)
num_terms <- 25

# initializing the workbook
wb <- createWorkbook()

# writing results for all k values in an excel sheet
for (i in 1:length(topic_k)){
  
  # current topic value
  k_val <- topic_k[i]
  
  # run LDA using Gibbs sampling
  ldaOut <- LDA(dtm, k_val, method = "Gibbs", control = list(nstart = nstart, seed = seed, best = best,
                                                         burnin = burnin, iter = iter, thin = thin))
  
  # listing the topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  #top terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,num_terms))
  
  #probabilities associated with each topic assignment
  topicProbabilities <- as.data.frame(ldaOut@gamma)
  
  # creating a data frame for lda topics
  lda_topics <- as.data.frame(ldaOut.topics)
  lda_topics$names <- rownames(lda_topics)
  names(lda_topics)[names(lda_topics) == 'V1'] <- 'topic'
  lda_topics <- lda_topics[c('names', 'topic')]
  lda_topics <- cbind(lda_topics, topicProbabilities)
  
  # writing the xlsx
  addWorksheet(wb, paste0("lda_terms_", k_val))
  addWorksheet(wb, paste0("topic_prob_", k_val))
  writeDataTable(wb, sheet = paste0("lda_terms_", k_val), x = as.data.frame(ldaOut.terms))
  writeDataTable(wb, sheet = paste0("topic_prob_", k_val), x = lda_topics)
}
  
# saving the final workbook
saveWorkbook(wb, "topic_modeling_out.xlsx", overwrite = TRUE)