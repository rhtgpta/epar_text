# Purpose of script: This script was developed by the Evans School Policy Analysis & Research Group (EPAR) to extract relevant grant 
# information from a word document. The relevant text is identified by an RA with relevant tags to identify the starting and ending
# of text relevant for analysis. 

# Authors: Rohit Gupta, Namrata Kolla, and Muel Kiel
# Date: 20 November 2018

# Inputs: Doc/Docx documents appropriately tagged to identify relevant text
# Outputs: Creates in-memory character vectors that can be processed further 

###############################################################
# LINES LIKE THESE MARK PLACES WHERE USERS MUST PROVIDE INPUT #
###############################################################


# clear workspace
rm(list = ls())

#getting packages installed/loaded
packages <- c("textreadr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# load packages
library(textreadr)

#############################################
# PROVIDE PATHS TO RELEVANT FILES & FOLDERS #
#############################################

# setting the working directory
setwd("R:/Project/EPAR/Working Files/RA Working Folders/Rohit/")

# path where the files are located
dest <- "R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/section_id/eg_docs_370/"

# getting all the sentences between these tags
start_tag <- "EPAR_TEXTBEGIN_RISK"
end_tag <- "EPAR_TEXTEND_RISK"

#############################################################

#getting all docx and doc files from the path provided
mydocfiles <- list.files(path = dest, pattern = "doc",  full.names = FALSE)

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

# processing documents one-by-one
for (doc_file in mydocfiles){
  
  # determine the name of the document
  doc_name <- gsub("\\..*", "", doc_file)
  
  # determine the type of document (doc vs docx)
  doc_ext <- sub('.*\\.', '', doc_file)
  
  # reading a single file
  if (doc_ext == 'doc'){
    doc <- read_doc(paste0(dest, doc_file))
  }
  else if (doc_ext == 'docx'){
    doc <- read_docx(paste0(dest, doc_file))
  }
  else {
    print ("Incompatible document format encountered. Only Word files (doc, docx) supported!")
  }
  
  # defining a function to extract relevant sentences
  # input of the function is a text block 
  # output of the function is a character vector of extracted text
  extract_text <- function(doc){
    # initializing a vector to store the results
    rtext <- vector(mode = "character")
    for (i in 1:length(doc)){
      # current paragraph read
      curr_par <- doc[i]
      # start tag encountered?
      if (curr_par == start_tag){
        j = i + 1
        # read everything till end tag encountered
        while_flag = TRUE
        while (while_flag){
          xtrct_word <- doc[j]
          # checking the end tag
          if (xtrct_word == end_tag){
            # update the loop file to start from the current index
            while_flag = FALSE
            break
          }
          else{
            rtext <- append(rtext, xtrct_word)
          }
          # updating index
          j = j + 1
        }
        # exit the loop once while breaks
        break
      }
    }
    # return the relevant text between the two tags
    return (rtext)
  }
  
  # loop to detect all the starting tags
  # a document can contain multiple start/end tagging combinations
  # "final text" contains all the extracted text for a particular document
  # initializing a character vector to save all the results
  final_text <- vector(mode = "character")
  for (i in 1:length(doc)){
    curr_par <- doc[i]
    if (curr_par == start_tag){
      # calling the extract function if start tag encountered
      res <- extract_text(doc[i:length(doc)])
      final_text <- append(final_text, res)
    }  
  }
  
  # assign resulting vector to the name of the document
  assign(doc_name, final_text)
}

##################################################