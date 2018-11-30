# clear workspace
rm(list = ls())

# loading packages
library(rvest)
library(stringi)
library(stringr)

# setting the work directory
setwd("R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/334/")

# downloading the top 10 results (within "web_results" folder)
dir.create("web_results")

# reading the file with urls
url_file <- read.csv("web_input_search_terms.csv")
colnames(url_file) <- "urls"

# taking urls one-by-one
len_url <- length(url_file$urls)
for (i in 1:len_url){
  
  url <- as.character(url_file[i,])
  # making the url in the right format
  # only retreiving pdf filetypes for the search query
  squery <- str_replace_all(url,"[\\s]+","+")
  squery <- paste0(squery, "+filetype:pdf")
  squery <- paste0("https://www.google.co.in/search?q=", squery)
  
  # getting cleaned results from the first page
  html_s <- read_html(squery)
  vector_links <- html_s %>% html_nodes(xpath='//h3/a') %>% html_attr('href')
  vector_links <- gsub('/url\\?q=','',sapply(strsplit(vector_links[as.vector(grep('url', vector_links))],split='&'),'[',1))
  
  # appending url string in front of every file
  file_url <- str_replace_all(url,"[\\s]+","_")
  file_url <- paste0(file_url, '#$')
  
  # downloading the results now
  down_path <- paste0(getwd(), "/web_results")
  for (myurl in vector_links) {
    filename <- tail(unlist(strsplit(myurl, "/", fixed = TRUE)), n = 1)
    download.file(myurl, destfile = paste0(down_path, "/", file_url, filename), method = 'curl')
    # sleep for 1 second
    Sys.sleep(1)
  }
}

# reading the downloaded files
pdf_path <- paste0(getwd(),'/web_results/') 
pdf_files <- list.files(path = pdf_path, pattern = ".pdf$",  full.names = FALSE)

# removing the files smaller than 10 KB
pdf_size <- list.files(path = pdf_path, pattern = ".pdf$",  full.names = TRUE)
files_keep <- pdf_size[sapply(pdf_size, file.size) > 10000]
all_files <- list.files(path = pdf_path, full.names = TRUE)
files_remove <- all_files[-pmatch(files_keep,all_files)]
for (i in files_remove){
  if (file.exists(i)) {
    file.remove(i)
  }
}

# reading all pdf files afresh
pdf_files <- list.files(path = pdf_path, pattern = ".pdf$",  full.names = FALSE)

# splitting the file names to seperate file name and file names
file_split <- stri_split_fixed(pdf_files, "#$")
queries <- vector(mode="character")
files <- vector(mode="character")
for (i in 1:length(file_split)){
  query_l <- file_split[[i]][1]
  file_l <- file_split[[i]][2]
  # appending both things to the initialized vectors
  queries <- append(queries, query_l)
  files <- append(files, file_l)
}

# for every query we assign associated files
unique_queries <- unique(queries)
unique_files <- unique(files)

for (i in 1:length(unique_queries)){
  assign(paste0('query',i), unique_queries[i])
  match_indexes <- which(queries %in% get(paste0('query',i)))
  assign(paste0('file',i), files[min(match_indexes):max(match_indexes)])
  assign(paste0('vec_',i), vector())
}

# for every unique file, does it appear for the query? 
for (i in 1:length(unique_files)){
  curr_file <- unique_files[i]
  # going through all files
  for (j in 1:length(unique_queries))
  {
    assign(paste0('present',j), (curr_file %in% get(paste0('file',j))))
    temp_flag <- append(get(paste0('vec_',j)), get(paste0('present',j)))
    assign(paste0('vec_',j), temp_flag*1)
  }
} 

# creating a matrix to indicate if a file is attached to a query
# creating a data frame with the requisite information
# initializing the data frame with the first query results
df <- as.data.frame(cbind(unique_files, vec_1))
# renaming the column for the added query result
names(df)[2] <- query1
for (i in 2:length(unique_queries)){
  # adding the subsequent columns
  df <- cbind(df, get(paste0('vec_',i)))
  # renaming the columns
  names(df)[i+1] <- get(paste0('query',i))
}

# cleaning up the downloaded files
# create a new folder (unique_files)
dir.create("unique_files")
# copying files to the new location
file.copy(file.path(pdf_path, pdf_files), paste0(getwd(),"/unique_files"))

# keeping only the unique files
old_names <- list.files(paste0(getwd(),"/unique_files"), full.names = FALSE)
# splitting the file names to seperate file name and file names
file_split <- stri_split_fixed(old_names, "#$")
new_names <- vector(mode="character")
unq_file_names <- vector(mode="character")
for (i in 1:length(file_split)){
  file_l <- file_split[[i]][2]
  # appending both things to the initialized vectors
  new_names <- append(new_names, file_l)
}

# "uniqueify" the files in the directory
file.rename(from = file.path(paste0(getwd(),"/unique_files"), old_names), to = file.path(paste0(getwd(),"/unique_files"), new_names))

# remove the web results folder
unlink("web_results", recursive = TRUE)

# prioritize the resultant dataframe
temp_df <- df
temp_df$unique_files <- NULL
# converting all columns into numeric
num_col <- dim(temp_df)[2]
for (i in 1:num_col){
  temp_df[,i] <- as.numeric(as.character(temp_df[,i]))
}

# sorting documents (appear most queries)
sum_rows <- rowSums(temp_df)
df$commonality <- sum_rows
df <- df[order(-df$commonality),] 
df$commonality <- NULL