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

################ input space ####################

#define the path to grant text files
dest <- "R:/Project/EPAR/Archive/334 - Gender Grand Challenge Portfolio Review/Grant documents/text files"

#setting the work directory
setwd("R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/334/")

#################################################

#reading the csv for the dictionary terms
dict_in <- read.csv("dict_input.csv")
#reading the csv for region information for grants
region_in <- read.csv("region_input.csv")

#set EPAR specific colors
epar_colors1 <-  c("#ffffff", "#e5f0ec", "#8ebfad", "#417361", "#e8e3ee", "#9179af", "#3d2f4f")
epar_colors2 <-  c("#9179af", "#8ebfad")
ggplot_theme <- theme(axis.text.x = element_text(color="#000000", angle=90, hjust=1, size=8), 
    panel.background = element_blank(), panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "gray"), 
    legend.position="top")

#getting all text files from the path provided
mytxtfiles <- list.files(path = dest, pattern = "txt",  full.names = TRUE)

#create the corpus
mycorpus <- Corpus(DirSource(dest, pattern = "txt"))
#cleaning the text corpus
#convert text to lowercase
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
#remove punctuation
mycorpus <- tm_map(mycorpus, removePunctuation) 
#remove common stop words from English
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
#eliminate extra white spaces
mycorpus <- tm_map(mycorpus, stripWhitespace)
#stemming the words to retain just the root
mycorpus <- tm_map(mycorpus, stemDocument)
#removing any number values
mycorpus <- tm_map(mycorpus, removeNumbers)

#create document-term matrix for reference and total word counts
#term document matrix with all terms
tdm_all <- as.matrix(TermDocumentMatrix(mycorpus))
#converting to a dataframe
tdm_df <- as.data.frame(tdm_all)
#add column with row totals
tdm_df <- cbind(tdm_df, total = rowSums(tdm_df))
#resort by most common
tdm_df <- tdm_df[order(-tdm_df$total),] 
#flip data frame: switch axes
tdm_flip <- as.data.frame(t(as.matrix(tdm_df)))
grants <- colnames(tdm_df)
rownames(tdm_flip) <- NULL
tdm_flip <- cbind(grants, tdm_flip)

#getting the dictionaries and legends
dict_words <- as.character(dict_in$word)

#getting the details of legends
name_legends <- as.character(unique(dict_in$bucket))
#adding _words suffix
name_legends <- paste(name_legends, "_words", sep="")
num_legends <- length(name_legends)

#document-term matrix with specific terms
#make matrix with terms and grants
tdm_term <- TermDocumentMatrix(mycorpus, list(dictionary = dict_words, removePunctuation = TRUE)) 
#converting to dataframe
tdm_term_df <- as.data.frame(as.matrix(tdm_term))
#summary of totals as a data frame
sum_term <- as.data.frame(sort(rowSums(tdm_term_df), decreasing=TRUE))
colnames(sum_term) <- c("freq")
sum_term$word <- rownames(sum_term)
rownames(sum_term) <- NULL
sum_term <- sum_term[,c("word", "freq")]

#create a grants vector retaining just text file names
#check if the '.txt' file extension exists in the name of the grant
txt_pattern <- grepl('.txt', grants)
grants2 <- grants[txt_pattern]
#removing the '.txt' extension from the strings
rem_txt <- function(string_val) {
  result <- gsub('.txt', '', string_val)
  return (result)
}

#applying the function to all values in the vector
grants2 <- as.vector(sapply(grants2, FUN = rem_txt, simplify = TRUE))

#flip data frame: switch axes so word counts are the columns
flip_term <- as.data.frame(t(as.matrix(tdm_term)))
rownames(flip_term) <- NULL
flip_term <- cbind(grants=grants2, flip_term)

#create list of words supplied for legend
for (i in 1:num_legends){
  #getting the legend on index
  legend <- as.character(name_legends[i])
  #getting all the names associated with the legend
  #removing the suffix _word
  subset_legend <- unlist(strsplit(legend, split='_', fixed=TRUE))[1]
  words <- dict_in[dict_in$bucket == subset_legend,]
  words <- as.character(words$word)
  #initilizing the sum of the legend
  flip_term[[legend]] <- 0
  #getting the word count of all associated words
  for (j in 1:length(words)){
    word <- words[j]
    flip_term[[legend]] <- flip_term[[word]] + flip_term[[legend]]
  }
}

#data frame for grants and words
sum_term <- flip_term[,append(name_legends, "grants", 0)]

#graph counts of legends between grants
sum_term_melt <- melt(sum_term, id.vars="grants")

#graph 2A: word counts across grants
graph_2A <- ggplot(sum_term_melt, aes(grants, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + ggplot_theme +
  xlab("") + ylab("Count of Document Words") +
  scale_fill_manual(values = c("#9179af", "#8ebfad"), name = "Legend")

#now preparing data for frequencies graph
#removing totals row
tdm_flip2 <- tdm_flip[!(tdm_flip$grants=="total"),]
#add new totals row
flip_term <- cbind(flip_term, "total" = rowSums(tdm_flip2[,-c(1)]))

#calculating frequency for all legends
#vector to store all legend frequencies
freq_vector <- numeric(0)
for (i in 1:num_legends){
  #getting the legend on index
  legend <- as.character(name_legends[i])
  #getting all the names associated with the legend
  #removing the suffix _word
  subset_legend <- unlist(strsplit(legend, split='_', fixed=TRUE))[1]
  subset_legend <- paste0(subset_legend, '_freq')
  #saving the name of the legend frequency variable in the vector
  freq_vector <- c(freq_vector, subset_legend)
  df <- (flip_term[[legend]]/flip_term$total)
  assign(subset_legend, df)
}

#create new data frames with frequencies
#getting the grants column first
freq_df_term <- as.data.frame(tdm_flip2$grants)
colnames(freq_df_term) <- c("grants")
#appending all the frequency terms to the empty dataframe
for (i in 1:length(freq_vector)){
  #getting all legend elements individually
  elem <- get(freq_vector[i])
  freq_df_term <- cbind(freq_df_term, elem)
  #renaming the column to get the actual name stored in frequency vector
  colnames(freq_df_term)[which(names(freq_df_term) == "elem")] <- freq_vector[i]
}

#removing 'txt' from grants
freq_df_term$grants <- as.vector(sapply(freq_df_term$grants, FUN = rem_txt, simplify = TRUE))
#graph frequencies between grants
freq_df_term_melt <- melt(freq_df_term, id.vars="grants")

#graph 2B: word frequencies across grants
graph_2B <-ggplot(freq_df_term_melt, aes(grants, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + ggplot_theme +
  xlab("") + ylab("Relative Frequency of Document Words") +
  scale_fill_manual(values = c("#9179af", "#8ebfad"), name = "Legend")

#redoing graph 2B as seperate regions (if >1 region supplied)
unq_regions <- as.character(unique(region_in$region))
if (length(unq_regions) > 1){
  for (i in 1:length(unq_regions)){
    rgn_name <- unq_regions[i]
    #getting associated grants
    grants_vector <- region_in[region_in$region == rgn_name,]
    grants_vector <- as.character(grants_vector$file_name)
    #converting to a lowercase
    rgn_name <- str_to_lower(rgn_name)
    #converting space with an underscore
    rgn_name <- gsub(" ", "_", rgn_name)
    #suffix _freq for the dataframe
    df_name <- paste0('freq_', rgn_name)
    df <- melt(freq_df_term[freq_df_term$grants %in% grants_vector,], id.vars="grants")
    #assign appropriate name to the dataframe
    assign(df_name, df)
    #creating the graphs
    grph_name <- paste0('graph_', rgn_name)
    grph <- ggplot(get(df_name), aes(grants, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") + ggplot_theme +
      xlab("")+ ylab("Relative Frequency of Document Words") +
      scale_y_continuous(limits = c(0, 0.07)) +
      scale_fill_manual(values = c("#9179af", "#8ebfad"), name = "Legend")
    #assign appropriate name to the graph
    assign(grph_name, grph)
  }
}

#now preparing data for next graph: all grants combined
#total counts for all grants
#selecting the legends 
total_term <- sum_term[name_legends]
total_term <- as.data.frame(colSums(total_term))
colnames(total_term) <- c("count")
total_term$terms <- row.names(total_term)

#graph 2C: word counts for all grants
graph_2C <- ggplot(total_term, aes(terms, count, fill = terms)) + ggplot_theme +
  geom_bar(aes(fill = terms), stat="identity") +
  xlab("") + ylab("Word Count (all proposals)") +
  scale_fill_manual(values = c("#9179af", "#8ebfad")) +
  guides(fill=FALSE)

#prep for next graph: word counts - all grants, word disaggregated
#add totals column
tdm_term_df$total <- rowSums(tdm_term_df)
#add column with terms (from the row names)
tdm_term_df$word <- rownames(tdm_term_df)
#new data frame with only the words and their totals
sel_var <- c("word", "total")
tdm_term_df2 <- tdm_term_df[sel_var]
#sort by totals
tdm_term_df2$word <- factor(tdm_term_df2$word, levels = tdm_term_df2[order(tdm_term_df2$total, decreasing=TRUE), "word" ])

#tagging all words
tdm_term_df2 <- merge(tdm_term_df2, dict_in, by = "word")
#renaming the column
colnames(tdm_term_df2)[which(names(tdm_term_df2) == "bucket")] <- "terms"

#graph 2D: word counts for all grants, disaggregated
graph_2D <- ggplot(tdm_term_df2, aes(word, total, fill = terms)) +
  geom_bar(aes(fill = terms), position = "dodge", stat = "identity") + ggplot_theme +
  xlab("") + ylab("Word Count (all proposals)") + 
  scale_fill_manual(values = c("#9179af", "#8ebfad"), name = "Legend")

#function to write the graphs to disk
#input 1: name of graph, 2: name of the output file
save_graph <- function(graph_name, name_file){
  win.metafile(name_file)
  plot(graph_name)
  dev.off()
}

#saving all graphs
#reading all variables in the environment
all_var <- ls()
all_graphs <- all_var[grepl("graph_", all_var)]    
file_ext <- ".wmf"
for (i in 1:length(all_graphs)){
  grph <- all_graphs[i]
  name_grph <- paste0(getwd(), "/", grph, file_ext)
  save_graph(get(grph), name_grph)
}

#############