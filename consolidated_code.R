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

#set path to text files; NK: Can we make it more obvious that this is a place where we need people's input? E.g. 
###############################################################################
# ACTION: PUT ALL GRANT DOCUMENTS IN A FOLDER AND SET PATH TO THAT FOLDER HERE 
###############################################################################
dest <- "R:/Project/EPAR/Archive/334 - Gender Grand Challenge Portfolio Review/Grant documents/text files"
# NK: I think this should be reading from "ALL PROPOSALS" folder, the proposals come in as .doc rather than .txt files

#set EPAR specific colors
epar_colors1 <-  c("#ffffff", "#e5f0ec", "#8ebfad", "#417361", "#e8e3ee", "#9179af", "#3d2f4f")
epar_colors2 <-  c("#9179af", "#8ebfad")

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

#dictionary of age terms
dict_age <- c("child", "children", "boy", "girl", "student", "adolesc", "teen", "youth",
              "man", "men", "woman", "women", "spous", "husband", "wife", "wive", "parent",
              "mother", "father", "entrepreneur", "farmer", "coupl", "partner")

#document-term matrix with specific terms
#make matrix with terms and grants
tdm_age <- TermDocumentMatrix(mycorpus, list(dictionary = dict_age, removePunctuation = TRUE)) 
#converting to dataframe
tdm_age_df <- as.data.frame(as.matrix(tdm_age))
#summary of totals as a data frame
sum_age <- as.data.frame(sort(rowSums(tdm_age_df),decreasing=TRUE))
colnames(sum_age) <- c("freq")
sum_age$word <- rownames(sum_age)
rownames(sum_age) <- NULL
sum_age <- sum_age[,c("word", "freq")]

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
flip_age <- as.data.frame(t(as.matrix(tdm_age)))
rownames(flip_age) <- NULL
flip_age <- cbind(grants=grants2, flip_age)

#create list of 'youth' words
flip_age$youth_words <- (flip_age$child + flip_age$children + flip_age$girl + flip_age$boy +
                        flip_age$student + flip_age$adolesc + flip_age$teen + flip_age$youth)

#create list of 'adult' words
flip_age$adult_words <- (flip_age$men + flip_age$man + flip_age$woman + flip_age$women + flip_age$spous +
                         flip_age$husband + flip_age$wife + flip_age$wive + flip_age$parent + flip_age$mother +
                         flip_age$father + flip_age$entrepreneur + flip_age$farmer + flip_age$coupl + flip_age$partner)

#data frame for grants and youth/adult words
sum_age <- flip_age[, c("grants", "youth_words", "adult_words")]

#sort by height of 'young' words
sum_age$grants <- factor(sum_age$grants, levels = sum_age[order(sum_age$youth_words, decreasing=TRUE), "grants"])
#graph counts of young and old words between grants
sum_age_melt <- melt(sum_age, id.vars="grants")

#graph 2A: age word counts across grants
graph_2A <- ggplot(sum_age_melt, aes(grants, value)) +
 geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
 theme(axis.text.x = element_text(color="#000000", angle=50, hjust=1, size=12), panel.background = element_blank()) +
 xlab("") + ylab("Count of document words") +
 scale_fill_manual(values = c("#9179af", "#8ebfad"))

#now preparing data for frequencies graph
#removing totals row
tdm_flip2 <- tdm_flip[!(tdm_flip$grants=="total"),]
#add new totals row
flip_age <- cbind(flip_age, "total" = rowSums(tdm_flip2[,-c(1)]))
#calculating frequency
youth_freq <- flip_age$youth_words/flip_age$total 
adult_freq <- flip_age$adult_words/flip_age$total
#create new data frames with frequencies
freq_df_age <- as.data.frame(cbind(youth_freq, adult_freq)) 
freq_df_age <-cbind(freq_df_age, "grants" = tdm_flip2$grants)
#removing 'txt' from grants
freq_df_age$grants <- as.vector(sapply(freq_df_age$grants, FUN = rem_txt, simplify = TRUE))
#sort by height of young.freq value
freq_df_age$grants <- factor(freq_df_age$grants, levels = freq_df_age[order(freq_df_age$youth_freq, decreasing=TRUE), "grants"])
#graph frequencies of old and young words between grants
freq_df_age_melt <- melt(freq_df_age, id.vars="grants")

#graph 2B: age word frequencies across grants
graph_2B <-ggplot(freq_df_age_melt, aes(grants, value)) +
 geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
 theme(axis.text.x = element_text(color="#000000", angle=50, hjust=1, size=12), panel.background=element_blank()) +
 xlab("") + ylab("Relative Frequency of Document Words")+
 scale_fill_manual(values=c("#9179af", "#8ebfad"))

#redoing graph 2B as 3 graphs for 3 seperate regions
#create seperate vectors with relevant rows selected in the freq_df_age dataframe
#east africa
east_africa <- c("SAGAA_HIV_swaziland", "CARE_Umodzi_Malawi", "PHI_child marriage_Malawi", "Wits_GAP Year_SouthAfrica", "ZANA_listenup_Kenya", 
"ICART_livelihoods_DR Congo", "BOMA_DecisionMaking_Kenya", "CARE_win-win_Burundi", "CARE_subwallets_Uganda", "DFS_REPOA_Tanzania")
freq_age_east <- melt(freq_df_age[freq_df_age$grants %in% east_africa,], id.vars="grants")
graph_2B_east <- ggplot(freq_age_east, aes(grants, value)) +
 geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
 theme(axis.text.x = element_text(color="#000000", angle=50, hjust=1, size=12), panel.background=element_blank())+
 xlab("")+ ylab("Relative Frequency of Document Words") +
 scale_y_continuous(limits = c(0, 0.07)) +
 scale_fill_manual(values=c("#9179af", "#8ebfad"))

#West Africa
west_africa <- c("5thH_4H_Ghana", "HPI_GirlsforHealth_Nigeria", "PSI_room to grow_Niger", "U-Ibadan_family planning_Nigeria")
freq_age_west <- melt(freq_df_age[freq_df_age$grants %in% west_africa,], id.vars="grants")
graph_2B_west <- ggplot(freq_age_west, aes(grants, value)) +
 geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
 theme(axis.text.x = element_text(color = "#000000", angle=50, hjust=1, size=12), panel.background = element_blank()) +
 xlab("") + ylab("Relative Frequency of Document Words") +
 scale_y_continuous(limits = c(0, 0.07)) +
 scale_fill_manual(values=c("#9179af", "#8ebfad"))

#asia
asia <- c("ICRW_plan-it girls_India", "Room to Read_FirstGirls_India", "ICDDR_menstrual hygiene_Bangladesh",
          "CINI_couple power_India", "Keller_Homestead-food_Cambodia", "Population Foundation_role models_India", 
          "WaterSHED_WASH_Cambodia", "Harvard_smartpayment_India")
freq_age_asia <- melt(freq_df_age[freq_df_age$grants %in% asia,], id.vars="grants")
graph_2B_asia <- ggplot(freq_age_asia, aes(grants, value)) +
 geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
 theme(axis.text.x = element_text(color="#000000", angle=50, hjust=1, size=12), panel.background = element_blank()) +
 xlab("") + ylab("Relative Frequency of Document Words") +
 scale_y_continuous(limits = c(0, 0.07)) +
 scale_fill_manual(values=c("#9179af", "#8ebfad"))

#now preparing data for next graph: all grants combined
#total counts for all grants
#selecting the two columns 
sel_var <- c("youth_words", "adult_words")
total_age <- sum_age[sel_var]
total_age <- as.data.frame(colSums(total_age))
colnames(total_age) <- c("count")
total_age$age <- row.names(total_age)

#graph 2C: age word counts for all grants
graph_2C <- ggplot(total_age, aes(age, count, fill = age)) +
 geom_bar(aes(fill = age), stat="identity") +
 theme(axis.text.x = element_text(color = "#000000", angle=50, hjust=1, size=16), panel.background = element_blank()) +
 xlab("") + ylab("Word count, all proposals") +
 scale_fill_manual(values = c("#9179af", "#8ebfad")) +
 guides(fill=FALSE)

#prep for next graph: age word counts - all grants, word disaggregated
#add totals column
tdm_age_df$total <- rowSums(tdm_age_df)
#add column with terms (from the row names)
tdm_age_df$word <- rownames(tdm_age_df)
#new data frame with only the words and their totals
sel_var <- c("word", "total")
tdm_age_df2 <- tdm_age_df[sel_var]
#sort by totals
tdm_age_df2$word <- factor(tdm_age_df2$word, levels = tdm_age_df2[order(tdm_age_df2$total, decreasing=TRUE), "word" ])
#tagging all youth and adult words seperately
youth_words <- c("boy", "child", "coupl", "girl", "youth", "wive", "adolesc", "father")
adult_words <- c("men", "parent", "partner", "women", "children", "entrepreneur", "husband",
                 "woman", "man", "mother", "wife", "farmer", "student", "teen", "spous")
tdm_age_df2$age <- ifelse(tdm_age_df2$word %in% youth_words, "youth", ifelse(tdm_age_df2$word %in% adult_words, "adult", "n/a"))

#graph 2D: age word counts for all grants, disaggregated
graph_2D <- ggplot(tdm_age_df2, aes(word, total, fill = age)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(color="#000000", angle=50, hjust=1, size=14), panel.background = element_blank()) +
  xlab("") + ylab("Word count, all proposals") + guides(fill=FALSE) +
  scale_fill_manual(values=c("#8ebfad", "#9179af"))

#####################

#saving all the graphs to disk now
graphs_path <- "R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/334" # Set path for the "OUTPUT", move this also up top? 
win.metafile('R:/Project/EPAR/Working Files/372 - EPAR Tools Development/Code/334/iris.wmf')
plot(graph_2A)
dev.off()
