#clear workspace
rm(list = ls())

####################################
# SET-UP LIBRARIES

# OPTION 1: First time running on this computer 
  # Installing tm (R text mining package)
    # Need to do work-around version because tm tool is not available for R vers 3.3.3
R.Version()
install.packages('devtools')
library(devtools)
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url) # May have to run twice
install.packages("slam")
library("slam", lib.loc="~/R/win-library/3.3")
  # If this path doesn't work, load "slam" by manually clicking "slam" in the packages tab 
install.packages("tm") # say yes 
library("tm")

  # Installing everything else  
install.packages("SnowballC")
library("SnowballC")
install.packages("reshape2")
library("reshape2")
install.packages("gplots") # type "y"
library("gplots")
install.packages("ggplot2") # type "y"
library("ggplot2")
install.packages("plyr") # Restarting session is OK, might need to run twice 
library("plyr")

# OPTION 2: Not first time running on this computer 
library(devtools)
library("tm")
library("SnowballC")
library("reshape2")
library("gplots")
library("ggplot2")
library("plyr")

####################################
# OTHER SET-UP

#set colors
EPAR_Colors_1 <-  c("#ffffff", "#e5f0ec", "#8ebfad", "#417361", "#e8e3ee", "#9179af", "#3d2f4f")
EPAR_Colors_2 <-  c("#9179af", "#8ebfad")

#Set path to text files

#Manually 
# 1. Create a folder within your project where your docs can be converted into simple text files 
# 2. Copy the path to that folder 

  # Put Path within the quotations below: 
dest <- "R:/Project/EPAR/Working Files/334 - Gender Grand Challenge Portfolio Review/Grant documents/text files"
mytxtfiles <- list.files(path = dest, pattern = "txt",  full.names = TRUE)

####################################
# PICK UP FROM HERE 

#create the corpus
mycorpus <- Corpus(DirSource(dest, pattern = "txt"))

#cleaning
mycorpus <- tm_map(mycorpus, content_transformer(tolower)) #convert text to lowercase
mycorpus <- tm_map(mycorpus, removePunctuation) #remove punctuation
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english")) #remove English common stop words
mycorpus <- tm_map(mycorpus, stripWhitespace) #Eliminate extra white spaces
mycorpus <- tm_map(mycorpus, stemDocument)
mycorpus <- tm_map(mycorpus, removeNumbers)

#Complete document-term matrix for reference and total word counts
#term document matrix with all terms
tdm.all <- TermDocumentMatrix(mycorpus)
m <- as.matrix(tdm.all) #convert it to a matrix
tdm.all.df <- as.data.frame(m) #convert to dataframe
tdm.all.df <- cbind(tdm.all.df, total=rowSums(tdm.all.df)) #add column with row totals
tdm.all.df <- tdm.all.df[order(-tdm.all.df$total),] #resort by most common
#flip data frame
tdm.all.flip<-as.data.frame(t(as.matrix(tdm.all.df))) #flip so that grants and words switch axes
grants <- c("Ghana_4H", "Kenya_BOMA", "Uganda_CARE", "Malawi_CARE", "Burundi_CARE", "India_CINI", 
            "Tanzania_DFS", "India_Harvard", "Nigeria_HPI", "DRC_ICART", "Bangladesh_ICDDF", 
            "India_ICRW", "Cambodia_Keller", "Malawi_PHI", "India_PopulationFoundation", "Niger_PSI", 
            "India_RoomToRead", "Swaziland_SAGAA", "Nigeria_UniversityOfIbadan", "Cambodia_WaterSHED", 
            "SouthAfrica_Wits", "Kenya_ZANA", "total")
rownames(tdm.all.flip) <- NULL
tdm.all.flip <- cbind(grants, tdm.all.flip)

#GENDERED TERMS:

#dictionary of terms
dict_gender <- c("women", "woman", "girl", "wife", "wive", "femal", "men", "man", "boy", "husband", "male")

#document-term matrix with specific terms
tdm.gender <- TermDocumentMatrix(mycorpus, list(dictionary = dict_gender, removePunctuation=TRUE)) #make matrix with terms and grants
inspect(tdm.gender)
m.tdm.gender <- as.matrix(tdm.gender) #convert to matrix
d.tdm.gender <- as.data.frame(m.tdm.gender) #convert to data frame
v.gender <- sort(rowSums(m.tdm.gender),decreasing=TRUE) #summary of total
d.sum.gender <- data.frame(word = names(v.gender),freq=v.gender) #summary of totals as a data frame

#flip data frame so word counts are the columns
flip.gender<-as.data.frame(t(as.matrix(d.tdm.gender))) #flip axes for grants and terms
rownames(flip.gender) <- NULL
grants2 <- c("Ghana_4H", "Kenya_BOMA", "Uganda_CARE", "Malawi_CARE", "Burundi_CARE", "India_CINI", 
            "Tanzania_DFS", "India_Harvard", "Nigeria_HPI", "DRC_ICART", "Bangladesh_ICDDF", 
            "India_ICRW", "Cambodia_Keller", "Malawi_PHI", "India_PopulationFoundation", "Niger_PSI", 
            "India_RoomToRead", "Swaziland_SAGAA", "Nigeria_UniversityOfIbadan", "Cambodia_WaterSHED", 
            "SouthAfrica_Wits", "Kenya_ZANA")
flip.gender <- cbind(grants=grants2, flip.gender)


#create list of female words (Note - error message, but the code still works)
flip.gender$female.words <- flip.gender$women + flip.gender$woman + flip.gender$girl + flip.gender$wife + flip.gender$wive + flip.gender$femal
flip.gender <- cbind(flip.gender, female.words)

#create list of male words (Note - error message, but the code still works)
flip.gender$male.words <- flip.gender$men + flip.gender$man + flip.gender$boy + flip.gender$husband + flip.gender$male
flip.gender <- cbind(flip.gender, male.words)

#create smaller data frame for next graphs
sum.gender <- flip.gender[, c("grants", "female.words", "male.words")]

#sort by height of female.words value
sum.gender$grants <- factor(sum.gender$grants, levels=sum.gender[order(sum.gender$female.words, decreasing=TRUE), "grants"])
#graph counts of male and female words between grants
sum.gender.melt <- melt(sum.gender, id.vars="grants")

#GRAPH 1A: Gender word counts across grants
#graph_1A <- ggplot(sum.gender.melt, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Count of Document Words")+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1A

#prep for next graph - frequencies
tdm.all.flip2 <- tdm.all.flip[-23,] #remove totals row
flip.gender <- cbind(flip.gender, "total" =rowSums(tdm.all.flip2[-1])) #add new totals row

female.freq <- flip.gender$female.words/flip.gender$total #calculate frequency
male.freq <- flip.gender$male.words/flip.gender$total

freq.df.gender <- as.data.frame(cbind(female.freq, male.freq)) #create new data frames with frequencies
freq.df.gender <-cbind(freq.df.gender, "grants"=tdm.all.flip2$grants)

#sort by height of female.words value
freq.df.gender$grants <- factor(freq.df.gender$grants, levels=freq.df.gender[order(freq.df.gender$female.freq, decreasing=TRUE), "grants"])
#graph frequencies of male and female words between grants
freq.df.gender.melt <- melt(freq.df.gender, id.vars="grants")

#GRAPH 1B: Gender word frequencies across grants
#graph_1B <-ggplot(freq.df.gender.melt, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Relative Frequency of Document Words")+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1B

#REDO GRAPH 1B as three graphs for three regions
#East Africa
#freq.gender.east <- melt(freq.df.gender[c(2:5, 7, 10, 14, 18, 21:22),], id.vars="grants")
#graph_1B_east <- ggplot(freq.gender.east, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Relative frequency of words")+
#  scale_y_continuous(limits = c(0, 0.05))+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1B_east
#West Africa
#freq.gender.west <- melt(freq.df.gender[c(1,9,16,19),], id.vars="grants")
#graph_1B_west <- ggplot(freq.gender.west, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Relative frequency of words")+
#  scale_y_continuous(limits = c(0, 0.05))+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1B_west
#Asia
#freq.gender.asia <- melt(freq.df.gender[c(6, 8, 11:13, 15, 17, 20),], id.vars="grants")
#graph_1B_asia <- ggplot(freq.gender.asia, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Relative frequency of words")+
#  scale_y_continuous(limits = c(0, 0.05))+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1B_asia


#prep for next graph - all grants combined
total.gender <- colSums(sum.gender[,2:3]) #total counts for all grants
total.gender <- as.data.frame(as.matrix(total.gender)) #make it a data frame
total.gender <- cbind(total.gender, gender=c("female.words", "male.words")) #add male words and female words

#GRAPH 1C: Gender word counts for all grants
#graph_1C <- ggplot(total.gender, aes(gender, V1)) +
#  geom_bar(aes(fill=gender), stat="identity", color=EPAR_Colors_2, fill=EPAR_Colors_2)+
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=16),panel.background=element_blank())+
#  xlab("")+
#  ylab("Word count, all proposals")+
#  guides(fill=FALSE)
#graph_1C

#prep for next graph: Gender word counts - all grants, word disaggregated
d.tdm.gender$total <- rowSums(d.tdm.gender) #add totals column
d.tdm.gender$word <- rownames(d.tdm.gender) #add column with terms (from the row names)
d.tdm.gender2 <- d.tdm.gender[, c(23:24)] #new data frame with only the last two columns

#sort by height
d.tdm.gender2$word <- factor(d.tdm.gender2$word, levels=d.tdm.gender2[order(d.tdm.gender2$total, decreasing=TRUE), "word" ])
d.tdm.gender2$gender <- c("male", "female", "female", "male", "male", "male", "male", "female", "female", "female", "female")


#GRAPH 1D: Gender word counts for all grants, disaggregated
graph_1D <- ggplot(d.tdm.gender2, aes(word, total, fill=gender)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=14),panel.background=element_blank())+
  xlab("")+
  ylab("Word count, all proposals")+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_1D

# of grants by term
d.tdm.gender$grant.num <- c(17, 22, 22, 14, 20, 8, 20, 9, 9, 13, 22)
#make a new dataset for the only vars we care about now - word and grant.num
gender.docs <-  d.tdm.gender[,24:25]
#label word as male or female
gender.docs$gender <- c("male", "female", "female", "male", "male", "male", "male", "female", "female", "female", "female")
#sort height by value of grant.num variable
gender.docs$word <- factor(gender.docs$word, levels=gender.docs[order(gender.docs$grant.num, decreasing=TRUE), "word"])


#GRAPH 1E: Grant counts for all terms, disaggregated (1 mention)
#graph_1E <- ggplot(gender.docs, aes(word, grant.num, fill=gender)) +
#  geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=14),panel.background=element_blank())+
#  xlab("")+
#  ylab("Number of proposals")+
#  guides(fill=FALSE)+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1E



# of grants by term - at least 3 mentions
d.tdm.gender$grant.num3 <- c(13, 19, 18, 9, 16, 4, 18, 5, 3, 6, 22)
#make a new dataset for the only vars we care about now - word and grant.num3
gender.docs3 <-  d.tdm.gender[,c(24,26)]
#label word as male or female
gender.docs3$gender <- c("male", "female", "female", "male", "male", "male", "male", "female", "female", "female", "female")
#sort height by value of grant.num variable
gender.docs3$word <- factor(gender.docs3$word, levels=gender.docs3[order(gender.docs3$grant.num3, decreasing=TRUE), "word"])

#GRAPH 1F: Grant counts for4 all terms, disaggregated
graph_1F <- ggplot(gender.docs3, aes(word, grant.num3, fill=gender)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=14),panel.background=element_blank())+
  xlab("")+
  ylab("Number of pocuments")+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_1F




#ANOTHER OPTION
#Resort by creating factors from country name
freq.df.gender$grants <- as.character(freq.df.gender$grants) #first convert it back to a character string
freq.df.gender$grants <- as.factor(freq.df.gender$grants) #then make it a factor (in order alphabetically)
#graph frequencies of male and female words between grants
freq.df.gender.melt <- melt(freq.df.gender, id.vars="grants")
#GRAPH 1B: Gender word frequencies across grants
graph_1B <-ggplot(freq.df.gender.melt, aes(grants, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
  xlab("")+
  ylab("Relative frequency of words")+
  scale_y_continuous(limits = c(0, 0.05))+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_1B
#Sub-Saharan Africa
freq.gender.africa <- melt(freq.df.gender[c(1:5, 7, 9:10, 14, 16, 18:19, 21:22),], id.vars="grants")
graph_1B_africa <- ggplot(freq.gender.africa, aes(grants, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=9),panel.background=element_blank())+
  xlab("")+
  ylab("Relative frequency of words")+
  scale_y_continuous(limits = c(0, 0.05))+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_1B_africa
#East Africa
#freq.gender.east <- melt(freq.df.gender[c(2:5, 7, 10, 14, 18, 21:22),], id.vars="grants")
#graph_1B_east <- ggplot(freq.gender.east, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Relative frequency of words")+
#  scale_y_continuous(limits = c(0, 0.05))+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1B_east
#West Africa
#freq.gender.west <- melt(freq.df.gender[c(1,9,16,19),], id.vars="grants")
#graph_1B_west <- ggplot(freq.gender.west, aes(grants, value)) +   
#  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
#  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+
#  xlab("")+
#  ylab("Relative frequency of words")+
#  scale_y_continuous(limits = c(0, 0.05))+
#  scale_fill_manual(values=c("#9179af", "#8ebfad"))
#graph_1B_west
#Asia
freq.gender.asia <- melt(freq.df.gender[c(6, 8, 11:13, 15, 17, 20),], id.vars="grants")
graph_1B_asia <- ggplot(freq.gender.asia, aes(grants, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=9),panel.background=element_blank())+
  xlab("")+
  ylab("Relative frequency of words")+
  scale_y_continuous(limits = c(0, 0.05))+
  scale_fill_manual(values=c("#9179af", "#8ebfad"))
graph_1B_asia


