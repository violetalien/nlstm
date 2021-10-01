# install.packages(c("tm", "wordcloud"))
# install.packages("topicmodels")
# install.packages('BioFTF')
# install.packages('ldatuning')


library(DescTools)
library(wordcloud)
library(tm)
library(topicmodels) # LDA, CTM
library(ldatuning)
library(tidytext)
library(dplyr)
library(ggplot2)


########################## FUNCTIONS ######################

inspect_smth <- function(data){
  Abstract(data)
  str(data)
  colnames(data)
}

ggplot_smth <- function(data, filename){
  png(filename = paste(filename, ".png", sep=""), width = 10, height = 4, units = "in", res = 300)
  dev.off()
  
  data %>%
    mutate(term = reorder(term, beta)) %>%
    mutate(topic = paste("Topic #", topic)) %>%
    ggplot(aes(term,beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          axis.text.x = element_text(face = "bold", color = "#993333", 
                                     size = 8, angle = 45)) +
    labs(
      title = "Topic Models",
      caption = "Top Terms by Topic (betas)"
    ) +
    ylab('') +  
    xlab('') + 
    coord_cartesian()
}
plot_smth <- function(filename, data) {
  png(filename = paste(filename, ".png", sep=""), width = 10, height = 4, units = "in", res = 300)
  plot(data, 
     main = filename,
     xlab = filename)
  dev.off()
}

wordcloud_smth <- function(filename, data){
  png(filename = paste(filename, ".png", sep=""), width = 10, height = 4, units = "in", res = 300)
  wordcloud(data, # corpus object
            random.order = FALSE, # most frequent in center
            colors = brewer.pal(8, "Dark2"), # color schema
            max.words = 150) # top 150 terms
  warnings()
  dev.off()
}

#############################################################

# read in your data that is in a spreadsheet form .csv file here)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL,
              help="dataset file name", metavar="character"),
  make_option(c("-a", "--aux"), type="character", default="aux",
              help="Auxiliary output name addition [default= %default]", metavar="character"),
  make_option(c("-p", "--path"), type="character", default="./",
              help="User specific path to the repository [default= %default]", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

datafilename = opt$file
outputaux = opt$aux
global_path= opt$path

# datafilename = "/home/user/repo/data/english-translation/translated_data_en.csv"
# outputaux = "english_translation"
# global_path="/home/user/repo/"


if ( is.null(datafilename) || is.null(outputaux) || is.null(global_path) ){
  print_help(opt_parser)
  quit()
}

# By default, when the R function read.csv reads data into R, the non-numerical data are converted to factors and the values of a vector are treated as different levels a factor. Because text data are the focus of text mining, we should keep the data as characters by setting stringsAsFactors = FALSE. 
rawdata <- read.csv(datafilename, header=T, sep=";", fileEncoding="latin1", stringsAsFactors = FALSE)

rawdata$openanswer <- as.character(rawdata$openanswer)

rawdata <- na.omit(rawdata) # thanks to http://www.statmethods.net/input/missingdata.html



inspect_smth(rawdata)

plot_smth("Age", factor(rawdata$age))
plot_smth("Gender", factor(rawdata$gender))
plot_smth("Income", factor(rawdata$income))
plot_smth("Education", factor(rawdata$edu3))
plot_smth("Concern", factor(rawdata$concern))
plot_smth("Humanmande", factor(rawdata$humanmade))
plot_smth("Efficacy", factor(rawdata$efficacy))



age <- Corpus(VectorSource(rawdata$age))
income <- Corpus(VectorSource(rawdata$income))
gender <- Corpus(VectorSource(rawdata$gender))
edu3 <- Corpus(VectorSource(rawdata$edu3))
humanmade <- Corpus(VectorSource(rawdata$humanmade))
openanswer <- Corpus(VectorSource(rawdata$openanswer))
concern <- Corpus(VectorSource(rawdata$concern))
efficacy <- Corpus(VectorSource(rawdata$efficacy))
en_translation<- Corpus(VectorSource(rawdata$translation))

# OR as en_translation<- Corpus(VectorSource(as.vector(rawdata$translation)))

corpus_list <- list(age, income, gender, edu3, humanmade, openanswer, concern, efficacy, en_translation)

corpus <- Corpus(VectorSource(x = corpus_list))

# Convert and preprocess text
preprocess_smth <- function(filename, language, data){
  data_preprocessed <- tm_map(data, PlainTextDocument)
  # To lower case
  data_preprocessed <- tm_map(data_preprocessed, tolower)
  wordcloud_smth(paste(filename, "preprocessed_", "1_lower", sep =""), data_preprocessed)
  # Number Removal 
  data_preprocessed <- tm_map(data_preprocessed, removeNumbers)
  wordcloud_smth(paste(filename, "preprocessed_", "2_no_numbers", sep =""), data_preprocessed)
  # stop word removal
  stops = stopwords(kind = "SMART")
  stemlanguage = "english"
  if (language == "no"){
    stops = stopwords(kind = language)
    stemlanguage = "norwegian"
  }
  else if (language == "is"){
    stops = stopwords(kind = language)
    stemlanguage = "icelandic"
  }
  data_preprocessed <- tm_map(x = data_preprocessed,  FUN = function(x) removeWords(x, stops))
  # OR data_preprocessed <- tm_map(x = data_preprocessed,  removeWords, stops)
  wordcloud_smth(paste(filename, "preprocessed_", "3_no_stopwords", sep =""), data_preprocessed)
  # remove punctuation, preserve dashes, but not contractions.
  data_preprocessed <- tm_map(x = data_preprocessed, # apply to all documents
                   FUN = removePunctuation, # removePunctuation() function
                   preserve_intra_word_contractions = FALSE, # remove contractions
                   preserve_intra_word_dashes = TRUE) # keep dashes
  
  wordcloud_smth(paste(filename, "preprocessed_", "4_no_punctuation", sep =""), data_preprocessed)
  # apply stemming
  data_preprocessed <- tm_map(x = data_preprocessed, # apply to all documents
                        FUN = stemDocument, # stemDocument() function
                        language = stemlanguage) # English language stems
  wordcloud_smth(paste(filename, "preprocessed_", "5_stemming", sep =""), data_preprocessed)
  return (data_preprocessed)
}


openanser_preprocessed <- preprocess_smth("openanswer", "norwegian", openanswer)
en_translation_preprocessed <- preprocess_smth("en_translation", "english", en_translation)

