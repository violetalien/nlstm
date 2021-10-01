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

age <- Corpus(VectorSource(rawdata$age))
income <- Corpus(VectorSource(rawdata$income))
gender <- Corpus(VectorSource(rawdata$gender))
edu3 <- Corpus(VectorSource(rawdata$edu3))
humanmade <- Corpus(VectorSource(rawdata$humanmade))
openanswer <- Corpus(VectorSource(rawdata$openanswer))
concern <- Corpus(VectorSource(rawdata$concern))
efficacy <- Corpus(VectorSource(rawdata$efficacy))
en_translation<- Corpus(VectorSource(rawdata$translation))


corpus_list <- list(age, income, gender, edu3, humanmade, openanswer, concern, efficacy, en_translation)

corpus <- Corpus(VectorSource(x = corpus_list))

# Convert and preprocess text
preprocess_smth <- function(filename, language, data){
  data_preprocessed <- tm_map(data, PlainTextDocument)
  # To lower case
  data_preprocessed <- tm_map(data_preprocessed, tolower)
  # Number Removal 
  data_preprocessed <- tm_map(data_preprocessed, removeNumbers)
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
  # remove punctuation, preserve dashes, but not contractions.
  data_preprocessed <- tm_map(x = data_preprocessed, # apply to all documents
                              FUN = removePunctuation, # removePunctuation() function
                              preserve_intra_word_contractions = FALSE, # remove contractions
                              preserve_intra_word_dashes = TRUE) # keep dashes
  
  # apply stemming
  data_preprocessed <- tm_map(x = data_preprocessed, # apply to all documents
                              FUN = stemDocument, # stemDocument() function
                              language = stemlanguage) # English language stems
  return (data_preprocessed)
}


openanser_preprocessed <- preprocess_smth("openanswer", "norwegian", openanswer)
en_translation_preprocessed <- preprocess_smth("en_translation", "english", en_translation)

# create a Document-Term Matrix (DTM)

dtm_openanswer <- DocumentTermMatrix(openanser_preprocessed)
dtm_en_translation_preprocessed <- DocumentTermMatrix(en_translation_preprocessed)

inspect(dtm_en_translation_preprocessed)
inspect(dtm_openanswer)
dtm_openanswer
# Dimension Reduction

dtm_openanswer_sr <- removeSparseTerms(dtm_openanswer, .999)
findFreqTerms(x = dtm_openanswer_sr, lowfreq = 50)
dtm_en_translation_preprocessed_sr <- removeSparseTerms(dtm_en_translation_preprocessed, .999)
findFreqTerms(x = dtm_en_translation_preprocessed_sr, lowfreq = 50)
# dtm_openanswer_sr

Terms(dtm_openanswer_sr)
Terms(dtm_en_translation_preprocessed_sr)

nTerms(dtm_openanswer_sr)
nTerms(dtm_en_translation_preprocessed_sr)

rowsums <- apply(X = dtm_openanswer_sr, # dataframe to apply the function to
                 MARGIN = 1, # apply to the rows
                 FUN = sum) # apply the sum() function

rowsums_en <- apply(X = dtm_en_translation_preprocessed_sr, # dataframe to apply the function to
                    MARGIN = 1, # apply to the rows
                    FUN = sum) # apply the sum() function

dtm_openanswer_sr_red <- dtm_openanswer_sr[rowsums > 0,] 
dtm_en_translation_preprocessed_sr_red <- dtm_en_translation_preprocessed_sr[rowsums_en > 0,] 


## Apply CTM
ctm_openanswer_sr <- CTM(dtm_openanswer_sr_red, k = 4, control=list(seed=831))
ctm_en_translation_preprocessed_sr <- CTM(dtm_en_translation_preprocessed_sr_red, k = 4, control=list(seed=831))
topic <- topics(ctm_openanswer_sr)
topics_en <- topics(ctm_en_translation_preprocessed_sr)
terms(ctm_en_translation_preprocessed_sr,10)
terms(ctm_openanswer_sr,10)

