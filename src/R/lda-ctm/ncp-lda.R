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
  png(filename = "/home/user/hello.png", width = 10, height = 4, units = "in", res = 300)
  # png(filename = paste(global_path,filename, ".png", sep=""), width = 10, height = 4, units = "in", res = 300)
  
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
      title = filename,
      # title = "Topic Models",
      caption = "Top Terms by Topic (betas)"
    ) +
    ylab('') +  
    xlab('') + 
    coord_cartesian()
  
   dev.off()
}
plot_smth <- function(filename, data) {
  png(filename = paste(filename, ".png", sep=""), width = 10, height = 4, units = "in", res = 300)
  plot(data, 
       main = filename,
       xlab = filename)
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

# TODO create word cloud of 2-grams of the words and a network graph :)

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



## Apply LDA with 4 topics

lda_mod <- LDA(dtm_openanswer_sr_red, 
               k = 4,
               control = list(seed=831))
lda_topic <- topics(lda_mod)

lda_mod_en <- LDA(dtm_en_translation_preprocessed_sr_red, 
                  k = 4,
                  control = list(seed=831)) # used for reproducibility

# pull the betas: the probabilities of each word being associated with each topic
lda_topics_en_beta <- tidy(lda_mod_en, matrix = "beta")
str(lda_topics_en_beta)

lda_topics_en_top_terms <- 
  lda_topics_en_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # top 5 words associated with that topic
  ungroup() %>%
  arrange(topic, -beta)
str(lda_topics_en_top_terms)

lda_topics_no_beta <- tidy(lda_mod, matrix = "beta")
str(lda_topics_no_beta)

lda_topics_no_top_terms <- 
  lda_topics_no_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # top 10 words associated with that topic
  ungroup() %>%
  arrange(topic, -beta)

data<-filter(lda_topics_en_top_terms, topic ==1)
data<-filter(lda_topics_en_top_terms, topic ==2)
data<-filter(lda_topics_en_top_terms, topic ==3)
data<-filter(lda_topics_en_top_terms, topic ==4)


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
    # title = filename,
    title = "Topic Models",
    caption = "Top Terms by Topic (betas)"
  ) +
  ylab('') +  
  xlab('') + 
  coord_cartesian()

data<-filter(lda_topics_no_beta, topic ==1)
data<-filter(lda_topics_no_beta, topic ==2)
data<-filter(lda_topics_no_beta, topic ==3)
data<-filter(lda_topics_no_beta, topic ==4)
# visualize
# lda_topics_en_top_terms %>%
ggplot_smth(filter(lda_topics_en_top_terms, topic ==1), "Terms of Topic 1 english")
ggplot_smth(filter(lda_topics_en_top_terms, topic ==2), "Terms of Topic 2 english")
ggplot_smth(filter(lda_topics_en_top_terms, topic ==3), "Terms of Topic 3 english")
ggplot_smth(filter(lda_topics_en_top_terms, topic ==4), "Terms of Topic 4 english")

lda_topic_en <- topics(lda_mod_en)

terms(lda_mod,10)
terms(lda_mod_en,5)

# Find number of topics
result <- FindTopicsNumber(
  dtm_openanswer_sr_red,
  topics = seq(from = 3, to = 7, by = 1),
  metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",
  control = list(seed = 831),
  mc.cores = 2L,
  verbose = TRUE)
FindTopicsNumber_plot(result)

lda_mod <- LDA(dtm_openanswer_sr_red, 
               k = 7, 
               # k = 4, 
               control = list(seed=831))
lda_topic <- topics(lda_mod)

lda_mod_en <- LDA(dtm_en_translation_preprocessed_sr_red, 
                  k = 7, 
                  # k = 4, 
                  control = list(seed=831)) # used for reproducibility
# pull the betas: the probabilities of each word being associated with each topic
lda_topics_en_beta <- tidy(lda_mod_en, matrix = "beta")
str(lda_topics_en_beta)

lda_topics_en_top_terms <- 
  lda_topics_en_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # top 5 words associated with that topic
  ungroup() %>%
  arrange(topic, -beta)
str(lda_topics_en_top_terms)

lda_topics_no_beta <- tidy(lda_mod, matrix = "beta")
lda_topics_no_top_terms <- 
  lda_topics_no_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # top 10 words associated with that topic
  ungroup() %>%
  arrange(topic, -beta)

data<-filter(lda_topics_en_top_terms, topic ==1)
data<-filter(lda_topics_en_top_terms, topic ==2)
data<-filter(lda_topics_en_top_terms, topic ==3)
data<-filter(lda_topics_en_top_terms, topic ==4)
data<-filter(lda_topics_en_top_terms, topic ==5)
data<-filter(lda_topics_en_top_terms, topic ==6)
data<-filter(lda_topics_en_top_terms, topic ==7)


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
    # title = filename,
    title = "Topic Models",
    caption = "Top Terms by Topic (betas)"
  ) +
  ylab('') +  
  xlab('') + 
  coord_cartesian()

data<-filter(lda_topics_no_beta, topic ==1)
data<-filter(lda_topics_no_beta, topic ==2)
data<-filter(lda_topics_no_beta, topic ==3)
data<-filter(lda_topics_no_beta, topic ==4)
data<-filter(lda_topics_no_beta, topic ==4)
data<-filter(lda_topics_no_beta, topic ==5)
data<-filter(lda_topics_no_beta, topic ==6)
data<-filter(lda_topics_no_beta, topic ==7)
