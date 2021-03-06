# ncp-stm.R
# Structural topic modeling of open-ended survey responses from the Norwegian Citizen Panel
# Endre Tvinnereim, September 2014-February 2015
# Background: http://structuraltopicmodel.com/ ; _vignette_ "stm: R Package for Structural Topic Models"

# uncomment in nedded
#install.packages("Rtools")
#install.packages("stm")
#if(!require(devtools)) install.packages("devtools")
#install.packages("evaluate")
#install.packages("devtools")
#install_github("bstewart/stm",dependencies=TRUE)
#install.packages("RCurl")
#install.packages("matrixStats")
#install.packages("R.methodsS3")
#install.packages("SnowballC")

########################
library(devtools)
library(R.methodsS3)
library(stm)
library(wordcloud)
library(tm)
library(foreign)

############################## FUNCTIONS ##############################
wordcloud_smth <- function(filename, data){
  png(filename = paste(filename, ".png", sep=""), width = 10, height = 4, units = "in", res = 300)
  wordcloud(data, # corpus object
            random.order = FALSE, # most frequent in center
            colors = brewer.pal(8, "Dark2"), # color schema
            max.words = 150) # top 150 terms
  warnings()
  dev.off()
}
######################################################################

# Clear all
rm(list=ls(all=TRUE))
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
# global_path="/home/user/repo/results/lda"


if ( is.null(datafilename) || is.null(outputaux) || is.null(global_path) ){
  print_help(opt_parser)
  quit()
}

# global_path = '/home/user/repo/'

# read in your data that is in a spreadsheet form .csv file here)

rawdata <- read.csv(paste(datafilename, sep=""), header=T, sep=";", fileEncoding="latin1")
# Make the open answers text not factor
rawdata$openanswer <- as.character(rawdata$openanswer)

# remove missing data
data <- na.omit(rawdata) # thanks to http://www.statmethods.net/input/missingdata.html
rm(rawdata)

# Manual pre-stemming, thanks to Michael Stoffel: 
oa <- data$openanswer
for(i in 1:length(oa)) {
  oa[[i]] <- gsub("frem", "fram", oa[[i]])
  oa[[i]] <- gsub("forurensing", "forurens", oa[[i]])
  oa[[i]] <- gsub("forurensning", "forurens", oa[[i]])
  oa[[i]] <- gsub("smelting", "smelt", oa[[i]])
  oa[[i]] <- gsub("altid", "alltid", oa[[i]])
  oa[[i]] <- gsub("arktisk", "arktis", oa[[i]])
  oa[[i]] <- gsub("bekymring", "bekymr", oa[[i]])
  oa[[i]] <- gsub("bekymringsfullt", "bekymr", oa[[i]])
  oa[[i]] <- gsub("betydning", "bety", oa[[i]])
  oa[[i]] <- gsub("betyr", "bety", oa[[i]])
  oa[[i]] <- gsub("d?d", "d?", oa[[i]])
  oa[[i]] <- gsub("d?r", "d?", oa[[i]])
#  oa[[i]] <- gsub("endring", "endr", oa[[i]])
  oa[[i]] <- gsub("enkelt", "enkel", oa[[i]])
  oa[[i]] <- gsub("ekstremt", "ekstrem", oa[[i]])
  oa[[i]] <- gsub("extrem", "ekstrem", oa[[i]])
  oa[[i]] <- gsub("fleir", "fler", oa[[i]])
  oa[[i]] <- gsub("flomm", "flom", oa[[i]])
  oa[[i]] <- gsub("flomr", "flom", oa[[i]])
  oa[[i]] <- gsub("forandring", "forandr", oa[[i]])
  oa[[i]] <- gsub("fosilt", "fossil", oa[[i]])
  oa[[i]] <- gsub("fossilt", "fossil", oa[[i]])
  oa[[i]] <- gsub("fremtid", "framtid", oa[[i]])
  oa[[i]] <- gsub("globaloppvarming", "global oppvarming", oa[[i]])
  oa[[i]] <- gsub("godt", "god", oa[[i]])
  oa[[i]] <- gsub("h?gar", "h?y", oa[[i]])
  oa[[i]] <- gsub("h?yer", "h?y", oa[[i]])
  oa[[i]] <- gsub("h?yt", "h?y", oa[[i]])
  oa[[i]] <- gsub("konsekvens", "konsekv", oa[[i]])
  oa[[i]] <- gsub("langt", "lang", oa[[i]])
  oa[[i]] <- gsub("laver", "lav", oa[[i]])
  oa[[i]] <- gsub("lavt", "lav", oa[[i]])
  oa[[i]] <- gsub("meir", "mer", oa[[i]])
  # oa[[i]] <- gsub("menneskeskapt", "menneskeskap", oa[[i]])
  # oa[[i]] <- gsub("menneske", "mennesk", oa[[i]])
  oa[[i]] <- gsub("overdrevent", "overdriv", oa[[i]])
  oa[[i]] <- gsub("overdrev", "overdriv", oa[[i]])
  oa[[i]] <- gsub("oson", "ozon", oa[[i]])
  oa[[i]] <- gsub("ozonlag", "ozon", oa[[i]])
  oa[[i]] <- gsub("politikern", "politiker", oa[[i]])
  oa[[i]] <- gsub("reell", "reel", oa[[i]])
  oa[[i]] <- gsub("reelt", "reel", oa[[i]])
  oa[[i]] <- gsub("somr", "sommer", oa[[i]]) # exception: shorter to longer
  oa[[i]] <- gsub("teknologisk", "teknologi", oa[[i]])
  oa[[i]] <- gsub("temperaturendr", "temperaturforandr", oa[[i]])
  oa[[i]] <- gsub("temperatur?kning", "temperaturstigning", oa[[i]])
  oa[[i]] <- gsub("tempratur", "temperatur", oa[[i]]) # mis-spelling
  oa[[i]] <- gsub("usikker", "usikk", oa[[i]])
  oa[[i]] <- gsub("ustabilt", "ustabil", oa[[i]])
  oa[[i]] <- gsub("utrydning", "utrydd", oa[[i]])
  oa[[i]] <- gsub("utslepp", "utslipp", oa[[i]])
  oa[[i]] <- gsub("uver", "uv?r", oa[[i]])
  oa[[i]] <- gsub("varmar", "varm", oa[[i]])
  oa[[i]] <- gsub("varmerev?ter", "varm v?t", oa[[i]])
  oa[[i]] <- gsub("varmer", "varm", oa[[i]])
  oa[[i]] <- gsub("varmt", "varm", oa[[i]])
  oa[[i]] <- gsub("vatn", "vann", oa[[i]])
  oa[[i]] <- gsub("viktiger", "vikt", oa[[i]])
  oa[[i]] <- gsub("viktigst", "vikt", oa[[i]])
  oa[[i]] <- gsub("vinter", "vint", oa[[i]])
  oa[[i]] <- gsub("vintr", "vint", oa[[i]])
  oa[[i]] <- gsub("?delagt", "?del", oa[[i]])
  oa[[i]] <- gsub("?delegg", "?del", oa[[i]])
  oa[[i]] <- gsub("?kend", "?ke", oa[[i]])
  oa[[i]] <- gsub("?ker", "?ke", oa[[i]])
  oa[[i]] <- gsub("?ket", "?ke", oa[[i]])
  oa[[i]] <- gsub("?kning", "?ke", oa[[i]])
  oa[[i]] <- gsub("?kt", "?ke", oa[[i]])
  oa[[i]] <- gsub("", "", oa[[i]])
  oa[[i]] <- gsub("", "", oa[[i]])
}
data$openanswer <- oa

#stemming/stopword removal, etc.
processed <- textProcessor(data$openanswer, metadata=data, language="norwegian", verbose=TRUE)

preprocessed_openanswer <-list()
for(i in 1:length(processed$documents)) {
  tmp_result <- ""
  s <- dim(processed$documents[[i]])[2]
  for(j in 1:s){
    index <- processed$documents[[i]][1,j]
    tmp_result <- paste(tmp_result, processed$vocab[index], sep =" ")
    
  }
  preprocessed_openanswer[i] <- tmp_result
}
to_wordcloud <- Corpus(VectorSource(preprocessed_openanswer))

wordcloud_smth(paste(global_path,"results/replication/original/preprocessed-textProcessor", sep=""),to_wordcloud)


#structure and index for usage in the stm model. 
# Verify no-missingness.

out <- prepDocuments(processed$documents,
                     processed$vocab, 
                     processed$meta,
                     lower.thresh=5)
#output will have object meta, documents, and vocab
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# save workspace before model runs
save(list = ls(all = TRUE), file = paste(global_path,"results/replication/original/preparedData-ncp-before-stm.RData", sep=""))

###########
# RUN STM #
###########

# Select the 4 best models out of 20 runs (see Vignette, pp. 8-9)
ncpSelect2 <- selectModel(out$documents,
                          out$vocab,
                          K=4,
                          prevalence =~ meta$concern+meta$humanmade+meta$efficacy+meta$edu3+meta$gender+meta$age, 
                          max.em.its=100, 
                          data=out$meta,
                          runs=20,
                          seed=5926696) #, emtol=1)


# Check the four selected models
png(filename = paste(global_path,"results/replication/original/SemanticCoherence_original.png", sep=""), width = 10, height = 4, units = "in", res = 300)

plotModels(ncpSelect2,xlab="Semantic Coherence")
dev.off()
# Choose run #1 based on qualitative assessment: 
ncpPrevFit <- ncpSelect2$runout[[1]]

#### ADDITIONAL #####
# 2 samples of passages for topic number 3, help
# to figure out what topic 3 is actually
findThoughts(ncpPrevFit, texts = out$meta$openanswer, n=2, topics = 3) 
# 2 samples of passages for topic number 3, help
# to figure out what topic 3 is actually

k_finding <- searchK(out$documents, 
                     out$vocab, 
                     K = c(4, 7),
                     prevalence =~ meta$concern+meta$humanmade+meta$efficacy+meta$edu3+meta$gender+meta$age, 
                     max.em.its=100, 
                     data=out$meta)

# numerous goodness-of-fit measures, no substitute for human validation of the model
#  how exactly???
plot.searchK(k_finding)
#########################
# Save workspace after model runs
save(list = ls(all = TRUE), file = paste(global_path,"results/replication/original/preparedModels-ncp-stm.RData", sep=""))

####
# Analysis on chosen model run 
# Table 1: most frequent and exclusive terms (FREX): 
labelTopics(ncpPrevFit, topics=NULL, n=10)

# Table 2: responses that are highly associated with topics
thoughts1<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=1)$docs[[1]]
thoughts2<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=2)$docs[[1]]
thoughts3<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=3)$docs[[1]]
thoughts4<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=4)$docs[[1]]

thoughts1
thoughts2
thoughts3
thoughts4

# Figures
# Co-variation with demographics and concern
# Generate education binary: university or not
univdummy <- as.numeric(meta$edu3==3)
print(univdummy)  
summary(univdummy)
table(meta$edu3, univdummy)
meta$univdummy <- univdummy

# Figure 1: Topical prevalence over co-variates
png(filename = paste(global_path,"results/replication/original/Figure1_original.png", sep=""), width = 10, height = 4, units = "in", res = 300)
prep <- estimateEffect(1:4 ~ concern+univdummy+gender+age, 
                       ncpPrevFit,
                       meta=meta, 
                       uncertainty = "Global")
par(mfrow = c(1, 2) ,mar=c(4,4,1,2)) 
plot.estimateEffect(prep, 
                    main="a. Climate concern",
                    covariate = "concern", 
                    topics = c(3:4),
                    model=ncpPrevFit, 
                    method="continuous",
                    labeltype="custom",
                    # custom.labels=labelnames,
                    custom.labels=c("Attribution", "Future/Impact"),
                    # custom.labels=c("1", "2", "3", "4"),
                    ylim=c(.1,.5), 
                    linecol=c("red", "black"),
                    ylab="Expected topic proportion",
                    xlab="Low concern             ...            High concern")

plot.estimateEffect(prep, 
                    main="b. Age",
                    covariate = "age", 
                    topics = c(2,4),
                    model=ncpPrevFit, 
                    method="continuous",
                    labeltype="custom",
                    custom.labels=c("Weather/Ice", "Future/Impact"),
                    ylim=c(.1,.5), 
                    linecol=c("green", "black"), 
                    ylab="",
                    xaxt='n', 
                    axis(1,at=seq(10, 70, by = 10), las=2),
                    # ylab="Expected topic proportion",
                    xlab="Age group")
labels <- c("18-25","26-35", "36-45", "46-55", "56-65", "66-75", "75+")
text(1:7, par("usr")[1] - .685, srt = 90, adj = 1,
     labels = labels, xpd = TRUE)
# save it
# jpeg('Figure1.jpg')
dev.off()

# Figure 2: Gender and education
png(filename = paste(global_path,"results/replication/original/Figure2_original.png", sep=""), width = 10, height = 4, units = "in", res = 300)
prep <- estimateEffect(1:4 ~ concern+age+gender+univdummy, 
                       ncpPrevFit,
                       meta=meta, 
                       uncertainty = "Global")
par(mfrow = c(1, 2) ,mar=c(4,2,1,2)) 
# 2a: gender
plot.estimateEffect(prep, 
                    main="a. Gender",
                    covariate = "gender", 
                    topics = c(1:4),
                    model=ncpPrevFit, 
                    #method="difference", 
                    labeltype="custom",
                    custom.labels=c("Money/Consum", "Weather/Ice", "Attribution", "Future/Impact"),                    method="difference",
                    # verbose.labels=TRUE,
                    xlim=c(-.15,.15),
                    cov.value1=2, cov.value2=1,
                    xlab="Difference in topical prevalence")
# 2b: education
plot.estimateEffect(prep, 
                    main="b. Education",
                    covariate = "univdummy", 
                    topics = c(1:4),
                    model=ncpPrevFit, 
                    method="difference", 
                    labeltype="custom",
                    custom.labels=c("Money/Consum", "Weather/Ice", "Attribution", "Future/Impact"),
                    xlim=c(-.1,.1), 
                    verbose.labels=TRUE,
                    cov.value1=1, cov.value2=0,
                    xlab="Difference in topical prevalence")
#dev.copy(jpeg, "FigureX2.jpeg")
dev.off()

###
# Part 3: Closer inspection of textual answer
# Look at topical prevalence over humanmade variable. Use XLS for that. 
# Add topical prevalence (theta parameters) to "meta" data frame
meta$theta1 <- ncpPrevFit$theta[,1]
meta$theta2 <- ncpPrevFit$theta[,2]
meta$theta3 <- ncpPrevFit$theta[,3]
meta$theta4 <- ncpPrevFit$theta[,4]
# and save it for XLS
write.table(meta, "dataset-thetas.txt", sep="\t") 
# Note: The first row of the XLS output will need to be moved one cell to the right. 

# Diagnostic for paper: what's the number of terms after stemming and stop word removal? 
length(vocab)


# End
###########################



