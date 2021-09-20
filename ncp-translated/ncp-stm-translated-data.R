# ncp-stm.R
# Structural topic modeling of open-ended survey responses from the Norwegian Citizen Panel
# Endre Tvinnereim, September 2014-February 2015
# Background: http://structuraltopicmodel.com/ ; _vignette_ "stm: R Package for Structural Topic Models"

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
#install.packages ("rapportools")
#install.packages("optparse")
########################
library(devtools)
library(R.methodsS3)
library(stm)
library(optparse)
library(foreign)

# Clear all
rm(list=ls(all=TRUE))

# read in your data that is in a spreadsheet form .csv file here)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL,
              help="dataset file name", metavar="character"),
	make_option(c("-a", "--aux"), type="character", default="aux",
              help="Auxiliary output name addition [default= %default]", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

datafilename = opt$file
outputaux = opt$aux

if ( is.null(datafilename) || is.null(outputaux) ){
	print_help(opt_parser)
	quit()
}



rawdata <- read.csv(datafilename, header=T, sep=";", fileEncoding = "latin1")
#rawdata <- read.csv(datafilename, header=T, sep=";", enc = "windows-1252")

drops <- c("openanswer")
rawdata <- subset(rawdata, select = -openanswer)
rawdata$translation <- as.character(rawdata$translation)

# remove missing data
data <- na.omit(rawdata) # thanks to http://www.statmethods.net/input/missingdata.html
rm(rawdata)

# MANUAL prestemming removed

processed <- textProcessor(data$translation, metadata=data, language="english", verbose=TRUE)
#structure and index for usage in the stm model. Verify no-missingness.
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=5)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# save workspace before model runs
save(list = ls(all = TRUE), file = paste("results/preparedData-ncp-before-stm-", outputaux, ".RData", sep = ""))
#save(list = ls(all = TRUE), file = "preparedData-ncp-before-stm.RData")
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
png(filename = "results/SemanticCoherence.png", width = 10, height = 4, units = "in", res = 300)
plotModels(ncpSelect2,xlab="Semantic Coherence")
dev.off()
# Choose run #1 based on qualitative assessment: 
ncpPrevFit <- ncpSelect2$runout[[1]]

# Save workspace after model runs
save(list = ls(all = TRUE), file = paste("results/preparedModels-ncp-stm-", outputaux, ".RData", sep = ""))

####
# Analysis on chosen model run 
# Table 1: most frequent and exclusive terms (FREX): 
labelTopics(ncpPrevFit, topics=NULL, n=10)
# Table 2: responses that are highly associated with topics
thoughts1<-findThoughts(ncpPrevFit, texts=meta$translation, n=10, topics=1)$docs[[1]]
thoughts2<-findThoughts(ncpPrevFit, texts=meta$translation, n=10, topics=2)$docs[[1]]
thoughts3<-findThoughts(ncpPrevFit, texts=meta$translation, n=10, topics=3)$docs[[1]]
thoughts4<-findThoughts(ncpPrevFit, texts=meta$translation, n=10, topics=4)$docs[[1]]

#thoughts1<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=1)$docs[[1]]
#thoughts2<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=2)$docs[[1]]
#thoughts3<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=3)$docs[[1]]
#thoughts4<-findThoughts(ncpPrevFit, texts=meta$openanswer, n=10, topics=4)$docs[[1]]


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


png(filename = paste("results/Figure1-", outputaux, ".png", sep = ""), width = 10, height = 4, units = "in", res = 300)
# Figure 1: Topical prevalence over co-variates
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
dev.off()


## Figure 2: Gender and education
png(filename = paste("results/Figure2-", outputaux, ".png", sep = ""), width = 10, height = 4, units = "in", res = 300)
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
write.table(meta, paste("results/dataset-thetas", outputaux, ".txt", sep = ""), sep="\t") 
# Note: The first row of the XLS output will need to be moved one cell to the right. 

# Diagnostic for paper: what's the number of terms after stemming and stop word removal? 
length(vocab)


# End
###########################



