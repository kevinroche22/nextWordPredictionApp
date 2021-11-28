#############################
# Exploratory Data Analysis #
#############################

## In this section we compute and plot the top 10 most common single words,
## word pairs, word trios, word quartets, and word quintets, and then plot
## their associated word clouds. We also create summary stats, build tf-idf 
## plots and perform sentiment analysis.

## Load packages
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wordcloud2)
library(tidytext)

## Set working directory
setwd("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/EDACharts/")

## List of file names for tidy data
tidyDataFolder <- "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/tidyData/"

## Read in tidy data
tidyTextData <- read_rds(file = paste0(tidyDataFolder, "tidyData.rds"))

## List of file names for raw data
rawDataFileNames <- paste0("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/rawData/",
                           list.files("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/rawData/"))

## Read in raw data
rawTextData <- rawDataFileNames %>% map_dfc(function(file) {
        
        ## Applies readLines functions to each of the three files
        readr::read_lines(file, skip = 3, n_max = 30000)
        
})

## Name raw data
names(rawTextData) <- c("blogs", "news", "twitter")

#################
# Summary Stats #
#################

## Calculate file size
fileSizes <- rawDataFileNames %>% map_dfc(function(file) {
        
        ## Return file size
        paste0(round(file.size(file) / 1000000, 2), " mb")
        
})

## Summarize raw data
rawDataSummary <- rawDataFileNames %>% map_dfr(function(file) {
        
        ## Applies readLines functions to each of the three files
        rawFile <- readLines(file)
        
        ## Return stats
        stringi::stri_stats_general(rawFile) 
        
})

## Add descriptive column and reorder
rawDataSummary <- rawDataSummary %>% 
        mutate(file = c("blogs", "news", "twitter")) %>% 
        bind_cols(fileSize = t(fileSizes)) %>% 
        relocate(file) %>% 
        select(-c("LinesNEmpty", "CharsNWhite")) %>% # Not particularly useful information imo
        as.matrix() %>%
        as.data.frame() # Has to be in df format to write but first needed to convert to matrix due to transpose

## Name columns
names(rawDataSummary) <- c("file", "numberOfLines", "numberOfCharacters", "fileSize")

## Write to folder
write_csv(rawDataSummary, "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/summaryStats/summaryStats.csv")

############
# Unigrams #
############

## Plot top 10 most commonly used words (%) from each source
unigramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>%
                filter(dataset == name) %>%
                count(word) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(word, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Words - ", tools::toTitleCase(name))) +
                labs(x = "Word", y = "Percentage Of Total") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(unigramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/unigram/Top10FrequencyWords - ", names(unigramFreqPlots), ".png"),
           plot = unigramFreqPlots),
      ggsave)

## Plot word clouds from each source and write to folder
unigramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name) %>%
                count(word) %>%
                filter(n >= 3) %>% 
                slice_max(n, n = 100)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
                
})

## Name wordclouds
names(unigramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(unigramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(unigramWordClouds[[name]], 
                                paste0("wordClouds/unigram/unigramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/unigram/unigramWordClouds - ", name, ".html"),
                         paste0("wordClouds/unigram/unigramWordClouds - ", name, ".png"),
                         vwidth = 600, 
                         vheight = 600, 
                         delay = 10)

})

###########
# Bigrams #
###########

## Plot top 10 most commonly used pairs of words (%) from each source
bigramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(bigram))) %>% 
                count(bigram) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(bigram, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Bigrams - ", tools::toTitleCase(name))) +
                labs(x = "Bigram", y = "Percentage Of Total") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(bigramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/bigram/Top10FrequencyPairs - ", names(bigramFreqPlots), ".png"),
           plot = bigramFreqPlots),
      ggsave)

## Plot word clouds from each source 
bigramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(bigram))) %>% 
                count(bigram) %>%
                filter(n >= 3) %>% 
                slice_max(n, n = 100)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
        
})

## Name wordclouds
names(bigramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(bigramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(bigramWordClouds[[name]], 
                                paste0("wordClouds/bigram/bigramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/bigram/bigramWordClouds - ", name, ".html"),
                         paste0("wordClouds/bigram/bigramWordClouds - ", name, ".png"),
                         vwidth = 600, 
                         vheight = 600, 
                         delay = 10)
        
})

###########
# 3-grams #
###########

## Plot top 10 most commonly used trios of words (%) from each source
trigramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(trigram))) %>% 
                count(trigram) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(trigram, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Trios Of Words - ", tools::toTitleCase(name))) +
                labs(x = "Word Trio", y = "Percentage Of Total") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(trigramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/trigram/Top10FrequencyTrios - ", names(trigramFreqPlots), ".png"),
           plot = trigramFreqPlots),
      ggsave)

## Plot word clouds from each source 
trigramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(trigram))) %>% 
                count(trigram) %>%
                filter(n >= 3) %>% 
                slice_max(n, n = 100)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
        
})

## Name wordclouds
names(trigramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(trigramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(trigramWordClouds[[name]], 
                                paste0("wordClouds/trigram/trigramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/trigram/trigramWordClouds - ", name, ".html"),
                         paste0("wordClouds/trigram/trigramWordClouds - ", name, ".png"),
                         vwidth = 600, 
                         vheight = 600, 
                         delay = 10)
        
})

###########
# 4-grams #
###########

## Plot top 10 most commonly used quartets of words (%) from each source
fourgramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(fourgram))) %>% 
                count(fourgram) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(fourgram, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Quartets Of Words - ", tools::toTitleCase(name))) +
                labs(x = "Word Quartet", y = "Percentage Of Total") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(fourgramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/fourgram/Top10FrequencyQuartets - ", names(fourgramFreqPlots), ".png"),
           plot = fourgramFreqPlots),
      ggsave)

## Plot word clouds from each source 
fourgramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(fourgram))) %>% 
                count(fourgram) %>%
                filter(n >= 2) %>% 
                slice_max(n, n = 100)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
        
})

## Name wordcloud
names(fourgramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(fourgramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(fourgramWordClouds[[name]], 
                                paste0("wordClouds/fourgram/fourgramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/fourgram/fourgramWordClouds - ", name, ".html"),
                         paste0("wordClouds/fourgram/fourgramWordClouds - ", name, ".png"),
                         vwidth = 600, 
                         vheight = 600, 
                         delay = 10)
        
})

###########
# 5-grams #
###########

## Plot top 10 most commonly used quintets of words (%) from each source
fivegramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(fivegram))) %>% 
                count(fivegram) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(fivegram, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Quintets Of Words - ", tools::toTitleCase(name))) +
                labs(x = "Word Quintet", y = "Percentage Of Total") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(fivegramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/fivegram/Top10FrequencyQuintets - ", names(fivegramFreqPlots), ".png"),
           plot = fivegramFreqPlots),
      ggsave)

## Plot word clouds from each source 
fivegramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(fivegram))) %>% 
                count(fivegram) %>%
                filter(n >= 2) %>% 
                slice_max(n, n = 100)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
        
})

## Name wordcloud
names(fivegramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(fivegramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(fivegramWordClouds[[name]], 
                                paste0("wordClouds/fivegram/fivegramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/fivegram/fivegramWordClouds - ", name, ".html"),
                         paste0("wordClouds/fivegram/fivegramWordClouds - ", name, ".png"),
                         vwidth = 600, 
                         vheight = 600, 
                         delay = 10)
        
})

######################
# Sentiment Analysis #
######################

## Plot average sentiment scores by data source and write to sentiment analysis folder
(tidyTextData %>% 
        inner_join(get_sentiments("afinn"), by = c("word" = "word")) %>% 
        group_by(dataset) %>% 
        ggplot(aes(x = dataset, y = value, fill = dataset)) +
        geom_boxplot() +
        ggtitle("Average Sentiment By Data Source") +
        labs(x = "Data Source", y = "Sentiment Value") +
        theme_bw() +
        scale_fill_tableau() +
        theme(legend.position = "none")) %>% 
        ggsave(file = paste0(getwd(), "/sentimentAnalysis/sentimentAnalysis.png"))

################
# TF-IDF Plots #
################

## TF-IDF scores account for how common a word is to determine which words
## are most important (ie. more common than is typical) to a text

## Plot 10 words with highest TF-IDF score
tfidfPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Check Progress
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                group_by(dataset) %>% 
                count(word) %>% 
                bind_tf_idf(word, dataset, n) %>% 
                filter(dataset == name,
                       n >= 30) %>% 
                slice_max(tf_idf, n = 10) %>% 
                ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = tf_idf)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Most Important Words By TF-IDF Score - ", tools::toTitleCase(name))) +
                labs(x = "Word", y = "Importance") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name TF-IDF Plots
names(tfidfPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("tfidfPlots/TF-IDF Plot - ", names(tfidfPlots), ".png"),
           plot = tfidfPlots),
      ggsave)