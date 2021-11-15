#############################
# Exploratory Data Analysis #
#############################

## In this section we compute and plot the top 10 most common single words,
## word pairs, word trios, word quartets, and word quintets, and then plot
## their associated word clouds.

## Load packages
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wordcloud2)
library(tidytext)

## Set working directory
setwd("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/EDACharts/")

## List of file names
tidyDataFolder <- "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/tidyData/"

## Read in tidy data
tidyTextData <- read_rds(file = paste0(tidyDataFolder, "tidyData.rds"))

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
                count(words) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(words, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Words As % Of Total - ", tools::toTitleCase(name))) +
                labs(x = "Word", y = "Percentage") + 
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
                count(words) %>%
                filter(n >= 75) %>% 
                slice_max(n, n = 300)
        
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
# 2-grams #
###########

## Plot top 10 most commonly used pairs of words (%) from each source
twogramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(wordDuo))) %>% 
                count(wordDuo) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(wordDuo, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Pairs Of Words As % Of Total - ", tools::toTitleCase(name))) +
                labs(x = "Word Duo", y = "Percentage") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(twogramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/twogram/Top10FrequencyPairs - ", names(twogramFreqPlots), ".png"),
           plot = twogramFreqPlots),
      ggsave)

## Plot word clouds from each source 
twogramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(wordDuo))) %>% 
                count(wordDuo) %>%
                filter(n >= 30) %>% 
                slice_max(n, n = 225)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
        
})

## Name wordclouds
names(twogramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(twogramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(twogramWordClouds[[name]], 
                                paste0("wordClouds/twogram/twogramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/twogram/twogramWordClouds - ", name, ".html"),
                         paste0("wordClouds/twogram/twogramWordClouds - ", name, ".png"),
                         vwidth = 600, 
                         vheight = 600, 
                         delay = 10)
        
})

###########
# 3-grams #
###########

## Plot top 10 most commonly used trios of words (%) from each source
threegramFreqPlots <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Build plot
        tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(wordTrio))) %>% 
                count(wordTrio) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(wordTrio, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Trios Of Words As % Of Total - ", tools::toTitleCase(name))) +
                labs(x = "Word Trio", y = "Percentage") + 
                theme_bw() +
                scale_color_tableau() +
                theme(legend.position = "none") 
        
})

## Name frequency plots
names(threegramFreqPlots) <- c("blogs", "news", "twitter")

## Write to EDA folder
pwalk(list(filename = paste0("frequencyPlots/threegram/Top10FrequencyTrios - ", names(threegramFreqPlots), ".png"),
           plot = threegramFreqPlots),
      ggsave)

## Plot word clouds from each source 
threegramWordClouds <- unique(tidyTextData$dataset) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        ## Manipulate data
        countWords <- tidyTextData %>% 
                filter(dataset == name,
                       !(is.na(wordTrio))) %>% 
                count(wordTrio) %>%
                filter(n >= 10) %>% 
                slice_max(n, n = 150)
        
        ## Build plot
        wordcloud2(data = countWords, 
                   size = 1,
                   backgroundColor = "black",
                   shape = "circle")
        
})

## Name wordclouds
names(threegramWordClouds) <- c("blogs", "news", "twitter")

## Write to folder
unique(names(threegramWordClouds)) %>% map(function(name) {
        
        ## Progress check
        print(name)
        
        htmlwidgets::saveWidget(threegramWordClouds[[name]], 
                                paste0("wordClouds/threegram/threegramWordClouds - ", name, ".html"), selfcontained = F)
        webshot::webshot(paste0("wordClouds/threegram/threegramWordClouds - ", name, ".html"),
                         paste0("wordClouds/threegram/threegramWordClouds - ", name, ".png"),
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
                       !(is.na(wordQuartet))) %>% 
                count(wordQuartet) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(wordQuartet, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Quartets Of Words As % Of Total - ", tools::toTitleCase(name))) +
                labs(x = "Word Quartet", y = "Percentage") + 
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
                       !(is.na(wordQuartet))) %>% 
                count(wordQuartet) %>%
                filter(n >= 3) %>% 
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
                       !(is.na(wordQuintet))) %>% 
                count(wordQuintet) %>% 
                mutate(percent = 100*(n/sum(n))) %>% 
                slice(-n) %>% 
                slice_max(percent, n = 10) %>%
                ggplot(aes(x = reorder(wordQuintet, percent), y = percent, fill = percent)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                ggtitle(paste0("Top 10 Most Frequently Used Quintets Of Words As % Of Total - ", tools::toTitleCase(name))) +
                labs(x = "Word Quintet", y = "Percentage") + 
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
                       !(is.na(wordQuintet))) %>% 
                count(wordQuintet) %>%
                filter(n >= 3) %>% 
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
        inner_join(get_sentiments("afinn"), by = c("words" = "word")) %>% 
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
                count(words) %>% 
                bind_tf_idf(words, dataset, n) %>% 
                filter(dataset == name,
                       n >= 30) %>% 
                slice_max(tf_idf, n = 10) %>% 
                ggplot(aes(x = reorder(words, tf_idf), y = tf_idf, fill = tf_idf)) +
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

########################
# Twogram Markov Chain #
########################

count_bigrams <- function(dataset) {
        dataset %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word) %>%
                count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
        set.seed(2016)
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        
        bigrams %>%
                graph_from_data_frame() %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
                geom_node_point(color = "lightblue", size = 5) +
                geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
                theme_void()
}
