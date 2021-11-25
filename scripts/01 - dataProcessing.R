###################
# Data Processing #
###################

## This script takes the first 30k lines from each of the three data sources
## and tokenizes them using the "tm" and "tidytext" packages.

## Load packages
library(tidyverse)
library(tm)
library(tidytext)
library(stopwords)
library(quanteda)

## Set working directory
setwd("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/rawData")

## List of file names
fileNames <- list.files(getwd())

## Read in raw data, capping the number of lines at 30k (huge files - this sample size should suffice)
rawTextData <- fileNames %>% map_dfc(function(file) {
        
        ## Applies readLines functions to each of the three files
        readr::read_lines(file, skip = 3, n_max = 30000)
        
})

names(rawTextData) <- c("blogs", "news", "twitter")

################
# Data for EDA #
################

## Define cleaning function used to replace desired regex patterns with a space
cleaningFn <- content_transformer(function(x, pattern) str_replace_all(pattern, " ", x))

## Format and clean using tm, stopwords and tidytext
tidyTextData <- unique(names(rawTextData)) %>% map_dfr(function(name) {
        
        ## Progress check
        print(name)
        
        ## Format and build out word groupings
        VCorpus(VectorSource(rawTextData[[name]])) %>% # Change to corpus format
                tm_map(removeNumbers) %>% # Remove numbers
                tm_map(removePunctuation) %>% # Remove punctuation
                tm_map(stripWhitespace) %>% # Strip whitespace
                tm_map(content_transformer(tolower)) %>% # Make lowercase
                tm_map(cleaningFn, "@[^\\s]+") %>%  # Remove twitter handles
                tm_map(cleaningFn, "(f|ht)tp(s?)://(.*)[.][a-z]+") %>% # Remove url's
                tm_map(cleaningFn, "\\b[A-Za-z0-9._-]*[@](.*?)[.]{1,3}\\b") %>% # Remove email addresses
                tidy() %>% # tidy returns a tbl_df with one-row-per-document
                unnest_tokens(word, text) %>% # Splits text column into word tokens, flattening the table into one-token-per-row
                anti_join(get_stopwords(source = "snowball"), by = "word") %>% 
                group_by(id) %>% # otherwise we'd get word strings across sentences
                mutate(stem = SnowballC::wordStem(word),
                       bigram = paste(lag(word), word, sep = " "),
                       trigram = paste(lag(word), word, lead(word), sep = " "),
                       fourgram = paste(lag(word, n = 2), lag(word), word, lead(word), sep = " "),
                       fivegram = paste(lag(word, n = 2), lag(word), word, lead(word), lead(word, n = 2), sep = " "),
                       dataset = name) %>% # identifier for which dataset text came from
                ungroup()

})

## Replace word groupings that aren't full with NA - faster using base R than piping into the mapping above
is.na(tidyTextData$bigram) <- str_detect(tidyTextData$bigram, "NA")
is.na(tidyTextData$trigram) <- str_detect(tidyTextData$trigram, "NA")
is.na(tidyTextData$fourgram) <- str_detect(tidyTextData$fourgram, "NA")
is.na(tidyTextData$fivegram) <- str_detect(tidyTextData$fivegram, "NA")

######################
# Data for SBO Model #
######################

## Pivot data longer
rawTextData <- rawTextData %>% 
        pivot_longer(everything(), values_to = "text") %>% 
        select(text)

## Split into training and testing
splitRawTextData <- rawTextData %>% 
        initial_split(prop = 0.8)

trainData <- training(splitRawTextData)
testData <- testing(splitRawTextData)

## Clean and convert to corpus
tidyTrainData <- trainData$text %>% 
        corpus()

tidyTestData <- testData$text %>% 
        corpus()

#####################################
## Write results to tidyData folder #
#####################################

write_rds(tidyTextData, "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/tidyData/tidyData.rds")
write_rds(tidyTrainData, "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/tidyData/tidyTrainData.rds")
write_rds(tidyTestData, "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/tidyData/tidyTestData.rds")
