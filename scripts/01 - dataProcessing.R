###################
# Data Processing #
###################

## This script takes the first 75k lines from each of the three data sources
## and tokenizes them using the "tm" and "tidytext" packages.

## Load packages
library(tidyverse)
library(tm)
library(tidytext)

## Set working directory
setwd("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/rawData")

## List of file names
fileNames <- list.files(getwd())

## Read in raw data, capping the number of lines at 30k (huge files - this sample size should suffice)
rawTextData <- fileNames %>% map_dfc(function(file) {
        
        ## Applies readLines functions to each of the three files
        readr::read_lines(file, skip = 1, n_max = 30000)
        
})

names(rawTextData) <- c("blogs", "news", "twitter")

## Format using tm and tidytext - runtime ~8min
tidyTextData <- unique(names(rawTextData)) %>% map_dfr(function(name) {
        
        ## Progress check
        print(name)
        
        ## Format and build out word groupings
        VCorpus(VectorSource(rawTextData[[name]])) %>% # Change to corpus format
                tm_map(removeNumbers) %>% # Remove numbers
                tm_map(removePunctuation) %>% # Remove punctuation
                tm_map(stripWhitespace) %>% # Strip whitespace
                tm_map(content_transformer(tolower)) %>% # Make lowercase
                tidy() %>% # tidy returns a tbl_df with one-row-per-document
                unnest_tokens(words, text) %>% # Splits text column into word tokens, flattening the table into one-token-per-row
                group_by(id) %>% # otherwise we'd get word strings across sentences
                mutate(wordDuo = paste(lag(words), words, sep = " "),
                       wordTrio = paste(lag(words), words, lead(words), sep = " "),
                       wordQuartet = paste(lag(words, n = 2), lag(words), words, lead(words), sep = " "),
                       wordQuintet = paste(lag(words, n = 2), lag(words), words, lead(words), lead(words, n = 2), sep = " "),
                       dataset = name) %>% # identifier for which dataset text came from
                ungroup()

})

## Replace word groupings that aren't full with NA - faster using base R than piping into the mapping above
is.na(tidyTextData$wordDuo) <- str_detect(tidyTextData$wordDuo, "NA")
is.na(tidyTextData$wordTrio) <- str_detect(tidyTextData$wordTrio, "NA")
is.na(tidyTextData$wordQuartet) <- str_detect(tidyTextData$wordQuartet, "NA")
is.na(tidyTextData$wordQuintet) <- str_detect(tidyTextData$wordQuintet, "NA")

#####################################
## Write results to tidyData folder #
#####################################

write_rds(tidyTextData, "/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/tidyData/tidyData.rds")

