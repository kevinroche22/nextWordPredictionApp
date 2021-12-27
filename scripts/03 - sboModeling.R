#################
# Data Modeling #
#################

## In this section we build a stupid backoff model (sbo) to predict the
## next word based on the previous n-grams (in this case, I use 5)

## Load packages
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(sbo)

## Set working directory
setwd("/Users/kevinroche22/RData/nextWordPredictionApp/models/")

## List of file names
tidyDataFolder <- "/Users/kevinroche22/RData/nextWordPredictionApp/tidyData/"

## Read in tidy data
trainData <- read_rds(file = paste0(tidyDataFolder, "tidyTrainData.rds"))
testData <- read_rds(file = paste0(tidyDataFolder, "tidyTestData.rds"))
fullData <- read_rds(file = paste0(tidyDataFolder, "tidyFullModelingData.rds"))

###############################
# Stupid Back Off (SBO) Model #
###############################

## Set seed
set.seed(824) # ripKobe

## Build predictor
sboPredictor <- sbo_predictor(object = trainData, # training data
                                            N = 5, # 5-gram model
                                            dict = target ~ 0.75, # 75% of training corpus used in dictionary
                                            .preprocess = sbo::preprocess, # removes anything non alphanumeric, whitespace, converts to lower, etc.
                                            EOS = ".?!:;", # End-Of-Sentence tokens
                                            lambda = 0.4, # Back-off penalization in SBO algorithm - parameter suggested by authors of methodology
                                            L = 3L, # Number of predictions
                                            filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> and <EOS> tokens from predictions
)

## Evaluate Predictions
sboEvaluation <- eval_sbo_predictor(sboPredictor, test = testData)

## Determine accuracy - ~18%
sboEvaluation %>% 
        filter(true != "<EOS>") %>%
        summarise(accuracyPercentage = (sum(correct)/n())*100, 
                  accuracy = sum(correct)/n(), 
                  uncertaintyPercentage = sqrt(accuracy * (1 - accuracy) / n()))

## Stores next word probability in order of score 
sboKGrams <- kgram_freqs(trainData, 
                         N = 5, 
                         dict = target ~ 0.75,
                         .preprocess = sbo::preprocess,
                         EOS = ".?!:;")

## Example - predict top 3 words after typing
predict(sboPredictor, "Thanks for having us, we had a great time with")

## Example - all words in dictionary arranged by probability
predict(sboKGrams, "Thanks for having us, we had a great time with")

###########################
# Shiny compatible models #
###########################

## To be deployed in Shiny app, needs to be saved in predtable format

## 3-gram model
sboShiny3 <- sbo_predtable(object = fullData, # full data
                          N = 3, # 3-gram model
                          dict = target ~ 0.8, # 80% of training corpus used in dictionary
                          .preprocess = sbo::preprocess, # removes anything non alphanumeric, whitespace, converts to lower, etc.
                          EOS = ".?!:;", # End-Of-Sentence tokens
                          lambda = 0.4, # Back-off penalization in SBO algorithm - parameter suggested by authors of methodology
                          L = 5L, # Number of predictions
                          filtered = c("<UNK>", "<EOS>"))

## 4-gram model
sboShiny4 <- sbo_predtable(object = fullData, # full data
                          N = 4, # 4-gram model
                          dict = target ~ 0.8, # 80% of training corpus used in dictionary
                          .preprocess = sbo::preprocess, # removes anything non alphanumeric, whitespace, converts to lower, etc.
                          EOS = ".?!:;", # End-Of-Sentence tokens
                          lambda = 0.4, # Back-off penalization in SBO algorithm - parameter suggested by authors of methodology
                          L = 5L, # Number of predictions
                          filtered = c("<UNK>", "<EOS>"))

## 5-gram model
sboShiny5 <- sbo_predtable(object = fullData, # full data
                          N = 5, # 5-gram model
                          dict = target ~ 0.75, # 80% of training corpus used in dictionary
                          .preprocess = sbo::preprocess, # removes anything non alphanumeric, whitespace, converts to lower, etc.
                          EOS = ".?!:;", # End-Of-Sentence tokens
                          lambda = 0.4, # Back-off penalization in SBO algorithm - parameter suggested by authors of methodology
                          L = 5L, # Number of predictions
                          filtered = c("<UNK>", "<EOS>"))

###################
# Write to folder #
###################

save(sboShiny3, file = "/Users/kevinroche22/RData/nextWordPredictionApp/models/sboShiny3.rda")
save(sboShiny4, file = "/Users/kevinroche22/RData/nextWordPredictionApp/models/sboShiny4.rda")
save(sboShiny5, file = "/Users/kevinroche22/RData/nextWordPredictionApp/models/sboShiny5.rda")
