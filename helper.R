library(shiny)
library(tidyverse)
library(DT)
library(caret)
library(randomForest)

myData <- read_csv("pokemon.csv")