library(shiny)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyverse)
library(reshape2)

dataset <- readRDS(file = file.path("data", "dataset.rds"))
hashtags <- readRDS(file = file.path("data", "hashtags.rds"))
lang <- readRDS(file = file.path("data", "lang.rds"))

# exclude <- data_frame(word = c("european", "europe", "union", "commission", "eu", "means", "europe's"))

library(RColorBrewer)
pal <- brewer.pal(9,"Blues")
pal <- pal[-(1:6)]