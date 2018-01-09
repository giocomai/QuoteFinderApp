library(shiny)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyverse)
library(reshape2)

dataset <- readRDS(file = file.path("data", "dataset.rds"))
hashtags <- readRDS(file = file.path("data", "hashtags.rds"))
lang <- readRDS(file = file.path("data", "lang.rds"))
EPGroupShort <- readRDS(file = file.path("data", "EPGroupShort.rds"))
countries <- readRDS(file = file.path("data", "countries.rds"))

library(RColorBrewer)
pal <- brewer.pal(9,"Blues")
pal <- pal[-(1:6)]