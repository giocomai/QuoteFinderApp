library(shiny)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(stringr)
library(tidyverse)
library(reshape2)
library(stopwords)
library(syuzhet)

dataset <- readRDS(file = file.path("data", "dataset.rds"))
hashtagsList <- readRDS(file = file.path("data", "hashtags.rds"))
lang <- readRDS(file = file.path("data", "lang.rds"))
EPGroupShort <- readRDS(file = file.path("data", "EPGroupShort.rds"))
countries <- readRDS(file = file.path("data", "countries.rds"))

library(RColorBrewer)
pal <- brewer.pal(9,"Blues")
pal <- pal[-(1:5)]

bluesFunc <- colorRampPalette(colors = c("#4292C6", "#08306B"))
blues <- bluesFunc(5)

# European formatting of large numbers
point <- scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)

## function to give wordcloud2 click interactivity
## from https://github.com/Lchiffon/wordcloud2/issues/25
wc2ClickedWord = function(cloudOutputId, inputId) {
  shiny::tags$script(shiny::HTML(
    sprintf("$(document).on('click', '#%s', function() {", cloudOutputId),
    'word = document.getElementById("wcSpan").innerHTML;',
    sprintf("Shiny.onInputChange('%s', word);", inputId),
    "});"
  ))
}

langTable <- left_join(x = data_frame(lang = unlist(lang)),
                       y = readRDS(file.path("data", "langCode.rds")) %>%rename(lang = alpha2), by = "lang") %>% 
  mutate(English = stringr::str_extract(string = English, pattern = regex("[[:alnum:]]+")))
