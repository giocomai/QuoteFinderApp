if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org") # for taking care of package installation/loading

pacman::p_load("shiny")
pacman::p_load("tidytext")
pacman::p_load("wordcloud")
pacman::p_load("wordcloud2")
pacman::p_load("stringr")
pacman::p_load("tidyverse")
pacman::p_load("reshape2")
pacman::p_load("stopwords")
pacman::p_load("syuzhet")
pacman::p_load("colourpicker")
pacman::p_load("shinyWidgets")
pacman::p_load("RColorBrewer")
pacman::p_load("shinydashboard")
pacman::p_load("shinycustomloader")
pacman::p_load("DT")
pacman::p_load("webshot")

# install phantomjs at first run to enable downloading png wordclouds
# webshot::install_phantomjs()

dataset <- readRDS(file = file.path("data", "dataset.rds"))
hashtagsList <- readRDS(file = file.path("data", "hashtags.rds"))
lang <- readRDS(file = file.path("data", "lang.rds"))
EPGroupShort <- readRDS(file = file.path("data", "EPGroupShort.rds"))
countries <- readRDS(file = file.path("data", "countries.rds"))

palettes <- readRDS(file = "palettes.rds")

pal <- brewer.pal(9,"Blues")
pal <- pal[-(1:5)]



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

# wordcloud2 html output fix from https://github.com/Lchiffon/wordcloud2/issues/20
simpleFixWc2 <- function(inputFile, outputFile){
  a = readLines(inputFile)
  output = paste(a, collapse = "\n")
  output = gsub(">\n\n</div>","></div>",output)
  writeLines(output, outputFile)
  invisible(NULL)
}

langTable <- left_join(x = data_frame(lang = unlist(lang)),
                       y = readRDS(file.path("data", "langCode.rds")) %>%rename(lang = alpha2), by = "lang") %>% 
  mutate(English = stringr::str_extract(string = English, pattern = regex("[[:alnum:]]+")))

# Enable bookmarking
enableBookmarking(store = "server")
