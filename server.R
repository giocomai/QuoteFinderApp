library(shiny)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyverse)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #### UI ####
  
  output$hashtags_UI <- renderUI({
    shiny::selectizeInput(inputId = "keywords",
                          label = "Introduce hashtag", choices = hashtags[[input$language]])
  })
  
  ## Wordcloud
  
  output$wordcloud <- renderPlot(expr = {
    # if (input$type=="static") {
    if (is.null(input$keywords)==FALSE) {
      temp <- dataset %>% 
        filter(lang==input$language) %>% 
        filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$keywords, set = x))) %>%
        select(clean_text) %>% 
        unnest_tokens(input = clean_text, output = word) %>% 
        anti_join(stop_words, by = "word") 
      if (input$sentimentL=="Sentiment") {
        temp %>% 
          inner_join(get_sentiments("bing"), by = "word") %>%
          count(word, sentiment, sort = TRUE) %>%
          acast(word ~ sentiment, value.var = "n", fill = 0) %>%
          comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                           max.words = 100, scale = c(2.5, 1), vfont=c("serif","plain"))
      } else {
        temp %>% 
          count(word) %>% 
          with(wordcloud(word, n, scale = c(2.5, 1), max.words = 100, min.freq = 1,
                         random.order = FALSE, vfont=c("sans serif","plain"), colors = pal))
        
      }
      # }
    }
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(
    if (is.null(input$keywords)==FALSE) {
      DT::datatable(data = dataset %>% 
                      filter(lang==input$language) %>% 
                      filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$keywords, set = x))) %>%
                      select(screen_name, date, text, Link) %>% 
                      arrange(desc(date)), escape = FALSE
      )
    }
  )
  
  # output$wordcloud2 <- renderWordcloud2({
  #   if (input$type=="mouseover") {
  #   speeches_by_sentence %>% 
  #     filter(stringr::str_detect(string = sentence, pattern = stringr::regex(paste(tolower(trimws(stringr::str_split(string = input$keywords, pattern = ",", simplify = TRUE))), collapse = "|")))) %>%
  #     unnest_tokens(input = sentence, output = word) %>% 
  #     anti_join(stop_words) %>% 
  #     anti_join(exclude) %>% 
  #     count(word) %>% 
  #     rename(freq = n) %>% 
  #     wordcloud2()
  #   }
  ## })
})



