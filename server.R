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
  
  #### Wordcloud ####
  
    output$wordcloud <- renderPlot(expr = {
      # If tab is "By hashtag"
      if (input$wordcloud_filters=="By hashtag") {
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
        }
        ##### By EP Group #####
      } else if (input$wordcloud_filters=="By EP group") {
        temp <- dataset %>% 
          filter(lang==input$language) %>% 
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|"))) %>%
          select(clean_text, NATIONALITY, GroupShort) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          anti_join(stop_words, by = "word") 
        if (length(input$EPgroup)==1) {
          
        } else if (length(input$EPgroup)>1) {
          temp %>% 
            count(word, GroupShort, sort = TRUE) %>% 
            acast(word ~ GroupShort, value.var = "n", fill = 0) %>%
            comparison.cloud(
              #colors = c("#F8766D", "#00BFC4"),
              max.words = 100, scale = c(2.5, 1), vfont=c("serif","plain"))
        }
      }
    }, execOnResize = TRUE)
  #### Wordcloud 2 ####
  
  output$wordcloud2 <- renderWordcloud2({
    if (input$wordcloud_filters=="By hashtag") {
      if (is.null(input$keywords)==FALSE) {
        temp <- dataset %>% 
          filter(lang==input$language) %>% 
          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$keywords, set = x))) %>%
          select(clean_text) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          anti_join(stop_words, by = "word")  %>% 
          count(word) 
        # customise output color, gradients of blue by frequency
        colour <- temp %>% 
          mutate(colour = case_when(
            n <= quantile(n)[1] ~ blues[1],
            n > quantile(n)[1]& n<=quantile(n)[2] ~ blues[2],
            n > quantile(n)[2]& n<=quantile(n)[3] ~ blues[3],
            n > quantile(n)[3]& n<=quantile(n)[4] ~ blues[4],
            n > quantile(n)[4]& n<=quantile(n)[5] ~ blues[4]
          )) %>% pull(colour)
        temp %>% 
          wordcloud2(size = 0.5, color = colour)
      }
    }
  })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(
    if (is.null(input$keywords)==FALSE) {
      if (is.null(input$selected_word)==FALSE) {
        DT::datatable(data = dataset %>% 
                        filter(lang==input$language) %>% 
                        filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$keywords, set = x))) %>%
                        filter(stringr::str_detect(string = clean_text, pattern = gsub(":.*","",isolate(input$selected_word)))) %>% 
                        select(screen_name, date, text, Link, GroupShort) %>% 
                        arrange(desc(date)), escape = FALSE)
      } else {
        DT::datatable(data = dataset %>% 
                        filter(lang==input$language) %>% 
                        filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$keywords, set = x))) %>%
                        select(screen_name, date, text, Link, GroupShort) %>% 
                        arrange(desc(date)), escape = FALSE)
      }
    }
  )
  
  ### InfoBox ####
  
  output$TweetsNr <- renderInfoBox({
    infoBox(title = "Number of tweets",
            value =  nrow(dataset %>% 
                            filter(lang==input$language) %>% 
                            filter(purrr::map_lgl(.x = hashtags,
                                                  .f = function (x) is.element(el = input$keywords, set = x)))),
            icon = icon("twitter"),
            color = "blue"
    )
  })
  
})



