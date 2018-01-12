library(shiny)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyverse)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #### UI ####
  
  output$hashtags_UI <- renderUI({
    shiny::selectizeInput(inputId = "selectedHashtag",
                          label = "Select hashtag",
                          choices = c(list("All tweets"),
                                      hashtagsList[[input$language]]))
  })
  
  output$MaxWords_UI <- renderUI({
    shiny::sliderInput(inputId = "MaxWords",
                          label = "Maximum number of words in the wordcloud",
                        min = 1, 
                       max = 1000
                       )
  })
  
  #### Subset date range ####
  
  observe({
    if (input$dateRangeRadio=="Last week") {
      startDate <- Sys.Date()-7
    } else if (input$dateRangeRadio=="Last month") {
      startDate <- Sys.Date()-31
    } else if (input$dateRangeRadio=="Last three months") {
      startDate <- Sys.Date()-91
    }
    
    if (input$dateRangeRadio=="Custom range") {
      updateDateRangeInput(session, "dateRange",
                           start = startDate,
                           end = Sys.Date())
    }
  })

  

  
  #### Wordcloud ####
  
    output$wordcloud <- renderPlot(expr = {
      # reload if dateRange is changed
      input$dateRange
      input$dateRangeRadio
      input$selectedHashtag

      # If tab is "By hashtag"
      if (input$wordcloud_filters=="By hashtag") {
        if (is.null(input$selectedHashtag)==FALSE) {
          par(mar = rep(0, 4))
          if (input$selectedHashtag=="All tweets") {
            temp <- dataset %>% 
              filter(lang==input$language) %>% 
              select(clean_text) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              anti_join(stop_words, by = "word") 
          } else {
            temp <- dataset %>% 
              filter(lang==input$language) %>% 
              filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$selectedHashtag, set = x))) %>%
              select(clean_text) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              anti_join(stop_words, by = "word") 
          }
          if (input$sentimentL=="Sentiment") {
            par(mar = rep(0, 4))
            temp %>% 
              inner_join(get_sentiments("bing"), by = "word") %>%
              count(word, sentiment, sort = TRUE) %>%
              acast(word ~ sentiment, value.var = "n", fill = 0) %>%
              comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                               max.words = 100, scale = c(3, 1), family = "Carlito", font = 1)
          } else {
            par(mar = rep(0, 4))
            temp %>% 
              count(word) %>% 
              with(wordcloud(word, n, scale = c(3, 1), max.words = 100, min.freq = 1,
                             random.order = FALSE, family = "Carlito", font = 1, colors = pal))
            
          }
        }
        ##### By EP Group #####
      } else if (input$wordcloud_filters=="By EP group") {
        par(mar = rep(0, 4))
        temp <- dataset %>% 
          filter(lang==input$language) %>% 
          filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|"))) %>%
          select(clean_text, NATIONALITY, GroupShort) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          anti_join(stop_words, by = "word") 
        if (length(input$EPgroup)==1) {
          
        } else if (length(input$EPgroup)>1) {
          par(mar = rep(0, 4))
          temp %>% 
            count(word, GroupShort, sort = TRUE) %>% 
            acast(word ~ GroupShort, value.var = "n", fill = 0) %>%
            comparison.cloud(
              #colors = c("#F8766D", "#00BFC4"),
              max.words = 100, scale = c(3, 1), family = "Carlito", font = 1)
        }
      }
    }, execOnResize = TRUE)
  
  #### Wordcloud 2 ####
  
  output$wordcloud2 <- renderWordcloud2({
    
    if (input$dateRangeRadio=="Last week") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-7)
    } else if (input$dateRangeRadio=="Last month") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-31)
    } else if (input$dateRangeRadio=="Last three months") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-91)
    } else {
      dataset <- dataset %>%
        filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
    }
    
    if (is.null(input$selectedHashtag)==FALSE) {
      if (input$selectedHashtag=="All tweets") {
        dataset <- dataset %>% 
          filter(lang==input$language)
      } else {
        dataset <- dataset %>% 
          filter(lang==input$language) %>% 
          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$selectedHashtag, set = x))) 
      }
      
      ##### Wordcloud2 sentiment or unified #####
      
      if (input$sentimentL=="Sentiment") {
        dataset <- dataset %>% 
          select(clean_text) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          anti_join(stop_words, by = "word")  %>% 
          inner_join(get_sentiments("bing"), by = "word") %>%
          count(word, sentiment, sort = TRUE) %>% 
          top_n(n = input$MaxWords, wt = n) %>% 
          mutate(colour = if_else(condition = sentiment=="negative",
                                  true = "#F8766D",
                                  false = "#00BFC4")) %>%
          select(-sentiment)
          colour <- dataset$colour
        
      } else {
        dataset <- dataset %>%
          select(clean_text) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          anti_join(stop_words, by = "word")  %>% 
          count(word, sort = TRUE) %>%
          top_n(n = input$MaxWords, wt = n)
        
        # customise output color, gradients of blue by frequency
        colour <- dataset %>% 
          mutate(colour = case_when(
            n <= quantile(n)[1] ~ blues[1],
            n > quantile(n)[1]& n<=quantile(n)[2] ~ blues[2],
            n > quantile(n)[2]& n<=quantile(n)[3] ~ blues[3],
            n > quantile(n)[3]& n<=quantile(n)[4] ~ blues[4],
            n > quantile(n)[4]& n<=quantile(n)[5] ~ blues[4]
          )) %>% pull(colour)
        
      }
      
      dataset %>% 
        wordcloud2(size = 0.5, color = colour)
    }
  })
  
  #### DataTable ####
  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    # reload if dateRange is changed
    input$dateRange
    input$dateRangeRadio
    input$selectedHashtag
    
    if (input$dateRangeRadio=="Last week") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-7)
    } else if (input$dateRangeRadio=="Last month") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-31)
    } else if (input$dateRangeRadio=="Last three months") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-91)
    } else {
      dataset <- dataset %>%
        filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
    }
    
    
    
      if (is.null(input$selected_word)==FALSE) {
        if(input$selectedHashtag=="All tweets") {
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>% 
                          filter(stringr::str_detect(string = clean_text, pattern = stringr::fixed(gsub(":.*","",isolate(input$selected_word)), ignore_case = TRUE))) %>% 
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date)) %>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"), escape = FALSE)
        } else {
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>% 
                          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$selectedHashtag, set = x)))%>% 
                          filter(stringr::str_detect(string = clean_text, pattern = gsub(":.*","",isolate(input$selected_word)))) %>% 
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date))%>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"), escape = FALSE)
        }
        
      } else {
        if(is.null(input$selectedHashtag)){
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>%
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date))%>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"), escape = FALSE)
        } else if(input$selectedHashtag=="All tweets") {
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>%
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date))%>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"), escape = FALSE)
        } else {
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>%
                          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$selectedHashtag, set = x))) %>% 
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date))%>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"), escape = FALSE)
        }
        
      }
  })
  
  ### InfoBox ####
  
  output$HeaderInfoBox <- renderText({

    if (is.null(input$selectedHashtag)) {
      paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>; hashtag: <i>All tweets</i></div>")
    } else if (input$selectedHashtag=="All tweets") {
        paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>; hashtag: <i>All tweets</i>;",  " selected word: <i>",  gsub(":.*","",input$selected_word), "</i></div>")
    } else {
      paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>; hashtag: <i>#", input$selectedHashtag, "</i>;", " selected word: <i>", gsub(":.*","",input$selected_word), "</i></div>")
    }
    
  })
  
  output$TweetsNr <- renderInfoBox({
    # reload if
    input$dateRange
    input$dateRangeRadio
    input$selectedHashtag
    input$selected_word
    
    if (input$dateRangeRadio=="Last week") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-7)
    } else if (input$dateRangeRadio=="Last month") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-31)
    } else if (input$dateRangeRadio=="Last three months") {
      dataset <-  dataset %>% 
        filter(date>Sys.Date()-91)
    } else {
      dataset <- dataset %>%
        filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
    }
    
    if (is.null(input$selected_word)==FALSE) {
      dataset <- dataset %>% filter(stringr::str_detect(string = clean_text, pattern = stringr::fixed(gsub(":.*","",input$selected_word), ignore_case = TRUE)))
    }
    
    if (is.null(input$selectedHashtag)) {
      filteredTweetsNr <- nrow(dataset %>%
                                 filter(lang==input$language) %>% 
                                 filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))))
    } else if (input$selectedHashtag=="All tweets") {
      filteredTweetsNr <- nrow(dataset %>%
                                 filter(lang==input$language) %>% 
                                 filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))))
    } else {
      filteredTweetsNr <- nrow(dataset %>%
                                 filter(lang==input$language) %>% 
                                 filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) %>% 
                                 filter(purrr::map_lgl(.x = hashtags,
                                                       .f = function (x) is.element(el = input$selectedHashtag, set = x))))
    }
    
    
    infoBox(title = "Tweets",
            value =  point(filteredTweetsNr),
            icon = icon("twitter"),
            color = "blue"
    )
    
    
  })
  
  output$MEPsNr <- renderInfoBox({
    # reload if dateRange is changed
    input$dateRange
    input$dateRangeRadio
    input$selected_word
    input$selectedHashtag
    
    dataset <- dataset %>% 
      filter(lang==input$language)
    
    if (input$dateRangeRadio=="Last week") {
      dataset <-  dataset %>% 
        filter(date>=Sys.Date()-7)
    } else if (input$dateRangeRadio=="Last month") {
      dataset <-  dataset %>% 
        filter(date>=Sys.Date()-31)
    } else if (input$dateRangeRadio=="Last three months") {
      dataset <-  dataset %>% 
        filter(date>=Sys.Date()-91)
    } else {
      dataset <- dataset %>%
        filter(date>=min(as.Date(input$dateRange))&date<=max(as.Date(input$dateRange))) 
    }
    
    if (is.null(input$selected_word)==FALSE) {
      dataset <- dataset %>% filter(stringr::str_detect(string = clean_text, pattern = stringr::fixed(gsub(":.*","",input$selected_word), ignore_case = TRUE)))
    }

    if (is.null(input$selectedHashtag)==FALSE) {
      if (input$selectedHashtag!="All tweets") {
        dataset <- dataset %>% filter(purrr::map_lgl(.x = hashtags,
                                                     .f = function (x) is.element(el = input$selectedHashtag, set = x)))
      }
    }
    
    infoBox(title = "by",
            value = paste(length(unique(dataset$screen_name)), "MEPs"),
            icon = icon("users"), color = "blue", fill = FALSE)
  
  })

  output$DaysNr <- renderInfoBox({
    # reload if dateRange is changed
    input$dateRange
    input$dateRangeRadio

    if (input$dateRangeRadio=="Last week") {
      infoBox(title = "posted in", value = paste(7, "days"),
              icon = icon("calendar"), color = "blue", fill = FALSE)
    } else if (input$dateRangeRadio=="Last month") {
      infoBox(title = "posted in", value = paste(31, "days"),
              icon = icon("calendar"), color = "blue", fill = FALSE)
    } else if (input$dateRangeRadio=="Last three months") {
      infoBox(title = "posted in", value = paste(91, "days"),
              icon = icon("calendar"), color = "blue", fill = FALSE)
    } else {
      infoBox(title = "posted in", value = paste(as.Date(input$dateRange[2])-as.Date(input$dateRange[1]), "days"),
              icon = icon("calendar"), color = "blue", fill = FALSE)
    }
    
  })
  

})



