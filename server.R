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
                           start = min(dataset$date),
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
              # remove stopwords, if list for the relevant language is available, otherwise do nothing
              when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                     anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
                   ~ .)
              
          } else {
            temp <- dataset %>% 
              filter(lang==input$language) %>% 
              filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x)))) %>%
              select(clean_text) %>% 
              unnest_tokens(input = clean_text, output = word) %>% 
              # remove stopwords, if list for the relevant language is available, otherwise do nothing
              when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                     anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
                   ~ .)
          }
          if (input$sentimentL=="Sentiment") {
            par(mar = rep(0, 4))
            temp %>% 
              # nrc sentiment, removing words that are both positive and negative
              inner_join(y = syuzhet::get_sentiment_dictionary(dictionary = "nrc",
                                                               lang = langTable %>% filter(lang==input$language) %>% pull(English) %>% tolower()) %>%
                           filter(sentiment=="negative"|sentiment=="positive") %>% add_count(word) %>% filter(n==1) %>% select(-n), by = "word") %>%
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
          # remove stopwords, if list for the relevant language is available, otherwise do nothing
          when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                 anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
               ~ .)
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
          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x)))) 
      }
      
      ##### Wordcloud2 sentiment or unified #####
      
      if (input$sentimentL=="Sentiment") {
        if (input$language=="en") {
          sentimentDictionary <- tidytext::get_sentiments("bing")
        } else {
          sentimentDictionary <- syuzhet::get_sentiment_dictionary(dictionary = "nrc",
                                                                   lang = langTable %>% filter(lang==input$language) %>% pull(English) %>% tolower()) %>%
            filter(sentiment=="negative"|sentiment=="positive") %>% add_count(word) %>% filter(n==1) %>% select(word, sentiment)
        }
        
        
        dataset <- dataset %>% 
          select(clean_text) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          # remove stopwords, if list for the relevant language is available, otherwise do nothing
          when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "snowball")) ~
                 anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "snowball")), by = "word"),
               is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                 anti_join(., data_frame(word = stopwords::stopwords(language = input$language, source = "stopwords-iso")), by = "word"),
               ~ .) %>% 
          # nrc sentiment, removing words that are both positive and negative
          inner_join(y = sentimentDictionary, by = "word") %>%
          count(word, sentiment, sort = TRUE) %>% 
          slice(1:input$MaxWords) %>% 
          mutate(colour = if_else(condition = sentiment=="negative",
                                  true = "#F8766D",
                                  false = "#00BFC4")) %>%
          select(-sentiment)
          colour <- dataset$colour
        
      } else {
        dataset <- dataset %>%
          select(clean_text) %>% 
          unnest_tokens(input = clean_text, output = word) %>% 
          # remove stopwords, if list for the relevant language is available, otherwise do nothing
          when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                 anti_join(., data_frame(word = c("via", stopwords::stopwords(language = input$language, source = "stopwords-iso"))), by = "word"),
               ~ .) %>% 
          count(word, sort = TRUE) %>%
          slice(1:input$MaxWords) 
        
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
      
      # try to deal with changing size of graph when little difference between values
      if (input$sentimentL=="Sentiment") {
        sizeVar <- as.numeric(quantile(dataset$n)[5]/quantile(dataset$n)[1]/nrow(dataset)*3.5)
      } else {
        sizeVar <- as.numeric(quantile(dataset$n)[5]/quantile(dataset$n)[1]/nrow(dataset)*5)
      }
      
      #sizeVar <- as.numeric(quantile(dataset$n)[5]/quantile(dataset$n)[1]/log(nrow(dataset))/10)
      
      dataset %>% 
        wordcloud2(size = sizeVar, color = colour)
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
                          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x))))%>% 
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
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"),
                        escape = FALSE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15, 20)), rownames=FALSE)
        } else if(input$selectedHashtag=="All tweets") {
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>%
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date))%>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"),
                        escape = FALSE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15, 20)), rownames=FALSE)
        } else {
          DT::datatable(data = dataset %>% 
                          filter(lang==input$language) %>%
                          filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = input$selectedHashtag, set = x))) %>% 
                          select(screen_name, date, text, Link, GroupShort) %>% 
                          arrange(desc(date))%>% 
                          rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"),
                        escape = FALSE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15, 20)), rownames=FALSE)
        }
        
      }
  })
  
  ### InfoBox ####
  
  output$HeaderInfoBox <- renderText({

    if (is.null(input$selectedHashtag)) {
      paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>; hashtag: <i>All tweets</i></div>")
    } else if (input$selectedHashtag=="All tweets") {
        paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>; hashtag: <i>All tweets</i>;",
               #" selected word: <i>",  gsub(":.*","",input$selected_word),
               "</i></div>")
    } else {
      paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>; hashtag: <i>#", input$selectedHashtag, "</i>;",
             #" selected word: <i>", gsub(":.*","",input$selected_word), 
             "</i></div>")
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
