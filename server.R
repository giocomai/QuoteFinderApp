library(shiny)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyverse)
library(webshot)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  randomString <- stringi::stri_rand_strings(n=1, length=16)
  
  #### Reset ####
  
  observeEvent(input$reset, {
    updateTextInput(session = session, inputId = "string", value = "")
    updateCheckboxGroupInput(session = session, inputId = "EPgroup", selected = character(0))
  })
  
  #### Reactive ####
  
  currentDataset <- reactive({
    # filter date
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
    # filter language
    dataset <- dataset %>% 
      filter(lang==input$language)
    #filter hashtag
    if(is.null(input$selectedHashtag)){
   
    } else if(input$selectedHashtag=="All tweets") {
     
    } else {
      dataset <- dataset %>% 
        filter(purrr::map_lgl(.x = hashtags, .f = function (x) is.element(el = tolower(input$selectedHashtag), set = tolower(x))))
    }
    
    if (input$string!="") {
      dataset <- dataset %>%
        filter(stringr::str_detect(string = text, pattern = stringr::regex(pattern = input$string, ignore_case = TRUE)))
      }
    
    if (is.null(input$EPgroup)==FALSE) {
      dataset <- dataset %>%
        filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|")))
    }
    
    dataset
  })

  currentHashtags <- reactive({
    
    dataset <- dataset %>% 
      filter(lang==input$language)
    
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
    
    if (is.null(input$EPgroup)==FALSE) {
      dataset <- dataset %>%
        filter(stringr::str_detect(string = GroupShort, pattern = paste(input$EPgroup, collapse = "|")))
    }
    
    currentHashtagsDF <- dataset %>%
      select(screen_name, hashtags) %>%
      unnest() %>%
      na.omit() %>% 
      group_by(hashtags) %>%
      add_count(sort = TRUE) %>% 
      rename(nTotalOrig = n) %>% 
      mutate(hashtagsLower = tolower(hashtags)) %>% # ignore case, but keep the case of the most frequently found case combination
      group_by(hashtagsLower) %>%
      add_tally() %>%
      ungroup() %>% 
      rename(nTotal = n) %>% 
      group_by(hashtags, nTotal) %>% 
      distinct(screen_name, .keep_all = TRUE) %>% 
      add_count() %>% 
      rename(nMepPerHashtag = n) %>% 
      select(-screen_name) %>% 
      arrange(desc(nMepPerHashtag), desc(nTotal)) %>% 
      ungroup() %>% 
      distinct(hashtagsLower, .keep_all = TRUE) %>% 
      mutate(hashtagString = paste0("#", hashtags, " (", nMepPerHashtag, " MEPs, ", nTotal, " tweets)"))
    currentHashtagsList <- as.list(currentHashtagsDF$hashtags)
    names(currentHashtagsList) <- currentHashtagsDF$hashtagString
    currentHashtagsList
  })

  
  #### UI ####
  
  output$hashtags_UI <- renderUI({
    shiny::selectizeInput(inputId = "selectedHashtag",
                          label = "Select hashtag",
                          choices = c(list("All tweets"),
                            currentHashtags()))
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
      # reload if dateRange or hashtag is changed
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
            # for log
            message(paste(Sys.time(), "WordcloudSentimentCreated", input$language, sep = "-"))
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
            # for log
            message(paste(Sys.time(), "WordcloudUnifiedCreated", input$language, sep = "-"))
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
  
  wc2 <- reactive(
      
      if (is.null(input$selectedHashtag)==FALSE) {
        
        ##### Wordcloud2 sentiment or unified #####
        if (input$sentimentL=="Sentiment") {
          if (input$language=="en") {
            sentimentDictionary <- tidytext::get_sentiments("bing")
          } else {
            sentimentDictionary <-
              syuzhet::get_sentiment_dictionary(dictionary = "nrc",
                                                lang = langTable %>%
                                                  filter(lang==input$language) %>%
                                                  pull(English) %>%
                                                  tolower()) %>%
              filter(sentiment=="negative"|sentiment=="positive") %>%
              add_count(word) %>%
              filter(n==1) %>%
              select(word, sentiment)
          }
          
          dataset <- currentDataset() %>% 
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
          
        } else {
          dataset <- currentDataset() %>%
            select(clean_text) %>% 
            unnest_tokens(input = clean_text, output = word) %>% 
            # remove stopwords, if list for the relevant language is available, otherwise do nothing
            when(is.element(el = input$language, set = stopwords::stopwords_getlanguages(source = "stopwords-iso")) ~
                   anti_join(., data_frame(word = c("via", stopwords::stopwords(language = input$language, source = "stopwords-iso"))), by = "word"),
                 ~ .) %>% 
            count(word, sort = TRUE) %>%
            slice(1:input$MaxWords) %>% 
            mutate(colour = bluesFunc(n()))
        }
        
        # add for log
        
        if (input$sentimentL=="Sentiment") {
          message(paste(Sys.time(), "Wordcloud2SentimentCreated", input$language, sep = "-"))
        } else {
          message(paste(Sys.time(), "Wordcloud2UnifiedCreated", input$language, sep = "-"))
        }
        
        dataset 
      }
      
      
  )
  
  output$wordcloud2 <- renderWordcloud2(
    if (is.null(wc2())==FALSE) wc2() %>% wordcloud2(size = if (is.null(input$sizeVarWC2)) 0.5 else input$sizeVarWC2,
                                                            color = wc2()$colour))
  
  #### Download ####
  
  
  
  output$downloadPng <- downloadHandler(
    filename = paste0(randomString, ".png"),
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(widget = wc2() %>% wordcloud2(size = input$sizeVarWC2*2,
                                               color = wc2()$colour), file = paste0(randomString, ".html"), selfcontained = FALSE)
      webshot(url = paste0(randomString, ".html"), file = file, delay = 3, vwidth = 1280, vheight = 960)
    },
    contentType = "image/png")
  
  output$downloadHtml <- downloadHandler(
    filename = paste0(randomString, ".html"),
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(widget = wc2() %>% wordcloud2(size = input$sizeVarWC2*2,
                                               color = wc2()$colour), file = file, selfcontained = TRUE)
      simpleFixWc2(file, file)
    })

  
  #### DataTable ####
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = currentDataset() %>% 
      select(screen_name, date, text, Link, GroupShort) %>% 
      arrange(desc(date))%>% 
      rename(`Twitter handle` = screen_name, Date = date, Tweet = text, `EP Group` = "GroupShort"),
    escape = FALSE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15, 20)), rownames=FALSE)
      
  })
  
  ### InfoBox ####
  
  output$HeaderInfoBox <- renderText({

      paste0(paste0("<div class='col-sm-12'><b>Enabled filters</b>: language: <i>", input$language, "</i>;"),
             if (is.null(input$selectedHashtag)==TRUE) (" hashtag: <i>All tweets</i>;") else if (input$selectedHashtag=="All tweets") (" hashtag: <i>All tweets</i>;") else paste0(" hashtag: <i>#", input$selectedHashtag, "</i>;"),
             if (input$string!="") paste0(" string: <i>", input$string, "</i>;"),
             #" selected word: <i>", gsub(":.*","",input$selected_word), 
             "</div>")
  })
  
  output$TweetsNr <- renderInfoBox({
    infoBox(title = "Tweets",
            value =  point(nrow(currentDataset())),
            icon = icon("twitter"),
            color = "blue"
    )
  })
  
  output$MEPsNr <- renderInfoBox({
  
    infoBox(title = "by",
            value = paste(length(unique(currentDataset()$screen_name)), "MEPs"),
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
