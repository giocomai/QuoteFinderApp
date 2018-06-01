library(shiny)
library(shinydashboard)
library(shinycustomloader)

function(request) {
dashboardPage(
  dashboardHeader(title = "EdjNet QuoteFinder"),
  dashboardSidebar(sidebarMenu(
    menuItem("Twitter", tabName = "TwitterMEP", icon = icon("twitter")),
    shiny::HTML("<hr><div class='col-sm-12'><p><b>The QuoteFinder lets you explore</b></p></div>"),
    infoBox(title = "tweets", value = point(nrow(dataset)), icon = icon("twitter"), width = 12, color = "blue", fill = TRUE),
    infoBox(title = "by", value = paste(length(unique(dataset$screen_name)), "MEPs"), icon = icon("users"), width = 12, color = "blue", fill = TRUE)),
    infoBox(title = "posted since", value = min(dataset$date), icon = icon("calendar"), width = 12, color = "blue", fill = TRUE),
    disable = TRUE
  ),
  dashboardBody(
  #  wc2ClickedWord(cloudOutputId = "wordcloud2", inputId = "selected_word"),
    
    tabItems(
      
      ##### TwitterMEP tab ####
      tabItem(tabName = "TwitterMEP",
              
              #### Box 1: Wordcloud ####
              tabBox(id = "wordcloud_plot",
                     tabPanel("Wordcloud",
                              withLoader(ui_element = wordcloud2Output("wordcloud2"),
                                         type = "html",
                                         loader = "loader5"),
                              splitLayout(
                                shiny::sliderInput(inputId = "sizeVarWC2",
                                                   label = "Wordcloud size",
                                                   min = 0.1, 
                                                   max = 2,
                                                   value = 0.5,
                                                   sep = "."
                                ),
                                shiny::sliderInput(inputId = "MaxWords",
                                                   label = "Max number of words",
                                                   min = 0L, 
                                                   max = 1000L,
                                                   value = 200L,
                                                   sep = ".", width = "95%"
                                )
                              ),
                              splitLayout(shiny::uiOutput(outputId = "colourMost_UI"),
                                          shiny::uiOutput(outputId = "colourLeast_UI"),
                                cellWidths = "50%",
tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
                              splitLayout(downloadButton("downloadHtml", "Download Html"),
                                          downloadButton("downloadPng", "Download PNG"))

                              # , 
                              # HTML("<b>Tip</b>: by clicking on a term in the wordcloud, only tweets including it are shown in the table below.")
                     ),

#### Box1, Tab2: Barcharts #####
tabPanel("Barcharts",
         plotOutput('barchartGG'),
         shiny::sliderInput(inputId = "MaxWordsInBarchart",
                            label = "Max number of words",
                            min = 0L, 
                            max = 30L,
                            value = 20L,
                            sep = ".",
                            width = "95%",
                            round = TRUE
         ),
         downloadButton("downloadTidyWordCount", "Download as spreadsheet")),
tabPanel("EP group comparison",
         plotOutput("barchartComparisonGG"),
         radioButtons(inputId = "wcOrBarchartComparison",
                      label = "How would you like to compare?",
                      choices = as.list(c("Comparison wordcloud",
                                          "Commonality wordcloud",
                                          "Comparison barchart")))
         )
                     # ,
                     # tabPanel("Classic wordcloud",
                     #          plotOutput(outputId = "wordcloud"))
              ),
              
              #### Box 2: Date and language ####
              box(
                shiny::radioButtons(inputId = "dateRangeRadio",
                                    label = "Select date range",
                                    choices = as.list(c("Last week", "Last month", "Last three months", "Custom range")),
                                    selected = "Last month", inline = TRUE),
                conditionalPanel(
                  condition = "input.dateRangeRadio == 'Custom range'",
                  shiny::dateRangeInput(inputId = 'dateRange',
                                        weekstart = 1,
                                        label = "Select date range",
                                        start = min(dataset$date), end = max(dataset$date)
                  )
                )
                ,
                HTML("<b>Filter tweets by language</b>"),
                shiny::checkboxInput(inputId = "anyLanguage",
                                     label = "Any language",
                                     value = FALSE),
                conditionalPanel("input.anyLanguage == false",
                                 shiny::selectInput(inputId = "language",
                                                    label = NULL,
                                                    choices = lang,
                                                    selected = "en")),
                conditionalPanel("input.anyLanguage == true",
                                 shiny::checkboxInput(inputId = "colourLanguage",
                                                      label = "A different colour for each language?",
                                                      value = TRUE))
                ,
                uiOutput(outputId = "hashtags_UI"),
                uiOutput(outputId = "trendingHashtagsTitle"),
                uiOutput(outputId = "trendingHashtags"),
                conditionalPanel("input.anyLanguage == false",
                                 shiny::radioButtons(inputId = "sentimentL",
                                                     label = "Type of wordcloud",
                                                     choices = c("Unified",
                                                                 "Sentiment"),
                                                     inline = TRUE)
                )
              ),
              
              #### Box 3: Wordcloud filters ####
              
              tabBox(title = "Search and filter",
                id = "wordcloud_filters",
                tabPanel("By string",
                         textInput(inputId = 'string', label = NULL)
                         # splitLayout(textInput(inputId = 'string', label = NULL),
                         #             #actionButton("filter", "Filter", icon = icon("filter", class = "font-awesome")),
                         #             cellWidths = c("75%", "25%"))
                         )
                ,
                tabPanel("By EP group",
                         shiny::checkboxGroupInput(inputId = "EPgroup",
                                                   label = "Choose group",
                                                   choices = EPGroupShort,
                                                   inline = TRUE)
                         #,actionButton("filterByGroup", "Filter", icon = icon("filter", class = "font-awesome"))
                ),
                tabPanel("By MEP", shiny::uiOutput(outputId = "MEPfilter_UI"))
              ), 
            box(actionButton(inputId = "reset",
                             label = "Reset filters",
                             icon = icon(name = "recycle", lib = "font-awesome")),
                bookmarkButton(label = "Get direct link with current settings")
                ),

              #### Box 4: infobox ####
              box(title = NULL,
                  shiny::htmlOutput(outputId = "HeaderInfoBox"),
                  infoBoxOutput(outputId = "TweetsNr"),
                  infoBoxOutput(outputId = "MEPsNr"),
                  infoBoxOutput(outputId = "DaysNr"),width = 12
              )
              ,
              #### Box 5: table ####
              fluidRow(DT::dataTableOutput(outputId = "table"))
      )
    )
  )
)

}
