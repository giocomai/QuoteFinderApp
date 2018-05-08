library(shiny)
library(shinydashboard)

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
                     tabPanel("Interactive wordcloud",
                              wordcloud2Output("wordcloud2"),
                              shiny::sliderInput(inputId = "MaxWords",
                                                 label = "Maximum number of words in the wordcloud",
                                                 min = 0L, 
                                                 max = 1000L,
                                                 value = 200L,
                                                 sep = "."
                              )
                              # , 
                              # HTML("<b>Tip</b>: by clicking on a term in the wordcloud, only tweets including it are shown in the table below.")
                     ),
                     tabPanel("Classic wordcloud",
                              plotOutput(outputId = "wordcloud"))
              ),
              
              #### Box 2: Date and language ####
              box(
                shiny::radioButtons(inputId = "dateRangeRadio",
                                    label = "Select date range",
                                    choices = as.list(c("Last week", "Last month", "Last three months", "Custom range")),
                                    selected = "Last month"),
                conditionalPanel(
                  condition = "input.dateRangeRadio == 'Custom range'",
                  shiny::dateRangeInput(inputId = 'dateRange',
                                        weekstart = 1,
                                        label = "Select date range",
                                        start = min(dataset$date), end = max(dataset$date)
                  )
                )
                ,

                shiny::selectInput(inputId = "language",
                                   label = "Filter tweets by language",
                                   choices = lang,
                                   selected = "en")
                ),
              
              #### Box 3: Wordcloud filters ####
              
              tabBox(
                id = "wordcloud_filters",
                tabPanel("Search and filter",
                         splitLayout(textInput(inputId = 'term', label = NULL),
                                     actionButton("filter", "Filter"), cellWidths = c("75%", "25%")),
                         uiOutput(outputId = "hashtags_UI"),
                         shiny::radioButtons(inputId = "sentimentL",
                                             label = "Type of wordcloud",
                                             choices = c("Unified",
                                                         "Sentiment")))
                ,
                tabPanel("By EP group",
                         shiny::checkboxGroupInput(inputId = "EPgroup",
                                                   label = "Choose group",
                                                   choices = EPGroupShort,
                                                   selected = c("EPP", "S&D")),
                         actionButton("filterByGroup", "Filter")
                         
                )
              ), 
            box(bookmarkButton(label = "Get direct link with current filters enabled"))
            ,
              
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
