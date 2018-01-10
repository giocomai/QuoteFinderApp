library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "EdjNet QuoteFinder"),
  dashboardSidebar(sidebarMenu(
    menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
    menuItem("Trends", tabName = "Trends", icon = icon("comments")),
    menuItem("Comparisons", tabName = "comparisons", icon = icon("bar-chart")),
    menuItem("Time series", tabName = "timeSeries", icon = icon("line-chart")),
    shiny::HTML("<hr><div class='col-sm-12'><p><b>The QuoteFinder lets you explore</b></p></div>"),
    infoBox(title = "tweets", value = point(nrow(datasetFull)), icon = icon("twitter"), width = 12, color = "blue", fill = TRUE),
    infoBox(title = "by", value = paste(length(unique(datasetFull$screen_name)), "MEPs"), icon = icon("users"), width = 12, color = "blue", fill = TRUE)),
    infoBox(title = "posted since", value = min(datasetFull$date), icon = icon("calendar"), width = 12, color = "blue", fill = TRUE)
  ),
  dashboardBody(
    tags$script(HTML(
    "$(document).on('click', '#canvas', function() {",
    'word = document.getElementById("wcSpan").innerHTML;',
    "Shiny.onInputChange('selected_word', word);",
    "});"
  )),
  tabItems(
    
    # First tab content
    tabItem(tabName = "wordcloud",
            tabBox(id = "wordcloud_plot",
                   tabPanel("Classic wordcloud",
                            plotOutput(outputId = "wordcloud")),
                   tabPanel("Interactive wordcloud",
                            wordcloud2Output("wordcloud2"), 
                            HTML("<b>Tip</b>: by clicking on a term in the wordcloud, only tweets including it are shown in the table below.")
                   )
            ),
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
                                      start = min(datasetFull$date), end = max(datasetFull$date)
                )
              ),
              
              shiny::selectInput(inputId = "language",
                                 label = "Filter tweets by language",
                                 choices = lang,
                                 selected = "en")),
            
            tabBox(
              # title = "First tabBox",
              # The id lets us use input$wordcloud_filters on the server to find the current tab
              id = "wordcloud_filters",
              # height = "250px",
              tabPanel("By hashtag",
                       uiOutput(outputId = "hashtags_UI"),
                       shiny::radioButtons(inputId = "sentimentL",
                                           label = "Type of wordcloud",
                                           choices = c("Unified",
                                                       "Sentiment"))),
              tabPanel("By EP group",
                       shiny::checkboxGroupInput(inputId = "EPgroup",
                          label = "Choose group",
                          choices = EPGroupShort, 
                          selected = c("EPP", "S&D"))
)
            ),
box(title = "Enabled filters",
    infoBoxOutput(outputId = "TweetsNr"), width = 12)

            ,
            fluidRow(DT::dataTableOutput(outputId = "table"))
    )
    ,

    tabItem(tabName = "Temp"

    )
  )
  )
)

