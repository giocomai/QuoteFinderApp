library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "EdjNet QuoteFinder"),
  dashboardSidebar(sidebarMenu(
    menuItem("wordcloud", tabName = "wordcloud", icon = icon("cloud")),
    menuItem("By EP group", tabName = "byGroup", icon = icon("comments")),
    menuItem("Comparisons", tabName = "comparisons", icon = icon("bar-chart")),
    menuItem("Time series", tabName = "timeSeries", icon = icon("line-chart"))
  )),
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
            box(shiny::selectInput(inputId = "language",
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
fluidRow(title = "Quick data on selected tweets",
    infoBoxOutput(outputId = "TweetsNr"))

            ,
            fluidRow(DT::dataTableOutput(outputId = "table"))
    )
    ,

    tabItem(tabName = "Temp"

    )
  )
  )
)

