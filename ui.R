library(shiny)

library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "EdjNet QuoteFinder"),
  dashboardSidebar(sidebarMenu(
    menuItem("Wordclouds", tabName = "wordcloud", icon = icon("cloud")),
    menuItem("Comparisons", tabName = "comparisons", icon = icon("bar-chart")),
    menuItem("Time series", tabName = "timeSeries", icon = icon("line-chart"))
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "wordcloud",
            box(plotOutput("wordcloud")),
            box(
              shiny::selectInput(inputId = "language",
                                 label = "Filter tweets by language", choices = lang, selected = "en"),
              uiOutput(outputId = "hashtags_UI"),
              shiny::radioButtons(inputId = "sentimentL", label = "Type of wordcloud", choices = c("Unified", "Sentiment"))
            ),
            fluidRow(DT::dataTableOutput(outputId = "table"))
    )
    ,
    
    # Second tab content
    tabItem(tabName = "comparisons",
            h2("Work in progress")
    )
    ,
    tabItem(tabName = "timeSeries",
            h2("Work in progress")
    )
  )
  )
)


