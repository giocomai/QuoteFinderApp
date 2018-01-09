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
  dashboardBody(tabItems(
    
    # First tab content
    tabItem(tabName = "wordcloud",
            box(plotOutput(outputId = "wordcloud")),
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
            )
            ,
            fluidRow(DT::dataTableOutput(outputId = "table"))
    )
    ,
    # By group
    tabItem(tabName = "byGroup",
            box(plotOutput("wordcloud_ByGroup")),
            box(
              shiny::selectInput(inputId = "language_ByGroup",
                                 label = "Filter tweets by language",
                                 choices = lang,
                                 selected = "en")
            ),
            fluidRow(DT::dataTableOutput(outputId = "table_ByGroup")
            )
    )
  )
  )
)

