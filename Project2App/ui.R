library(shiny)
library(DT)
library(caret)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Project 2 - TV Shows!"),
  tabsetPanel(
    # About Tab
    tabPanel("About",
    mainPanel(
      h3("Purpose of the App"),
      p("The purpose is to:"),
      h3("Data and Source"),
      p("THe data is "),
      tags$img(src = 'tvshowcollage.png', align = "center", width = "600px", length = "900px")
      

    )         
    ),
    # Data Download Tab
   tabPanel("Data Query and Download",
  sidebarLayout(
    sidebarPanel(
      radioButtons("RB", "Select a Choice Below",
                   choiceNames = c("Return All Shows","Specific Show Search"), 
                   choiceValues = c("all", "specific")),
      
        conditionalPanel("input.RB == 'specific'", 
                         textInput("show_name", "Name of Show", 
                                   value = "", 
                                   width = NULL, 
                                   placeholder = "Enter show name"),
                         br(),
                         selectInput("endpoint", label = "What are you interested in?", 
                                     choices = c("general", "episodes", "cast", "seasons"),
                                     selected = "general"),
                        br()
                         ),
        conditionalPanel("input.RB == 'all'",
                         sliderInput("min_show", "Minimum Show Rating (0 to 10)" ,
                                     min=0, max=10,value=0 ,animate=FALSE,step=0.1)),
        conditionalPanel("input.endpoint == 'episodes'",
                       sliderInput("min_ep", "Minimum Episode Rating (0 to 10)" ,
                                   min=0, max=10,value=0 ,animate=FALSE,step=0.1)),
      
        conditionalPanel("input.endpoint == 'cast'", 
                         selectInput("gender", label = "Gender to keep?", 
                                     choices = c("Both", "Male", "Female"),
                                     selected = "Both")),
      uiOutput("columns"),
      
    
      br(),
      actionButton("submit", "GO!"),
      downloadButton("downloadData", "Download CSV")),
    mainPanel(
      dataTableOutput("summary")
    )
    )),
  tabPanel("Data Exploration Tab"))))
  

