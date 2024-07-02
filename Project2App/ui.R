library(shiny)
library(DT)
library(caret)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Querying Show Data"),
  
  # Sidebar with a slider input for number of bins
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
                        br(),
                         ),
      uiOutput("columns"),
      
    
      br(),
      actionButton("submit", "GO!")),
    mainPanel(
      dataTableOutput("summary")
    )))) 