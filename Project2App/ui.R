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
      textInput("show_name", "Name of Show", value = "", width = NULL, placeholder = "Enter show name"),
      br(),
      selectInput("endpoint", label = "What are you interested in?", 
                  choices = c("episodes", "cast", "seasons"),
                  selected = "cast")),
    
    mainPanel(
      dataTableOutput("summary")
    ))))