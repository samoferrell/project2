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
      p("The purpose of the app is to create an environment for the user to analyze data about shows."),
      h3("Data and Source"),
      p("The data comes from https://www.tvmaze.com/api. This API was made to allow those interested to
        bring information about TV Shows to their website or app. This API has many endpoints and allows
        the user to access general information about some of the most popular shows. Then, for each show,
        users can access information about the episodes, seasons, cast, and much more. "),
      h3("Tabs"),
      p("There are two other tabs within this app. In the 'Data Query and Download' tab, users are 
      able to query through the TV Show API and subset by rows and columns! Then, they are able to download the data they have subsetted. Within the 
        'Data Exploration' tab, users are able to choose variables/ combinations of variables that are 
        summarized via numerical and graphical summaries."),
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
        conditionalPanel("input.endpoint == 'episodes' && input.RB == 'specific'",
                       sliderInput("min_ep", "Minimum Episode Rating (0 to 10)" ,
                                   min=0, max=10,value=0 ,animate=FALSE,step=0.1)),
      
        conditionalPanel("input.endpoint == 'cast' && input.RB == 'specific'", 
                         selectInput("gender", label = "Gender to keep?", 
                                     choices = c("Both", "Male", "Female"),
                                     selected = "Both")),
      uiOutput("columns"),
      
    
      br(),
      actionButton("submit", "GO!"),
      p("note: hitting GO! will reset the column checkboxes for easy refreshing :)"),
      downloadButton("downloadData", "Download CSV")),
    mainPanel(
      dataTableOutput("summary")
    )
    )),
  # data exploration tab, mainPanel and sidebarPanel changes when condition is met:
  tabPanel("Data Exploration",
           sidebarLayout(
             sidebarPanel(
               selectInput("tabs", "What are you interested in?", 
                           choices = c("Numerical Summaries across Genres" = "numeric",
                                       "Contingency Tables" = "tables",
                                       "Plots" = "plots"),
                           selected = "numeric"),
               conditionalPanel("input.tabs == 'numeric'",
                            checkboxGroupInput("selected_genres", "Select Genres:",
                                  choices = c("Action", "Adventure", "Anime", "Comedy", "Crime", 
                                              "Drama", "Espionage", "Family", "Fantasy", "History", 
                                              "Horror", "Legal", "Medical", "Music", "Mystery", 
                                              "Romance", "Science-Fiction", "Sports", "Supernatural", 
                                              "Thriller", "War", "Western"))),
               conditionalPanel("input.tabs == 'tables'",
                                checkboxGroupInput("table_vars", "Contingency Table Variables:",
                                                   choices = c("status", "type", "runtime", "language", "rating")),
                                p("Rating Classification:"),
                                p("0 - 2.5 = bad"),
                                p("2.6 - 5 = poor"),
                                p("5.1 - 7.5 = good"),
                                p("7.6 - 10 = great")),
               conditionalPanel("input.tabs == 'plots'",
                                radioButtons("RBplots", "Select a Plot Below",
                                             choiceNames = c("Episode Ratings by Season","Show Ratings by Year and Type", "Cast Plot"), 
                                             choiceValues = c("choice_ep", "choice_show", "choice_cast"))),
               conditionalPanel("input.tabs == 'plots' && input.RBplots == 'choice_ep'",
                                radioButtons("season_avg", "Bar Graph for Season Average?",
                                             choiceNames = c("Yes","No"), 
                                             choiceValues = c("yes", "no"),
                                             selected = "no"),
                                radioButtons("facet", "Facet Plots by Season?",
                                             choiceNames = c("Yes","No"), 
                                             choiceValues = c("yes", "no"),
                                             selected = "no"))
                                
             ),
             mainPanel(
               conditionalPanel("input.tabs == 'numeric'",
               h4("All Shows Filtered by Selected Genre"),
               dataTableOutput("genre_data"),
               br(),
               h4("Summary Statistics for Ratings across Selected Genres"),
               verbatimTextOutput("summary_table")),
               conditionalPanel("input.tabs == 'tables'",
                                h4("Contingiency Tables"),
                                verbatimTextOutput("tables")),
               conditionalPanel("input.tabs == 'plots' && (input.RBplots == 'choice_ep' || input.RBplots == 'choice_cast')",
                                textInput("show_eps", "Name of Show", 
                                          value = "", 
                                          width = NULL, 
                                          placeholder = "Enter show name, try: 'Game of Thrones'")),
               conditionalPanel("input.tabs == 'plots' && input.RBplots == 'choice_ep'",
                                plotOutput("ep_rating_plot")),
               conditionalPanel("input.tabs == 'plots' && input.RBplots == 'choice_show'",
                                plotOutput("show_rating_plot")),
               conditionalPanel("input.tabs == 'plots' && input.RBplots == 'choice_cast'",
                                plotOutput("cast_plot"))
               
               
               ),
             
               
             ))
           
  
  
  
  
    )))
  

