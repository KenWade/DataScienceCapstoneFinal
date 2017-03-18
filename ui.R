#
# Ken Wade
#
# Johns Hopkins School of Public Health - Data Science 10 - Capstone Project: ThisNextWord
#
# Date 2017-03-17
#

library(shiny)
library(shinydashboard)

Title <- "ThisNextWord: Johns Hopkins School of Public Health Data Science Capstone Project"
titleWidth <- 750

ui <- dashboardPage(
  dashboardHeader(title=Title, titleWidth = titleWidth),
  
  dashboardSidebar(disable = TRUE),

  dashboardBody(
    tags$head(tags$script(
      'Shiny.addCustomMessageHandler("refocus", function(NULL) {document.getElementById("inputPhrase").focus();});')),
    
    fluidRow(
      box(title = "Directions for ThisNextWord:", status = "primary",
        "This webpage acts like a simple word predictor tool often used for mobile device text messaging.",
        br(),br(),
        "Start typing into the text box below. The best completion word, or the best next word, will appear in the four buttons above the text box.",
        br(),br(),
        "The more probable words will be in the buttons towards the left, the least probable towards the right.",
        br(),br(),
        "Additional directions and more information about this application can be found on the ",
        HTML("<a href='http://rpubs.com/KenWade/259776' target='_blank'>ThisNextWord Slide Deck</a>."),
        br(),br(),
        "For the Coursera Peer-graded Assignment, please consider the left-most, highest probability word, the 'prediction of a single word'. ",
        "The others are for 'showing off' and 'making it really sing' as instructor Roger Peng advised in the lectures.",
        width = 12, solidHeader = TRUE)
    ),
    HTML("<br>"),
    fluidRow(
      column(3, uiOutput("button1")),
      column(3, uiOutput("button2")),
      column(3, uiOutput("button3")),
      column(3, uiOutput("button4"))
      ),
    HTML("<br>"),
    fluidRow(
      column(width = 12,
      tags$textarea(id = "inputPhrase", rows =  4, cols = 119, "", autofocus = "autofocus", placeholder="Enter Text Here:"))
#      verbatimTextOutput("outputPhrase"))
    )
  )
)