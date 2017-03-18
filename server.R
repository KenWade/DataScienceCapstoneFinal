#
# Ken Wade
#
# Johns Hopkins School of Public Health - Data Science 10 - Capstone Project: ThisNextWord
#
# Date 2017-03-17
#

library(shiny)
library(ngram)

# Variables

#setwd("C:/Users/Ken/Documents/Ken/Continuing Education/Johns Hopkins School of Public Health - Data Science 10 - Capstone/Code/Data")
uniFreqFileName  <- "uniFreq.txt"
biFreqFileName   <- "biFreq.txt"
triFreqFileName  <- "triFreq.txt"
quadFreqFileName <- "quadFreq.txt"
profanityFileName  <- "profanity.txt"
contractionFileName <- "contraction.txt"

number_of_predicted_words <- 4

buttonWords <- c("-", "-", "-", "-")


# Shiny Server Logic
shinyServer(function(input, output, session) {

# Functions
  
####
##
## updateButtons - function call to update the buttons on the UI
##
updateButtons <- function(words) {
  output$button1 <- renderUI({ actionButton("action1", label = words[1]) })
  output$button2 <- renderUI({ actionButton("action2", label = words[2]) })
  output$button3 <- renderUI({ actionButton("action3", label = words[3]) })
  output$button4 <- renderUI({ actionButton("action4", label = words[4]) })
}

####
##
## cleanString - function call to do all the string replacements to make a good search string
##
cleanString <- function(x) {
  x <- preprocess(x, case ="lower", remove.punct = FALSE)
  x <- gsub("[[:digit:]]", "", x)
  x <- gsub(paste(profanity, collapse = "|"), "", x)
  for (i in 1:nrow(contraction)) {
    x <- gsub(contraction$Contraction[i], contraction$Replacement[i], x)
  }
  x <- gsub("[[:punct:]]", " ", x)
  for (i in 1:nrow(contraction)) {
    x <- gsub(contraction$Replacement[i], contraction$Contraction[i], x)
  }
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE)
  return(x)
}

####
##
## get the last word in the vector.
##
lastWord <- function(x) {
  x= x[[length(x)]]
}


##########
##
## predictNextWord
##
##  given a string of at least one word predict the next word
##
##  Input: string of at least one word
##
##  Output: List of 0 to 4 predictions in decending probability order
##
#########
predictNextWord <- function(searchString) {
  #browser()
  searchString <- cleanString(searchString)
  searchString_NumberOfWords <- length(strsplit(searchString," ")[[1]])
  predictedWords <- c()
  
  # search for last 3 words in quad and see what the most common next word is 
  if (searchString_NumberOfWords >= 3) {
    threeWords <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-2) :
                                                          (searchString_NumberOfWords-0)], collapse=" ")
    quadList <- grep(paste("^", threeWords, " ", sep=""), quadFreq$word)
    if (length(quadList) > 0) {
      for (i in 1:min(number_of_predicted_words,length(quadList))) {
        predictedWords <- append(predictedWords,
                                 lastWord(strsplit(quadFreq[quadList,]$word, " ")[[i]]))
      }
    }
  }
  
  # search for last 2 words in tri and see what the most common next word is
  if (searchString_NumberOfWords >= 2) {
    twoWords <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-1) :
                                                        (searchString_NumberOfWords-0)], collapse=" ")
    triList <- grep(paste("^", twoWords, " ", sep=""), triFreq$word)
    if (length(triList) > 0) {
      for (i in 1:min(number_of_predicted_words,length(triList))) {
        predictedWords <- append(predictedWords, lastWord(strsplit(triFreq[triList,]$word,
                                                                   " ")[[i]]))
      }
    }
  }
  
  # search for last word in bi and see what the most common next word is
  if (searchString_NumberOfWords >= 1) {
    lastWord <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-0) :
                                                        (searchString_NumberOfWords-0)], collapse=" ")
    biList <- grep(paste("^", lastWord, " ", sep=""), biFreq$word)
    if (length(biList) > 0) {
      for (i in 1:min(number_of_predicted_words,length(biList))) {
        predictedWords <- append(predictedWords, lastWord(strsplit(biFreq[biList,]$word,
                                                                   " ")[[i]]))
      }
    }
  }
  
  # finally, just pick the first few most common words
  for (i in 1:min(number_of_predicted_words,length(uniFreq$word))) {
    predictedWords <- append(predictedWords, trimws(uniFreq$word[i]))
  }
  
  predictedWords <- unique(predictedWords)
  predictedWords <- head(predictedWords, number_of_predicted_words)
  return(predictedWords)
}


##########
##
## predictNextLetter
##
##  given a string of at least one word predict the character
##
##  Input: string of at least one word
##
##  Output: List of 0 to 4 predictions in decending probability order
##
#########

predictNextLetter <- function(searchString) {
  #browser()
  
  searchString <- cleanString(searchString)
  searchString_NumberOfWords <- length(strsplit(searchString," ")[[1]])
  predictedWords <- c()
  
  # search for last 4 words in quad and see what the most common next word completion is 
  if (searchString_NumberOfWords >= 4) {
    fourWords <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-3) :
                                                         (searchString_NumberOfWords-0)], collapse=" ")
    quadList <- grep(paste("^", fourWords, sep=""), quadFreq$word)
    if (length(quadList) > 0) {
      for (i in 1:min(number_of_predicted_words,length(quadList))) {
        predictedWords <- append(predictedWords,
                                 lastWord(strsplit(quadFreq[quadList,]$word, " ")[[i]]))
      }
    }
  }
  
  # search for last 3 words in tri and see what the most common next word completion is
  if (searchString_NumberOfWords >= 3) {
    threeWords <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-2) :
                                                          (searchString_NumberOfWords-0)], collapse=" ")
    triList <- grep(paste("^", threeWords, sep=""), triFreq$word)
    if (length(triList) > 0) {
      for (i in 1:min(number_of_predicted_words,length(triList))) {
        predictedWords <- append(predictedWords, 
                                 lastWord(strsplit(triFreq[triList,]$word, " ")[[i]]))
      }
    }
  }
  
  # search for last 2 words in bi and see what the most common next word completion is
  if (searchString_NumberOfWords >= 2) {
    twoWords <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-1) :
                                                        (searchString_NumberOfWords-0)], collapse=" ")
    biList <- grep(paste("^", twoWords, sep=""), biFreq$word)
    if (length(biList) > 0) {
      for (i in 1:min(number_of_predicted_words,length(biList))) {
        predictedWords <- append(predictedWords, lastWord(strsplit(biFreq[biList,]$word,
                                                                   " ")[[i]]))
      }
    }
  }
  
  # finally search for the word in uni and see what the most common word completion is
  oneWord <- paste(strsplit(searchString," ")[[1]][(searchString_NumberOfWords-0) :
                                                     (searchString_NumberOfWords-0)], collapse=" ")
  uniList <- grep(paste("^", oneWord, sep=""), uniFreq$word)
  if (length(uniList) > 0) {
    for (i in 1:min(number_of_predicted_words,length(uniList))) {
      predictedWords <- append(predictedWords, lastWord(strsplit(uniFreq[uniList,]$word,
                                                                 " ")[[i]]))
    }
  }
  
  # just for safety, pick the first few most common words
  for (i in 1:min(number_of_predicted_words,length(uniFreq$word))) {
    predictedWords <- append(predictedWords, trimws(uniFreq$word[i]))
  }
  
  predictedWords <- unique(predictedWords)
  predictedWords <- head(predictedWords, number_of_predicted_words)
  return(predictedWords)
}

# Run Once

  ## Reading the dataset files:
  
  uniFreq  <- read.table(uniFreqFileName,  sep="\t", stringsAsFactors = FALSE)
  biFreq   <- read.table(biFreqFileName,   sep="\t", stringsAsFactors = FALSE)
  triFreq  <- read.table(triFreqFileName,  sep="\t", stringsAsFactors = FALSE)
  quadFreq <- read.table(quadFreqFileName, sep="\t", stringsAsFactors = FALSE)
  
  connection <- file(profanityFileName, "rb")
  profanity <- readLines(connection, encoding="UTF-8", skipNul=TRUE)
  close(connection)
  
  connection <- file(contractionFileName, "rb")
  cont <- readLines(connection, encoding="UTF-8", skipNul=TRUE)
  close(connection)
  
  contraction <- data.frame(cont, as.character(101:(100+length(cont))),
                            stringsAsFactors = FALSE)
  colnames(contraction) <- c("Contraction", "Replacement")
  
# Setup the buttons for the first time
  
  updateButtons(buttonWords)
  
# Button Reactive Code

  observeEvent(input$action1, {
    if (substr(input$inputPhrase, nchar(input$inputPhrase), nchar(input$inputPhrase)) == " ") {
      updateTextInput(session, "inputPhrase", value = paste(input$inputPhrase, buttonWords[1], " ", sep = ""))
    } else {
      updateTextInput(session, "inputPhrase",
            value = paste(gsub("\\s*\\w*$", "", input$inputPhrase), " ", buttonWords[1], " ", sep = ""))
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(input$action2, {
    if (substr(input$inputPhrase, nchar(input$inputPhrase), nchar(input$inputPhrase)) == " ") {
      updateTextInput(session, "inputPhrase", value = paste(input$inputPhrase, buttonWords[2], " ", sep = ""))
    } else {
      updateTextInput(session, "inputPhrase",
            value = paste(gsub("\\s*\\w*$", "", input$inputPhrase), " ", buttonWords[2], " ", sep = ""))
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(input$action3, {
    if (substr(input$inputPhrase, nchar(input$inputPhrase), nchar(input$inputPhrase)) == " ") {
      updateTextInput(session, "inputPhrase", value = paste(input$inputPhrase, buttonWords[3], " ", sep = ""))
    } else {
      updateTextInput(session, "inputPhrase",
            value = paste(gsub("\\s*\\w*$", "", input$inputPhrase), " ", buttonWords[3], " ", sep = ""))
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })

  observeEvent(input$action4, {
    if (substr(input$inputPhrase, nchar(input$inputPhrase), nchar(input$inputPhrase)) == " ") {
      updateTextInput(session, "inputPhrase", value = paste(input$inputPhrase, buttonWords[4], " ", sep = ""))
    } else {
      updateTextInput(session, "inputPhrase",
            value = paste(gsub("\\s*\\w*$", "", input$inputPhrase), " ", buttonWords[4], " ", sep = ""))
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
# Text change Reactive Code
  
  observe({
    output$outputPhrase <- renderText({input$inputPhrase})
    lc <- substr(input$inputPhrase, nchar(input$inputPhrase), nchar(input$inputPhrase))
    if (!grepl("[[:alnum:]]", lc)) {
      buttonWords <<- predictNextWord(input$inputPhrase)
    } else {
      buttonWords <<- predictNextLetter(input$inputPhrase)
    }
    updateButtons(buttonWords)
  })
  
})
