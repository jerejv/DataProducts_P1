
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

isValidState <- function(state)
{
    isState <- state %in%
        c(
            'AL',
            'AK',
            'AZ',
            'AR',
            'CA',
            'CO',
            'CT',
            'DE',
            'FL',
            'GA',
            'HI',
            'ID',
            'IL',
            'IN',
            'IA',
            'KS',
            'KY',
            'LA',
            'ME',
            'MD',
            'MA',
            'MI',
            'MN',
            'MS',
            'MO',
            'MT',
            'NE',
            'NV',
            'NH',
            'NJ',
            'NM',
            'NY',
            'NC',
            'ND',
            'OH',
            'OK',
            'OR',
            'PA',
            'RI',
            'SC',
            'SD',
            'TN',
            'TX',
            'UT',
            'VT',
            'VA',
            'WA',
            'WV',
            'WI',
            'WY'
        )
    
    isState
}

isValidOutcome <- function(outcome)
{
    isOutcome <- outcome %in%
        c(
            'heart attack',
            'heart failure',
            'pneumonia'
        )
    
    isOutcome
}


rankhospital <- function(state, outcome, num = "best")
{
    if(!file.exists("./data")){dir.create("./data")}
    if(!file.exists("./data/outcome-of-care-measures.csv"))
    {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
        download.file(fileUrl,destfile="./data/data.zip",mode="wb")
        unzip("./data/data.zip",exdir="./data")
    }
    
    ##Read outcome data
    outcomeData <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
    
    ##Check that state and outcome are valid
    if(!isValidState(state))
    {
        stop("Please enter a valid 2-letter state abbreviation")
    }
    if(!isValidOutcome(outcome))
    {
        stop("Please select a valid outcome")
    }
    
    ##Return hospital name in that state with lowest 30-day death rate
    if(num == 'best')
    {
        if(outcome == 'heart attack')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                                                 , stateData$Hospital.Name),]
            sortedOutcomeData$Hospital.Name[1]
        }
        else if(outcome == 'heart failure')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                                                 , stateData$Hospital.Name),]
            sortedOutcomeData$Hospital.Name[1]
        }
        else if(outcome == 'pneumonia')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                                                 , stateData$Hospital.Name),]
            sortedOutcomeData$Hospital.Name[1]
        }
    }
    else if(num == 'worst')
    {
        if(outcome == 'heart attack')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                                                 , stateData$Hospital.Name, decreasing = TRUE),]
            sortedOutcomeData$Hospital.Name[1]
        }
        else if(outcome == 'heart failure')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                                                 , stateData$Hospital.Name, decreasing = TRUE),]
            sortedOutcomeData$Hospital.Name[1]
        }
        else if(outcome == 'pneumonia')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                                                 , stateData$Hospital.Name, decreasing = TRUE),]
            sortedOutcomeData$Hospital.Name[1]
        }
    }
    else
    {
        if(outcome == 'heart attack')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            if(num == nrow(stateData))
            {
                NA
            }
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                                                 , stateData$Hospital.Name),]
            sortedOutcomeData$Hospital.Name[num]
        }
        else if(outcome == 'heart failure')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            if(num == nrow(stateData))
            {
                NA
            }
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                                                 , stateData$Hospital.Name),]
            sortedOutcomeData$Hospital.Name[num]
        }
        else if(outcome == 'pneumonia')
        {
            neededColumns <- outcomeData[c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                                           "Hospital.Name", "State")]
            
            stateData <- neededColumns[neededColumns$State == state,]
            
            if(num == nrow(stateData))
            {
                NA
            }
            
            stateData[,1] <- 
                as.numeric(stateData[,1])
            
            sortedOutcomeData <- stateData[order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                                                 , stateData$Hospital.Name),]
            sortedOutcomeData$Hospital.Name[num]
        }
    }
}

shinyServer(function(input, output) {

  output$hospital <- renderText({rankhospital(input$state, input$outcome, input$rank)
  })
  output$rank <- renderText({input$rank})
  output$state <- renderText({input$state})
  output$statement <- renderText({paste0("The following hospital is ranked #",input$rank," in the state of ",input$state," for ",input$outcome,":")})
})
