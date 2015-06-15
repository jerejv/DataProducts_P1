
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

stateList <-
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

shinyUI(fluidPage(

  # Application title
  titlePanel("Hospital Rankings by State"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Select a state, outcome, and rank to find the hospital corresponding to your inputs:"),
      selectInput('state',"State:", stateList),
      radioButtons("outcome","Outcome:",c("heart attack" = "heart attack", "heart failure" = "heart failure", "pneumonia" = "pneumonia")),
        sliderInput("rank",
                  "Rank of Hospital in State:",
                  min = 1,
                  max = 50,
                  value = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("statement"),
      verbatimTextOutput("hospital"),
        h5("Hospitals are ranked based on lowest 30-day mortality rate."),
        h5("Data was pulled from https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip at the start of the application."),
        h5("A result of NA indicates there is no hospital of the input ranking in the input state.")
    )
  )
))
