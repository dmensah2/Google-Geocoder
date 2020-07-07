#Author: Deidre Mensah
#Date: 06/29/2020
#Purpose: Create a Shiny App that takes a csv file of addresses and geocodes them for someone

library(shiny)
library(ggmap)
library(tidyr)
library(xtable)

# Define UI for application that accepts a csv file and geocodes and return a table
# Creates html
# you add inputs and outputs in fluidPage section
ui <- fluidPage(

    # Application title
    titlePanel("Google Geocoder"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "csv",#input slot to access the value
                        label = "Upload your csv of addresses:",#just a label
                        multiple = FALSE,
                        accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
                        width = NULL,
                        placeholder = "No file selected."
                      ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            downloadButton('downloadData', 'Download')
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
           tableOutput(outputId = "geocoded")
        )
    )
)

# 1 save output you build to output$ list - makes outout accessbile to plotOutput function
# 2 build output with render*() function - makes the HTML and keeps track of reactivity
# 3 access input values with input$

# Define server logic required to draw a histogram
# Creates shiny object
# Server side contains most of the code
server <- function(input, output) {
#saves our output of the generated distrubution to output object
    #what you build with output should be saved with render function 
    output$geocoded <- renderTable({
        
        #register Google API Key
        register_google("AIzaSyAryhQfKiL1MiGHHiBO6wL_Nciw4LR61TU")
        
        # Select the file from the file chooser
        path <- input$csv
        
        if (is.null(path))
            return (NULL)
        
        # Read in the CSV data and store it in a variable 
        origAddress <- read.csv(path$datapath, header = TRUE, stringsAsFactors = FALSE)
        
        #stores address csv as a tibble
        new_Addr <- as_tibble(origAddress)
        
        # Loop through the addresses to get the latitude and longitude of each address and add it to the
        # origAddress data frame in new columns lat and lon
        for(i in 1:nrow(new_Addr))
        {
            # geocodes each address and writes the result to a new column in the tibble/dataframe
            result <- geocode(new_Addr$Address[i], output = "latlona", source = "google")
            new_Addr$lon[i] <- as.numeric(result[1])
            new_Addr$lat[i] <- as.numeric(result[2])
            new_Addr$geoAddress[i] <- as.character(result[3])
        }
        

        # Downloadable csv of selected dataset ----
        # we're passing 2 of 4 arguments in the downloadHandler (filename and content)
        output$downloadData <- downloadHandler(
            filename = function() {
                #concatenates strings to form filename
                paste("data-", Sys.Date(),".csv", sep = "")
            },
        
        content = function(file) {
            # Write a CSV file containing origAddress to the working directory
            write.csv(new_Addr, file, row.names=FALSE)
            }
        )
            
        new_Addr.table <- xtable(new_Addr)

    })
}

# Run the application - in R you call function through variable you stored the function it.
#not the name of it, which is always "function"
shinyApp(ui = ui, server = server)
