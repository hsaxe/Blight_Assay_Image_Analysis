#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(tidytext)
library(data.table)
library(DT)
library(stringr)
library(shinyWidgets)
library(magick)


x = list.files(path = 'All_Data_Renamed/All_Data/', pattern = '*.jpg')

# read each file and store as list




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("In Vitro Blight Assay Show Image"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput('image',
                  'Select/search image file to view',
                  choices = x),
      width = 4
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot",
                 height = '800px'),
      
      # textOutput('imageLabel'),
      
      width = 8
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    image = reactive({
      
      magick::image_read(paste0('All_Data_Renamed/All_Data/', input$image))
      
    })
    
    
    # output$imageLabel = renderText({
    #   
    #   paste("You have selected", input$image)
    #   
    #   })
  
    output$plot = renderPlot({
    
      image <- image_ggplot(image())
      return(image)
      
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
