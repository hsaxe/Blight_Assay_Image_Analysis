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
library(lubridate)


x = list.files(path = 'All_Data_Renamed/All_Data/', pattern = '*.csv')

# read each file and store as list
y = lapply(x, FUN = function(z){
  # reads file
  a = read.csv(paste0('All_Data_Renamed/All_Data/', z))
  # makes a variable containing the filename
  b = a %>%
    mutate(filename = gsub('.csv', '', z))

})

# bind all data frames together
z = bind_rows(y)

b = z %>%
  # Removed transposed trays and EPL experiments
  filter(!filename %like% 'EPL|2022-03-28') %>%
  group_by(filename) %>%
  mutate(tray_number = seq(1:n()),
         row = str_extract(kernel_name, '_.._')) %>%
  group_by(filename, row) %>%
  # Make more intuitive tray numbering
  mutate(new_tray_number = rev(tray_number)) %>%
  ungroup() %>%
  select(!c(row, tray_number)) %>%
  arrange(-desc(new_tray_number)) %>%
  # Extract variables from filename
  separate(filename, into = c('Date', 'Genotype(s)', 'Locations', 'Assay_type', 'DAI', 'Stage', 'Side'), sep = '_') %>%
  filter(!DAI %in% c('0dai', '1dai')) %>% 
  group_by(Date, `Genotype(s)`, Locations, Assay_type, DAI, Stage, Side) %>%
  mutate(
    # New filename is encoded with information about what each cell contains (genotype, treatment, assay type). This code extracts that information with string manipulation and regular expressions.
    Treatment = if(str_split(`Genotype(s)`, ',')[[1]] %>% length() > 1){
      ifelse(between(new_tray_number,
                     str_extract_all(Locations, '(?<=M)\\d+') %>% unlist() %>% as.numeric(.),
                     str_extract_all(Locations, '(?<=-)\\d+(?=Mm)') %>% unlist() %>% as.numeric(.)),
             'Mock',
             ifelse(between(new_tray_number,
                            str_extract_all(Locations, '(?<=Mm)\\d+') %>% unlist() %>% as.numeric(.),
                            str_extract_all(Locations, '(?<=-)\\d+(?=,X)') %>% unlist() %>% as.numeric(.)),
                    'Mock',
                    ifelse(between(new_tray_number,
                                   str_extract_all(Locations, '(?<=X)\\d+') %>% unlist() %>% as.numeric(.),
                                   str_extract_all(Locations, '(?<=-)\\d+(?=Xx)') %>% unlist() %>% as.numeric(.)),
                           'Xaj',
                           ifelse(between(new_tray_number,
                                          str_extract_all(Locations, '(?<=Xx)\\d+') %>% unlist() %>% as.numeric(.),
                                          str_extract_all(Locations, '(?<=-)\\d+(?=$)') %>% unlist() %>% as.numeric(.)),
                                  'Xaj', 'Empty'))))
    }else{
      ifelse(between(new_tray_number,
                     str_extract_all(Locations, '(?<=M)\\d+') %>% unlist() %>% as.numeric(.),
                     str_extract_all(Locations, '(?<=-)\\d+(?=X)') %>% unlist() %>% as.numeric(.)),
             'Mock',
             ifelse(between(new_tray_number,
                            str_extract_all(Locations, '(?<=X)\\d+') %>% unlist() %>% as.numeric(.),
                            str_extract_all(Locations, '(?<=-)\\d+(?=$)') %>% unlist() %>% as.numeric(.)),
                    'Xaj', 'Empty'))
    },
    Genotype = if(str_split(`Genotype(s)`, ',')[[1]] %>% length() > 1){
      ifelse(between(new_tray_number,
                     str_extract_all(Locations, '(?<=M)\\d+') %>% unlist() %>% as.numeric(.),
                     str_extract_all(Locations, '(?<=-)\\d+(?=Mm)') %>% unlist() %>% as.numeric(.)),
             str_split(`Genotype(s)`, ',')[[1]][1],
             ifelse(between(new_tray_number,
                            str_extract_all(Locations, '(?<=Mm)\\d+') %>% unlist() %>% as.numeric(.),
                            str_extract_all(Locations, '(?<=-)\\d+(?=,X)') %>% unlist() %>% as.numeric(.)),
                    str_split(`Genotype(s)`, ',')[[1]][2],
                    ifelse(between(new_tray_number,
                                   str_extract_all(Locations, '(?<=X)\\d+') %>% unlist() %>% as.numeric(.),
                                   str_extract_all(Locations, '(?<=-)\\d+(?=Xx)') %>% unlist() %>% as.numeric(.)),
                           str_split(`Genotype(s)`, ',')[[1]][1],
                           ifelse(between(new_tray_number,
                                          str_extract_all(Locations, '(?<=Xx)\\d+') %>% unlist() %>% as.numeric(.),
                                          str_extract_all(Locations, '(?<=-)\\d+(?=$)') %>% unlist() %>% as.numeric(.)),
                                  str_split(`Genotype(s)`, ',')[[1]][2], 'Empty'))))
    }else{
      ifelse(between(new_tray_number,
                     str_extract_all(Locations, '(?<=M)\\d+') %>% unlist() %>% as.numeric(.),
                     str_extract_all(Locations, '(?<=-)\\d+(?=X)') %>% unlist() %>% as.numeric(.)),
             `Genotype(s)`,
             ifelse(between(new_tray_number,
                            str_extract_all(Locations, '(?<=X)\\d+') %>% unlist() %>% as.numeric(.),
                            str_extract_all(Locations, '(?<=-)\\d+(?=$)') %>% unlist() %>% as.numeric(.)),
                    `Genotype(s)`, 'Empty'))
    },
    Assay_type = if(str_split(Assay_type, '-')[[1]] %>% length() > 1){
      ifelse(between(new_tray_number,
                     str_extract_all(Locations, '(?<=M)\\d+') %>% unlist() %>% as.numeric(.),
                     str_extract_all(Locations, '(?<=-)\\d+(?=Mm)') %>% unlist() %>% as.numeric(.)),
             str_split(Assay_type, '-')[[1]][1],
             ifelse(between(new_tray_number,
                            str_extract_all(Locations, '(?<=Mm)\\d+') %>% unlist() %>% as.numeric(.),
                            str_extract_all(Locations, '(?<=-)\\d+(?=,X)') %>% unlist() %>% as.numeric(.)),
                    str_split(Assay_type, '-')[[1]][2],
                    ifelse(between(new_tray_number,
                                   str_extract_all(Locations, '(?<=X)\\d+') %>% unlist() %>% as.numeric(.),
                                   str_extract_all(Locations, '(?<=-)\\d+(?=Xx)') %>% unlist() %>% as.numeric(.)),
                           str_split(Assay_type, '-')[[1]][1],
                           ifelse(between(new_tray_number,
                                          str_extract_all(Locations, '(?<=Xx)\\d+') %>% unlist() %>% as.numeric(.),
                                          str_extract_all(Locations, '(?<=-)\\d+(?=$)') %>% unlist() %>% as.numeric(.)),
                                  str_split(Assay_type, '-')[[1]][2], 'Empty'))))
    }else{
      ifelse(between(new_tray_number,
                     str_extract_all(Locations, '(?<=M)\\d+') %>% unlist() %>% as.numeric(.),
                     str_extract_all(Locations, '(?<=-)\\d+(?=X)') %>% unlist() %>% as.numeric(.)),
             Assay_type,
             ifelse(between(new_tray_number,
                            str_extract_all(Locations, '(?<=X)\\d+') %>% unlist() %>% as.numeric(.),
                            str_extract_all(Locations, '(?<=-)\\d+(?=$)') %>% unlist() %>% as.numeric(.)),
                    Assay_type, 'Empty'))
      
    },
    Julian_date = yday(Date)
  ) %>%
  ungroup() %>%
  select(!`Genotype(s)`) %>%
  # Remove these strings from genotype variable as I was able to encode this information into the variable 'Assay_type'
  mutate(Genotype = gsub('nutlets|-nutlets|twigs|-twigs', '', Genotype)) %>% 
  # This code combines the two sides of the nuts
  group_by(Date, Julian_date, Genotype, Treatment, Assay_type, DAI, Stage, new_tray_number) %>%
  summarise(Total_Area2 = sum(total.area), Blighted_Values2 = sum(blighted.values)) %>%
  mutate(Reps = max(new_tray_number)) %>% 
  ungroup() %>%
  replace_na(list(Total_Area2 = 0, Blighted_Values2 = 0)) %>%
  # Calculate percent blighted
  mutate(Percent_blighted = (Blighted_Values2/Total_Area2)*100) %>%
  drop_na()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("In Vitro Blight Assay Analysis"),

    # Sidebar with a select inputs for multiple variables
    sidebarLayout(
        sidebarPanel(
            selectInput('x',
                        'Select x-axis value/factor',
                        choices = NULL),

            selectInput('y',
                        'Select y-axis value/factor',
                        choices = NULL),

            selectInput('fill',
                        'Select what to fill color by',
                        choices = NULL),

            selectInput('facet1',
                        'Select facet one',
                        choices = NULL),

            selectInput('facet2',
                        'Select facet two',
                        choices = NULL),
            
            pickerInput('subset',
                        'Subset x-axis data',
                        options = list(`actions-box` = TRUE),
                        choices = NULL,
                        multiple = T),
            
            selectInput('filterCol',
                        'Filter dataset: Select column',
                        choices = NULL,
                        multiple = F),
            
            pickerInput('filterVal',
                        'Filter dataset: Select value(s)',
                        options = list(`actions-box` = TRUE),
                        choices = NULL,
                        multiple = T),
            
            selectInput('filterCol2',
                        'Filter dataset again: Select column',
                        choices = NULL,
                        multiple = F),
            
            pickerInput('filterVal2',
                        'Filter dataset again: Select value(s)',
                        options = list(`actions-box` = TRUE),
                        choices = NULL,
                        multiple = T),
            
            textInput('Plot_name', 'Name this plot'),
            
            numericInput('Save_width', 'Change size of saved plot',
                         value = 8),
            
            downloadButton('save_plot', 
                           'Save this plot'),
            
            width = 2
        ),

        # Show a plot of the selected variables and data table used to generate it
        mainPanel(
           plotOutput("plot",
                      height = '600px'),

           br(),

           DT::DTOutput('plotData'),
           
           width = 10
        )
    )
)

# Define server logic required to render plot and data table
server <- function(input, output, session) {

  # Dataset must be reactive to accept inputs from ui
  dataset = reactive({

    b

  })

  # datasetMod = reactiveValues()
  # 
  # observe({
  # 
  #   datasetMod$x = dataset()
  # 
  # })

  # Renders data table to ui
  output$plotData = DT::renderDT({

    datatable(dataset3(), 
              selection = list(target = 'column'),
              filter = 'top')
  })


  # These observers are needed for dynamic plotting of variables
  observe({
    updateSelectInput(session, 'x',
                      'Select x-axis value/factor',
                      choices = names(dataset()),
                      selected = 'Genotype')
  })

  observe({
    updateSelectInput(session, 'y',
                      'Select y-axis value/factor',
                      choices = names(dataset()),
                      selected = 'Percent_blighted')
  })

  observe({
    updateSelectInput(session, 'fill',
                      'Select what to fill color by',
                      choices = c('None', names(dataset())),
                      selected = 'Percent_blighted')
  })

  observe({
    updateSelectInput(session, 'facet1',
                      'Select facet one',
                      choices = c('None', names(dataset())),
                      selected = 'Stage')
  })

  observe({
    updateSelectInput(session, 'facet2',
                      'Select facet two',
                      choices = c('None', names(dataset())),
                      selected = 'Treatment')
  })
  
  observe({
    updatePickerInput(session, 'subset',
                      'Subset x-axis data',
                      choices = c('None', unique(dataset()[[input$x]])),
                      selected = 'None')
  })
  
  observe({
    updateSelectInput(session, 'filterCol',
                      'Filter dataset: Select column',
                      choices = c('None', names(dataset())),
                      selected = 'None')
  })
  
  observe( {
    updatePickerInput(session, 'filterVal',
                      'Filter dataset: Select value(s)',
                      choices = unique(dataset()[[input$filterCol]]))
  })
  
  observe({
    updateSelectInput(session, 'filterCol2',
                      'Filter dataset again: Select column',
                      choices = c('None', names(dataset())),
                      selected = 'None')
  })
  
  observe( {
    updatePickerInput(session, 'filterVal2',
                      'Filter dataset again: Select value(s)',
                      choices = unique(dataset()[[input$filterCol2]]))
  })

  
   
    # Conditional statements for proper filling of boxplots and filtering of data
    dataset2 = reactive({
      
      if(input$subset != 'None' && input$facet1 != 'None' && input$facet2 != 'None'){
        
        dataset() %>%
          group_by(.data[[input$x]], .data[[input$facet1]], .data[[input$facet2]]) %>%
          mutate(fill = mean(.data[[input$fill]])) %>%
          ungroup() %>%
          filter(.data[[input$x]] %in% input$subset)
        
        
      } else if(input$subset != 'None' && input$facet1 != 'None' && input$facet2 == 'None') {
        
        dataset() %>%
          group_by(.data[[input$x]], .data[[input$facet1]]) %>%
          mutate(fill = mean(.data[[input$fill]])) %>%
          ungroup() %>%
          filter(.data[[input$x]] %in% input$subset)
        
      } else if(input$subset == 'None' && input$facet1 != 'None' && input$facet2 != 'None'){
        
        dataset() %>%
          group_by(.data[[input$x]], .data[[input$facet1]], .data[[input$facet2]]) %>%
          mutate(fill = mean(.data[[input$fill]])) %>%
          ungroup()
        
      } else if(input$subset != 'None' && input$facet1 == 'None' && input$facet2 == 'None'){
        
        dataset() %>%
          group_by(.data[[input$x]]) %>%
          mutate(fill = mean(.data[[input$fill]])) %>%
          ungroup() %>%
          filter(.data[[input$x]] %in% input$subset)
        
      } else if(input$subset != 'None' && input$facet1 == 'None' && input$facet2 != 'None'){
        
        dataset() %>%
          group_by(.data[[input$x]], .data[[input$facet2]]) %>%
          mutate(fill = mean(.data[[input$fill]])) %>%
          ungroup() %>%
          filter(.data[[input$x]] %in% input$subset)
        
      } else if(input$subset == 'None' && input$facet1 == 'None' && input$facet2 != 'None'){
        
        dataset() %>%
          group_by(.data[[input$x]], .data[[input$facet2]]) %>%
          mutate(fill = mean(.data[[input$fill]])) %>%
          ungroup()
        
      } else {
        
        dataset() %>%
          group_by(.data[[input$x]]) %>%
          mutate(fill = mean(.data[[input$fill]]))
        
      }
      
    })
    
    dataset3 = reactive({
      
      req(input$filterCol)
      
      if(input$filterCol != 'None'){
        
        dataset2() %>% 
          filter(.data[[input$filterCol]] %in% input$filterVal)
        
      } else {
        
        dataset2()
        
      }
      
    })
    
    dataset4 = reactive({
      
      req(input$filterCol2)
      
      if(input$filterCol2 != 'None'){
        
        dataset3() %>% 
          filter(.data[[input$filterCol2]] %in% input$filterVal2)
        
      } else {
        
        dataset3()
        
      }
      
    })
 

  
  # Making reactive plot
  plotInput = reactive({

    if(input$facet1 != 'None' && input$facet2 != 'None'){

      ggplot(dataset4(),
             aes(reorder_within(.data[[input$x]], .data[[input$y]], list(.data[[input$facet1]], .data[[input$facet2]])),
                 .data[[input$y]],
                 fill = fill,
                 group = .data[[input$x]]))+
        geom_boxplot()+
        theme_gray(base_size = 15)+
        theme(axis.text.x = element_text(angle = 25, face = 'bold'),
              plot.title = element_text(hjust = 0.5))+
        scale_fill_continuous(low = 'green', high = 'black',
                              limits = c(0,100))+
        scale_x_reordered()+
        facet_wrap(get(input$facet1) ~ get(input$facet2), scales = 'free_x')

    } else if (input$facet1 != 'None' && input$facet2 == 'None'){

      ggplot(dataset4(),
             aes(reorder_within(.data[[input$x]], .data[[input$y]], .data[[input$facet1]]),
                 .data[[input$y]],
                 fill = fill,
                 group = .data[[input$x]]))+
        geom_boxplot()+
        theme_gray(base_size = 15)+
        theme(axis.text.x = element_text(angle = 25, face = 'bold'),
              plot.title = element_text(hjust = 0.5))+
        scale_fill_continuous(low = 'green', high = 'black',
                              limits = c(0,100))+
        scale_x_reordered()+
        facet_wrap(~get(input$facet1), scales = 'free_x')

    } else if (input$facet1 == 'None' && input$facet2 != 'None'){

      ggplot(dataset4(),
             aes(reorder_within(.data[[input$x]], .data[[input$y]], .data[[input$facet2]]),
                 .data[[input$y]],
                 fill = fill,
                 group = .data[[input$x]]))+
        geom_boxplot()+
        theme_gray(base_size = 15)+
        theme(axis.text.x = element_text(angle = 25, face = 'bold'),
              plot.title = element_text(hjust = 0.5))+
        scale_fill_continuous(low = 'green', high = 'black',
                              limits = c(0,100))+
        scale_x_reordered()+
        facet_wrap(~get(input$facet2), scales = 'free_x')

    } else {

      ggplot(dataset4(),
             aes(reorder(.data[[input$x]], .data[[input$y]]), .data[[input$y]],
                 fill = fill,
                 group = .data[[input$x]]))+
        geom_boxplot()+
        theme_gray(base_size = 15)+
        theme(axis.text.x = element_text(angle = 25, face = 'bold'),
              plot.title = element_text(hjust = 0.5))+
        scale_fill_continuous(low = 'green', high = 'black',
                              limits = c(0,100))+
        scale_x_reordered()

    }


  })
  
  # Rendering reactive plot to ui
  output$plot = renderPlot({
    
    print(plotInput())
    
  })
  
  ## This creates a button to downnload the current plot
  output$save_plot <- downloadHandler(
    filename = function() { paste(input$Plot_name, '.png', sep='_') },
    content = function(file) {
      save_plot(file, plotInput(), base_height = input$Save_width)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
