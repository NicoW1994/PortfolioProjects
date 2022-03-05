#----------------------------- shiny web app ---------------------------------
# Nico Wagner
# https://github.com/NicoW1994
# 04.03.2022
#------------------------------------------------------------------------------


# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(tidyverse)
library(zoo)
library(DT)


# Read data
data <- readRDS("/Users/nico/Documents/VSC/website/Projekte/Pandemic/covid_data_monthly.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
#                headerPanel('Pandemic Dataset'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("country", label = "Country:", 
                              choices = list("Germany", "France", "Russia" ,"Iran")),
                  selectInput("data", "Data:",
                              choices = list("Cases" = "cases", "Deaths" = "deaths", "Vaccinations" = "vaccinations"),
                              selected = "Cases"),
                  selectInput("plottype", "Plot:",
                              choices = list("Lines" = "lines", "Histogramm" = "histo"),
                              selected = "Lines"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary"),
                  
                  width = 3
                ),
                
                mainPanel(
                  # tags$label(h3('Status/Output')), # Status/Output Text Box
                  # verbatimTextOutput('contents'),
                  plotOutput(outputId = "plot"),
                  dataTableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  dataInput <- reactive({  
                 data %>% 
                    filter(location == input$country) %>% 
                      arrange(date_year_month) %>% 
                       mutate(dummy = seq(1:length(date_year_month))) %>% 
                          select(location, x = date_year_month, y = input$data)
  })
    
  summarytbl <- reactive({ 
                  data %>%
                    filter(location == input$country) %>%
                      arrange(date_year_month) %>%
                        select(Country = location, Date = year_month, cases, deaths, vaccinations)
    
  })
  
  v <- reactiveValues(plot1 = NULL)

  observeEvent(c(input$country, input$data, input$plottype), {
    
                if(input$plottype == "lines"){
    
                v$plot1 <- ggplot(dataInput(), aes(x,y)) +
                  geom_line(color = "dodgerblue3", size =1.5)
                
                } else {
    
                v$plot1 <- ggplot(dataInput(), aes(x,y)) +
                  geom_bar(stat="identity", color ="gold3")
                  }
  })
  
  # Plot result
  output$plot <- renderPlot({
    if (input$submitbutton>0) {
      isolate(v$plot1)
    }
    
  })
  
  # Print output table
   output$tabledata <- renderDataTable({
     if (input$submitbutton>0) {
       isolate(summarytbl())
     }

   })


}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)