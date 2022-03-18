#----------------------------- Airbnb web app --------------------------------
# Nico Wagner
# https://github.com/NicoW1994
# 10.03.2022
#------------------------------------------------------------------------------
# load libraries
#library(shiny)
library(shiny.semantic)
library(shiny.fluent)
library(semantic.dashboard)
library(plotly)

library(leaflet)
library(maps)
library(sp)
library(sf)

library(dplyr)
library(lubridate)
library(ggplot2)
library(devtools)
# library(stringr)
# library(stringi)
# library(tools)
# library(mgsub)

airbnb_listings <- readRDS("./airbnb_listings.rds")

airbnb_calendar <- readRDS("./airbnb_calendar.rds")

# function for ggplot
neutral <- theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              strip.background = element_rect(colour="grey85"),
                              panel.border = element_rect(colour = "grey85")) 

#------------------------------------------------------------------------------
# defining options fpr dropdown 1
options_beds <- list(
    list(key = "1", text = "1"),
    list(key = "2", text = "2"),
    list(key = "3", text = "3")
)

# options_loc <- list(
#   list(key = "center", text = "Center"),
#   list(key = "outside", text = "Outside")
# )

#------------------------------------------------------------------------------
ui <- semanticPage(
    titlePanel("Find An Airbnb In Munich!"),
    sidebar_layout(
        sidebar_panel(width = 1,
                      
                      Text("Number of People:"),
                      Dropdown.shinyInput("dropdown1", value = "1", options = options_beds),
                      
                      Text("Pricing Min. :"),
                      Slider.shinyInput("slider1", value = 100, min = 15, max = 1000, showValue=F),
                      textOutput("sliderValueMin"),
                      
                      Text("Pricing Max. :"),
                      Slider.shinyInput("slider2", value = 150, min = 15, max = 1000, showValue=F),
                      textOutput("sliderValueMax"),
                      
                      Toggle.shinyInput("toggle2", label = "City Center"),
                      
                      Toggle.shinyInput("toggle1", label = "Only show Superhosts"),
                      
                      button("submitbutton", "Submit", class = "btn btn-primary")
        ),
        main_panel(
            leafletOutput('mymap'),
            split_layout(cell_widths = 300,
                         cell_args = "padding: 6px;",
                         style = "border: 1px solid silver;",
                         plotOutput("plot1"),
                         semantic_DTOutput('tabledata')
            )),
        mirrored = FALSE
    )
)

server <- function(input, output, session){
    
    # render slider values
    output$sliderValueMin <- renderText({
        sprintf("%s $", input$slider1)
    })
    
    output$sliderValueMax <- renderText({
        sprintf("%s $", input$slider2)
    })
    
    # create reactive elements and filter for data according to input
    dataInput <- reactive({
        if(input$toggle1 == TRUE & input$toggle2 == TRUE){
            
            airbnb_listings %>%
                filter(host_is_superhost == "t", location == "center", price >= input$slider1 & price <= input$slider2 &
                           accommodates == input$dropdown1)
            
        } else if(input$toggle1 == TRUE) {
            
            airbnb_listings %>%
                filter(host_is_superhost == "t", price >= input$slider1 & price <= input$slider2 &
                           accommodates == input$dropdown1)
            
        } else if(input$toggle2 == TRUE) {
            
            airbnb_listings %>%
                filter(location == "center", price >= input$slider1 & price <= input$slider2 &
                           accommodates == input$dropdown1)
            
        } else {
            
            airbnb_listings %>%
                filter(price >= input$slider1 & price <= input$slider2 &
                           accommodates == input$dropdown1)
        }
        
    })
    
    # empty reactive container for map updates
    # define colors for superhost
    getColor <- function(x){
        if_else(x$host_is_superhost == "f" | is.na(x$host_is_superhost), "blue", "orange")
    }
    
    # reactive Values for map and output table
    rv <- reactiveValues(map1 = NULL, table1 = NULL, plot1 = NULL, click = NULL)
    
    observeEvent(c(input$slider1, input$slider2, input$dropdown1, input$toggle1, input$toggle2, 
                   input$map_marker_click), {
                       
                       # create icons fo map
                       icons <- awesomeIcons(
                           icon = 'ios-close',
                           iconColor = 'black',
                           library = 'ion',
                           markerColor = getColor(dataInput())
                       )
                       
                       # create map
                       rv$map1 <- leaflet(data = dataInput()) %>%
                           addTiles() %>%
                           #                  setView(lat = 48.137154, lng = 11.576124, zoom = 12) %>%
                           #                    clearMarkers() %>%   ## clear previous markers
                           addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(paste0(price," $")),
                                             popup = dataInput()$name)
                       
                       # create table"
                       rv$table1 <- dataInput() %>%
                           mutate(host_is_superhost = if_else(host_is_superhost == "t", "Yes", "")) %>%
                           arrange(desc(price)) %>%
                           select("Name" = name, "Price ($)" = price , "Superhost" = host_is_superhost, "Nr. of People" = accommodates,
                                  "Beds" = beds,"Min. Nights" = minimum_nights, "Rating" = review_scores_rating)
                       
                       # create dataset for calendar data and plot
                       
                       rv$click <- input$map_marker_click
                       
                       rv$plot1 <- airbnb_calendar %>%
                           filter(listing_id  %in% dataInput()$id) %>% 
                           ggplot(mapping = aes(x=month, y=price_mean))+ neutral +
#                           geom_line(aes(group = listing_id))+
                           geom_boxplot()+
                           # geom_text(aes(label=name), vjust = -0.5)+
#                           scale_color_gradient2(low = "dodgerblue3", mid = "gold3", high = "firebrick") +
#                           geom_point()+
                           labs(x = "\nMonth", y = "Price", title = "Average seasonal prices for 1 night")+
                           theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                 axis.text=element_text(size=10, face ="bold"),
                                 axis.title=element_text(size=15))
                       
                   }
    )
    #--------------------------
    # generate outputs
    
    # update map
    output$mymap <- renderLeaflet({
        if (input$submitbutton>0) {
            isolate(rv$map1)
        }
    })
    
    # Print output table
    output$tabledata <- DT::renderDataTable(
        if (input$submitbutton>0) {
            isolate(semantic_DT(rv$table1))
        })
    
    # output$temp <- renderPrint({
    #   rv$click$long
    # })
    
    
    
    # Print output plot
    output$plot1 <- renderPlot(width = 450, height = 450,
        if (input$submitbutton>0) {
            isolate(rv$plot1)
        })
    
}

shinyApp(ui, server)
