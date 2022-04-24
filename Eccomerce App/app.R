#----------------------------- Ecommerce web app --------------------------------
# Nico Wagner
# https://github.com/NicoW1994
# 10.03.2022
#------------------------------------------------------------------------------
# load libraries
#library(shiny)
library(shiny.semantic)
library(shiny.fluent)
library(plotly)

library(ggplot2)
library(dplyr)
library(tidyr)

library(devtools)
library(scales)
library(zoo)



# define Layout Grids
myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("social", "logo1","logo2"),
      c("info" , "plot1","plot2"),
      c("info" ,"plot3","plot4"),
      c("holder" ,"table1", "report")
    ),
    cols_width = c("20%", "40%","40%"),
    rows_height = c("45px", "350px", "350px", "350px")
  )#,
  # mobile = list(
  #   areas = rbind(
  #     "title",
  #     "info",
  #     "map",
  #     "plot",
  #     "table",
  #     "image",
  #     "logo",
  #     "logo2"
  #   ),
  #   rows_height = c("25px", "auto", "auto", "auto", "auto", "auto", "auto", "auto"),
  #   cols_width = c("100%")
  # )
 )

subGrid <- grid_template(default = list(
  areas = rbind(
    c("left", "mid", "right")
  ),
  cols_width = c("33%","33%","33%")
))

subGrid2 <- grid_template(default = list(
  areas = rbind(
    c("top"), 
    c("mid"), 
    c("mid2"),
    c("bottom")
  ),
  rows_height = c("25%"),
                c("25%"),
                c("25%"),
                c("25%")
))

#------------------
# load data
selling_data <- readRDS("./data.rds")

#------------------
# Functions
neutral <- theme_bw() + theme(panel.grid.major = element_blank(),
                              # panel.grid.minor = element_blank(),
                              strip.background = element_rect(colour="grey85"),
                              panel.border = element_rect(colour = "grey85")) 

colfunc <- colorRampPalette(c("dodgerblue3", "gold", "firebrick"))

#------------------------------------------------------------------------------

# defining options for dropdowns
options_cat <- arrange(data.frame("category" = unique(selling_data$category_code)), category) %>% 
                 drop_na() %>% 
                  add_row(category = "all", .before = 1)
options_cat <- as.vector(options_cat$category)

#--
options_brand <- arrange(data.frame("brand" = unique(selling_data$brand)), brand)  %>% 
                  drop_na() %>% 
                    add_row(brand = "all", .before = 1)
options_brand <- as.vector(options_brand$brand)

# text for report
t_popular_brand <- "Brand with the most purchases: "
t_best_brand <- "Brand with the most Sales: "
t_best_week <- "Week with the highest Sales: "
t_worst_week <- "Week with the lowest Sales: "

#
#------------------------------------------------------------------------------
ui <- semanticPage(
  
        grid(
          myGridTemplate,
          area_styles = list(info = "background: #F9F9F9"),
          
            social = grid(subGrid,
                         
                        left = div(
                          tags$a(imageOutput("image2"), href = "https://github.com/NicoW1994/")),

                        mid = div(
                          tags$a(imageOutput("image3"), href = "https://www.linkedin.com/in/nico-wagner-32a146234/")),

                        right = div(
                          tags$a(imageOutput("image4"), href = "https://www.upwork.com/freelancers/~01f38adff9e0b586f3"))
                        ),

            logo1 = div(
                        tags$a(imageOutput("logo"), href = "https://www.airbnb.com")),

            logo2 = div(
                        tags$a(imageOutput("logo2"), href = "https://stadt.muenchen.de/")),
          
            info = div(style = "padding: 20px",
              
              Text("Category:"),
              selectizeInput("dropdown1", label = NULL, choices = options_cat, selected = "electronics.smartphone"),
              
              Text("Brand:"),
              selectizeInput("dropdown2", label = NULL, choices = options_brand, selected = "apple"),
              
              Text("Pricing Min.:"),
              Slider.shinyInput("slider1", value = 10, min = 10, max = 2000, step = 10, 
                                snapToStep = TRUE
                                ),

              Text("Pricing Max.:"),
              Slider.shinyInput("slider2", value = 500, min = 10, max = 2000, step = 10, 
                                snapToStep = TRUE),
 
              DatePicker.shinyInput("fromDate", value = selling_data$ymd[2], label = "From date"),
              DatePicker.shinyInput("toDate", value = selling_data$ymd[length(selling_data$ymd)], label = "To date"),
              
              # Toggle.shinyInput("toggle1", label = "Highlight Holidays"),
              
              splitLayout(cellWidths = c("50%", "50%"),
                          button("submitbutton", "Analyse Data", class = "btn btn-primary")
                          ,
                          button("submitbutton2", "Print Report", class = "btn btn-primary")
                          ),
              
              # tags$a(imageOutput("image1"), href = "https://www.amazon.com/")
              
              ),

          plot1 = div(style = "padding-right: 10px",
            plotOutput("plot1")),
          
          plot2 = div(style = "padding-right: 10px",
                      plotOutput("plot2")),
          
          plot3 = div(style = "padding-top: 35px",
                      plotOutput("plot3")),
          
          plot4 = div(style = "padding-top: 35px",
                      plotOutput("plot4")),
          
          table1 = div(style = "padding: 75px",
            semantic_DTOutput('tabledata')),
          
          holder = div(
            Text("")),
          
          report = grid(subGrid2,
                        
                        top = div(
                          verbatimTextOutput('text1')),
                        
                        mid = div(
                          verbatimTextOutput('text2')),
                        
                        mid2 = div(
                          verbatimTextOutput('text3')),
                        
                        bottom = div(
                          verbatimTextOutput('text4'))
                        
                        
          ),
      
       )
)

server <- function(input, output, session){
  
  #------------------------
  # filter for category and brand input. Also check if "all" values should be selected
  dataInput <- eventReactive(c(input$submitbutton,
                               
                             input$dropdown1, input$dropdown2, 
                             input$slider1, input$slider2, 
                             input$fromDate, input$toDate), {
                                 
    req(c(input$slider1, input$slider2, input$fromDate, input$toDate))

    if(input$dropdown1 == "all" & input$dropdown2 == "all"){
      selling_data %>%        
        filter(price >= input$slider1 & price <= input$slider2 &
                 ymd >= input$fromDate & ymd <= input$toDate) %>% 
          arrange(event_time) 

    } else if (input$dropdown1 == "all"){
      selling_data  %>%
        filter(brand == input$dropdown2,
               price >= input$slider1 & price <= input$slider2 &
                 ymd >= input$fromDate & ymd <= input$toDate) %>% 
          arrange(event_time) 

      } else if (input$dropdown2 == "all"){
        selling_data %>%
          filter(category_code == input$dropdown1,
                 price >= input$slider1 & price <= input$slider2 &
                   ymd >= input$fromDate & ymd <= input$toDate) %>% 
            arrange(event_time) 

          } else {
             selling_data %>% 
              filter(category_code == input$dropdown1 & brand == input$dropdown2,
                     price >= input$slider1 & price <= input$slider2 &
                       ymd >= input$fromDate & ymd <= input$toDate) %>% 
                arrange(event_time) 
       }
  })
  
  dataBrand <- eventReactive(c(input$submitbutton,
                               
                               input$dropdown1,
                               input$slider1, input$slider2,
                               input$fromDate, input$toDate), {

      req(c(input$slider1, input$slider2, input$fromDate, input$toDate))

      selling_data %>%
        filter(category_code == input$dropdown1 &
                 price >= input$slider1 & price <= input$slider2 &
                 ymd >= input$fromDate & ymd <= input$toDate) %>%
          drop_na(brand) %>%

            group_by(ymd, brand) %>%
              mutate(count = 1) %>%
                summarize(sum_count = sum(count),
                          sum_price = sum(price)) %>%
            ungroup()
      })
  #------------------------
  
      summary_tbl <- eventReactive(input$submitbutton, {
        
        dataInput() %>%
          group_by(ymd, brand) %>%
          mutate(count = 1) %>%
          summarize(sum_count = sum(count),
                    sum_price = sum(price)) %>%
          ungroup() %>%
          
          select("Day" = ymd, "Brand" = brand, "Sales ($)" = sum_price , "Nr. of Purchases" = sum_count)
        
        
      })
  
      report <- eventReactive(input$submitbutton, {
        
        dataInput() %>%
          group_by(week, brand) %>%
            mutate(count = 1) %>%
              summarize(sum_count = sum(count),
                        sum_price = sum(price)) %>%
          ungroup() %>%

          select("Week" = week, "Brand" = brand, sum_price , sum_count)
          
        
      })
  
      event_plot1 <- eventReactive(input$submitbutton, {
  
      # 1. plot: number of daily purchases
      coeff <- 200
      countColor <- "black"
      priceColor <- rgb(0.2, 0.6, 0.9, 1)
    
      dataInput() %>%
        group_by(ymd) %>%
           mutate(count = 1) %>%
             summarize(sum_count = sum(count),
                       sum_price = sum(price)) %>%
                ungroup() %>%
    
               ggplot(aes(x=ymd)) + neutral +
                 geom_point(mapping = aes(y = sum_count), size = .5, color = countColor) +
                 geom_point(mapping = aes(y = sum_count), size = .5, color = countColor) +
                 geom_bar(mapping = aes(y = sum_price/coeff), stat = "identity", size =.1,
                                        fill=priceColor, color = "black", alpha = .4) +
                 labs(x = "Date", title = "Daily Purchases") +
    
                     scale_y_continuous(
                       # Features of the first axis
                       name = "\nNr. of purchases",
    
                       # Add a second axis and specify its features
                       sec.axis = sec_axis(~.*coeff, name= "Sales ($)\n")
                       ) +
    
                       #theme_ipsum() +
    
                        theme(
                          axis.title.y = element_text(color = countColor, size=15),
                          axis.title.y.right = element_text(color = priceColor, size=15),
                          axis.text=element_text(size=10, face ="bold")
                        )
      })

      event_plot2 <- eventReactive(input$submitbutton, {
      
                     # 2. plot: number of monthly purchases by price
                     # brandCol <- ifelse(dataBrand()$brand == input$dropdown2, "black", "gray50")

                     # colfunc <- colorRampPalette(c("dodgerblue3", "gold", "firebrick"))
                     # colfunc(10)
                          
                         dataBrand() %>%

                           ggplot(mapping = aes(x= ymd, y= brand)) + neutral +
                            geom_tile(aes(fill= sum_count), colour = "white") +
    
                            scale_fill_continuous(low = "blue", high = "yellow") +
                             labs(y = "", x = "", fill = "Nr. of purchases", title = "Comparison of Brands") +
                             theme(
                                   axis.text=element_text(size=10, face ="bold"),
                                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                   # axis.text.y = element_text(colour = brandCol),
                                   axis.title=element_text(size=15)) +
                           
                           if((max(dataInput()$ymd) - min(dataInput()$ymd)) > 21) {
                             scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%b")
                             
                           } else {
                             scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d-%b")
                           }

     })
      
      event_plot3 <- eventReactive(input$submitbutton, {
        
        # 3. plot:
        
        dataInput() %>%
          mutate(sum_roll = cumsum(price)) %>%
          
          ggplot(mapping = aes(x= event_time, y= sum_roll)) + neutral +
            geom_line() +
          
            labs(y = "Sales ($)", x = "", title = "Accumulated Sales") +
            theme(
              axis.text=element_text(size=10, face ="bold"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.text.y = element_text(colour = "black"),
              axis.title=element_text(size=15))
        
      })
      
      event_plot4 <- eventReactive(input$submitbutton, {
        
        # 4. plot:
        
        dataInput() %>%
          mutate(count = 1) %>% 
          mutate(count_roll = cumsum(count)) %>%
          
          ggplot(mapping = aes(x= event_time, y= count_roll)) + neutral +
          geom_line() +
          
          labs(y = "Nr. of Sales", x = "", title = "Accumulated Nr. of purchases") +
          theme(
            axis.text=element_text(size=10, face ="bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text.y = element_text(colour = "black"),
            axis.title=element_text(size=15))
        
      })
    
    output$plot1 <- renderPlot(
      if (input$submitbutton > 0) {
        isolate(event_plot1())
      })
    
    output$plot2 <- renderPlot(
      if (input$submitbutton > 0) {
        isolate(event_plot2())
      })
    
    output$plot3 <- renderPlot(
      if (input$submitbutton>0) {
        isolate(event_plot3())
      })  
    
    output$plot4 <- renderPlot(
      if (input$submitbutton > 0) {
        isolate(event_plot4())
      })

    output$tabledata <- DT::renderDataTable(options = list(pageLength = 3,
                                                           lengthMenu = c(3,5,10)),
                                            
                                            if (input$submitbutton2 > 0) {
                                              isolate(summary_tbl())
                                              })
    
    # output$text1 <- renderText(if (input$submitbutton > 0) {
    #                                     isolate(paste0(t_popular_brand,
    #                                                    report()[which.max(report()$sum_price),2]))
    # })
    
    output$text2 <- renderText(if (input$submitbutton2 > 0) {
                                        isolate(paste0(t_best_week,
                                                       report()[which.max(report()$sum_price),1],
                                                       " ",
                                                       report()[which.max(report()$sum_price),2]))
      })
    
    # Print Company Logo
    output$image1 <- renderImage({
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path('./images',
                                          paste('image', 1, '.png', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", 1),
           width = "100%",
           heigth = "100%")
      
      }, deleteFile = FALSE)
    
    # Print Tags for social Media
    output$image2 <- renderImage({
      filename <- normalizePath(file.path('./images',
                                          paste('image', 2, '.png', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", 2),
           width = "50%",
           heigth = "50%")
      }, deleteFile = FALSE)
    
    # Print Tags for social Media
    output$image3 <- renderImage({
      filename <- normalizePath(file.path('./images',
                                          paste('image', 3, '.png', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", 3),
           width = "45%",
           heigth = "45%")
    }, deleteFile = FALSE)
    
    # Print Tags for social Media
    output$image4 <- renderImage({
      filename <- normalizePath(file.path('./images',
                                          paste('image', 4, '.png', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", 4),
           width = "45%",
           heigth = "45%")
    }, deleteFile = FALSE)

}

shinyApp(ui, server)
