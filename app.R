library(shiny)
library(shinyWidgets)
library(tibble)
library(plotly)
library(ggplot2)
library(DT)
library(data.table)

######################
## Pre-Process Data ##
######################

## NCVS ##

# Counts 
count_data <- fread(file.path("www", "Results", "FINAL_NCVS_Dataset_Model_Counts.csv"))
count_data$Time <- as.Date(paste0(count_data$Year, "-", count_data$Month, "-1"))

## Murder ##

# Counts
murder_data_count <- fread(file.path("www", "Results", "FINAL_Murder_Dataset_Model_Counts.csv"))
MonthConversion <- c("January", "February", "March", "April", "May", "June",
                     "July", "August", "September", "October", "November", "December")
murder_data_count$Month <- lapply(murder_data_count$Month, function(x) {match(x, MonthConversion)}) %>% unlist()
murder_data_count$Time <- as.Date(paste0(murder_data_count$Year, "-", murder_data_count$Month, "-1"))


########
## UI ##
########

ui <- fluidPage(
  
  # Create two panels - one for each dataset
  tabsetPanel(
    tabPanel("Crime Victimization Survey", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h2("Crime Victim Profiles through Time"),
                            uiOutput("y_var_render")),
               mainPanel(plotlyOutput(outputId = "plot"))
             )
    ),
    
    tabPanel("Murder Dataset", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h2("Murder Victim Profiles through Time"),
                            uiOutput("murder_y_var_render")),
               mainPanel(plotlyOutput(outputId = "plot_two"))
             ))
  )
)

############
## SERVER ##
############

server <- function(input, output) {
  
  # Render y variable selecter for crime 
  output$y_var_render <- renderUI({
      
    pickerInput(inputId = "y_var", label = "Select y-axis variable",
                selected = "Total Crime",
                choices = colnames(count_data)[4:ncol(count_data)],
                options = list(`live-search` = TRUE),
                multiple = FALSE)
      
  })
  
  # Render y variable selecter for murder
  output$murder_y_var_render <- renderUI({
    
    pickerInput(inputId = "var_y", label = "Select y-axis variable",
                selected = "Total",
                choices = colnames(murder_data_count)[4:ncol(murder_data_count)],
                options = list(`live-search` = TRUE),
                multiple = FALSE)
    
  })
  
  # Render plot for crime
  output$plot <- renderPlotly({
    
    if (is.null(input$y_var)) {return(NULL)}
          
    ggplotly({
      
      ggplot(count_data, aes(x = Time, y = .data[[input$y_var]], color = Region)) + 
        geom_point(alpha = 0.5) + 
        geom_line() +
        theme(legend.position = "none") + 
        xlab("Month & Year") +
        ylab("Count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) +
        ggtitle(input$y_var)
      
    })
  
  })
  
  # Render plot for murder
  output$plot_two <- renderPlotly({
    
    if (is.null(input$var_y)) {return(NULL)}
    
    (ggplot(murder_data_count, aes(x = Time, y = .data[[input$var_y]], color = Region)) +
        geom_point(alpha = 0.5) +
        geom_line() +
        theme(legend.position = "none") +
        xlab("Month & Year") +
        ylab("Count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) +
        ggtitle(input$var_y)) %>%
      ggplotly()
      
  })
  
  
  #output$table <- renderDT({
  #  
  #  if (is.null(input$data_format)) {return(NULL)}
  #  
  #  if (input$data_format == "Count") {
  #    data <- data.frame(
  #      Variable = colnames(count_data)[4:length(colnames(count_data))],
  #      Importance = NA
  #    )
  #  } else if (input$data_format == "Proportion") {
  #    data <- data.frame(
  #      Variable = colnames(proportion_data)[5:length(colnames(proportion_data))],
  #      Importance = NA
  #    )
  #  }
  #
  #  datatable(data, options = list(scrollX = TRUE), select = "single")
  #  
  #})
  
}  


shinyApp(ui = ui, server = server)