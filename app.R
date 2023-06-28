library(shiny)
library(shinyWidgets)
library(tibble)
library(plotly)
library(ggplot2)
library(DT)
library(stringr)
library(here)

count_data <- read.csv(file.path("www", "Final_DSP_Dataset_v3.csv"))
proportion_data <- read.csv(file.path("www", "DSP_Data_Proportions_by_Pop.csv"))

ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h2("Crime Through Time"),
      uiOutput("y_var_render"),
     selectInput(inputId = "data_format", label = "Select data format",
                 choices = list("Log Transformed Proportions" = "Proportion", "Count" = "Count"),
                 multiple = FALSE)

    ),
    mainPanel(
      plotlyOutput(outputId = "plot"),
      em("This is a plot that shows the different values in the crime dataset"),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {
  
  
  output$y_var_render <- renderUI({
    
    if (is.null(input$data_format)) {return(NULL)}
    
    if (input$data_format == "Count") {
      
      pickerInput(inputId = "y_var", label = "Select y-axis variable",
                  selected = "TotalCrime",
                  choices = colnames(count_data[4:length(colnames(count_data))]),
                  options = list(`live-search` = TRUE),
                  multiple = FALSE)
      
    } else if (input$data_format == "Proportion") { 
      
      pickerInput(inputId = "y_var", label = "Select y-axis variable",
                  selected = "TotalCrime",
                  choices = colnames(proportion_data[5:length(colnames(proportion_data))]),
                  options = list(`live-search` = TRUE),
                  multiple = FALSE)
      
    }

    
  })

  output$plot <- renderPlotly({
    
    if (is.null(input$data_format)) {return(NULL)}
    
    if (input$data_format == "Count") {
      
      plot_df <- tibble(count_data) %>%
        mutate(mo_yr = as.Date(str_c(Year, Month, "01", sep = "-"))) %>%
        select(mo_yr, everything())
      
      (ggplot(plot_df,
                  aes_string(x = "mo_yr", y = input$y_var, color = "Region")) + 
        geom_point(alpha = 0.5) + 
        geom_line() +
        theme(legend.position = "none") + 
        xlab("Month & Year") +
        ylab("Count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45))) %>%
        ggplotly()
      
    } else if (input$data_format == "Proportion") {
      
      ggplotly({
        
        plot_df <- tibble(proportion_data) %>%
          mutate(mo_yr = as.Date(str_c(Year, Month, "01", sep = "-"))) %>%
          select(mo_yr, everything())
        
        p <- ggplot(plot_df,
                    aes_string(x = "mo_yr", y = input$y_var, color = "Region")) + 
          geom_point(alpha = 0.5) + 
          geom_line() +
          theme(legend.position = "none") + 
          xlab("Month & Year") +
          ylab("Log Transformed Proportion") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45))
        #scale_x_date(labels = date_format("%m-%Y"))
        
        p
      
      })
      
    }
    

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
