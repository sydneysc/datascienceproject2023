library(shiny)
library(shinyWidgets)
library(tibble)
library(plotly)
library(ggplot2)
library(DT)
library(data.table)
library(dplyr)

######################
## Pre-Process Data ##
######################

## NCVS ##

# Counts 
count_data <- fread(file.path("www", "Results", "FINAL_NCVS_Dataset_Model_Counts.csv"))
count_data$Time <- as.Date(paste0(count_data$Year, "-", count_data$Month, "-1"))

# Anomalies 
ncvs_anomalies <- fread(file.path("www", "Results", "Anomalies_NCVS_Dataset.csv"))

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
               mainPanel(
                 plotlyOutput(outputId = "plot", height = "900px"),
               )
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
          
    total_plot <- ggplotly({
      
      ggplot(count_data, aes(x = Time, y = .data[[input$y_var]], color = Region)) + 
        geom_point(alpha = 0.5) + 
        geom_line() +
        theme(legend.position = "none") + 
        xlab("Month & Year") +
        ylab("Count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) 
      
    })
      
    midwest_data <- ncvs_anomalies %>%
      dplyr::filter(Region == "Midwest" & Variable == input$y_var) %>%
      dplyr::mutate(Time = as.Date(Time)) 
      
    midwest_plot <- (ggplot(midwest_data, aes(x = Time, y = Count)) + 
      geom_point(aes(color = Anomaly), size = 2) + 
      scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
      theme_bw()) %>% ggplotly()

    northeast_data <- ncvs_anomalies %>%
      dplyr::filter(Region == "Northeast" & Variable == input$y_var) %>%
      dplyr::mutate(Time = as.Date(Time)) 
    
    northeast_plot <- (ggplot(northeast_data, aes(x = Time, y = Count)) + 
        geom_point(aes(color = Anomaly), size = 2) + 
        scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
        theme_bw()) %>% ggplotly()
    
    south_data <- ncvs_anomalies %>%
      dplyr::filter(Region == "South" & Variable == input$y_var) %>%
      dplyr::mutate(Time = as.Date(Time)) 
    
    south_plot <- (ggplot(south_data, aes(x = Time, y = Count)) + 
        geom_point(aes(color = Anomaly), size = 2) + 
        scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
        theme_bw()) %>% ggplotly()
    
    west_data <- ncvs_anomalies %>%
      dplyr::filter(Region == "West" & Variable == input$y_var) %>%
      dplyr::mutate(Time = as.Date(Time)) 
    
    west_plot <- (ggplot(west_data, aes(x = Time, y = Count)) + 
                         geom_point(aes(color = Anomaly), size = 2) + 
                         scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
                         theme_bw()) %>% ggplotly()
    
    
    subplot(
      total_plot, midwest_plot, northeast_plot, south_plot, west_plot, 
      nrows = 5, shareX = TRUE, titleY = TRUE, titleX = TRUE
    ) %>%
      layout(
        title = list(text = ""),
        legend = list(title = list(text = "Legend")),
        annotations = list(
          list(
            x = 0.1, xref = "paper", xanchor = "center",  
            y = 1, yref = "paper", yanchor = "bottom",  
            text = "Counts per Region", showarrow = FALSE
          ),
          list(
            x = 0.1, xref = "paper", xanchor = "center",
            y = 0.78, yref = "paper", yanchor = "bottom",
            text = "Midwest Anomalies", showarrow = FALSE
          ),
          list(
            x = 0.1, xref = "paper", xanchor = "center",
            y = 0.58, yref = "paper", yanchor = "bottom",
            text = "Northeast Anomalies", showarrow = FALSE
          ),
          list(
            x = 0.1, xref = "paper", xanchor = "center",
            y = 0.38, yref = "paper", yanchor = "bottom",
            text = "South Anomalies", showarrow = FALSE
          ),
          list(
            x = 0.1, xref = "paper", xanchor = "center",
            y = 0.18, yref = "paper", yanchor = "bottom",
            text = "West Anomalies", showarrow = FALSE
          )
       )
      )
    
    
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