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
count_data <- fread(file.path("www", "FINAL_NCVS_Dataset_Model_Counts.csv"))
count_data$Time <- as.Date(paste0(count_data$Year, "-", count_data$Month, "-1"))

# Anomalies 
ncvs_anomalies <- fread(file.path("www", "ANOMALIES_NCVS_Dataset.csv"))

# Correlation
ncvs_correlation <- fread(file.path("www", "CORRELATIONS_NCVS_Dataset.csv"))

## Murder ##

# Counts
murder_data_count <- fread(file.path("www", "FINAL_Murder_Dataset_Model_Counts.csv"))
MonthConversion <- c("January", "February", "March", "April", "May", "June",
                     "July", "August", "September", "October", "November", "December")
murder_data_count$Month <- lapply(murder_data_count$Month, function(x) {match(x, MonthConversion)}) %>% unlist()
murder_data_count$Time <- as.Date(paste0(murder_data_count$Year, "-", murder_data_count$Month, "-1"))

# Anomalies
murder_anomalies <- fread(file.path("www", "ANOMALIES_Murder_Dataset.csv"))

# Correlation
murder_correlation <- fread(file.path("www", "CORRELATIONS_Murder_Dataset.csv"))

########
## UI ##
########

ui <- fluidPage(
  
  # Create two panels - one for each dataset
  tabsetPanel(
    tabPanel("Crime Victimization Survey", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h2("Crime Victim Profiles through Time"),
                            uiOutput("y_var_render"),
                            DTOutput("ncvs_correlations")),
               mainPanel(
                 plotlyOutput(outputId = "plot", height = "900px"),
               )
             )
    ),
    
    tabPanel("Murder Dataset", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h2("Murder Victim Profiles through Time"),
                            uiOutput("murder_y_var_render"),
                            DTOutput("murder_correlations")),
               mainPanel(plotlyOutput(outputId = "plot_two", height = "900px"))
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
                choices = colnames(count_data)[colnames(count_data) %in% c("Year", "Month", "Region", "Time") == FALSE],
                options = list(`live-search` = TRUE),
                multiple = FALSE)
      
  })
  
  # Render y variable selecter for murder
  output$murder_y_var_render <- renderUI({
    
    pickerInput(inputId = "var_y", label = "Select y-axis variable",
                selected = "Total",
                choices = colnames(murder_data_count)[colnames(murder_data_count) %in% c("Year", "Month", "Region", "Time") == FALSE],
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
  
  # NCVS correlations table
  output$ncvs_correlations <- renderDT({
    
    if (is.null(input$y_var)) {return(NULL)}
    
    ncvs <- ncvs_correlation %>%
      dplyr::filter(Variable == input$y_var) %>%
      dplyr::select(-Variable) %>% 
      t()
    colnames(ncvs) <- "Spearman Correlation"
    ncvs <- round(ncvs, 6)
    
    DT::datatable(ncvs, options = list(dom = 't'), filter = "none")
    
  })
  
  # Render plot for murder
  output$plot_two <- renderPlotly({
    
    if (is.null(input$var_y)) {return(NULL)}
    
    total_plot <- ggplotly({
      
      ggplot(murder_data_count, aes(x = Time, y = .data[[input$var_y]], color = Region)) + 
        geom_point(alpha = 0.5) + 
        geom_line() +
        theme(legend.position = "none") + 
        xlab("Month & Year") +
        ylab("Count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) 
      
    })
    
    midwest_data <- murder_anomalies %>%
      dplyr::filter(Region == "Midwest" & Variable == input$var_y) %>%
      dplyr::mutate(Time = as.Date(Time)) 
    
    midwest_plot <- (ggplot(midwest_data, aes(x = Time, y = Count)) + 
                       geom_point(aes(color = Anomaly), size = 2) + 
                       scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
                       theme_bw()) %>% ggplotly()
    
    northeast_data <- murder_anomalies %>%
      dplyr::filter(Region == "Northeast" & Variable == input$var_y) %>%
      dplyr::mutate(Time = as.Date(Time)) 
    
    northeast_plot <- (ggplot(northeast_data, aes(x = Time, y = Count)) + 
                         geom_point(aes(color = Anomaly), size = 2) + 
                         scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
                         theme_bw()) %>% ggplotly()
    
    south_data <- murder_anomalies %>%
      dplyr::filter(Region == "South" & Variable == input$var_y) %>%
      dplyr::mutate(Time = as.Date(Time)) 
    
    south_plot <- (ggplot(south_data, aes(x = Time, y = Count)) + 
                     geom_point(aes(color = Anomaly), size = 2) + 
                     scale_color_manual(values = list("Yes" = "red", "No" = "black")) +
                     theme_bw()) %>% ggplotly()
    
    west_data <- murder_anomalies %>%
      dplyr::filter(Region == "West" & Variable == input$var_y) %>%
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
  
  # NCVS correlations table
  output$murder_correlations <- renderDT({
    
    if (is.null(input$var_y)) {return(NULL)}
    
    murder <- murder_correlation %>%
      dplyr::filter(Variable == input$var_y) %>%
      dplyr::select(-Variable) %>% 
      t()
    colnames(murder) <- "Spearman Correlation"
    murder <- round(murder, 6)
    
    DT::datatable(murder, options = list(dom = 't'), filter = "none")
    
  })
  
  
}  


shinyApp(ui = ui, server = server)