library(shiny)
library(shinyWidgets)
library(tibble)
library(plotly)
library(ggplot2)
library(DT)
library(data.table)
library(stringr)
library(here)

count_data <- fread(file.path("www", "FINAL_NCVS_Dataset_Model_Counts.csv"))
proportion_data <- fread(file.path("www", "FINAL_NCVS_Dataset_Model_Proportions.csv"))
murder_data_count <- fread(file.path("www", "FINAL_Murder_Dataset_Model_Counts.csv"))
murder_data_prop <- fread(file.path("www", "FINAL_Murder_Dataset_Model_Proportions.csv"))


ui <- fluidPage(
  #"new code" that is trying to create two separate tabs populated with two separate sidebars and plots for different datasets
  tabsetPanel(
    tabPanel("Crime Victimization Survey", fluid = TRUE,
             sidebarLayout(
               sidebarPanel( h2("Crime Victim Profiles through Time"),
                             uiOutput("y_var_render"),
                             selectInput(inputId = "data_format", label = "Select data format",
                                         choices = list("Log Transformed Proportions" = "Proportion", "Count" = "Count"),
                                         multiple = FALSE)),
               mainPanel(plotlyOutput(outputId = "plot"))
             )
    ),
    
    tabPanel("Murder Dataset", fluid = TRUE,
             sidebarLayout(
               sidebarPanel( h2("Murder Victim Profiles through Time"),
                             uiOutput("murder_y_var_render"),
                             selectInput(inputId = "murder_data_format", label = "Select data format",
                                         choices = list("Log Transformed Proportions" = "Proportion", "Count" = "Count"),
                                         multiple = FALSE)),
               mainPanel(plotlyOutput(outputId = "plot_two"))
             ))
  )
)


server <- function(input, output) {
  
  
  output$y_var_render <- renderUI({
    
    if (is.null(input$data_format)) {return(NULL)}
    
    if (input$data_format == "Count") {
      
      pickerInput(inputId = "y_var", label = "Select y-axis variable",
                  selected = "TotalCrime",
                  choices = colnames(count_data)[4:ncol(count_data)],
                  options = list(`live-search` = TRUE),
                  multiple = FALSE)
      
    } else if (input$data_format == "Proportion") { 
      
      pickerInput(inputId = "y_var", label = "Select y-axis variable",
                  selected = "TotalCrime",
                  choices = colnames(proportion_data)[4:ncol(proportion_data)],
                  options = list(`live-search` = TRUE),
                  multiple = FALSE)
      
    }
    
    
  })
  
  output$murder_y_var_render <- renderUI({
    
    if (is.null(input$murder_data_format)) {return(NULL)}
    
    if (input$murder_data_format == "Count") {
    
    pickerInput(inputId = "var_y", label = "Select y-axis variable",
                selected = "referenceSex_male",
                choices = colnames(murder_data_count)[5:ncol(murder_data_count)],
                options = list(`live-search` = TRUE),
                multiple = FALSE)
      
    } else if (input$murder_data_format == "Proportion") { 
      
    pickerInput(inputId = "var_y", label = "Select y-axis variable",
                  selected = "referenceSex_male",
                  choices = colnames(murder_data_prop)[5:ncol(murder_data_prop)],
                  options = list(`live-search` = TRUE),
                  multiple = FALSE)
      
    }
    
    
  })
  
  
  output$plot <- renderPlotly({
    
    if (is.null(input$data_format)) {return(NULL)}
    if (is.null(input$y_var)) {return(NULL)}
    
    if (input$data_format == "Count") {
      
      plot_df <- tibble(count_data) %>%
        mutate(mo_yr = as.Date(str_c(Year, Month, "01", sep = "-"))) %>%
        select(mo_yr, everything())
      
      (ggplot(plot_df, aes(x = mo_yr, y = .data[[input$y_var]], color = Region)) + 
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
        
        p <- ggplot(plot_df, aes(x = mo_yr, y = .data[[input$y_var]], color = Region)) + 
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
  #trying to make this plot be for the murder dataset, not showing up
  output$plot_two <- renderPlotly({
    
    if (is.null(input$murder_data_format)) {return(NULL)}
    if (is.null(input$var_y)) {return(NULL)}
    
    if (input$murder_data_format == "Count") {
      
    
    #if (is.null(input$var_y)) {return(NULL)}
    
    MonthConversion <- c("January", "February", "March", "April", "May", "June",
                         "July", "August", "September", "October", "November", "December")
    
    plot_df_murder <-  murder_data_count %>%
      mutate(Month = match(Month, MonthConversion)) %>%
      mutate(mo_yr = as.Date(str_c(Year, Month, "01", sep = "-"))) %>%
      select(mo_yr, everything())
    
    (ggplot(plot_df_murder, aes(x = mo_yr, y = .data[[input$var_y]], color = Region)) +
        geom_point(alpha = 0.5) +
        geom_line() +
        theme(legend.position = "none") +
        xlab("Month & Year") +
        ylab("Count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45))) %>%
      ggplotly()
    
#  })
  
  } else if (input$murder_data_format == "Proportion") {
    
    ggplotly({
      
      MonthConversion <- c("January", "February", "March", "April", "May", "June",
                           "July", "August", "September", "October", "November", "December")
      
      plot_df_murder <- murder_data_prop %>%
        mutate(Month = match(Month, MonthConversion)) %>%
        mutate(mo_yr = as.Date(str_c(Year, Month, "01", sep = "-"))) %>%
        select(mo_yr, everything())
      
      p <- ggplot(plot_df_murder,
                  aes(x = mo_yr, y = .data[[input$var_y]], color = Region)) + 
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