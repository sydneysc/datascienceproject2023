library(tibble)
library(plotly)
library(ggplot2)
library(DT)
library(stringr)

crime_data <- read.csv("/Users/schw939/Library/CloudStorage/OneDrive-PNNL/DataScienceProject2023/Final_DSP_Dataset_v2.csv")
crime_data
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h2("Crime Through Time"),
      selectInput(inputId = "y_var", label = "Select variable",
                  choices = colnames(crime_data[4:length(colnames(crime_data))]),
                  multiple = FALSE),
     selectInput(inputId = "x_var", label = "Select y-axis",
                 choices = c("Count","Proportion"),
                 multiple = FALSE,
                 selected = "Count")

    ),
    mainPanel(
      plotlyOutput(outputId = "plot"),
      em("This is a plot that shows the different values in the crime dataset"),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {

  output$plot <- renderPlotly({
    ggplotly({
      plot_df <- tibble(crime_data) %>%
        mutate(mo_yr = as.Date(str_c(Year, Month, "01", sep = "-"))) %>%
        select(mo_yr, everything())
      
      p <- ggplot(plot_df,
                  aes_string(x = "mo_yr", y = "TotalCrime", color = "Region")) + 
              geom_point(alpha = 0.5) + 
              geom_line() +
              theme(legend.position = "none") + 
              ylab("") +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45))
              #scale_x_date(labels = date_format("%m-%Y"))
      
      p
    })
  })
  
  output$table <- renderDT({
    datatable(crime_data, options = list(scrollX = TRUE))
  })
  
    }

shinyApp(ui = ui, server = server)
