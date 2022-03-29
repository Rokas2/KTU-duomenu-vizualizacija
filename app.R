library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Odontologijos praktikos veikla"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("test", "Kompanijos kodas")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

getwd()
# Define server logic required to draw a histogram
server <- function(input, output) {
  data = read_csv("lab_sodra.csv")
  data = data %>%
    filter(ecoActCode == 862300)
  data$month = as.Date(paste0(as.character(data$month), '01'), format = '%Y%m%d')
  
  output$distPlot <- renderPlot({
    data %>%
      filter(code ==as.numeric((input$test))) %>%
      ggplot(aes(x = month, y = avgWage)) +
      xlab("Month") +
      ylab("Average Wage") +
      geom_line(color = "red") +
      scale_x_date(date_breaks = "months", date_labels = "%b")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)