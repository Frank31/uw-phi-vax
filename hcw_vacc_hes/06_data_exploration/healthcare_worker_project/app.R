#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(readxl)
library(tidyverse)

# load the necessary data
data <- readRDS(file=paste0("./data_folder/brfss_estimated_coverage_all_years.RDS")) %>%
  filter(factor=="factor(FLUSHOT7)1")

# load the state FIPS values to label data properly
fips_codebook <- read_excel(path = "./data_folder/state_fips_code.xlsx")

# add STATE_FIPS_CODE variable to use in visuals
data$STATE_FIPS_CODE <- as.numeric(data$STATE)

# add STATE_NAME label to use in visuals
data <- data %>% full_join(fips_codebook, by = "STATE_FIPS_CODE")

# any necessary data transformations
# data$YEAR <- as.numeric(data$YEAR)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualizing State Vaccination Levels"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
          plotlyOutput("linePlot")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    output$linePlot <- renderPlotly(
      
      # plot the trend line
      plot_ly(data=data, x = ~YEAR, color=~STATE_NAME) %>%
      add_lines(y = ~mean, mode = 'lines', connectgaps = FALSE) %>%
        layout(title='Flu Vaccine')
      # plot_ly(data = data %>% filter(STATE=="01"), x=~YEAR, y = ~mean, color = ~STATE) %>%
      #   add_lines()
      
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
