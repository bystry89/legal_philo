#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
source("../src/functions/labelClust.R")
nodes <- read_csv("../data/cits/nodes.csv") %>% labelClusts() %>% 
  mutate(cluster_label = if_else(is.na(cluster_label), "Uninterpreted", cluster_label))
works <- read.csv("../data/raw/works_full.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("communities","Choose campaign(s):",unique(nodes$cluster_label)),
          actionLink("selectall","Select All") 
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"communities","Choose campaign(s):",choices=unique(nodes$cluster_label))
    }
    else
    {
      updateCheckboxGroupInput(session,"communities","Choose campaign(s):",choices=unique(nodes$cluster_label),selected=unique(nodes$cluster_label))
    }
  })
    output$table <- renderDataTable({
        # generate bins based on input$bins from ui.R
        nodes %>% left_join(works) %>% select(first_author, display_name, publication_year, eigen, betweenness, cluster_label) %>% 
        filter(cluster_label %in% input$communities)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
