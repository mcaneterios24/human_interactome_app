library(shiny)
library(ggplot2)
library(dplyr)
library(geomnet)
library(DT)

ui <- fluidPage(
  titlePanel(
    h1("Homo Sapiens interactomic data")),
  sidebarLayout(
    sidebarPanel(
      h4("Please follow the instructions"),
      fileInput("file", 'Select the XXX.csv file',
                accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      selectInput("Protein", "Select one protein", c("9606.ENSP00000001008", "9606.ENSP00000004531", "CPEB3", "CPEB4")),
      sliderInput("interaction_score", "Choose interaction score", min = 0, max = 1, value = 0.6)),
    mainPanel(
      h3("Circular interaction plot"),
      plotOutput("Circular_plot")
    )
  ) 
)

server <- function(input, output) {
  output$Circular_plot <- renderPlot({
    
    cut_off <- input$interaction_score
    protein <- input$Protein 
    
    data_input <- all_data %>%
      filter(combined_score >= cut_off) %>%
      filter(protein1 == protein)
    
    unique <- unique(data_input$protein2)
    self_int <- data.frame(protein1 = unique, protein2 = unique, combined_score = 0.999)
    data_input_2 <- rbind(data_input, self_int) %>%
      unique()
    
    ggplot(data = data_input_2, aes(from_id = protein1, to_id = protein2)) +
      geom_net(layout.alg = "circle", labelon = F, 
               size = 12, directed = F, vjust = 0.5, labelcolour = "black", linewidth = 0.35,
               selfloops = F, ecolour = "grey10", ealpha = 0.5, show.legend = F) + 
      theme_net(base_size = 1)
  })
}

shinyApp(ui, server)

