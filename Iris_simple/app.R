#' A very simple example of the Shiny structure using the Iris data set.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

species <- unique(iris$Species)


# UI is set up with two variable selection inputs to choose y and x axis
# and a check box group to choose which species to show.
ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", iris, selected = "Petal.Length"),
    varSelectInput("yvar", "Y Variable", iris, selected = "Petal.Width"),
    checkboxGroupInput("species", "Species", species, selected = species)
  ),
  # An output for a plot is added to the UI
  plotOutput("scatter")
)


# Server function handles changes in output and input
server <- function(input, output, session){
  
  # Output is set with renderPlot.
  # It responds to changes in included inputs
  output$scatter <- renderPlot({
    # Iris is first filtered by chosen species, then displayed with a scatter plot
    iris |> 
      filter(Species %in% !!input$species) |> 
      
      ggplot(aes(!!input$xvar, !!input$yvar, color=Species)) +
        geom_point(size=3)

  }, res=100)
}

# App is run in showcase mode to show code.
shinyApp(ui, server, options = list("display.mode" = "showcase"))