library(dplyr)
library(ggplot2)
library(ggiraph)

library(bslib)
library(shiny)



# Data from gapminder 
gapm <- gapminder::gapminder |> 
  left_join(gapminder::country_codes) |> 
  filter(continent == "Africa") |> 
  mutate("Population (millions)" = pop/(10^6),
         "Life expectancy" = lifeExp,
         "GDP per capita" = gdpPercap) |> 
  select(-continent, -iso_num, -pop)


# Map of Africa from natural earth data set
afr_map <- rnaturalearth::ne_countries(continent = "africa") |> 
  select(iso_a3)

ui <- page_sidebar(
  title = "ggiraph example: Gapminder data exploration",
  
  sidebar = sidebar(
    varSelectInput("measure", "Measure", gapm |> select("Population (millions)", "Life expectancy", "GDP per capita")),
    selectInput("year", "Year", unique(gapm$year), selected = "2007")
    ),
  
  layout_columns(
    card(girafeOutput("map"), textOutput("descr")),
    card(girafeOutput("measure_ts")),
    col_widths = c(6,6),

    
  )
  )



server <- function(input, output, session){
  selected <- NULL
  
  observeEvent(input$map_selected, {
    selected <<- input$map_selected
  }, ignoreNULL = FALSE)
  
  output$map <- renderGirafe({
    g <- 
      gapm |> 
      filter(year == !!input$year) |> 
      right_join(afr_map, join_by(iso_alpha == iso_a3)) |> 
      
      ggplot(aes(fill = !!input$measure, 
                 geometry = geometry, 
                 data_id = iso_alpha,
                 tooltip = paste("Country: ", country, "\n", input$measure, ": ", !!input$measure))) +
        geom_sf_interactive(hover_nearest=TRUE) +
        labs(x = "Latitude", y = "Longitude")
    

    girafe(ggobj = g, options = list(opts_selection(selected = selected), 
                                     opts_hover(nearest_distance = 30)))
  })
  
  output$measure_ts <- renderGirafe({
    req(input$map_selected)
    g <- 
      gapm |> 
      filter(iso_alpha %in% !!input$map_selected) |> 
      
      ggplot(aes(x = year, y=!!input$measure, data_id=year, tooltip=!!input$measure, color=country)) +
        geom_line() +
        geom_point_interactive(hover_nearest=TRUE)
    
    girafe(ggobj = g, options = list(opts_hover(nearest_distance = 30)))
  })
  
  
  output$descr <- renderText("Select countries to view historical data in a time series; Multiple can be selected at a time.")
  
}


shinyApp(ui, server, options = list("display.mode" = "showcase"))






















