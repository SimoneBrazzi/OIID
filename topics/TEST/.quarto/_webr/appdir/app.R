source("~/R/OIID/topics/function.R")
library(tidyverse, verbose = FALSE)
library(ggplot2)
library(lubridate)
library(rsdmx)
library(owidR)
library(reticulate)
library(gt)
library(scales, verbose = FALSE)
library(janitor)
library(shiny)
library(shinylive)
library(thematic)
library(waiter)
library(plotly)
library(ggiraph)
library(fst)
library(readr)  # For reading and writing CSV files
library(R.utils)  # For gzip compression

omicidi_vs_popolazione <- felonies_total %>% 
  left_join(
    population_total,
    by = c("year", "sesso"),
    suffix = c("_omicidi", "_popolazione")
  ) %>% 
  filter(!is.na(value_popolazione)) %>% 
  mutate(pct = value_omicidi / value_popolazione) %>% 
  rename(
    omicidi = value_omicidi,
    popolazione = value_popolazione
    ) %>% 
  select(year, sesso, pct, omicidi, popolazione) %>% 
  pivot_longer(
    cols = !c("year", "sesso"),
    names_to = "variabile",
    values_to = "valore"
  )

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "sesso",
        label = "Sesso",
        choices = unique(omicidi_vs_popolazione$sesso),
        multiple = TRUE,
        selected = c("maschi", "femmine")
      ),
      selectizeInput(
        inputId = "variabile",
        label = "Variabile",
        choices = unique(omicidi_vs_popolazione$variabile),
        selected = "value_omicidi"
      )
    ),
    mainPanel(
      plotlyOutput("plot", width = "100%", height = "600px")
    )
  )
)

server <- function(input, output) {

  data <- reactive({
    omicidi_vs_popolazione %>% 
      filter(
        sesso %in% input$sesso &
          variabile %in% input$variabile
      )
  })
  
  output$plot <- renderPlotly({
    
    g <- ggplot(data(), aes(x = year, y = valore, colour = sesso)) +
      geom_line() +
      geom_point() +
      scale_color_manual(
        values = c("maschi" = "#1B9E77",
                   "femmine" = "#D95F02")
      ) +
      scale_x_date(
        date_breaks = "year",
        date_labels = "%Y"
      ) +
      labs(
        title = "Omicidi",
        subtitle = "Per sesso e variabile",
        x = "Anni",
        y = input$variabile,
        color = "Sesso"
      ) +
      scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
      theme_minimal()
    ggplotly(g) %>% 
      layout(hovermode = "x unified")
    
  })
}

shinyApp(ui = ui, server = server)
