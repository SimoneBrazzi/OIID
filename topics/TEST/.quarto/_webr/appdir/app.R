# Assuming felonies_total and population_total data frames are available
omicidi_vs_popolazione <- felonies_total %>% 
  left_join(
    population_total,
    by = c("year", "sesso"),
    suffix = c("_omicidi", "_popolazione")
  ) %>% 
  filter(!is.na(value_popolazione)) %>% 
  mutate(valore_pct = value_omicidi / value_popolazione) %>% 
  pivot_longer(
    cols = !c("year", "sesso"),
    names_to = "variabile",
    values_to = "valore"
  )

ui <- fluidPage(
  useWaiter(), 
  waiterShowOnLoad(html = spin_hexdots()),
  
  titlePanel("Interactive Time Series Plot"),
  hr(),
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
  
  Sys.sleep(4) # simulate a delay
  waiter_hide()
  
  thematic::thematic_shiny()
  
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
