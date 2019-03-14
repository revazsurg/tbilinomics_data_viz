library(shiny)
library(leaflet)
library(r2d3)
library(geojsonio)
library(DT)
library(ggplot2)


ui <- fixedPage(
  tags$style(type = "text/css", ".irs-grid-pol.small { height: 0px; }
    div.tooltip { position: absolute;text-align: center;width: 150px;                  
    height: 25px;padding: 2px;font-size: 10px;background: #FFFFE0;
    border: 1px;border-radius: 8px;pointer-events: none; }"),
  titlePanel("Tbilinomics Data Visualizations"),
  
  tabsetPanel(type = "tabs",
    
    tabPanel("Home",
      h2("Visualize economic data at the municipal level."),
      p("We provide tools to visualize trends and relationships in Georgian economic data"),
      p("All of the data on this site was collected from the Georgian National Statistics Office (GeoStat)")
    ),
    
    # tabPanel("Map (Raster)",
    #   p(),
    #   fixedRow(
    #     column(4,
    #       wellPanel(
    #         selectInput("indicator_raster", "Choose Indicator", c("")),
    #         sliderInput("year_raster", "Select Year", min = 2008, max = 2018,
    #                     value = 2018, step = 1, sep = ""),
    #         actionButton("recalc_raster", "Map It")
    #       )
    #     ),
    #     column(8,
    #       leafletOutput("map_raster")
    #     )
    #   )
    # ),
    
    tabPanel("Plot",
      p(),
      fixedRow(
        column(4,
          wellPanel(
            h2("Explore Our Dataset"),
            p("As a first step, choose the type of visual representation that you would like to create."),
            radioButtons("radio_plot_type", label = "", choices = c("Map" = 1, "Chart" = 2, "Scatter" = 3),
                         selected = 1, inline = TRUE)
          ),
          wellPanel(
            selectInput("indicator_svg", "Select a variable to map", c("")),
            sliderInput("year_svg", "And a year", min = 2008, max = 2018,
                        value = 2018, step = 1, sep = ""),
            actionButton("recalc_svg", "Map It"),
            id = "well_panel_controls"
          ),
          id = "sidebar_column"
        ),
        column(8,
          
          id = "main_column"
        )
      )
    ),
                    
    tabPanel("Municipalities",
      p(),
      fixedRow(
        column(4,
          wellPanel(
            h2("Data by Municipality"),
            p("Choose a municipality to see data on all indicators in given year.")
          ),
          wellPanel(
            selectInput("muni", "Select a municipality", c("")),
            sliderInput("year_muni", "and a year", min = 2008, max = 2018,
                        value = 2018, step = 1, sep = ""),
            actionButton("recalc_muni", "Get Data")
          )
        ),
        column(8,
          DT::dataTableOutput("table_muni")
        )
      )         
    ),
                    
    tabPanel("All Data",
      p(),
      DT::dataTableOutput("all_data")
    )
  ),
  p()
)

server <- function(input, output, session) {
  raw_data <- read.csv("indicators_data.csv")
  choices <- gsub("\\.", " ", names(raw_data)[-(1:2)])
  munis <- levels(raw_data$Municipality)
        
  # updateSelectInput(
  #   session,
  #   "indicator_raster",
  #   choices = choices
  # )
  
  updateSelectInput(
    session,
    "muni",
    choices = munis
  )
        
  # dataRaster <- eventReactive(input$recalc_raster, {
  #   raw_coor <- geojsonio::geojson_read("www/georgia_munis.geojson", what = "sp")
  #   choiceIndicator <- gsub(" ", "\\.", input$indicator_raster)
  #   data <- raw_data[c("Municipality", "Year", choiceIndicator)][raw_data$Year == input$year_raster, -2]
  #   colnames(data) <- c("Municipality", "Indicator")
  #   sp::merge(raw_coor, data, by.x = "NAME_2", by.y = "Municipality")
  # })
  
  dataSvg <- eventReactive(input$recalc_svg, {
    choiceIndicator <- gsub(" ", "\\.", input$indicator_svg)
    data <- raw_data[c("Municipality", "Year", choiceIndicator)][raw_data$Year == input$year_svg, -2]
    colnames(data) <- c("Municipality", "Indicator")
    data
  })
  
  dataChart <- eventReactive(input$recalc_chart, {
    choice_indicator <- gsub(" ", "\\.", input$indicator_chart)
    choice_muni <- input$muni_chart
    data <- raw_data[, c("Year", choice_indicator, "Municipality")][raw_data$Municipality %in% choice_muni, ]
    colnames(data) <- c("Year", "Indicator", "Municipality")
    data
  })
  
  dataScatter <- eventReactive(input$recalc_scatter, {
    choice_indicator1 <- gsub(" ", "\\.", input$indicator_scatter_1)
    choice_indicator2 <- gsub(" ", "\\.", input$indicator_scatter_2)
    data <- raw_data[, c("Year", "Municipality", choice_indicator1, choice_indicator2)]
    colnames(data) <- c("Year", "Municipality", "Indicator1", "Indicator2")
    data
  })
  
  dataMuni <- eventReactive(input$recalc_muni, {
    data <- raw_data[raw_data$Year == input$year_muni & raw_data$Municipality == input$muni,]
    cols <- gsub("\\.", " ", colnames(data))
    colnames(data) <- cols
    data <- data[-(1:2)]
    data <- t(data)
    colnames(data) <- c("Value")
    data
  })
  
  observeEvent(input$radio_plot_type, {
    plot_type = input$radio_plot_type
    raw_data <- read.csv("indicators_data.csv")
    choices <- gsub("\\.", " ", names(raw_data)[-(1:2)])
    munis <- levels(raw_data$Municipality)
    
    removeUI(selector = "#well_panel_controls")
    removeUI(selector = "#main_plot")
    
    insertUI(
      selector = "#sidebar_column",
      where = "beforeEnd",
      ui = `if`(
        plot_type == 1,
        tags$div(
          wellPanel(
            p("Map the spatial distribution of different indicators."),
            selectInput("indicator_svg", "Select a variable to map", choices = choices),
            sliderInput("year_svg", "and a year", min = 2008, max = 2018,
                        value = 2018, step = 1, sep = ""),
            actionButton("recalc_svg", "Map It"),
            id = "well_panel_controls"
          )
        ),
        `if`(
          plot_type == 2,
          tags$div(
            wellPanel(
              p("Visualize trends for different municipalities."),
              selectInput("indicator_chart", "Select a variable to plot", choices = choices),
              selectInput("muni_chart", "and some municipalities", choices = munis, multiple = TRUE,
                          selectize = TRUE),
              actionButton("recalc_chart", "Plot It"),
              id = "well_panel_controls"
            )
          ),
          tags$div(
            wellPanel(
              p("Plot the relationship between different indicators."),
              selectInput("indicator_scatter_1", "Select one variable", choices = choices),
              selectInput("indicator_scatter_2", "and another", choices = choices, selected = "Indicator 2"),
              actionButton("recalc_scatter", "Scatter Plot"),
              id = "well_panel_controls"
            )
          )
        )
      )
    )
    
    insertUI(
      selector = "#main_column",
      where = "beforeEnd",
      ui = `if`(
        plot_type == 1,
        tags$div(
          d3Output("map_svg"),
          id = "main_plot"
        ),
        `if`(
          plot_type == 2,
          tags$div(
            plotOutput("chart"),
            id = "main_plot"
          ),
          tags$div(
            plotOutput("scatter"),
            id = "main_plot"
          )
        )
      )
      
    )
  })
  
  observeEvent(input$recalc_svg, {
    indicator <- input$indicator_svg
    
    removeUI(
      selector = "#title_map"
    )
    
    insertUI(
      selector = "#main_plot",
      where = "afterBegin",
      ui = h3(
        paste0("Geographical distribution of ", indicator, " by municipality"),
        class = "text-center",
        id = "title_map"
      )
    )
  })
  
  observeEvent(input$recalc_chart, {
    indicator <- input$indicator_chart
    muni <- input$muni_chart
    
    removeUI(
      selector = "#title_chart"
    )
    
    insertUI(
      selector = "#main_plot",
      where = "afterBegin",
      ui = h3(
        paste0("Time series of ", indicator),
        class = "text-center",
        id = "title_chart"
      )
    )
  })
  
  observeEvent(input$recalc_scatter, {
    indicator1 <- input$indicator_scatter_1
    indicator2 <- input$indicator_scatter_2
    
    removeUI(
      selector = "#title_chart"
    )
    
    insertUI(
      selector = "#main_plot",
      where = "afterBegin",
      ui = h3(
        paste0("Scatterplot of ", indicator1, " vs. ", indicator2),
        class = "text-center",
        id = "title_chart"
      )
    )
  })
  
  output$chart <- renderPlot(
    ggplot(dataChart(), aes(x = Year, y = Indicator)) +
      geom_line(aes(color = Municipality), size = 2) +
      scale_x_continuous(breaks = 2008:2018, labels = c("2008", "2009", "2010", "2011", "2012",
                                    "2013", "2014", "2015", "2016", "2017",
                                    "2018"))
  )
  
  output$scatter <- renderPlot(
    ggplot(dataScatter(), aes(x = Indicator1, y = Indicator2)) +
      geom_point() +
      geom_smooth(method = "lm")
  )
  
  # output$map_raster <- renderLeaflet({
  #   bins <- c(0, 25, 50, 75, 100)
  #   pal <- colorBin("YlOrRd", domain = dataRaster()$Indicator, bins = bins)
  #         
  #   labels <- sprintf(
  #     "<strong>%s</strong><br/>%g",
  #     dataRaster()$NAME_2, dataRaster()$Indicator
  #   ) %>% lapply(htmltools::HTML)
  #         
  #   leaflet(dataRaster()) %>%
  #     setView(43.39, 42.3, 7) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       fillColor = ~pal(Indicator),
  #       weight = 2,
  #       opacity = 1,
  #       color = "white",
  #       dashArray = "3",
  #       fillOpacity = 0.7,
  #       highlight = highlightOptions(
  #         weight = 5,
  #         color = "#666",
  #         dashArray = "",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE
  #       ),
  #       label = labels,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "15px",
  #         direction = "auto")) %>%
  #     addLegend(pal = pal, values = ~Indicator, opacity = 0.7, title = NULL,
  #                     position = "topright")
  # })
  
  output$map_svg <- renderD3({
    r2d3(
      dataSvg(),
      script = "www/index.js"
    )
  })
  
  output$table_muni <- DT::renderDataTable({
    dataMuni()
  })
      
  output$all_data <- DT::renderDataTable(
    raw_data,
    options = list(scrollX = TRUE)
  )
}

shinyApp(ui, server)