library(shiny)
library(leaflet)
library(r2d3)
library(geojsonio)
library(DT)
library(ggplot2)
library(tbilinomics.htmlwidgets)
library(shinythemes)


ui <- fixedPage(
  theme = shinytheme("superhero"),
  
  tags$head(
    tags$style(
      HTML("
        @import url('https://fonts.googleapis.com/css?family=Montserrat');
      
        #main-title {
          font-family: Montserrat;
          padding: 40px 0px;
        }
        
        #logo {
          width: auto;
          height: 80px;
          float: left;
        }
        
        h1, h2, h3 {
          font-family: Montserrat;
        }
        
        #hexbin svg {
          display: block;
          margin: auto;
        }
        
        .dashboard-column {
          height: 650px;
        }
        
        .main-well-map {
          height: 595px;
        }
        
        .main-well-chart {
          height: 540px;
        }
        
        .main-well-scatter {
          height: 560px;
        }
        
        .dataset-well {
          height: 250px;
        }
        
        .map-controls-well {
          height: 325px;
        }
        
        .chart-controls-well {
          height: 270px;
        }
        
        .scatter-controls-well {
          height: 290px;
        }
        
        
        
        table, .table {
          font-size: 15px;
        }
        
        .irs-grid-pol.small { 
          height: 0px; 
        }
        
        div.tooltip { 
          position: absolute;
          text-align: center;
          width: 150px;                  
          height: 25px;
          padding: 2px;
          font-size: 10px;
          background: #FFFFE0;
          border: 1px;
          border-radius: 8px;
          pointer-events: none; 
        }
      ")
    )
  ),
  
  tags$div(
    img(src = "images/bird.png", id = "logo"),
    h1("Tbilinomics Data Visualizations"),
    id = "main-title"  
  ),
  
  tabsetPanel(type = "tabs",
    
    tabPanel("Home",
      p(),
      fixedRow(
        column(4, 
          wellPanel(
            h2("Explore Our Datasets"),
            p("Choose a dataset and get a dashboard"),
            selectInput("dataset", "", c("GeoStat Municipal Database"))
          ),
          
          wellPanel(
            h2("Cutting Edge Visualizations"),
            p("This site is built on the R language and D3.js.")
          )
        ),
        
        column(8,
          wellPanel(
            rnormHexbinOutput("hexbin")
          )
        )
      )
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
    
    tabPanel("Dashboard",
      p(),
      fixedRow(
        column(4,
          tags$div(
            wellPanel(
              h2("GeoStat Municipalities"),
              p("First, choose the type of visualization that you would like to see."),
              radioButtons("radio_plot_type", label = "", 
                           choices = c("Map" = 1, "Chart" = 2, "Scatter" = 3, "Table" = 4),
                           selected = 1, inline = TRUE),
              class = "dataset-well"
            ),
            wellPanel(
              selectInput("indicator_svg", "Select a variable to map", c("")),
              sliderInput("year_svg", "And a year", min = 2008, max = 2018,
                          value = 2018, step = 1, sep = ""),
              actionButton("recalc_svg", "Map It"),
              id = "well_panel_controls"
            ),
            class = "dashboard-column",
            id = "sidebar_column"
          )
        ),
        column(8,
          
          id = "main_column"
        )
      )
    ),
                    
    # tabPanel("Municipalities",
    #   p(),
    #   fixedRow(
    #     column(4,
    #       wellPanel(
    #         h2("Data by Municipality"),
    #         p("Choose a municipality to see data on all indicators in given year.")
    #       ),
    #       wellPanel(
    #         selectInput("muni", "Select a municipality", c("")),
    #         sliderInput("year_muni", "and a year", min = 2008, max = 2018,
    #                     value = 2018, step = 1, sep = ""),
    #         actionButton("recalc_muni", "Get Data")
    #       )
    #     ),
    #     column(8,
    #       DT::dataTableOutput("table_muni")
    #     )
    #   )         
    # ),
                    
    tabPanel("Data",
      p(),
      h2("GeoStat Municipal Dataset", class = "text-center"),
      p(),
      DT::dataTableOutput("data")
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
            actionButton("recalc_svg", "Map It", class = "btn-primary"),
            id = "well_panel_controls",
            class = "map-controls-well"
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
              actionButton("recalc_chart", "Plot It", class = "btn-primary"),
              id = "well_panel_controls",
              class = "chart-controls-well"
            )
          ),
          `if`(
            plot_type == 3,
            tags$div(
              wellPanel(
                p("Plot the relationship between different indicators."),
                selectInput("indicator_scatter_1", "Select one variable", choices = choices),
                selectInput("indicator_scatter_2", "and another", choices = choices, selected = "Indicator 2"),
                actionButton("recalc_scatter", "Scatter Plot", class = "btn-primary"),
                id = "well_panel_controls",
                class = "scatter-controls-well"
              )
            ),
            tags$div(
              wellPanel(
                p("See data on all indicators in given year."),
                selectInput("muni", "Select a municipality", munis),
                sliderInput("year_muni", "and a year", min = 2008, max = 2018,
                            value = 2018, step = 1, sep = ""),
                actionButton("recalc_muni", "Get Data", class = "btn-primary"),
                id = "well_panel_controls",
                class = "table-controls-well"
              )
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
          wellPanel(
            d3Output("map_svg"),
            class = "main-well-map",
            id = "main-well"
          ),
          class = "dashboard-column",
          id = "main_plot"
        ),
        `if`(
          plot_type == 2,
          tags$div(
            wellPanel(
              plotOutput("chart"),
              class = "main-well-chart",
              id = "main-well"
            ),
            class = "dashboard-column",
            id = "main_plot"
          ),
          `if`(
            plot_type == 3,
            tags$div(
              wellPanel(
                plotOutput("scatter"),
                class = "main-well-scatter",
                id = "main-well"
              ),
              class = "dashboard-column",
              id = "main_plot"
            ),
            tags$div(
                p(),
                DT::dataTableOutput("table_muni"),
                id = "main_plot",
                class = "dashboard-column"
            )
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
      selector = "#main-well",
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
      selector = "#main-well",
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
      selector = "#main-well",
      where = "afterBegin",
      ui = h3(
        paste0("Scatterplot of ", indicator1, " vs. ", indicator2),
        class = "text-center",
        id = "title_chart"
      )
    )
  })
  
  observeEvent(input$recalc_muni, {
    muni <- input$muni
    
    removeUI(
      selector = "#title_chart"
    )
    
    insertUI(
      selector = "#main_plot",
      where = "afterBegin",
      ui = h3(
        paste0("Table of Indicators for ", muni),
        class = "text-center",
        id = "title_chart"
      )
    )
  })
  
  var1 <- runif(1, 100, 10000)
  var2 <- runif(1, 100, 10000)
  covar <- runif(1, -1 * min(var1, var2), min(var1, var2))
  
  output$hexbin <- renderRnormHexbin(
    rnormHexbin(list(var1 = var1, var2 = var2, covar = covar))
  )
  
  output$chart <- renderPlot(
    ggplot(dataChart(), aes(x = Year, y = Indicator)) +
      geom_line(aes(color = Municipality), size = 2) +
      scale_x_continuous(breaks = 2008:2018, labels = c("2008", "2009", "2010", "2011", "2012",
                                    "2013", "2014", "2015", "2016", "2017",
                                    "2018")) +
      theme(
        plot.background = element_rect(
          fill = "#4e5d6c",
          color = "#4e5d6c"
        ),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(
          color = "white"
        ),
        axis.text = element_text(
          color = "white"
        ),
        axis.ticks = element_line(
          color = "white"
        )
      )
  )
  
  output$scatter <- renderPlot(
    ggplot(dataScatter(), aes(x = Indicator1, y = Indicator2)) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme(
        plot.background = element_rect(
          fill = "#4e5d6c",
          color = "#4e5d6c"
        ),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(
          color = "white"
        ),
        axis.text = element_text(
          colour = "white"
        ),
        axis.ticks = element_line(
          color = "white"
        )
      )
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
      script = "www/tbilinomics-choropleth-map.js"
    )
  })
  
  output$table_muni <- DT::renderDataTable(dataMuni(), style = "bootstrap")
      
  output$data <- DT::renderDataTable(
    raw_data,
    options = list(scrollX = TRUE),
    style = "bootstrap"
  )
}

shinyApp(ui, server)