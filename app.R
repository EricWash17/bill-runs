library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(flexdashboard)
library(lubridate)
library(DT)

data = readRDS("./data/shiny_data.Rds")

## function to change numeric back to time format
numeric_to_time <- function(numeric_time){
  pct <- as.POSIXct(numeric_time * 60, origin = "1960-01-01 00:00:00","GMT")
  format(pct,format = "%M:%S")
}


### Shiny App
app <- shinyApp(
  ui <- fluidPage(
    titlePanel("Run Bill, Run"),
           tags$style(type = "text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }",
                      "#average-pace {font-size:20px; color:red; display:block; }"
                      ),
    fluidRow(
      wellPanel(
        column(
          width = 6,
          # Input: Date Range
          dateRangeInput('date_range',
                         label = 'Race Date',
                         start = "1980/01/01",
                         end = Sys.Date())
               ),
        column(
          width = 6,
          pickerInput("selected_race_type",
                      "Race Type",
                      choices = c("short race", "5 km", "5 miles", "8 km", "10 km", "10 miles", "13 miles", "26 miles", "other distance", "relay"),
                      selected = "26 miles",
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE)
          )
        )
      ),
    
    fluidRow(
      column(
        width = 8,
        plotlyOutput("pace_scatter_plot")
      ),
      column(
        width = 4,
        align = "center",
        h3(textOutput("total_distance_title")),
        textOutput("total_distance"),
        br(),
        h3(textOutput("avg_pace_title")),
        textOutput("avg_pace"),
        br(),
        h3(textOutput("percentile_gauge_title")),
        gaugeOutput("percentile_gauge")
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        plotlyOutput("race_count_plot")
        ),
      column(
        width = 6,
        dataTableOutput("race_table")
        )
      )
    ),

  server <- function(input, output) {
    
    ##################################
    ####       Reactive Data      ####
    ##################################
    all_race_data <- reactive({
      race_type_filter = input$selected_race_type
      all_race_data = data %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2] &
                   race_type %in% input$selected_race_type)
      })
    
    data_by_race_type <- reactive({
      data_by_race_type = data %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2] &
                 race_type %in% input$selected_race_type) %>%
        mutate(distance_in_miles = ifelse(metric == "km", distance * 1.60934, distance)) %>%
        group_by(race_type) %>%
        dplyr::summarise(n = n()
                         , avg_pace = numeric_to_time(round(mean(pace, na.rm = TRUE), 2))
                         , total_distance_in_miles =round(sum(distance_in_miles, na.rm = TRUE)), 2)
      })
    
    ##################################
    #####         Plots         ######
    ##################################
   
    ### Scatter Plot
    output$pace_scatter_plot <- renderPlotly({
      g = ggplot(all_race_data(), aes(x = date, y = pace
                           , text = paste('Event: ', event
                                          , '<br>Type:', race_type
                                          , '<br>Date:', as.Date(date)
                                          , '<br>Pace:', pace_as_time
                                          , '<br>Time:', time
                                          ,'<br>Percentile', percentile))) +
        geom_point(aes(col = race_type)) + 
        geom_smooth(aes(group = 1), method = "loess", se = FALSE) + 
        theme_minimal() + 
        theme(legend.position = "none") + 
        labs(y = "Pace (minutes)", 
             x = "Date", 
             title = "Pace Over Time with Trend Line")
      
      ggplotly(g, tooltip = c("text")) %>% plotly::config(displayModeBar = FALSE)
    })
    
    ### Total Distance Text
    output$total_distance <- renderText({
      total_distance = paste(round(sum(data_by_race_type()$total_distance_in_miles, na.rm = TRUE), 0), "miles")
    })
    
    ### Avg Pace Text
    output$avg_pace <- renderText({
      avg_pace = numeric_to_time(round(mean(all_race_data()$pace, na.rm = TRUE), 2))
    })
    
    ### Percentile Gauge
    output$percentile_gauge <- renderGauge({
      ### Percentile Finish
      gauge_value = round(mean(all_race_data()$percentile, na.rm = TRUE), 0)
      gauge(gauge_value, min = 0, max = 100, symbol = "%", label = "Percentile"
            , gaugeSectors(colors = "red"))
    })
    
    ### Bar Plots by race_type
    output$race_count_plot <- renderPlotly({
      g = ggplot(all_race_data(), aes(year(date)
                       , text = paste('Type: ', race_type
                                      , '<br>Year:', year(date)
                                      , '<br>Avg. Pace:',  numeric_to_time(round(mean(pace, na.rm = TRUE), 2))
                                      , '<br>Avg. Percentile:', round(mean(percentile, na.rm = TRUE), 0)
                                      ))) +
        geom_bar(aes(fill = race_type), width = 0.5) + 
        theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
        theme_minimal() +
        labs(y = "Race Count" 
             , x = "Year"
             , fill = "Race Type"
             , title = "Number of Races by Year and Type")
      
      ggplotly(g, tooltip = c("text")) %>% plotly::config(displayModeBar = FALSE)
    })
    
    ### Data table
    # display 10 rows initially
    output$race_table <- DT::renderDataTable(
      DT::datatable(all_race_data() %>%
                      select(date, event, race_type, time, pace_as_time, percentile) %>%
                      mutate(date = as.Date(date)) %>%
                      rename(Date = date, Event = event, `Race Type` = race_type, Time = time, Pace = pace_as_time, `Finish %` = percentile)
                    , options = list(pageLength = 5))
    )
    
    ### Title Text
    output$percentile_gauge_title <- renderText("Average Finish") 
    output$avg_pace_title <- renderText("Average Pace")
    output$total_distance_title <- renderText("Total Distance")
    }
)

