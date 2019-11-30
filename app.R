library(shiny)
library(ggplot2)
library(plotly)
library(flexdashboard)
library(ggpubr)
library(lubridate)


data = readRDS("./data/shiny_data.Rds")



### Scatter Plot
g = ggplot(data, aes(x = date, y = pace
                     , text = paste('Event: ', event
                                    , '<br>Date:', date
                                    , '<br>Pace:', pace
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


### Percentile Finish
gauge_value = round(mean(data$percentile, na.rm = TRUE), 0)
gauge(gauge_value, min = 0, max = 100, symbol = "%", label = "Average Percentile Finish"
      , gaugeSectors(colors = "red")
)
    

### Donut Graph
# Create test data.
# Compute percentages
data_by_race_type = data %>%
  group_by(race_type) %>%
  dplyr::tally()

total_races = sum(data_by_race_type$n)
data_by_race_type = data_by_race_type %>%
  mutate(race_type = ifelse(n < total_races * .05, "other", race_type)) %>%
  group_by(race_type) %>%
  mutate(n = sum(n)
         , race_percent = round((n / total_races) * 100, 0)) %>%
  distinct() %>%
  as.data.frame()


donut_labs <- paste0(data_by_race_type$race_type, " (", data_by_race_type$race_percent, "%)")
ggdonutchart(data_by_race_type, "race_percent", label = donut_labs
             , fill = "race_type",  lab.pos = "in", lab.font = "white") + 
  theme(legend.position = "none") + 
  labs(y = "Pace (minutes)", 
       x = "Date", 
       title = "Pace Over Time")
  


ggplot(data, aes(year(date))) +
  geom_bar(aes(fill = race_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  theme_minimal() +
  labs(y = "Race Count" 
       , x = "Year"
       , fill = "Race Type"
       , title = "Number of Races by Year and Type")


### Shiny App
app <- shinyApp(
  ui <- navbarPage(
    windowTitle = "Run Bill, Run",
    tabPanel('Race Stats',
             # titlePanel("Decision API Dashboard"),
             fluidPage(
               tags$style(type = "text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }",
                          "#average-time {font-size:20px; color:red; display:block; }"
                          ),
               fluidRow(
                 column(
                   width = 3, 
                   wellPanel(
                     # Input: Date Range
                     dateRangeInput('date_range',
                                    label = 'Filter tests by date',
                                    start = min(data$date),
                                    end = Sys.Date()),
                     pickerInput("selected_race_type",
                                 "Race Type",
                                 choices = unique(data$race_type),
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE)
                     )
                   ),
                 column(
                   width = 6,
                   plotlyOutput('pace_scatter_plot')
                   ),
                 column(
                   width = 3,
                   align = "center",
                   h3(textOutput("percentile_gauge_title")),
                   gaugeOutput("percentile_gauge"),
                   br(),
                   h3(textOutput("avg_time_title")),
                   textOutput("avg_time")
                 )
                 ),
               
               fluidRow(
                 column(
                   width = 6,
                   #align = "center",
                   #h3(textOutput("roc_plot_title")),
                   plotOutput("race_count_plot")
                 ),
                 column(
                   width = 6,
                   dataTableOutput("race_table")
                 )
                 )
               )
             )
    ),

  server <- function(input, output) {
    
    ##################################
    ####       Reactive Data      ####
    ##################################
    all_race_data <- reactive({
      race_type_filter = input$selected_race_type
      all_race_data = data[date >= input$date_range[1] & date <= input$date_range[2] &
                   race_type %in% race_type_filter, ]
      })
    
    
    ##################################
    #####         Plots         ######
    ##################################
    
    
    output$dist_plot <- renderPlot({
      
      x    <- na.omit(data$pace)
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "#75AADB", border = "white",
           xlab = "Pace (minutes)",
           main = "Race Pace")
      })
    
    }
)