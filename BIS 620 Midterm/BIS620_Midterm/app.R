#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("ct-util.R")
max_num_studies = 1000

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Clinical Trials Query"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    position = 'left',
    sidebarPanel(
      textInput("brief_title_kw", h3("Brief Title Keywords")),
      textInput("condition_kw", h3("Condition Names")),
      # Q3: Add a drop-down on sponsor type
      selectInput("source_class", 
                  label = h3("Sponsor Type"),
                  choices = list("Federal" = "FED", 
                                 "Individual" = "INDIV", 
                                 "Industry" = "INDUSTRY", 
                                 "Network" = "NETWORK", 
                                 "NTH" = "NTH",
                                 "Other" = "OTHER", 
                                 "Other gov" = "OTHER_GOV", 
                                 "Unknown" = "Unknown"),
                  multiple = TRUE),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        tabPanel("Conditions", plotOutput("condition_plot")),
        tabPanel("World Map", plotOutput("world_map_plot")),
      ),
      dataTableOutput("trial_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # react to user input
  # when user input change, get_studies will be updated, this will trigger other functions
  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    
    # if input is NULL, return all values
    if (!is.null(input$source_class)) {
      ret = ret |> 
        filter(source_class %in% !!input$source_class) 
    }
    ret |>
      head(max_num_studies) |>
      collect()
    
  })
  
  # Feature2: Add a conditions input to search for conditions based on keywords
  get_conditions = reactive({
    if (input$condition_kw != "") {
      si = input$condition_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret2 = query_kwds(conditions, si, "name", match_all = FALSE)
    } else {
      ret2 = conditions
    }
    ret2 |>
      collect()
  })
  
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })
  
  output$concurrent_plot = renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") + 
      theme_bw()
  })
  
  output$condition_plot = renderPlot({
    # Feature2: update on condition plot
    study = get_studies()
    condition = get_conditions()
    condition_data <- get_condition_histogram(study, condition) 
    
    ggplot(condition_data, aes(x=name, y=n)) +
      geom_col()+
      theme_bw()+
      xlab("Condition")+
      ylab("Count") + 
      title("Top 6 Conditions by Condition Name")

  })
  
  output$world_map_plot = renderPlot({
    get_studies()|>
      plot_country_map()
  })
  
  output$trial_table = renderDataTable({
    get_studies() |> 
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
