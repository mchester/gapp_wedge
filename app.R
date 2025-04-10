# Load libraries
library(rvest)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(janitor)
library(dplyr)
library(DT)

# Helper function to clean and process leaderboard data
process_team_data <- function(team_data) {
  team_data$R1 <- as.numeric(as.character(team_data$R1))
  team_data$R2 <- as.numeric(as.character(team_data$R2))
  team_data$R3 <- as.numeric(as.character(team_data$R3))
  team_data$R4 <- as.numeric(as.character(team_data$R4))

  team_data <- team_data %>%
    mutate(PENALTY = if_else(SCORE == "CUT", 154, 0))
  
  team_data[team_data == "--"] <- NA
  team_data <- adorn_totals(team_data, where = "col", fill = "-", na.rm = TRUE)
  team_data <- adorn_totals(team_data, where = "row", fill = "-", na.rm = TRUE)
  
  return(team_data)
}

# Scrape leaderboard data
url <- 'https://www.espn.com/golf/leaderboard?tournamentId=401703504'
webpage <- read_html(url)
node_list <- webpage %>% html_nodes("table")
leaderboard <- data.frame(node_list %>% html_table())
leaderboard <- leaderboard %>% select(-c(Var.1))

# Load team data (Marshall)
marshall_team <- leaderboard %>%
  filter(PLAYER %in% c("Scottie Scheffler", "Tom Kim", "Taylor Pendrith", "Tommy Fleetwood"))
marshall_team <- process_team_data(marshall_team)

# Load team data (Adam)
adam_team <- leaderboard %>%
  filter(PLAYER %in% c("Rory McIlroy", "Collin Morikawa", "Hideki Matsuyama", "Corey Conners"))
adam_team <- process_team_data(adam_team)

# Define UI for dataset viewer app
ui <- fluidPage(
  
  # App title
  titlePanel("2025 Tariff International"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      selectInput(inputId = "dataset", 
                  label = "View", 
                  choices = c("Leaderboard", "Adam", "Marshall")),
      
      numericInput(inputId = "obs", 
                   label = "Number of observations to view:", 
                   value = 10)
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h3(textOutput("caption", container = span)),
      verbatimTextOutput("summary"),
      DTOutput("view")
    )
  )
)

# Define server logic to summarize and view selected dataset
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Leaderboard" = leaderboard,
           "Adam" = adam_team,
           "Marshall" = marshall_team)
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  output$view <- renderDT({
    datatable(head(datasetInput(), n = input$obs), options = list(pageLength = 10))
  })
  
}

# Create Shiny app
shinyApp(ui, server)
