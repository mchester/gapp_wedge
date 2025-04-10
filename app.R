# Load libraries
library(shiny)
library(rvest)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(janitor)

# Scrape the leaderboard table
get_leaderboard <- function() {
  tryCatch({
    url <- 'https://www.espn.com/golf/leaderboard'
    page <- read_html(url)
    tables <- html_nodes(page, "table") %>% html_table(fill = TRUE)
    leaderboard <- as.data.frame(tables[[1]])
    names(leaderboard) <- make_clean_names(names(leaderboard))
    leaderboard <- leaderboard %>% filter(player != "")
    return(leaderboard)
  }, error = function(e) {
    data.frame(player = "Error", score = "Could not fetch data")
  })
}

leaderboard <- get_leaderboard()

# Build fantasy teams
create_team <- function(players, leaderboard) {
  team <- leaderboard %>% filter(player %in% players)
  team <- team %>%
    mutate(across(starts_with("r"), ~as.numeric(gsub("--", NA, .)))) %>%
    mutate(penalty = if_else(score == "CUT", 154, 0)) %>%
    adorn_totals(where = "col", fill = "-", na.rm = TRUE) %>%
    adorn_totals(where = "row", fill = "-", na.rm = TRUE)
  return(team)
}

marshall_players <- c("Scottie Scheffler", "Tom Kim", "Taylor Pendrith", "Tommy Fleetwood")
adam_players <- c("Rory McIlroy", "Collin Morikawa", "Hideki Matsuyama", "Corey Conners")

marshall_team <- create_team(marshall_players, leaderboard)
adam_team <- create_team(adam_players, leaderboard)

# UI
ui <- fluidPage(
  titlePanel("2025 Tariff International"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Team or Leaderboard:", 
                  choices = c("Leaderboard", "Marshall", "Adam")),
      numericInput("obs", "Number of observations to view:", value = 10),
      checkboxInput("show_totals", "Show totals row/column", value = TRUE)
    ),
    mainPanel(
      h3(textOutput("caption")),
      tableOutput("view")
    )
  )
)

# Server
server <- function(input, output) {
  datasetInput <- reactive({
    data <- switch(input$dataset,
                   "Leaderboard" = leaderboard,
                   "Marshall" = marshall_team,
                   "Adam" = adam_team)
    if (!input$show_totals) {
      data <- data[!grepl("Total", data$player, ignore.case = TRUE), ]
    }
    return(data)
  })
  
  output$caption <- renderText({
    paste("Current View:", input$dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
}

# Run app
shinyApp(ui = ui, server = server)
