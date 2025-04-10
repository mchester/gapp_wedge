#load libraries

library(rvest)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(janitor)
library(dplyr)

#load table

library(rmarkdown)
library(dplyr)

url <- 'https://www.espn.com/golf/leaderboard'
webpage <- read_html(url)
node_list <- webpage %>% 
    html_nodes("table")
leaderboard <- data.frame(node_list %>% 
                              html_table())
leaderboard <- leaderboard %>%
  select(-c(Var.1, Var.3))

paged_table(leaderboard)

#load team marshall

Scottie_Scheffler <- leaderboard[leaderboard$PLAYER == "Scottie Scheffler", ] #American
Tom_Kim <- leaderboard[leaderboard$PLAYER == "Tom Kim", ] #Asia
Taylor_Pendrith <- leaderboard[leaderboard$PLAYER == "Taylor Pendrith", ] #Canada
Tommy_Fleetwood <- leaderboard[leaderboard$PLAYER == "Tommy Fleetwood", ] #UK


marshall_team <- data.frame(rbind(Scottie_Scheffler,
                                  Tom_Kim,
                                  Taylor_Pendrith,
                                  Tommy_Fleetwood),
                            header = TRUE,
                            stringsAsFactors = FALSE)

marshall_team$R1 <- as.numeric(as.character(marshall_team$R1))
marshall_team$R2 <- as.numeric(as.character(marshall_team$R2))
marshall_team$R3 <- as.numeric(as.character(marshall_team$R3))
marshall_team$R4 <- as.numeric(as.character(marshall_team$R4))
marshall_team <- subset(marshall_team, select = -c(header, TOT))
marshall_team <- marshall_team %>% mutate(PENALTY = if_else(SCORE == "CUT", 154, 0))
marshall_team <- adorn_totals(marshall_team, where = "col", fill = "-", na.rm = TRUE)
marshall_team <- adorn_totals(marshall_team, where = "row", fill = "-", na.rm = TRUE)

#load team adam

Rory_McIlroy <- leaderboard[leaderboard$PLAYER == "Rory McIlroy", ] #Euro
Collin_Morikawa <- leaderboard[leaderboard$PLAYER == "Collin Morikawa", ] #amateur
Hideki_Matsuyama <- leaderboard[leaderboard$PLAYER == "Hideki Matsuyama", ] #Asia
Corey_Conners <- leaderboard[leaderboard$PLAYER == "Corey Conners", ] #Canada

adam_team <- data.frame(rbind(Rory_McIlroy, 
                              Collin_Morikawa,
                              Hideki_Matsuyama,
                              Corey_Conners),
                        header = TRUE,
                        stringsAsFactors = FALSE)
adam_team$R1 <- as.numeric(as.character(adam_team$R1))
adam_team$R2 <- as.numeric(as.character(adam_team$R2))
adam_team$R3 <- as.numeric(as.character(adam_team$R3))
adam_team$R4 <- as.numeric(as.character(adam_team$R4))
adam_team <- subset(adam_team, select = -c(header, TOT))
adam_team <- adam_team %>% mutate(PENALTY = if_else(SCORE == "CUT", 154, 0))
adam_team[adam_team == "--"] <- NA
adam_team <- adorn_totals(adam_team, where = "col", fill = "-", na.rm = TRUE)
adam_team <- adorn_totals(adam_team, where = "row", fill = "-", na.rm = TRUE)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("2025 Tariff International"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Text for providing a caption ----
            # Note: Changes made to the caption in the textInput control
            # are updated in the output area immediately as you type
         
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "View",
                        choices = c("Leaderboard", "Adam", "Marshall")),
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3(textOutput("caption", container = span)),
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            tableOutput("view")
            
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <<- function(input, output) {
    observe({invalidateLater(1000)
        })
    datasetInput <- reactive({
        switch(input$dataset,
               "Leaderboard" = leaderboard,
               "Adam" = adam_team,
               "Marshall" = marshall_team)
    })
    
    output$caption <- renderText({
        input$caption
    })
    
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)