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

Cameron_Smith <- leaderboard[leaderboard$PLAYER == "Cameron Smith", ] #LIV
Scottie_Scheffler <- leaderboard[leaderboard$PLAYER == "Scottie Scheffler", ] #American
Tom_Kim <- leaderboard[leaderboard$PLAYER == "Tom Kim", ] #Asia
Jon_Rahm <- leaderboard[leaderboard$PLAYER == "Jon Rahm", ] #euro
Jordan_Spieth <- leaderboard[leaderboard$PLAYER == "Jordan Spieth", ] #world 
Sam_Bennett <- leaderboard[leaderboard$PLAYER == "Sam Bennett (a)", ] #amateur
Max_Homa <- leaderboard[leaderboard$PLAYER == "Max Homa", ] #wildcard

marshall_team <- data.frame(rbind(Cameron_Smith, 
                                  Scottie_Scheffler,
                                  Tom_Kim,
                                  Jon_Rahm,
                                  Jordan_Spieth,
                                  Sam_Bennett,
                                  Max_Homa),
                            header = TRUE,
                            stringsAsFactors = FALSE)

marshall_team$R1 <- as.numeric(as.character(marshall_team$R1))
marshall_team$R2 <- as.numeric(as.character(marshall_team$R2))
marshall_team$R3 <- as.numeric(as.character(marshall_team$R3))
marshall_team$R4 <- as.numeric(as.character(marshall_team$R4))
marshall_team <- subset(marshall_team, select = -c(header, TOT))
marshall_team <- marshall_team %>% mutate(PENALTY = if_else(SCORE == "CUT", 160, 0))
marshall_team <- adorn_totals(marshall_team, where = "col", fill = "-", na.rm = TRUE)
marshall_team <- adorn_totals(marshall_team, where = "row", fill = "-", na.rm = TRUE)

#load team adam

Rory_McIlroy <- leaderboard[leaderboard$PLAYER == "Rory McIlroy", ] #Euro
Xander_Schauffele <- leaderboard[leaderboard$PLAYER == "Xander Schauffele", ] #Wildcard
Collin_Morikawa <- leaderboard[leaderboard$PLAYER == "Collin Morikawa", ] #amateur
Joaquin_Niemann <- leaderboard[leaderboard$PLAYER == "Joaquin Niemann", ] #Wildcard
Tony_Finau <- leaderboard[leaderboard$PLAYER == "Tony Finau", ] #American
Hideki_Matsuyama <- leaderboard[leaderboard$PLAYER == "Hideki Matsuyama", ] #Asia
Dustin_Johnson <- leaderboard[leaderboard$PLAYER == "Dustin Johnson", ] #Asia

adam_team <- data.frame(rbind(Rory_McIlroy, 
                              Xander_Schauffele,
                              Collin_Morikawa, 
                              Joaquin_Niemann,
                              Tony_Finau,
                              Hideki_Matsuyama,
                              Dustin_Johnson),
                        header = TRUE,
                        stringsAsFactors = FALSE)
adam_team$R1 <- as.numeric(as.character(adam_team$R1))
adam_team$R2 <- as.numeric(as.character(adam_team$R2))
adam_team$R3 <- as.numeric(as.character(adam_team$R3))
adam_team$R4 <- as.numeric(as.character(adam_team$R4))
adam_team <- subset(adam_team, select = -c(header, TOT))
adam_team <- adam_team %>% mutate(PENALTY = if_else(SCORE == "CUT", 160, 0))
adam_team[adam_team == "--"] <- NA
adam_team <- adorn_totals(adam_team, where = "col", fill = "-", na.rm = TRUE)
adam_team <- adorn_totals(adam_team, where = "row", fill = "-", na.rm = TRUE)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("2023 Masters"),
    
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