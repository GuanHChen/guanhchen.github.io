## EPPS 6354 Information Management
## Connecting DuckDB to Shiny app
## Packages: DBI, RPostgres, duckdb

library(shiny)
library(DBI)
library(duckdb)

# Define UI for application
ui <- fluidPage(
  sliderInput("nrows", "Enter the number of rows to display:",
              min = 1,
              max = 519,
              value = 15),
  # tableOutput("tbl")
  dataTableOutput("tbl")
)

# Define server logic
server <- function(input, output) {
  table <- renderDataTable({    # duckdb
    # Be sure the data file must be in same folder
    duckdb_conn <- dbConnect(duckdb::duckdb(), "nba.db",":memory:")
    
    # Create SQL commmand to join variables from tables for query
    
    duckdb_sql="SELECT p.id, p.first_name, p.last_name, p.full_name, pp.urlplayerheadshot FROM player p INNER JOIN player_photos pp ON pp.idplayer=p.id WHERE p.is_active=1"
    
    conn=duckdb_conn
    str_sql = duckdb_sql
    
    on.exit(dbDisconnect(conn), add = TRUE)
    table_df = dbGetQuery(conn, paste0(str_sql, " LIMIT ", input$nrows, ";"))
    table_df$headshot<-c(paste0('<img src="', table_df$urlPlayerHeadshot, '"></img>'))
    table_df<-table_df[c('full_name', 'headshot')]
  }, escape = FALSE)
  
  output$tbl <- table
}

# Run the application 
shinyApp(ui = ui, server = server)

