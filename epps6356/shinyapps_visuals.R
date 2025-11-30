# Load required libraries
library(shiny)
library(tidyverse)
library(ggrepel)

uspop_df <- data.frame(
  Year = as.numeric(time(uspop)),
  Population_Millions = as.numeric(uspop) / 1000 # Population is in thousands, converting to millions for readability
)

plot_height <- "800px"
# --- Shiny UI Definition (using navbarPage for clear separation) ---
ui <- navbarPage(
  "R Dataset Visualizations",
  # --- Tab 1: mtcars Histogram ---
  tabPanel("MTCARS: MPG Histogram",
           sidebarLayout(
             sidebarPanel(
               h4("Adjust Histogram Parameters"),
               p("Visualize the distribution of Miles Per Gallon (MPG) in the mtcars dataset."),
               
               # Input for histogram bin width
               sliderInput("bin_width", "Bin Width:",
                           min = 1, max = 5, value = 3, step = 0.5)
             ),
             mainPanel(
               plotOutput("mpg_hist", height = plot_height)
             )
           )
  ),
  
  # --- Tab 2: USArrests Scatterplot ---
  tabPanel("USARRESTS: Murder vs. UrbanPop",
           fluidRow(
             column(12, 
                    h3("Relationship between Murder Rate and Urban Population"),
                    p("Each point represents a US state. This plot helps visualize if states with higher urban populations also experience higher murder rates."),
                    plotOutput("arrests_scatter", height = plot_height)
             )
           )
  ),
  
  # --- Tab 3: USPOP Line Plot ---
  tabPanel("USPOP: Population Over Time",
           fluidRow(
             column(12, 
                    h3("US Population (1790–1970)"),
                    p("Shows the historical US population data, recorded every decade."),
                    plotOutput("uspop_line", height = plot_height)
             )
           )
  )
)

# --- Shiny Server Logic ---
server <- function(input, output) {
  
  # --- Plot 1: mtcars Histogram (Interactive) ---
  output$mpg_hist <- renderPlot({
    ggplot(mtcars, aes(x = mpg)) +
      geom_histogram(
        binwidth = input$bin_width,
        fill = "#457B9D", 
        color = "white",
        alpha = 0.8
      ) +
      labs(
        title = paste("Distribution of MPG (Bin Width:", input$bin_width, ")"),
        x = "Miles Per Gallon (mpg)",
        y = "Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # --- Plot 2: USArrests Scatterplot (Static) ---
  output$arrests_scatter <- renderPlot({
    # Use state names as labels
    USArrests_df <- USArrests %>%
      rownames_to_column(var = "State")
    
    ggplot(USArrests_df, aes(x = UrbanPop, y = Murder, label = State)) +
      geom_point(aes(color = Assault), size = 6) +
      geom_text_repel(size = 6, color = "gray30", max.overlaps = 20) + 
      scale_color_gradient(low = "#E63946", high = "#1D3557") + # Color by Assault rate
      labs(
        title = "Murder Rate vs. % Urban Population",
        subtitle = "Points colored by Assault Rate",
        x = "% Urban Population",
        y = "Murder Arrests (per 100,000 residents)",
        color = "Assault Rate"
      ) +
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # --- Plot 3: uspop Line Plot (Static) ---
  output$uspop_line <- renderPlot({
    ggplot(uspop_df, aes(x = Year, y = Population_Millions)) +
      geom_line(color = "#2A9D8F", linewidth = 1.5) +
      geom_point(color = "#2A9D8F", size = 6) +
      labs(
        title = "Historical US Population Growth",
        x = "Year (Decade)",
        y = "Population (Millions)"
      ) +
      scale_x_continuous(breaks = seq(min(uspop_df$Year), max(uspop_df$Year), by = 20)) +
      theme_light(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)