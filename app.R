#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load libraries for building the Shiny app and data visualization
library(shiny)         # Core Shiny package for building web apps
library(tidyverse)     # Includes dplyr, ggplot2, tidyr for manipulating data and plotting
library(broom)         # Used to tidy up model outputs (e.x., regression results)
library(plotly)        # Converts ggplot objects into interactive plots
library(shinythemes)   # Provides preset UI themes for cleaner and sleek look

# UI definition: this sets up the layout and navigation of the app
ui <- dashboardPage(
  skin = "blue",  # Set the overall skin color of the dashboard - it makes it pretty
  
  dashboardHeader(title = "Hair Data App"),  # Title put at the top of the app
  
  # Sidebar with dropdown menu that uses different icons
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home / Graphs", tabName = "main", icon = icon("chart-bar")),        # First tab: plots and controls
      menuItem("Glossary", tabName = "glossary", icon = icon("book")),              # Second tab: glossary definitions
      menuItem("Help / More Info", tabName = "help", icon = icon("info-circle"))    # Third tab: app instructions
    )
  ),
  
  # Main content area where each tab is shoewd based on what you select
  dashboardBody(
    tabItems(
      
      # Tab for uploading data and viewing plots
      tabItem(tabName = "main",
              fluidRow(
                
                # First column: inputs and checkboxes
                box(width = 4, status = "primary", solidHeader = TRUE,
                    title = "Upload & Controls",
                    
                    # Upload CSV file that contains the dataset
                    fileInput("file", "Upload CSV", accept = ".csv"),
                    
                    # Dropdown to choose which type of graph you want to display
                    selectInput("plot_type", "Select a plot:",
                                choices = c("Boxplot by Region" = "boxplot",
                                            "Violin Plot by Diagnosis" = "violin",
                                            "Sinclair by Region (Faceted)" = "faceted_line",
                                            "Sinclair vs Hair Thickness" = "scatter",
                                            "Residuals Plot" = "residuals",
                                            "Diagnosis Frequency Bar" = "bar")),
                    
                    # Checkbox to add a trendline to scatter plot
                    checkboxInput("show_trendline", "Show trendline (for scatter)", TRUE),
                    
                    # Checkbox to show summary statistics
                    checkboxInput("show_summary", "Show summary statistics", FALSE)
                ),
                
                # Second column: shows description, interactive plot, and stats
                box(width = 8, status = "info", solidHeader = TRUE,
                    title = "Plot Viewer",
                    uiOutput("plot_description"),        # text explaining the graph
                    plotlyOutput("main_plot"),           # Interactive plot output
                    
                    # Shows summary stats only if checkbox is checked
                    conditionalPanel(
                      condition = "input.show_summary == true",
                      verbatimTextOutput("summary_stats")
                    )
                )
              )
      ),
      
      # Glossary tab explains terms used in the dataset and visualizations
      tabItem(tabName = "glossary",
              fluidRow(
                box(width = 12, title = "Glossary of Terms", status = "primary",
                    tags$ul(
                      tags$li(strong("Sinclair Score:"), " A clinical scale used to evaluate the degree of hair loss in females across different scalp regions."),
                      tags$li(strong("Hair Count (Hair_Num_*):"), " The number of hairs counted in a certain region of the scalp."),
                      tags$li(strong("Hair Thickness (Hair_Thick_*):"), " Average thickness of hair measured in a certain region."),
                      tags$li(strong("Diagnosis:"), " The clinical diagnosis of the patient, for example; Androgenetic Alopecia (AGA), Telogen Effluvium (TE)."),
                      tags$li(strong("Residuals Plot:"), " A graph showing differences between observed and predicted values to assess model fit.")
                    )
                )
              )
      ),
      
      # Help tab gives directions on how to use the app
      tabItem(tabName = "help",
              fluidRow(
                box(width = 12, title = "Help & More Info", status = "primary",
                    p("Welcome to the Hair Data Visualization App. This app allows you to upload a CSV file and explore your dataset."),
                    p("Use the left-hand sidebar menu to switch between graph views, the glossary, and help information."),
                    p("Each plot provides a description, and you can optionally view summary statistics using the checkbox.")
                )
              )
      )
    )
  )
)

# this controls data processing and what gets shown in the UI
server <- function(input, output) {
  
  # expression to read and clean uploaded CSV file
  data_clean <- reactive({
    req(input$file)  # this makes you wait for the file to upload before proceeding
    
    df <- read.csv(input$file$datapath, header = TRUE)  # Read the file that was uploaded
    
    # Clean the dataset: remove missing IDs and drop a row (non-data row)
    df <- df %>%
      filter(SampleID != "" & !is.na(SampleID)) %>%
      slice(-9) %>%  # Remove row 9 manually
      
      # Convert relevant columns from a character to a numerical value
      mutate(across(starts_with("Hair_"), ~ as.numeric(as.character(.))),
             across(starts_with("Sinclair_"), ~ as.numeric(as.character(.))),
             across(starts_with("Single_Per_"), ~ as.numeric(as.character(.))))
    return(df)
  })
  
  # display a short description for each selected plot
  output$plot_description <- renderUI({
    desc <- switch(input$plot_type,
                   "boxplot" = "This boxplot visualizes the distribution of hair counts across different scalp regions. It helps identify regions with higher or lower hair density and see potential outliers.",
                   "violin" = "The violin plot shows the distribution of hair counts for each region, categorized by diagnosis. It combines a boxplot and a density plot, offering insight into data spread and group differences.",
                   "faceted_line" = "This faceted line plot displays Sinclair scores across scalp regions for individual subjects, grouped by diagnosis. It allows for comparison of scalp patterns and how they vary across diagnoses.I had this graph in the midterm however, I changed it a little to make it easier to understand.",
                   "scatter" = "This scatter plot compares Sinclair score and hair thickness on the vertex. It is useful to assess correlation or trends, especially when a regression line is shown.This was a visualization I did for the midterm. On the midtem I plotted it with the sinclair score on the y-axis but here I plotted it with the sinclair score on the x-axis.",
                   "residuals" = "The residuals plot helps evaluate the fit of a linear model predicting hair count from Sinclair scores. Patterns in residuals can indicate non-linearity or outliers.",
                   "bar" = "This bar chart summarizes the frequency of each diagnosis in the dataset. It helps understand how balanced or skewed the sample is. I had this graph in the midterm project, I thought it was a good way to see the amount of people who had which diagnosis."
    )
    HTML(paste("<b>Plot Description:</b><br>", desc)) #formats and returns HTML text, shows bold heading
  })
  
  # makes the selected plot an interactive Plotly object type thing
  output$main_plot <- renderPlotly({
    df <- data_clean()  # Get cleaned data to use the clean data
    
    # Generate different plot types depending on what you select in the drop down menu on plots tab
    if (input$plot_type == "boxplot") {
      hair_long <- df %>% pivot_longer(cols = starts_with("Hair_Num_"), names_to = "Region", values_to = "Hair_Count")
      p <- ggplot(hair_long, aes(x = Region, y = Hair_Count, fill = Region)) + 
        geom_boxplot() + 
        theme_minimal() + 
        labs(title = "Hair Count Distribution by Region")
      
    } else if (input$plot_type == "violin") {
      hair_long <- df %>% pivot_longer(cols = starts_with("Hair_Num_"), names_to = "Region", values_to = "Hair_Count")
      p <- ggplot(hair_long, aes(x = Region, y = Hair_Count, fill = Diagnosis)) +
        geom_violin(trim = FALSE) + 
        theme_minimal() + 
        labs(title = "Violin Plot: Hair Count by Region and Diagnosis")
      
    } else if (input$plot_type == "faceted_line") {
      sinclair_long <- df %>% pivot_longer(cols = starts_with("Sinclair_"), names_to = "Scalp_Region", values_to = "Sinclair_Score")
      p <- ggplot(sinclair_long, aes(x = Scalp_Region, y = Sinclair_Score, group = SampleID)) +
        geom_line(alpha = 0.5) + 
        facet_wrap(~ Diagnosis) + theme_minimal() + 
        labs(title = "Sinclair Scores by Region")
      
    } else if (input$plot_type == "scatter") {
      p <- ggplot(df, aes(x = Sinclair_Vert, y = Hair_Thick_Vert, color = Diagnosis)) +
        geom_point(size = 2, alpha = 0.7) + 
        theme_minimal() + 
        labs(title = "Sinclair Score vs Hair Thickness") + 
        ylim(0,90)
      if (input$show_trendline) p <- p + 
          geom_smooth(method = "lm", se = TRUE, color = "black") 
      
    } else if (input$plot_type == "residuals") {
      model <- lm(Hair_Num_Vert ~ Sinclair_Vert, data = df)
      model_data <- augment(model)
      p <- ggplot(model_data, aes(x = .fitted, y = .resid)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        theme_minimal() +
        labs(title = "Model Residuals")
      
    } else if (input$plot_type == "bar") {
      p <- ggplot(df, aes(x = Diagnosis, fill = Diagnosis)) +
        geom_bar() + 
        theme_minimal() + 
        labs(title = "Diagnosis Frequency") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggplotly(p)  # Convert ggplot to interactive plot
  })
  
  # summary statistics showed depending on the plot selected
  output$summary_stats <- renderPrint({
    df <- data_clean()
    if (input$plot_type == "boxplot" || input$plot_type == "violin") {
      hair_long <- df %>% pivot_longer(cols = starts_with("Hair_Num_"), names_to = "Region", values_to = "Hair_Count")
      summary(hair_long)  # Descriptive statistics for hair counts
      
    } else if (input$plot_type == "scatter") {
      summary(df[, c("Sinclair_Vert", "Hair_Thick_Vert")])  # Basic correlation data
      
    } else if (input$plot_type == "residuals") {
      model <- lm(Hair_Num_Vert ~ Sinclair_Vert, data = df)
      summary(model)  # Regression output
      
    } else if (input$plot_type == "bar") {
      table(df$Diagnosis)  # Diagnosis counts
      
    } else if (input$plot_type == "faceted_line") {
      sinclair_long <- df %>% pivot_longer(cols = starts_with("Sinclair_"), names_to = "Scalp_Region", values_to = "Sinclair_Score")
      summary(sinclair_long)  # Statistics on Sinclair scores by region
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
