library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)  # For editable tables

# Define UI
ui <- fluidPage(
  titlePanel("CSV Upload and Plot App"),

  tabsetPanel(
    tabPanel("View Confluence and Dropout",
      sidebarLayout(
        sidebarPanel(
          fileInput("files", "Upload NyOne Results Files", multiple = TRUE, accept = ".csv"),
          fileInput("metadata", "Upload Benchling Registration Tables", multiple = TRUE, accept = ".csv"),
          checkboxInput("filter_by_plate", "Filter by Source Plate ID", value = FALSE),
          downloadButton("download_data", "Download Transformed Data"),
          downloadButton("download_plot", "Download Plot"),
          actionButton("plot_data", "Generate Plot")
        ),
        
        mainPanel(
          # Section for the plot (separate from collapsible sections)
          h3("Mean Confluence +/-SEM"),
          plotOutput("plot"),
          # Section for second plot (dropout plot)
          h3("Number of Wells with Confluence >1%"),
          plotOutput("plot2")
        )
      )
    ),
    tabPanel("Navbar 2", "This panel is intentionally left blank")
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Store the uploaded imaging data and metadata
  file_data <- reactiveValues(data = NULL, metadata = NULL)
  
  # Reactive to upload imaging data files
  observeEvent(input$files, {
    file_list <- lapply(input$files$datapath, read_csv)
    names(file_list) <- input$files$name
    
    # Extract the "M000", "M001", etc., from filenames and add as a new column
    file_list <- mapply(function(df, filename) {
      df$Imaging_Identifier <- stringr::str_extract(filename, "M00[0-9]")
      return(df)
    }, file_list, input$files$name, SIMPLIFY = FALSE)
    
    file_data$data <- file_list
    
    # Show a preview of the first few rows of one file as a table
    output$preview <- renderTable({
      head(file_data$data[[1]]) # Show the first few rows of one file as a preview
    })
  })
  
  # Reactive to upload multiple metadata (Benchling Registration Tables) CSV files
  observeEvent(input$metadata, {
    req(input$metadata)
    metadata_list <- lapply(input$metadata$datapath, read_csv)
    names(metadata_list) <- input$metadata$name
    file_data$metadata <- metadata_list
    
    # Display the metadata tables
    output$metadata_table <- renderDT({
      combined_metadata <- bind_rows(metadata_list)
      datatable(combined_metadata, editable = TRUE)
    })
  }) # <-- Missing closing bracket for metadata upload observer
  
  # Join metadata with imaging data and generate the plot
  observeEvent(input$plot_data, {
    req(file_data$data, file_data$metadata)
    
    # Combine all the imaging data into one dataframe
    combined_data <- bind_rows(file_data$data)
    
    # Combine all metadata into one dataframe
    combined_metadata <- bind_rows(file_data$metadata)
    
    # Join with metadata based on "Plate Barcode" in ima
