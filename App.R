library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)  # For editable tables
library(shinythemes)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("NyOne Dashboard"),
  
  # Add cards across the top
  fluidRow(
    column(4,
           div(class = "card",
               div(class = "card-body",
                   h4(class = "card-title", "Select Current Campaign"),
                   selectInput("campaign_name", "Choose from list", choices = NULL),
                   numericInput("hours_since_last_run", "Hours Since Last Imaging Run", value = 0, min = 0, step = 1),
                   p(class = "card-text", textOutput("median_confluence"))
               )
           )
    ),
    column(4,
           div(class = "card",
               div(class = "card-body",
                   h4(class = "card-title", "Median Confluence of the Campaign"),
                   p(class = "card-text", textOutput("campaign_median_confluence")),
                   p(class = "card-text", textOutput("adjusted_median_confluence"))
               ))
    ),
    column(4,
           div(class = "card",
               div(class = "card-body",
                   h4(class = "card-title", "Optimal Passaging Day (AM/PM)"),
                   p(class = "card-text", "This is some text for card 3.")))
    )
  ),
  
  tabsetPanel(
    tabPanel("View Confluence and Dropout",
             sidebarLayout(
               sidebarPanel(
                 fileInput("files", "Upload NyOne Results Files", multiple = TRUE, accept = ".csv"),
                 fileInput("metadata", "Upload Benchling Registration Tables", multiple = TRUE, accept = ".csv"),
                 actionButton("plot_data", "Load Data into App"),
                 downloadButton("download_data", "Download Transformed Data"),
                 downloadButton("download_plot1", "Download Confluence Plot"),
                 downloadButton("download_plot2", "Download Test Dropout")
               ),
               
               mainPanel(
                 # Section for plots displayed side by side
                 fluidRow(
                   column(6,
                          h3("Mean Confluence +/- SD"),
                          plotOutput("plot")
                   ),
                   column(6,
                          h3("Number of Wells with Confluence >1%"),
                          plotOutput("plot2")
                   )
                 )
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
    file_list <- mapply(function(df, filename, filepath) {
      df$Imaging_Identifier <- stringr::str_extract(filename, "M00[0-9]")
      
      return(df)
    }, file_list, input$files$name, input$files$datapath, SIMPLIFY = FALSE)
    
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
  })
  
  # Join metadata with imaging data and generate the plot
  observeEvent(input$plot_data, {
    req(file_data$data, file_data$metadata)
    
    # Combine all the imaging data into one dataframe
    combined_data <- bind_rows(file_data$data)
    
    # Combine all metadata into one dataframe
    combined_metadata <- bind_rows(file_data$metadata)
    
    # Join with metadata based on "Plate Barcode" in imaging data and "Plate Name" in metadata
    merged_data <- combined_data %>%
      inner_join(combined_metadata, by = c("Plate Barcode" = "Plate Name")) %>%
      select(everything(), Passage_Number = `Passage Number`, Parent_Plate = `Parent Plate`)  # Add Passage Number and Parent Plate
    
    #Rob Testing dropdown options
    updateSelectInput(session, inputId = "campaign_name", choices = unique(merged_data$`Campaign Name`))
    
    # Calculate the median confluence for Card 2
    output$campaign_median_confluence <- renderText({
      req(input$campaign_name, file_data$data, file_data$metadata)
      
      # Combine all the imaging data into one dataframe
      combined_data <- bind_rows(file_data$data)
      
      # Combine all metadata into one dataframe
      combined_metadata <- bind_rows(file_data$metadata)
      
      # Join with metadata based on "Plate Barcode" in imaging data and "Plate Name" in metadata
      merged_data <- combined_data %>%
        inner_join(combined_metadata, by = c("Plate Barcode" = "Plate Name")) %>%
        select(everything(), Passage_Number = `Passage Number`, Parent_Plate = `Parent Plate`)
      
      # Filter by selected campaign name
      campaign_data <- merged_data %>%
        filter(`Campaign Name` == input$campaign_name)
      
      if (nrow(campaign_data) == 0) {
        return("No data available for selected campaign")
      }
      
      # Get the data with the highest imaging identifier
      highest_imaging_data <- campaign_data %>%
        filter(Imaging_Identifier == max(Imaging_Identifier, na.rm = TRUE))
      
      # Calculate median confluence
      median_confluence <- highest_imaging_data %>%
        summarise(Median_Confluence = median(`Cell Confluence (%)`, na.rm = TRUE)) %>%
        pull(Median_Confluence)
      
      paste(median_confluence)
    })
    
    # Calculate adjusted median confluence
    output$adjusted_median_confluence <- renderText({
      req(input$campaign_name, input$hours_since_last_run, file_data$data, file_data$metadata)
      
      # Combine all the imaging data into one dataframe
      combined_data <- bind_rows(file_data$data)
      
      # Combine all metadata into one dataframe
      combined_metadata <- bind_rows(file_data$metadata)
      
      # Join with metadata based on "Plate Barcode" in imaging data and "Plate Name" in metadata
      merged_data <- combined_data %>%
        inner_join(combined_metadata, by = c("Plate Barcode" = "Plate Name")) %>%
        select(everything(), Passage_Number = `Passage Number`, Parent_Plate = `Parent Plate`)
      
      # Filter by selected campaign name
      campaign_data <- merged_data %>%
        filter(`Campaign Name` == input$campaign_name)
      
      if (nrow(campaign_data) == 0) {
        return("No data available for selected campaign")
      }
      
      # Get the data with the highest imaging identifier
      highest_imaging_data <- campaign_data %>%
        filter(Imaging_Identifier == max(Imaging_Identifier, na.rm = TRUE))
      
      # Calculate median confluence
      median_confluence <- highest_imaging_data %>%
        summarise(Median_Confluence = median(`Cell Confluence (%)`, na.rm = TRUE)) %>%
        pull(Median_Confluence)
      
      # Calculate adjusted median confluence
      adjusted_median_confluence <- median_confluence * (1+2)^(input$hours_since_last_run / 24)
      
      paste(adjusted_median_confluence)
    })
    
    # Calculate the mean and standard deviation, grouped by Passage and Imaging_Identifier
    plot_data <- merged_data %>%
      group_by(Passage_Number, Imaging_Identifier, Parent_Plate) %>%
      summarise(Mean = mean(`Cell Confluence (%)`, na.rm = TRUE),
                Error = sd(`Cell Confluence (%)`, na.rm = TRUE))
    
    # Create the first plot with Imaging_Identifier nested within Passage_Number
    p <- ggplot(plot_data, aes(x = as.factor(Passage_Number), y = Mean, color = Parent_Plate, group = Imaging_Identifier)) +
      geom_point(position = position_dodge(width = 0.3)) +
      geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, position = position_dodge(width = 0.3)) +
      labs(x = "Passage Number", y = "Cell Confluence (%)", 
           title = "Plot of Cell Confluence (%) by Passage Number and Imaging Identifier") +
      theme_bw() +
      ylim(0, 100)  # Fixed y-axis range from 0 to 100
    
    output$plot <- renderPlot({ p })
    
    # Create the second plot for dropout rate as a line graph
    dropout_data <- merged_data %>%
      group_by(Passage_Number, Imaging_Identifier, Parent_Plate) %>%
      summarise(Count = sum(`Cell Confluence (%)` > 1, na.rm = TRUE), .groups = 'drop') %>%
      # For each Passage_Number and Parent_Plate, keep only the row with the highest Imaging_Identifier
      group_by(Passage_Number, Parent_Plate) %>%
      filter(Imaging_Identifier == max(Imaging_Identifier)) %>%
      ungroup()
    
    # Create a line graph for dropout data
    p2 <- ggplot(dropout_data, aes(x = as.factor(Passage_Number), y = Count, color = Parent_Plate, group = Parent_Plate)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Passage Number", y = "Number of Wells with Confluence > 1%", 
           title = "Number of Wells with Confluence > 1% by Passage Number (Highest Imaging Identifier per Parent Plate)") +
      theme_bw() +
      ylim(0,96)
    
    output$plot2 <- renderPlot({ p2 })
    
    # Save the first plot for download
    output$download_plot1 <- downloadHandler(
      filename = function() { "Mean Confluence.png" },
      content = function(file) {
        ggsave(file, p, width = 8, height = 6)
      }
    )
    # Save the second plot for download
    output$download_plot2 <- downloadHandler(
      filename = function() { "Dropout.png" },
      content = function(file) {
        ggsave(file, p2, width = 8, height = 6)
      }
    )
  })
  
  # Download transformed data (merged with metadata and imaging identifier)
  output$download_data <- downloadHandler(
    filename = function() { "transformed_data.csv" },
    content = function(file) {
      combined_data <- bind_rows(file_data$data)
      combined_metadata <- bind_rows(file_data$metadata)
      merged_data <- combined_data %>%
        left_join(combined_metadata, by = c("Plate Barcode" = "Plate Name")) %>%
        select(everything(), Passage_Number = `Passage Number`, Parent_Plate = `Parent Plate`, Imaging_Identifier, File_Creation_Date, File_Creation_Time)  # Add Passage Number, Parent Plate, Imaging Identifier, File Creation Date, and File Creation Time
      write_csv(merged_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
