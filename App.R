library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)  # For editable tables
library(bslib)

# Define UI
ui <- fluidPage(
  bs_themer(),
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
    file_list <- mapply(function(df, filename, filepath) {
      df$Imaging_Identifier <- stringr::str_extract(filename, "M00[0-9]")
      
      # Get file creation date and time
      file_info <- file.info(filepath)
      df$File_Creation_Date <- as.Date(file_info$ctime)
      df$File_Creation_Time <- format(file_info$ctime, "%H:%M:%S")
      
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
  }) # <-- Missing closing bracket for metadata upload observer
  
  
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

  # Calculate the mean and error (using SEM here), grouped by Passage and Imaging_Identifier
  plot_data <- merged_data %>%
    group_by(Passage_Number, Imaging_Identifier, Parent_Plate) %>%
    summarise(Mean = mean(`Cell Confluence (%)`, na.rm = TRUE),
              Error = sd(`Cell Confluence (%)`, na.rm = TRUE) / sqrt(n()))

  # Create the first plot with Imaging_Identifier nested within Passage_Number
  p <- ggplot(plot_data, aes(x = as.factor(Passage_Number), y = Mean, color = Parent_Plate, group = Imaging_Identifier)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, position = position_dodge(width = 0.3)) +
    labs(x = "Passage Number", y = "Cell Confluence (%)", 
         title = "Plot of Cell Confluence (%) by Passage Number and Imaging Identifier") +
    theme_minimal() +
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
  theme_minimal()

output$plot2 <- renderPlot({ p2 })
  
  # Save the first plot for download
  output$download_plot <- downloadHandler(
    filename = function() { "plot.png" },
    content = function(file) {
      ggsave(file, p, width = 8, height = 6)
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
