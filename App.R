library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(shinythemes)
library(DT)


# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("NyOne Dashboard"),
  
  # Add tabset panel at the top
  tabsetPanel(
    tabPanel("Dashboard",
             
             # Sidebar panel with file inputs, cards, and download buttons
             sidebarLayout(
               sidebarPanel(
                 # File input fields
                 fileInput("files", "Upload NyOne Results Files", multiple = TRUE, accept = ".csv"),
                 fileInput("metadata", "Upload Benchling Registration Tables", multiple = TRUE, accept = ".csv"),
                 actionButton("plot_data", "Load Data into App"),
                 
                 # Cards in the sidebar
                 div(class = "card",
                     div(class = "card-body",
                         h4(class = "card-title", "Select Current Campaign"),
                         selectInput("campaign_name", "Choose from list", choices = NULL),
                         p(class = "card-text", textOutput("median_confluence"))
                     )
                 ),
                 div(class = "card",
                     div(class = "card-body",
                         h4(class = "card-title", "Median Confluence of Current Campaign"),
                         numericInput("hours_since_last_run", "Hours Since Last Imaging Run", value = 0, min = 0, step = 1),
                         p(class = "card-text", textOutput("adjusted_median_confluence"))
                     )
                 ),

                 h3(textOutput("adjusted_median_confluence"), style = "color: red;"),
                 
                 # Download buttons moved underneath the cards
                 h4("Downloads"),
                 downloadButton("download_data", "Download Transformed Data"),
                 downloadButton("download_plot1", "Download Mean Plate Confluence Over Time"),
                 downloadButton("download_plot2", "Download Number of Wells with Confluence >1%")
               ),
               
               # The main panel for the dashboard content
               mainPanel(
                 # Section for plots displayed side by side within the first tab
                 fluidRow(
                   column(6,
                          h3("Mean Plate Confluence over Time +/- SD"),
                          plotOutput("plot")
                   ),
                   column(6,
                          h3("Number of Wells with Confluence >1%"),
                          plotOutput("plot2")
                   )
                 ),
                 
                 # Third plot positioned below the first plot
                 fluidRow(
                   column(12, 
                          h3("Third Plot (Example Title)"),
                          plotOutput("plot3")
                   )
                 ),
                 
                 # Optional display for metadata table and data preview
                 DTOutput("metadata_table"), # Display metadata table (optional)
                 tableOutput("preview")      # Display data preview (optional)
               )
             )
    ),
    
    # Empty second tab as a placeholder
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
    
    # Optional: If you add 'preview' to UI
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
    
    # Optional: If you add 'metadata_table' to UI
    output$metadata_table <- renderDT({
      combined_metadata <- bind_rows(metadata_list)
      datatable(combined_metadata, editable = TRUE)
    })
  })
  
  # Reactive expression for merged data
  merged_data_reactive <- reactive({
    req(file_data$data, file_data$metadata)
    
    combined_data <- bind_rows(file_data$data)
    combined_metadata <- bind_rows(file_data$metadata)
    
    merged_data <- combined_data %>%
      inner_join(combined_metadata, by = c("Plate Barcode" = "Plate Name")) %>%
      select(everything(), Passage_Number = `Passage Number`, Parent_Plate = `Parent Plate`)
    
    return(merged_data)
  })
  
  # Reactive expression for plot data
  plot_data_reactive <- reactive({
    req(merged_data_reactive())
    
    merged_data_reactive() %>%
      group_by(Passage_Number, Imaging_Identifier, Parent_Plate) %>%
      summarise(Mean = mean(`Cell Confluence (%)`, na.rm = TRUE),
                Error = sd(`Cell Confluence (%)`, na.rm = TRUE))
  })
  
  # Reactive expression for dropout data
  dropout_data_reactive <- reactive({
    req(merged_data_reactive())
    
    merged_data_reactive() %>%
      group_by(Passage_Number, Imaging_Identifier, Parent_Plate) %>%
      summarise(Count = sum(`Cell Confluence (%)` > 1, na.rm = TRUE), .groups = 'drop') %>%
      group_by(Passage_Number, Parent_Plate) %>%
      filter(Imaging_Identifier == max(Imaging_Identifier)) %>%
      ungroup()
  })
  
  # Observe plot_data button to update select input and trigger plot rendering
  observeEvent(input$plot_data, {
    updateSelectInput(session, inputId = "campaign_name", choices = unique(merged_data_reactive()$`Campaign Name`))
  })
  
  # Median Confluence Output
  output$campaign_median_confluence <- renderText({
    req(input$campaign_name)
    
    campaign_data <- merged_data_reactive() %>%
      filter(`Campaign Name` == input$campaign_name)
    
    if (nrow(campaign_data) == 0) {
      return("No data available for selected campaign")
    }
    
    highest_imaging_data <- campaign_data %>%
      filter(Imaging_Identifier == max(Imaging_Identifier, na.rm = TRUE))
    
    median_confluence <- highest_imaging_data %>%
      summarise(Median_Confluence = median(`Cell Confluence (%)`, na.rm = TRUE)) %>%
      pull(Median_Confluence)
    
    paste(median_confluence)
  })
  
  # Adjusted Median Confluence Output
  output$adjusted_median_confluence <- renderText({
    req(input$campaign_name, input$hours_since_last_run)
    
    campaign_data <- merged_data_reactive() %>%
      filter(`Campaign Name` == input$campaign_name)
    
    if (nrow(campaign_data) == 0) {
      return("No data available for selected campaign")
    }
    
    highest_imaging_data <- campaign_data %>%
      filter(Imaging_Identifier == max(Imaging_Identifier, na.rm = TRUE))
    
    median_confluence <- highest_imaging_data %>%
      summarise(Median_Confluence = median(`Cell Confluence (%)`, na.rm = TRUE)) %>%
      pull(Median_Confluence)
    
    adjusted_median_confluence <- round((median_confluence * (1 + 2)^(input$hours_since_last_run / 24)), 1)
    
    paste(adjusted_median_confluence)
  })
  
  # Render Plot 1
  output$plot <- renderPlot({
    req(plot_data_reactive())
    
    ggplot(plot_data_reactive(), aes(x = as.factor(Passage_Number), y = Mean, color = Parent_Plate, group = Imaging_Identifier)) +
      geom_point(position = position_dodge(width = 0.3)) +
      geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, position = position_dodge(width = 0.3)) +
      labs(x = "Passage Number", y = "Cell Confluence (%)", 
           title = "Plot of Cell Confluence (%) by Passage Number and Imaging Identifier") +
      theme_bw() +
      ylim(0, 100)
  })
  
  # Render Plot 2
  output$plot2 <- renderPlot({
    req(dropout_data_reactive())
    
    ggplot(dropout_data_reactive(), aes(x = as.factor(Passage_Number), y = Count, color = Parent_Plate, group = Parent_Plate)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Passage Number", y = "Number of Wells with Confluence > 1%", 
           title = "Number of Wells with Confluence > 1% by Passage Number (Highest Imaging Identifier per Parent Plate)") +
      theme_bw() +
      ylim(0, 96)
  })
  
  # Download Handler for Plot 1
  output$download_plot1 <- downloadHandler(
    filename = function() { "Mean_Confluence.png" },
    content = function(file) {
      req(plot_data_reactive())
      
      p <- ggplot(plot_data_reactive(), aes(x = as.factor(Passage_Number), y = Mean, color = Parent_Plate, group = Imaging_Identifier)) +
        geom_point(position = position_dodge(width = 0.3)) +
        geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, position = position_dodge(width = 0.3)) +
        labs(x = "Passage Number", y = "Cell Confluence (%)", 
             title = "Plot of Cell Confluence (%) by Passage Number and Imaging Identifier") +
        theme_bw() +
        ylim(0, 100)
      
      ggsave(file, p, width = 8, height = 6)
    }
  )
  
  # Download Handler for Plot 2
  output$download_plot2 <- downloadHandler(
    filename = function() { "Dropout.png" },
    content = function(file) {
      req(dropout_data_reactive())
      
      p2 <- ggplot(dropout_data_reactive(), aes(x = as.factor(Passage_Number), y = Count, color = Parent_Plate, group = Parent_Plate)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(x = "Passage Number", y = "Number of Wells with Confluence > 1%", 
             title = "Number of Wells with Confluence > 1% by Passage Number (Highest Imaging Identifier per Parent Plate)") +
        theme_bw() +
        ylim(0, 96)
      
      ggsave(file, p2, width = 8, height = 6)
    }
  )
  
  # Download transformed data (merged with metadata and imaging identifier)
  output$download_data <- downloadHandler(
    filename = function() { "transformed_data.csv" },
    content = function(file) {
      req(merged_data_reactive())
      
      merged_data_reactive() %>%
        select(everything(), Imaging_Identifier, File_Creation_Date, File_Creation_Time) %>%
        write_csv(file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
