# R/app_ui.R

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      titlePanel("Interactive FASTA Tree Clustering"),
      sidebarLayout(
        sidebarPanel(
          # Data selection method
          radioButtons("data_source", "Select Data Source:",
                       choices = c("Upload FASTA File" = "upload",
                                   "Use Internal Data" = "internal"),
                       selected = "upload"),
          # Only show file upload when upload is selected
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput("upload_fasta", "Upload FASTA File", accept = ".fasta")
          ),
          # Only show internal data options when internal is selected
          conditionalPanel(
            condition = "input.data_source == 'internal'",
            helpText("Using internal data: www/PosNegFiltered_Light_JunctionAA.fasta")
          ),
          actionButton("process_btn", "Process Data"),
          br(), br(),
          verbatimTextOutput("status_text", placeholder = TRUE),
          # Clustering method selection
          radioButtons("cluster_method", "Clustering Method:",
                       choices = c("Linear Cutting" = "linear",
                                   "Dynamic Tree Cut" = "dynamic",
                                   "Height-based Cutting" = "height"),
                       selected = "linear"),
          # Distance method selector (for all methods)
          selectInput("distance_method",
                      "Distance Method:",
                      choices = c("Levenshtein" = "lv",
                                  "Damerau-Levenshtein" = "dl",
                                  "Longest Common Substring" = "lcs",
                                  "Optimal String Alignment" = "osa",
                                  "q-gram" = "qgram",
                                  "Cosine" = "cosine",
                                  "Jaccard" = "jaccard",
                                  "Jaro-Winkler" = "jw"),
                      selected = "lv"),
          # Linear Cut specific parameters
          conditionalPanel(
            condition = "input.cluster_method == 'linear'",
            sliderInput("num_bins",
                        "Select Number of Bins:",
                        min = 5, max = 100, value = 20, step = 1)
          ),
          # Height-based Cutting parameters
          conditionalPanel(
            condition = "input.cluster_method == 'height'",
            uiOutput("height_slider_ui"),
            checkboxInput("show_height_line", "Show Cut Height Line", value = TRUE),
            checkboxInput("color_height_clusters", "Color Tips by Cluster", value = TRUE),
            checkboxInput("show_height_rectangles", "Show Cluster Rectangles", value = TRUE)
          ),
          # Dynamic Cut specific parameters
          conditionalPanel(
            condition = "input.cluster_method == 'dynamic'",
            sliderInput("deepSplit",
                        "deepSplit (0=conservative, 4=aggressive):",
                        min = 0, max = 4, value = 2, step = 1),
            numericInput("minClusterSize",
                         "Minimum Cluster Size:",
                         value = 20, min = 1, step = 1),
            checkboxInput("show_rectangles", "Show Red Cluster Rectangles", value = TRUE),
            checkboxInput("show_bin_numbers", "Show Cluster Bin Numbers", value = TRUE),
            checkboxInput("color_tip_text", "Color Tip Labels by Cluster", value = TRUE),
            checkboxInput("show_bottom_labels", "Show Bottom Black Text Labels", value = TRUE)
          ),
          # Common visualization options
          checkboxInput("show_group_plot", "Show Group Composition Bar Chart", value = TRUE),
          checkboxInput("show_histogram", "Show Histogram Plot", value = TRUE),
          # Plot size controls
          sliderInput("plot_width", "Plot Width (pixels):", min = 800, max = 15000, value = 1300, step = 100),
          sliderInput("plot_height", "Plot Height (pixels):", min = 200, max = 2000, value = 400, step = 100),
          # Download buttons
          downloadButton("download_data", "Download Cluster Metadata"),
          downloadButton("download_plot", "Download Tree Plot"),
          downloadButton("download_group", "Download Group Plot"),
          downloadButton("download_histogram", "Download Histogram Plot"),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Clustering",
                     uiOutput("tree_plot_ui"),
                     conditionalPanel(condition = "input.show_group_plot == true", uiOutput("group_plot_ui")),
                     conditionalPanel(condition = "input.show_histogram == true", uiOutput("histogram_plot_ui"))
            ),
            tabPanel("Instructions", mod_instructions_ui("instructions_ui_1"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fastaTreeClustering'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}
