# R/mod_clustering.R

#' Clustering UI Function
#'
#' @description A shiny Module providing clustering functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_clustering_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("tree_plot_ui")),
    conditionalPanel(condition = "input.show_group_plot == true", 
                     uiOutput(ns("group_plot_ui")), 
                     ns = ns),
    conditionalPanel(condition = "input.show_histogram == true", 
                     uiOutput(ns("histogram_plot_ui")), 
                     ns = ns)
  )
}

#' Clustering Server Function
#'
#' @noRd
mod_clustering_server <- function(id, rv, input_main){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- Dynamic UI Elements ---
    output$tree_plot_ui <- renderUI({
      req(rv$ready)
      plotOutput(ns("tree_plot"), 
                 height = paste0(input_main$plot_height, "px"), 
                 width = paste0(input_main$plot_width, "px"))
    })
    
    output$group_plot_ui <- renderUI({
      req(rv$ready)
      plotOutput(ns("group_plot"), 
                 height = paste0(input_main$plot_height, "px"), 
                 width = paste0(input_main$plot_width, "px"))
    })
    
    output$histogram_plot_ui <- renderUI({
      req(rv$ready)
      plotOutput(ns("histogram_plot"), 
                 height = paste0(input_main$plot_height, "px"), 
                 width = paste0(input_main$plot_width, "px"))
    })
    
    # --- Clustering results based on method ---
    clustering_results <- reactive({
      req(rv$ready)
      
      if(input_main$cluster_method == "linear") {
        get_linear_bins(rv$hc, rv$df, input_main$num_bins)
      } else if(input_main$cluster_method == "dynamic") {
        get_dynamic_clusters(rv$hc, rv$df, rv$dist_matrix, input_main$deepSplit, input_main$minClusterSize)
      } else if(input_main$cluster_method == "height") {
        req(input_main$cut_height)
        get_height_clusters(rv$hc, rv$df, input_main$cut_height)
      }
    })
    
    # --- Render Tree Plot ---
    output$tree_plot <- renderPlot({
      req(rv$ready)
      
      if(input_main$cluster_method == "linear") {
        plot_linear_tree(rv$hc, clustering_results(), input_main$num_bins, input_main$distance_method)
      } else if(input_main$cluster_method == "dynamic") {
        plot_dynamic_tree(
          rv$hc, rv$dend, rv$df, clustering_results(), 
          input_main$distance_method, input_main$deepSplit, input_main$minClusterSize,
          input_main$show_rectangles, input_main$show_bin_numbers, 
          input_main$color_tip_text, input_main$show_bottom_labels
        )
      } else if(input_main$cluster_method == "height") {
        plot_height_tree(
          rv$hc, rv$dend, rv$df, clustering_results(), 
          input_main$distance_method, input_main$cut_height,
          input_main$show_height_line, input_main$color_height_clusters, input_main$show_height_rectangles
        )
      }
    })
    
    # --- Group Composition Plot ---
    output$group_plot <- renderPlot({
      req(rv$ready)
      plot_group_composition(clustering_results(), input_main$cluster_method)
    })
    
    # --- Bin/Cluster Size Histogram ---
    output$histogram_plot <- renderPlot({
      req(rv$ready)
      if(input_main$cluster_method == "height") {
        title <- paste0("Cluster Size Distribution at h=", round(input_main$cut_height, 2))
      } else if(input_main$cluster_method == "linear") {
        title <- paste0("Bin Size Distribution with ", input_main$num_bins, " bins")
      } else {
        title <- paste0("Dynamic Cluster Size Distribution (deepSplit=", input_main$deepSplit,
                       ", minSize=", input_main$minClusterSize, ")")
      }
      
      plot_histogram(clustering_results(), input_main$cluster_method, title)
    })
    
    # Return the clustering results for use elsewhere
    return(clustering_results)
  })
}
