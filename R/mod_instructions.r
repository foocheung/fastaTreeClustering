# R/mod_instructions.R
#' Instructions UI Function
#'
#' @description A shiny Module providing instructions for using the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_instructions_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             h2("User Instructions: Interactive FASTA Tree Clustering Tool"),
             p("This tool allows you to upload a FASTA file or use internal data, build a sequence distance tree, and cluster sequences interactively."),
             
             h3("Getting Started"),
             tags$ul(
               tags$li("Choose either 'Upload FASTA File' or 'Use Internal Data' as your data source."),
               tags$li("If uploading, select your FASTA file using the 'Upload FASTA File' button."),
               tags$li("Click 'Process Data' to start building the tree."),
               tags$li("Light chain files take ~3 minutes, Heavy chain files take ~5 minutes."),
               tags$li("After processing, tree cutting and re-clustering are instant."),
               tags$li("Only change the Distance Method if you want to reprocess (~3–5 minutes).")
             ),
             
             h3("Clustering Methods"),
             tags$ul(
               tags$li("Linear Cutting: divides sequences evenly into bins."),
               tags$li("Dynamic Tree Cut: automatically detects clusters based on tree structure."),
               tags$li("Height-based Cutting: allows you to cut the tree at a specific height.")
             ),
             
             h3("Important Tips"),
             tags$ul(
               tags$li("Tree cutting, binning, and plotting are instant after initial processing."),
               tags$li("Changing the Distance Method will trigger full reprocessing."),
               tags$li("Use the plot width/height sliders to adjust plot size."),
               tags$li("Download your cluster metadata and plots using the buttons at the bottom.")
             ),
             
             h3("Summary"),
             tags$table(
               tags$thead(
                 tags$tr(
                   tags$th("Action"), tags$th("Behavior")
                 )
               ),
               tags$tbody(
                 tags$tr(tags$td("Process Data"), tags$td("Takes ~3–5 min once")),
                 tags$tr(tags$td("Change Clustering Parameters"), tags$td("Instantaneous")),
                 tags$tr(tags$td("Change Distance Method"), tags$td("Triggers full reprocessing"))
               )
             )
      )
    )
  )
}

#' Instructions Server Function
#'
#' @noRd 
mod_instructions_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # No server-side functionality needed for instructions
  })
}
