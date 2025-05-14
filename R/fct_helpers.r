# R/fct_helpers.R

#' Get linear bins
#'
#' This function creates linear bins from hierarchical clustering
#'
#' @param hc Hierarchical clustering object
#' @param df Data frame with sequence data
#' @param num_bins Number of bins to create
#'
#' @return List with binning results
#' @importFrom dplyr left_join
#' @noRd
get_linear_bins <- function(hc, df, num_bins) {
  tip_order <- hc$labels[hc$order]
  n_tips <- length(tip_order)
  bin_assignments <- cut(seq_along(tip_order), breaks = num_bins, labels = FALSE)
  tip_bin_df <- data.frame(Tip = tip_order, Cluster = bin_assignments)
  df$Tip <- df$header
  df_merged <- left_join(tip_bin_df, df, by = "Tip")
  
  list(
    df_merged = df_merged,
    tip_order = tip_order,
    bin_assignments = bin_assignments,
    n_tips = n_tips
  )
}

#' Get dynamic clusters
#'
#' This function performs dynamic tree cutting
#'
#' @param hc Hierarchical clustering object
#' @param df Data frame with sequence data
#' @param dist_matrix Distance matrix for sequences
#' @param deepSplit Deep split parameter for dynamic tree cut
#' @param minClusterSize Minimum cluster size
#'
#' @return List with clustering results
#' @importFrom dynamicTreeCut cutreeDynamic
#' @importFrom dplyr left_join
#' @noRd
get_dynamic_clusters <- function(hc, df, dist_matrix, deepSplit, minClusterSize) {
  clusters <- cutreeDynamic(
    dendro = hc,
    distM = as.matrix(dist_matrix),
    deepSplit = deepSplit,
    minClusterSize = minClusterSize,
    pamRespectsDendro = TRUE
  )
  
  # Combine cluster info with df
  df_temp <- data.frame(
    Header = df$header,
    Cluster = clusters
  )
  df_merged <- left_join(df_temp, df, by = c("Header" = "header"))
  
  list(
    df_merged = df_merged,
    clusters = clusters
  )
}

#' Get height-based clusters
#'
#' This function performs height-based tree cutting
#'
#' @param hc Hierarchical clustering object
#' @param df Data frame with sequence data
#' @param cut_height Height at which to cut the tree
#'
#' @return List with clustering results
#' @importFrom dplyr left_join
#' @noRd
get_height_clusters <- function(hc, df, cut_height) {
  clusters <- cutree(hc, h = cut_height)
  
  # Combine cluster info with df
  df_temp <- data.frame(
    Header = df$header,
    Cluster = clusters
  )
  df_merged <- left_join(df_temp, df, by = c("Header" = "header"))
  
  list(
    df_merged = df_merged,
    clusters = clusters
  )
}
