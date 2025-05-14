# R/utils_plotting.R

#' Plot linear tree
#'
#' Create a linear binning tree plot
#'
#' @param hc Hierarchical clustering object
#' @param bd Linear binning data
#' @param num_bins Number of bins
#' @param distance_method Distance method used
#'
#' @return Plot
#' @importFrom graphics abline par plot points rect text
#' @noRd
plot_linear_tree <- function(hc, bd, num_bins, distance_method) {
  group_colors <- c("Shared" = "purple", "BEAM_Only" = "steelblue", "Dual_Only" = "darkorange")
  tip_group_vec <- bd$df_merged$Group[match(bd$tip_order, bd$df_merged$Tip)]
  tip_colors <- group_colors[tip_group_vec]
  
  breakpoints <- seq(0, bd$n_tips, length.out = num_bins + 1)
  bin_centers <- (head(breakpoints, -1) + tail(breakpoints, -1)) / 2
  
  par(mar = c(5, 4, 4, 2))
  plot(hc, labels = FALSE, hang = -1,
       main = paste0("HCLUST Tree with ", num_bins, " Linear Bins (", distance_method, ")"))
  
  for (i in 2:(length(breakpoints)-1)) {
    abline(v = breakpoints[i], col = "black", lty = 2, lwd = 1)
  }
  
  points(x = 1:bd$n_tips, y = rep(0, bd$n_tips), pch = 21, bg = tip_colors, cex = 0.5)
  
  text(x = bin_centers, y = par("usr")[3] - 0.02 * (par("usr")[4] - par("usr")[3]),
       labels = paste0("Bin ", 1:num_bins), xpd = TRUE, srt = 90, adj = 1, cex = 0.6)
}

#' Plot dynamic tree
#'
#' Create a dynamic tree cut plot
#'
#' @param hc Hierarchical clustering object
#' @param dend Dendrogram object
#' @param df Data frame with sequence data
#' @param dyn Dynamic clustering data
#' @param distance_method Distance method used
#' @param deepSplit Deep split parameter
#' @param minClusterSize Minimum cluster size
#' @param show_rectangles Whether to show rectangles around clusters
#' @param show_bin_numbers Whether to show bin numbers
#' @param color_tip_text Whether to color tip text
#' @param show_bottom_labels Whether to show bottom labels
#'
#' @return Plot
#' @importFrom dendextend set
#' @importFrom stats na.omit
#' @importFrom graphics par axis rect text
#' @noRd
plot_dynamic_tree <- function(hc, dend, df, dyn, distance_method, deepSplit, minClusterSize,
                             show_rectangles = TRUE, show_bin_numbers = TRUE, 
                             color_tip_text = TRUE, show_bottom_labels = TRUE) {
  clusters <- dyn$clusters
  ordered_tips <- df$Clone_ID[hc$order]
  clusters_ordered <- clusters[hc$order]
  labels_new <- paste(ordered_tips, clusters_ordered, sep = "_")
  
  if (color_tip_text) {
    cluster_colors <- rainbow(length(unique(na.omit(clusters))))
    tip_colors <- cluster_colors[as.numeric(as.factor(clusters))][hc$order]
  } else {
    tip_colors <- rep("black", length(hc$order))
  }
  
  dend_colored <- dend %>%
    set("labels", labels_new) %>%
    set("labels_colors", tip_colors) %>%
    set("labels_cex", 0.5)
  
  plot(dend_colored,
       main = paste0("Dynamic Tree Cut (deepSplit=", deepSplit,
                    ", minSize=", minClusterSize,
                    ", Dist=", distance_method, ")"),
       axes = FALSE)
  axis(2)
  
  cluster_vec <- clusters_ordered
  n_tips <- length(cluster_vec)
  
  run_start <- 1
  while (run_start <= n_tips) {
    current_cluster <- cluster_vec[run_start]
    run_end <- run_start
    while (run_end + 1 <= n_tips && cluster_vec[run_end + 1] == current_cluster) {
      run_end <- run_end + 1
    }
    
    if (show_rectangles) {
      rect(
        xleft = run_start - 0.5,
        ybottom = par("usr")[3],
        xright = run_end + 0.5,
        ytop = max(hc$height) * 0.6,
        border = "red",
        lwd = 2,
        xpd = TRUE
      )
    }
    
    if (show_bin_numbers) {
      text(
        x = mean(c(run_start, run_end)),
        y = par("usr")[3] - 0.02 * (par("usr")[4] - par("usr")[3]),
        labels = current_cluster,
        xpd = TRUE,
        srt = 90,
        adj = 1,
        cex = 0.7,
        col = "black"
      )
    }
    
    run_start <- run_end + 1
  }
  
  if (show_bottom_labels) {
    text(
      x = 1:length(labels_new),
      y = par("usr")[3] - 0.06 * (par("usr")[4] - par("usr")[3]),
      labels = labels_new,
      xpd = TRUE,
      srt = 90,
      adj = 1,
      cex = 0.5,
      col = "black"
    )
  }
}

#' Plot height tree
#'
#' Create a height-based tree cut plot
#'
#' @param hc Hierarchical clustering object
#' @param dend Dendrogram object
#' @param df Data frame with sequence data
#' @param h_clusters Height clustering data
#' @param distance_method Distance method used
#' @param cut_height Height for cutting tree
#' @param show_height_line Whether to show cut height line
#' @param color_height_clusters Whether to color clusters
#' @param show_height_rectangles Whether to show rectangles
#'
#' @return Plot
#' @importFrom dendextend set
#' @importFrom stats na.omit
#' @importFrom graphics abline par plot rect text
#' @noRd
plot_height_tree <- function(hc, dend, df, h_clusters, distance_method, cut_height,
                            show_height_line = TRUE, color_height_clusters = TRUE, 
                            show_height_rectangles = TRUE) {
  clusters <- h_clusters$clusters
  
  # Order clusters to match tree display order
  clusters_ordered <- clusters[hc$order]
  ordered_tips <- df$Clone_ID[hc$order]
  
  if (color_height_clusters) {
    cluster_colors <- rainbow(length(unique(na.omit(clusters))))
    tip_colors <- cluster_colors[as.numeric(as.factor(clusters))][hc$order]
    
    dend_colored <- dend %>%
      set("labels_colors", tip_colors) %>%
      set("labels_cex", 0.5)
    
    plot(dend_colored,
         main = paste0("Height-based Tree Cut (h=", round(cut_height, 2),
                      ", Dist=", distance_method, ")"))
  } else {
    plot(dend,
         main = paste0("Height-based Tree Cut (h=", round(cut_height, 2),
                      ", Dist=", distance_method, ")"))
  }
  
  if (show_height_line) {
    abline(h = cut_height, col = "red", lty = 2, lwd = 2)
  }
  
  # Add cluster rectangles similar to dynamic tree cut
  if (show_height_rectangles) {
    n_tips <- length(clusters_ordered)
    run_start <- 1
    
    while (run_start <= n_tips) {
      current_cluster <- clusters_ordered[run_start]
      run_end <- run_start
      
      while (run_end + 1 <= n_tips && clusters_ordered[run_end + 1] == current_cluster) {
        run_end <- run_end + 1
      }
      
      rect(
        xleft = run_start - 0.5,
        ybottom = par("usr")[3],
        xright = run_end + 0.5,
        ytop = cut_height * 0.95,
        border = "red",
        lwd = 2,
        xpd = TRUE
      )
      
      # Add cluster number below
      text(
        x = mean(c(run_start, run_end)),
        y = par("usr")[3] - 0.02 * (par("usr")[4] - par("usr")[3]),
        labels = current_cluster,
        xpd = TRUE,
        srt = 90,
        adj = 1,
        cex = 0.7,
        col = "black"
      )
      
      run_start <- run_end + 1
    }
  }
}

#' Plot group composition
#'
#' Create a group composition bar chart
#'
#' @param clustering_results Clustering results
#' @param cluster_method Clustering method
#'
#' @return ggplot object
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs scale_fill_manual theme element_text geom_text position_stack
#' @noRd
plot_group_composition <- function(clustering_results, cluster_method) {
  df_clustered <- clustering_results$df_merged
  
  # Sort clusters numerically for better visualization
  df_clustered$Cluster <- as.factor(df_clustered$Cluster)
  
  group_summary <- df_clustered %>%
    group_by(Cluster, Group) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Cluster = factor(Cluster, levels = sort(as.numeric(as.character(unique(Cluster))))))
  
  group_colors <- c("Shared" = "purple", "BEAM_Only" = "steelblue", "Dual_Only" = "darkorange")
  
  p <- ggplot(group_summary, aes(x = Cluster, y = Count, fill = Group)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 3, color = "black") +
    theme_minimal(base_size = 14) +
    labs(title = paste("Group Composition Across",
                      if(cluster_method == "linear") "Bins"
                      else if(cluster_method == "dynamic") "Dynamic Clusters"
                      else "Height-based Clusters"),
         x = if(cluster_method == "linear") "Bin" else "Cluster",
         y = "Count") +
    scale_fill_manual(values = group_colors) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))
  
  return(p)
}

#' Plot histogram
#'
#' Create a histogram of cluster sizes
#'
#' @param clustering_results Clustering results
#' @param cluster_method Clustering method
#' @param title Plot title
#'
#' @return ggplot object
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs theme element_text
#' @noRd
plot_histogram <- function(clustering_results, cluster_method, title) {
  df_clustered <- clustering_results$df_merged
  
  # Sort clusters numerically for better visualization
  df_clustered$Cluster <- as.factor(df_clustered$Cluster)
  
  bin_counts <- df_clustered %>%
    group_by(Cluster) %>%
    summarise(Total = n(), .groups = "drop") %>%
    mutate(Cluster = factor(Cluster, levels = sort(as.numeric(as.character(unique(Cluster))))))
  
  p <- ggplot(bin_counts, aes(x = Cluster, y = Total)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    theme_minimal(base_size = 14) +
    labs(title = title,
         x = if(cluster_method == "linear") "Bin" else "Cluster",
         y = "Clone Count") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))
  
  return(p)
}
