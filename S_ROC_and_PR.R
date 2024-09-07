# Function to plot ROC curves
plot_roc_curves <- function(roc_data, biomarkers, colors) {
  # Create an empty data frame to hold the ROC data
  roc_df <- data.frame(FPR = numeric(0), TPR = numeric(0), Biomarker = character(0), stringsAsFactors = FALSE)
  
  # Populate the data frame with the ROC data for each biomarker
  for (i in seq_along(roc_data)) {
    roc_df <- rbind(roc_df, data.frame(
      FPR = roc_data[[i]]$rocperf@x.values[[1]],
      TPR = roc_data[[i]]$rocperf@y.values[[1]],
      Biomarker = gsub("^B\\.", "", biomarkers[i])
    ))
  }
  
  # Create AUC labels for the legend
  auc_labels <- sapply(seq_along(roc_data), function(i) {
    if (!is.null(roc_data[[i]]$auc)) {
      paste(gsub("^B\\.", "", biomarkers[i]), "(AUC =", round(roc_data[[i]]$auc, 2), ")")
    } else {
      paste(gsub("^B\\.", "", biomarkers[i]), "(AUC not available)")
    }
  })
  
  # Ensure that the factor levels of Biomarker match the order of biomarkers for consistent color mapping
  roc_df$Biomarker <- factor(roc_df$Biomarker, levels = gsub("^B\\.", "", biomarkers))
  
  # Create the ggplot object
  p <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Biomarker, group = Biomarker)) +
    geom_line(size = 1) +
    scale_color_manual(values = colors, labels = auc_labels) +
    labs(
      title = "ROC Curves",
      x = "False Positive Rate",
      y = "True Positive Rate",
      color = "Biomarker"
    ) +
    theme_bw() +  # Use theme_bw()
    coord_fixed() +  # Ensure the plot is square
    theme(
      legend.position = c(0.95, 0.05),  # Position the legend in the bottom right corner
      legend.justification = c(1, 0),   # Adjust the legend's anchor point
      legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # White background with a black border
      legend.title = element_text(size = 10), 
      legend.text = element_text(size = 8),
      legend.key = element_rect(fill = "white"),
      aspect.ratio = 1
    ) +
    guides(color = guide_legend(override.aes = list(size = 2)))  # Adjust legend key sizes
  
  return(p)
}


# Function to plot Precision-Recall curves
plot_pr_curves <- function(pr_data, biomarkers, colors) {
  # Create an empty data frame to hold the PR data
  pr_df <- data.frame(Recall = numeric(0), Precision = numeric(0), Biomarker = character(0), stringsAsFactors = FALSE)
  
  # Populate the data frame with the PR data for each biomarker
  for (i in seq_along(pr_data)) {
    pr_df <- rbind(pr_df, data.frame(
      Recall = pr_data[[i]]$prperf@x.values[[1]],
      Precision = pr_data[[i]]$prperf@y.values[[1]],
      Biomarker = gsub("^B\\.", "", biomarkers[i])
    ))
  }
  
  # Create labels for the legend
  pr_labels <- sapply(seq_along(pr_data), function(i) {
    if (!is.null(pr_data[[i]]$auc)) {
      paste(gsub("^B\\.", "", biomarkers[i]), "(AUC =", round(pr_data[[i]]$auc, 2), ")")
    } else {
      paste(gsub("^B\\.", "", biomarkers[i]), "(AUC not available)")
    }
  })
  
  # Ensure that the factor levels of Biomarker match the order of biomarkers for consistent color mapping
  pr_df$Biomarker <- factor(pr_df$Biomarker, levels = gsub("^B\\.", "", biomarkers))
  
  # Create the ggplot object
  p <- ggplot(pr_df, aes(x = Recall, y = Precision, color = Biomarker, group = Biomarker)) +
    geom_line(size = 1) +
    scale_color_manual(values = colors, labels = pr_labels) +
    labs(
      title = "Precision-Recall Curves",
      x = "Recall",
      y = "Precision",
      color = "Biomarker"
    ) +
    theme_bw() +  # Use theme_bw()
    coord_fixed() +  # Ensure the plot is square
    theme(
      legend.position = c(0.95, 0.05),  # Position the legend in the bottom right corner
      legend.justification = c(1, 0),   # Adjust the legend's anchor point
      legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # White background with a black border
      legend.title = element_text(size = 10), 
      legend.text = element_text(size = 8),
      legend.key = element_rect(fill = "white"),
      aspect.ratio = 1
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    guides(color = guide_legend(override.aes = list(size = 2)))  # Adjust legend key sizes
  
  return(p)
}