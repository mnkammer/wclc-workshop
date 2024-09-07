# Function to create a reclassification scatter plot of two biomarkers with customizable colors
plot_biomarker_scatter <- function(data, biomarker1, biomarker2, color_var, title = NULL, ar = NULL) {
  ggplot(data, aes_string(x = biomarker1, y = biomarker2, color = color_var)) +
    geom_point(size = 2, alpha = 0.3) +
    scale_color_manual(values = c("blue", "red")) +
    labs(title = title, x = biomarker1, y = biomarker2, color = color_var) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_hline(yintercept = c(0.1, 0.7), color = "lightgray", linetype = "solid") +
    geom_vline(xintercept = c(0.1, 0.7), color = "lightgray", linetype = "solid") +
    theme_bw() +
    theme(
      aspect.ratio = ar,
      legend.position = "none",
      panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
      panel.grid.minor = element_blank()
    )
}


# Function to calculate Net Reclassification Index (NRI)
calculate_nri <- function(data, outcome, biomarker1, biomarker2, thresholds1, thresholds2) {
  # Ensure thresholds are correct length
  if (length(thresholds1) != 2 | length(thresholds2) != 2) {
    stop("Thresholds must be vectors of length 2.")
  }
  
  # Create a categorical variable for each biomarker based on the thresholds
  data <- data %>%
    mutate(group1 = case_when(
      !!sym(biomarker1) < thresholds1[1] ~ "Low",
      !!sym(biomarker1) >= thresholds1[1] & !!sym(biomarker1) < thresholds1[2] ~ "Medium",
      !!sym(biomarker1) >= thresholds1[2] ~ "High"
    )) %>%
    mutate(group2 = case_when(
      !!sym(biomarker2) < thresholds2[1] ~ "Low",
      !!sym(biomarker2) >= thresholds2[1] & !!sym(biomarker2) < thresholds2[2] ~ "Medium",
      !!sym(biomarker2) >= thresholds2[2] ~ "High"
    ))
  
  # Ensure the levels are set correctly
  data$group1 <- factor(data$group1, levels = c("Low", "Medium", "High"))
  data$group2 <- factor(data$group2, levels = c("Low", "Medium", "High"))
  
  # Create 3x3 tables for outcome = 0 and outcome = 1
  table_outcome_0 <- table(data %>% filter(!!sym(outcome) == 0) %>% select(group1, group2))
  table_outcome_1 <- table(data %>% filter(!!sym(outcome) == 1) %>% select(group1, group2))
  
  # Convert tables to data frames for better readability
  df_table_outcome_0 <- as.data.frame.matrix(table_outcome_0)
  df_table_outcome_1 <- as.data.frame.matrix(table_outcome_1)
  
  # Calculate observed reclassification index for outcome = 0
  obs_reclass_0 <- (df_table_outcome_0["Medium", "Low"] - df_table_outcome_0["Medium", "High"]) / sum(df_table_outcome_0["Medium",])
  
  # Calculate expected reclassification index for outcome = 0
  exp_reclass_0 <- (
    ((df_table_outcome_0["Low", "Medium"] + 
        df_table_outcome_0["Medium", "High"]) / 2) -
      ((df_table_outcome_0["Medium", "Low"] + 
          df_table_outcome_0["High", "Medium"]) / 2)) /
    (((df_table_outcome_1["Low", "Medium"] + 
         df_table_outcome_1["Medium", "High"]) / 2) + 
       ((df_table_outcome_1["Medium", "Low"] + 
           df_table_outcome_1["High", "Medium"]) / 2) + 
       sum(df_table_outcome_0["Medium",]))
  
  # Calculate cNRI for outcome = 0
  cNRI_0 <- obs_reclass_0 - exp_reclass_0
  
  # Calculate observed reclassification index for outcome = 1
  obs_reclass_1 <- (
    df_table_outcome_1["Medium", "High"] - 
      df_table_outcome_1["Medium", "Low"]) / 
    sum(df_table_outcome_1["Medium",])
  
  # Calculate expected reclassification index for outcome = 1
  exp_reclass_1 <- (
    ((df_table_outcome_1["Low", "Medium"] + 
        df_table_outcome_1["Medium", "High"]) / 2) -
      ((df_table_outcome_1["Medium", "Low"] + 
          df_table_outcome_1["High", "Medium"]) / 2)) /
    (((df_table_outcome_1["Low", "Medium"] + 
         df_table_outcome_1["Medium", "High"]) / 2) + 
       ((df_table_outcome_1["Medium", "Low"] + 
           df_table_outcome_1["High", "Medium"]) / 2) + 
       sum(df_table_outcome_1["Medium",]))
  
  # Calculate cNRI for outcome = 1
  cNRI_1 <- obs_reclass_1 - exp_reclass_1
  
  # # Print tables
  # cat("Reclassification Table for Outcome = 0\n")
  # print(df_table_outcome_0)
  # cat("\nReclassification Table for Outcome = 1\n")
  # print(df_table_outcome_1)
  
  return(list(
    obs_reclass_0 = obs_reclass_0,
    exp_reclass_0 = exp_reclass_0,
    cNRI_0 = cNRI_0,
    obs_reclass_1 = obs_reclass_1,
    exp_reclass_1 = exp_reclass_1,
    cNRI_1 = cNRI_1
  ))
}


# Combined function to calculate NRI, plot the reclassification tables, and scatter plots
calculate_nri_and_plot <- function(data, outcome, biomarker1, biomarker2, thresholds1, thresholds2) {
  # Ensure thresholds are correct length
  if (length(thresholds1) != 2 | length(thresholds2) != 2) {
    stop("Thresholds must be vectors of length 2.")
  }
  
  # Create a categorical variable for each biomarker based on the thresholds
  data <- data %>%
    mutate(group1 = case_when(
      !!sym(biomarker1) < thresholds1[1] ~ "Low",
      !!sym(biomarker1) >= thresholds1[1] & !!sym(biomarker1) < thresholds1[2] ~ "Medium",
      !!sym(biomarker1) >= thresholds1[2] ~ "High"
    )) %>%
    mutate(group2 = case_when(
      !!sym(biomarker2) < thresholds2[1] ~ "Low",
      !!sym(biomarker2) >= thresholds2[1] & !!sym(biomarker2) < thresholds2[2] ~ "Medium",
      !!sym(biomarker2) >= thresholds2[2] ~ "High"
    ))
  
  # Ensure the levels are set correctly
  data$group1 <- factor(data$group1, levels = c("Low", "Medium", "High"))
  data$group2 <- factor(data$group2, levels = c("Low", "Medium", "High"))
  
  # Create 3x3 tables for outcome = 0 and outcome = 1
  table_outcome_0 <- table(data %>% filter(!!sym(outcome) == 0) %>% select(group1, group2))
  table_outcome_1 <- table(data %>% filter(!!sym(outcome) == 1) %>% select(group1, group2))
  
  # Add totals for rows and columns
  add_totals <- function(table) {
    table <- cbind(table, Total = rowSums(table))
    table <- rbind(table, Total = colSums(table))
    return(table)
  }
  
  table_outcome_0 <- add_totals(table_outcome_0)
  table_outcome_1 <- add_totals(table_outcome_1)
  
  # Convert tables to data frames for plotting
  plot_data <- function(df_table, is_case_0 = TRUE) {
    df <- as.data.frame(df_table)
    df <- df %>%
      mutate(CBM = rownames(df)) %>%
      pivot_longer(-CBM, names_to = "Mayo", values_to = "Count")
    
    df$Color <- "white"  # Default color
    if (is_case_0) {
      df$Color[df$Mayo == "Low" & df$CBM == "Medium"] <- "lightblue"
      df$Color[df$Mayo == "High" & df$CBM == "Medium"] <- "lightcoral"
    } else {
      df$Color[df$Mayo == "Low" & df$CBM == "Medium"] <- "lightcoral"
      df$Color[df$Mayo == "High" & df$CBM == "Medium"] <- "lightblue"
    }
    
    df$Color[df$Mayo == "Total" | df$CBM == "Total"] <- "white"
    
    df$Mayo <- factor(df$Mayo, levels = c("Low", "Medium", "High", "Total"))
    df$CBM <- factor(df$CBM, levels = c("Low", "Medium", "High", "Total"))
    
    return(df)
  }
  
  data_0 <- plot_data(table_outcome_0, is_case_0 = TRUE)
  data_1 <- plot_data(table_outcome_1, is_case_0 = FALSE)
  
  # Function to create reclassification table plots
  create_reclassification_plot <- function(data, title) {
    ggplot(data, aes(y = Mayo, x = CBM, label = Count)) +  # Transpose by switching x and y
      geom_tile(aes(fill = Color), color = "black") +
      geom_text(size = 4) +  # Smaller font size
      scale_x_discrete(limits = c("Low", "Medium", "High", "Total"), expand = c(0, 0)) +
      scale_y_discrete(limits = c("Low", "Medium", "High", "Total"), expand = c(0, 0)) +
      scale_fill_identity() +
      coord_fixed() +  # Ensure the plot is square
      theme_minimal() +
      ggtitle(title) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 10),  # Adjust font size
            axis.text.y = element_text(size = 10),  # Adjust font size
            panel.grid = element_blank())
  }
  
  # Create the reclassification table plots
  plot_table_0 <- create_reclassification_plot(data_0, "Benign")
  plot_table_1 <- create_reclassification_plot(data_1, "Cancer")
  
  # Function to create scatter plots
  create_scatter_plot <- function(data_subset, color_low, color_high, fill_low, fill_high, title) {
    ggplot(data_subset, aes_string(x = biomarker1, y = biomarker2)) +
      geom_rect(aes(xmin = thresholds1[1], xmax = thresholds1[2], ymin = 0, ymax = thresholds1[1]),
                fill = fill_low, alpha = 0.1) +
      geom_rect(aes(xmin = thresholds1[1], xmax = thresholds1[2], ymin = thresholds2[2], ymax = 1),
                fill = fill_high, alpha = 0.1) +
      geom_point(aes(fill = ifelse(interaction(group1, group2) %in% c("Medium.Low", "Medium.High"),
                                   interaction(group1, group2), "Other")),
                 shape = 21, size = 2, stroke = 1, color = "black", alpha = 0.5) +
      scale_fill_manual(values = c("Medium.Low" = color_low, "Medium.High" = color_high, "Other" = "white")) +
      scale_color_manual(values = c("Medium.Low" = color_low, "Medium.High" = color_high, "Low.Medium" = color_low, "High.Medium" = color_high, "Low.Low" = "white", "High.High" = "white", "Medium.Medium" = "white", "Low.High" = "white", "High.Low" = "white")) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      geom_hline(yintercept = c(thresholds1[1], thresholds2[2]), color = "gray", linetype = "solid") +
      geom_vline(xintercept = c(thresholds1[1], thresholds2[2]), color = "gray", linetype = "solid") +
      labs(title = title, x = biomarker1, y = biomarker2) +
      theme_minimal() +
      theme(
        aspect.ratio = 1,
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
      )
  }
  
  # Create the scatter plots
  plot_scatter_0 <- create_scatter_plot(data %>% filter(!!sym(outcome) == 0),
                                        color_low = "blue", color_high = "red",
                                        fill_low = "lightblue", fill_high = "lightcoral",
                                        title = "Benign")
  
  plot_scatter_1 <- create_scatter_plot(data %>% filter(!!sym(outcome) == 1),
                                        color_low = "red", color_high = "blue",
                                        fill_low = "lightcoral", fill_high = "lightblue",
                                        title = "Cancer")
  
  # Arrange plots in a 2x2 layout
  grid.arrange(
    plot_scatter_0, plot_scatter_1,
    plot_table_0, plot_table_1,
    ncol = 2, nrow = 2
  )
  
  # Return the plots and other relevant data
  return(list(
    scatter_plot_0 = plot_scatter_0,
    scatter_plot_1 = plot_scatter_1,
    reclassification_table_0 = plot_table_0,
    reclassification_table_1 = plot_table_1
  ))
}