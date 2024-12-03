data_load <- function() {
  # List files in the working directory
  files <- list.files()
  cat("Files in the working directory:\n")
  print(files)
  
  # Prompt the user to select a file
  file_name <- readline("Enter the name of the data file to load (e.g., 'data.csv'): ")
  
  # Check if the file exists
  if (!file_name %in% files) {
    cat("The file does not exist in the working directory.\n")
    return(NULL)
  }
  
  # Determine the file extension
  file_extension <- tools::file_ext(file_name)
  
  # Read the data based on the file type
  if (file_extension == "csv") {
    data <- read.csv(file_name)
  } else if (file_extension == "txt") {
    data <- read.table(file_name, header = TRUE)
  } else {
    cat("Unsupported file type. Please provide a .csv or .txt file.\n")
    return(NULL)
  }
  
  # Count the number of existing datasets named 'df<i>' and save the new one
  existing_dfs <- ls(pattern = "^df\\d+$", envir = .GlobalEnv)
  new_index <- length(existing_dfs) + 1
  assign(paste0("df", new_index), data, envir = .GlobalEnv)
  
  # Notify the user
  cat(paste0("Data loaded successfully and saved as 'df", new_index, "'.\n"))
}








#' Plot all pairs of columns in a dataframe
#'
#' This function takes each pair of columns in a dataframe and generates 
#' scatterplots, boxplots, and histograms. The plots are colorblind-friendly.
#'
#' @param df The dataframe to use for plotting.
#' @return A list of ggplot objects.
#' @import ggplot2
#' @examples
#' data_plot_all(mtcars)
data_plot_all <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed. Please install it first.")
  }
  
  library(ggplot2)
  
  # Prompt the user for the dataframe name
  df_name <- readline("Enter the dataframe name (e.g., 'df1'): ")
  
  # Check if the dataframe exists in the global environment
  if (!exists(df_name, envir = .GlobalEnv)) {
    cat("The dataframe does not exist.\n")
    return(NULL)
  }
  
  # Retrieve the dataframe
  df <- get(df_name, envir = .GlobalEnv)
  
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    stop("The input must be a dataframe.")
  }
  
  # Ensure there are at least two columns
  if (ncol(df) < 2) {
    stop("The dataframe must have at least two columns.")
  }
  
  # Identify numeric columns for plotting
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) < 2) {
    stop("The dataframe must have at least two numeric columns.")
  }
  
  # Initialize a list to store plots
  plot_list <- list()
  
  # Loop through all pairs of numeric columns
  for (i in seq_along(numeric_cols)) {
    for (j in seq_along(numeric_cols)) {
      if (i != j) {
        x_col <- numeric_cols[i]
        y_col <- numeric_cols[j]
        
        # Scatterplot
        scatter <- ggplot(df, aes_string(x = x_col, y = y_col)) +
          geom_point(color = "blue", size = 2, alpha = 0.7) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(title = paste("Scatterplot of", x_col, "vs", y_col),
               x = x_col,
               y = y_col) +
          scale_color_brewer(palette = "Set2")
        

        
        # Histogram
        hist <- ggplot(df, aes_string(x = x_col)) +
          geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", x_col),
               x = x_col,
               y = "Frequency")
        
        # Add plots to the list
        plot_list[[paste("scatter", x_col, y_col, sep = "_")]] <- scatter

        plot_list[[paste("hist", x_col, sep = "_")]] <- hist
      }
    }
  }
  
  # Return the list of plots
  return(plot_list)
}

# Example usage:
# plot_list <- data_plot_all(mtcars)
# To display a specific plot: print(plot_list[["scatter_mpg_disp"]])






#' Plot the residuals of a model
#'
#' This function takes a dataframe with residuals and plots them using ggplot2.
#' It creates a histogram and scatter plot to evaluate the distribution of the residuals.
#'
#' @param df The dataframe containing residuals.
#' @param residual_col The name of the column containing residuals (default is 'residuals').
#' @return A list of ggplot objects (histogram and scatter plot).
#' @import ggplot2
#' @examples
#' data_plot_res(df1)
data_plot_res <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed. Please install it first.")
  }
  library(ggplot2)
  
  # Prompt for the dataframe name
  df_name <- readline("Enter the dataframe name (e.g., 'df1'): ")
  
  # Check if the dataframe exists in the global environment
  if (!exists(df_name, envir = .GlobalEnv)) {
    cat("The dataframe does not exist.\n")
    return(NULL)
  }
  
  # Retrieve the dataframe
  df <- get(df_name, envir = .GlobalEnv)
  
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    cat("The specified object is not a dataframe.\n")
    return(NULL)
  }
  
  
  residual_col = "residuals"
  # Check if the dataframe contains residuals
  if (!(residual_col %in% names(df))) {
    cat("Residual column not found in the dataframe.\n")
    return(NULL)
  }
  
  # Extract residuals
  residuals <- df[[residual_col]]
  
  # Create a histogram of residuals
  hist_res <- ggplot(df, aes(x = residuals)) +
    geom_histogram(binwidth = 0.5, color = "black", fill = "blue", alpha = 0.7) +
    theme_classic() +
    labs(title = paste("Histogram of", residual_col), x = "Residuals", y = "Frequency")
  
  # Create a scatter plot of residuals vs fitted values
  fitted_values <- df$fitted.values
  scatter_res <- ggplot(df, aes(x = fitted_values, y = residuals)) +
    geom_point(color = "red", alpha = 0.7) +
    theme_classic() +
    labs(title = paste("Residuals vs Fitted Values"),
         x = "Fitted Values", y = "Residuals")
  
  # Return the list of plots
  return(list(histogram = hist_res, scatter = scatter_res))
}

# Example usage:
# plot_list <- data_plot_res(df1)
# print(plot_list$histogram)
# print(plot_list$scatter)







#' Fit a statistical model interactively
#'
#' This function prompts the user to input a dataframe name, selects variables,
#' fits a mixed-effects model (or other types of models), and optionally saves
#' the residuals and fitted values to the dataframe and new variables.
#'
#' @import nlme
#' @examples
#' design()
design <- function() {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("The 'nlme' package is required but not installed. Please install it first.")
  }
  library(nlme)
  
  # Prompt for the dataframe name
  df_name <- readline("Enter the dataframe name (e.g., 'df1'): ")
  
  # Check if the dataframe exists in the global environment
  if (!exists(df_name, envir = .GlobalEnv)) {
    cat("The dataframe does not exist.\n")
    return(NULL)
  }
  
  # Retrieve the dataframe
  df <- get(df_name, envir = .GlobalEnv)
  
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    cat("The specified object is not a dataframe.\n")
    return(NULL)
  }
  
  # Print column names
  cat("Column names:\n")
  print(names(df))
  
  # Prompt for response and independent variables
  response <- readline("Enter the response variable: ")
  independents <- readline("Enter independent variables (comma-separated): ")
  independents <- strsplit(independents, ",")[[1]]
  
  # Prompt for interactions
  interactions <- readline("Enter interaction terms (comma-separated, or leave blank): ")
  if (nchar(interactions) > 0) {
    independents <- c(independents, interactions)
  }
  
  # Prompt for random effects (if any)
  random_effects <- readline("Enter random effects (e.g., ~ 1 | group, or leave blank): ")
  
  # Construct the formula
  fixed_formula <- paste(response, "~", paste(independents, collapse = " + "))
  
  # Fit the model
  if (nchar(random_effects) > 0) {
    model <- lme(as.formula(fixed_formula), random = as.formula(random_effects), data = df)
  } else {
    model <- lm(as.formula(fixed_formula), data = df)
  }
  
  # Print the model summary
  print(summary(model))
  print(model)
  
  # Calculate fitted values
  fitted_values <- fitted(model)
  df$fitted_values <- fitted_values  # Add fitted values to the dataframe
  
  # Prompt to save residuals
  save_residuals <- tolower(readline("Do you want to save the residuals? (yes/no): "))
  
  if (save_residuals == "yes") {
    # Calculate residuals
    residuals <- resid(model)
    
    # Add residuals to the dataframe
    df$residuals <- residuals
    assign(df_name, df, envir = .GlobalEnv)
    
    # Save residuals to a variable res{i}
    data_number <- gsub("df", "", df_name)
    residual_var <- paste0("res", data_number)
    assign(residual_var, residuals, envir = .GlobalEnv)
    
    # Save fitted values to a variable fitted{i}
    fitted_var <- paste0("fitted", data_number)
    assign(fitted_var, fitted_values, envir = .GlobalEnv)
    
    cat(paste("Residuals saved as", residual_var, "and fitted values saved as", fitted_var, ".\n"))
    cat(paste("Both residuals and fitted values have been added to", df_name, ".\n"))
  } else {
    cat("Residuals and fitted values not saved.\n")
  }
}
