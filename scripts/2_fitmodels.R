#' This scripts fits and evaluates the different models 
#' 

# Import libraries
suppressWarnings(library(caret))
suppressWarnings(library(jsonlite))
suppressWarnings(library(yardstick))
suppressWarnings(library(pROC))
suppressWarnings(library(CalibrationCurves))
suppressWarnings(library(xgboost))
suppressWarnings(library(stringr))
suppressWarnings(library(data.table))
suppressWarnings(library(Matrix))
suppressWarnings(library(dplyr))
suppressWarnings(library(recipes))

split_train_environment <- function(data, test_environment, test_variable) {
  # This functions splits the data into a training and test set, 
  # the training set includes the test_variable 
  # and the test set includes the rest of the data
  train_df <- subset(data, data[[test_environment]] == test_variable)
  test_df <- subset(data, data[[test_environment]] != test_variable)
  return(list(train=train_df, test=test_df))
}

fit_task <- function(train, task) {
  #' This function fits a logistic regression model based on the task metadata description and returns the model
  
  outcome_name <- task$outcome_variable
  feature_names <- unlist(task$features)
  
  task_formula <- reformulate(feature_names, outcome_name)
  
  if(task$type=="diagnosis") {  
    fit <- glm(task_formula, data=train, family = "binomial") 
  }
  if(task$type=="prognosis") {
    fit <- glm(task_formula, data=train, family = "binomial")
  }
  fit
}

analysis <- function(data_tag="ari") { 
  # Get the processed data
  path_processed_data <- here::here("data", "processed", paste0("processed_", data_tag, ".csv"))
  processed_data <- read.csv(path_processed_data)
  
  # Get the tasks from the metadata
  path_metadata <- here::here("data", "metadata", paste0("metadata_", data_tag, ".json"))
  metadata <- fromJSON(path_metadata, simplifyVector=TRUE)
  tasks_metadata <- metadata$tasks # this is a data.frame of tasks
  environment <- metadata$test_environment
  
  # Initialize dataframe
  combined_df <- data.frame(
    dataset = character(),
    test_environment = character(),
    train_variable = character(),
    test_variable = character(),
    type = character(),
    task_number = numeric(),
    AUC_train = integer(),
    AUC_test = integer(),
    calibration_train.incpt = numeric(),
    calibration_train.slope = numeric(),
    calibration_test.incpt = numeric(),
    calibration_test.slope = numeric(),
    N_train = integer(),
    N_test = integer(),
    model = character()
  )
  
  # Split the data into training and test set based on the environment
  for(train_var in unique(processed_data[[environment]])) {
    split_env <- split_train_environment(processed_data, environment, train_var)
    # For each task, fit and evaluate the model
    for (i in 1:nrow(tasks_metadata)) {
      model_glm <- fit_task(split_env$train, tasks_metadata[i,]) 
      for (test_var in unique(processed_data[[environment]])) {
        if (test_var != train_var) {
          test_set <- subset(split_env$test, split_env$test[[environment]]==test_var)
          df_model_glm <- evaluate_model(model_glm, split_env$train, test_set, tasks_metadata[i,], metadata, train_var, test_var) 
          
          combined_df <- rbind(combined_df, df_model_glm)
        }
      }
      # row_list[[length(row_list)+1]] <- df_model
      # combined_df <- rbind(combined_df, df_model_glm)
    }
  }
  # Return the dataframe
  combined_df
}

evaluate_model <- function(fitted_model, train_data, test_data, task, metadata, train_var, test_var) {
  #' This function evaluates the model on the training and test data and 
  #' returns a dataframe with outcome variables
  #' 
  # Get the outcome variable
  outcome_var <- task$outcome_variable
  
  # Get the actual result
  actual_train <- as.integer(train_data[[outcome_var]]) 
  actual_test <- as.integer(test_data[[outcome_var]]) 
  
  # Get the predicted probabilities of the model based on the data
  probability_train <- predict(fitted_model, type="response")
  probability_test <- predict(fitted_model, test_data, type="response")
  
  # Create a confusion matrix plot 
  # conf_plot <- create_conf_plot(actual, probability)
  
  # Plot the roc curves for the probabilities and the actual values
  if (task$type=="diagnosis") {
    roc_curve_train <- roc(actual_train, probability_train, plot=FALSE, levels=c(0,1))
    roc_curve_test <- roc(actual_test, probability_test, plot=FALSE, levels=c(0,1))
  }
  if (task$type=="prognosis") {
    roc_curve_train <- roc(actual_train, probability_train, plot=FALSE, levels=c(0,1))
    roc_curve_test <- roc(actual_test, probability_test, plot=FALSE, levels=c(0,1))
  }
  # Get the AUC values
  auc_value_train <- auc(roc_curve_train)
  auc_value_test <- auc(roc_curve_test)
  
  # Save ROC plot and calibration plot in a .png file, store calibration values
  save_roc_curve(roc_curve_train, roc_curve_test, task, metadata$tag, train_var, test_var)
  cal_values <- save_cal(probability_train, probability_test, actual_train, actual_test, task, metadata$tag, train_var, test_var)
  
  # Save variables in a dataframe
  df_eval <- data.frame(dataset = metadata$tag,
                        test_environment = metadata$test_environment,
                        train_variable = train_var,
                        test_variable = test_var,
                        type = task$type,
                        task_tag = task$tag,
                        AUC_train = auc_value_train[1], 
                        AUC_test = auc_value_test[1], 
                        calibration_train = list(int=cal_values$calibration_train.incpt, slope=cal_values$calibration_train.slope), 
                        calibration_test = list(int=cal_values$calibration_test.incpt, slope=cal_values$calibration_test.slope), 
                        N_train = nrow(train_data), 
                        N_test = nrow(test_data),
                        model = "glm"
  )
  df_eval
}

save_roc_curve <- function(roc_curve_train, roc_curve_test, task, dataset_name, train_var, test_var) {
  # Save the ROC-curve as a .png file
  path <- here::here("results", "graphics", dataset_name, "train_env")
  if(!dir.exists(path)) {
    dir.create(path)
  }
  file_path <- file.path(path, paste0(dataset_name, "_", str_extract(task$tag, "\\d+"), "_roc_", task$type, "_", train_var, "_", test_var, ".png"))
  png(file_path)
  plot(roc_curve_train, main=paste("ROC Curve", dataset_name, task$type, train_var, test_var), print.auc = TRUE, print.auc.y = 0.4, legacy.axes=TRUE, col='blue', lwd=2)
  plot(roc_curve_test, print.auc = TRUE, print.auc.y = 0.35, legacy.axes=TRUE, col='purple', lwd=2, add=TRUE)
  legend(1, 1, legend=c("Train", "Test"),  
         fill = c("blue","purple") 
  )
  dev.off()
}

save_cal <- function(probability_train, probability_test, actual_train, actual_test, task, dataset_name, train_var, test_var) {
  # Save the calibration curve as a .png file
  path <- here::here("results", "graphics", dataset_name, "train_env")
  file_path <- file.path(path, paste0(dataset_name, "_", str_extract(task$tag, "\\d+"), "_cal_train_", task$type, "_", train_var, "_", test_var, ".png"))
  png(file_path)
  cal_train = val.prob.ci.2(probability_train, actual_train, logistic.cal=TRUE, smooth="none", col.log = "blue")
  title(main = paste("Calibration Curve of train", dataset_name, task$type, train_var, test_var))
  dev.off()
  
  file_path2 <- file.path(path, paste0(dataset_name, "_", str_extract(task$tag, "\\d+"), "_cal_test_", task$type, "_", train_var, "_", test_var, ".png"))
  png(file_path2)
  cal_test = val.prob.ci.2(probability_test, actual_test, logistic.cal=TRUE, smooth="none", col.log = "purple")
  title(main = paste("Calibration Curve of test", dataset_name, task$type, train_var, test_var))
  dev.off()
  
  incpt_train <- round(cal_train$Calibration$Intercept[[1]],2)
  slope_train <- round(cal_train$Calibration$Slope[[1]],2)
  incpt_test <- round(cal_test$Calibration$Intercept[[1]],2)
  slope_test <- round(cal_test$Calibration$Slope[[1]],2)
  
  # Store the calibration values
  cal_values <- data.frame(calibration_train = list("incpt"=incpt_train, "slope"=slope_train),
                           calibration_test = list("incpt"=incpt_test, "slope"=slope_test))
  cal_values
}


# Run analysis for separate datasets
df_train_glm_ari <- analysis(data_tag="ari")
df_train_glm_news <- analysis(data_tag="news")

# Combine the results
combined_train_df <- rbind(df_train_glm_ari, df_train_glm_news)

# Store the values in a .csv file
write.csv(combined_train_df, file=file.path(here::here("results"), "results_1trainenv.csv"), row.names=FALSE)
