#' The goal of this script is to process ari from raw to processed
#' 
#' 
library(jsonlite)
library(haven)
library(dplyr)

data_path <- here::here('data', 'raw', 'ari.csv')
raw_data <- read.csv(data_path)
metadata_path <- here::here('data', 'metadata', 'ari.json')

ari <- raw_data

# data cleaning: remove duplicates
ari <- distinct(ari)

# check for missing values in all columns and find these columns
missing_values <- colSums(is.na(ari))
complete_columns <- names(missing_values)[missing_values == 0]
cat("Columns without missing values:", paste(complete_columns, collapse = ", "), "\n")
print(missing_values)

# create a new logical variable 'pneumonia_or_worse' based on values in 'cprot' column
ari$pneumonia_or_worse <- ari$cprot %in% c("Pneumonia", "Severe pneumonia", "Very severe disease")
# create a new logical variable 'death' based on values in 'dead' column
ari$death <- ari$dead %in% c("L", "P", "Y")

# create new variable for shortening the environment names - from metadata?
ari$env <- substr(tolower(ari$country), 1, 3)

# save the processed data to a new CSV file in the 'processed' folder
processed_file_path <- here::here('data', 'processed', 'processed_ari.csv')
write.csv(ari, file = processed_file_path, row.names = FALSE)

# converting 'coh' and 'hdb' to factor
ari$coh <- as.factor(ari$coh)
ari$hdb <- as.factor(ari$hdb)
ari$hfa <- as.factor(ari$hfa)
ari$hfe <- as.factor(ari$hfe)
ari$apn <- as.factor(ari$apn)


# select variables for modeling 
selected_vars <- c("pneumonia_or_worse", "coh", "rr", "temp","lgth", "hdb", "age", "hfa", "hfe", "apn", "death")

# check missing data for selected variables
missing_data <- colSums(is.na(ari[selected_vars]))

# check data type for selected variables
data_types <- sapply(ari[selected_vars], class)

# check number of unique values for categorical variables
unique_values <- sapply(ari[selected_vars], function(x) length(unique(x)))

# check summary table
summary_table <- data.frame(
  Variables = selected_vars,
  Missing_Data = missing_data,
  Data_Type = data_types,
  Unique_Values = unique_values
)

# save the table to a CSV file
write.csv(summary_table, "summary_table_ari.csv", row.names = FALSE)

# generate data_json
data_json <- list(
  tag = "ari",
  full_name = "ARI",
  url = "https://hbiostat.org/data/repo/ari.zip",
  description = "Dataset containing information about patients with Acute Respiratory Infection (ARI)",
  num_rows = nrow(summary_table),
  num_columns = ncol(summary_table),
  column_names = colnames(summary_table),
  column_data_types = sapply(summary_table, class),
  creation_date = Sys.Date(),
  source_file = "ari.csv",
  tasks = list(
    list(
      tag = "task_1",
      outcome_variable = "pneumonia_or_worse",
      type = "diagnosis",
      features = c("age","coh", "rr")),
    list(
      tag = "task_2",
      outcome_variable = "pneumonia_or_worse",
      type = "diagnosis",
      features = c("age","coh", "rr","lgth","hdb","hfa","hfe","apn")),
    list(
      tag = "task_3",
      outcome_variable = "death",
      type = "prognosis",
      features = c("age","coh", "rr")),
    list(
      tag = "task_4",
      outcome_variable = "death",
      type = "prognosis",
      features = c("age","coh", "rr","lgth", "hdb","hfa","hfe","apn"))
  ),
  test_environment = "env"
)

# convert metadata to JSON format
metadata <- toJSON(data_json, pretty = TRUE, auto_unbox = TRUE)

# save metadata to a JSON file
metadata_file_path <- here::here('data', 'metadata', 'metadata_ari.json')
writeLines(metadata, metadata_file_path)
