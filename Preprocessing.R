start_time <- Sys.time()  # Record start time
################################################################################
# Data Cleaning and Preprocessing

# Step 1: Load the dataset
data <- read.csv("uci-secom.csv", stringsAsFactors = FALSE)

# Step 2: Drop columns with more than 50% missing values
missing_percentage <- sapply(data, function(x) sum(is.na(x)) / nrow(data) * 100)
columns_to_drop <- names(data)[missing_percentage > 50]
data_cleaned <- data[, !(names(data) %in% columns_to_drop)]

# Step 3: Convert the first column to datetime format if applicable
if (class(data_cleaned[[1]]) == "character") {
  data_cleaned[[1]] <- as.POSIXct(data_cleaned[[1]], format = "%Y/%m/%d %H:%M", tz = "UTC")
}

# Step 4: Impute missing values in numeric columns using the median
numeric_cols <- sapply(data_cleaned, is.numeric)
data_cleaned[, numeric_cols] <- lapply(data_cleaned[, numeric_cols], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

# Step 5: Remove columns with constant values
constant_columns <- sapply(data_cleaned, function(x) length(unique(x)) == 1)
data_cleaned <- data_cleaned[, !constant_columns]

# Step 6: Keep columns with at least 5 unique values, plus the last column
columns_to_keep <- names(data_cleaned)[sapply(data_cleaned, function(x) length(unique(x)) >= 5)]
columns_to_keep <- c(columns_to_keep, names(data_cleaned)[ncol(data_cleaned)]) 
data_cleaned <- data_cleaned[, columns_to_keep]

# Step 7: Rename columns for clarity
column_names <- c("Time", paste0("V", seq(1, ncol(data_cleaned) - 2)), "Group")
colnames(data_cleaned) <- column_names

# Step 8: Save the cleaned dataset
write.csv(data_cleaned, "final_cleaned_dataset.csv", row.names = FALSE)
cat("Final cleaned dataset saved as 'final_cleaned_dataset.csv'\n")