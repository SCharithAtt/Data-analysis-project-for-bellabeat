install.packages("tidyverse")
library(tidyverse)
####################################
#   Uploading Datasets             #
####################################
#uploading all the data into variables

dailyActivity<-read_csv("Fitbit Data/dailyActivity_merged.csv")
dailyCalories<-read_csv("Fitbit Data/dailyCalories_merged.csv")
dailyIntensities<-read_csv("Fitbit Data/dailyIntensities_merged.csv")
dailySteps<-read_csv("Fitbit Data/dailySteps_merged.csv")
heartRateSeconds<-read_csv("Fitbit Data/heartrate_seconds_merged.csv")
hourlyCalories<-read_csv("Fitbit Data/hourlyCalories_merged.csv")
hourlyIntensities<-read_csv("Fitbit Data/hourlyIntensities_merged.csv")
hourlySteps<-read_csv("Fitbit Data/hourlySteps_merged.csv")
minuteCaloriesNarrow<-read_csv("Fitbit Data/minuteCaloriesNarrow_merged.csv")
minuteCaloriesWide<-read_csv("Fitbit Data/minuteCaloriesWide_merged.csv")
minuteIntensitiesNarrow<-read_csv("Fitbit Data/minuteIntensitiesNarrow_merged.csv")
minuteIntensitiesWide<-read_csv("Fitbit Data/minuteIntensitiesWide_merged.csv")
minuteMETsNarrow<-read_csv("Fitbit Data/minuteMETsNarrow_merged.csv")
minuteSleep<-read_csv("Fitbit Data/minuteSleep_merged.csv")
minuteStepsNarrow<-read_csv("Fitbit Data/minuteStepsNarrow_merged.csv")
minuteStepsWide<-read_csv("Fitbit Data/minuteStepsWide_merged.csv")
sleepDaily<-read_csv("Fitbit Data/sleepDay_merged.csv")
weightLogInfo<-read_csv("Fitbit Data/weightLogInfo_merged.csv")

View(weightLogInfo)
head(dailyActivity)

# Check for missing values
colSums(is.na(dailyActivity))
colSums(is.na(dailyCalories))
colSums(is.na(dailyIntensities))
colSums(is.na(dailySteps))
colSums(is.na(heartRateSeconds))
colSums(is.na(hourlyCalories))
colSums(is.na(hourlyIntensities))
colSums(is.na(hourlySteps))
colSums(is.na(minuteCaloriesNarrow))
colSums(is.na(minuteCaloriesWide))
colSums(is.na(minuteIntensitiesNarrow))
colSums(is.na(minuteIntensitiesWide))
colSums(is.na(minuteMETsNarrow))
colSums(is.na(minuteSleep))
colSums(is.na(minuteStepsNarrow))
colSums(is.na(minuteStepsWide))
colSums(is.na(sleepDaily))
colSums(is.na(weightLogInfo))

#weightLogInfo -> "Fat" column has missing values

####################################
#   Exploring The Data             #
####################################

install.packages("here")
library("here")

install.packages("skimr")
library("skimr")

install.packages("janitor")
library("janitor")


#to get summary
skim_without_charts(weightLogInfo)


weightLogInfo%>%
  select(-Fat)

####################################
#   Viewing Summary Statistics     #
####################################

summary(dailyActivity)
summary(dailyCalories)
summary(dailyIntensities)
summary(dailySteps)
summary(heartRateSeconds)
summary(hourlyCalories)
summary(hourlyIntensities)
summary(hourlySteps)
summary(minuteCaloriesNarrow)
summary(minuteCaloriesWide)
summary(minuteIntensitiesNarrow)
summary(minuteIntensitiesWide)
summary(minuteMETsNarrow)
summary(minuteSleep)
summary(minuteStepsNarrow)
summary(minuteStepsWide)
summary(sleepDaily)
summary(weightLogInfo)














# Making a list of all the Csv Data, for reusability
fitbit_data <- list(
  dailyActivity = dailyActivity,
  dailyCalories = dailyCalories,
  dailyIntensities = dailyIntensities,
  dailySteps = dailySteps,
  heartRateSeconds = heartRateSeconds,
  hourlyCalories = hourlyCalories,
  hourlyIntensities = hourlyIntensities,
  hourlySteps = hourlySteps,
  minuteCaloriesNarrow = minuteCaloriesNarrow,
  minuteCaloriesWide = minuteCaloriesWide,
  minuteIntensitiesNarrow = minuteIntensitiesNarrow,
  minuteIntensitiesWide = minuteIntensitiesWide,
  minuteMETsNarrow = minuteMETsNarrow,
  minuteSleep = minuteSleep,
  minuteStepsNarrow = minuteStepsNarrow,
  minuteStepsWide = minuteStepsWide,
  sleepDaily = sleepDaily,
  weightLogInfo = weightLogInfo
)

# Assuming fitbit_data is already defined as a list containing your datasets

# Create an empty list to store the results
missing_value_counts <- vector("list", length = length(fitbit_data))

# Loop through each dataset in fitbit_data
for (i in seq_along(fitbit_data)) {
  dataset <- fitbit_data[[i]]  # Extract the dataset from the list
  missing_counts <- colSums(is.na(dataset))  # Calculate column-wise NA counts
  missing_value_counts[[i]] <- missing_counts  # Store the result in the list
}

# Print or view the results
for (i in seq_along(fitbit_data)) {
  cat("Dataset:", names(fitbit_data)[i], "\n")
  print(missing_value_counts[[i]])
  cat("\n")
}


# Initialize an empty list to store column names
column_names_list <- list()

# Iterate through each element of fitbit_data
for (df_name in names(fitbit_data)) {
  # Get the data frame
  df <- fitbit_data[[df_name]]
  
  # Get the column names of the current data frame
  column_names <- colnames(df)
  
  # Store the column names in the column_names_list
  column_names_list[[df_name]] <- column_names
}

# Print the column names for each data frame (just for verification)
print(column_names_list)



# Assuming fitbit_data is a list of 18 CSV files, each loaded as a data frame
# Example of how fitbit_data might be initialized (adjust as per your actual data loading method):
# fitbit_data <- lapply(list.files(pattern = "*.csv"), read.csv)

# Initialize an empty data frame
csv_tables_and_columns <- data.frame(
  table_name = character(),
  column_name = character(),
  stringsAsFactors = FALSE
)

# Iterate through each file (data frame) in fitbit_data
for (i in seq_along(fitbit_data)) {
  file <- fitbit_data[[i]]
  file_name <- names(fitbit_data)[i]  # Get the name of the file (table name)
  column_names <- names(file)  # Get column names of the file
  
  # Create a temporary data frame for the current file
  temp_df <- data.frame(
    table_name = rep(file_name, length(column_names)),
    column_name = column_names,
    stringsAsFactors = FALSE
  )
  
  # Append the temporary data frame to csv_tables_and_columns
  csv_tables_and_columns <- rbind(csv_tables_and_columns, temp_df)
}

# Reset row names if necessary
rownames(csv_tables_and_columns) <- NULL

# Print the first few rows of the resulting data frame
head(csv_tables_and_columns)

View(csv_tables_and_columns)






