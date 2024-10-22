####################################
#   Load Required Packages         #
####################################
install.packages("tidyverse")
library(tidyverse)
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
library(dplyr)
library(ggplot2)
library(lubridate)  # For handling date-time

####################################
#   Uploading Datasets             #
####################################
# Uploading all the data into variables
dailyActivity <- read_csv("Fitbit Data/dailyActivity_merged.csv")
dailyCalories <- read_csv("Fitbit Data/dailyCalories_merged.csv")
dailyIntensities <- read_csv("Fitbit Data/dailyIntensities_merged.csv")
dailySteps <- read_csv("Fitbit Data/dailySteps_merged.csv")
heartRateSeconds <- read_csv("Fitbit Data/heartrate_seconds_merged.csv")
hourlyCalories <- read_csv("Fitbit Data/hourlyCalories_merged.csv")
hourlyIntensities <- read_csv("Fitbit Data/hourlyIntensities_merged.csv")
hourlySteps <- read_csv("Fitbit Data/hourlySteps_merged.csv")
minuteCaloriesNarrow <- read_csv("Fitbit Data/minuteCaloriesNarrow_merged.csv")
minuteCaloriesWide <- read_csv("Fitbit Data/minuteCaloriesWide_merged.csv")
minuteIntensitiesNarrow <- read_csv("Fitbit Data/minuteIntensitiesNarrow_merged.csv")
minuteIntensitiesWide <- read_csv("Fitbit Data/minuteIntensitiesWide_merged.csv")
minuteMETsNarrow <- read_csv("Fitbit Data/minuteMETsNarrow_merged.csv")
minuteSleep <- read_csv("Fitbit Data/minuteSleep_merged.csv")
minuteStepsNarrow <- read_csv("Fitbit Data/minuteStepsNarrow_merged.csv")
minuteStepsWide <- read_csv("Fitbit Data/minuteStepsWide_merged.csv")
sleepDaily <- read_csv("Fitbit Data/sleepDay_merged.csv")
weightLogInfo <- read_csv("Fitbit Data/weightLogInfo_merged.csv")

# View initial dataset
View(weightLogInfo)
head(dailyActivity)

####################################
#   Checking for Missing Values     #
####################################
# Check for missing values in each dataset
missing_values <- lapply(list(dailyActivity, dailyCalories, dailyIntensities, dailySteps,
                              heartRateSeconds, hourlyCalories, hourlyIntensities,
                              hourlySteps, minuteCaloriesNarrow, minuteCaloriesWide,
                              minuteIntensitiesNarrow, minuteIntensitiesWide,
                              minuteMETsNarrow, minuteSleep, minuteStepsNarrow,
                              minuteStepsWide, sleepDaily, weightLogInfo), 
                         function(x) colSums(is.na(x)))

# Remove "Fat" column due to missing values in weightLogInfo
weightLogInfo <- weightLogInfo %>% select(-Fat)

####################################
#   Exploring The Data             #
####################################
# Summary statistics for the weight log info
skim_without_charts(weightLogInfo)

####################################
#   Viewing Summary Statistics     #
####################################
# Summary statistics for all datasets
datasets <- list(dailyActivity, dailyCalories, dailyIntensities, dailySteps,
                 heartRateSeconds, hourlyCalories, hourlyIntensities,
                 hourlySteps, minuteCaloriesNarrow, minuteCaloriesWide,
                 minuteIntensitiesNarrow, minuteIntensitiesWide,
                 minuteMETsNarrow, minuteSleep, minuteStepsNarrow,
                 minuteStepsWide, sleepDaily, weightLogInfo)

lapply(datasets, summary)

# View information about the shape of the data
lapply(datasets, skim_without_charts)

####################################
#   Analyzing Daily Activity Data   #
####################################
# Calculate average calories burned by each user
average_calories <- dailyActivity %>%
  group_by(Id) %>%
  summarize(AverageCalories = mean(Calories, na.rm = TRUE)) %>%
  arrange(AverageCalories)

# Display the average calories
print(average_calories)

# Plot average calories burned by each user with limits
lower_limit <- 1600  # Example lower limit
upper_limit <- 2200  # Example upper limit

ggplot(average_calories, aes(x = reorder(factor(Id), AverageCalories), y = AverageCalories)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = lower_limit, linetype = "dashed", color = "red") +
  geom_hline(yintercept = upper_limit, linetype = "dashed", color = "red") +
  labs(title = "Average Calories Burned by Each User",
       x = "User ID",
       y = "Average Calories Burned") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  annotate("text", x = 1, y = lower_limit + 50, label = "Lower Limit based on Healthline", color = "red", vjust = 0) +
  annotate("text", x = 1, y = upper_limit + 50, label = "Upper Limit based on Healthline", color = "red", vjust = 0)

# Calculate activity level percentages for each user
activity_percentages <- dailyActivity %>%
  mutate(TotalMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes) %>%
  mutate(SedentaryPercentage = (SedentaryMinutes / TotalMinutes) * 100,
         FairlyActivePercentage = (FairlyActiveMinutes / TotalMinutes) * 100,
         VeryActivePercentage = (VeryActiveMinutes / TotalMinutes) * 100,
         LightlyActivePercentage = (LightlyActiveMinutes / TotalMinutes) * 100) %>%
  select(Id, ActivityDate, SedentaryPercentage, FairlyActivePercentage, VeryActivePercentage, LightlyActivePercentage)

# Transform the dataframe to long format for plotting
long_dataframe <- activity_percentages %>%
  pivot_longer(cols = c(SedentaryPercentage, FairlyActivePercentage, VeryActivePercentage, LightlyActivePercentage), 
               names_to = "ActivityLevel", 
               values_to = "Percentage")

# Convert ActivityDate to Date type for proper plotting
long_dataframe <- long_dataframe %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y"))

# Plot activity level percentages by user
ggplot(long_dataframe, aes(x = ActivityDate, y = Percentage, color = ActivityLevel)) +
  geom_line() + 
  facet_wrap(~ Id) + 
  labs(title = "Activity Level Percentages by Day - Indicator of how users spend most of their time", 
       x = "Activity Date", 
       y = "Percentage (%)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################################
#   Analyzing Weight Log Data      #
####################################
# Convert the Date column to Date type
weightLogInfo$Date <- as.Date(weightLogInfo$Date, format = "%m/%d/%Y %I:%M:%S %p")

# Get earliest BMI record for each user
earliestBMI <- weightLogInfo %>%
  group_by(Id) %>%
  slice_min(order_by = Date) %>%
  ungroup() %>%
  select(Id, BMI)

# Define BMI categories
bmiData <- earliestBMI %>%
  mutate(Category = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 24.9 ~ "Healthy weight",
    BMI >= 25 & BMI < 29.9 ~ "Overweight",
    BMI >= 30 ~ "Obese",
    TRUE ~ "Unknown"
  ))

# Plot BMI by user ID
ggplot(bmiData, aes(x = factor(Id), y = BMI, color = Category)) +
  geom_point(size = 3) +
  labs(title = "BMI of Individuals by ID",
       x = "ID",
       y = "BMI") +
  scale_color_manual(values = c("Underweight" = "blue", 
                                "Healthy weight" = "green", 
                                "Overweight" = "orange", 
                                "Obese" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide x-axis labels
        axis.ticks.x = element_blank())  # Hide x-axis ticks

####################################
#   Analyzing Heart Rate Data      #
####################################
# Convert the Time column to POSIXct format
heartRateSeconds$Time <- as.POSIXct(heartRateSeconds$Time, format = "%m/%d/%Y %I:%M:%S %p")

# Sample a fraction of the heart rate data (e.g., 1%)
set.seed(123)
sampledData <- heartRateSeconds %>%
  sample_frac(0.01)

# Create the line plot with facets for each user ID
ggplot(sampledData, aes(x = Time, y = Value)) +
  geom_line(alpha = 0.5) +
  labs(title = "Heart Rate Over Time (Sampled Data)",
       x = "Time",
       y = "Heart Rate (BPM)") +
  facet_wrap(~ Id, scales = "free_y") +  # Create separate plots for each ID
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend

# Calculate mean heart rate for each user
meanHeartRate <- heartRateSeconds %>%
  group_by(Id) %>%
  summarize(MeanHeartRate = mean(Value, na.rm = TRUE), .groups = 'drop')

# Create the bar chart for mean heart rate
ggplot(meanHeartRate, aes(x = factor(Id), y = MeanHeartRate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 60, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  labs(title = "Mean Heart Rate by User ID",
       x = "User ID",
       y = "Mean Heart Rate (BPM)",
       caption = "Dashed lines represent healthy heart rate range (60-100 BPM)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate variance of heart rate for each user
varianceHeartRate <- heartRateSeconds %>%
  group_by(Id) %>%
  summarize(VarianceHeartRate = var(Value, na.rm = TRUE), .groups = 'drop')

# Calculate variance from a healthy point (80 BPM)
healthyPoint <- 80
varianceHeartRate <- varianceHeartRate %>%
  mutate(VarianceFromHealthy = abs(VarianceHeartRate - healthyPoint))

# Create the bar chart for heart rate variance
ggplot(varianceHeartRate, aes(x = factor(Id), y = VarianceFromHealthy)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Variance of Heart Rate from Healthy Point (80 BPM) by User ID",
       x = "User ID",
       y = "Variance from Healthy Point (BPM)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################################
#   Analyzing Sleep Data           #
####################################
# Reshape sleep data for analysis
sleepDataLong <- sleepDaily %>%
  group_by(Id) %>%
  summarize(TotalTimeInBed = sum(TotalTimeInBed), TotalMinutesAsleep = sum(TotalMinutesAsleep), .groups = 'drop') %>%
  pivot_longer(cols = c(TotalTimeInBed, TotalMinutesAsleep),
               names_to = "TimeType",
               values_to = "Minutes")

# Create the bar chart for time in bed vs time spent sleeping
ggplot(sleepDataLong, aes(x = Id, y = Minutes, fill = TimeType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Time in Bed vs Time Spent Sleeping by User ID",
       x = "User ID",
       y = "Minutes",
       fill = "Time Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate average sleep values for the pie chart
averageSleep <- sleepDaily %>%
  summarize(AverageSleep = mean(TotalMinutesAsleep), 
            AverageTimeInBed = mean(TotalTimeInBed),
            AverageAwakeTime = mean(TotalTimeInBed - TotalMinutesAsleep)) %>%
  select(AverageSleep, AverageAwakeTime) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Minutes") %>%
  mutate(Percentage = Minutes / sum(Minutes) * 100)  # Calculate percentages

# Create the pie chart for average time asleep vs time awake
ggplot(averageSleep, aes(x = "", y = Minutes, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Average Time Sleeping vs Time Awake",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  # Remove x-axis text
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))  # Add percentage labels
