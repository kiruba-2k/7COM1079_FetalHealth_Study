# 1. Load or Installation of  necessary libraries
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra") # Required for combining plots


# Here we are importing required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
raw_data <- read.csv("fetal_health.csv")


# According to the assignment requirement, we retain 1 (Normal) and 3 (Pathological). We remove 2 (Suspect).
study_data <- raw_data %>%
  filter(fetal_health != 2)

# We must convert this column to a Factor so R treats it as distinct groups.

study_data$fetal_health <- factor(study_data$fetal_health, 
                                  levels = c(1, 3), 
                                  labels = c("Normal", "Pathological"))


# Check that we only have two levels now
print("Table of Counts per Group:")
print(table(study_data$fetal_health))

# Check the structure to ensure the variable type is 'Factor'
print("Variable Structure:")
str(study_data$fetal_health)

# We calculate stats for each group to discuss in the "Analysis" section.

stats_summary <- study_data %>%
  group_by(fetal_health) %>%
  summarise(
    Count = n(),
    Mean_HeartRate = mean(baseline.value, na.rm = TRUE),
    SD_HeartRate = sd(baseline.value, na.rm = TRUE)
  )

print("Descriptive Statistics:")
print(stats_summary)


# We create a subset just for the Normal group to plot its specific curve
data_normal <- subset(study_data, fetal_health == "Normal")

p1 <- ggplot(data_normal, aes(x = baseline.value)) +
  # The Histogram (Blue bars)
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  # The Normal Curve (Red line) - Calculated using the Mean/SD of THIS group
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_normal$baseline.value), 
                            sd = sd(data_normal$baseline.value)), 
                color = "red", size = 1.2) +
  labs(title = "Distribution of Heart Rate: Normal Group",
       x = "Baseline Heart Rate (bpm)", y = "Density") +
  theme_minimal()

print(p1)

# Here will create Pathological Group and print
data_patho <- subset(study_data, fetal_health == "Pathological")

p2 <- ggplot(data_patho, aes(x = baseline.value)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "salmon", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_patho$baseline.value), 
                            sd = sd(data_patho$baseline.value)), 
                color = "darkred", size = 1.2) +
  labs(title = "Distribution of Heart Rate: Pathological Group",
       x = "Baseline Heart Rate (bpm)", y = "Density") +
  theme_minimal()

print(p2)
