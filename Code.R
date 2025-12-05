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
