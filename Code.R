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
# ==============================================================================
# PHASE 4: ASSUMPTION TESTING (The "V. Good" Marks)
# Objective: Check Normality and Homogeneity of Variance
# ==============================================================================

# 1. Normality Check (Shapiro-Wilk Test)
# Null Hypothesis: Data is Normal.
# P < 0.05 means Data is NOT Normal (Significant deviation).
# Note: With large sample sizes (N > 50), this test is very sensitive. 
# Refer to your histograms (Phase 3) as the primary evidence.

print("--- Normality Test (Normal Group) ---")
print(shapiro.test(data_normal$baseline.value))

print("--- Normality Test (Pathological Group) ---")
print(shapiro.test(data_patho$baseline.value))

# 2. Variance Check (F-test / Levene's Equivalent)
# Null Hypothesis: Variances are equal.
# If p < 0.05, we must use the "Welch" correction in the T-test.

print("--- Variance Test ---")
var_test_result <- var.test(baseline.value ~ fetal_health, data = study_data)
print(var_test_result)
# ==============================================================================
# PHASE 5: HYPOTHESIS TESTING
# Objective: Run the statistical test to answer the Research Question.
# ==============================================================================

# We run the Independent Samples T-Test.
# We set var.equal based on the result of Phase 4 (usually FALSE is safer/default).

print("--- Independent Samples T-Test Results ---")
t_test_result <- t.test(baseline.value ~ fetal_health, 
                        data = study_data, 
                        var.equal = FALSE) # Welch's T-test (safer assumption)

print(t_test_result)

# ALTERNATIVE: Mann-Whitney U Test (Non-Parametric)
# If your histograms looked very skewed (not bell-shaped), use this result instead.
print("--- Mann-Whitney U Test Results (Non-Parametric) ---")
wilcox_result <- wilcox.test(baseline.value ~ fetal_health, data = study_data)
print(wilcox_result)


# 3. Create Plot 1: Normal Group (Blue)
data_normal <- subset(study_data, fetal_health == "Normal")

p1 <- ggplot(data_normal, aes(x = baseline.value)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_normal$baseline.value), 
                            sd = sd(data_normal$baseline.value)), 
                color = "red", size = 1.2) +
  labs(title = "Distribution of Heart Rate: Normal Group",
       x = "Baseline Heart Rate (bpm)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# 4. Create Plot 2: Pathological Group (Red)
data_patho <- subset(study_data, fetal_health == "Pathological")

p2 <- ggplot(data_patho, aes(x = baseline.value)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "salmon", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_patho$baseline.value), 
                            sd = sd(data_patho$baseline.value)), 
                color = "darkred", size = 1.2) +
  labs(title = "Distribution of Heart Rate: Pathological Group",
       x = "Baseline Heart Rate (bpm)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# 5. Combine and Save (The Critical Step)
# This arranges them in 2 columns (side by side)
combined_plot <- grid.arrange(p1, p2, ncol = 2)

# Save as a PNG file
# width = 10, height = 5 ensures the aspect ratio is wide and professional
ggsave("Figure1_HeartRate_Distribution.png", plot = combined_plot, width = 10, height = 5, dpi = 300)

print("Success! Check your folder for 'Figure1_HeartRate_Distribution.png'")

