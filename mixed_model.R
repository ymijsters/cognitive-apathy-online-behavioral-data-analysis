# Load necessary libraries
library(lme4)  # For glmer()
library(ggplot2)
library(dplyr)

# Load the CSV file (make sure to update the path)
file_path <- "Pilot Data/all_trials.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Check for the unique participant IDs (this looks good)
unique_count <- n_distinct(df$participant_id)
print(unique_count)

# Ensure factors are correctly specified (factorizing necessary columns)
df$delay <- factor(df$delay)
df$reward <- factor(df$reward)
df$effort <- factor(df$effort)
df$participant_id <- factor(df$participant_id)

# Fit the GLMM (Logistic model with random intercept for Participant ID)
model <- glmer(acceptance ~ delay * reward * effort + (1 | participant_id), 
               data = df, 
               family = binomial)

# Store the results
print(model)
summary(model)

