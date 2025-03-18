# Load necessary libraries
library(dplyr)
library(ordinal) # For the clm() function
library(ggplot2)
library(cowplot)
library(afex) # For the mixed() function

# Load the CSV file
file_path <- "Pilot Data/all_trials.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Count unique participant_id values
unique_count <- n_distinct(df$participant_id)
print(unique_count)

# Initialize a list to store results
model_results <- list()

# Initialize a list to store plots
plots <- list()

# Ensure 'condition' is a factor and apply sum contrasts
df$delay <- factor(df$delay)
df$reward <- factor(df$reward)
df$effort <- factor(df$effort)
df$participant_id <- factor(df$participant_id)

# Fit a cumulative model for each aspect
model_mixed <- mixed(acceptance ~ delay * reward * effort + (1 | participant_id),
               data = df, method = "LRT", family = binomial(link = "logit"))
# nAGQ = 10,
# control = clmm.control(maxIter = 200, maxLineIter = 200, trace = TRUE),
#trace = TRUE)  # Debug optimization process


# Store the results
print(model_mixed)

summary(model_mixed)

levels(df$reward)
contrasts(df$reward)
