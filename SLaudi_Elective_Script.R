# ==============================================================================
# ==============================================================================
# This script serves as a supplement to the written final elective report for the elective "Monitoring craving and snacking behaviors". It is split into two sections, in accordance with the written report: 1) data and network estimation and 2) network comparison. The first section includes code that checks for assumptions, evaluates correlation between variables, identifies outliers and justifies their exclusion, generates plots and diagrams for data visualisation, and sets up for network plotting. The second section comprises code for different approaches that account for the density disparity between the groups (in number of observations).

# Author: Sonya Laudi (i6270576)
# Supervisor: Leonardo Pimpini
# Date: 24th July 2025
# ==============================================================================

# 1: data and network estimation

# A) Check mlVAR assumptions. 

  # Install and load readxl package, and set input path to condition files.
install.packages("readxl")
library(readxl)

High_freq <- read_xlsx("/Users/Sonya/Downloads/Elective with Leonardo/EMA Datasets (Clean; FINAL with 0 instead of NA)/HIgh_freq.xlsx")
Med_freq <- read_xlsx("C:/Users/Sonya/Downloads/Elective with Leonardo/EMA Datasets (Clean; FINAL with 0 instead of NA)/Medium_freq.xlsx")
Low_freq <- read_xlsx("C:/Users/Sonya/Downloads/Elective with Leonardo/EMA Datasets (Clean; FINAL with 0 instead of NA)/Low_freq.xlsx")

    #Convert Self_Control and Stress from character type to numeric type variables. 
cols_to_convert <- c("Self_Control", "Stress")
High_freq[cols_to_convert] <- lapply(High_freq[cols_to_convert], as.numeric)
Med_freq[cols_to_convert] <- lapply(Med_freq[cols_to_convert], as.numeric)
Low_freq[cols_to_convert] <- lapply(Low_freq[cols_to_convert], as.numeric)

  # Assumption: Normal distribution of variables.
install.packages("MVN")
library(MVN)

    #Test multivariate normality using Mardia's test.
vars_H <- High_freq[,c("CD", "CA", "SA", "Self_Control", "Stress")]
vars_M <- Med_freq[,c("CD", "CA", "SA", "Self_Control", "Stress")]
vars_L <- Low_freq[,c("CD", "CA", "SA", "Self_Control", "Stress")]
#CD = Craving Degree, CA = Craving Amount, SA = Snacking Amount
vars <- ("vars_H", "vars_M", "vars_L")
mvn(data = vars_H, mvnTest = "mardia")
mardia(vars_H)

#//957 rows with missing values removed.//

mvn(data = vars_M, mvnTest = "mardia")
mardia(vars_M)

#//722 rows with missing values removed.//

mvn(data = vars_L, mvnTest = "mardia")
mardia(vars_L)

#//396 rows with missing values removed.//

  # Assumption: Stationarity.

    #Install and load required packages and libraries.
install.packages("mlVAR")
library(mlVAR)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggraph")
library(ggraph)
install.packages("tidyr")
library(tidyr)
install.packages("tidygraph")
library(tidygraph)
install.packages("dplyr")
library(dplyr)
install.packages("showtext")
library(showtext)
install.packages("tseries")
library(tseries)

    #De-trending.

    # Confirm that no NAs are in the data frame.

      #High frequency:
sum(is.na(High_freq$CD))
# 944
sum(is.na(High_freq$CA))
# 944
sum(is.na(High_freq$SA))
# 948
sum(is.na(High_freq$Self_Control))
# 953
sum(is.na(High_freq$Stress))
# 956

      #Medium frequency:
sum(is.na(Med_freq$CD))
# 713
sum(is.na(Med_freq$CA))
# 713
sum(is.na(Med_freq$SA))
# 716
sum(is.na(Med_freq$Self_Control))
# 720
sum(is.na(Med_freq$Stress))
# 720

      #Low frequency:
sum(is.na(Low_freq$CD))
# 392
sum(is.na(Low_freq$CA))
# 392
sum(is.na(Low_freq$SA))
# 395
sum(is.na(Low_freq$Self_Control))
# 395
sum(is.na(Low_freq$Stress))
# 394

    #Define the variables to check.
variables_to_check <- c("CD", "CA", "SA", "Self_Control", "Stress")

    #Check if variables are numeric.

     #High frequency:
check_numeric_H <- function(data, vars_H) {
  for (var in vars_H) {
    if (is.numeric(data[[var]])) {
      cat(sprintf("The variable '%s' is numeric.\n", var))
    } else {
      cat(sprintf("The variable '%s' is NOT numeric. It is of type '%s'.\n", var, class(data[[var]])[1]))
    }
  }
}

check_numeric_H(High_freq, variables_to_check)

#//All variables are numeric.//

     #Medium frequency:
check_numeric_M <- function(data, vars_M) {
  for (var in vars_M) {
    if (is.numeric(data[[var]])) {
      cat(sprintf("The variable '%s' is numeric.\n", var))
    } else {
      cat(sprintf("The variable '%s' is NOT numeric. It is of type '%s'.\n", var, class(data[[var]])[1]))
    }
  }
}

check_numeric_M(Med_freq, variables_to_check)

#//All variables are numeric.//

     #Low frequency:
check_numeric_L <- function(data, vars_L) {
  for (var in vars_L) {
    if (is.numeric(data[[var]])) {
      cat(sprintf("The variable '%s' is numeric.\n", var))
    } else {
      cat(sprintf("The variable '%s' is NOT numeric. It is of type '%s'.\n", var, class(data[[var]])[1]))
    }
  }
}

check_numeric_L(Low_freq, variables_to_check)

#//All variables are numeric.//

    #Check for correlations among variables.

     #High frequency:
cor(High_freq[, c("CD", "CA", "SA", "Self_Control", "Stress")], use = "complete.obs")

     #Medium frequency:
cor(Med_freq[, c("CD", "CA", "SA", "Self_Control", "Stress")], use = "complete.obs")

     #Low frequency:
cor(Low_freq[, c("CD", "CA", "SA", "Self_Control", "Stress")], use = "complete.obs")

# B) Identify outliers and justify their exclusion, re-run network estimation, and generate exclusion reports.

  # Load required packages.
install.packages("lubridate")
library(lubridate)
install.packages("esmtools")
library(esmtools)
install.packages("naniar")
library(naniar)
install.packages("tibble")
library(tibble)

# Bi) Identify participants to exclude (due to no variance, <20 observations) in each condition.
  
# HIGH FREQUENCY #

  # Inspect variables of interest, printing the mean, SD, and range for each.
High_freq$ID <- as.character(High_freq$ID)
vars <- c("CD", "CA", "SA", "Self_Control", "Stress")
High_freq$Date_Time <- as.POSIXct(High_freq$Date_Time)

summary_HF <- High_freq %>%
  select(ID, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    n = n_distinct(ID[!is.na(Value)])
  ) %>%
  mutate(range = paste0(min, " – ", max)) %>%
  select(Variable, mean, sd, range, n)

print(summary_HF, n = Inf)

  # Plot the variable distribution per participant average.
output_dir <- "/Users/Sonya/Downloads/Elective with Leonardo/Generated Output" # set output directory
plot_dir <- file.path(output_dir, "Variable Distribution") #set file path (where plots can be found once generated)
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

avg_scores_H <- High_freq %>%
  group_by(ID) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Mean_Score")

avg_scores_H$Variable <- factor(avg_scores_H$Variable, 
                              levels = c("CA", "SA", "CD", "Self_Control", "Stress"))

for (v in levels(avg_scores_H$Variable)) {
  p <- ggplot(avg_scores_H %>% filter(Variable == v), aes(x = Variable, y = Mean_Score, label = ID)) +
    geom_text(size = 3.5, position = position_jitter(width = 0.15, height = 0)) +
    theme_bw(base_size = 14) +
    labs(title = paste("Per-Participant Average:", v), y = "Average Score", x = NULL) +
    scale_y_continuous(limits = if (v %in% c("CD", "Self_Control", "Stress")) c(0, 100) else NULL)
  
  filename_safe <- paste0("Distribution_", gsub(" ", "_", v), ".png")
  ggsave(file.path(plot_dir, filename_safe), plot = p, width = 8, height = 6)
}

  # Visualise the data per participant, per assessment. 
HF_long <- High_freq %>%
  select(ID, Timepoint, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  group_by(ID, Variable) %>%
  arrange(Timepoint) %>%
  mutate(Timepoint = row_number()) %>%
  ungroup()

plot_group <- function(participant_id, vars_subset, title_suffix) {
  HF_sub <- HF_long %>% filter(ID == participant_id, Variable %in% vars_subset)
  pH <- ggplot(HF_sub, aes(x = Timepoint, y = Score, color = Variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    theme_bw(base_size = 14) +
    labs(title = paste("Participant", participant_id, "-", title_suffix),
         x = "Assessment",
         y = "Score",
         color = "Variable") +
    theme(legend.position = "right")
  print(pH)
}

for (participant in unique(High_freq$ID)) {
  plot_group(participant, c("CD", "Self_Control", "Stress"), "Craving Degree / Self Control / Stress")
  plot_group(participant, c("CA", "SA"), "Craving Amount / Snacking Amount")
}

  # Calendar Plots (when pp were assessed).
heatcalendar_plot(High_freq, "Date_Time")
calendar_plot(High_freq, "Date_Time")

  # Missingness plot per variable.
vis_miss(High_freq) +
  theme(axis.text.x = element_text(size = 9, angle = 80))

miss_var_summary(High_freq)

gg_miss_case(High_freq)

gg_miss_upset(High_freq[, c('CD', 'CA', 'SA', 'Self_Control', 'Stress')], nsets = 5)

  # Initialise tracking data frame for de-trending.
detrendsHF <- data.frame(matrix(ncol = length(vars), nrow = length(unique(High_freq$ID))))
colnames(detrendsHF) <- vars
rownames(detrendsHF) <- as.character(unique(High_freq$ID))  # ensure row names are character

  # Track variables excluded from de-trending
excluded_detrendHF <- data.frame(ID = character(), variable = character(), reason = character(), stringsAsFactors = FALSE)

  # Loop through each participant for each condition.

    # Round doubles off to integers (suggested by ChatGPT).
vars_H_rounded <- round(vars_H)

for (current_id in unique(High_freq$ID)) {
  row_indices <- which(High_freq$ID == current_id)
  pp <- High_freq[row_indices, ]
  
  for (current_var in names(vars_H_rounded)) { # suggestion to add names() to circumvent an issue with naming by ChatGPT; remaining script for this section from supervisor
    values <- pp[[current_var]]
    complete_cases <- complete.cases(values, pp$Date_Time)
  
    #Exclude pp if there is no variance:
    if (length(unique(values[complete_cases])) <= 1) {
      excluded_detrend <- rbind(excluded_detrendHF, data.frame(ID = current_id, Variable = current_var, reason = "No variance"))
      next
    }
    
    #Exclude pp if too few data points were found (<20 complete cases (i.e., time-value pairs)):
    if (sum(complete_cases) < 20) {
      excluded_detrend <- rbind(excluded_detrend, data.frame(ID = current_id, variable = current_var, reason = "Too few observations (i.e., < 20)"))
      next
    }
    
    #Fit trend model:
    fitHF <- lm(values[complete_cases] ~ pp$Date_Time[complete_cases])
    
    #If significant trend is observed, apply de-trending:
    if (anova(fitHF)$`Pr(>F)`[1] < 0.05) {
      detrendsHF[as.character(current_id), current_var] <- 1
      residuals_adjusted <- residuals(fitHF) + mean(values[complete_cases], na.rm = TRUE)
      High_freq[row_indices[complete_cases], current_var] <- residuals_adjusted
    } else {
      detrendsHF[as.character(current_id), current_var] <- 0
    }
  }
}

  # Check for missing data (exclude pp with fewer than 20 observation in any variable) and flag (but doesn't discard) zero-variance variables.

    #Count non-missing observations per (id, variable):
obs_summary_H <- High_freq %>%
  pivot_longer(all_of(names(vars_H)), names_to = "variable", values_to = "value") %>%
  group_by(ID, variable) %>%
  summarise(n_obs = sum(!is.na(value)), .groups = "drop")

    #Find IDs with any variable <20 obs:
excluded_few_obs_H <- obs_summary_H %>%
  filter(n_obs < 20) %>%
  distinct(ID) %>%
  pull(ID)

summary(obs_summary_H$n_obs)

    #Create a one-column table of excluded IDs (useful later for plotting networks):
excluded_missing_H <- data.frame(ID = excluded_few_obs_H)

    #Report and drop those participants:
cat("Excluding", length(excluded_few_obs_H),
    "participant(s) with <20 observations on at least one variable H.\n")
df_H <- High_freq %>% filter(!ID %in% excluded_few_obs_H)

    #Compute per-participant variance for each variable:
zero_var_summary_H <- df_H %>%
  group_by(ID) %>%
  summarise(across(all_of(names(vars_H)), ~ var(.x, na.rm = TRUE),
                   .names = "var_{.col}"),
            .groups = "drop")

    #Flag zero-variance pairs:
excluded_zero_var_H <- zero_var_summary_H %>%
  pivot_longer(starts_with("var_"), names_to = "variable",
               values_to = "variance") %>%
  mutate(variable = sub("^var_", "", variable)) %>%
  filter(variance == 0)

cat("Identified", nrow(excluded_zero_var_H),
    "zero-variance variable(s) across participants H.\n")

    #Rename all variables for the network plots (var_labels):
vars_renamed <- c("CD", "CA", "SA", "Self_Control", "Stress")
var_labels <- c(
  "Craving Degree",
  "Craving Amount",
  "Snacking Amount",
  "Self-Control",
  "Stress"
)

# MED FREQUENCY #

# Inspect variables of interest, printing the mean, SD, and range for each.
Med_freq$ID <- as.character(Med_freq$ID)
Med_freq$Date_Time <- as.POSIXct(Med_freq$Date_Time)

summary_MF <- Med_freq %>%
  select(ID, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    n = n_distinct(ID[!is.na(Value)])
  ) %>%
  mutate(range = paste0(min, " – ", max)) %>%
  select(Variable, mean, sd, range, n)

print(summary_MF, n = Inf)

# Plot the variable distribution per participant average.
avg_scores_M <- Med_freq %>%
  group_by(ID) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Mean_Score")

avg_scores_M$Variable <- factor(avg_scores_M$Variable, 
                              levels = c("CA", "SA", "CD", "Self_Control", "Stress"))

for (v in levels(avg_scores_M$Variable)) {
  p <- ggplot(avg_scores_M %>% filter(Variable == v), aes(x = Variable, y = Mean_Score, label = ID)) +
    geom_text(size = 3.5, position = position_jitter(width = 0.15, height = 0)) +
    theme_bw(base_size = 14) +
    labs(title = paste("Per-Participant Average:", v), y = "Average Score", x = NULL) +
    scale_y_continuous(limits = if (v %in% c("CD", "Self_Control", "Stress")) c(0, 100) else NULL)
  
  filename_safe <- paste0("Distribution_", gsub(" ", "_", v), ".png")
  ggsave(file.path(plot_dir, filename_safe), plot = p, width = 8, height = 6)
}

# Visualise the data per participant, per assessment. 
MF_long <- Med_freq %>%
  select(ID, Timepoint, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  group_by(ID, Variable) %>%
  arrange(Timepoint) %>%
  mutate(Timepoint = row_number()) %>%
  ungroup()

plot_group <- function(participant_id, vars_subset, title_suffix) {
  MF_sub <- MF_long %>% filter(ID == participant_id, Variable %in% vars_subset)
  pM <- ggplot(MF_sub, aes(x = Timepoint, y = Score, color = Variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    theme_bw(base_size = 14) +
    labs(title = paste("Participant", participant_id, "-", title_suffix),
         x = "Assessment",
         y = "Score",
         color = "Variable") +
    theme(legend.position = "right")
  print(pM)
}

for (participant in unique(Med_freq$ID)) {
  plot_group(participant, c("CD", "Self_Control", "Stress"), "Craving Degree / Self Control / Stress")
  plot_group(participant, c("CA", "SA"), "Craving Amount / Snacking Amount")
}

# Calendar Plots (when pp were assessed).
heatcalendar_plot(Med_freq, "Date_Time")
calendar_plot(Med_freq, "Date_Time")

# Missingness plot per variable.
vis_miss(Med_freq) +
  theme(axis.text.x = element_text(size = 9, angle = 80))

miss_var_summary(Med_freq)

gg_miss_case(Med_freq)

gg_miss_upset(Med_freq[, c('CD', 'CA', 'SA', 'Self_Control', 'Stress')], nsets = 5)

# Initialize tracking data frame for de-trending.
detrendsMF <- data.frame(matrix(ncol = length(vars), nrow = length(unique(Med_freq$ID))))
colnames(detrendsMF) <- vars
rownames(detrendsMF) <- as.character(unique(Med_freq$ID))  # ensure row names are character

# Track variables excluded from detrending
excluded_detrendMF <- data.frame(ID = character(), variable = character(), reason = character(), stringsAsFactors = FALSE)

# Loop through each participant for each condition.

  # Round doubles off to integers.
vars_M_rounded <- round(vars_M)

for (current_id in unique(Med_freq$ID)) {
  row_indices <- which(Med_freq$ID == current_id)
  pp <- Med_freq[row_indices, ]
  
  for (current_var in names(vars_M_rounded)) {
    values <- pp[[current_var]]
    complete_cases <- complete.cases(values, pp$Date_Time)
    
    #Exclude pp if there is no variance:
    if (length(unique(values[complete_cases])) <= 1) {
      excluded_detrend <- rbind(excluded_detrendMF, data.frame(ID = current_id, Variable = current_var, reason = "No variance"))
      next
    }
    
    #Exclude pp if too few data points were found, that is, less than 20 complete cases (i.e., time-value pairs):
    if (sum(complete_cases) < 20) {
      excluded_detrend <- rbind(excluded_detrend, data.frame(ID = current_id, variable = current_var, reason = "Too few observations (i.e., < 20)"))
      next
    }
    
    #Fit trend model:
    fitMF <- lm(values[complete_cases] ~ pp$Date_Time[complete_cases])
    
    #If significant trend is observed, apply de-trending:
    if (anova(fitMF)$`Pr(>F)`[1] < 0.05) {
      detrendsMF[as.character(current_id), current_var] <- 1
      residuals_adjusted <- residuals(fitMF) + mean(values[complete_cases], na.rm = TRUE)
      Med_freq[row_indices[complete_cases], current_var] <- residuals_adjusted
    } else {
      detrendsMF[as.character(current_id), current_var] <- 0
    }
  }
}

# Check for missing data (exclude pp with fewer than 20 observation in any variable) and flag (but doesn't discard) zero-variance variables.

#Count non-missing observations per (id, variable):
obs_summary_M <- Med_freq %>%
  pivot_longer(all_of(names(vars_M)), names_to = "variable", values_to = "value") %>%
  group_by(ID, variable) %>%
  summarise(n_obs = sum(!is.na(value)), .groups = "drop")

#Find IDs with any variable <20 obs:
excluded_few_obs_M <- obs_summary_M %>%
  filter(n_obs < 20) %>%
  distinct(ID) %>%
  pull(ID)

summary(obs_summary_M$n_obs)

#Create a one-column table of excluded IDs (useful later for plotting networks):
excluded_missing_M <- data.frame(ID = excluded_few_obs_M)

#Report and drop those participants:
cat("Excluding", length(excluded_few_obs_M),
    "participant(s) with <20 observations on at least one variable M.\n")
df_M <- Med_freq %>% filter(!ID %in% excluded_few_obs_M)

#Compute per-participant variance for each variable:
zero_var_summary_M <- df_M %>%
  group_by(ID) %>%
  summarise(across(all_of(names(vars_M)), ~ var(.x, na.rm = TRUE),
                   .names = "var_{.col}"),
            .groups = "drop")

#Flag zero-variance pairs:
excluded_zero_var_M <- zero_var_summary_M %>%
  pivot_longer(starts_with("var_"), names_to = "variable",
               values_to = "variance") %>%
  mutate(variable = sub("^var_", "", variable)) %>%
  filter(variance == 0)

cat("Identified", nrow(excluded_zero_var_M),
    "zero-variance variable(s) across participants M.\n")

df_M_excl54559 <- df_M %>% filter(!(ID %in% c(54559))) #ID 54559 identified to have zero variance for Self-Control. (code originally provided by supervisor, syntax modification suggested by ChatGPT)

# LOW FREQUENCY #

# Inspect variables of interest, printing the mean, SD, and range for each.
Low_freq$ID <- as.character(Low_freq$ID)
Low_freq$Date_Time <- as.POSIXct(Low_freq$Date_Time)

summary_LF <- Low_freq %>%
  select(ID, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    n = n_distinct(ID[!is.na(Value)])
  ) %>%
  mutate(range = paste0(min, " – ", max)) %>%
  select(Variable, mean, sd, range, n)

print(summary_LF, n = Inf)

# Plot the variable distribution per participant average.
avg_scores_L <- Low_freq %>%
  group_by(ID) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Mean_Score")

avg_scores_L$Variable <- factor(avg_scores_L$Variable, 
                              levels = c("CA", "SA", "CD", "Self_Control", "Stress"))

for (v in levels(avg_scores_L$Variable)) {
  p <- ggplot(avg_scores_L %>% filter(Variable == v), aes(x = Variable, y = Mean_Score, label = ID)) +
    geom_text(size = 3.5, position = position_jitter(width = 0.15, height = 0)) +
    theme_bw(base_size = 14) +
    labs(title = paste("Per-Participant Average:", v), y = "Average Score", x = NULL) +
    scale_y_continuous(limits = if (v %in% c("CD", "Self_Control", "Stress")) c(0, 100) else NULL)
  
  filename_safe <- paste0("Distribution_", gsub(" ", "_", v), ".png")
  ggsave(file.path(plot_dir, filename_safe), plot = p, width = 8, height = 6)
}

# Visualise the data per participant, per assessment. 
LF_long <- Low_freq %>%
  select(ID, Timepoint, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  group_by(ID, Variable) %>%
  arrange(Timepoint) %>%
  mutate(Timepoint = row_number()) %>%
  ungroup()

plot_group <- function(participant_id, vars_subset, title_suffix) {
  LF_sub <- LF_long %>% filter(ID == participant_id, Variable %in% vars_subset)
  pL <- ggplot(LF_sub, aes(x = Timepoint, y = Score, color = Variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    theme_bw(base_size = 14) +
    labs(title = paste("Participant", participant_id, "-", title_suffix),
         x = "Assessment",
         y = "Score",
         color = "Variable") +
    theme(legend.position = "right")
  print(pL)
}

for (participant in unique(Low_freq$ID)) {
  plot_group(participant, c("CD", "Self_Control", "Stress"), "Craving Degree / Self Control / Stress")
  plot_group(participant, c("CA", "SA"), "Craving Amount / Snacking Amount")
}

# Calendar Plots (when pp were assessed).
heatcalendar_plot(Low_freq, "Date_Time")
calendar_plot(Low_freq, "Date_Time")

# Missingness plot per variable.
vis_miss(Low_freq) +
  theme(axis.text.x = element_text(size = 9, angle = 80))

miss_var_summary(Low_freq)

gg_miss_case(Low_freq)

gg_miss_upset(Low_freq[, c('CD', 'CA', 'SA', 'Self_Control', 'Stress')], nsets = 5)

# Initialize tracking data frame for de-trending.
detrendsLF <- data.frame(matrix(ncol = length(vars), nrow = length(unique(Low_freq$ID))))
colnames(detrendsLF) <- vars
rownames(detrendsLF) <- as.character(unique(Low_freq$ID))  # ensure row names are character

# Track variables excluded from de-trending
excluded_detrendLF <- data.frame(ID = character(), variable = character(), reason = character(), stringsAsFactors = FALSE)

# Loop through each participant for each condition.

  # Round doubles off to integers.
vars_L_rounded <- round(vars_L)

for (current_id in unique(Low_freq$ID)) {
  row_indices <- which(Low_freq$ID == current_id)
  pp <- Low_freq[row_indices, ]
  
  for (current_var in names(vars_M_rounded)) {
    values <- pp[[current_var]]
    complete_cases <- complete.cases(values, pp$Date_Time)
    
    #Exclude pp if there is no variance:
    if (length(unique(values[complete_cases])) <= 1) {
      excluded_detrend <- rbind(excluded_detrendLF, data.frame(ID = current_id, Variable = current_var, reason = "No variance"))
      next
    }
    
    #Exclude pp if too few data points were found, that is, less than 20 complete cases (i.e., time-value pairs):
    if (sum(complete_cases) < 20) {
      excluded_detrend <- rbind(excluded_detrend, data.frame(ID = current_id, variable = current_var, reason = "Too few observations (i.e., < 20)"))
      next
    }
    
    #Fit trend model:
    fitLF <- lm(values[complete_cases] ~ pp$Date_Time[complete_cases])
    
    #If significant trend is observed, apply de-trending:
    if (anova(fitLF)$`Pr(>F)`[1] < 0.05) {
      detrendsLF[as.character(current_id), current_var] <- 1
      residuals_adjusted <- residuals(fitLF) + mean(values[complete_cases], na.rm = TRUE)
      Low_freq[row_indices[complete_cases], current_var] <- residuals_adjusted
    } else {
      detrendsLF[as.character(current_id), current_var] <- 0
    }
  }
}

# Check for missing data (exclude pp with fewer than 20 observation in any variable) and flag (but doesn't discard) zero-variance variables.

#Count non-missing observations per (id, variable):
obs_summary_L <- Low_freq %>%
  pivot_longer(all_of(names(vars_L)), names_to = "variable", values_to = "value") %>%
  group_by(ID, variable) %>%
  summarise(n_obs = sum(!is.na(value)), .groups = "drop")

#Find IDs with any variable <20 obs:
excluded_few_obs_L <- obs_summary_L %>%
  filter(n_obs < 20) %>%
  distinct(ID) %>%
  pull(ID)

summary(obs_summary_L$n_obs)

#Create a one-column table of excluded IDs (useful later for plotting networks):
excluded_missing_L <- data.frame(ID = excluded_few_obs_L)

#Report and drop those participants:
cat("Excluding", length(excluded_few_obs_L),
    "participant(s) with <20 observations on at least one variable L.\n")
df_L <- Low_freq %>% filter(!ID %in% excluded_few_obs_L)

#Compute per-participant variance for each variable:
zero_var_summary_L <- df_L %>%
  group_by(ID) %>%
  summarise(across(all_of(names(vars_L)), ~ var(.x, na.rm = TRUE),
                   .names = "var_{.col}"),
            .groups = "drop")

#Flag zero-variance pairs:
excluded_zero_var_L <- zero_var_summary_L %>%
  pivot_longer(starts_with("var_"), names_to = "variable",
               values_to = "variance") %>%
  mutate(variable = sub("^var_", "", variable)) %>%
  filter(variance == 0)

cat("Identified", nrow(excluded_zero_var_L),
    "zero-variance variable(s) across participants L.\n")

# Bii) Run network estimation.

# HIGH FREQUENCY #

# Estimate 5 networks (contemp, temporal, BS, random effects contemp, random effects temporal).
library(mlVAR)
install.packages("qgraph")
library(qgraph)

mlVAR_res_H <- mlVAR(df_H, vars = vars, idvar = "ID", dayvar = "Day_nr", beepvar = "Timepoint", estimator = "lmer")

cont_H <- getNet(mlVAR_res_H, "contemporaneous", nonsig = "hide", rule = "and")
temp_H <- getNet(mlVAR_res_H, "temporal", nonsig = "hide")
bet_H  <- getNet(mlVAR_res_H, "between", nonsig = "hide", rule = "and")
Layout_H <- averageLayout(cont_H, temp_H, bet_H)

# Estimate random effects contemporaneous and temporal networks (individual SDs; BS random effects network is not available in mlVAR). 
SD_temp_H <- getNet(mlVAR_res_H, "temporal", SD = TRUE, lag = 1, partial = FALSE)
SD_contemp_H <- getNet(mlVAR_res_H, "contemporaneous", SD = TRUE, partial = FALSE)

network_plot_dir <- file.path(output_dir, "Network Plots")
if (!dir.exists(network_plot_dir)) dir.create(network_plot_dir, recursive = TRUE)

png(file.path(network_plot_dir, "RandomEffects_Temporal_H.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_temp_H, layout = Layout_H, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Temporal Random Effects for High Frequency")
dev.off()

png(file.path(network_plot_dir, "RandomEffects_Contemporaneous_H.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_contemp_H, layout = Layout_H, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Contemporaneous Random Effects for High Frequency")
dev.off()

# Plot Networks (5 in total).
plot_network <- function(net, title, filename) {
  png(file.path(network_plot_dir, filename), width = 1400, height = 1200, res = 200)
  qgraph(net,
         layout = "circular",     # Use circular layout for even spacing between nodes + center  
         repulsion = 1.5,       # Increase repulsion for more equal spacing between nodes
         theme = "colorblind",
         labels = var_labels,
         label.scale.equal = TRUE,
         vsize = 11,
         label.cex = 2.3,
         asize = 4.5,
         border.width = 3,
         border.color = "gray35",
         edge.labels = TRUE,
         edge.label.cex = 1,
         edge.label.position = 0.5,
         fade = FALSE,
         mar = c(6, 6, 6, 20),
         color = ifelse(vars %in% c("CD", "CA", "SA"), "lightyellow", "lightblue"),
         title = title)
  dev.off()
}

plot_network(cont_H, "Contemporaneous Network for High Frequency", "Contemporaneous_Network _H.png")
plot_network(temp_H, "Temporal Network for High Frequency", "Temporal_Network_H.png")
plot_network(bet_H,  "BS Network for High Frequency", "BS_Network_H.png")

# MED FREQUENCY #

# Estimate 5 networks (contemp, temporal, BS, random effects contemp, random effects temporal).
mlVAR_res_M <- mlVAR(df_M_excl54559, vars = vars, idvar = "ID", dayvar = "Day_nr", beepvar = "Timepoint", estimator = "lmer")

cont_M <- getNet(mlVAR_res_M, "contemporaneous", nonsig = "hide", rule = "and")
temp_M <- getNet(mlVAR_res_M, "temporal", nonsig = "hide")
bet_M  <- getNet(mlVAR_res_M, "between", nonsig = "hide", rule = "and")
Layout_M <- averageLayout(cont_M, temp_M, bet_M)

# Estimate random effects contemporaneous and temporal networks (individual SDs; BS random effects network is not available in mlVAR). 
SD_temp_M <- getNet(mlVAR_res_M, "temporal", SD = TRUE, lag = 1, partial = FALSE)
SD_contemp_M <- getNet(mlVAR_res_M, "contemporaneous", SD = TRUE, partial = FALSE)

png(file.path(network_plot_dir, "RandomEffects_Temporal_M.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_temp_M, layout = Layout_M, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Temporal Random Effects for Medium Frequency")
dev.off()

png(file.path(network_plot_dir, "RandomEffects_Contemporaneous_M.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_contemp_M, layout = Layout_M, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Contemporaneous Random Effects for Medium Frequency")
dev.off()

# Plot Networks (5 in total).
plot_network <- function(net, title, filename) {
  png(file.path(network_plot_dir, filename), width = 1400, height = 1200, res = 200)
  qgraph(net,
         layout = "circular",       
         repulsion = 1.5,       
         theme = "colorblind",
         labels = var_labels,
         label.scale.equal = TRUE,
         vsize = 11,
         label.cex = 2.3,
         asize = 4.5,
         border.width = 3,
         border.color = "gray35",
         edge.labels = TRUE,
         edge.label.cex = 1,
         edge.label.position = 0.5,
         fade = FALSE,
         mar = c(6, 6, 6, 20),
         color = ifelse(vars %in% c("CD", "CA", "SA"), "lightyellow", "lightblue"),
         title = title)
  dev.off()
}

plot_network(cont_M, "Contemporaneous Network for Medium Frequency", "Contemporaneous_Network_M.png")
plot_network(temp_M, "Temporal Network for Medium Frequency", "Temporal_Network_M.png")
plot_network(bet_M,  "BS Network for Medium Frequency", "BS_Network_M.png")

# LOW FREQUENCY #

# Estimate 5 networks (contemp, temporal, BS, random effects contemp, random effects temporal).
mlVAR_res_L <- mlVAR(df_L, vars = vars, idvar = "ID", dayvar = "Day_nr", beepvar = "Timepoint", estimator = "lmer")

cont_L <- getNet(mlVAR_res_L, "contemporaneous", nonsig = "hide", rule = "and")
temp_L <- getNet(mlVAR_res_L, "temporal", nonsig = "hide")
bet_L  <- getNet(mlVAR_res_L, "between", nonsig = "hide", rule = "and")
Layout_L <- averageLayout(cont_L, temp_L, bet_L)

# Estimate random effects contemporaneous and temporal networks (individual SDs; BS random effects network is not available in mlVAR). 
SD_temp_L <- getNet(mlVAR_res_L, "temporal", SD = TRUE, lag = 1, partial = FALSE)
SD_contemp_L <- getNet(mlVAR_res_L, "contemporaneous", SD = TRUE, partial = FALSE)

png(file.path(network_plot_dir, "RandomEffects_Temporal_L.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_temp_L, layout = Layout_L, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Temporal Random Effects for Low Frequency")
dev.off()

png(file.path(network_plot_dir, "RandomEffects_Contemporaneous_L.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_contemp_L, layout = Layout_L, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Contemporaneous Random Effects for Low Frequency")
dev.off()

# Plot Networks (5 in total).
plot_network <- function(net, title, filename) {
  png(file.path(network_plot_dir, filename), width = 1400, height = 1200, res = 200)
  qgraph(net,
         layout = "circular",     
         repulsion = 1.5,       
         theme = "colorblind",
         labels = var_labels,
         label.scale.equal = TRUE,
         vsize = 11,
         label.cex = 2.3,
         asize = 4.5,
         border.width = 3,
         border.color = "gray35",
         edge.labels = TRUE,
         edge.label.cex = 1,
         edge.label.position = 0.5,
         fade = FALSE,
         mar = c(6, 6, 6, 20),
         color = ifelse(vars %in% c("CD", "CA", "SA"), "lightyellow", "lightblue"),
         title = title)
  dev.off()
}

plot_network(cont_L, "Contemporaneous Network for Low Frequency", "Contemporaneous_Network_L.png")
plot_network(temp_L, "Temporal Network for Low Frequency", "Temporal_Network_L.png")
plot_network(bet_L,  "BS Network for Low Frequency", "BS_Network_L.png")

# Biii) Generate and save exclusion reports.

# HIGH FREQUENCY #

# excluded_detrend$id <- as.character(excluded_detrend$ID), if needed
excluded_missing_H$id <- as.character(excluded_missing_H$ID)
excluded_zero_var_H$id <- as.character(excluded_zero_var_H$ID)

all_exclusions_H <- bind_rows(
  #  excluded_detrend, if needed
  excluded_missing_H,
  excluded_zero_var_H
)

excl_dir <- file.path(output_dir, "Exclusion Reports")
if (!dir.exists(excl_dir)) dir.create(excl_dir, recursive = TRUE)

# write.table(excluded_detrend, file = file.path(excl_dir, "excluded_due_to_detrending.csv"), sep = ",", row.names = FALSE, col.names = TRUE), if needed
write.table(excluded_missing_H, file = file.path(excl_dir, "excluded_due_to_missingness_H.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(excluded_zero_var_H, file = file.path(excl_dir, "excluded_due_to_zero_variance_H.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(all_exclusions_H, file = file.path(excl_dir, "excluded_all_combined_H.csv"), sep = ",", row.names = FALSE, col.names = TRUE)

# MED FREQUENCY #

# excluded_detrend$id <- as.character(excluded_detrend$ID), if needed
excluded_missing_M$id <- as.character(excluded_missing_M$ID)
excluded_zero_var_M$id <- as.character(excluded_zero_var_M$ID)

all_exclusions_M <- bind_rows(
  #  excluded_detrend, if needed
  excluded_missing_M,
  excluded_zero_var_M
)

# write.table(excluded_detrend, file = file.path(excl_dir, "excluded_due_to_detrending.csv"), sep = ",", row.names = FALSE, col.names = TRUE), if needed
write.table(excluded_missing_M, file = file.path(excl_dir, "excluded_due_to_missingness_M.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(excluded_zero_var_M, file = file.path(excl_dir, "excluded_due_to_zero_variance_M.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(all_exclusions_M, file = file.path(excl_dir, "excluded_all_combined_M.csv"), sep = ",", row.names = FALSE, col.names = TRUE)

# LOW FREQUENCY #

# excluded_detrend$id <- as.character(excluded_detrend$ID), if needed
excluded_missing_L$id <- as.character(excluded_missing_L$ID)
excluded_zero_var_L$id <- as.character(excluded_zero_var_L$ID)

all_exclusions_L <- bind_rows(
  #  excluded_detrend, if needed
  excluded_missing_L,
  excluded_zero_var_L
)

# write.table(excluded_detrend, file = file.path(excl_dir, "excluded_due_to_detrending.csv"), sep = ",", row.names = FALSE, col.names = TRUE), if needed
write.table(excluded_missing_L, file = file.path(excl_dir, "excluded_due_to_missingness_L.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(excluded_zero_var_L, file = file.path(excl_dir, "excluded_due_to_zero_variance_L.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(all_exclusions_L, file = file.path(excl_dir, "excluded_all_combined_L.csv"), sep = ",", row.names = FALSE, col.names = TRUE)

#===============================================================================

# 2: network comparison

# Approaches to resolving group density disparity. 

# Approach 1: downsampling. (code suggested by ChatGPT to allow for stratification (preserving equal observations per ID, per day); was experimented with and successfully produced downsampled data)
library(dplyr)

set.seed(123)  

#Downsampling to 3 observations per ID per day; keeping the first and last + averaging the remainder into a mid-point. (chosen for this elective; originally taken from supervisor's code, minimally modified using ChatGPT to preserve no. of IDs per day within each condition.)
assign_to_time_slot <- function(Date_Time) {
  target_hours <- c(10, 16.5, 23)
  time_decimal <- hour(Date_Time) + minute(Date_Time) / 60
  slot_index <- sapply(time_decimal, function(x) which.min(abs(x - target_hours)))
  slot_labels <- c("10:00", "16:30", "23:00")
  slot_labels[slot_index]
}

downsample_to_3x_avg <- function(df) {
  df %>%
    mutate(Date_Time = as.POSIXct(Date_Time),
           time_slot = assign_to_time_slot(Date_Time)) %>%
    group_by(ID, Day_nr, time_slot) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(ID, Day_nr, time_slot)
}

downsample_to_3x_avg_preserve_IDs <- function(df) { # addition suggested by ChatGPT
  df %>%
    mutate(
      Date_Time = as.POSIXct(Date_Time),
      time_slot = assign_to_time_slot(Date_Time)
    ) %>%
    group_by(ID, Day_nr, time_slot) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
    complete(ID, Day_nr, time_slot = c("10:00", "16:30", "23:00")) %>%  
    arrange(ID, Day_nr, time_slot)
}

df_H_down <- downsample_to_3x_avg_preserve_IDs(df_H)
df_M_down <- downsample_to_3x_avg_preserve_IDs(df_M_excl54559)
df_L_down <- downsample_to_3x_avg_preserve_IDs(df_L)

  #Downsampling to 3 observations per ID per day; random but time-ordered removal of observations. (NOT chosen for this elective, first experimented with; suggested by ChatGPT)
downsample_3_per_day <- function(df) {
  df %>%
    group_by(ID, Day_nr) %>%
    mutate(row_id = row_number()) %>%  
    mutate(n_per_day = n()) %>%
    group_modify(~ {
      if (nrow(.x) >= 3) {
        .x <- slice_sample(.x, n = 3)
      }
      return(.x)
    }) %>%
    ungroup() %>%
    select(-row_id, -n_per_day)
}

sort_by_time <- function(df) { # to preserve time ordering even with random timepoint sampling
  df %>%
    arrange(ID, Day_nr, Timepoint)
}

df_H_down <- downsample_3_per_day(df_H)
df_M_down  <- downsample_3_per_day(df_M_excl54559)
df_L_down <- downsample_3_per_day(df_L)

  # Test: Granger causality (GC) test. (was experimented with, ultimately unused due to coding issues out of the author's control)

# Prepare group data
vars_df_H_down <- df_H_down %>%
  dplyr::select(ID, all_of(vars), Day_nr, Timepoint) %>%
  filter(!Timepoint %in% c(9, 10, 11)) %>%
  mutate(group = "High")

vars_df_M_down <- df_M_down %>%
  dplyr::select(ID, all_of(vars), Day_nr, Timepoint) %>%
  filter(!Timepoint %in% c(9, 10, 11)) %>%
  mutate(group = "Medium")

vars_df_L_down <- df_L_down %>%
  dplyr::select(ID, all_of(vars), Day_nr, Timepoint) %>%
  filter(!Timepoint %in% c(9, 10, 11)) %>%
  mutate(group = "Low")

# Function to plot and save significant effects
plot_and_save_sig <- function(mat, type, labelA, labelB) {
  sig_mat <- mat
  sig_mat[sig_mat >= 0.05] <- NA
  if (all(is.na(sig_mat))) return(NULL)
  
  filename <- paste0("Sig_", type, "_", labelA, "_vs_", labelB, ".png")
  png(filename, width = 800, height = 800)
  qgraph(sig_mat, layout = "spring", labels = Vars, 
         title = paste("Significant", type, "Effects:", labelA, "vs", labelB))
  dev.off()
}

run_comparison_down <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_down <- bind_rows(groupA, groupB)
  
  comparison <- mlVAR_GC(complete_df,
                         vars = vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = "group",
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  #Print p-values in the console.
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  #Plot and save significant matrices.
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}

  # Run comparisons
run_comparison(vars_df_L_down, vars_df_M_down, "df_L_down", "df_M_down")
run_comparison(vars_df_L_down, vars_df_H_down, "df_L_down", "df_H_down")
run_comparison(vars_df_M_down, vars_df_H_down, "df_M_down", "df_H_down")

 # Variations of the run_comparison function, as suggested by ChatGPT based on issues the author faced during experimentation.

  # A) Explicit creation of (numeric) group vector in place of group column name.
run_comparison_down <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_down <- bind_rows(groupA, groupB)
  
  group_labels <- unique(complete_df$group) # NEW!
  print(paste("Group labels detected:", paste(group_labels, collapse = ", "))) # NEW!
  
  if (length(group_labels) != 2) { # NEW!
    stop("Expected exactly 2 groups for comparison.")
  }
  
  # Create numeric group vector: 1 for groupA, 2 for groupB
  group_vector <- ifelse(complete_df$group == group_labels[1], 1, # NEW!
                         ifelse(complete_df$group == group_labels[2], 2, NA))
  
  if (any(is.na(group_vector))) { # NEW!
    stop("NA detected in group_vector — check group labeling.")
  }
  
  group_vector <- as.numeric(group_vector) # NEW!
  
  comparison <- mlVAR_GC(complete_df,
                         vars = vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = group_vector, # MODIFIED!
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  #Print p-values in the console.
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  #Plot and save significant matrices.
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}
  # B) Checking vector creation within function.
run_comparison_down <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_down <- bind_rows(groupA, groupB)
  
  group_labels <- unique(complete_df$group)
  print(paste("Group labels detected:", paste(group_labels, collapse = ", ")))
  
  print("Counts per group:")
  print(table(complete_df$group))
  
  if (length(group_labels) != 2) {
    stop("Expected exactly 2 groups for comparison.")
  }
  
  # Create numeric group vector: 1 for groupA, 2 for groupB
  group_vector <- ifelse(complete_df$group == group_labels[1], 1,
                         ifelse(complete_df$group == group_labels[2], 2, NA))
  
  if (any(is.na(group_vector))) {
    stop("NA detected in group_vector — check group labeling.")
  }
  
  print("Group vector counts:") # NEW!
  print(table(group_vector)) # NEW!
  
  group_vector <- as.numeric(group_vector) 
  
  comparison <- mlVAR_GC(complete_df,
                         vars = vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = group_vector,
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  #Print p-values in the console.
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  #Plot and save significant matrices.
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}

  # C) Checking and filtering for NAs + creating group labels.
run_comparison_down <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_down <- bind_rows(groupA, groupB)
  
  group_labels <- unique(complete_df$group)
  print(paste("Group labels detected:", paste(group_labels, collapse = ", ")))
  
  print("Counts per group:") # NEW!
  print(table(complete_df$group)) # NEW!
  
  complete_df <- complete_df %>% # NEW!
    filter(!is.na(Timepoint)) %>%
    filter(if_all(all_of(vars), ~ !is.na(.x)))
  
  print("After removing NAs:") # NEW!
  print(table(complete_df$group)) # NEW!
  
  # Create numeric group vector: 1 for groupA, 2 for groupB
  group_vector <- ifelse(complete_df$group == group_labels[1], 1,
                         ifelse(complete_df$group == group_labels[2], 2, NA))
  
  if (any(is.na(group_vector))) {
    stop("NA detected in group_vector — check group labeling.")
  }
  
  print("Group vector counts:")
  print(table(group_vector))
  
  # Try coercing group_vector to integer or factor
  group_vector <- factor(group_vector, levels = c(1, 2)) # MODIFIED!
  
  comparison <- mlVAR_GC(complete_df,
                         vars = vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = group_vector,
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  #Print p-values in the console.
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  #Plot and save significant matrices.
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}

  # D) Running modifications up till C) but removing the group vector modification.
run_comparison_down <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_down <- bind_rows(groupA, groupB)
  
  group_labels <- unique(complete_df$group)
  print(paste("Group labels detected:", paste(group_labels, collapse = ", ")))
  print(table(complete_df$group))
  
  complete_df <- complete_df %>%
    filter(!is.na(Timepoint)) %>%
    filter(if_all(all_of(vars), ~ !is.na(.x)))
  
  cat("After removing NAs:\n")
  print(table(complete_df$group))
  
  comparison <- mlVAR_GC(complete_df,
                         vars = vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = "group",  # MODIFIED! (back to original code)
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  # Print p-values
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  # Plot and save significant effects (assuming plot_and_save_sig is defined)
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}

  # E) Different definition of group column.
run_comparison_down <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_down <- bind_rows(groupA, groupB)
  
  group_labels <- unique(complete_df$group)
  print(paste("Group labels detected:", paste(group_labels, collapse = ", ")))
  print(table(complete_df$group))
  
  complete_df <- complete_df %>%
    filter(!is.na(Timepoint)) %>%
    filter(if_all(all_of(vars), ~ !is.na(.x)))
  
  cat("After removing NAs:\n")
  print(table(complete_df$group))
  
  # Convert group column to numeric: 1 for first group, 2 for second group
  complete_df$group <- ifelse(complete_df$group == group_labels[1], 1, # NEW!
                              ifelse(complete_df$group == group_labels[2], 2, NA))
  
  if (any(is.na(complete_df$group))) stop("NA detected in group column after conversion.") # NEW!
  
  comparison <- mlVAR_GC(complete_df,
                         vars = vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = "group",
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  # Print p-values
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  # Plot and save significant effects
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}

# Approach 2: post-hoc weighted aggregation. (code suggested by ChatGPT)

# Test: Granger causality (GC) test.

  # Prepare group data
vars_df_H_post <- df_H %>%
  dplyr::select(ID, all_of(vars), Day_nr, Timepoint) %>%
  filter(!Timepoint %in% c(9, 10, 11)) %>%
  mutate(group = "High")

vars_df_M_post <- df_M_excl54559 %>%
  dplyr::select(ID, all_of(vars), Day_nr, Timepoint) %>%
  filter(!Timepoint %in% c(9, 10, 11)) %>%
  mutate(group = "Medium")

vars_df_L_post <- df_L %>%
  dplyr::select(ID, all_of(vars), Day_nr, Timepoint) %>%
  filter(!Timepoint %in% c(9, 10, 11)) %>%
  mutate(group = "Low")

  # Function to plot and save significant effects
plot_and_save_sig <- function(mat, type, labelA, labelB) {
  sig_mat <- mat
  sig_mat[sig_mat >= 0.05] <- NA
  if (all(is.na(sig_mat))) return(NULL)
  
  filename <- paste0("Sig_", type, "_", labelA, "_vs_", labelB, ".png")
  png(filename, width = 800, height = 800)
  qgraph(sig_mat, layout = "spring", labels = Vars, 
         title = paste("Significant", type, "Effects:", labelA, "vs", labelB))
  dev.off()
}

run_comparison_post <- function(groupA, groupB, labelA, labelB) {
  cat(paste("\n### Comparison:", labelA, "vs", labelB, "\n"))
  
  complete_df_post <- bind_rows(groupA, groupB)
  
  comparison <- mlVAR_GC(complete_df,
                         vars = Vars,
                         idvar = "ID",
                         dayvar = "Day_nr",
                         beepvar = "Timepoint",
                         groups = "group",
                         test = "parametric",
                         temporal = "orthogonal",
                         contemporaneous = "orthogonal",
                         nP = 1000,
                         pbar = TRUE)
  
  #Print p-values in the console.
  cat("Temporal Effects:\n")
  print(comparison$Pval$Temporal_fixed)
  cat("Contemporaneous Effects:\n")
  print(comparison$Pval$Contemp_fixed)
  cat("Between Effects:\n")
  print(comparison$Pval$Between)
  
  #Plot and save significant matrices.
  plot_and_save_sig(comparison$Pval$Temporal_fixed, "Temporal", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Contemp_fixed, "Contemporaneous", labelA, labelB)
  plot_and_save_sig(comparison$Pval$Between, "Between", labelA, labelB)
}

  # Run comparisons
run_comparison(vars_df_L_post, vars_df_M_post, "df_L", "df_M_excl54559")
run_comparison(vars_df_L_post, vars_df_H_post, "df_L", "df_H")
run_comparison(vars_df_M_post, vars_df_H_post, "df_M_excl54559", "df_H")

# Weighted average of network metrics: edge strengths, centralities. (suggested by ChatGPT; not experimented with due to previous issues with GC code running)

# Aggregate by group, given group info is available.
group_info <- complete_df_post %>% select(ID, group) %>% distinct()

network_summary <- network_summary %>%
  left_join(group_info, by = "ID")

group_weighted <- network_summary %>%
  group_by(group) %>%
  summarise(
    w_avg_strength = weighted.mean(EdgeStrength, w = n_obs),
    w_avg_centrality = weighted.mean(AvgCentrality, w = n_obs)
  )

# OR

# Compute individual metrics.
mlvar_indiv <- mlVAR(complete_df_post, # extract individual VARs
                     vars = vars, 
                     idvar = "ID",
                     dayvar = "Day_nr",
                     beepvar = "Timepoint",
                     lags = 1,
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     nCores = 4)
temporal_individual <- mlvar_indiv$results$Beta_i  # access estimated individual temporal networks

library(qgraph)

edge_strengths <- sapply(temporal_individual, function(mat) {
  mean(abs(mat[mat != 0]))
})

centralities <- lapply(temporal_individual, function(mat) {
  centrality_auto(mat)
})

  # Create a summary data frame.
network_summary <- data.frame( 
  ID = complete_df_post %>% # where complete_df_post <- bind_rows(df_H, df_M_excl54559, df_L)
    distinct(ID) %>%
    arrange(ID) %>%
    pull(ID),
  EdgeStrength = edge_strengths,
  AvgCentrality = strength_centralities
)

  # Add weights and compute weighted averages, guarding against missing values. 
obs_per_id <- complete_df_post %>%
  group_by(ID) %>%
  summarise(n_obs = n())

network_summary <- network_summary %>%
  left_join(obs_per_id, by = "ID")

weighted_avg_strength <- weighted.mean(network_summary$EdgeStrength,
                                       w = network_summary$n_obs,
                                       na.rm = TRUE)

weighted_avg_centrality <- weighted.mean(network_summary$AvgCentrality,
                                         w = network_summary$n_obs,
                                         na.rm = TRUE)

# ==============================================================================
# ==============================================================================