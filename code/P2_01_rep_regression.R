library(tidyverse)
library(lubridate)
library(readxl)
library(modelr)
library(broom)

# Read drain flow data
drain_flow <- read_rds("data/drain_flow_with_rain_ORIGINAL.rds")


# Select sites with replicated plots
drain_flow <- 
  drain_flow %>%
  # select sites with more than 2 plots
  group_by(siteid) %>%
  mutate(n = length(unique(plotid))) %>%
  filter(n > 2) %>%
  # make sure that there are replicated treatments
  mutate(n = length(unique(dwm))) %>%
  filter(n > 1) %>%
  select(siteid, plotid, dwm, year, date, flow)


# assign reps numbers (arbitrary)
dwm_reps <- 
  drain_flow %>%
  count(siteid, plotid, dwm) %>%
  group_by(siteid, dwm) %>%
  mutate(rep = 1:n()) %>%
  ungroup() %>%
  select(siteid, plotid, dwm, rep) %>%
  mutate(rep = paste0("rep_", rep))

# add replication numbers
drain_flow_reps <-
  drain_flow %>%
  # assign rep numbers
  left_join(dwm_reps, by = c("siteid", "plotid", "dwm")) %>%
  select(-year, -plotid) %>%
  spread(rep, flow)



# Predict missing data using replicates -----------------------------------

# function to fit linear model to replicated plots
rep_model_1 <- function(df) {
  add_predictions(data = df, 
                  model = lm(rep_1 ~ rep_2 - 1, data = df), 
                  var = "pred_1")
}

rep_model_2 <- function(df) {
  add_predictions(data = df, 
                  model = lm(rep_2 ~ rep_1 - 1, data = df), 
                  var = "pred_2")
}

# predict missing values by fitting models
drain_flow_fitted <-
  drain_flow_reps %>%
  # add season as factor
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall"))) %>%
  group_by(siteid, dwm, season) %>%
  nest() %>%
  # add predictions
  mutate(data = map(data, rep_model_1),
         data = map(data, rep_model_2)) %>%
  unnest(data) %>%
  ungroup() %>%
  select(-season) %>%
  # transform table so predictions are next to actual measurements
  gather(key = key, value = flow, rep_1:pred_2) %>%
  separate(key, into = c("key", "rep_number")) %>%
  spread(key, flow) %>%
  rename(flow = rep, rep = rep_number) %>%
  # assign plot ids
  mutate(rep = paste0("rep_", rep)) %>%
  left_join(dwm_reps, by = c("siteid", "rep", "dwm")) %>%
  select(siteid, plotid, dwm, date, flow, flow_pred_phase_2 = pred) %>%
  # remove predictions of reps at DPAC in 2017
  mutate(flow_pred_phase_2 = ifelse(
    siteid == "DPAC" &
      year(date) == 2017, 
    NA, 
    flow_pred_phase_2)) %>%
  # remove predictions of reps at S2 in SERF_IA in 2009-2010
  mutate(flow_pred_phase_2 = ifelse(
    siteid == "SERF_IA" &
      year(date) %in% 2009:2010,
    NA,
    flow_pred_phase_2
  )) %>%
  # add comments for predicted values
  mutate(comments = ifelse(
    is.na(flow) &
      !is.na(flow_pred_phase_2),
    "predicted via rep regression",
    NA
  ),
  # replace predicted measurements with actual
  flow_pred_phase_2 = ifelse(
    is.na(comments), 
    flow, 
    flow_pred_phase_2)
  ) %>%
  arrange(siteid, plotid, date)



# Save predicted data -----------------------------------------------------

write_rds(drain_flow_fitted, 
          path = "data/phase2/drain_flow_fitted_PHASE2.rds", 
          compress = 'xz')



# Combine predicted data with other site data -----------------------------

read_rds('data/phase1/drain_flow_with_rain_PHASE1.rds') %>%
  # merge the tables from phases 1 and 2
  full_join(drain_flow_fitted, by = c('siteid', 'plotid', 'dwm', 'date', 'flow')) %>%
  # substitute missing data with predictions
  mutate(flow = ifelse(
    is.na(flow), 
    flow_pred_phase_2, 
    flow)) %>%
  select(siteid, plotid, year, date, flow, dwm, precip_on_site) %>%
  write_rds(path = "data/phase2/drain_flow_with_rain_PHASE2.rds", 
            compress = 'xz') 

