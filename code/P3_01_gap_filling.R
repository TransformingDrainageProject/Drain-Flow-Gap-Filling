# PHASE 3 
library(tidyverse)
library(lubridate)
library(zoo)
library(modelr)
library(broom)



# Read data ---------------------------------------------------------------

drain_flow <- read_rds("data/phase2/drain_flow_with_rain_PHASE2.rds")



# Prepare data ------------------------------------------------------------

# calculate 2-day moving ave precipitation
drain_flow_data <-
  drain_flow %>%
  # select only Free and Controlled drainage plots
  filter(dwm %in% c("FD", "CD")) %>%
  group_by(siteid, plotid, dwm) %>%
  nest() %>%
  # count number of plots per site
  group_by(siteid) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  # find sites where DWM treatment was swapped
  group_by(siteid, dwm) %>%
  mutate(plot_count = n_distinct(plotid)) %>%
  group_by(siteid) %>%
  mutate(dwm_count = n_distinct(plotid)) %>%
  # assign reps by taking into account plots with swapped treatment
  mutate(
    rep = ifelse(plot_count == dwm_count, 1, NA),
    rep = ifelse(dwm_count == 1, dwm_count, rep),
    rep = ifelse(is.na(rep), 1:plot_count, rep)
  ) %>%
  select(-ends_with("count")) %>%
  unnest() %>%
  # add season
  mutate(season = factor(quarter(date), labels = c("winter", "spring", "summer", "fall")),
         rep = as.factor(rep)) %>%
  select(siteid, plotid, dwm, rep, year, season , date, flow, rain = precip_on_site) %>%
  # calculate limiting drain flow as mean daily summer flow
  group_by(siteid, plotid, dwm, rep) %>%
  mutate(
    min_flow = mean(flow[season == "summer"], na.rm = TRUE),
    # correct limiting flow so it is not > 0.3 mm/day
    min_flow = ifelse(min_flow > 0.3, 0.3, min_flow)
  ) %>%
  # calculate 3-day average precipitation
  group_by(siteid, plotid) %>%
  mutate(
    rain_2 = rollapplyr(rain, 2, mean, na.rm = TRUE, partial = TRUE),
    # remove rolling average values from days when there was no rainfall
    rain_2 = ifelse(rain > 0, rain_2, 0)
  ) %>%
  group_by(siteid, plotid, dwm, rep) %>%
  nest()



# Develop "Seasonal" Regression model ------------------------------------- 
# predicting peak flows caused by precipitation event

# function to fit linear model
reg_model <- function(df) {
  lm(flow ~ rain_2 - 1, data = df)
}

drain_flow_reg_model <-
  drain_flow_data %>%
  # filter data to be used for regression 
  mutate(reg_data = map(.x = data,
                        .f = ~ .x %>%
                          # remove days when there was no rain OR drain flow was below minimum limit
                          filter(flow > min_flow, rain_2 > 0))) %>%
  # add season to nesting
  unnest(reg_data) %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest(.key = "reg_data") %>%
  # fit the model
  mutate(reg_model = map(reg_data, reg_model))



# Calculate recession slope -----------------------------------------------
# slope of falling limb of the graph

# function to select peak and inflection points
peak_inflection <- function(df) {
  df %>%
    mutate(
      first =  ln_flow - lag(ln_flow),
      second = first - lag(first),
      point =  lead(ln_flow) - ln_flow,
      point1 = ifelse(is.na(point), "Pos", ifelse(point < 0, "Neg", "Pos")),
      point1a = ifelse(is.na(first), "Pos", ifelse(first < 0, "Neg", "Pos")),
      point2 = ifelse(is.na(second), "Pos", ifelse(second < 0, "Neg", "Pos"))
    ) %>%
    mutate(
      group1 = rep(seq_along(rle(point1)$length), rle(point1)$length),
      group1a = rep(seq_along(rle(point1a)$length), rle(point1a)$length),
      group2 = rep(seq_along(rle(point2)$length), rle(point2)$length)
    ) %>%
    # group1 = from START to END-1day or ressesion
    group_by(group1) %>%
    mutate(POINT = ifelse(point1 == "Neg" &
                            row_number() == 1, "peak", NA)) %>%
    group_by(group2) %>%
    mutate(POINT = ifelse(point2 == "Neg" &
                            row_number() == n(), "inf", POINT)) %>%
    ungroup() %>%
    select(-point1,-point1a,-point2) %>%
    mutate(group = ifelse(POINT == "peak", group1, group1a)) %>%
    select(-group1,-group1a,-group2) %>%
    filter(!is.na(POINT)) %>%  
    # remove single peaks|ubflections and double inflections
    group_by(group) %>%
    mutate(n = n(),
           count = 1:n()) %>%
    ungroup() %>%
    filter(n != 1,
           count < 3) %>%
    select(year, season, date, flow, rain, ln_flow, group, POINT) %>%
    # calculate duration of PEAK-TO-INFLECTION period and slope
    mutate(days_bw_pni = lead(date) - date,
           days = as.numeric(days_bw_pni),
           change = (lead(ln_flow) - ln_flow),
           slope = change/days)
}


# Calculate all recession slope by season
recession_slope_dist <- 
  drain_flow_data %>%
  # log-transform flow data 
  mutate(data = map(.x = data, 
                    .f = ~ .x %>% 
                      # make sure to handle log(0) 
                      mutate(ln_flow = ifelse(flow == 0, NA, log(flow))))) %>%
  # find PEAK and INFLECTION points
  mutate(data = map(data, peak_inflection)) %>%
  unnest() %>%
  # select "peak" POINT to calculate slope of recession (falling) limb 
  filter(POINT == 'peak') %>%
  filter(!is.infinite(slope))
  

# Calculate average recession slope by season
recession_slope <-
  recession_slope_dist %>%
  # calculate average number of days between peak and first point of inflection
  # and the slope of recession limb 
  group_by(siteid, plotid, season, dwm) %>%
  summarise(ave_days = mean(days),
            # calculate trimmed mean since the distribution of slopes is skewed 
            ave_slope = mean(slope),
            ave_slope_trim = mean(slope, trim = 0.1)
  ) %>%
  ungroup()



# Predict missing data -----------------------------------------------------

drain_flow_pred <-
  drain_flow_data %>%
  unnest(data) %>%
  group_by(siteid, plotid, dwm, rep, season) %>%
  nest() %>%
  # combine regression model and recession slope with flow data
  left_join(drain_flow_reg_model,
            by = c("siteid", "plotid", "dwm", "rep", "season")) %>%
  left_join(recession_slope, by = c("siteid", "plotid", "season", "dwm")) %>%
  select(-reg_data,-ave_days) %>%
  # add predictions of pick flows using actual data
  mutate(isnull = map_lgl(reg_model, is.null),
         # predict pick flow only when there is a model available
         data = ifelse(isnull == 1,
                       data,
                       map2(
                         .x = reg_model,
                         .y = data,
                         .f = ~ augment(.x, newdata = .y)
                       ))) %>%
  select(siteid:data, starts_with("ave")) %>%
  unnest(data) %>%
  # resolve the problem of skipping first data points when rain < rain_limit
  # for example after missing dates of precip there is a first record of 0 rain,
  # we need to keep prediction of corresponding drain flow of 0, so we can predict down the timeline
  group_by(siteid, plotid) %>%
  # find the first non-NA value for precip within each site-plot
  mutate(is_rain = is.na(rain),
         group = rep(seq_along(rle(is_rain)$length), rle(is_rain)$length)) %>%
  group_by(siteid, plotid, group) %>% 
  # select only those first non-NA values when there was a measurement
  mutate(is_first = ifelse(row_number() == 1 &
                             is_rain == FALSE, "Y", "N")) %>%
  ungroup() %>%
  # choose predictions for days when it was raining and no flow measured
  mutate(
    pred = ifelse(.fitted < 0, 0, .fitted),
    # eliminate predicted negative flows
    flow_pred = ifelse(is.na(flow) & rain > 0, pred, flow),
    # add predictions for the first non-NA days when they are missing (due to rain < 0)
    flow_pred = ifelse(is.na(flow_pred) &
                         is_first == "Y", pred, flow_pred)
  ) %>%
  select(siteid:rain, flow_pred, starts_with("ave")) %>%
  # predict flow of falling limb
  group_by(siteid, plotid, dwm) %>%
  mutate(LOGIC = ifelse(is.na(flow_pred), "Y", "N"),
         group = rep(seq_along(rle(LOGIC)$length), rle(LOGIC)$length)) %>%
  group_by(siteid, plotid, dwm, group) %>%
  mutate(count = 1:n(),
         count = ifelse(LOGIC == "N", 0, count)) %>%
  group_by(siteid, plotid, dwm) %>%
  # limit predicted flow to measured max drain flow
  mutate(
    flow_max = max(flow, na.rm = T),
    flow_pred = ifelse(flow_pred > flow_max, flow_max, flow_pred),
    flow_max = NULL
  ) %>%
  select(-group,-LOGIC) %>%
  # apply recession equation (Qi = Qi-1 * e^k, where k is seasonal recession slope)
  mutate(
    flow_pred2 = na.locf(flow_pred, na.rm = FALSE),
    # use arithmetic mean slope
    flow_pred2_mean = round(flow_pred2 * exp(ave_slope * count), 6),
    # use trimmed mean slope
    flow_pred2_trim = round(flow_pred2 * exp(ave_slope_trim * count), 6),
    flow_pred_mean = ifelse(is.na(flow_pred), flow_pred2_mean, flow_pred),
    flow_pred_trim = ifelse(is.na(flow_pred), flow_pred2_trim, flow_pred)
  ) %>%
  # There are days with very high flow day before and very small precipitation
  # this results in predicting very low flow based on precipitation
  # creating a strange recession slope (see DEFI_M in Jan 2018)
  # To compensate for this, we will do recession for actual flow and compare with imputed value
  # if actual recession is greater than imputed drain flow (which was based on precipitation)
  # than substitute the data
  # THIS WILL BE DONE FOR ONLY SINGLE GAPS
  mutate(
    flow2 = na.locf(flow, na.rm = FALSE, maxgap = 1),
    # add recession of original flow with trimmed slope
    flow2_pred_trim = ifelse(is.na(flow), round(flow2 * exp(ave_slope_trim), 6), flow2),
    # replace underestimated prediction of trimmed flow only with recession estimates
    flow_pred_trim = ifelse(
      flow2_pred_trim > flow_pred_trim &
        !is.na(flow2_pred_trim) & is.na(flow),
      flow2_pred_trim,
      flow_pred_trim
    )
  ) %>%
  select(-starts_with("flow_pred2"),-count,-flow2,-flow2_pred_trim) %>%
  # remove predictions of flow in days when there was no rainfall data (see CRAWF)
  mutate(
    flow_pred_mean = ifelse(is.na(flow) &
                              is.na(rain), NA, flow_pred_mean),
    flow_pred_trim = ifelse(is.na(flow) &
                              is.na(rain), NA, flow_pred_trim)
  ) %>%
  select(-starts_with("ave_slope"))


# Count number of predicted values
drain_flow_pred %>%
  arrange(siteid, plotid, date) %>%
  mutate(season = quarter(date),
         pred = ifelse(is.na(flow) &
                         !is.na(flow_pred_trim), "predicted", NA)) %>%
  # exclude winter months from HICKS_B & SERF_SD
  mutate(
    pred = case_when(
      siteid == "SERF_SD" & month(date) %in% c(1:3) ~ NA_character_,
      siteid == "HICKS_B" &
        month(date) %in% c(1:3) ~ NA_character_,
      TRUE ~ pred
    )
  ) %>%
  group_by(siteid, plotid, year) %>%
  summarise(total = n(),
            total_pred = sum(!is.na(pred))) %>%
  ungroup() %>%
  filter(total_pred > 0) %>%
  arrange(desc(total_pred)) ->
  drain_flow_number_of_missing_days


# If number of predicted days per plot exceeds 152 (5 months) in a calendar year, remove predictions
# Based on the above code predictions for following site-plot-years should be removed
drain_flow_number_of_missing_days %>%
  filter(total_pred > 152) 



# Select predicted data ---------------------------------------------------

drain_flow_pred %>% 
  ungroup() %>%
  # select predictions made based on trimmed average recession slope
  select(siteid, plotid, dwm, rep, year, date, rain, flow, flow_pred_trim) %>%
  # remove predictions for plot-years when the whole year was missing
  left_join(drain_flow_number_of_missing_days %>%
              filter(total_pred > 152), 
            by = c('siteid', 'plotid', 'year')) %>%
  mutate(flow_pred = ifelse(is.na(total_pred), flow_pred_trim, flow)) %>%
  select(-total, -total_pred) %>%
  # comment predicted values that were left after above filter
  mutate(comments = ifelse(is.na(flow) & !is.na(flow_pred), "predicted", "")) %>%
  # add zero flows with corresponding comments for SERF_SD 
  mutate(
    comments = case_when(
      siteid == "SERF_SD" &
        month(date) %in% c(1:3) & is.na(flow_pred) ~ "filled in zero",
      # NEED to get this from STEP2
      siteid == "HICKS_B" &
        month(date) %in% c(1:3) & is.na(flow) ~ "filled in zero",
      TRUE ~ comments
    ),
    flow_pred = case_when(
      siteid == "SERF_SD" &
        month(date) %in% c(1:3) & is.na(flow_pred) ~ 0,
      siteid == "HICKS_B" &
        month(date) %in% c(1:3) & is.na(flow) ~ 0,
      TRUE ~ flow_pred
    )
  ) %>% 
  # comment predicted values from Phase 2
  left_join(read_rds("data/phase2/drain_flow_fitted_PHASE2.rds") %>% 
              filter(!is.na(comments)) %>% 
              select(siteid, plotid, date, comments) %>%
              rename(comm = comments),
            by = c('siteid', 'plotid', 'date')) %>%
  mutate(comments = ifelse(!is.na(comm), comm, comments)) %>%
  select(-flow_pred_trim, - comm) %>%
  ungroup() -> drain_flow_prediction



# Save predicted data ----------------------------------
drain_flow_prediction %>%
  # get rid of erroneous measurement at WN in STJOHNS in 2015
  mutate(
    flow_pred = ifelse(siteid == "STJOHNS" &
                         year == 2015 & plotid == "WN",
                       NA, flow_pred),
    comments  = ifelse(
      siteid == "STJOHNS" & year == 2015 & plotid == "WN",
      "removed",
      comments
    )
  ) %>%
  arrange(siteid, plotid, date) %>%
  select(siteid, plotid, dwm, rep, year, date, flow_pred, comments) %>%
  write_rds("data/phase3/drain_flow_daily_filled_PHASE3.rds") 
  

