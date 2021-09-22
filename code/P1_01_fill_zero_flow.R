library(tidyverse)
library(lubridate)

# Read drain flow data

drain_flow <- read_rds("data/drain_flow_with_rain_ORIGINAL.rds")


# Fill in zero tile flows

drain_flow_with_rain <-
  drain_flow %>%
  arrange(siteid, plotid, date) %>%
  mutate(
    flow = case_when(
      siteid == 'HICKS_B' &
        is.na(flow) & month(date) %in% c(1, 2, 3, 11, 12) ~ 0,
      # Jeff: 2011-2013 were drought
      # Gio: in 2011 drainage system was reworked (west plot was split into two)
      siteid == 'HICKS_B' &
        is.na(flow) & year %in% 2012:2013 ~ 0,
      TRUE ~ flow
    )
  )


# Save drain flow and on-site precipitation data

write_rds(drain_flow_with_rain,
          path = "data/phase1/drain_flow_with_rain_PHASE1.rds", 
          compress = 'xz')
