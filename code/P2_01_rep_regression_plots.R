# check site-years with predicted data
drain_flow_fitted %>%
  filter(is.na(flow) & !is.na(flow_pred_phase_2)) %>%
  count(siteid, year(date))


# Plot to preview predicted data ------------------------------------------

# DPAC
drain_flow_fitted %>%
  mutate(year = year(date)) %>%
  filter(siteid == "DPAC" & year == 2016) %>%
  mutate(plotid = factor(plotid, levels = c('SW', 'NE', 'SE', 'NW'))) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred_phase_2)) +
  geom_point(aes(y = flow, colour = dwm)) + 
  geom_line(aes(y = flow, colour = dwm), alpha = 0.25) +
  labs(x = NULL, y = 'Drain Flow, mm',
       title = 'DPAC 2016',
       subtitle = 'Daily Drainage Predictions based on Replicated Plot Data') +
  facet_grid(plotid ~ .) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('figs/phase2/DPAC_drain_flow_predictions_2016.png',
       width = 16, height = 10)


# SERF_IA
drain_flow_fitted %>%
  mutate(year = year(date)) %>%
  filter(siteid == "SERF_IA" & year == 2007) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred_phase_2)) +
  geom_point(aes(y = flow, colour = dwm)) + 
  geom_line(aes(y = flow, colour = dwm), alpha = 0.25) +
  labs(x = NULL, y = 'Drain Flow, mm',
       title = 'SERF_IA 2007',
       subtitle = 'Daily Drainage Predictions based on Replicated Plot Data') +
  facet_grid(plotid ~ .) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('figs/phase2/SERF_IA_drain_flow_predictions_2007.png',
       width = 16, height = 10)

drain_flow_fitted %>%
  mutate(year = year(date)) %>%
  filter(siteid == "SERF_IA" & year == 2017) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred_phase_2)) +
  geom_point(aes(y = flow, colour = dwm)) + 
  geom_line(aes(y = flow, colour = dwm), alpha = 0.25) +
  labs(x = NULL, y = 'Drain Flow, mm',
       title = 'SERF_IA 2017',
       subtitle = 'Daily Drainage Predictions based on Replicated Plot Data') +
  facet_grid(plotid ~ .) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('figs/phase2/SERF_IA_drain_flow_predictions_2017.png',
       width = 16, height = 10)


# TIDE
drain_flow_fitted %>%
  mutate(year = year(date)) %>%
  filter(siteid == "TIDE" & year == 2007) %>%
  mutate(plotid = factor(plotid, levels = c('H2', 'H5', 'H3', 'H4'))) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred_phase_2)) +
  geom_point(aes(y = flow, colour = dwm)) + 
  geom_line(aes(y = flow, colour = dwm), alpha = 0.25) +
  labs(x = NULL, y = 'Drain Flow, mm',
       title = 'TIDE 2007',
       subtitle = 'Daily Drainage Predictions based on Replicated Plot Data') +
  facet_grid(plotid ~ .) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('figs/phase2/TIDE_drain_flow_predictions_2007.png',
       width = 16, height = 10)


drain_flow_fitted %>%
  mutate(year = year(date)) %>%
  filter(siteid == "TIDE" & year == 2009) %>%
  mutate(plotid = factor(plotid, levels = c('H2', 'H5', 'H3', 'H4'))) %>%
  ggplot(aes(x = date, group = plotid)) +
  geom_point(aes(y = flow_pred_phase_2)) +
  geom_point(aes(y = flow, colour = dwm)) + 
  geom_line(aes(y = flow, colour = dwm), alpha = 0.25) +
  labs(x = NULL, y = 'Drain Flow, mm',
       title = 'TIDE 2009',
       subtitle = 'Daily Drainage Predictions based on Replicated Plot Data') +
  facet_grid(plotid ~ .) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        text = element_text(size = 12))
ggsave('figs/phase2/TIDE_drain_flow_predictions_2009.png',
       width = 16, height = 10)
