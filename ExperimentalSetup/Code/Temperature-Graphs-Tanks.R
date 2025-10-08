library(pacman)
p_load(tidyverse, readxl, grid, lubridate, withr)

# Load data, change file path!
T1 <- read_excel('C:/R/research project/prep/data/temp-T1-6DEG.xlsx')
T2 <- read_excel('C:/R/research project/prep/data/temp-T2-10DEG.xlsx')
T3 <- read_excel('C:/R/research project/prep/data/temp-T3-16DEG.xlsx')
T4 <- read_excel('C:/R/research project/prep/data/temp-T4-22DEG.xlsx')
T6 <- read_excel('C:/R/research project/prep/data/temp-T6-32DEG.xlsx')

# Tag each table with its tank id
T1 <- T1 %>% mutate(Tank = "T1 - 6°C")
T2 <- T2 %>% mutate(Tank = "T2 - 10°C")
T3 <- T3 %>% mutate(Tank = "T3 - 16°C")
T4 <- T4 %>% mutate(Tank = "T4 - 22°C")
T6 <- T6 %>% mutate(Tank = "T6 - 32°C")

# Define experimental windows (inclusive of start & end dates)
ranges <- tribble(
  ~Tank,        ~start_date,       ~end_date,
  "T3 - 16°C",  ymd("2025-04-30"), ymd("2025-05-28"),
  "T2 - 10°C",  ymd("2025-04-30"), ymd("2025-05-31"),
  "T4 - 22°C",  ymd("2025-04-30"), ymd("2025-05-31"),
  "T1 - 6°C",   ymd("2025-04-30"), ymd("2025-06-02"),
  "T6 - 32°C",  ymd("2025-04-30"), ymd("2025-06-04")
)

# Bind, filter to windows, and order facets
temps <- bind_rows(T1, T2, T3, T4, T6) %>%
  mutate(Date = as_date(Reading)) %>%
  left_join(ranges, by = "Tank") %>%
  filter(Date >= start_date, Date <= end_date) %>%
  mutate(Tank = factor(Tank, levels = c("T1 - 6°C","T2 - 10°C","T3 - 16°C","T4 - 22°C","T6 - 32°C"))) %>%
  arrange(Tank, Reading)

# ----- English month labels (portable): set LC_TIME to "C" while plotting -----
old_loc <- Sys.getlocale("LC_TIME")
on.exit(Sys.setlocale("LC_TIME", old_loc), add = TRUE)
Sys.setlocale("LC_TIME", "C")  # gives English month names like "May", "Jun"

# ----- Vline data -----
tz_used <- "Australia/Hobart"

# Common vline across all facets
vline_common <- as.POSIXct("2025-04-30", tz = tz_used)

# One vline per facet
vlines_per_facet <- tribble(
  ~Tank,        ~x_date,
  "T1 - 6°C",   "2025-06-02",
  "T2 - 10°C",  "2025-05-31",
  "T3 - 16°C",  "2025-05-28",
  "T4 - 22°C",  "2025-05-31",
  "T6 - 32°C",  "2025-06-04"
) %>%
  mutate(x = as.POSIXct(x_date, tz = tz_used))

# ---- Facetted time-series plot ----#
theme_format <- 
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(colour = "black"),
        title = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.width = unit(1.2,"cm"),
        strip.text = element_text(size = 10, colour = "black"),
        strip.background = element_rect("white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

p <- ggplot(temps, aes(x = Reading, y = Values)) +
  geom_line() +
  # Common dotted vline (same in all panels)
  geom_vline(xintercept = vline_common, linetype = "dotted", linewidth = 1) +
  # Facet-specific dotted vlines
  geom_vline(data = vlines_per_facet, aes(xintercept = x),
             linetype = "dotted", linewidth = 1, inherit.aes = FALSE) +
  facet_wrap(~ Tank, ncol = 2, scales = "free_y") + 
  labs(title = "Temperature of water baths over experimental duration",
       x = "Time", 
       y = expression(paste("Temperature (", degree, "C)"))) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%d %b") +
  theme_format

p

## save plot
path_plots <- "C:/R/research project/prep/plots/"
ggsave(filename = paste0(path_plots, "T_tanks.png"), 
       plot = p, width = 10, height = 8, dpi = 600)

##derive Tmin and Tmax for treatment periods
targets <- tribble(
  ~Tank, ~target,
  "T1",  6,
  "T2", 10,
  "T3", 16,
  "T4", 22,
  "T6", 32
)

ranges2 <- tribble(
  ~Tank, ~start_date,       ~end_date,
  "T3",  ymd("2025-04-30"), ymd("2025-05-27"),
  "T2",  ymd("2025-05-03"), ymd("2025-05-30"),
  "T4",  ymd("2025-05-03"), ymd("2025-05-30"),
  "T1",  ymd("2025-05-05"), ymd("2025-06-01"),
  "T6",  ymd("2025-05-07"), ymd("2025-06-03")
)

# ---- Bind your five tables and trim to windows ----
temps_trim <- bind_rows(
  T1 %>% mutate(Tank = "T1"),
  T2 %>% mutate(Tank = "T2"),
  T3 %>% mutate(Tank = "T3"),
  T4 %>% mutate(Tank = "T4"),
  T6 %>% mutate(Tank = "T6")
) %>%
  mutate(Date = as_date(Reading)) %>%
  left_join(ranges2, by = "Tank") %>%
  filter(Date >= start_date, Date <= end_date) %>%
  left_join(targets, by = "Tank")

# ---- Daily means per tank ----
daily <- temps_trim %>%
  mutate(day = as_date(Reading)) %>%
  group_by(Tank, day, target) %>%
  summarise(daily_mean = mean(Values, na.rm = TRUE), .groups = "drop") %>%
  mutate(dev = daily_mean - target,
         absdev = abs(dev))

# ---- Nested summary: within ±1; within ±2; above +2; below −2 ----
days_nested <- daily %>%
  group_by(Tank) %>%
  summarise(
    total_days      = n_distinct(day),
    days_within_pm1 = sum(absdev <= 1, na.rm = TRUE),
    days_within_pm1.5 = sum(absdev <= 1.5, na.rm = TRUE),
    days_within_pm2 = sum(absdev <= 2, na.rm = TRUE),   # inclusive; includes ±1
    days_above_p2   = sum(dev >  2, na.rm = TRUE),
    days_below_m2   = sum(dev < -2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(total_days, days_within_pm1, days_within_pm2, days_above_p2, days_below_m2), as.integer),
    pct_within_pm1 = round(100 * days_within_pm1 / total_days, 1),
    pct_within_pm2 = round(100 * days_within_pm2 / total_days, 1),
    pct_above_p2   = round(100 * days_above_p2   / total_days, 1),
    pct_below_m2   = round(100 * days_below_m2   / total_days, 1)
  )

days_nested
