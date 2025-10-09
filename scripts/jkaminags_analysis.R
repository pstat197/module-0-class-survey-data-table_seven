library(tidyverse)

# Read in data
background <- read_csv('data/background-clean.csv')
interest <- read_csv('data/interest-clean.csv')

# Question of interest
#
# After controlling from research experience and domain specialization,
# can the number of upper division programming & stats courses taken
# predict a student's comfort level in those respective areas?

# First, we will filter the data to just the information we need
# Using the base pipe |> since it's faster
background_filter <- background |>
  select(prog.comf, stat.comf, dom, rsrch,
         starts_with("PSTAT"), starts_with("LING"), 
         starts_with("CS"), ECON145) |>
  filter(dom != "Unsure")

# Create a count variable for no. of programming & no. of stats courses.
# Here we will count PSTAT100, 115, 122, 126, 131, 134, and 174
# All CS courses, LING 104, 110, 111, and ECON 145 as programming courses
# and PSTAT 115, 120, 122, 126, 127, 160, 174, and LING 104 as stats courses
background_final <- background_filter |>
  mutate(
    n_programming = PSTAT100 + PSTAT115 + PSTAT122 + PSTAT126 + PSTAT131 +
      PSTAT134 + PSTAT174 + CS5 + CS9 + CS16 + CS130 + CS165 + LING104 + 
      LING110 + LING111 + ECON145,
    n_statistics = PSTAT115 + PSTAT120 + PSTAT122 + PSTAT126 + PSTAT127 +
      PSTAT174 + LING104,
    dom_fct = as.factor(dom),
    rsrch_fct = as.factor(rsrch)
  ) |>
  select(prog.comf, stat.comf, dom_fct, rsrch_fct, n_programming, n_statistics)

# Fit linear model for effect of no. programming courses on programming comfort
mod_prog <- lm(prog.comf ~ dom_fct + rsrch_fct + n_programming,
               data = background_final)

# Fit linear model for effect of no. stats courses on statistics comfort
mod_stats <- lm(stat.comf ~ dom_fct + rsrch_fct + n_statistics,
                data = background_final)

# Check output of both models
summary(mod_prog)
summary(mod_stats)

# Create visual summary of data
plot_stats <- background_final |>
  ggplot(aes(x = n_statistics, y = stat.comf, col = dom_fct,
             shape = rsrch_fct)) +
  geom_point(size = 2.5) +
  geom_jitter(width = 0.25, height = 0.25, size = 2.5) +
  labs(x = "No. of statistics courses",
       y = "Statistics comfort level",
       title = "Statistics comfort level vs. no. of stats. courses",
       col = "Domain spec.",
       shape = "Rsrch. exp.") +
  theme_minimal()

plot_prog <- background_final |>
  ggplot(aes(x = n_programming, y = prog.comf, col = dom_fct,
             shape = rsrch_fct)) +
  geom_point(size = 2.5) +
  geom_jitter(width = 0.25, height = 0.25, size = 2.5) +
  labs(x = "No. of programming courses",
       y = "Programming comfort level",
       title = "Programming comfort level vs. no. of prog. courses",
       col = "Domain spec.",
       shape = "Rsrch. exp.") +
  theme_minimal()

# Save visual summar to results folder
ggsave("results/plot_stats.png", plot = plot_stats)
ggsave("results/plot_prog.png", plot = plot_prog)