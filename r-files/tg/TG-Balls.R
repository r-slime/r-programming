library(tidyverse)
library(ggridges)
library(patchwork)
theme_set(theme_minimal())

balls <- read_csv("TG-Balls.csv")

selected_days <- c("monday", "tuesday", "wednesday", "thursday")
selected_days_2 <- c("monday", "tuesday", "wednesday", "thursday", "friday-sunday")

# UPPER LIMIT
p2<- balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_upper_limit, y = day, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability",
                    values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"), 
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  labs(title = "Topgolf: Number of Balls Hit per Day", 
       subtitle = "Upper Limit", 
       x = "Balls Hit", 
       y = "Day")
  

balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_upper_limit, y = day))+
  geom_density_ridges(
    jittered_points = TRUE, 
    position = "raincloud",
    alpha = 0.7, scale = 0.9)


balls %>%
filter(day %in% selected_days) %>%
ggplot(aes(x = total_balls_estimation_upper_limit, y = day))+
geom_density_ridges(jittered_points = TRUE, 
                    quantile_lines = TRUE, 
                    scale = 0.9, 
                    alpha = 0.7,
                    vline_size = 1, 
                    vline_color = "red",
                    point_size = 0.4, 
                    point_alpha = 1,
                  position = position_raincloud(adjust_vlines = TRUE))


# Lower Limit
p1 <- balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_lower_limit, y = day, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability",
                    values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"), 
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  labs(title = "Topgolf: Number of Balls Hit per Day", 
       subtitle = "Lower Limit", 
       x = "Balls Hit", 
       y = "Day")


balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_lower_limit, y = day))+
  geom_density_ridges(
    jittered_points = TRUE, 
    position = "raincloud",
    alpha = 0.7, scale = 0.9)


balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_lower_limit, y = day))+
  geom_density_ridges(jittered_points = TRUE, 
                      quantile_lines = TRUE, 
                      scale = 0.9, 
                      alpha = 0.7,
                      vline_size = 1, 
                      vline_color = "red",
                      point_size = 0.4, 
                      point_alpha = 1,
                      position = position_raincloud(adjust_vlines = TRUE))


# Limit Mean
p3 <- balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_limit_mean, y = day, rel_min_height = 0.01, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability",
                    values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"), 
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  labs(title = "Topgolf: Number of Balls Hit per Day", 
       subtitle = "", 
       x = "Balls Hit", 
       y = "Day")


balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_limit_mean, y = day))+
  geom_density_ridges(
    jittered_points = TRUE, 
    position = "raincloud",
    alpha = 0.7, scale = 0.9)


p4 <- balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_limit_mean, y = day, rel_min_height = 0.01))+
  geom_density_ridges(jittered_points = TRUE, 
                      quantile_lines = TRUE, 
                      scale = 0.9, 
                      alpha = 0.7,
                      vline_size = 1, 
                      vline_color = "red",
                      point_size = 0.4, 
                      point_alpha = 1,
                      position = position_raincloud(adjust_vlines = TRUE)) +
  labs(title = "Topgolf: Number of Balls Hit per Day", 
       subtitle = "Raincloud", 
       x = "Balls Hit", 
       y = "Day")


# Patchwork
(p1 + p2) / (p3 + p4)


# practice balls
p5 <- balls %>% 
  filter(day %in% selected_days) %>%
  ggplot(aes(x = practice_balls, y = day, rel_min_height = 0.0001, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability",
                    values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"), 
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  labs(title = "Topgolf: Number of Practice Balls", 
       subtitle = "", 
       x = "Number of Practice Balls", 
       y = "Day")


balls %>% 
  filter(day %in% selected_days) %>%
  ggplot(aes(x = decimal, y = day, rel_min_height = 0.001, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability",
                    values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"), 
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  labs(title = "Topgolf: Percent of Practice Balls", 
       subtitle = "", 
       x = "Percent of Practice Balls", 
       y = "Day")

balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = practice_balls, y = day, rel_min_height = 0.0001))+
  geom_density_ridges(jittered_points = TRUE, 
                      quantile_lines = TRUE, 
                      scale = 0.9, 
                      alpha = 0.7,
                      vline_size = 1, 
                      vline_color = "red",
                      point_size = 0.4, 
                      point_alpha = 1,
                      position = position_raincloud(adjust_vlines = TRUE)) +
  labs(title = "Topgolf: Number of Practice Balls", 
       subtitle = "Raincloud", 
       x = "Number of Practice Balls", 
       y = "Day")

balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = decimal, y = day, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE,
                      quantiles = 4, 
                      quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles")


balls %>%
  filter(day %in% selected_days) %>%
  ggplot(aes(x = total_balls_estimation_limit_mean, y = day)) +
  geom_boxplot()


p6 <- balls %>%
  filter(day %in% selected_days_2) %>%
  ggplot(aes(x = total_balls_estimation_limit_mean, y = practice_balls, color = decimal)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  labs(title = "Topgolf: Practiceballs and Total Balls Hit",
       x = "Total Balls Hit", 
       y = "Practice Balls")

# Patchwork
(p3 + p4) / (p5 + p6)
