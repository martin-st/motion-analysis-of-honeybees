# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)
library(here)

# Constants
cbb_Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Read Data ---------------------------------------------------------------
# Read data and drop_na as cleanup, if we have empty rows appended by accident
df <- read_csv("./datasets/amplitude_experiments.csv") %>%
  drop_na()

# Normalize ---------------------------------------------------------------
source(paste0(here(), "/data_analysis/utils.R"))

# Plots -------------------------------------------------------------------

# Control Plot
plot_amp_control <- df %>%
  filter(amplitude == 0) %>%
  ggplot(aes(x = frequency_factor, y = norm_activity, color = dateTime)) +
  geom_boxplot() +
  ggtitle("Control and Frequency at 0 Amplitude after Normalize") +
  ylab("Normalized Activity") + xlab("Frequency") +
  scale_color_hue(l=40) +
  theme_classic()

ggsave("output/plot_amp_control.pdf", width = 10, plot_amp_control)

# Paper Plot
p1 <- df %>% 
  filter(frequency_factor != "Control") %>% 
  group_by(frequency_factor, amplitude) %>% 
  summarize(
    mean = mean(norm_activity),
    upper = mean + sd(norm_activity),
    lower = mean - sd(norm_activity)
  ) %>%
  ggplot(aes(x = amplitude, y = mean, color = frequency_factor)) +
  geom_hline(aes(yintercept = 1), color = "black", alpha = 0.5, show.legend = F) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  ylab("Relative Pixel-Value Change") +
  xlab("Amplitude") +
  labs(color = "Frequency") +
  scale_color_manual(values = cbb_Palette) +
  geom_line() +
  scale_x_continuous(breaks = c(unique(df$amplitude))) +
  theme_classic()

ggsave(paste0("./data_analysis/output/", "amp_p1_lines.pdf"), width = 11, height = 5, p1)
