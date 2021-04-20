# Libs ----------------------------------------------------
library(tidyverse)
library(readr)
library(here)

# colorblind friendly colours
cbb_Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Read Data ---------------------------------------------------------------
# Read data and drop_na as cleanup, if we have empty rows appended by accident
df_freq <- read_csv("./datasets/frequency_experiments.csv") %>%
  drop_na() %>% 
  mutate(
    amplitude = 1,
    dataset = "freq"
  )

df_amp <- read_csv("./datasets/amplitude_experiments.csv") %>%
  drop_na() %>% 
  mutate(
    dataset = "amp"
  )


df <- bind_rows(
  df_freq, df_amp
)

# Velocity ----------------------------------------------------------------
velo <- read_csv("./datasets/velo_freq_amp.csv") %>%
  drop_na()

# Joining Together --------------------------------------------------------
df <- df %>% 
  left_join(velo, by = c("frequency", "amplitude"))

# Normalize ---------------------------------------------------------------
source(paste0(here(), "/data_analysis/utils.R"))

# Velocity ----------------------------------------------------------------
# -1 would be white noise and 440 is broken
df <- df %>% 
  filter(
   frequency != -1 & frequency != 440 
  )

p <- df %>% 
  mutate(
    # set control to low number so we can plot with
    velocity = if_else(frequency == -2, 0.04, velocity)
  ) %>% 
  filter(
    !is.na(velocity)
    ) %>% 
  ggplot(
    aes(
      x = log10(velocity), 
      y = norm_activity, 
      group = velocity, 
      color = as.factor(frequency), 
      fill = as.factor(frequency)
      )
  ) +
  geom_hline(aes(yintercept = 1), color = "black", alpha = 0.5, show.legend = F) +
  geom_boxplot(
    width = 0.01,
    outlier.shape = NA, 
    ) +
  scale_fill_viridis_d(
    option = "viridis",
    guide = guide_legend("Frequency")
  ) + 
  scale_colour_viridis_d(
    option = "viridis",
    guide = guide_legend("Frequency")
  ) + 
  ylab("normalized pixel-based motion index") +
  xlab("Velocity [log10(mm/s)]") +
  scale_y_continuous(breaks = seq(0, 2, 0.2)) +
  scale_x_continuous(breaks = seq(-2, 2, 0.2)) +
  theme_classic()

ggsave(paste0("./data_analysis/output/", "velocity_vir.pdf"), width = 11, height = 5, p) 







