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

# Generate needed Cols ----------------------------------------------------
# generate dateTime Field
df$dateTime      <- paste0(df$date, "-", df$time)
# generate norm activity cols
df$norm_activity                     <- df$activity
# save frequency original column as we do some mutating with it
df$frequency_org <- df$frequency

# Generate Groups, to represent each Frequency Change (round) ---------------------
# group each frequenz change
df$subgroup <- NA
lastFreq    <- 0
curFreq     <- 0
freqGroup   <- 0

for(i in 1:nrow(df)){
  curFreq <- df$frequency[i]
  # check if current frequency is last frequency
  # if not we change the group
  if(curFreq != lastFreq){
    freqGroup <- freqGroup + 1
  }
  df$subgroup[i] <- freqGroup
  lastFreq <- curFreq
}
# Cleanup
rm(lastFreq, curFreq, freqGroup, i)

# Cumulative Sum ---------------------------------------------------------
# generate a cumulative sum in our frequency breaks
# this will help us to pick out the middle of the trial
df$csum <- 1
df <- df %>% 
  group_by(subgroup) %>%
  mutate(
    csum = cumsum(csum)
  ) %>% 
  ungroup()

# control are frames between breaks with 0 frequency from 15-20 cumsum (-2)
df <- df %>%
  mutate(
    frequency = ifelse(between(csum, 15, 20) & frequency_org == 0, -2, frequency_org)
  )

# Filter frames in our breaks, which are at the beginning < 3 and at the end > 10
df <- df %>% filter(between(csum, 3,10) | frequency == -2 )

# Remove Zeroes
df <- df %>% filter(frequency != 0)

# Add count, we could filter for it eg. that we only plot if we have at least 30 rounds
df <- df %>% add_count(frequency, name = "count")


# Calculate Mean Difference of the Control -----------------------------------------------

# Calculate mean difference of base -2 between rounds
dfBaseSummary <- df %>%
  filter(frequency == -2) %>%
  group_by(dateTime) %>%
  summarise(
    m_activity = mean(activity)
    #m_new_activity_calculation = mean(new_activity_calculation),
    #m_activity_calculation_frame = mean(activity_calculation_frame)
  ) %>% 
  mutate(
    d_activity = m_activity - m_activity[[1]]
    #d_new_activity_calculation = m_new_activity_calculation - m_new_activity_calculation[[1]],
    #d_activity_calculation_frame = m_activity_calculation_frame - m_activity_calculation_frame[[1]]
  )


# Normalize based on Mean Base Activity -----------------------------------
testUnits <- unique(df$dateTime)
for(i in 1:length(testUnits)){
  testName <- testUnits[[i]]
  baseDiff <- dfBaseSummary %>% filter(dateTime == testName)
  df <- df %>%
    mutate(
      norm_activity = ifelse(
        dateTime == testName, 
        activity / baseDiff$m_activity, norm_activity # we could here change to median
      )
      #norm_new_activity_calculation = ifelse(
      #  dateTime == testName, 
      #  new_activity_calculation / baseDiff$m_new_activity_calculation, norm_new_activity_calculation # we could here change to median
      #),
      #norm_activity_calculation_frame = ifelse(
      #  dateTime == testName, 
      #  activity_calculation_frame / baseDiff$m_activity_calculation_frame, norm_activity_calculation_frame # we could here change to median
      #),
    )
}

# Cleanup
rm(testUnits, i, testName)

# Generate Factor Levels for Plots ----------------------------------------
# generate factor frequency
df <- df %>% 
  mutate(
    frequency_factor = factor(frequency) %>% fct_recode( Control = "-2", WN = "-1")
  )

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
      color = as.factor(amplitude), 
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







