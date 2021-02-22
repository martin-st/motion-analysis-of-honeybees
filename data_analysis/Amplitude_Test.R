# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)
library(here)

# Constants
cbb_Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Read Data ---------------------------------------------------------------
# Read data and drop_na as cleanup, if we have empty rows appended by accident
df <- read_csv("../datasets/amplitude_experiments.csv") %>%
  drop_na()

# Add Dummy Fields --------------------------------------------------------
# generate dateTime Field
df$dateTime      <- paste0(df$date, "-", df$time)
# generate norm activity cols
df$norm_activity <- df$activity
# save frequency original column as we do some mutating with it
df$frequency_org <- df$frequency

# Generate Groups, to represent each Frequency / Amplitude Change (round) ---------------------
# group each change
df$subgroup <- NA
lastFreq    <- 0
curFreq     <- 0
freqGroup   <- 0

for(i in 1:nrow(df)){
  curFreq <- df$frequency[i]
  if(curFreq != lastFreq){
    freqGroup <- freqGroup + 1
  }
  df$subgroup[i] <- freqGroup
  lastFreq <- curFreq
}
# Cleanup
rm(lastFreq, curFreq, freqGroup, i)

# Cumulative Sum ---------------------------------------------------------
# generate a cummulative sum in our frequency breaks
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
df <- df %>% add_count(frequency, amplitude, name = "count")

# Calculate Mean Difference of the Control -----------------------------------------------
dfBaseSummary <- df %>%
  filter(frequency == -2) %>%
  group_by(dateTime) %>%
  summarise(
    m_activity = mean(activity),
  ) %>%
  mutate(
    d_activity = m_activity - m_activity[[1]],
  )

# Normalize based on Mean Base Activity -----------------------------------
testUnits <- unique(df$dateTime)

for(i in 1:length(testUnits)) {
  testName <- testUnits[[i]]
  baseDiff <- dfBaseSummary %>% filter(dateTime == testName)
  df <- df %>%
    mutate(
      norm_activity = ifelse(
        dateTime == testName,
        activity / baseDiff$m_activity, norm_activity # we could here change to median
      ),
    )
}

# Cleanup
rm(testUnits, i, testName)

# Arrange Amplitude and Frequency
df <- df %>% arrange(
  frequency, amplitude
)
# generate factor frequency 
df <- df %>% 
  mutate(
    frequency_factor = factor(frequency) %>% fct_recode(Control = "-2")
  )

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
plot_amp_control
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

ggsave(paste0("output/", "amp_p1_lines.pdf"), width = 11, height = 5, p1)
