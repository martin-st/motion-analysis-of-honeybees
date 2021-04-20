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