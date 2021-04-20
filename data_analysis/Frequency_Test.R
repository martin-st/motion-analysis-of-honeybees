# Libs ----------------------------------------------------
library(tidyverse)
library(readr)
library(here)
library(multcompView)
# Not used in Paper
library(ggsignif)
library(coin)
library(broom)

# colorblind friendly colours
cbb_Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Read Data ---------------------------------------------------------------
# Read data and drop_na as cleanup, if we have empty rows appended by accident
df <- read_csv("./datasets/frequency_experiments.csv") %>%
  drop_na()

# Activity Cols Description -----------------------------------------------
# activity: four white points
# new_activity_calculation: smaller ROI, symmetrical around the emitter
# activity_calculation_frame: ROI over whole frame

# Normalize ---------------------------------------------------------------
source(paste0(here(), "/data_analysis/utils.R"))

# Plotting Loop of Different ROIs -----------------------------------------------------------
# in the final dataset we only have one ROI left
# our different roi techniques
rois <- c("norm_activity")

for(i in seq_along(rois)){
  roi <- rois[i]
  # col y will include our normalized value for given roi
  df <- bind_cols(df, df[,roi] %>% rename(., "y" = roi))
  
  # dateTime and each "block" with sd
  plot1 <- df %>% 
    group_by(date, dateTime, frequency_factor, subgroup) %>% 
    summarise(
      m = mean(y),
      sd = sd(y)
    ) %>% 
    ggplot(aes(frequency_factor, m, color = dateTime)) +
    geom_hline(aes(yintercept = 1), color = "black", alpha = 0.5) +
    geom_pointrange(aes(ymin = m - sd, ymax = m + sd)) +
    scale_color_hue(l = 40) +
    ggtitle(paste("Roi:", roi), subtitle = "Block Mean + SD") +
    ylab("Normalized Activity") + xlab("Frequency") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 65, vjust = 0.5)
    )
  
  plot2 <- df %>%
    group_by(date, frequency_factor) %>%
    summarise(
      m = mean(y),
      sd = sd(y)
    ) %>%
    ggplot(aes(frequency_factor, m, color = date)) +
    geom_hline(aes(yintercept = 1), color = "black", alpha = 0.5)+
    geom_pointrange(aes(ymin = m - sd, ymax = m + sd)) +
    scale_color_manual(values = cbb_Palette) +
    ggtitle(paste("Roi:", roi), subtitle = "Day Mean + SD") +
    ylab("Normalized Activity") + xlab("Frequency") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 65, vjust = 0.5)
    )

  plot3 <- df %>%
    ggplot(aes(frequency_factor, y, color = date)) +
    geom_hline(aes(yintercept = 1), color = "black", alpha = 0.5) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = cbb_Palette) +
    ggtitle(paste("Roi:", roi), subtitle = "All Datapoints colored by given date") +
    ylab("Normalized Activity") + xlab("Frequency") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 65, vjust = 0.5)
    )

  ggsave(paste0("./data_analysis/output/", roi,"_plot1.pdf"), width = 10, plot1) 
  ggsave(paste0("./data_analysis/output/", roi,"_plot2.pdf"), width = 10, plot2) 
  ggsave(paste0("./data_analysis/output/", roi,"_plot3.pdf"), width = 10, plot3) 

  # drop y for next round
  df <- df %>% select(-"y")
}

# Plots Points -------------------------------------------------------------
# Drop 440 Frequency, did not work
p1_df <- df %>%
  filter(frequency_factor != "WN" & frequency_factor != "440") %>%
  group_by(frequency) %>%
  summarize(
    mean = mean(norm_activity),
    upper = mean + sd(norm_activity),
    lower = mean - sd(norm_activity)
  ) %>%
  mutate(
    colorb = ifelse(mean > 1, "A", "B"),
    colorb = ifelse(mean == 1, "1", colorb),
    frequency = ifelse(frequency == -2, 0, frequency)
    )

p1_df_group <- p1_df %>% 
  mutate(
    colorb = ifelse(frequency >= 500, "C", colorb),
    colorb = fct_recode(colorb, Control = "1", A = "A", B = "B")
  ) %>% 
  group_by(colorb) %>% 
  summarise(
    xmin = min(frequency),
    xmax = max(frequency)
  ) %>% 
  filter(colorb != "Control")

p1 <- p1_df %>% 
  ggplot(aes(x = frequency, y = mean, color = colorb)) +
  geom_hline(
    aes(yintercept = 1), color = "black", alpha = 0.5, show.legend = F
    ) +
  geom_line(
    aes(color = NA), color = "black", show.legend = F
    ) +
  #geom_function(fun=f, colour="red") +
  geom_rect(
    data = p1_df_group,
    aes(
      xmin = xmin, xmax = xmax, 
      ymin = -Inf, ymax = Inf, fill = colorb, color = NA),
    alpha = 0.2, show.legend = T, inherit.aes = FALSE
    ) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper), show.legend = F
    ) +
  scale_color_manual(values = cbb_Palette, na.translate = F) +
  guides(color = FALSE) +
  labs(fill = "Group") +
  scale_fill_manual(values = cbb_Palette[-1]) +
  ylim(0, 1.25) +
  ylab("Relative Pixel-Value Change") +
  xlab("Frequency") +
  scale_x_continuous(breaks = c(unique(p1_df$frequency))) +
  scale_y_continuous(breaks = seq(0,5,0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3))

ggsave(paste0("./data_analysis/output/", "freq_p1_points.pdf"), width = 11, height = 5, p1)

# Permutation Test  -------------------------------------------------------
# Did not make it into the final Paper
# preverable we would like to make permutation tests and we would only do white noise against control
# Dataframe of only Control and White Noise
df_test <- df %>% 
  filter(frequency_factor == "Control" | frequency_factor == "WN") %>%
  mutate(
    dateTime = as_factor(dateTime),
    subgroup = as_factor(subgroup)
  )

# we use permutation test -> distribution is no problem
# repeated measurement, we need to take it into with dateTime
(wt <- coin::oneway_test(
  norm_activity ~ frequency_factor | dateTime, 
  data = df_test,
  distribution = approximate(10000)
))

p2_text = paste0(
  "Z = ", round(statistic(wt),1),
  ", ",
  "p-value ", ifelse(pvalue(wt)<0.001, "< 0.001", pvalue(wt))
)

p2 <- df_test %>% 
  ggplot(aes(x = frequency_factor, y = norm_activity)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,5,0.2)) +
  ylab("Relative Pixel-Value Change") +
  xlab("") +
  labs(caption = "Approximative Two-Sample Fisher-Pitman Permutation Test, 10,000 resamples") +
  scale_x_discrete(labels = c("Control" = "Control", "WN" = "White Noise")) +
  ggsignif::geom_signif(
    annotations = p2_text,
    y_position = c(1.6), xmin=c(1), xmax=c(2)
    ) +
  theme_classic()

ggsave(paste0("./data_analysis/output/", "freq_p2_test.pdf"), p2, width = 5, height = 5) 


# BoxPlots ----------------------------------------------------------------
p3_df <- df %>%
  left_join(p1_df, by = c("frequency")) %>% 
  mutate(
    colorb = ifelse(is.na(colorb), "1", colorb)
  )

p3_df_group <-
  tibble(
    Group = p1_df_group$colorb,
    xmin = c(2, 5, 11),
    xmax = c(4, 10, 20)
  )
# See above, we did drop 440 because of data error
p3_df <- p3_df %>% 
  filter(frequency_factor != "440") %>%
  mutate(
    frequency_factor = fct_relevel(frequency_factor, "WN", after = Inf),
    frequency_factor = fct_recode(frequency_factor, `White\nNoise` = "WN")
  )

p3 <- p3_df %>%
  ggplot(aes(x = frequency_factor, y = norm_activity, color = colorb)) +
  geom_hline(
    aes(yintercept = 1), color = "black", alpha = 0.5, show.legend = F
  ) +
  geom_boxplot() +
  theme_classic()  +
  geom_rect(
    data = p3_df_group,
    aes(
      xmin = xmin, xmax = xmax, 
      x = 0, y = 0,
      ymin = -Inf, ymax = Inf, fill = Group, color = NA), 
    alpha = 0.2, show.legend = T, inherit.aes = T
  ) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,5,0.2)) +
  scale_color_manual(values = cbb_Palette, na.translate = F) +
  guides(color = FALSE) +
  labs(fill = "Group") +
  scale_fill_manual(values = cbb_Palette[-1]) +
  ylab("Relative Pixel-Value Change") +
  xlab("Frequency")

ggsave(paste0("./data_analysis/output/", "freq_p3_box.pdf"), width = 11, height = 5, p3) 

# Pairwise T-Test ---------------------------------------------------------
# parametic and non-parametic give both more or less same results
# we choose non-parametic as some assumptions for parametic are not met
res     <- aov(norm_activity ~ frequency_factor, data = df)
res_hsd <- TukeyHSD(res)
p.res   <- tidy(res_hsd)
write_csv2(p.res, "output/pairwise_HSD.csv")

# Same letters indiciate non-siginifant comparissons
multcompLetters(res_hsd$frequency_factor[,4])
letters   <- multcompLetters4(res, res_hsd)
lettersDf <- tibble(
  frequency_factor = names(letters[["frequency_factor"]][["Letters"]]),
  letters = letters[["frequency_factor"]][["Letters"]]
  ) %>%
  mutate(
    frequency_factor = ifelse(frequency_factor == "WN", "White\nNoise", frequency_factor)
  )

p3_letters <- p3_df %>%
  left_join(lettersDf, by = c("frequency_factor")) %>% 
  group_by(frequency_factor) %>% summarise(
    y = max(norm_activity),
    letter = unique(letters)
  )

p4 <- p3_df %>% 
  ggplot(aes(x = frequency_factor, y = norm_activity, color = colorb)) +
  geom_hline(
    aes(yintercept = 1), color = "black", alpha = 0.5, show.legend = F
  ) +
  geom_boxplot() +
  geom_text(
    data = p3_letters, 
    aes(y = 0, label = letter, color = "black"),
    color = "black",
    alpha = 0.5,
    nudge_y = -0.05
  ) +
  theme_classic()  +
  geom_rect(
    data = p3_df_group,
    aes(
      xmin = xmin, xmax = xmax, 
      x = 0, y = 0,
      ymin = -Inf, ymax = Inf, fill = Group, color = NA), 
    alpha = 0.2, show.legend = T, inherit.aes = T
  ) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,5,0.2)) +
  scale_color_manual(values = cbb_Palette, na.translate = F) +
  guides(color = FALSE) +
  labs(fill = "Group") +
  scale_fill_manual(values = cbb_Palette[-1]) +
  ylab("Relative Pixel-Value Change") +
  xlab("Frequency")

ggsave(paste0("./data_analysis/output/", "freq_p3_box_sign.pdf"), width = 11, height = 5, p4) 

# Pairwise T-Test - Non-Parametic -----------------------------------------
# see above we choose to use non-parametic test, its not as powerful
# but also takes less assumptions
# res_k     <- kruskal.test(norm_activity ~ 0 + frequency_factor, data = df)
res_hsd_w <- pairwise.wilcox.test(df$norm_activity, df$frequency_factor, p.adj = "holm")
p.res_w   <- tidy(res_hsd_w)
write_csv2(p.res_w, "output/pairwise_wilcox_holm.csv")
# matrix from pairwise wilcox is not quatratic
# generate named vector for multcompLetters, see help for multcompLetters
test_vector        <- p.res_w$p.value
names(test_vector) <- paste(p.res_w$group1, p.res_w$group2, sep = "-")
(letters_w         <- multcompLetters(test_vector))
# Generate Tibble for Joining with Main Table
lettersDf_w <- tibble(
  frequency_factor = names(letters_w$Letters),
  letters = letters_w$Letters
) %>% 
  mutate(
    frequency_factor = ifelse(frequency_factor == "WN", "White\nNoise", frequency_factor)
  )
# Plot df
p5_letters <- p3_df %>%
  left_join(lettersDf_w, by = c("frequency_factor")) %>% 
  group_by(frequency_factor) %>% summarise(
    y = max(norm_activity),
    letter = unique(letters)
  )

p5 <- p3_df %>% 
  ggplot(aes(x = frequency_factor, y = norm_activity, color = colorb)) +
  geom_hline(
    aes(yintercept = 1), color = "black", alpha = 0.5, show.legend = F
  ) +
  geom_boxplot() +
  geom_text(
    data = p5_letters, 
    aes(y = 0, label = letter, color = "black"),
    color = "black",
    alpha = 0.5,
    nudge_y = -0.05
  ) +
  theme_classic()  +
  geom_rect(
    data = p3_df_group,
    aes(
      xmin = xmin, xmax = xmax, 
      x = 0, y = 0,
      ymin = -Inf, ymax = Inf, fill = Group, color = NA), 
    alpha = 0.2, show.legend = T, inherit.aes = T
  ) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,5,0.2)) +
  scale_color_manual(values = cbb_Palette, na.translate = F) +
  guides(color = FALSE) +
  labs(fill = "Group") +
  scale_fill_manual(values = cbb_Palette[-1]) +
  ylab("Relative Pixel-Value Change") +
  xlab("Frequency")

ggsave(paste0("./data_analysis/output/", "freq_p5_box_sign_wilcox.pdf"), width = 11, height = 5, p5)

