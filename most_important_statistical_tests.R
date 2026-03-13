# Most important statistical tests in R
# The Data Digest
# https://www.youtube.com/watch?v=GI6FfwbZUo0

library(tidyverse)
library(car)
theme_set(theme_bw())

# Do groups differ in mean or rate?
# t.test(), aov(), TukeyHSD(), var.test(), leveneTest(), prop.test()

# Are two variables associated?
# cor.test(), chisq.test()

# Can X predict Y?
# lm(y ~ x1 + x2), glm(family = binomial)

# T-Test ----

# When to use
# - You're comparing means and your data is roughly normally distributed
# - Choose the version based on group structure (1 group, 2 independent, or paired)

# What it tells you 
# - Whether the mean difference is statistically significant
# - Provides t-statistic, degrees of freedom, and p-value

# Watch out for
# - Normality assumption (especially important for small samples)
# - Equal variance for 2-sample test (use var.test()) or leveneTest()
# - Paired t-test assumes same subjects under both conditions

## One Sample T-Test ----

set.seed(2025)

sample_100 <- data.frame(
  id = 1:100,
  bp = rnorm(n = 100, mean = 140, sd = 10)
)

sample_100

t.test(x = sample_100$bp, mu = 120)

sample_100 |> 
  ggplot(aes(bp)) +
  geom_histogram(color = "black", fill = "grey")

sample_100 |> 
  ggplot(aes(bp, after_stat(density))) +
  geom_histogram(color = "black", fill = "grey") +
  geom_density(color = "orange", linewidth = 1.5) +
  geom_vline(xintercept = 120, linewidth = 1.5, color = "darkgreen")

## Two Sample T-Test ----

sample_A <- sample_100 |> 
  filter(id %in% 1:50)

sample_B <- sample_100 |> 
  filter(id %in% 51:100) |> 
  mutate(bp = bp - 10)

t.test(x = sample_A$bp, y = sample_B$bp)

sample_100 <- rbind(sample_A, sample_B) |> 
  mutate(group = rep(c("A", "B"), each = 50))

t.test(bp ~ group, data = sample_100)

sample_100 |> 
  ggplot(aes(x = group, y = bp,
             color = group)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(height = 0)

sample_100 |> 
  ggplot(aes(x =bp, color = group, fill = group)) +
  geom_density(alpha = 0.3)

t.test(bp ~ group, data = sample_100)
t.test(bp ~ group, data = sample_100, var.equal = TRUE)

var.test(x = sample_100$bp[sample_100$group == "A"],
         y = sample_100$bp[sample_100$group == "B"])

t.test(bp ~ group, data = sample_100,
       var.equal = TRUE,
       alternative = "greater")

## Paired T-Test ----

set.seed(1)
bp1 <- rnorm(n = 25, mean = 140, sd = 15)
bp2 <- bp1 - 5 + rnorm(25) * 10

sample_50 <- data.frame(id = rep(1:25, 2),
                        bp = c(bp1, bp2),
                        group = rep(c("A", "B"), each = 25))

sample_50 |> 
  ggplot(aes(x = group, y = bp, color = group)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(height = 0)

sample_50 |> 
  ggplot(aes(x =bp, color = group, fill = group)) +
  geom_density(alpha = 0.3)

t.test(bp ~ group, data = sample_50)

t.test(x = sample_50$bp[1:25],
       y = sample_50$bp[26:50],
       paired = TRUE)

sample_50 |> 
  ggplot(aes(x = group, y = bp, group = id)) +
  geom_line()

sample_50 |> 
  pivot_wider(values_from = bp, names_from = group) |> 
  mutate(change = case_when(B > A ~ "increase",
                            .default = "decrease")) |> 
  pivot_longer(cols = c(A, B), names_to = "group", values_to = "bp") |> 
  ggplot(aes(x = group, y = bp, group = id, color = change)) +
  geom_line(alpha = 0.6, linewidth = 0.6) +
  geom_point() +
  scale_color_manual(values = c("increase" = "darkred",
                                "decrease" = "darkgreen")) +
  labs(title = "Paired Blood Pressure (before vs. after treatment",
       x = "Group",
       y = "Blood Pressure", 
       color = "Change:")

result <- t.test(x = sample_50$bp[1:25],
                 y = sample_50$bp[26:50],
                 paired = TRUE)

str(result)
result$p.value

change <- sample_50$bp[26:50] - sample_50$bp[1:25]
hist(change)
abline(v = 0, col = "green")
t.test(x = change, mu = 0)

# ANOVA ----

# When to use
# - You're comparing the means across 3 or more groups and your data is 
# approximately normally distributed, with equal variances.

# What it tells you 
# - Whether at least one group differs significantly from the others
# - It gives you the F-statistic, degrees of freedom, and p-value

# Watch out for
# - Only tells you that a difference exists - use TukeyHSD() for follow-up pairwise comparisons
# - Equal variance assumption - use bartlett.test() or leveneTest()
# - Highly sensitive with many groups or large samples - statistical is different 
# from practical significance

## One-way ANOVA ----

himym <- read_csv("himym_episodewise.csv") |> 
  mutate(Season = factor(Season))

himym |> 
  ggplot(aes(x = Season, y = IMDB_Rating,
             fill = Season, color = Season)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(height = 0, width = 0.2)

himym |> 
  ggplot(aes(x = IMDB_Rating)) +
  geom_density() +
  geom_density(data = himym |> filter(Season == 2),
               aes(x = IMDB_Rating), color = "green") +
  geom_density(data = himym |> filter(Season == 9),
               aes(x = IMDB_Rating), color = "red")

himym_plot <- himym |> 
  mutate(plot_group = case_when(
    Season == 2 ~ "Season 2",
    Season == 9 ~ "Season 9",
    .default = "All Other Seasons"
  ))

himym_plot |> 
  ggplot(aes(x = IMDB_Rating, fill = plot_group)) +
  geom_histogram(binwidth = 0.2, color = "white") +
  facet_wrap(~ plot_group, scales = "free_y", nrow = 3) +
  theme(legend.position = "none")

aov(IMDB_Rating ~ Season, data = himym)

summary(aov(IMDB_Rating ~ Season, data = himym))

TukeyHSD(aov(IMDB_Rating ~ Season, data = himym))

# Find pairs of highest difference
aov_model <- aov(IMDB_Rating ~ Season, data = himym)
tukey_df <- as.data.frame(TukeyHSD(aov_model)$Season)
tukey_df$comparison <- rownames(tukey_df)
tukey_sorted <- tukey_df[order(tukey_df$`p adj`), ]

tukey_sorted

tukey_sorted |> 
  ggplot(aes(x = reorder(comparison, `p adj`), y = `p adj`)) +
  geom_point(color = "darkred", size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "gray50") +
  coord_flip()

# Bartlett test of homogeneity of variances
bartlett.test(IMDB_Rating ~ Season, data = himym)
bartlett.test(IMDB_Rating ~ Season, data = himym |> filter(Season != 9))

# Levene test
leveneTest(IMDB_Rating ~ Season, data = himym)

himym |> 
  summarise(variance = var(IMDB_Rating),
            sd = sd(IMDB_Rating),
            n = n(),
            .by = Season)

t.test(IMDB_Rating ~ Season, 
       data = himym |> filter(Season %in% c(2, 9)), 
       var.equal = FALSE)

## Two-way ANOVA ----

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

ToothGrowth |> 
  count(supp, dose)

# run two-way ANOVA with interaction
model <- aov(len ~ supp * dose,
             data = ToothGrowth)

summary(model)

ggplot(ToothGrowth,
       aes(x = dose, y = len, color = supp, group = supp)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line")

# Correlation ----

# When to use
# - You're exploring the strength and direction of the relationship
# between two numeric variables

# What it tells you 
# - The correlation coefficient (range: -1 to +1) and p-value for testing if r != 0

# Watch out for
# - Outliers (can distort r)
# - Non-linear relationships (low r, even if there's a pattern)
# - Correlation != causation
# - Use cor.test() for Pearson, Kendall, Spearman
# - Use corr.test() from {psych} for multiple variables + CI

# Visuals
# - pairs(), GGally::ggpairs()