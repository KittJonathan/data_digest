# Most important statistical tests in R
# The Data Digest
# https://www.youtube.com/watch?v=GI6FfwbZUo0

library(tidyverse)
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