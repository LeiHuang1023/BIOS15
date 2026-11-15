install.packages("ggplot2")
install.packages("dplyr")
install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(patchwork)

butterflies <- read.csv("Project1/data/butterflies.csv")

butterflies <- butterflies %>%
  mutate(
    LarvalHost = as.factor(LarvalHost),
    MaternalHost = as.factor(MaternalHost),
    Sex = as.factor(Sex),
    MotherID = as.factor(MotherID)
  )

str(butterflies)
table(butterflies$LarvalHost, butterflies$MaternalHost)

model_aov <- lm(AdultWeight ~ LarvalHost * MaternalHost, data = butterflies)

par(mfrow = c(2, 2))
plot(model_aov)
par(mfrow = c(1, 1))

summary(model_aov)
anova(model_aov) 

summary_stats <- butterflies %>%
  group_by(MaternalHost, LarvalHost) %>%
  summarise(
    n = n(),
    Mean = mean(AdultWeight, na.rm = TRUE),
    SD = sd(AdultWeight, na.rm = TRUE),
    SE = SD / sqrt(n),
    .groups = "drop"
  )
print(summary_stats)

host_colors <- c("Barbarea" = "#D55E00", "Berteroa" = "#0072B2")


p1 <- ggplot(butterflies, aes(x = LarvalHost, y = AdultWeight, fill = MaternalHost, color = MaternalHost)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.55,
    alpha = 0.25,
    outlier.shape = NA,
    linewidth = 0.7
  ) +

  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    alpha = 0.45,
    size = 1.6
  ) +
  scale_fill_manual(values = host_colors, name = "Maternal Host") +
  scale_color_manual(values = host_colors, name = "Maternal Host") +
  labs(
    x = "Larval Host Plant",
    y = "Adult Weight (mg)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.4)
  )

p2 <- ggplot(butterflies, aes(x = LarvalHost, y = AdultWeight, color = MaternalHost, group = MaternalHost)) +
  stat_summary(
    fun = mean,
    geom = "line",
    position = position_dodge(width = 0.15),
    linewidth = 1
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.15),
    width = 0.12,
    linewidth = 0.85
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    position = position_dodge(width = 0.15),
    size = 3.5
  ) +
  scale_color_manual(values = host_colors, name = "Maternal Host") +
  labs(
    x = "Larval Host Plant",
    y = "Mean Adult Weight ± SE (mg)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.4)
  )


combined_plot <- (p1 + p2) +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(
    legend.position = "top",
    plot.tag = element_text(face = "bold", size = 13)
  )

print(combined_plot)

ggsave("Project1/plots/butterfly_combined_plot.png", plot = combined_plot, width = 9.5, height = 4.5, dpi = 300)
