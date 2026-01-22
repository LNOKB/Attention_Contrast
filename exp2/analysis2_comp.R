library(tidyverse)
data <- read_csv("dat_comp.csv")

summary_stats <- data %>%
  group_by(participant, contrast, cuetype) %>%
  summarise(
    mean_selectrate = mean(select_test, na.rm = TRUE)*100,
    se_selectrate = sd(select_test, na.rm = TRUE)*100 / sqrt(n()),#このSEの計算の仕方あってるかな？
    n = n(),
    .groups = "drop"
  )

n_participants <- length(unique(data$participant))
ncol_value <- ceiling(sqrt(n_participants))  

p <- ggplot(summary_stats, aes(x = contrast, y = mean_selectrate,
                               color = factor(cuetype), group = factor(cuetype))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_selectrate - se_selectrate,
                    ymax = mean_selectrate + se_selectrate),
                width = 0.3, linewidth = 0.8) +
  scale_color_manual(
    values = c("-1" = "#E74C3C", "0" = "#95A5A6", "1" = "#3498DB"),
    labels = c("-1" = "anti-cued", "0" = "uncued", "1" = "cued"),
    name = "CueType"
  ) +
  scale_x_continuous(breaks = sort(unique(summary_stats$contrast))) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  facet_wrap(~ participant, ncol = ncol_value) +  # この行を追加
  labs(
    x = "Contrast (%)",
    y = "Test Selection Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "gray90", color = "gray70"),  
    strip.text = element_text(face = "bold")  
  )

ggsave("comp.png",
       plot = p, width = 25, height = 10, dpi = 300)  
