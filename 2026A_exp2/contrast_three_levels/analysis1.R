library(tidyverse)
library(ggplot2)
library(patchwork)

# ============================================================
data_dir   <- "/Users/lana/Desktop/Attention_Contrast/2026A_exp2/contrast_three_levels/subdata"
output_dir <- "/Users/lana/Desktop/Attention_Contrast/2026A_exp2/contrast_three_levels"

# ============================================================
# 1. 全CSVをマージ
# ============================================================
files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
cat("Found", length(files), "files\n")

df_all <- map_dfr(files, \(f) read_csv(f, show_col_types = FALSE))

# 列型変換
df_all <- df_all %>%
  mutate(
    participantNo    = as.integer(participantNo),
    blockNo          = as.integer(blockNo),
    detectorcomp     = as.integer(detectorcomp),
    validity         = as.numeric(validity),
    relevantContrast = as.numeric(relevantContrast),
    respdetect       = as.numeric(respdetect),
    contrastCW       = as.numeric(contrastCW),
    contrastCCW      = as.numeric(contrastCCW),
    precueCWorCCW    = as.numeric(precueCWorCCW),
    respCWorCCW      = as.numeric(respCWorCCW)
  )


# ============================================================
# 2. マージCSV保存
# ============================================================
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
merged_csv <- file.path(output_dir, "2026A_exp2_merged.csv")
write_csv(df_all, merged_csv)
cat("Merged CSV saved:", merged_csv, "\n\n")

# ============================================================
# 3. 共通設定
# ============================================================
cw_values <- c(0, 3.7, 4.9, 6.1)

# validity の表示ラベル（数値→文字）
validity_labels <- c(
  "0"    = "Invalid",
  "1"    = "Valid",
  "1000" = "None"
)

# precue の表示ラベル
precue_labels <- c(
  "0" = "Unattended (Precue: CW)",
  "1" = "Attended (Precue: CCW)"
)

# カラーパレット
col_validity <- c(
  "Invalid" = "#377eb8",
  "Valid"   = "#e41a1c",
  "None"    = "#4daf4a"
)
col_precue <- c(
  "Unattended (Precue: CW)" = "#377eb8",
  "Attended (Precue: CCW)"  = "#e41a1c"
)

# SE計算ヘルパー
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# 共通テーマ
theme_exp <- theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================
# 4. 描画関数（個人用）
# ============================================================

# ---- 4-1. Detect グラフ ------------------------------------
make_detect_plot <- function(pid, data) {
  d <- data %>%
    filter(
      detectorcomp == 0,
      participantNo == pid
    ) %>%
    mutate(
      validity_label = factor(
        as.character(validity),
        levels = names(validity_labels),
        labels = validity_labels
      )
    ) %>%
    group_by(validity_label, relevantContrast) %>%
    summarise(
      mean_resp = mean(respdetect, na.rm = TRUE),
      n         = n(),
      .groups   = "drop"
    )
  
  ggplot(d, aes(x = relevantContrast, y = mean_resp,
                color = validity_label, group = validity_label)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n), y = mean_resp + 0.06),
      size = 3, show.legend = FALSE
    ) +
    scale_color_manual(values = col_validity, name = "Validity") +
    scale_y_continuous(limits = c(-0.05, 1.15),
                       breaks = seq(0, 1, 0.2)) +
    labs(
      title = paste0("Participant ", pid,
                     "  |  Detect task (detectorcomp = 0)"),
      x = "Relevant Contrast",
      y = "Detection Rate  (mean respdetect)"
    ) +
    theme_exp
}

# ---- 4-2. Comp グラフ (upper panel only) --------------------
make_comp_plot <- function(pid, data) {
  base_d <- data %>%
    filter(
      detectorcomp == 1,
      participantNo == pid
    ) %>%
    mutate(
      precue_label = factor(
        as.character(precueCWorCCW),
        levels = names(precue_labels),
        labels = precue_labels
      )
    )
  
  # 上段: x = contrastCCW, facet = contrastCW
  d_upper <- base_d %>%
    mutate(cw_label = factor(paste0("CW = ", contrastCW),
                             levels = paste0("CW = ", cw_values))) %>%
    group_by(precue_label, cw_label, contrastCW, contrastCCW) %>%
    summarise(mean_resp = mean(respCWorCCW, na.rm = TRUE), n = n(),
              .groups = "drop")
  
  ggplot(d_upper, aes(x = contrastCCW, y = mean_resp,
                      color = precue_label, group = precue_label)) +
    geom_vline(aes(xintercept = contrastCW), linetype = "dotted",
               color = "gray40", linewidth = 0.9) +
    geom_line(linewidth = 1.0) +
    geom_point(shape = 15, size = 3) +
    geom_text(aes(label = paste0("n=", n), y = mean_resp + 0.06),
              size = 2.5, show.legend = FALSE) +
    facet_wrap(~ cw_label, nrow = 1) +
    scale_color_manual(values = col_precue, name = "Precue") +
    scale_y_continuous(limits = c(-0.05, 1.15), breaks = seq(0, 1, 0.2)) +
    labs(title = paste0("Participant ", pid, "  |  Comp task (detectorcomp = 1)"),
         x = "contrastCCW", y = "CCW Choice Rate (%)") +
    theme_exp
}

# ============================================================
# 5. 参加者平均プロット関数（SEエラーバー付き）
# ============================================================

# ---- 5-1. Group Average: Detect ----------------------------
make_detect_group_plot <- function(data) {
  
  # Step 1: 参加者ごとの平均を計算
  d_by_pid <- data %>%
    filter(detectorcomp == 0) %>%
    mutate(
      validity_label = factor(
        as.character(validity),
        levels = names(validity_labels),
        labels = validity_labels
      )
    ) %>%
    group_by(participantNo, validity_label, relevantContrast) %>%
    summarise(
      mean_resp = mean(respdetect, na.rm = TRUE),
      .groups   = "drop"
    )
  
  # Step 2: 参加者平均のグループ平均・SEを計算
  d_group <- d_by_pid %>%
    group_by(validity_label, relevantContrast) %>%
    summarise(
      grand_mean = mean(mean_resp, na.rm = TRUE),
      se_resp    = se(mean_resp),
      n_pids     = n(),
      .groups    = "drop"
    )
  
  d_group <- d_group %>%
    mutate(grand_mean = grand_mean * 100,
           se_resp    = se_resp    * 100)
  
  ggplot(d_group, aes(x = relevantContrast, y = grand_mean,
                      color = validity_label, group = validity_label)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = grand_mean - se_resp,
                      ymax = grand_mean + se_resp),
                  width = 0.1, linewidth = 0.8) +
    scale_color_manual(values = col_validity, name = "Validity") +
    scale_y_continuous(limits = c(-5, 115), breaks = seq(0, 100, 20)) +
    labs(
      x = "Relevant Contrast",
      y = "Detection Rate (%)"
    ) +
    theme_exp
}

# ---- 5-2. Group Average: Comp ------------------------------
make_comp_group_plot <- function(data) {
  
  # Step 1: 参加者ごとの平均を計算
  d_by_pid <- data %>%
    filter(detectorcomp == 1) %>%
    mutate(
      precue_label = factor(
        as.character(precueCWorCCW),
        levels = names(precue_labels),
        labels = precue_labels
      ),
      cw_label = factor(paste0("CW = ", contrastCW),
                        levels = paste0("CW = ", cw_values))
    ) %>%
    group_by(participantNo, precue_label, cw_label, contrastCW, contrastCCW) %>%
    summarise(
      mean_resp = mean(respCWorCCW, na.rm = TRUE),
      .groups   = "drop"
    )
  
  # Step 2: グループ平均・SEを計算
  d_group <- d_by_pid %>%
    group_by(precue_label, cw_label, contrastCW, contrastCCW) %>%
    summarise(
      grand_mean = mean(mean_resp, na.rm = TRUE),
      se_resp    = se(mean_resp),
      n_pids     = n(),
      .groups    = "drop"
    )
  
  d_group <- d_group %>%
    mutate(grand_mean = grand_mean * 100,
           se_resp    = se_resp    * 100)
  
  ggplot(d_group, aes(x = contrastCCW, y = grand_mean,
                      color = precue_label, group = precue_label)) +
    geom_vline(aes(xintercept = contrastCW), linetype = "dotted",
               color = "gray40", linewidth = 0.9) +
    geom_line(linewidth = 1.0) +
    geom_point(shape = 15, size = 3) +
    geom_errorbar(aes(ymin = grand_mean - se_resp,
                      ymax = grand_mean + se_resp),
                  width = 0.1, linewidth = 0.8) +
    facet_wrap(~ cw_label, nrow = 1) +
    scale_color_manual(values = col_precue, name = "Precue") +
    scale_y_continuous(limits = c(-5, 115), breaks = seq(0, 100, 20)) +
    labs(
      x = "contrastCCW",
      y = "CCW Choice Rate (%)"
    ) +
    theme_exp
}

# ============================================================
# 6. 参加者ごとにループして保存
# ============================================================
plot_dir <- file.path(output_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

participants <- sort(unique(na.omit(df_all$participantNo)))
cat("Participants:", paste(participants, collapse = ", "), "\n\n")

for (pid in participants) {
  cat("Plotting participant:", pid, "... ")
  
  p_detect <- make_detect_plot(pid, df_all)
  p_comp   <- make_comp_plot(pid, df_all)
  
  ggsave(
    file.path(plot_dir, sprintf("P%02d_1_detect.png", pid)),
    plot = p_detect, width = 8, height = 5.5, dpi = 150
  )
  ggsave(
    file.path(plot_dir, sprintf("P%02d_2_comp.png", pid)),
    plot = p_comp, width = 8, height = 5.5, dpi = 150
  )
  cat("done\n")
}

# ============================================================
# 7. グループ平均プロットの保存
# ============================================================
cat("\nPlotting group averages...\n")

p_detect_group <- make_detect_group_plot(df_all)
p_comp_group   <- make_comp_group_plot(df_all)

ggsave(
  file.path(plot_dir, "GROUP_1_detect_avg.png"),
  plot = p_detect_group, width = 7, height = 4, dpi = 150
)
ggsave(
  file.path(plot_dir, "GROUP_2_comp_avg.png"),
  plot = p_comp_group, width = 7, height = 4, dpi = 150
)

cat("Group average plots saved to:", plot_dir, "\n")
