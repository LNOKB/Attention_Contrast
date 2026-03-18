library(tidyverse)
library(ggplot2)
library(effectsize)
library(plotly)
library(viridis)

dat <- read_csv("2026A_exp2_merged.csv")
n <- 17

# detect
result = dat %>%             
  filter(detectorcomp %in% c(0)) %>%
  select(
    validity, detectorcomp, contrastCW, contrastCCW, precueCWorCCW, oriCW, oriCCW, 
    outframe, probedcontrast, respdetect, relevantContrast, correctDetect, TorFdetect,
    respCWorCCW, key_resp.keys, participantNo, blockNo
  ) %>%    
  group_by(participantNo, validity, relevantContrast) %>%                
  summarize(detect_rate = mean(respdetect), .groups = "drop")

# comparison
result2 = dat %>%             
  filter(detectorcomp %in% c(1)) %>%
  select(
    contrastCW, contrastCCW, precueCWorCCW, oriCW, oriCCW, 
    outframe, respCWorCCW, participantNo, blockNo
  ) %>%    
  group_by(participantNo, precueCWorCCW, contrastCW, contrastCCW) %>%                
  summarize(CCW_choice_rate = mean(respCWorCCW), .groups = "drop")

# fixed parameters
sigma_attended <- 1
mu_attended_0  <- 0

# contrast値 → muインデックス対応
contrast_to_idx <- c("0" = 1, "3.7" = 2, "4.9" = 3, "6.1" = 4)

# ============================================================
### Likelihood function（detect + comp 合算）
# ============================================================
PFCI_logL <- function(x, data_detect, data_comp) {
  
  mu_attended_3.7   <- x[1]
  mu_attended_4.9   <- x[2]
  mu_attended_6.1   <- x[3]
  lambda_unattended <- x[4]
  sigma_unattended  <- x[5]
  theta_detect      <- x[6]
  theta_comp        <- x[7]
  # theta_comp = 0 固定
  
  mus_attended   <- c(mu_attended_0, mu_attended_3.7, mu_attended_4.9, mu_attended_6.1)
  mus_unattended <- mus_attended * lambda_unattended
  
  # ---------- detect logL ----------
  pred_detect <- matrix(NA, nrow = 8, ncol = 2)
  for (cond in 1:4) {
    pred_detect[cond, 1] <- pnorm(theta_detect, mean = mus_unattended[cond], sd = sigma_unattended,
                                  lower.tail = FALSE)
    pred_detect[cond, 2] <- 1 - pred_detect[cond, 1]
  }
  for (cond in 5:8) {
    pred_detect[cond, 1] <- pnorm(theta_detect, mean = mus_attended[cond - 4], sd = sigma_attended,
                                  lower.tail = FALSE)
    pred_detect[cond, 2] <- 1 - pred_detect[cond, 1]
  }
  pred_detect  <- pmax(pred_detect, 1e-10)
  logL_detect  <- sum(data_detect * log(pred_detect))
  
  # ---------- comp logL ----------
  pred_comp <- numeric(nrow(data_comp))
  
  for (row in 1:nrow(data_comp)) {
    cw_contrast  <- as.character(data_comp$contrastCW[row])
    ccw_contrast <- as.character(data_comp$contrastCCW[row])
    precue       <- data_comp$precueCWorCCW[row]
    
    idx_cw  <- contrast_to_idx[cw_contrast]
    idx_ccw <- contrast_to_idx[ccw_contrast]
    
    if (precue == 0) {
      # precue=0: CW=attended, CCW=unattended
      mu_cw  <- mus_attended[idx_cw];   sd_cw  <- sigma_attended
      mu_ccw <- mus_unattended[idx_ccw]; sd_ccw <- sigma_unattended
    } else {
      # precue=1: CCW=attended, CW=unattended
      mu_cw  <- mus_unattended[idx_cw]; sd_cw  <- sigma_unattended
      mu_ccw <- mus_attended[idx_ccw];  sd_ccw <- sigma_attended
    }
    
    # P(choose CCW) = P(X_CCW - X_CW > theta_comp)
    mu_diff  <- mu_ccw - mu_cw
    sd_diff  <- sqrt(sd_cw^2 + sd_ccw^2)
    pred_comp[row] <- pnorm(theta_comp, mean = mu_diff, sd = sd_diff, lower.tail = FALSE)
  }
  
  pred_comp <- pmax(pmin(pred_comp, 1 - 1e-10), 1e-10)
  obs_ccw   <- data_comp$CCW_choice_rate
  logL_comp <- sum(obs_ccw * log(pred_comp) + (1 - obs_ccw) * log(1 - pred_comp))
  
  # ---------- 合算 ----------
  global_pred_detect <<- pred_detect
  global_pred_comp   <<- pred_comp
  
  logL_total <- logL_detect + logL_comp
  if (!is.finite(logL_total)) logL_total <- -1e10
  return(-logL_total)
}

# ============================================================
### Fitting function
# ============================================================
fit_PFCI_mle <- function(data_detect, data_comp, add_constant = TRUE) {
  if (add_constant) {
    data_detect <- data_detect + 0.5
  }
  
  guess        <- c(1, 2, 3, 1, 1, 2, 0)            # 7パラメータ
  lower_bounds <- c(0, 0, 0, 0.5, 0.5, 0.5, -3)
  upper_bounds <- c(3.5, 3.5, 3.5, 2.0, 2.0, 5.0, 3)
  
  fit <- suppressWarnings(
    optim(
      par         = guess,
      fn          = PFCI_logL,
      data_detect = data_detect,
      data_comp   = data_comp,
      lower       = lower_bounds,
      upper       = upper_bounds,
      method      = "L-BFGS-B",
      control     = list(maxit = 10000)
    )
  )
  
  est <- data.frame(
    mu_attended_3.7   = fit$par[1],
    mu_attended_4.9   = fit$par[2],
    mu_attended_6.1   = fit$par[3],
    lambda_unattended = fit$par[4],
    sigma_unattended  = fit$par[5],
    theta_detect      = fit$par[6],
    theta_comp        = fit$par[7],
    logL              = fit$value
  )
  return(list(est, global_pred_detect, global_pred_comp))
}

# ============================================================
### Conducting fitting on individual data
# ============================================================
estimates         <- c()
predicted_array   <- array(NA, dim = c(8, 2, n))
data_rate_array   <- array(NA, dim = c(8, 2, n))
pred_comp_list    <- vector("list", n)

participants <- sort(unique(result$participantNo))

for (i in seq_along(participants)) {
  pid <- participants[i]
  
  # detectデータ（8行 × 2列）
  subdata_detect <- result[result$participantNo == pid, ]
  yes_rate    <- subdata_detect$detect_rate
  data_detect <- cbind(yes_rate, 1 - yes_rate)
  data_rate_array[, , i] <- data_detect
  
  # compデータ（32行のdata.frame）
  data_comp <- result2[result2$participantNo == pid, ]
  
  # fitting
  fit <- fit_PFCI_mle(data_detect, data_comp, add_constant = TRUE)
  df  <- fit[[1]]
  df$sub <- i
  estimates            <- rbind(estimates, df)
  predicted_array[, , i] <- fit[[2]]
  pred_comp_list[[i]]  <- fit[[3]]
  
  cat("Participant", pid, "done\n")
}

# ============================================================
### Preparation for data visualization
# ============================================================
mean_predicted <- apply(predicted_array, c(1, 2), mean) * 100
mean_data      <- apply(data_rate_array, c(1, 2), mean) * 100
se_data        <- apply(data_rate_array, c(1, 2), sd)   * 100 / sqrt(n)

mean_predicted2 <- mean_predicted[, 1]
mean_data2      <- mean_data[, 1]
se_data2        <- se_data[, 1]

# ============================================================
### t-test, lambda
# ============================================================
lambda_log <- log(estimates[, 4])
t_test_below_lambda <- t.test(lambda_log, mu = 0, alternative = "less")
t_test_above_lambda <- t.test(lambda_log, mu = 0, alternative = "greater")
effect <- cohens_d(lambda_log, mu = 0)
print(t_test_below_lambda)
print(t_test_above_lambda)
print(effect)

### t-test, sigma
sigma_log <- log(estimates[, 5])
t_test_below_sigma <- t.test(sigma_log, mu = 0, alternative = "less")
t_test_above_sigma <- t.test(sigma_log, mu = 0, alternative = "greater")
effect <- cohens_d(sigma_log, mu = 0)
print(t_test_below_sigma)
print(t_test_above_sigma)
print(effect)

# ============================================================
### violin plot（λ, σ, theta_detect, theta_comp）
# ============================================================
data_parameter_plot <- data.frame(
  Parameters = rep(c("λunattended", "σunattended", "θdetect", "θcomp"), each = n),
  Value = c(estimates[, 4], estimates[, 5], estimates[, 6], estimates[, 7])
)
# θ系はlog変換不要なのでそのまま、λ・σはlog軸で見たい場合は別途
parameters_graph <- ggplot(data_parameter_plot, aes(x = Parameters, y = Value)) +
  geom_violin(fill = "skyblue", color = "black", scale = "width") +  
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
  labs(y = "Value") + 
  scale_x_discrete("Parameters", limits = c("λunattended", "σunattended", "θdetect", "θcomp"),
                   labels = c(expression("λ"[unattended]), expression("σ"[unattended]),
                              expression("θ"[detect]),     expression("θ"[comp]))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "black") +
  theme_classic() +  
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 14 * 2),  
    axis.text.x  = element_text(size = 11 * 2),  
    axis.text.y  = element_text(size = 11 * 2)    
  )
plot(parameters_graph)
ggsave(file = "parameters_graph.png", plot = parameters_graph, dpi = 150, width = 12, height = 6)

# ============================================================
### Individual participant plots
# ============================================================
plot_dir <- "plots_individual"
dir.create(plot_dir, showWarnings = FALSE)

for (i in seq_along(participants)) {
  pid <- participants[i]
  
  # --- detect ---
  detect_i <- data.frame(
    Imagetype  = factor(rep(c("0%", "3.7%", "4.9%", "6.1%"), 2),
                        levels = c("0%", "3.7%", "4.9%", "6.1%")),
    Observed   = c(data_rate_array[1:4, 1, i], data_rate_array[5:8, 1, i]) * 100,
    Predicted  = c(predicted_array[1:4, 1, i], predicted_array[5:8, 1, i]) * 100,
    Condition  = factor(c(rep("unattended", 4), rep("attended", 4)),
                        levels = c("attended", "unattended"))
  )
  
  p_detect_i <- ggplot(detect_i, aes(x = Imagetype, y = Observed)) +
    geom_bar(stat = "identity") +
    geom_point(aes(y = Predicted), color = "red", size = 3) +
    facet_grid(. ~ Condition) +
    scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
    labs(title = paste0("P", pid, "  |  Detect"),
         x = NULL, y = "Detection rate (%)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --- comp ---
  comp_i <- comp_pred_df %>%
    filter(participantNo == pid) %>%
    mutate(
      precue_label = factor(
        ifelse(precueCWorCCW == 0,
               "Attended: CW  (CCW unattended)",
               "Attended: CCW  (CW unattended)"),
        levels = c("Attended: CW  (CCW unattended)",
                   "Attended: CCW  (CW unattended)")
      ),
      cw_label = factor(paste0("CW = ", contrastCW),
                        levels = paste0("CW = ", c(0, 3.7, 4.9, 6.1))),
      obs_pct  = CCW_choice_rate * 100,
      pred_pct = pred_CCW * 100
    )
  
  p_comp_i <- ggplot(comp_i, aes(x = contrastCCW, y = obs_pct)) +
    geom_vline(aes(xintercept = contrastCW),
               linetype = "dotted", color = "gray40", linewidth = 0.8) +
    geom_line(linewidth = 1.0, color = "#377eb8") +
    geom_point(size = 3, color = "#377eb8") +
    geom_line(aes(y = pred_pct), linewidth = 1.0, color = "#e41a1c") +
    geom_point(aes(y = pred_pct), size = 3, color = "#e41a1c", shape = 17) +
    facet_grid(precue_label ~ cw_label) +
    scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
    labs(title = paste0("P", pid, "  |  Comp  (blue=obs, red=pred)"),
         x = "contrastCCW", y = "CCW Choice Rate (%)") +
    theme_bw(base_size = 11) +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 9))
  
  ggsave(file.path(plot_dir, sprintf("P%02d_detect.png", pid)),
         plot = p_detect_i, width = 8,  height = 5, dpi = 150)
  ggsave(file.path(plot_dir, sprintf("P%02d_comp.png",   pid)),
         plot = p_comp_i,   width = 14, height = 7, dpi = 150)
  
  cat("Individual plots saved: P", pid, "\n")
}

# ============================================================
### Estimated distribution
# ============================================================
plot_sdt_distributions <- function(means, sds, attention_levels, contrast_types, colors) {
  data_SDT_plot <- data.frame()
  
  for (i in 1:length(attention_levels)) {
    for (j in 1:length(contrast_types)) {
      x <- seq(means[i, j] - 3 * sds[i, j], means[i, j] + 3 * sds[i, j], length.out = 100)
      y <- dnorm(x, mean = means[i, j], sd = sds[i, j]) 
      
      data_SDT_plot <- rbind(data_SDT_plot, data.frame(
        x = x, y = y,
        Attention = attention_levels[i],
        Contrast = contrast_types[j],
        color     = colors[j]
      ))
    }
  }
  
  theta_comp_mean <- mean(estimates[, 7])
  
  Distribution <- ggplot(data_SDT_plot, aes(x = x, y = y, color = Contrast)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = colors) +
    labs(x = "Signal Strength", y = "Probability density") +
    scale_x_continuous(limits = c(-3, 6)) +
    scale_y_continuous(breaks = seq(0, 0.6, length = 4), limits = c(0, 0.6)) +
    geom_vline(xintercept = mean(estimates[, 6]), linetype = "dashed", color = "black") + 
    annotate("text", x = 4.5, y = 0.55,
             label = sprintf("θcomp = %.2f", theta_comp_mean),
             size = 5, hjust = 0, color = "black") +
    facet_wrap(~ Attention, nrow = 2, scales = "free_y") +  
    theme_minimal(base_size = 18) +
    theme(
      panel.grid.major  = element_blank(),   
      panel.grid.minor  = element_blank(),   
      axis.line         = element_line(size = 0.5, color = "black"), 
      axis.ticks        = element_line(color = "black"),  
      axis.ticks.length = unit(0.3, "cm"),  
      legend.position   = "bottom"
    )
  
  plot(Distribution)
  ggsave(file = "Distribution.png", plot = Distribution, dpi = 150, width = 8, height = 6)
}

means_attended   <- c(mu_attended_0, mean(estimates[, 1]), mean(estimates[, 2]), mean(estimates[, 3]))
means_unattended <- means_attended * mean(estimates[, 4])
means            <- rbind(means_attended, means_unattended)
sigma_attended_v  <- rep(sigma_attended,          4)
sigma_unattended_v <- rep(mean(estimates[, 5]),   4)
sds <- rbind(sigma_attended_v, sigma_unattended_v)

attention_levels <- factor(c("attended", "unattended"), levels = c("attended", "unattended"))
contrast_types <- factor(c("0%", "3.7%", "4.9%", "6.1%"), levels = c("0%", "3.7%", "4.9%", "6.1%"))
colors <- viridis(4, option = "plasma") 

plot_sdt_distributions(means, sds, attention_levels, contrast_types, colors)

# ============================================================
### Model prediction bar graph (detect)
# ============================================================
data_bar <- data.frame(
  Contrast = factor(rep(c("0%", "3.7%", "4.9%", "6.1%"), 2),
                     levels = c("0%", "3.7%", "4.9%", "6.1%")),
  Proportion = as.vector(t(mean_data2)),
  SE         = as.vector(t(se_data2)),
  Condition  = factor(c(rep("attended", 4), rep("unattended", 4)),
                      levels = c("attended", "unattended"))
)

predicted_data_bar <- data.frame(
  Contrast = factor(rep(c("0%", "3.7%", "4.9%", "6.1%"), 2),
                     levels = c("0%", "3.7%", "4.9%", "6.1%")),
  Predicted = as.vector(t(mean_predicted2)),
  Condition = factor(c(rep("attended", 4), rep("unattended", 4)),
                     levels = c("attended", "unattended"))
)

bar_graph <- ggplot(data_bar, aes(x = Contrast, y = Proportion)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Detection rate (%)") +
  facet_grid(. ~ Condition) +  
  geom_point(data = predicted_data_bar, aes(x = Contrast, y = Predicted),
             color = "red", size = 3) +  
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), width = 0.2) + 
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 24, angle = 45, hjust = 1, color = "black"),   
    axis.text.y  = element_text(size = 22),    
    axis.title.y = element_text(size = 28),
    strip.text   = element_text(size = 36)
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) 

plot(bar_graph)
ggsave(file = "bar_graph.png", plot = bar_graph, dpi = 150, width = 14, height = 8)

# ============================================================
### Model prediction graph (comp)
# ============================================================

# pred_comp_list から参加者×条件のデータフレームを作成
comp_pred_df <- do.call(rbind, lapply(seq_along(participants), function(i) {
  pid       <- participants[i]
  data_comp <- result2[result2$participantNo == pid, ]
  data_comp$pred_CCW <- pred_comp_list[[i]]
  data_comp$participantNo <- pid
  data_comp
}))

# グループ平均・SE（参加者平均を経由）
comp_obs_group <- comp_pred_df %>%
  group_by(participantNo, precueCWorCCW, contrastCW, contrastCCW) %>%
  summarise(
    obs  = mean(CCW_choice_rate),
    pred = mean(pred_CCW),
    .groups = "drop"
  ) %>%
  group_by(precueCWorCCW, contrastCW, contrastCCW) %>%
  summarise(
    mean_obs  = mean(obs)  * 100,
    se_obs    = sd(obs)    * 100 / sqrt(n),
    mean_pred = mean(pred) * 100,
    .groups   = "drop"
  ) %>%
  mutate(
    precue_label = factor(
      ifelse(precueCWorCCW == 0,
             "Attended: CW  (CCW unattended)",
             "Attended: CCW  (CW unattended)"),
      levels = c("Attended: CW  (CCW unattended)",
                 "Attended: CCW  (CW unattended)")
    ),
    cw_label = factor(paste0("CW = ", contrastCW),
                      levels = paste0("CW = ", c(0, 3.7, 4.9, 6.1)))
  )

comp_graph <- ggplot(comp_obs_group,
                     aes(x = contrastCCW, y = mean_obs, group = 1)) +
  geom_vline(aes(xintercept = contrastCW),
             linetype = "dotted", color = "gray40", linewidth = 0.8) +
  geom_errorbar(aes(ymin = mean_obs - se_obs, ymax = mean_obs + se_obs),
                width = 0.15, linewidth = 0.7) +
  geom_line(linewidth = 1.0, color = "#377eb8") +
  geom_point(size = 3, color = "#377eb8") +
  geom_line(aes(y = mean_pred), linewidth = 1.0,
            color = "#e41a1c", linetype = "solid") +
  geom_point(aes(y = mean_pred), size = 3,
             color = "#e41a1c", shape = 17) +
  facet_grid(precue_label ~ cw_label) +
  scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
  labs(
    x     = "contrastCCW",
    y     = "CCW Choice Rate (%)",
    title = "Comp task: Observed (blue) vs Model prediction (red)",
    caption = "Error bars = ±1 SE across participants\nDotted line = contrastCW"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 10),
    plot.title       = element_text(size = 12, face = "bold")
  )

plot(comp_graph)
ggsave(file = "comp_graph.png", plot = comp_graph,
       dpi = 150, width = 14, height = 7)

# ============================================================
### d' plot for detect task
# ============================================================

contrast_levels <- c(0, 3.7, 4.9, 6.1)

dprime_df <- estimates %>%
  mutate(
    # attended
    dp_att_0   = (mu_attended_0   - mu_attended_0) / sigma_attended,  # = 0
    dp_att_3.7 = (mu_attended_3.7 - mu_attended_0) / sigma_attended,  # = mu_attended_3.7
    dp_att_4.9 = (mu_attended_4.9 - mu_attended_0) / sigma_attended,
    dp_att_6.1 = (mu_attended_6.1 - mu_attended_0) / sigma_attended,
    
    # unattended
    dp_una_0   = (mu_attended_0  　- mu_attended_0) * lambda_unattended　/ sigma_unattended,  # = 0
    dp_una_3.7 = (mu_attended_3.7　- mu_attended_0) * lambda_unattended　/ sigma_unattended,
    dp_una_4.9 = (mu_attended_4.9  - mu_attended_0) * lambda_unattended　/ sigma_unattended,
    dp_una_6.1 = (mu_attended_6.1  - mu_attended_0) * lambda_unattended  / sigma_unattended
  ) %>%
  select(sub, starts_with("dp_")) %>%
  pivot_longer(
    cols          = starts_with("dp_"),
    names_to      = c("condition", "contrast"),
    names_pattern = "dp_(att|una)_(.*)",
    values_to     = "dprime"
  ) %>%
  mutate(
    condition = factor(ifelse(condition == "att", "attended", "unattended"),
                       levels = c("attended", "unattended")),
    contrast  = as.numeric(contrast)
  )

# グループ平均・SE
dprime_group <- dprime_df %>%
  group_by(condition, contrast) %>%
  summarise(
    mean_dp = mean(dprime),
    se_dp   = sd(dprime) / sqrt(n()),
    .groups = "drop"
  )

col_condition <- c("attended" = "#e41a1c", "unattended" = "#377eb8")

# --- 図1: attended / unattended を別パネル ---
dprime_plot_sep <- ggplot() +
  geom_line(data  = dprime_df,
            aes(x = contrast, y = dprime, group = sub),
            color = "gray70", linewidth = 0.5, alpha = 0.7) +
  geom_point(data = dprime_df,
             aes(x = contrast, y = dprime),
             color = "gray70", size = 1.5, alpha = 0.7) +
  geom_errorbar(data = dprime_group,
                aes(x = contrast, ymin = mean_dp - se_dp, ymax = mean_dp + se_dp),
                width = 0.15, linewidth = 0.9) +
  geom_line(data  = dprime_group,
            aes(x = contrast, y = mean_dp),
            linewidth = 1.5, color = "black") +
  geom_point(data = dprime_group,
             aes(x = contrast, y = mean_dp),
             size = 4, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~ condition, nrow = 1) +
  scale_x_continuous(breaks = contrast_levels,
                     labels = c("0%", "3.7%", "4.9%", "6.1%")) +
  labs(x = "Contrast level", y = "d'",
       title    = "d' for detect task  (separate panels)",
       subtitle = "Gray = individual, Black = group mean ± SE") +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 14, face = "bold"),
        plot.title       = element_text(face = "bold"))

# --- 図2: attended / unattended を同じパネルに重ねる ---
dprime_plot_overlay <- ggplot() +
  geom_line(data  = dprime_df,
            aes(x = contrast, y = dprime, group = interaction(sub, condition),
                color = condition),
            linewidth = 0.5, alpha = 0.4) +
  geom_point(data = dprime_df,
             aes(x = contrast, y = dprime, color = condition),
             size = 1.5, alpha = 0.4) +
  geom_errorbar(data = dprime_group,
                aes(x = contrast, ymin = mean_dp - se_dp, ymax = mean_dp + se_dp,
                    color = condition),
                width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.2)) +
  geom_line(data  = dprime_group,
            aes(x = contrast, y = mean_dp, color = condition),
            linewidth = 1.5,
            position = position_dodge(width = 0.2)) +
  geom_point(data = dprime_group,
             aes(x = contrast, y = mean_dp, color = condition),
             size = 4,
             position = position_dodge(width = 0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = col_condition, name = "Condition") +
  scale_x_continuous(breaks = contrast_levels,
                     labels = c("0%", "3.7%", "4.9%", "6.1%")) +
  labs(x = "Contrast level", y = "d'",
       title    = "d' for detect task  (overlaid)",
       subtitle = "Thin = individual, Thick = group mean ± SE") +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        plot.title       = element_text(face = "bold"))

plot(dprime_plot_sep)
plot(dprime_plot_overlay)
ggsave(file = "dprime_separate.png",  plot = dprime_plot_sep,
       dpi = 150, width = 10, height = 5)
ggsave(file = "dprime_overlay.png",   plot = dprime_plot_overlay,
       dpi = 150, width = 6,  height = 5)

# --- 図3: 参加者ごとのoverlay ---
dprime_plot_dir <- file.path(plot_dir, "dprime")
dir.create(dprime_plot_dir, showWarnings = FALSE)

for (i in seq_along(participants)) {
  pid <- participants[i]
  
  d_i <- dprime_df %>% filter(sub == i)
  
  p_dp_i <- ggplot(d_i, aes(x = contrast, y = dprime, color = condition)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = col_condition, name = "Condition") +
    scale_x_continuous(breaks = contrast_levels,
                       labels = c("0%", "3.7%", "4.9%", "6.1%")) +
    labs(x = "Contrast level", y = "d'",
         title = paste0("Participant ", pid, "  |  d' (detect task)")) +
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          plot.title       = element_text(face = "bold"))
  
  ggsave(file.path(dprime_plot_dir, sprintf("P%02d_dprime.png", pid)),
         plot = p_dp_i, dpi = 150, width = 5, height = 4)
}
cat("Individual d' plots saved to:", dprime_plot_dir, "\n")
