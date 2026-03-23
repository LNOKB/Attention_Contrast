library(tidyverse)
library(ggplot2)
library(effectsize)
library(plotly)
library(viridis)

# ============================================================
### Pre-processing
# ============================================================
dat <- read_csv("2026A_exp2_merged.csv")

result_detect = dat %>%
  filter(detectorcomp %in% c(0)) %>%
  #filter(detectorcomp %in% c(0), outframe < 7) %>%
  select(
    validity, contrastCW, contrastCCW, precueCWorCCW, oriCW, oriCCW, 
    outframe, respdetect, relevantContrast, correctDetect, TorFdetect,
    participantNo, blockNo
  ) %>%    
  group_by(participantNo, validity, relevantContrast) %>%                
  summarize(detect_rate = mean(respdetect), .groups = "drop")

result_comp = dat %>%             
  filter(detectorcomp %in% c(1)) %>%
  #filter(detectorcomp %in% c(1), outframe < 7) %>%
  select(
    contrastCW, contrastCCW, precueCWorCCW, oriCW, oriCCW, 
    outframe, respCWorCCW, participantNo, blockNo
  ) %>%    
  group_by(participantNo, precueCWorCCW, contrastCW, contrastCCW) %>%                
  summarize(CCW_choice_rate = mean(respCWorCCW), .groups = "drop")

# fixed parameters
sigma_attended <- 1
mu_attended_0  <- 0

n <- length(unique(result_detect$participantNo))
contrast_to_idx <- c("0" = 1, "3.7" = 2, "4.9" = 3, "6.1" = 4)

# ============================================================
### Likelihood function
# ============================================================
calculate_logL <- function(x, data_detect, data_comp) {
  
  mu_attended_3.7   <- x[1]
  mu_attended_4.9   <- x[2]
  mu_attended_6.1   <- x[3]
  lambda_unattended <- x[4]
  sigma_unattended  <- x[5]
  theta_detect      <- x[6]
  theta_comp        <- x[7]

  mus_attended   <- c(mu_attended_0, mu_attended_3.7, mu_attended_4.9, mu_attended_6.1)
  mus_unattended <- mus_attended * lambda_unattended
  
  # ---------- detect logL ----------
  pred_yes <- numeric(nrow(data_detect))
  for (row in 1:nrow(data_detect)) {
    relevantContrast  <- as.character(data_detect$relevantContrast[row])
    validity <- data_detect$validity[row]
    idx_rel  <- contrast_to_idx[relevantContrast]

    if (validity == 0) {
      # invalid
      mu <- mus_unattended[idx_rel]; sigma <- sigma_unattended
    } else {
      # valid
      mu <- mus_attended[idx_rel]; sigma <- sigma_attended
    }
    
    pred_yes[row] <- pnorm(theta_detect, mean = mu, sd = sigma, lower.tail = FALSE)
  }
  pred_yes <- pmax(pmin(pred_yes, 1 - 1e-10), 1e-10)
  obs_yes   <- data_detect$detect_rate
  logL_detect <- sum(obs_yes * log(pred_yes) + (1 - obs_yes) * log(1 - pred_yes))
  
  # ---------- comp logL ----------
  pred_ccw <- numeric(nrow(data_comp))
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
    pred_ccw[row] <- pnorm(theta_comp, mean = mu_diff, sd = sd_diff, lower.tail = FALSE)
  }
  pred_ccw <- pmax(pmin(pred_ccw, 1 - 1e-10), 1e-10)
  obs_ccw   <- data_comp$CCW_choice_rate
  logL_comp <- sum(obs_ccw * log(pred_ccw) + (1 - obs_ccw) * log(1 - pred_ccw))
  
  # ---------- sum of logL ----------
  global_pred_yes <<- pred_yes 
  global_pred_ccw   <<- pred_ccw
  
  logL_total <- logL_detect + logL_comp
  if (!is.finite(logL_total)) logL_total <- -1e10
  return(-logL_total)
}

# ============================================================
### Fitting function
# ============================================================
fit_mle <- function(data_detect, data_comp, add_constant = TRUE) {
  
  #0と1の処理の仕方これでOK？
  if (add_constant) {
    data_detect$detect_rate <- pmax(pmin(data_detect$detect_rate, 1 - 1e-10), 1e-10)
    data_comp$CCW_choice_rate <- pmax(pmin(data_comp$CCW_choice_rate, 1 - 1e-10), 1e-10)
  }
  
  guess        <- c(1, 2, 3, 1, 1, 2, 0)            
  lower_bounds <- c(0, 0, 0, 0.5, 0.5, 0.5, -3)
  upper_bounds <- c(3.5, 3.5, 3.5, 2.0, 2.0, 5.0, 3)
  
  fit <- suppressWarnings(
    optim(
      par         = guess,
      fn          = calculate_logL,
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
  return(list(est, global_pred_yes, global_pred_ccw))
}

# ============================================================
### Fitting for individual data
# ============================================================
estimates         <- c()
pred_yes_list    <- vector("list", n)
pred_ccw_list    <- vector("list", n)
participants <- sort(unique(result_detect$participantNo))

for (i in seq_along(participants)) {
  pid <- participants[i]
  data_detect <- result_detect[result_detect$participantNo == pid, ]
  data_comp <- result_comp[result_comp$participantNo == pid, ]
  
  fit <- fit_mle(data_detect, data_comp, add_constant = TRUE)
  df  <- fit[[1]]
  df$sub <- i
  estimates <- rbind(estimates, df)
  pred_yes_list[[i]]  <- fit[[2]]
  pred_ccw_list[[i]]  <- fit[[3]]
  cat("Participant", pid, "done\n")
}

# combining prediction and observation
detect_df <- do.call(rbind, lapply(seq_along(participants), function(i) {
  pid       <- participants[i]
  data_detect <- result_detect[result_detect$participantNo == pid, ]
  data_detect$pred_yes <- pred_yes_list[[i]]
  data_detect$participantNo <- pid
  data_detect
}))
comp_df <- do.call(rbind, lapply(seq_along(participants), function(i) {
  pid       <- participants[i]
  data_comp <- result_comp[result_comp$participantNo == pid, ]
  data_comp$pred_CCW <- pred_ccw_list[[i]]
  data_comp$participantNo <- pid
  data_comp
}))

# ============================================================
### t-test for parameters
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
### violin plot for parameters
# ============================================================
data_parameter_plot <- data.frame(
  Parameters = rep(c("λunattended", "σunattended", "θdetect", "θcomp"), each = n),
  Value = c(estimates[, 4], estimates[, 5], estimates[, 6], estimates[, 7])
)

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
### Estimated distribution plot
# ============================================================
plot_sdt_distributions <- function(means, sds, attention_levels, image_types, colors) {
  data_SDT_plot <- data.frame()
  
  for (i in 1:length(attention_levels)) {
    for (j in 1:length(image_types)) {
      x <- seq(means[i, j] - 3 * sds[i, j], means[i, j] + 3 * sds[i, j], length.out = 100)
      y <- dnorm(x, mean = means[i, j], sd = sds[i, j]) 
      
      data_SDT_plot <- rbind(data_SDT_plot, data.frame(
        x = x, y = y,
        Attention = attention_levels[i],
        Contrast = image_types[j],
        color     = colors[j]
      ))
    }
  }
  
  theta_comp_mean <- mean(estimates[, 7])
  
  Distribution <- ggplot(data_SDT_plot, aes(x = x, y = y, color = Contrast)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = colors) +
    labs(x = "Strength of peripheral color signal", y = "Probability density") +
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
image_types <- factor(c("0%", "3.7%", "4.9%", "6.1%"), levels = c("0%", "3.7%", "4.9%", "6.1%"))
colors <- viridis(4, option = "plasma") 

plot_sdt_distributions(means, sds, attention_levels, image_types, colors)

# ============================================================
### Detect plot (group)
# ============================================================

detect_obs_group <- detect_df %>%
  group_by(participantNo, validity, relevantContrast) %>%
  summarise(
    obs  = mean(detect_rate),
    pred = mean(pred_yes),
    .groups = "drop"
  ) %>%
  group_by(validity, relevantContrast) %>%
  summarise(
    mean_obs  = mean(obs)  * 100,
    se_obs    = sd(obs)    * 100 / sqrt(n),
    mean_pred = mean(pred) * 100,
    .groups   = "drop"
  ) %>%
  mutate(
    Attention = factor(ifelse(validity == 0, "unattended", "attended"),
                       levels = c("attended", "unattended")),
    Contrast = factor(paste0(relevantContrast, "%"),
                       levels = c("0%", "3.7%", "4.9%", "6.1%"))
  )

p_detect <- ggplot() +
  # 予測（薄い色）
  geom_line(data = detect_obs_group,
            aes(x = Contrast, y = mean_pred, color = Attention, group = Attention),
            linewidth = 0.8, alpha = 0.4) +
  geom_point(data = detect_obs_group,
             aes(x = Contrast, y = mean_pred, color = Attention),
             size = 2.5, shape = 17, alpha = 0.4) +
  # 観測値（濃い色）
  geom_errorbar(data = detect_obs_group,
                aes(x = Contrast,
                    ymin = mean_obs - se_obs,
                    ymax = mean_obs + se_obs,
                    color = Attention),
                width = 0.15, linewidth = 0.7) +
  geom_line(data = detect_obs_group,
            aes(x = Contrast, y = mean_obs, color = Attention, group = Attention),
            linewidth = 1.2) +
  geom_point(data = detect_obs_group,
             aes(x = Contrast, y = mean_obs, color = Attention),
             size = 3) +
  scale_color_manual(
    values = c("attended"   = "#e41a1c",
               "unattended" = "#377eb8"),
    name = "Attention"
  ) +
  scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
  labs(x = "Contrast", y = "Detection rate (%)",
       title   = "Observed (solid) vs Predicted (transparent)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(size = 12, face = "bold")
  )

plot(p_detect)
ggsave(file = "detect.png", plot = p_detect, dpi = 150, width = 7, height = 5)

# ============================================================
### Comp plot (group)
# ============================================================

comp_obs_group <- comp_df %>%
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
             "unattended(Cued:CW))",
             "attended(Cued:CCW)"),
      levels = c("unattended(Cued:CW)",
                 "attended(Cued:CCW)")
    ),
    cw_label = factor(paste0("CW = ", contrastCW),
                      levels = paste0("CW = ", c(0, 3.7, 4.9, 6.1)))
  )

p_comp <- ggplot(comp_obs_group,
                     aes(x = contrastCCW, color = precue_label, fill = precue_label)) +
  geom_vline(aes(xintercept = contrastCW),
             linetype = "dotted", color = "gray40", linewidth = 0.8) +
  # モデル予測（薄い色）
  geom_line(aes(y = mean_pred),
            linewidth = 0.8, linetype = "solid", alpha = 0.4) +
  geom_point(aes(y = mean_pred),
             size = 2.5, shape = 17, alpha = 0.4) +
  # 観測値（濃い色）+ エラーバー
  geom_errorbar(aes(ymin = mean_obs - se_obs, ymax = mean_obs + se_obs),
                width = 0.15, linewidth = 0.7) +
  geom_line(aes(y = mean_obs), linewidth = 1.2) +
  geom_point(aes(y = mean_obs), size = 3) +
  facet_wrap(~ cw_label, nrow = 1) +
  scale_color_manual(
    values = c("unattended(Cued:CW)"  = "#377eb8",
               "attended(Cued:CCW)"  = "#e41a1c"),
    name   = "Attention"
  ) +
  scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
  labs(
    x       = "contrastCCW",
    y       = "CCW Choice Rate (%)",
    title   = "Comp task: Observed (solid) vs Predicted (transparent)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 10),
    legend.position  = "bottom",
    plot.title       = element_text(size = 12, face = "bold")
  )

plot(p_comp)
ggsave(file = "comp.png", plot = p_comp,
       dpi = 150, width = 14, height = 7)


# ============================================================
### plots (individual)
# ============================================================
plot_dir <- "plots_individual"
dir.create(plot_dir, showWarnings = FALSE)

for (i in seq_along(participants)) {
  pid <- participants[i]
  
  # --- detect ---
  detect_i <- detect_df %>%
    filter(participantNo == pid) %>%
    mutate(
      Attention = factor(ifelse(validity == 0, "unattended", "attended"),
                         levels = c("attended", "unattended")),
      Contrast  = factor(paste0(relevantContrast, "%"),
                         levels = c("0%", "3.7%", "4.9%", "6.1%"))
    )
  
  p_detect_i <- ggplot(detect_i, aes(x = Contrast, color = Attention, group = Attention)) +
    # 予測（薄い色）
    geom_line(aes(y = pred_yes * 100),
              linewidth = 0.8, alpha = 0.4) +
    geom_point(aes(y = pred_yes * 100),
               size = 2.5, shape = 17, alpha = 0.4) +
    # 観測値（濃い色）
    geom_line(aes(y = detect_rate * 100),
              linewidth = 1.2) +
    geom_point(aes(y = detect_rate * 100),
               size = 3) +
    scale_color_manual(
      values = c("attended"   = "#e41a1c",
                 "unattended" = "#377eb8"),
      name = "Attention"
    ) +
    scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
    labs(x = "Contrast", y = "Detection rate (%)",
         title = paste0("P", pid, "  |  Detect  (solid=obs, transparent=pred)")) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      plot.title       = element_text(size = 12, face = "bold")
    )
  
  # --- comp ---
  comp_i <- comp_df %>%
    filter(participantNo == pid) %>%
    mutate(
      precue_label = factor(
        ifelse(precueCWorCCW == 0,
               "unattended(Cued:CW)",
               "attended(Cued:CCW)"),
        levels = c("unattended(Cued:CW)",
                   "attended(Cued:CCW)")
      ),
      cw_label = factor(paste0("CW = ", contrastCW),
                        levels = paste0("CW = ", c(0, 3.7, 4.9, 6.1))),
      obs_pct  = CCW_choice_rate * 100,
      pred_pct = pred_CCW * 100
    )

  p_comp_i <- ggplot(comp_i, aes(x = contrastCCW, color = precue_label)) +
    geom_vline(aes(xintercept = contrastCW),
               linetype = "dotted", color = "gray40", linewidth = 0.8) +
    geom_line(aes(y = pred_pct), linewidth = 0.8, alpha = 0.4) +
    geom_point(aes(y = pred_pct), size = 2.5, shape = 17, alpha = 0.4) +
    geom_line(aes(y = obs_pct), linewidth = 1.2) +
    geom_point(aes(y = obs_pct), size = 3) +
    facet_wrap(~ cw_label, nrow = 1) +
    scale_color_manual(
      values = c("unattended(Cued:CW)"  = "#377eb8",
                 "attended(Cued:CCW)"  = "#e41a1c"),
      name = "Attention"
    ) +
    scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
    labs(title = paste0("P", pid, "  |  Comp  (solid=obs, transparent=pred)"),
         x = "contrastCCW", y = "CCW Choice Rate (%)") +
    theme_bw(base_size = 11) +
    theme(panel.grid    = element_blank(),
          strip.text    = element_text(size = 9),
          legend.position = "bottom")

  ggsave(file.path(plot_dir, sprintf("P%02d_detect.png", pid)),
         plot = p_detect_i, width = 8,  height = 5, dpi = 150)
  ggsave(file.path(plot_dir, sprintf("P%02d_comp.png",   pid)),
         plot = p_comp_i,   width = 14, height = 7, dpi = 150)
  
  cat("Individual plots saved: P", pid, "\n")
}


# # ============================================================
# ### d' plot for detect task
# # ============================================================
# 
# contrast_levels <- c(0, 3.7, 4.9, 6.1)
# 
# dprime_df <- estimates %>%
#   mutate(
#     # attended: (mu_attended - theta_detect) / sigma_attended
#     dp_att_0   = (mu_attended_0             - mu_attended_0) / sigma_attended,
#     dp_att_3.7 = (mu_attended_3.7           - mu_attended_0) / sigma_attended,
#     dp_att_4.9 = (mu_attended_4.9           - mu_attended_0) / sigma_attended,
#     dp_att_6.1 = (mu_attended_6.1           - mu_attended_0) / sigma_attended,
#     # unattended: (mu_attended * lambda - theta_detect) / sigma_unattended
#     dp_una_0   = (mu_attended_0   * lambda_unattended - mu_attended_0 * lambda_unattended) / sigma_unattended,
#     dp_una_3.7 = (mu_attended_3.7 * lambda_unattended - mu_attended_0 * lambda_unattended) / sigma_unattended,
#     dp_una_4.9 = (mu_attended_4.9 * lambda_unattended - mu_attended_0 * lambda_unattended) / sigma_unattended,
#     dp_una_6.1 = (mu_attended_6.1 * lambda_unattended - mu_attended_0 * lambda_unattended) / sigma_unattended
#   ) %>%
#   select(sub, starts_with("dp_")) %>%
#   pivot_longer(
#     cols          = starts_with("dp_"),
#     names_to      = c("condition", "contrast"),
#     names_pattern = "dp_(att|una)_(.*)",
#     values_to     = "dprime"
#   ) %>%
#   mutate(
#     condition = factor(ifelse(condition == "att", "attended", "unattended"),
#                        levels = c("attended", "unattended")),
#     contrast  = as.numeric(contrast)
#   )
# 
# dprime_group <- dprime_df %>%
#   group_by(condition, contrast) %>%
#   summarise(
#     mean_dp = mean(dprime),
#     se_dp   = sd(dprime) / sqrt(n()),
#     .groups = "drop"
#   )
# 
# col_condition <- c("attended" = "#e41a1c", "unattended" = "#377eb8")
# 
# 
# dprime_plot_overlay <- ggplot() +
#   geom_line(data  = dprime_df,
#             aes(x = contrast, y = dprime, group = interaction(sub, condition),
#                 color = condition),
#             linewidth = 0.5, alpha = 0.4) +
#   geom_point(data = dprime_df,
#              aes(x = contrast, y = dprime, color = condition),
#              size = 1.5, alpha = 0.4) +
#   geom_errorbar(data = dprime_group,
#                 aes(x = contrast, ymin = mean_dp - se_dp, ymax = mean_dp + se_dp,
#                     color = condition),
#                 width = 0.15, linewidth = 0.9,
#                 position = position_dodge(width = 0.2)) +
#   geom_line(data  = dprime_group,
#             aes(x = contrast, y = mean_dp, color = condition),
#             linewidth = 1.5,
#             position = position_dodge(width = 0.2)) +
#   geom_point(data = dprime_group,
#              aes(x = contrast, y = mean_dp, color = condition),
#              size = 4,
#              position = position_dodge(width = 0.2)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
#   scale_color_manual(values = col_condition, name = "Condition") +
#   scale_x_continuous(breaks = contrast_levels,
#                      labels = c("0%", "3.7%", "4.9%", "6.1%")) +
#   labs(x = "Contrast level", y = "d'",
#        title    = "d' for detect task") +
#   theme_bw(base_size = 13) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position  = "bottom",
#         plot.title       = element_text(face = "bold"))
# 
# plot(dprime_plot_overlay)
# ggsave(file = "dprime_overlay.png",   plot = dprime_plot_overlay,
#        dpi = 150, width = 6,  height = 5)
# 
# # --- 図3: 参加者ごとのoverlay ---
# dprime_plot_dir <- file.path(plot_dir, "dprime")
# dir.create(dprime_plot_dir, showWarnings = FALSE)
# 
# for (i in seq_along(participants)) {
#   pid <- participants[i]
#   
#   d_i <- dprime_df %>% filter(sub == i)
#   
#   p_dp_i <- ggplot(d_i, aes(x = contrast, y = dprime, color = condition)) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
#     geom_line(linewidth = 1.2) +
#     geom_point(size = 3) +
#     scale_color_manual(values = col_condition, name = "Condition") +
#     scale_x_continuous(breaks = contrast_levels,
#                        labels = c("0%", "3.7%", "4.9%", "6.1%")) +
#     labs(x = "Contrast level", y = "d'",
#          title = paste0("Participant ", pid, "  |  d' (detect task)")) +
#     theme_bw(base_size = 13) +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position  = "bottom",
#           plot.title       = element_text(face = "bold"))
#   
#   ggsave(file.path(dprime_plot_dir, sprintf("P%02d_dprime.png", pid)),
#          plot = p_dp_i, dpi = 150, width = 5, height = 4)
# }
# cat("Individual d' plots saved to:", dprime_plot_dir, "\n")
# 
# # ============================================================
# ### Criterion C plot for detect task
# # ============================================================
# 
# criterion_df <- estimates %>%
#   mutate(
#     # attended: c = theta - (mu_att_c + 0) / 2
#     c_att_0   = theta_detect - (mu_attended_0             + mu_attended_0) / 2,
#     c_att_3.7 = theta_detect - (mu_attended_3.7           + mu_attended_0) / 2,
#     c_att_4.9 = theta_detect - (mu_attended_4.9           + mu_attended_0) / 2,
#     c_att_6.1 = theta_detect - (mu_attended_6.1           + mu_attended_0) / 2,
#     # unattended: c = theta - (mu_una_c + 0) / 2
#     c_una_0   = theta_detect - (mu_attended_0   * lambda_unattended + mu_attended_0 * lambda_unattended) / 2,
#     c_una_3.7 = theta_detect - (mu_attended_3.7 * lambda_unattended + mu_attended_0 * lambda_unattended) / 2,
#     c_una_4.9 = theta_detect - (mu_attended_4.9 * lambda_unattended + mu_attended_0 * lambda_unattended) / 2,
#     c_una_6.1 = theta_detect - (mu_attended_6.1 * lambda_unattended + mu_attended_0 * lambda_unattended) / 2
#   ) %>%
#   select(sub, starts_with("c_")) %>%
#   pivot_longer(
#     cols          = starts_with("c_"),
#     names_to      = c("condition", "contrast"),
#     names_pattern = "c_(att|una)_(.*)",
#     values_to     = "criterion"
#   ) %>%
#   mutate(
#     condition = factor(ifelse(condition == "att", "attended", "unattended"),
#                        levels = c("attended", "unattended")),
#     contrast  = as.numeric(contrast)
#   )
# 
# criterion_group <- criterion_df %>%
#   group_by(condition, contrast) %>%
#   summarise(
#     mean_c = mean(criterion),
#     se_c   = sd(criterion) / sqrt(n()),
#     .groups = "drop"
#   )
# 
# criterion_plot_overlay <- ggplot() +
#   geom_line(data  = criterion_df,
#             aes(x = contrast, y = criterion, group = interaction(sub, condition),
#                 color = condition),
#             linewidth = 0.5, alpha = 0.4) +
#   geom_point(data = criterion_df,
#              aes(x = contrast, y = criterion, color = condition),
#              size = 1.5, alpha = 0.4) +
#   geom_errorbar(data = criterion_group,
#                 aes(x = contrast, ymin = mean_c - se_c, ymax = mean_c + se_c,
#                     color = condition),
#                 width = 0.15, linewidth = 0.9,
#                 position = position_dodge(width = 0.2)) +
#   geom_line(data  = criterion_group,
#             aes(x = contrast, y = mean_c, color = condition),
#             linewidth = 1.5,
#             position = position_dodge(width = 0.2)) +
#   geom_point(data = criterion_group,
#              aes(x = contrast, y = mean_c, color = condition),
#              size = 4,
#              position = position_dodge(width = 0.2)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
#   scale_color_manual(values = col_condition, name = "Condition") +
#   scale_x_continuous(breaks = contrast_levels,
#                      labels = c("0%", "3.7%", "4.9%", "6.1%")) +
#   labs(x = "Contrast level", y = "C",
#        title    = "C for detect task") +
#   theme_bw(base_size = 13) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position  = "bottom",
#         plot.title       = element_text(face = "bold"))
# 
# plot(criterion_plot_overlay)
# ggsave(file = "criterion_overlay.png",  plot = criterion_plot_overlay,
#        dpi = 150, width = 6,  height = 5)
# 
# # --- 図3: 参加者ごとのoverlay ---
# criterion_plot_dir <- file.path(plot_dir, "criterion")
# dir.create(criterion_plot_dir, showWarnings = FALSE)
# 
# for (i in seq_along(participants)) {
#   pid <- participants[i]
#   
#   c_i <- criterion_df %>% filter(sub == i)
#   
#   p_c_i <- ggplot(c_i, aes(x = contrast, y = criterion, color = condition)) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
#     geom_line(linewidth = 1.2) +
#     geom_point(size = 3) +
#     scale_color_manual(values = col_condition, name = "Condition") +
#     scale_x_continuous(breaks = contrast_levels,
#                        labels = c("0%", "3.7%", "4.9%", "6.1%")) +
#     labs(x = "Contrast level", y = "C",
#          title = paste0("Participant ", pid, "  |  Criterion C (detect task)")) +
#     theme_bw(base_size = 13) +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position  = "bottom",
#           plot.title       = element_text(face = "bold"))
#   
#   ggsave(file.path(criterion_plot_dir, sprintf("P%02d_criterion.png", pid)),
#          plot = p_c_i, dpi = 150, width = 5, height = 4)
# }
# cat("Individual criterion plots saved to:", criterion_plot_dir, "\n")
# 
# library(patchwork)
# 
