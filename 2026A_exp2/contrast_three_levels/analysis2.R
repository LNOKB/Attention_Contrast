library(tidyverse)
library(ggplot2)
library(effectsize)
library(plotly)
library(viridis)

dat <- read_csv("2026A_exp2_merged.csv")
n <- 17
#detect
result = dat %>%             
  filter(detectorcomp %in% c(0)) %>%
  select(
    validity, detectorcomp, contrastCW, contrastCCW, precueCWorCCW, oriCW, oriCCW, 
    outframe, probedcontrast, respdetect,	relevantContrast,	correctDetect, TorFdetect,
    respCWorCCW, key_resp.keys, participantNo, blockNo
  ) %>%    
  group_by(participantNo, validity, relevantContrast) %>%                
  summarize(detect_rate = mean(respdetect))     

# fixed parameters
sigma_attended <- 1
mu_attended_0 <- 0

### Fitting function
fit_PFCI_mle <- function(data, add_constant = TRUE) {
  if (add_constant) {
    data <- data + 0.5
  }
  
  # setting initial values
  guess <- c(
    1,  # mu_attended_3.7
    2,  # mu_attended_4.9
    3,  # mu_attended_6.1
    1, 1, 2         # lambda_unattended, sigma_unattended, theta 
  )
  
  # fitting specifications
  lower_bounds <- c(0, 0, 0, 0.5, 0.5, 0.5)
  upper_bounds <- c(3.5, 3.5, 3.5, 2.0, 2.0, 5.0)
  control_params <- list(
    "maxit" = 10000,
    "parscale" = c(1, 1, 1, 0.001, 0.001, 1)
  )
  
  # fitting
  fit <- suppressWarnings(
    optim(
      PFCI_logL,
      par = guess,
      lower = lower_bounds,
      upper = upper_bounds,
      gr = NULL,
      method = "BFGS",
      control = control_params
    )
  )
  
  # outputs
  mu_attended_3.7  <- fit$par[1]
  mu_attended_4.9 <- fit$par[2] 
  mu_attended_6.1 <- fit$par[3] 
  lambda_unattended <- fit$par[4] 
  sigma_unattended <- fit$par[5] 
  theta <- fit$par[6] 
  logL <- fit$value
  
  est <- data.frame(mu_attended_3.7   = mu_attended_3.7, 
                    mu_attended_4.9  = mu_attended_4.9, 
                    mu_attended_6.1  = mu_attended_6.1, 
                    lambda_unattended = lambda_unattended,
                    sigma_unattended = sigma_unattended,
                    theta = theta,
                    logL = logL)
  return(list(est, global_predicted_data))
}


### Likelihood function
PFCI_logL <- function(x, inputs) {
  
  # target parameters
  mu_attended_3.7  <- x[1]
  mu_attended_4.9 <- x[2] 
  mu_attended_6.1 <- x[3] 
  lambda_unattended <- x[4] 
  sigma_unattended <- x[5] 
  theta <- x[6] 
  
  # model predictions
  mus_attended <- c(mu_attended_0, mu_attended_3.7, mu_attended_4.9, mu_attended_6.1)
  mus_unattended <- mus_attended * lambda_unattended
  predicted_data <- matrix(NA, nrow = 8, ncol = 2)
  
  #unattended
  for (cond in 1:4) {
    predicted_data[cond, 1] <- pnorm(theta, mean = mus_unattended[cond], sd = sigma_unattended) # pred_yes_rate
    predicted_data[cond, 2] <- 1 - predicted_data[cond, 1] # pred_no_rate
  }
  
  #attended
  for (cond in 5:8) {
    predicted_data[cond, 1] <- pnorm(theta, mean = mus_attended[cond-4], sd = sigma_attended) # pred_yes_rate
    predicted_data[cond, 2] <- 1 - predicted_data[cond, 1] # pred_no_rate
  }
  
  global_predicted_data <<- predicted_data 
  # log likelihood
  logL <- sum(data * log(predicted_data))
  if (is.nan(logL)) {
    logL <- -Inf
  }
  logL <- -logL
  return(logL)
  
}

### Conducting fitting on individual data
estimates <- c()
predicted_array <- array(NA, dim = c(8, 2, n))
data_rate_array <- array(NA, dim = c(8, 2, n))

#for (i in 4:47) {
for (i in 1:17) {
  
  subdata <- result[((i - 1)* 8 + 1):((i - 1)* 8 + 8), ]
  yes_rate <- subdata[[4]]
  data <- cbind(yes_rate, 1 - yes_rate)
  data_rate_array[, , i] <-  data
   
  # fitting
  fit <- fit_PFCI_mle(data, add_constant = TRUE)
  df <- fit[[1]]
  df$sub <- i
  estimates <- rbind(estimates, df)
  predicted_array[, , i] <-  fit[[2]]
}
#######################################
### Preparation for data visualization
mean_predicted <- apply(predicted_array, c(1, 2), mean)*100
mean_data <-  apply(data_rate_array, c(1, 2), mean)*100
se_data <- (apply(data_rate_array, c(1, 2), sd)*100)/sqrt(n)
# index_order <- c(
#   1, 4:8,   19, 
#   2, 9:13,  20, 
#   3, 14:18, 21
# )
mean_predicted2 <- mean_predicted[, 1]
mean_data2 <- mean_data[, 1]
se_data2 <- se_data[, 1]


### t-test, lambda
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

# violin plot
data_parameter_plot <- data.frame(
  Parameters = rep(c("λunattended", "σunattended") , each = n),
  Value = c(estimates[, 4], estimates[, 5])
)
parameters_graph <- ggplot(data_parameter_plot, aes(x = Parameters, y = Value)) +
  geom_violin(fill = "skyblue", color = "black", scale = "width") +  
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
  labs(y = "Value") + 
  #scale_y_continuous(breaks = seq(0.5, 1.5, length = 5), limits = c(0.5, 1.5)) +
  scale_x_discrete("Parameters", labels = c(expression("λ"[unattended]), expression("σ"[unattended]))) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 6, color = "black") +
  theme_classic() +  
  theme(
    plot.title =   element_text(size = 20 * 2),    
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 14 * 2),  
    axis.text.x =  element_text(size = 11 * 2),  
    axis.text.y =  element_text(size = 11 * 2)    
  )
plot(parameters_graph)
ggsave(file = "parameters_graph.png", plot = parameters_graph, dpi = 150, width = 8, height = 6)


### Estimated distribution ここでミスってる
plot_sdt_distributions <- function(means, sds, attention_levels, image_types, colors) {
  data_SDT_plot <- data.frame()
  
  for (i in 1:length(attention_levels)) {
    for (j in 1:length(image_types)) {
      x <- seq(means[i, j] - 3 * sds[i, j], means[i, j] + 3 * sds[i, j], length.out = 100) #これなんだ？
      y <- dnorm(x, mean = means[i, j], sd = sds[i, j]) 
      
      data_SDT_plot <- rbind(data_SDT_plot, data.frame(
        x = x,
        y = y,
        Attention = attention_levels[i],
        ImageType = image_types[j],
        color = colors[j]
      ))
    }
  }
  
  Distribution <- ggplot(data_SDT_plot, aes(x = x, y = y, color = ImageType)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = colors) +
    labs(x = "Strength of peripheral color signal",
         y = "Probability density") +
    scale_x_continuous(limits = c(-3, 6)) +
    scale_y_continuous(breaks = seq(0, 0.6, length = 4),limits = c(0, 0.6)) +
    geom_vline(xintercept = mean(estimates[, 6]), linetype = "dashed", color = "black") + 
    facet_wrap(~ Attention, nrow = 2, scales = "free_y") +  
    theme_minimal(base_size = 18) +
    theme(
      panel.grid.major = element_blank(),   
      panel.grid.minor = element_blank(),   
      axis.line = element_line(size = 0.5, color = "black"), 
      axis.ticks = element_line(color = "black"),  
      axis.ticks.length = unit(0.3, "cm"),  
      legend.position =  c(0.1, 5),
    )
  
  plot(Distribution)
  ggsave(file = "Distribution.png", plot = Distribution, dpi = 150, width = 8, height = 6)
  
}

means_attended <- c(
  mu_attended_0, mean(estimates[, 1]), mean(estimates[, 2]), mean(estimates[, 3]))  
means_unattended <- means_attended * mean(estimates[, 4])
means <- rbind(means_attended, means_unattended)
sigma_attended <-  c(rep(sigma_attended, 4))  
sigma_unattended <- c(rep(mean(estimates[, 5]), 4))  
sds <- rbind(sigma_attended, sigma_unattended)

attention_levels <- factor(c("attended", "unattended"), levels = c("attended", "unattended"))
image_types <- factor(
  c("0%", "3.7%", "4.9%", "6.1%"),
  levels = c("0%", "3.7%", "4.9%", "6.1%"))
colors <- viridis(4, option = "plasma") 

plot_sdt_distributions(means, sds, attention_levels, image_types, colors)


### Model prediction
data_bar <- data.frame(
  Imagetype = factor(rep(c("0%", "3.7%", "4.9%", "6.1%"), 2),
                     levels = c("0%", "3.7%", "4.9%", "6.1%")),
  Proportion =  as.vector(t(mean_data2)),
  SE =  as.vector(t(se_data2)),
  Condition = factor(c(rep("attended", 4), rep("unattended", 4)), 
                     levels = c("attended", "unattended"))
)

predicted_data_bar <- data.frame(
  Imagetype = factor(rep(c("0%", "3.7%", "4.9%", "6.1%"), 2),
                     levels = c("0%", "3.7%", "4.9%", "6.1%")),
  Predicted = as.vector(t(mean_predicted2)),  
  Condition = factor(c(rep("attended", 4), rep("unattended", 4)), 
                     levels = c("attended", "unattended"))
)

bar_graph <- ggplot(data_bar, aes(x = Imagetype, y = Proportion)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = "Detection rate (%)") +
  facet_grid(. ~ Condition) +  
  geom_point(data = predicted_data_bar, aes(x = Imagetype, y = Predicted), 
             color = "red", size = 3) +  
  geom_errorbar(data = data_bar, aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2) + 
  theme(legend.position = "right", text = element_text(family = "Arial")) +  
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    plot.title =   element_text(size = 20 * 2),   
    axis.title.x = element_text(size = 14 * 2),  
    axis.title.y = element_text(size = 14 * 2),  
    axis.text.x =  element_text(size = 24, angle = 45, hjust = 1, color = "black"),   
    axis.text.y =  element_text(size = 11 * 2),    
    legend.position = "right",
    strip.text =   element_text(size = 36),  
    legend.text =  element_text(size = 18),  
    legend.title = element_text(size = 18)  # 
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),    
    limits = c(0, 100)                  
  ) 

plot(bar_graph)
ggsave(file = "bar_graph.png", plot = bar_graph, dpi = 150, width = 14, height = 8)
