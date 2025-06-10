library(ggplot2)
library(gganimate)
library(MASS)
library(dplyr)
library(av)

  set.seed(5)
  
  # Generate the datasets
  df_list <- list()
  for (i in 1:9) {
    corr <- i / 10
    sigma <- matrix(c(1, corr, corr,
                      corr, 1, corr,
                      corr, corr, 1), nrow = 3)
    mu <- c(0.5, 0.5, 0.5)
    df <- as.data.frame(mvrnorm(n = 1000, mu = mu, Sigma = sigma))
    colnames(df) <- c("v1", "v2", "v3")
    df$id <- sprintf("df%02d", i)
    df_list[[i]] <- df
  }
  combined_df <- bind_rows(df_list)
  
  combined_df = combined_df |> mutate(v = v1 + v2 + v3)
  
  # Compute summary statistics
  summaries <- combined_df %>%
    group_by(id) %>%
    summarise(
      mean_v = mean(v),
      sd_v = sd(v),
      .groups = "drop"
    ) %>%
    mutate(
      label_mean = paste0("Mean = ", round(mean_v, 2)),
      label_sd1 = paste0("+1 SD = ", round(mean_v + sd_v, 2)),
      label_sd2 = paste0("-1 SD = ", round(mean_v - sd_v, 2))
    )
  
  # Merge back
  combined_df <- left_join(combined_df, summaries, by = "id")
  
  # Plot with lines and numeric labels
  p <- ggplot(combined_df, aes(x = v)) +
    geom_histogram(bins = 30, fill = "navy", color = "navy", alpha = 0.7) +
    geom_vline(aes(xintercept = mean_v), color = "firebrick4", linetype = "dashed", size = 1.5) +
    geom_vline(aes(xintercept = mean_v + sd_v), color = "black", linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = mean_v - sd_v), color = "black", linetype = "dotted", size = 1) +
    geom_text(aes(x = mean_v, y = 100, label = label_mean), angle = 90, vjust = -0.5, hjust = 0, size = 5, color = "firebrick4") +
    geom_text(aes(x = mean_v + sd_v, y = 100, label = label_sd1), angle = 90, vjust = -0.5, hjust = 0, size = 5, color = "black") +
    geom_text(aes(x = mean_v - sd_v, y = 100, label = label_sd2), angle = 90, vjust = -0.5, hjust = 0, size = 5, color = "black") +
    theme_minimal() +
    labs(x = "Ideological Position", y = "Count") +
    transition_states(id, wrap = FALSE) +
    ggtitle("Now showing {closest_state}")
  
  # Animate
  animate(p, nframes = 100, fps = 10)
  
  ggplot(combined_df, aes(v1, v2)) + geom_point() + facet_wrap(~id)
  
 p2 <-  ggplot(combined_df, aes(v1, v2)) +
    geom_point(fill = "grey30", color = "grey30", size = 2) +
    theme_minimal() + theme(text = element_text(size = 11)) +
    labs(x = "Issue 1", y = "Issue 2") +
    transition_states(id, transition_length = 2, state_length = 1) +
    ggtitle("{closest_state}")
 
 animate(p2 + enter_fade() + exit_shrink(),  nframes = 100, fps = 10)

 
 
 
 mu <- c(0,0,0,0,0,0,0,0,0,0)
 sigma <- matrix(0.7, nrow = 10, ncol = 10)
 diag(sigma) <- 1
 df2 <- as.data.frame(mvrnorm(n = 1000, mu = mu, Sigma = sigma))

 
 df2$all_same_sign <- apply(df2, 1, function(row) {
   all(row > 0) || all(row < 0)
 }) * 1 
 
 
 var_names <- paste0("V", 1:10)
 var_pairs <- combn(var_names, 2, simplify = FALSE)
 
 df2$id <- 1:nrow(df2)
 df2_long <- pivot_longer(df2, cols = starts_with("V"), names_to = "var", values_to = "value")
 
 # Join long data twice to get all combinations
 df_pairs <- expand.grid(id = 1:nrow(df2), pair = seq_along(var_pairs)) %>%
   mutate(var1 = map_chr(pair, ~ var_pairs[[.x]][1]),
          var2 = map_chr(pair, ~ var_pairs[[.x]][2])) %>%
   left_join(df2_long, by = c("id", "var1" = "var")) %>%
   rename(x = value) %>%
   left_join(df2_long, by = c("id", "var2" = "var")) %>%
   rename(y = value) %>%
   mutate(pair_label = paste0(var1, " vs ", var2))
 
 df_pairs <- df_pairs %>%
   select(-all_same_sign.y) %>%
   rename(all_same_sign = all_same_sign.x) 
 
ggplot(df2, aes(V1, V2)) + geom_point()

ggplot(df_pairs, aes(x = x, y = y)) +
  geom_point(aes(color = factor(all_same_sign)), size = 1.5, alpha = 0.8) +
  scale_color_manual(
    values = c("0" = "gray70", "1" = "firebrick4"),
    name = "All Same Sign",
    labels = c("No", "Yes")
  ) + xlim(-5,5) + ylim(-5,5) +
  labs(title = 'Pair: {closest_state}', x = NULL, y = NULL) +
  transition_states(pair_label, transition_length = 2, state_length = 1) +
  ease_aes('linear') + theme_minimal()






mu2 <- c(0,0,0,0,0,0,0,0,0,0)
sigma2 <- matrix(0.3, nrow = 10, ncol = 10)
diag(sigma2) <- 1
df3 <- as.data.frame(mvrnorm(n = 1000, mu = mu2, Sigma = sigma2))


df3$all_same_sign <- apply(df3, 1, function(row) {
  all(row > 0) || all(row < 0)
}) * 1 


df3$id <- 1:nrow(df3)
df3_long <- pivot_longer(df3, cols = starts_with("V"), names_to = "var", values_to = "value")

# Join long data twice to get all combinations
df_pairs2 <- expand.grid(id = 1:nrow(df3), pair = seq_along(var_pairs)) %>%
  mutate(var1 = map_chr(pair, ~ var_pairs[[.x]][1]),
         var2 = map_chr(pair, ~ var_pairs[[.x]][2])) %>%
  left_join(df3_long, by = c("id", "var1" = "var")) %>%
  rename(x = value) %>%
  left_join(df3_long, by = c("id", "var2" = "var")) %>%
  rename(y = value) %>%
  mutate(pair_label = paste0(var1, " vs ", var2))

df_pairs2 <- df_pairs2 %>%
  select(-all_same_sign.y) %>%
  rename(all_same_sign = all_same_sign.x) 

ggplot(df_pairs2, aes(x = x, y = y)) +
  geom_point(aes(color = factor(all_same_sign)), size = 1.5, alpha = 0.8) +
  scale_color_manual(
    values = c("0" = "gray70", "1" = "firebrick4"),
    name = "All Same Sign",
    labels = c("No", "Yes")
  ) + xlim(-5,5) + ylim(-5,5) +
  labs(title = 'Pair: {closest_state}', x = NULL, y = NULL) +
  transition_states(pair_label, transition_length = 2, state_length = 1) +
  ease_aes('linear') + theme_minimal()

table(df3$all_same_sign)/1000
table(df2$all_same_sign)/1000

?annotate()


library(MASS)
library(tibble)

library(MASS)
library(tibble)
library(dplyr)
library(purrr)

# Reuse the function
simulate_bimodal_fixed_var <- function(target_r = 0.3, n = 1000, tol = 0.01, max_iter = 1000, seed = 123, fixed_sd = TRUE) {
  set.seed(seed)
  
  p <- 0.5
  n1 <- round(n * p)
  n2 <- n - n1
  sigma <- diag(2)
  
  d <- 0.1
  step <- 0.1
  
  for (i in 1:max_iter) {
    mu1 <- c(-d, -d)
    mu2 <- c(d, d)
    
    comp1 <- mvrnorm(n1, mu = mu1, Sigma = sigma)
    comp2 <- mvrnorm(n2, mu = mu2, Sigma = sigma)
    
    df <- rbind(comp1, comp2)
    df <- as_tibble(scale(df, center = TRUE, scale = FALSE))
    
    if (fixed_sd) {
      col_sds <- apply(df, 2, sd)
      df <- as_tibble(scale(df, center = FALSE, scale = col_sds))
    }
    
    names(df) <- c("X1", "X2")
    r <- cor(df$X1, df$X2)
    
    if (abs(r - target_r) < tol) {
      return(df)
    }
    
    if (r < target_r) {
      d <- d + step
    } else {
      d <- d - step
      step <- step / 2
      d <- d + step
    }
  }
  
  stop("Did not converge")
}

# Simulate for correlations 0.1 to 0.9
target_corrs <- seq(0.1, 0.9, by = 0.1)

all_data <- map2_dfr(
  .x = target_corrs,
  .y = paste0("df_0", 1:9),
  .f = function(corr, name) {
    df <- simulate_bimodal_fixed_var(target_r = corr)
    df$id <- name
    return(df)
  }
)

# View structure
glimpse(all_data)

all_data = all_data |> mutate(id = case_when(id == "df_01" ~ "0.1",
                                             id == "df_02" ~ "0.2",
                                             id == "df_03" ~ "0.3",
                                             id == "df_04" ~ "0.4",
                                             id == "df_05" ~ "0.5",
                                             id == "df_06" ~ "0.6",
                                             id == "df_07" ~ "0.7",
                                             id == "df_08" ~ "0.8",
                                             id == "df_09" ~ "0.9"))

ggplot(all_data, aes(X1, X2)) + geom_point(fill = "grey30", color = "grey30", size = 1.5, alpha = 0.8) + xlim(-3, 3) + ylim(-3,3) + theme_classic() +
  labs(title = 'Correlation * approx * {closest_state}', x = NULL, y = NULL) +
  transition_states(id, transition_length = 2, state_length = 1)


ggplot(df_pairs2, aes(x = x, y = y)) +
  geom_point(aes(color = factor(all_same_sign)), size = 1.5, alpha = 0.8) +
  scale_color_manual(
    values = c("0" = "gray70", "1" = "firebrick4"),
    name = "All Same Sign",
    labels = c("No", "Yes")
  ) + xlim(-5,5) + ylim(-5,5) +
  labs(title = 'Pair: {closest_state}', x = NULL, y = NULL) +
  transition_states(pair_label, transition_length = 2, state_length = 1) +
  ease_aes('linear') + theme_minimal()

