# Core data manipulation and functional programming
library(dplyr)
library(purrr)
library(tidyr)

# Resampling infrastructure
library(rsample)

# Preprocessing and modeling (tidymodels stack)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)

# Guarded resampling framework
library(fastml)

# Visualization
library(ggplot2)
library(patchwork)

# Ensure reproducibility across simulations
set.seed(2025)

# Number of Monte Carlo repetitions
n_sims <- 100

# Number of groups (sites) and observations per group
n_sites <- 10
n_per_site <- 100

# Single simulation run
run_iteration <- function(seed) {

  # Reproducibility at the iteration level
  set.seed(seed)
  print(seed)

  # Total sample size
  total_n <- n_sites * n_per_site

  # Site membership (group structure)
  sites <- rep(paste0("S", 1:n_sites), each = n_per_site)

  # Latent signal shared across sites
  z <- rnorm(total_n, mean = 0, sd = 1)

  # Site-specific offsets (batch effects)
  site_offsets <- rnorm(n_sites, mean = 5, sd = 5)

  # Observed feature contaminated by site effects
  x_observed <- z + rep(site_offsets, each = n_per_site)

  # True outcome-generating mechanism depends only on z
  prob <- 1 / (1 + exp(-(z * 2)))
  y <- factor(
    ifelse(runif(total_n) < prob, "Case", "Control"),
    levels = c("Control", "Case")
  )

  # Assemble dataset
  df <- tibble(
    site = factor(sites),
    outcome = y,
    x = x_observed
  )

  # ------------------------------------------------------------------
  # Leaky workflow:
  # Global site-wise standardization performed BEFORE resampling.
  # This leaks information from assessment folds into training folds.
  # ------------------------------------------------------------------

  df_leaky <- df %>%
    group_by(site) %>%
    mutate(x_scaled = as.numeric(scale(x))) %>%
    ungroup()

  # Grouped cross-validation using site as the grouping variable
  folds_leaky <- group_vfold_cv(df_leaky, group = "site", v = n_sites)

  # Fit and evaluate a model separately within each fold
  auc_leaky <- map_dbl(folds_leaky$splits, function(spl) {

    model <- rand_forest(trees = 50) %>%
      set_mode("classification") %>%
      set_engine("ranger") %>%
      fit(outcome ~ x_scaled, data = analysis(spl))

    predict(model, assessment(spl), type = "prob") %>%
      bind_cols(assessment(spl)) %>%
      roc_auc(
        truth = outcome,
        .pred_Case,
        event_level = "second"
      ) %>%
      pull(.estimate)
  })

  # ------------------------------------------------------------------
  # Guarded workflow:
  # Grouped resampling with fold-specific preprocessing using fastml.
  # Preprocessing is estimated ONLY on analysis folds.
  # ------------------------------------------------------------------

  grouped_folds <- rsample::group_vfold_cv(
    df,
    group = site,
    v = n_sites
  )

  guarded_model <- fastml(
    data = df,
    label = "outcome",
    algorithms = "rand_forest",
    resamples = grouped_folds,
    metric = "roc_auc",
    event_class = "second",
    recipe = recipe(outcome ~ x + site, data = df) %>%
      update_role(site, new_role = "id") %>%   # site used only for grouping
      step_normalize(x),                       # normalization learned per fold
    tune_params = list(
      rand_forest = list(ranger = list(trees = 50))
    )
  )

  # Return paired AUC estimates for this simulation run
  tibble(
    Leaky_AUC = mean(auc_leaky),
    Guarded_AUC = guarded_model$performance$rand_forest$ranger %>%
      filter(.metric == "roc_auc") %>%
      pull(.estimate)
  )
}

# Run the Monte Carlo simulation
simulation_results <- map_dfr(1:n_sims, run_iteration)

# Summary statistics across simulations
summary_stats <- simulation_results %>%
  summarise(
    Mean_Leaky   = mean(Leaky_AUC),
    SD_Leaky     = sd(Leaky_AUC),
    Mean_Guarded = mean(Guarded_AUC),
    SD_Guarded   = sd(Guarded_AUC)
  )

print(summary_stats)

# Paired difference analysis (leaky minus guarded)
diffs <- simulation_results$Leaky_AUC - simulation_results$Guarded_AUC
mean_diff <- mean(diffs)
se_diff <- sd(diffs) / sqrt(n_sims)

# 95% confidence interval for the mean difference
ci <- mean_diff + c(-1, 1) * qt(0.975, df = n_sims - 1) * se_diff

mean_diff
ci

# ------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------

# Distribution of AUCs under leaky vs guarded workflows
p1 <- simulation_results %>%
  pivot_longer(cols = c(Leaky_AUC, Guarded_AUC)) %>%
  mutate(
    name = factor(
      name,
      levels = c("Leaky_AUC", "Guarded_AUC"),
      labels = c("Leaky", "Guarded")
    )
  ) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = c("Leaky" = "#FF9999", "Guarded" = "#99CC99")) +
  labs(
    title = "A",
    y = "ROC AUC",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Paired AUC drop per simulation
p2 <- ggplot(simulation_results) +
  geom_segment(
    aes(
      x    = factor("Leaky",   levels = c("Leaky", "Guarded")),
      xend = factor("Guarded", levels = c("Leaky", "Guarded")),
      y    = Leaky_AUC,
      yend = Guarded_AUC
    ),
    alpha = 0.3
  ) +
  geom_point(
    aes(
      x = factor("Leaky", levels = c("Leaky", "Guarded")),
      y = Leaky_AUC
    ),
    color = "#FF9999"
  ) +
  geom_point(
    aes(
      x = factor("Guarded", levels = c("Leaky", "Guarded")),
      y = Guarded_AUC
    ),
    color = "#99CC99"
  ) +
  labs(
    title = "B",
    y = "ROC AUC",
    x = NULL
  ) +
  theme_minimal()


# Combine plots
combined_plot <- p1 + p2

# Save plot
ggsave(
  filename = "auc_leaky_vs_guarded.png",
  plot = combined_plot,
  width = 10,
  height = 5,
  dpi = 300
)

