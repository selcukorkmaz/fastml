---
title: "fastml: Guarded Resampling Workflows for Safe and Automated Machine Learning in R"
date: "2026-03-16"
abstract: >
  Preprocessing leakage arises when scaling, imputation, or other data-dependent transformations are estimated before resampling, inflating apparent performance while remaining hard to detect. We present fastml, an R package that emphasizes leakage-aware execution through guarded resampling, where preprocessing is re-estimated inside each resample and then applied to the corresponding assessment data. The package supports grouped and time-ordered resampling, blocks high-risk configurations such as full-analysis splits, and audits recipes for dependencies on external data or environments. We evaluate fastml with a Monte Carlo simulation contrasting global and fold-local normalization, a usability comparison with tidymodels under matched specifications, and survival benchmarks across datasets of different sizes. Across 100 simulation runs, global preprocessing yielded a mean ROC AUC of 0.809 (SD 0.018) versus 0.651 (SD 0.046) under guarded execution, an average inflation of 0.158 (95% CI 0.149 to 0.167). fastml matched held-out performance obtained with tidymodels while reducing workflow orchestration, and it supported consistent benchmarking of multiple survival model classes through a unified interface. fastml is an execution model designed to make leakage-aware evaluation easier to implement correctly in R.
draft: true
author:
  - name: Selcuk Korkmaz
    affiliation: Trakya University
    address:
      - Faculty of Medicine, Department of Biostatistics, Balkan Campus
      - Edirne 22030, Turkiye
    email: selcukorkmaz@gmail.com
  - name: Dincer Goksuluk
    affiliation: Sakarya University
    address:
      - Faculty of Medicine, Department of Biostatistics, Korucuk Campus
      - Sakarya 54290, Turkiye
    email: dincergoksuluk@sakarya.edu.tr
  - name: Eda Karaismailoglu
    affiliation: University of Health Sciences
    address:
      - Faculty of Medicine, Department of Biostatistics, Gulhane Campus
      - Ankara 06010, Turkiye
    email: eda.karaismailoglu@sbu.edu.tr
type: package
output:
  rjtools::rjournal_article:
    self_contained: yes
    toc: no
bibliography: RJreferences.bib
---


# Introduction
Over the past decade, machine learning workflows in R have evolved from monolithic interfaces, such as \pkg{caret} [@caret_kuhn:2008aa], toward modular ecosystems including \pkg{tidymodels} [@tidymodels_Kuhn:2020aa] and \pkg{mlr3} [@mlr3_Lang:2019aa]. These frameworks decompose modeling into explicit steps for resampling, preprocessing, model specification, and evaluation, providing flexibility and methodological transparency. However, this modularity also shifts responsibility to the user to correctly assemble and coordinate multiple components into a valid pipeline. This shift increases the risk of methodological error by expanding the number of decisions that must be coordinated correctly. Users must ensure that preprocessing, resampling, and model fitting are applied in the proper order and with strict separation between training and evaluation data. In complex workflows, particularly those involving grouped or nested resampling, even experienced practitioners may inadvertently introduce errors that are not detected by standard software checks.

Among these errors, data leakage remains one of the most consequential. In particular, preprocessing leakage occurs when transformations such as scaling, imputation, or feature construction are estimated using information that should be unavailable at evaluation time, for example by learning transformations on the full dataset prior to resampling. This violates training-test isolation and can substantially inflate performance estimates while remaining difficult to detect, because models may still appear well-validated under standard cross-validation [@Hastie:2009_ESL_book]. Recent studies report that leakage and related evaluation flaws are widespread in applied machine learning and contribute to irreproducible and overly optimistic results across multiple scientific domains [@Kapoor:2023aa; @Rosenblatt:2024aa; @Tampu:2022aa]. 

Modern R frameworks provide tools that can prevent preprocessing leakage, most notably by coupling transformations to resampling-aware workflows. However, these safeguards are typically optional rather than enforced. Correct usage requires users to explicitly define recipes or workflows, ensure that preprocessing is trained within each resampling split, and avoid global data-dependent transformations prior to model evaluation. When these conditions are not met, incorrect pipelines can run without warnings while yielding biased estimates. As a result, methodological validity often depends more on user discipline than on guarantees provided by the software.

The \pkg{fastml} package [@Korkmaz:2025_fastml_Rpkg] addresses this gap through a safety-by-design interface that emphasizes leakage-aware execution patterns while retaining access to established modeling engines. A central mechanism is its guarded resampling path, in which preprocessing is re-estimated within each resample on the analysis split and the fold-trained transformations are then applied to the corresponding assessment split. \pkg{fastml} also includes checks intended to surface common configuration errors, including aborting a resampling run when a full-analysis (no-holdout) split is detected and rejecting user recipes that appear to depend on external data sources. When hyperparameter tuning is requested under this guarded path, tuning is performed inside the same resampling loop.

More broadly, \pkg{fastml} provides resampling options and diagnostics that target additional evaluation pitfalls. Grouped resampling can keep related observations together when repeated measures or clustered records exist. For time-ordered settings, blocked or rolling resampling relies on an explicit ordering variable and \pkg{fastml} warns when the ordering required by the chosen design is not provided. An optional audit mode can flag risky patterns in custom preprocessing, such as references to the global environment or code that reads data from files or writes outputs to files, which can create hidden dependencies and undermine reproducibility.

These mechanisms are best understood as leakage-risk mitigation rather than universal guarantees. The guarded resampling path reduces leakage risk under supported configurations, but users can still introduce leakage upstream by supplying preprocessed inputs, externally constructed resamples, or otherwise encoding information before invoking \pkg{fastml}. The contribution of \pkg{fastml} is therefore not a new learning algorithm, but an execution model that makes leakage-aware evaluation easier to carry out correctly in practice by packaging common patterns, defaults, and checks into a single interface. This paper describes the design and implementation of \pkg{fastml}, details how guarded resampling and related checks are realized when those execution paths are used, and demonstrates via simulation how preprocessing leakage can inflate performance estimates and how fold-local workflow fitting mitigates this failure mode under the evaluated configurations.

# Materials and methods
## Guarded resampling {#sec:guarded-resampling}
Preprocessing leakage arises when data-dependent transformation parameters are estimated using information that should be unavailable at evaluation time [@Kapoor:2023aa; @Kaufman:2012aa]. This most commonly occurs when steps such as scaling, imputation, or dimensionality reduction are learned on the full dataset prior to resampling [@Vabalas:2019aa]. Because transformation parameters are estimated using all rows before the data are split into folds, observations that later belong to assessment folds contribute to those parameters, which are then applied within each training fold, breaking training-test separation and tending to yield optimistic performance estimates [@Hastie:2009_ESL_book]. This failure mode does not require access to outcome labels: even unsupervised transformations can leak information when their parameters are estimated globally [@Moscato:2021aa]. The general problem of optimistic evaluation due to leakage from preprocessing and evaluation design has been widely discussed in the applied machine learning literature [@Hastie:2009_ESL_book; @Kaufman:2012aa]

Modern R frameworks provide the capability to avoid preprocessing leakage by embedding transformations within resampling-aware workflows. However, this isolation is typically optional rather than enforced, and leaky pipelines can run without warnings while yielding biased estimates if preprocessing is applied globally before resampling. Figure \@ref(fig:leakage-workflow) illustrates the distinction. In a leaky workflow, transformation parameters are estimated on the full dataset before resampling, so each fold’s assessment data influences the transformation applied to that fold’s training data. In a properly specified workflow, transformation parameters are estimated using only the training portion of each fold and then applied to that fold’s assessment portion.


<div class="figure">
<img src="figures/leaky_cv_workflow_AB.png" alt="Two side-by-side workflow diagrams compare leaky and guarded cross-validation. The leaky workflow applies preprocessing before resampling, while the guarded workflow estimates preprocessing separately inside each fold before evaluation." width="100%" />
<p class="caption">Illustration of a leaky cross-validation workflow caused by global preprocessing.</p>
</div>


When \pkg{fastml} executes its guarded resampling path (i.e., when workflows are fitted through `fastml_guarded_resample_fit`), it organizes preprocessing and model fitting fold-by-fold to reduce preprocessing-leakage risk. This should be understood as risk mitigation rather than a universal guarantee: users can still introduce leakage upstream via preprocessed inputs or externally constructed resamples, and other leakage sources may require an appropriate resampling design (e.g., grouped or blocked schemes).

### The guarded architecture
Under guarded resampling, any data-dependent preprocessing is re-estimated within each resample. For each fold $k$, the procedure is:

- **Generate split $k$**: Construct a resampling split and obtain an analysis (training) set $D_{analysis}^{k}$ and an assessment (hold-out) set $D_{assess}^{k}$.

- **Fit fold-specific preprocessing**: 	Fit a fresh preprocessing specification $R^{(k)}$ using only $D_{analysis}^{(k)}$. This includes estimating any data-dependent transformation parameters (e.g., imputation values, scaling parameters, feature construction rules) from the fold’s analysis data.

- **Apply fold-trained preprocessing**: Apply $R^{(k)}$ to $D_{analysis}^{(k)}$ to produce the preprocessed training features, and apply the same fold-trained preprocessing $R^{(k)}$ to $D_{assess}^{(k)}$ to produce the corresponding preprocessed assessment features. The assessment set is not used when fitting $R^{(k)}$; it is only transformed after $R^{(k)}$ has been learned from the analysis data.

- **Fit and evaluate within the fold**: Train the model on the preprocessed $D_{analysis}^{(k)}$ and evaluate it on the transformed $D_{assess}^{(k)}$, producing performance for fold $k$.


Repeating these steps across folds ensures that, within the resampling process, data-dependent preprocessing parameters are estimated without access to the corresponding assessment data. This reduces preprocessing-leakage risk for workflow-based pipelines, while still requiring users to avoid upstream leakage sources that occur before resampling is invoked.

### Implementation in \pkg{fastml}
In \pkg{fastml}, the guarded resampling behavior is implemented in the resampling engine used when the package executes its guarded resampling path. The core implementation is `fastml_guarded_resample_fit`, which iterates directly over \pkg{rsample} [@Frick:2025_rsample_Rpkg] split objects. For each split $k$, the guarded resampling engine:
- Extracts the analysis (training) and assessment (hold-out) portions of the raw data for that split.
- Fits the user-defined workflow, including preprocessing via \pkg{recipes} [@Kuhn:2025_recipes_Rpkg], on the analysis portion of the split.
- Applies the fitted preprocessing to the assessment portion and evaluates the trained model on that fold’s assessment data.


This structure ensures that, within the guarded resampling loop, recipes are trained on the analysis portion of each split and then applied to the corresponding assessment portion for evaluation. The guard also performs a structural check to detect pathological resampling definitions. In particular, it detects “full-analysis” splits, cases where the analysis indices cover the full dataset and therefore no holdout remains, and stops with an error when such a configuration is encountered. This check is implemented by inspecting resample indices (the `in_id` index sets), rather than by comparing full data objects directly. However, this safeguard does not prevent upstream preprocessing performed before calling `fastml_guarded_resample_fit`, nor does it guarantee that user-provided resamples or inputs are leakage-free.

In addition to the resampling structure, \pkg{fastml} includes a recipe-scanning guard implemented, which is not a full static analyzer and does not attempt to detect every potentially unsafe operation (e.g., arbitrary function calls, or code that reads data from files or writes outputs to files). Instead, it targets a narrower class of high-impact problems: it scans recipe steps for references to external environments or global objects (e.g., `.GlobalEnv`, parent frames, or environment inheritance patterns) and flags steps that appear to embed data objects (for example, steps that inherit from `data.frame`). Recipes flagged as depending on external environments or embedded data objects are rejected before model training begins, with an informative message. This reduces the risk of recipes implicitly depending on global state or carrying fold-external data inside step definitions, but it does not guarantee detection of all possible leakage or reproducibility hazards.

Taken together, \pkg{fastml}’s guarded resampling functionality reduces leakage risk by (i) fitting workflows within each resample when executed through `fastml_guarded_resample_fit`, (ii) detecting full-analysis resampling failures by checking `in_id` indices, and (iii) blocking recipe steps that appear to reference external environments or embed external data objects. These safeguards lower the probability of common leakage mistakes in workflow-based modeling, but they should be interpreted as targeted protections rather than a universal guarantee against all leakage scenarios.
Beyond preprocessing leakage, \pkg{fastml} also supports resampling designs that reduce common evaluation artifacts. Grouped cross-validation keeps related rows together to reduce group-based leakage [@Kuhn:2013_APM], and blocked/rolling resampling modes surface warnings that correct ordering is required to avoid training on future information [@Hyndman:2018_forecasting]. These features reduce risk in common settings, but they do not eliminate the need for leakage-aware dataset construction upstream.

### Resampling design for grouped and time-ordered data

Fold-local preprocessing addresses a specific leakage mechanism: estimating data-dependent transformation parameters using information that should be unavailable at evaluation time. However, valid evaluation can still fail when the resampling design does not match the dependence structure of the data. Two common cases are grouped (clustered) data and time-ordered data.

In grouped or clustered settings, optimistic evaluation can occur even when preprocessing is strictly fold-local. If correlated records from the same entity (e.g., repeated measurements from a patient, multiple samples from the same subject, or clustered observations from the same unit) are split across folds, models can exploit within-entity similarity and appear to generalize better than they truly do [@Roberts:2017aa]. To reduce this risk, \pkg{fastml} supports grouped resampling designs in which all rows sharing a grouping identifier are assigned to the same split, using `group_cols` under the corresponding grouped resampling method. This can mitigate group-based leakage when the grouping variable captures the relevant dependence structure, but it is not a guarantee: leakage may persist if relatedness is only partially represented or if the chosen grouping identifier is incomplete.

In time-ordered settings, leakage can arise when the resampling scheme allows training data to include observations occurring after the assessment period, producing "future-to-past" contamination. This can occur without any explicit preprocessing mistake, purely because the split violates the temporal direction implied by the prediction task [@Bergmeir:2012aa]. \pkg{fastml} supports blocked and rolling resampling modes intended for such settings, which rely on an explicit ordering variable (e.g., `block_col`) and associated block/window parameters. Because correct ordering is a prerequisite for valid time-aware evaluation, \pkg{fastml} warns when the ordering required by the chosen resampling design is not provided. As with grouped designs, these options mitigate a common leakage route under appropriate configuration, but they cannot validate whether the supplied ordering fully captures the information flow in the underlying data-generating process [@Hyndman:2018_forecasting].

These resampling designs therefore complement fold-local preprocessing: guarded resampling primarily targets preprocessing leakage within resampling, while grouped and time-aware resampling options help reduce leakage that arises from dependence structures and temporal ordering.

### Failure-mode demonstration: impact of improper batch correction
A central motivation for guarded resampling is that preprocessing leakage can arise even in workflows that otherwise appear methodologically careful. In multi-site or multi-center studies, a particularly common failure mode is improper batch correction, where site-specific normalization is performed globally prior to resampling. Although often intended to reduce heterogeneity, such preprocessing can incorporate information from held-out groups and inflate apparent performance estimates [@Luo:2010aa].

To demonstrate this failure mode and to illustrate the consequences of global preprocessing under grouped validation, we conducted a Monte Carlo simulation contrasting improper global batch correction with fold-specific preprocessing embedded in a resampling workflow. This simulation is presented as *external experimental evidence in the paper*, not as a built-in demonstration performed automatically by the package. The extent to which fastml’s guarded resampling path is used depends on internal execution logic and the configuration of the modeling call.

**Simulation setting.**

We simulate a binary classification problem with a latent signal shared across sites and a strong site-specific batch effect in the observed predictor. For observation $i$ belonging to site $s(i)$, we generate a latent signal $z_i \sim N(0,1)$, a site offset $b_s \sim N(5,5)$, and an observed predictor $x_i=z_i+b_s(i)$. The outcome depends only on the latent signal, not on site membership: $p_i = logit^{-1}(2z_i)$, and $y_i \sim \text{Bernoulli}(p_i)$. Evaluation uses grouped cross-validation, holding out one entire site per fold (leave-one-site-out), and the simulation is repeated over multiple independent runs to assess stability.

We compare two workflows:
- Leaky workflow (global site-wise standardization before resampling). The predictor is standardized within each site using the full dataset before folds are created. This induces leakage because observations that later appear in assessment folds contribute to the site-specific scaling parameters applied in training folds, partially aligning held-out site distributions with the training data and inflating AUC.
- Guarded workflow (fold-specific preprocessing inside resampling via fastml). Preprocessing is defined in a recipe and is re-estimated on the analysis portion of each fold, then applied to the corresponding assessment portion. The site identifier is assigned an ID role to prevent it from being used as a predictor.



We repeated the simulation for `n_sims` independent runs. In each run, we computed a leaky AUC (site-wise scaling performed globally before grouped CV) and a guarded AUC (fold-local preprocessing inside grouped resampling). We summarize the distribution of AUC values across runs and quantify inflation using the paired difference (Leaky minus Guarded) across simulation runs.


Summary statistics across iterations quantify the inflation under improper batch correction.


``` r
summary_stats <- simulation_results %>%
  summarise(
    Mean_Leaky = mean(Leaky_AUC),
    SD_Leaky = sd(Leaky_AUC),
    Mean_Guarded = mean(Guarded_AUC),
    SD_Guarded = sd(Guarded_AUC)
  )

print(summary_stats)
```
Across 100 Monte Carlo runs, the leaky workflow achieved a mean ROC AUC of 0.809 (SD: 0.018), whereas the guarded workflow achieved a mean ROC AUC of 0.651 (SD: 0.046).


``` r
diffs <- simulation_results$Leaky_AUC - simulation_results$Guarded_AUC
mean_diff <- mean(diffs)
se_diff <- sd(diffs) / sqrt(n_sims)
ci <- mean_diff + c(-1, 1) * qt(0.975, df = n_sims - 1) * se_diff

mean_diff
ci
```
The paired inflation, defined as Leaky AUC - Guarded AUC within each run, was 0.158 on average, with a 95% t-based confidence interval of $[0.149, 0.167]$.

These results indicate a large and consistent upward bias in apparent discrimination when site-wise normalization is performed globally prior to grouped validation. The guarded workflow substantially reduces this bias by re-estimating preprocessing within each resample and applying fold-trained transformations only to the corresponding held-out site.

Figure \@ref(fig:auc-leaky-vs-guarded)A shows the distribution of ROC AUC values across runs for the leaky and guarded workflows. Figure \@ref(fig:auc-leaky-vs-guarded)B shows the paired within-run change in AUC, highlighting the systematic drop in performance when preprocessing is moved inside the resampling loop.

<div class="figure">
<img src="figure/auc-leaky-vs-guarded-1.png" alt="A two-panel figure. Panel A compares the distribution of ROC AUC values for leaky and guarded workflows across simulation runs. Panel B shows paired points and connecting segments, with most guarded values lower than the corresponding leaky values." width="100%" />
<p class="caption">Preprocessing placement under grouped cross-validation: (A) ROC AUC distributions for leaky versus guarded workflows; (B) paired ROC AUC showing inflation with global preprocessing.</p>
</div>

This experiment demonstrates a concrete and practically relevant failure mode: site-wise normalization performed prior to grouped validation can substantially inflate apparent discrimination, even without access to outcome labels. The paired reductions in ROC AUC show that re-estimating preprocessing within each fold is necessary for valid evaluation when group-level structure is present. In fastml, guarded resampling reduces this class of error by fitting preprocessing and models within resamples when the guarded execution path is used; however, this protection applies only within that execution path and can be bypassed by upstream preprocessing or externally supplied, leaky inputs.

## Native survival analysis implementation {#sec:native-survival}
### Survival support: learners, outcomes, and resampling
\pkg{fastml} supports a set of survival learners that require explicit handling of censoring and, for some engines, specialized prediction objects. In the current release, \pkg{fastml} includes Cox family models: Cox proportional hazards (`cox_ph`), penalized Cox regression (`penalized_cox`), stratified Cox regression (`stratified_cox`), and time varying Cox regression (`time_varying_cox`). It also includes parametric survival models: Accelerated Failure Time regression (`survreg`), generic parametric survival (`parametric_surv`), Royston Parmar flexible parametric survival (`royston_parmar`), and a custom piecewise exponential model (`piecewise_exp`). Machine learning survival learners include oblique random survival forest survival (`rand_forest_survival`) and XGBoost accelerated failure time (`xgboost_aft`). These methods are trained through a shared interface but are dispatched to different fitting backends depending on method and configuration.

**Outcome representation.** 
\pkg{fastml} represents survival outcomes internally using a survival response constructed from follow-up time and event status (i.e., a `Surv(time, status)` object) for standard right-censored data [@Therneau:2000_survival_book; @Therneau:2024_survival_Rpkg]. For models that require alternative encodings, \pkg{fastml} constructs the appropriate target representation explicitly. In particular, the `xgboost_aft` backend uses lower and upper interval bounds for each observation (exact bounds for events; one-sided bounds for right-censoring) rather than a single response value, aligning with the AFT objective function [@Barnwal:2020_arXiv]. Basic validation checks ensure that the required outcome fields are present and can be coerced into the expected form, but \pkg{fastml} does not modify censoring patterns (e.g., it does not rebalance events or alter follow-up times).

**Prediction conventions.** 
Survival engines differ in what they naturally return (risk scores, linear predictors, survival curves, time quantiles, or distributional parameters). \pkg{fastml} therefore standardizes predictions only to the extent needed for evaluation: Cox-type learners provide a risk score/linear predictor suitable for concordance-style evaluation [@Harrell:1982aa], parametric learners provide model-based predictions on the scale implied by the fitted distribution, and `xgboost_aft` provides predictions aligned with its AFT objective (typically on the log-time scale, with evaluation defined accordingly). When metrics require time-indexed quantities, evaluation uses explicitly supplied time points rather than assuming that every engine can natively return a full survival curve.

**Fitting under resampling and tuning.** 
\pkg{fastml} uses conditional dispatch to choose between workflow-based fitting (via tidymodels abstractions) and direct engine calls. When resampling and/or hyperparameter tuning is requested and a method is supported through a resampling-aware workflow, \pkg{fastml} fits preprocessing and model training within each split so that fold-local preprocessing is preserved. Methods that are not executed through the workflow-based resampling path are fit using their native interfaces and evaluated under the corresponding train/test or resampling configuration supported by the training pipeline. Practically, this means that “resampling-aware” fitting is method- and configuration-dependent, and survival resampling is more constrained than for classification/regression tasks.

**Explicit non-support and constraints.** 
The survival interface is intentionally scoped. Not all survival data structures and evaluation designs are supported in every backend. For example, the `xgboost_aft` path supports right-censoring through interval bounds but does not accept counting-process (start-stop) outcomes. More generally, some combinations of survival method, resampling design, and tuning configuration are restricted by the training pipeline (and may be rejected with an error) rather than silently coerced. These constraints are treated as part of the package’s design: \pkg{fastml} aims to make supported survival workflows easy to execute correctly, while failing fast when a requested configuration is not implemented or would be ambiguous.

### XGBoost accelerated failure time model with interval bounds
One of the native survival implementations in \pkg{fastml} is the Accelerated Failure Time (AFT) model using XGBoost’s survival objective. This path does not rely on a formula interface. Instead, \pkg{fastml} constructs the necessary inputs explicitly and calls the lower-level XGBoost API to ensure correct handling of censoring. The AFT model assumes a parametric form for log-survival time, modeling
\begin{equation}
  \log (T_i) = \eta_i + \varepsilon_i,
\end{equation}
where $\eta_i$ is a predictor-dependent location parameter and $\varepsilon_i$ follows a specified error distribution (e.g., Normal, Logistic, or Extreme Value) [@Kalbfleisch:2002aa]. To represent censoring correctly, XGBoost requires interval-censored targets rather than single point estimates.

For each observation with observed time $t_i$ and event indicator $\delta_i$, \pkg{fastml} computes
\begin{equation}
  \ell_i = \log(t_i),
\end{equation} 
and defines the target interval as $[\ell_i, u_i)$ where $u_i = \ell_i$ for uncensored observations ($\delta_i = 1$) and $u_i = \infty$ for right-censored observations ($\delta_i = 0$). These lower and upper bounds are supplied to XGBoost by setting `label_lower_bound` and `label_upper_bound` in the `xgb.DMatrix`. By doing so, \pkg{fastml} ensures that right-censored observations contribute a one-sided constraint to the loss function, indicating that the event time exceeds the observed value without assuming an exact failure time. This avoids treating censored times as exact regression targets [@Barnwal:2020_arXiv].

The implementation operates directly on log-time and interval bounds and does not support start-stop (counting process) survival data; such outcomes are explicitly rejected for the XGBoost AFT path. \pkg{fastml}’s role here is to translate standard right-censored survival data into the interval-censored representation required by XGBoost’s AFT objective, without altering the underlying censoring information or relying on inappropriate regression abstractions.

### Custom piecewise exponential distribution
In addition to standard parametric survival models, \pkg{fastml} implements a custom piecewise exponential distribution integrated with the \pkg{flexsurv} framework [@Jackson:2016aa]. The goal is to allow flexible baseline hazard shapes while retaining a parametric likelihood and interpretable parameters.

The piecewise exponential model assumes that the hazard function is constant within predefined time intervals [@Friedman:1982aa]. Let $0 < c_1 < c_2 < \cdots < c_K$ denote user-supplied cutpoints. Within each interval $(c_{k-1}, c_k]$, the hazard is assumed to be
\begin{equation}
  h(t) = \lambda_{k},
\end{equation}
where each $\lambda_k > 0$ is an interval-specific hazard rate. \pkg{fastml} implements this model by defining the hazard, cumulative hazard, density, distribution, and quantile functions algorithmically in a custom distribution family compatible with \pkg{flexsurv}.

User-provided breakpoints are normalized to positive, finite values; \pkg{fastml} does not automatically append an infinite upper bound, nor does it enforce a specific number of intervals. Internally, the custom distribution parameterizes hazards using a baseline log-rate (`log_rate`) and a sequence of log hazard ratios (`log_ratio_1`, `log_ratio_2`, \ldots), which are then converted into interval-specific rates $\lambda_k$. This parameterization is used to ensure positivity and stable estimation while still yielding hazard rates that correspond to piecewise-constant intervals.

The cumulative hazard at time $t$ is computed as the sum of hazards over all completed intervals plus the partial contribution of the interval containing $t$. The survival function is then
\begin{equation}
  S(t) = \exp(-H(t)),
\end{equation}
and the density follows directly from the hazard and survival functions. These expressions correspond to the standard piecewise constant hazard formulation, while the implementation itself evaluates them procedurally.

Model fitting is performed via `flexsurv::flexsurvreg()`, with \pkg{fastml} passing the custom distribution definition and the normalized cutpoints through its internal fitting utilities. This allows the piecewise exponential model to be estimated using maximum likelihood in the same manner as built-in \pkg{flexsurv} distributions, while offering greater flexibility than single-parameter parametric forms such as the exponential or Weibull [@Lawless:2003aa]. Each estimated interval hazard rate retains an interpretation as the event rate within a specified time band, making the model useful when hazard rates are expected to vary across follow-up time.

## Software architecture and security {#sec:software-architecture-and-security}
Beyond statistical validity, an AutoML framework must be robust as software. In practice, this means returning results in a form that is easy to inspect, serialize, and reuse, while also reducing the chance that extensibility mechanisms introduce hidden dependencies or unintended side effects [@Wilson:2014aa]. In \pkg{fastml}, these goals are addressed through (i) a structured S3 object [@Wickham:2019aa] that centralizes fitted models, preprocessing, evaluation outputs, and (optionally) the data partitions used during training and evaluation, and (ii) a set of security and audit utilities that can surface risky patterns in user-supplied recipes and hooks.

The audit and validation utilities are designed to reduce hidden dependencies and common misuse, such as reliance on global environment variables which harms reproducibility [@Peng:2011aa], but they do not provide data-security guarantees; confidentiality depends on what the user chooses to retain and how objects are stored and shared.

### The object model: a self-contained result object
Many R workflows store fitted models, preprocessing artifacts, predictions, and evaluation results as separate objects, or recreate them on demand. \pkg{fastml} instead returns a single S3 object that aggregates the key artifacts produced during training and evaluation. The object is intended to be sufficient for prediction and plotting in a stable/compatible R environment, given the stored fitted model objects and preprocessing specification. However, portability across machines or R installations is not guaranteed for all engines or workflows, because some fitted objects may contain external pointers or compiled components [@Eddelbuettel:2013], serialization behavior can be version-dependent, and prediction may depend on the availability and behavior of upstream packages and recipe steps. Reproducibility also depends on package versions, RNG state, and platform-specific numerical differences [@Stodden:2014aa]. In particular, the \pkg{fastml} object:
- stores fitted model objects, organized by algorithm and engine,
- stores the preprocessing object used for prediction,
- stores evaluation artifacts used for reporting (notably holdout/test predictions and performance summaries),
- and may retain the raw and processed training/test partitions used for fitting and evaluation, enabling inspection and post hoc validation. This improves portability and auditability, but also means the object may carry data and is therefore not "data-free".


The object represents either a resampling-based workflow or a fixed holdout workflow. When resampling is requested and executed, the object records the resampling plan and summary performance outputs associated with that plan. When a single train-test split is used, resampling-related fields are absent or `NULL`, and reported metrics refer to the holdout/test evaluation.





Table: Core components of a `fastml` object and associated guarantees.

|Slot                                          |Description                                                 |Practical guarantee                                                                                                                                                                                                                                                                    |
|:---------------------------------------------|:-----------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|`models`                                      |Fitted model objects, indexed by algorithm and engine       |Models are stored as fitted objects; downstream `predict` and `plot` methods are intended to use stored fits rather than refitting. Reproducibility depends on package versions and environment consistency.                                                                           |
|`preprocessor`                                |Stored preprocessing object                                 |The stored preprocessor corresponds to the final training split (`train_data`), not to per-fold analysis subsets. Fold-local preprocessing occurs inside resampling when that path is used, but the stored final preprocessor is trained on the training split used for the final fit. |
|`predictions`                                 |Stored predictions from evaluation                          |Predictions stored here are the holdout or test predictions produced during evaluation. Training-set predictions and per-fold resampling predictions are not necessarily stored in the same slot by default.                                                                           |
|`performance`                                 |Aggregated performance metrics                              |Metrics are computed from evaluation outputs, typically holdout or test predictions. If resampling is used, summary performance reflects the resampling results recorded by the resampling layer.                                                                                      |
|`metric`                                      |Primary evaluation metric                                   |Defines the main comparison target used in model reporting within the object.                                                                                                                                                                                                          |
|`best_model_name`                             |Identifier of the default reference model                   |A presentation or default-selection label; it should not be interpreted as changing how metrics were computed.                                                                                                                                                                         |
|`resampling_plan`                             |Resampling specification (e.g., CV folds, repeats)          |Encodes how resampling was defined when resampling is executed; used for reporting and consistency.                                                                                                                                                                                    |
|`raw_train_data`, `raw_test_data`             |Data partitions used for fitting/evaluation (when retained) |Enables inspection of the exact splits used. This supports auditability, but the object may include data and therefore is not data-free.                                                                                                                                               |
|`processed_train_data`, `processed_test_data` |Preprocessed partitions (when retained)                     |Supports inspection of preprocessing output and helps detect unexpected preprocessing behavior; this is an audit convenience, not a mathematical guarantee of leakage absence.                                                                                                         |
|`audit`                                       |Audit log entries and a flagged indicator                   |When `audit_mode = TRUE`, the audit structure records security-related warnings or flags. It is not a full training provenance log.                                                                                                                                                    |



Table \@ref(tab:fastml-object-table) summarizes common components of a \pkg{fastml} object and the practical guarantees they provide. This object contract is used throughout the package. Plotting methods are intended to operate on stored predictions and performance summaries rather than recomputing from scratch. Similarly, `predict.fastml` applies the stored preprocessor and stored fitted model(s), yielding consistent predictions within a stable R environment. The object is designed to be serializable (e.g., via `saveRDS`), enabling reuse in downstream workflows.

### Security and audit utilities
AutoML frameworks often support extensibility through custom recipe steps, custom metrics, or user hooks. This extensibility introduces two practical risks: (i) hidden dependencies (for example, reliance on `.GlobalEnv` or objects defined outside the modeling call), which undermines reproducibility and auditability, and (ii) side effects (for example, reading from files, writing outputs to files, or modifying global objects), which can make automated pipelines fragile.

\pkg{fastml} addresses these risks through two distinct mechanisms that serve different purposes: (a) recipe validation, which is preventive but intentionally narrow, and (b) audit-mode instrumentation, which is broader but observational and incomplete. Neither mechanism should be interpreted as a comprehensive security boundary inside R. Users can bypass many checks via qualified namespace calls (e.g., `base::`), alternative I/O functions and connections, `system`, non-local assignment, `getFromNamespace`, or side effects in compiled code.

**Recipe validation (preventive, narrow).** 
\pkg{fastml} includes a recipe-scanning guard (implemented in `R/security_guards.R`) that focuses on a small class of high-impact issues that commonly produce hidden dependencies or fold-external data injection. In particular, it scans recipe steps for patterns suggesting reliance on external environments or global objects (e.g., `.GlobalEnv`, parent-frame access, or related environment inheritance patterns) and flags steps that appear to embed external data objects (for example, steps inheriting from `data.frame`). Recipes flagged as depending on external environments or embedded data objects are rejected before model training begins, with an informative message. This guard is not a full static analyzer and it does not attempt to detect file reads/writes or arbitrary unsafe operations inside recipe code.

**Audit-mode instrumentation (observational, broader but incomplete).** 
When `audit_mode = TRUE`, \pkg{fastml} can run parts of the pipeline under an instrumented environment that records and flags selected risk patterns during execution. This includes (i) flagging common forms of global-environment dependence under the instrumented execution path (for example, by detecting attempted `.GlobalEnv` access and monitoring calls to functions such as `assign`, `rm`, and `get` when they target the global workspace), and (ii) flagging or recording usage of common functions that read from files or write outputs to files (for example, wrappers around frequently used read/write helpers). \pkg{fastml} may also perform lightweight symbol/name scans of function bodies (e.g., via `all.names`) to flag obvious `.GlobalEnv` and I/O-related patterns. These checks are best interpreted as audit signals under the instrumented execution path rather than as proof of safety or guarantees of prevention.

**Integration with the training pipeline.**
Recipe validation is invoked before model training to prevent a narrow class of unsafe recipe definitions. Audit logging is invoked when `audit_mode = TRUE` to emit warnings and record flags. Helper utilities for instrumented execution of arbitrary user hooks may exist, but they are not necessarily used by the default training flow; the audit mechanism should therefore be described as optional and configuration-dependent.

If contrasted with OS-level solutions (e.g., \pkg{RAppArmor} [@Ooms:2013aa]), the appropriate framing is that OS-level tools can enforce stronger process constraints, whereas \pkg{fastml} operates within the R runtime and focuses on targeted prevention (recipe validation) plus optional observability (`audit_mode`) for common reproducibility risks.

### Exploratory diagnostics and architectural isolation
Before initiating automated model training, workflows commonly include an exploratory assessment of variable types, missingness patterns, distributions, and data-quality issues. In fastml, this is supported by an optional diagnostic utility, `fastexplore`, designed to remain separate from the automated resampling and training core.

First, `fastexplore` is not called internally by `fastml` and does not participate in model fitting, recipe estimation inside resampling, or evaluation. Second, its default behavior is low side-effect: it returns structured summaries (tables and plots) without writing to disk. Disk output is explicitly opt-in through arguments such as `save_results` or `render_report`.

The outputs of `fastexplore` are intended to inform modeling configuration decisions (for example, whether to impute, transform, or remove features), but they do not automatically alter the training pipeline. Any actions based on diagnostics must be encoded explicitly by the user in the subsequent modeling call.

## Comparison with existing frameworks
The R ecosystem offers several mature frameworks for machine learning. To contextualize \pkg{fastml}, we compare it against four widely used alternatives: \pkg{caret} [@caret_kuhn:2008aa] (legacy unified interface), \pkg{tidymodels} [@tidymodels_Kuhn:2020aa] (modular workflow system), \pkg{mlr3} [@mlr3_Lang:2019aa] (object-oriented framework with a strong benchmarking culture), and \pkg{h2o} [@Fryda:2024_h2o_Rpkg] (an AutoML platform with an internal training/evaluation engine). The comparison below combines (i) architectural differences relevant to leakage risk, survival modeling, and workflow ergonomics, and (ii) external benchmarks conducted for this paper. Implementation details described as properties of \pkg{fastml} refer to its current codebase; statements about other frameworks reflect their documented behavior and typical usage patterns rather than properties established by \pkg{fastml}’s code.

### Feature comparison matrix
Frameworks differ in where they sit on the flexibility-to-automation spectrum and in how explicitly users must assemble preprocessing, resampling, model fitting, and evaluation. \pkg{caret} provides a largely unified training interface; \pkg{tidymodels} emphasizes explicit composition (recipes + model specification + workflow + rsample); \pkg{mlr3} provides an object-oriented task/learner design with strong support for benchmarking; and \pkg{h2o} provides an integrated AutoML system with its own training and evaluation engine. \pkg{fastml} adopts a single-call façade that orchestrates preprocessing, fitting, and evaluation while exposing a guarded resampling path and diagnostics intended to reduce common workflow errors.

A key comparison dimension is how preprocessing is coupled to evaluation. \pkg{tidymodels} and \pkg{mlr3} are flexible enough to express both leakage-safe and leakage-prone workflows; leakage-resistant evaluation is obtained when preprocessing is estimated on the analysis portion of each resample and then applied to the corresponding assessment data. \pkg{fastml}’s guarded resampling path is designed to re-estimate preprocessing within each resample when that execution mode is used, reducing the risk of preprocessing leakage under supported configurations. This should be interpreted as risk reduction under specific execution paths rather than a universal guarantee, because users can still introduce leakage upstream (e.g., by supplying preprocessed inputs or externally constructed resamples) (Section \@ref(sec:guarded-resampling)).

Survival-model support also differs across ecosystems in both breadth and evaluation tooling. \pkg{caret}, \pkg{tidymodels}, and \pkg{mlr3} offer survival modeling through different abstractions and extension packages (e.g., \pkg{censored} package [@Hvitfeldt:2025_censored_Rpkg]), while \pkg{fastml} exposes a set of survival methods through a unified interface and dispatches between parsnip-based workflows and native engines depending on whether resampling/tuning is requested (Section \@ref(sec:native-survival)). The practical set of methods available to any wrapper framework ultimately depends on engine availability and on which internal execution path is invoked.

Finally, extensibility and "safety" features should be interpreted precisely. \pkg{fastml} includes preventive recipe validation plus optional audit-mode instrumentation aimed at surfacing common sources of hidden dependencies and side effects (Section \@ref(sec:software-architecture-and-security)). These utilities improve auditability under an instrumented execution path, but they do not constitute a security boundary inside R. Table \@ref(tab:framework-comparison-table) highlights differences in design philosophy, safety enforcement, and domain-specific support.


  

Table: Architectural and functional comparison of major R machine learning frameworks.

|Feature                           |\\pkg{caret}                                            |\\pkg{tidymodels}                                                                    |\\pkg{mlr3}                                                       |\\pkg{h2o}                                                    |\\pkg{fastml}                                                                                   |
|:---------------------------------|:-------------------------------------------------------|:------------------------------------------------------------------------------------|:-----------------------------------------------------------------|:-------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|
|Interface philosophy              |Unified wrapper; function-based syntax such as `train`. |Modular pipeline built from recipes, model specifications, workflows, and resamples. |Object-oriented design with tasks, learners, and graphs.          |Integrated AutoML engine.                                     |Guarded facade with a simplified single-function call (`fastml`).                               |
|Preprocessing and leakage control |Manual; users must prevent leakage explicitly.          |Optional and user-assembled; leakage-safe workflows are possible but not enforced.   |Robust graph-based pipelines can express leakage-safe evaluation. |Automated preprocessing and evaluation inside the H2O engine. |Guarded resampling re-estimates preprocessing within folds on supported execution paths.        |
|Survival analysis support         |Basic support, primarily Cox proportional hazards.      |Expanding support, including extensions such as \pkg{censored}.                      |Extensive support via extensions such as \pkg{mlr3proba}.         |Standard survival support including Cox and AFT variants.     |Unified interface spanning penalized Cox, native XGBoost AFT, and piecewise exponential models. |
|Extensibility and safety          |Standard execution inside the R session.                |Standard execution inside the R session.                                             |Standard execution inside the R session.                          |Engine-isolated execution on the Java backend.                |Preventive recipe validation plus optional audit instrumentation.                               |



\emph{Note}: \pkg{mlr3proba} is currently removed from CRAN; archived versions remain available.



# Results
## Benchmarks

### Benchmark A: usability and code efficiency
To quantify differences in user-facing orchestration burden, we implemented a standard clinical classification task using the Pima Indians Diabetes dataset [@Smith:1988aa] (N = 768; 8 predictors; missingness in physiologically related variables) in the \pkg{mlbench} package [@Leisch:2024_mlbench_Rpkg]. This benchmark is not intended to claim that \pkg{fastml} and \pkg{tidymodels} are definitionally equivalent in preprocessing or modeling defaults. Instead, it compares the amount of explicit pipeline construction typically required to obtain a valid holdout evaluation when preprocessing is fitted on the training partition and then applied to the test partition.

**Common setup (data splitting).**
We define a single stratified train-test split and supply the same partitions to both approaches so that evaluation is performed on identical holdout data.


``` r
library(mlbench)
library(rsample)

data("PimaIndiansDiabetes2")

set.seed(123)
split <- initial_split(PimaIndiansDiabetes2, strata = "diabetes")
train_data <- training(split)
test_data  <- testing(split)
```
**\pkg{tidymodels} implementation (explicit pipeline).**
A tidymodels workflow typically requires the user to explicitly define preprocessing, assemble a workflow, fit the model, generate predictions, and compute metrics. The recipe below includes steps that are common in general-purpose templates (for example, novel/unknown/dummy handling), although this particular dataset is entirely numeric. These steps are included to reflect a typical reusable tidymodels template rather than a dataset-specific minimal recipe.


``` r
library(tidymodels)

rec <- recipe(diabetes ` ., data = train_data) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

log_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(log_spec)

final_fit <- fit(wf, data = train_data)

preds <- predict(final_fit, new_data = test_data) %>%
  bind_cols(predict(final_fit, new_data = test_data, type = "prob")) %>%
  bind_cols(test_data)

multi_metric <- metric_set(
  accuracy, kap, f_meas, precision, 
  sens, spec, roc_auc
)

metrics_tm <- multi_metric(
  preds,
  truth = diabetes,
  estimate = .pred_class,
  .pred_pos,
  event_level = "second"
)

print(metrics_tm)
```
**\pkg{fastml} implementation (single-call interface).**
\pkg{fastml} accepts pre-split data and orchestrates preprocessing, model fitting, prediction, and evaluation within a single call. The specific preprocessing applied depends on the arguments provided and package defaults; therefore, numerical parity with the preceding \pkg{tidymodels} example should only be claimed when preprocessing choices are explicitly matched.


``` r
library(fastml)

fm <- fastml(
  train_data    = train_data,
  test_data     = test_data,
  label         = "diabetes",
  algorithms    = "logistic_reg",
  impute_method = "medianImpute",
  event_class   = "second"
)

fm$performance$logistic_reg$glm
```
In this comparison, both implementations yielded numerically equivalent performance metrics on the held-out test set. This demonstrates that \pkg{fastml} preserves the computational integrity of the underlying \pkg{tidymodels} engine when identical model specifications are used.

While \pkg{fastml} significantly reduces code volume, its most important contribution is minimizing the risk of methodological error through architectural constraints. Manual \pkg{tidymodels} workflows place the burden of correctness predominantly on the user, who must explicitly define and sequence numerous independent components, such as imputation, dummy encoding, and zero-variance filtering. In such workflows, every added step creates a new opportunity for omission or data leakage.

In contrast, \pkg{fastml} treats methodological correctness as an architectural requirement rather than a user responsibility. By enforcing a structured pipeline with safe defaults, the framework significantly reduces the cognitive load required to build a valid model. This ensures critical preprocessing steps are applied consistently and minimizes common failure modes, such as leaking test data into training statistics.

### Benchmark B: survival analysis across data scales
To evaluate survival modeling behavior under different data regimes, we conducted an external benchmark comparing classical baselines with fastml-exposed survival engines across datasets of substantially different sizes. This benchmark is intended to illustrate how a single interface can be used to run multiple survival model classes under a consistent evaluation plan. The benchmark is "external" in the sense that all splits, metrics, and summaries are defined by the experimental code in this paper rather than being properties enforced by \pkg{fastml} itself.

**Datasets.** 
We used widely available survival datasets representing distinct scales:
- **Small**: NCCTG Lung Cancer (`lung`, $N = 228$) [@Loprinzi:1994aa]
- **Medium**: Rotterdam Breast Cancer (`rotterdam`, $N = 2,982$) [@Foekens:2000aa]
- **Large**: Serum Free Light Chain (`flchain`, $N = 7,874$) [@Robert:2006aa]


**Evaluation protocol.** 
Performance was estimated using repeated 5-fold cross-validation (3 repeats). Folds were stratified by event status to stabilize the censoring/event mix across splits, a common pragmatic choice in finite-sample survival benchmarking. Reported values summarize the distribution of fold-level concordance scores across all resamples.

**Models compared.** 
We evaluated two classical baselines implemented outside \pkg{fastml} using the same resampling splits and metric definitions: Cox proportional hazards (`survival::coxph`) and Weibull AFT (`survival::survreg`), alongside fastml-exposed survival methods (e.g., penalized Cox, random-forest survival, and XGBoost AFT) using fixed, pre-specified engine settings. To avoid confounding evaluation with model selection, hyperparameter tuning was not performed in this benchmark; any non-default parameter settings were chosen a priori and held constant within each dataset.

**Uncertainty summaries.** 
For each dataset–model combination, we report the mean concordance across resamples with the corresponding standard deviation, and $95%$ confidence intervals computed from the empirical quantiles of resample scores. Because cross-validation fold estimates are not independent, these intervals are descriptive summaries of resampling variability rather than formal inferential confidence intervals [@Bengio:2004aa].

**Results reporting.** 
Table \@ref(tab:external-survival-table) reports mean C-index $\pm$ SD with $95%$ confidence intervals computed over the resamples (5 folds $\times$ 3 repeats = 15 evaluations). All numeric values in such a table are external experimental results from this study’s benchmarking code.



Table: Mean standardized Harrell's C-index +/- SD (95% CI) across data scales from the external benchmark.

|Dataset               |Cox PH (Baseline)                |Weibull AFT (Baseline)           |fastml (Penalized Cox)           |fastml (XGBoost AFT)             |fastml (RF Survival)             |
|:---------------------|:--------------------------------|:--------------------------------|:--------------------------------|:--------------------------------|:--------------------------------|
|Lung (N = 228)        |0.631 +/- 0.064 (0.599 to 0.663) |0.633 +/- 0.062 (0.601 to 0.664) |0.641 +/- 0.066 (0.607 to 0.674) |0.618 +/- 0.050 (0.593 to 0.644) |0.644 +/- 0.061 (0.613 to 0.675) |
|Rotterdam (N = 2,982) |0.690 +/- 0.014 (0.683 to 0.697) |0.690 +/- 0.014 (0.683 to 0.697) |0.695 +/- 0.015 (0.687 to 0.702) |0.705 +/- 0.012 (0.699 to 0.711) |0.711 +/- 0.015 (0.704 to 0.719) |
|Flchain (N = 7,874)   |0.794 +/- 0.013 (0.787 to 0.800) |0.794 +/- 0.013 (0.788 to 0.801) |0.793 +/- 0.012 (0.787 to 0.799) |0.795 +/- 0.014 (0.788 to 0.802) |0.799 +/- 0.013 (0.793 to 0.806) |


Any narrative interpretation (e.g., "nonlinear models outperform linear baselines in medium-scale settings") should be phrased as conclusions from this benchmark under the stated protocol, not as an implication that fastml enforces those outcomes. The main contribution to highlight here is workflow comparability: multiple survival model classes can be evaluated under a single, consistent interface and metric convention, which reduces the cost of comparing methods across regimes, provided the evaluation design (splits, preprocessing placement, and parameter constraints) is defined carefully by the experimenter.

## Case studies
This section presents two applied examples that mirror common biomedical workflows: (i) comparative diagnostic modeling for binary classification, and (ii) resampling-based regression benchmarking under a fixed tuning budget. The goal is to show how fastml coordinates preprocessing, model fitting, and evaluation within a single interface while keeping key experimental choices explicit (data filtering, random seed, resampling plan, and tuning grid).

All reported metrics (e.g., AUC, RMSE) are empirical outputs of the specific code shown and therefore depend on the declared evaluation design (holdout vs resampling), tuning scope, random seed, and software environment (package versions and available engines).

### Case study A: diagnostic benchmarking (breast cancer)
In diagnostic research, it is common to compare a linear baseline with one or more nonlinear learners to evaluate whether additional model flexibility improves discrimination. We illustrate this pattern using the Wisconsin Breast Cancer dataset (`mlbench::BreastCancer`, N = 569) [@Street:1993aa] and three algorithm classes: logistic regression, random forest [@Breiman:2001aa], and gradient boosting [@Chen:2016aa_xgboost]. The primary outcome is the ROC AUC on the evaluation data generated under the specified workflow.

The dataset includes an identifier column (Id), which is not a meaningful predictor and should be excluded to avoid distorted performance estimates. The code below removes Id, ensures the outcome (Class) is observed, and uses complete-case analysis for simplicity. When missingness is substantial or plausibly informative, missing-data handling should be modeled explicitly (e.g., via an imputation recipe) rather than addressed by dropping incomplete rows [@Little:2019aa].


``` r
library(fastml)
library(mlbench)
library(dplyr)

set.seed(2025)

data("BreastCancer")

bc <- BreastCancer %>%
  select(-Id) %>%           # exclude identifier
  filter(!is.na(Class)) %>% # ensure outcome observed
  na.omit()                 # simplify this case study: complete-case analysis

fm_bc <- fastml(
  data          = bc,
  label         = "Class",
  algorithms    = c("logistic_reg", "rand_forest", "xgboost"),
  metric        = "roc_auc",
  event_class   = "second",
  impute_method = "medianImpute"
)

summary(fm_bc, type = "metrics")
plot(fm_bc, type = "roc")
```
The summary output provides a compact comparison of model performance under a common preprocessing and evaluation configuration. The ROC visualization complements the metric table by showing trade-offs in sensitivity and specificity across thresholds.

Model explanation is often desirable in diagnostic applications. \pkg{fastml} provides `fastexplain` for post hoc explanations via external explainability tooling. The availability of specific explanation artifacts (e.g., permutation importance, SHAP-style outputs) depends on installed explainer packages and on model/engine compatibility [@Lundberg:2017aa]; therefore, explanation figures should be reported only when they are produced in the paper’s software environment.


``` r
# Optional: explanation outputs depend on installed explainers 
#           and model compatibility
fastexplain(fm_bc, algorithm = "rand_forest")
```
### Case study B: mixed-type clinical regression (hypertension)
The second example demonstrates resampling-based regression benchmarking in a larger, mixed-type clinical dataset. Using NHANES data (N $\approx$ 10,000 after filtering to observed outcomes) [@Pruim:2025_NHANES_aa], the objective is to predict systolic blood pressure (`BPSysAve`) from demographic, lifestyle, and laboratory variables. We compare three model families: elastic net (regularized linear model) [@Zou:2005aa], random forest (bagging ensemble), and \pkg{LightGBM} (gradient boosting) [@Ke:2017aa]. Hyperparameters are tuned using a predefined grid under 5-fold cross-validation.

This design reflects a practical goal: evaluate whether a linear model is sufficient, or whether nonlinear learners provide measurable gains under a fixed and transparent tuning budget. Engine availability is an external requirement (e.g., \pkg{LightGBM} must be installed and accessible), and performance will vary with the tuning grid; thus, results should be presented as outcomes of the specified benchmarking design rather than as general statements about the algorithms.


``` r
library(fastml)
library(bonsai) 
library(NHANES)
library(dplyr)

set.seed(2025)

data("NHANES")

df <- NHANES %>%
  select(
    BPSysAve,
    Age, Gender, BMI, Race1, Education, Poverty,
    SleepHrsNight, PhysActive, Smoke100, TotChol, Diabetes
  ) %>%
  filter(!is.na(BPSysAve))

fm_bp <- fastml(
  data              = df,
  label             = "BPSysAve",
  algorithms        = c("elastic_net", "rand_forest", "lightgbm"),
  metric            = "rmse",
  impute_method     = "medianImpute",
  resampling_method = "cv",
  folds             = 5,
  tune_params       = list(
    elastic_net = list(
      glmnet = list(
        penalty = c(0.01, 0.1),
        mixture = c(0.0, 0.5, 1.0)  # ridge, elastic net, lasso
      )
    ),
    rand_forest = list(
      ranger = list(
        mtry  = c(2, 3, 5),
        min_n = c(20, 50),
        trees = c(100)
      )
    ),
    lightgbm = list(
      lightgbm = list(
        num_leaves    = c(20, 31),
        learning_rate = c(0.05, 0.1)
      )
    )
  )
)

summary(fm_bp)
```
The resulting summary can be used to populate a regression comparison table, reporting RMSE and any additional metrics computed under the same resampling and tuning design. Any conclusions about which model "wins" should be tied to the declared tuning grid and evaluation plan: in particular, boosting methods can be sensitive to the search space, and a small grid may not reflect their best attainable performance.

These case studies demonstrate two applied patterns that recur in biomedical modeling: (i) comparative benchmarking across model families for diagnostic classification, and (ii) resampling-based regression benchmarking with explicit tuning budgets. The examples are intentionally written so that preprocessing decisions (e.g., exclusion of identifier fields), evaluation design (seed, resampling plan), and tuning scope are visible and reportable.


# Conclusion
The R ecosystem provides increasingly capable machine-learning tooling, but correct evaluation still depends on how users structure preprocessing, resampling, and model selection. A common failure mode is leakage introduced by global data-dependent operations (e.g., imputation, scaling, feature construction) performed before resampling. Such issues can bias performance estimates even when outcome labels are never explicitly used [@Kapoor:2023aa].

\pkg{fastml} is motivated by reducing the practical risk of these errors through a constrained interface and guarded evaluation paths. When \pkg{fastml} executes its guarded resampling path, workflows are fitted within resamples so that data-dependent preprocessing is estimated on analysis splits and then applied to the corresponding assessment splits. This design can substantially reduce leakage risk in typical cross-validation setups, but it is not a universal guarantee: users can still bypass fold-local estimation by supplying preprocessed inputs, by constructing resamples upstream, or by otherwise encoding leakage before calling the package.

\pkg{fastml} also extends the same single-call workflow style to survival modeling. In addition to dispatching standard survival learners through established engines, \pkg{fastml} includes native implementations for selected survival methods (e.g., XGBoost AFT with interval bounds and a piecewise-exponential model implemented through flexsurv utilities). This enables side-by-side benchmarking of multiple survival model classes under one interface, while keeping the evaluation design (holdout vs. resampling) explicit in the analysis.

With respect to operational safety, \pkg{fastml} includes audit utilities and sandbox helpers intended to surface risky patterns in user-defined functions and recipe steps. In the current implementation, recipe validation and audit logging can be invoked by `fastml` when `audit_mode = TRUE`, and the audit layer can flag `.GlobalEnv`-style dependencies and I/O-related symbol usage. The more general sandboxed-execution helpers exist but are not invoked by the default training path, and the audit layer is better characterized as warning/logging infrastructure rather than a hard enforcement mechanism that blocks all unsafe operations.

The empirical results presented in this paper (performance comparisons, model rankings, and usability examples) should be interpreted as outcomes of the experimental designs we executed, defined by specific datasets, preprocessing choices, tuning grids, random seeds, and resampling plans, rather than as properties guaranteed by the implementation. Likewise, any claims about reduced boilerplate relative to alternative frameworks should be treated as qualitative observations or externally measured comparisons, not as invariant guarantees.

Future work will focus on expanding the set of supported workflows while preserving evaluation clarity. Two concrete directions are (i) model combination strategies (e.g., stacking/super learning) implemented in a way that keeps resampling and leakage considerations explicit [@vanDerLaan:2007aa], and (ii) improved deployment ergonomics (e.g., generating standardized prediction wrappers and deployment-ready artifacts). These are planned extensions rather than current capabilities and will require careful design to maintain the package’s emphasis on transparent evaluation and auditable outputs.

