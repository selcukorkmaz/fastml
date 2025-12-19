# fastml version 0.7.5

## Breaking changes

* Removed incomplete or unstable survival backends where correct, leakage-safe behavior could not be guaranteed.

## New features

* **Full Survival Analysis Support**: Added training, resampling, prediction, metric computation, and model summarization for time-to-event outcomes.
* **Guarded Survival Resampling**: Introduced a workflow enforcing leakage-safe preprocessing, imputation, and model fitting within each resampling split.
* **Integrated Brier Score (IBS)**: Added IBS and expanded survival metric support with flexible time handling and user-configurable summary outputs.
* **New Survival Engines**: Added support for parametric and semi-parametric models, including Cox, penalized Cox, Royston–Parmar, and flexible parametric survival models.
* **Advanced Resampling Strategies**: Implemented grouped, blocked, rolling, stratified, and unbiased nested cross-validation.
* **Fold-wise Imputation**: Added support for advanced imputation during resampling while preventing outcome leakage.
* **Engine Parameters**: Introduced an `engine_params` argument to allow passing engine-specific options in a consistent way.
* **S3 Methods**: Added explicit S3 method annotations for `fastml` generics.

## Improvements

* Improved robustness of survival predictions, including risk scores, survival probabilities, quantiles, medians, and time estimates.
* Enhanced survival summary outputs with clearer metric alignment and better handling of stratified and time-varying Cox models.
* Improved extraction of predictions and summaries for parametric survival engines.
* Strengthened recipe validation and sandboxing to harden preprocessing isolation and reduce user-induced leakage.
* Improved handling of novel and missing categorical levels during prediction.
* Integrated resampling metadata more tightly into training workflows and summaries.

## Bug fixes

* Fixed multiple issues in survival label validation, prediction post-processing, and metric computation.
* Corrected survival risk and probability calculations for several engines and model types.
* Fixed log-rank calculation for time-varying Cox models.
* Fixed summary formatting when confidence intervals are unavailable.
* Removed inappropriate confusion matrix warnings for non-classification tasks.
* Fixed edge cases leading to `NA` survival predictions and early exits during survival time computation.
* Addressed naming collisions and alignment issues in tuning grids and metric selection.
