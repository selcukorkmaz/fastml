# Lightweight exploratory helper

\`fastexplore()\` is an optional, lightweight exploratory data analysis
(EDA) helper. It returns summary tables and plot objects; it only writes
to disk or renders a report when you explicitly request it via
\`save_results\` or \`render_report\`.

## Usage

``` r
fastexplore(
  data,
  label = NULL,
  visualize = c("histogram", "boxplot", "barplot", "heatmap", "scatterplot"),
  save_results = FALSE,
  render_report = FALSE,
  output_dir = NULL,
  sample_size = NULL,
  interactive = FALSE,
  corr_threshold = 0.9,
  auto_convert_numeric = TRUE,
  visualize_missing = TRUE,
  imputation_suggestions = FALSE,
  report_duplicate_details = TRUE,
  detect_near_duplicates = FALSE,
  auto_convert_dates = FALSE,
  feature_engineering = FALSE,
  outlier_method = c("iqr", "zscore", "dbscan", "lof"),
  run_distribution_checks = TRUE,
  normality_tests = c("shapiro"),
  pairwise_matrix = TRUE,
  max_scatter_cols = 5,
  grouped_plots = TRUE,
  use_upset_missing = TRUE
)
```

## Arguments

- data:

  A \`data.frame\` to explore.

- label:

  Optional column name of the target/label. If supplied and categorical,
  grouped plots and class balance summaries are produced.

- visualize:

  Character vector indicating which plot families to build. Defaults to
  \`c("histogram", "boxplot", "barplot", "heatmap", "scatterplot")\`.

- save_results:

  Logical; if \`TRUE\`, plots/results are saved under \`output_dir\`
  (defaults to the working directory). Default is \`FALSE\`.

- render_report:

  Logical; if \`TRUE\`, a short HTML report is rendered via
  \`rmarkdown\` (if available). Default is \`FALSE\`.

- output_dir:

  Directory to save results/report when \`save_results\` or
  \`render_report\` is \`TRUE\`.

- sample_size:

  Optional integer; if supplied, visualizations are produced on a random
  sample of this size.

- interactive:

  Logical; if \`TRUE\` and \`plotly\` is available, an interactive
  correlation heatmap is produced. Falls back to static ggplot output
  otherwise.

- corr_threshold:

  Absolute correlation threshold for flagging high correlations.

- auto_convert_numeric:

  Logical; convert factor/character columns that look numeric into
  numeric.

- visualize_missing:

  Logical; if \`TRUE\`, include simple missingness visualizations.

- imputation_suggestions:

  Logical; if \`TRUE\`, prints lightweight suggestions based on
  missingness patterns.

- report_duplicate_details:

  Logical; if \`TRUE\`, returns a small sample of duplicated rows when
  present.

- detect_near_duplicates:

  Placeholder for future fuzzy duplicate checks.

- auto_convert_dates:

  Logical; convert YYYY-MM-DD strings to \`Date\`.

- feature_engineering:

  Logical; if \`TRUE\`, derive day/month/year from date columns to aid
  inspection of temporal structure.

- outlier_method:

  One of \`"iqr"\`, \`"zscore"\`, \`"dbscan"\`, \`"lof"\`.

- run_distribution_checks:

  Logical; if \`TRUE\`, run normality tests on numeric columns.

- normality_tests:

  Character vector of normality tests to run; currently supports
  \`"shapiro"\` and \`"ks"\`.

- pairwise_matrix:

  Logical; if \`TRUE\` and \`GGally\` is available, returns a ggpairs
  scatterplot matrix for a subset of numeric columns.

- max_scatter_cols:

  Maximum number of numeric columns to include in the pairwise matrix.

- grouped_plots:

  Logical; if \`TRUE\` and \`label\` is a factor, group
  histograms/boxplots/density plots by label.

- use_upset_missing:

  Logical; retained for compatibility. When \`TRUE\` and \`UpSetR\` is
  installed, an UpSet plot of missingness is returned; otherwise a
  simpler missingness heatmap is used.

## Value

A list of summaries (tables/tibbles) and plot objects (ggplot/plotly),
plus any saved file paths when \`save_results\`/\`render_report\` are
enabled.

## Details

This helper is intentionally decoupled from the core modeling workflow.
Most of its heavy dependencies are treated as optional and loaded via
\`requireNamespace()\` when requested features are used.
