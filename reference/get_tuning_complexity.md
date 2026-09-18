# Tuning Complexity Presets

Returns the configuration for a given tuning complexity level, including
grid levels, parameter ranges, and expected computational
characteristics.

## Usage

``` r
get_tuning_complexity(
  complexity = c("balanced", "quick", "thorough", "exhaustive")
)
```

## Arguments

- complexity:

  Character string specifying the tuning complexity level. One of:

  `"quick"`

  :   Minimal tuning for fast iteration. 2-3 levels per parameter,
      narrow ranges. Best for: initial exploration, prototyping, small
      datasets, time-constrained scenarios. Typical grid size: 4-27
      points.

  `"balanced"`

  :   Moderate tuning balancing speed and thoroughness. 3-4 levels per
      parameter, standard ranges. Best for: most production use cases,
      medium datasets. Typical grid size: 27-256 points. This is the
      default.

  `"thorough"`

  :   Comprehensive tuning for maximum model quality. 4-5 levels per
      parameter, wide ranges. Best for: final model selection,
      publications, competitions, when compute time is not a constraint.
      Typical grid size: 256-3125 points.

  `"exhaustive"`

  :   Maximum coverage tuning. 5-7 levels per parameter, very wide
      ranges. Best for: research, benchmarking, when you need to be
      certain you've found the best hyperparameters. Warning: Can be
      very slow. Typical grid size: 1000-10000+ points. Consider using
      Bayesian tuning instead.

## Value

A list with components:

- grid_levels:

  Integer number of levels per parameter for grid search.

- bayes_iterations:

  Integer number of iterations for Bayesian tuning.

- description:

  Human-readable description of the complexity level.

- speed_estimate:

  Relative speed estimate (1 = baseline).

- robustness_estimate:

  Relative robustness estimate (1-5 scale).

## Details

\## Speed-Robustness Trade-offs

Hyperparameter tuning involves a fundamental trade-off between
computational cost and the likelihood of finding optimal
hyperparameters:

|            |               |          |                |                        |
|------------|---------------|----------|----------------|------------------------|
| **Level**  | **Grid Size** | **Time** | **Robustness** | **Use Case**           |
| quick      | 4-27          | ~1x      | Low            | Prototyping, debugging |
| balanced   | 27-256        | ~10x     | Medium         | Most production use    |
| thorough   | 256-3125      | ~100x    | High           | Final models, papers   |
| exhaustive | 1000-10000+   | ~1000x   | Very High      | Research, competitions |

\### Recommendations:

1\. \*\*Start with "quick"\*\* during development to iterate fast 2.
\*\*Use "balanced"\*\* for most production pipelines 3. \*\*Switch to
"thorough"\*\* for final model selection 4. \*\*Consider Bayesian
tuning\*\* (\`tuning_strategy = "bayes"\`) for high-dimensional
parameter spaces instead of exhaustive grid search 5. \*\*Use
adaptive/racing\*\* (\`adaptive = TRUE\`) to early-stop poor
configurations

\### Computational Scaling:

Grid search scales as O(L^P \* F \* N) where: - L = number of levels per
parameter - P = number of parameters being tuned - F = number of
cross-validation folds - N = dataset size

For a model with 5 tunable parameters and 10-fold CV: - quick (L=2): 2^5
\* 10 = 320 model fits - balanced (L=3): 3^5 \* 10 = 2,430 model fits -
thorough (L=5): 5^5 \* 10 = 31,250 model fits

## Examples

``` r
# Get configuration for balanced tuning
config <- get_tuning_complexity("balanced")
print(config$grid_levels)  # 3
#> [1] 3

# See all available presets
print_tuning_presets()
#> 
#> =============================================================================
#>                     fastml Tuning Complexity Presets
#> =============================================================================
#> 
#> SPEED vs ROBUSTNESS TRADE-OFF:
#> -----------------------------------------------------------------------------
#> Preset       Levels   Bayes Iter   Rel. Time    Robustness
#> -----------------------------------------------------------------------------
#> quick        2        5            ~1x          **
#> balanced     3        15           ~10x         ***
#> thorough     5        30           ~100x        ****
#> exhaustive   7        50           ~1000x       *****
#> -----------------------------------------------------------------------------
#> 
#> ESTIMATED GRID SIZES (for 5 tunable parameters):
#>   quick:      2^5 =      32 combinations
#>   balanced:   3^5 =     243 combinations
#>   thorough:   5^5 =   3,125 combinations
#>   exhaustive: 7^5 =  16,807 combinations
#> 
#> RECOMMENDATIONS:
#>   - Start with 'quick' during development for fast iteration
#>   - Use 'balanced' (default) for most production pipelines
#>   - Switch to 'thorough' for final model selection or publications
#>   - Consider tuning_strategy='bayes' instead of 'exhaustive' grid search
#>   - Enable adaptive=TRUE for early stopping of poor configurations
#> 
#> USAGE:
#>   fastml(..., tuning_complexity = 'balanced')     # Use preset
#>   fastml(..., grid_levels = 4)                    # Custom grid levels
#>   fastml(..., tuning_strategy = 'bayes',          # Bayesian tuning
#>               tuning_iterations = 20)
#> 
```
