---
title: 'fastml: An R package for leakage-aware machine learning workflows with fold-local preprocessing'
tags:
  - R
  - machine learning
  - data leakage
  - cross-validation
  - reproducibility
  - survival analysis
authors:
  - name: Selcuk Korkmaz
    orcid: 0000-0003-4632-6850
    corresponding: true
    affiliation: 1
  - name: Dincer Goksuluk
    orcid: 0000-0002-2752-7668
    affiliation: 2
  - name: Eda Karaismailoglu
    orcid: 0000-0003-3085-7809
    affiliation: 3
affiliations:
  - name: Department of Biostatistics, Trakya University, Edirne, Turkey
    index: 1
    ror: 00xa0xn82
  - name: Department of Biostatistics, Sakarya University, Sakarya, Turkey
    index: 2
    ror: 04ttnw109
  - name: Department of Medical Informatics, University of Health Sciences, Istanbul, Turkey
    index: 3
    ror: 03k7bde87
date: 18 September 2026
bibliography: paper.bib
---

# Summary

A predictive model is only as trustworthy as the procedure used to evaluate it.
*Preprocessing leakage* occurs when a data-dependent transformation — scaling,
imputation, class rebalancing, feature construction — is estimated on the whole
dataset before resampling, so that each fold's held-out observations have
already influenced the data the model trains on. The resulting performance
estimate is optimistic, and the effect appears even for transformations that
never touch the outcome labels [@moscovich2022preprocessing; @kaufman2012leakage].
Leakage of this kind has been found across seventeen scientific fields, affecting
294 papers [@kapoor2023leakage], and has been shown to inflate published results
in specific domains [@rosenblatt2024leakage].

`fastml` is an R package that trains, tunes and compares machine-learning models
through a single call, and in which *fold-local* preprocessing is the default
execution path rather than a convention the user must maintain. Every
data-dependent transformation is re-estimated inside each resampling split from
that split's analysis data alone, then applied to the corresponding assessment
data. The package covers classification, regression and survival tasks, offers
resampling designs that respect grouping and time order as well as ordinary
cross-validation, and keeps the evaluation design explicit and under user
control.

Its contribution is ergonomic rather than methodological. Leakage-aware
workflows can be built in `tidymodels` and `mlr3`; what `fastml` adds is an
interface in which the unsafe arrangement is not expressible.

# Statement of need

Modular machine-learning frameworks decompose modelling into resampling,
preprocessing, model specification and evaluation. That modularity is a virtue,
but it moves the burden of correct composition onto the user, who must apply the
steps in the right order and keep training and assessment data strictly
separated. In grouped, nested or time-ordered designs, experienced practitioners
still make mistakes that no software check catches, because a leaky pipeline is
a valid pipeline: it runs, it reports a number, and nothing warns.

The audience is applied researchers who need a defensible performance estimate
rather than a maximally flexible pipeline — the case where the cost of a silent
error is high and the value of arbitrary composition is low. For them the
relevant question is not whether a framework *can* express a safe workflow, but
whether it can express an unsafe one by accident.

`fastml` answers that by narrowing the interface. Its preprocessing arguments
compile to untrained specifications that are estimated only inside the
resampling loop, and the one channel that could smuggle in externally estimated
parameters — a recipe already passed through `prep()` — is rejected before
training begins. The trade is deliberate and is the substance of the design: a
vocabulary that cannot express the unsafe composition also cannot express
arbitrary pipeline topologies, tuning of preprocessing parameters, or stacking.
Two protections lie outside it, and the package says so rather than implying
otherwise: preprocessing performed before the call is beyond its reach, and a
custom step inside a user-supplied recipe can still read values computed
elsewhere.

# State of the field

Four frameworks anchor the comparison. `caret` [@kuhn2008caret] offers a legacy
unified training interface. `tidymodels` [@kuhn2020tidymodels] emphasises
explicit composition of recipes, model specifications, workflows and resampling.
`mlr3` [@lang2019mlr3] provides an object-oriented task/learner design with a
strong benchmarking culture, and `h2o` [@fryda2020h2o] supplies an integrated
AutoML system with its own training engine.

The dimension that matters here is how preprocessing is coupled to evaluation,
and on it the honest comparison is with `mlr3pipelines` [@binder2021mlr3pipelines]
rather than with the looser alternatives. Its `GraphLearner` composes
preprocessing and a learner into one object that is re-fitted on the training
part of every fold, so fold-local preprocessing is already a structural property
there and no separate guard is needed. `tidymodels` and `mlr3` can likewise
express leakage-safe workflows — and leakage-prone ones, with equal ease and no
diagnostic.

`fastml` was therefore not built because safe composition was impossible
elsewhere. It was built because the safe arrangement was optional everywhere,
and because the population that most needs it is least likely to assemble it
correctly. The difference is the default and the refusal: `fastml` accepts a
narrow vocabulary in exchange for the guarantee that nothing inside it places
preprocessing outside the loop, and refuses designs it cannot execute correctly
instead of approximating them. For survival modelling specifically, specialised
frameworks such as `mlr3proba` [@sonabend2021mlr3proba] provide comparable or
broader capability; `fastml` offers a unified interface across heterogeneous
survival learners rather than a deeper one.

# Software design

Under the guarded path, each fold $k$ proceeds in four stages: construct the
split; fit a fresh preprocessing specification using the analysis data
$D^{(k)}_{\text{analysis}}$ alone; apply that fold-trained specification to both
$D^{(k)}_{\text{analysis}}$ and $D^{(k)}_{\text{assess}}$; then fit and evaluate
within the fold. The assessment set is never used to estimate the
transformation, only to be transformed by it. Tuning happens inside the same
loop, so hyperparameter selection inherits the separation rather than
undermining it.

A second design decision concerns designs the software should refuse. Fold-local
preprocessing addresses one leakage mechanism, but evaluation still fails when
the resampling design ignores the dependence structure of the data: splitting
correlated records from one entity across folds [@roberts2017crossvalidation],
or letting training data postdate the assessment period
[@bergmeir2012crossvalidation]. `fastml` supports grouped, blocked and rolling
designs for these, and treats an unexecutable request as an error rather than
approximating it — `blocked_cv` without an ordering column, `grouped_cv` without
grouping columns, or data not sorted in ascending order of the ordering variable
all stop the run.

The initial holdout split follows the same logic. When `data` and `test_size`
are given together, the holdout rule is derived from the dependence structure the
user declared: whole groups when grouping columns are present, the final
proportion in time order when an ordering column is, and a stratified row-wise
split otherwise. A grouped cross-validation therefore cannot be paired with a
row-wise holdout.

# Core functionality

A single `fastml()` call trains and compares models across 15 classification, 14
regression and 11 survival families, dispatching to established engines in the
`tidymodels` stack. Seven resampling designs are available, alongside `none` for
a plain holdout: ordinary and repeated cross-validation, bootstrap,
`grouped_cv`, `blocked_cv` and `rolling_origin` for dependent data, and
`nested_cv`, which separates hyperparameter selection from performance reporting
because selecting on the folds used to report biases the estimate
[@varma2006bias].

Survival support spans parsnip-compatible engines and native implementations,
including XGBoost accelerated failure time with interval bounds
[@barnwal2020aft] and a piecewise-exponential model through `flexsurv`
[@jackson2016flexsurv]. Beyond fitting, the package provides audit utilities that
screen user-supplied recipes for external-data and global-environment
dependencies, exploratory diagnostics, and model explanation through permutation
importance, ALE, LIME, ICE curves, surrogate trees and counterfactuals.

# Example workflow

```r
library(fastml)

fit <- fastml(
  data       = my_data,
  label      = "outcome",
  algorithms = c("rand_forest", "logistic_reg"),
  resampling_method = "cv",
  folds      = 10
)

summary(fit)                 # cross-validated comparison and the selected model
plot(fit, type = "roc")
predict(fit, newdata = new_rows)
```

Preprocessing arguments such as `impute_method`, `scaling_methods` and
`balance_method` are passed to the same call; each becomes an untrained step
estimated inside every fold. Dependence structure is declared the same way —
`group_cols` for grouped data, `block_col` with `blocked_cv` or `rolling_origin`
for time-ordered data — and governs the holdout split as well as the folds.

# Research impact statement

`fastml` has been on CRAN since November 2024 [@fastml] and has been downloaded
approximately 108,700 times. Development has been sustained and public across 22
months and fifteen CRAN releases, and the repository has attracted 109 stars and
8 forks.

Use extends beyond the authors. The public issue tracker carries reports from
independent users — among them a maintainer of widely used R parallelisation
infrastructure, on the package's handling of `future` backends, and others on
its visualisation behaviour. The package is accompanied by a separate
methodological manuscript that quantifies, through Monte Carlo simulation and
applied case studies, how much preprocessing placement alone inflates
performance [@korkmaz2026fastmlpaper]; that article reports the empirical
findings, while the present paper describes the software.

# AI usage disclosure

<!--
  AUTHORS: JOSS requires this section to state (i) which generative-AI tools and
  model versions were used and where (code, documentation, paper text), (ii) the
  nature and scope of the assistance, and (iii) an explicit confirmation that the
  human authors reviewed, edited and validated all AI-assisted output and made
  the core design decisions. An incomplete or inaccurate disclosure is treated by
  JOSS as an ethical breach, not a formatting problem. Please replace the
  paragraph below with an accurate account, or with an explicit statement of
  non-use if no such tools were involved.
-->

*[To be completed by the authors before submission.]*

# Conflicts of interest and funding

The authors declare no financial conflicts of interest. The work received no
external financial support.

# Acknowledgements

We thank the users who reported issues against the package for feedback that
shaped its interface and resampling behaviour.

# References
