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

Preprocessing leakage happens when a data-dependent transformation, such as
scaling, imputation, class rebalancing or feature construction, is estimated on
the whole dataset before resampling. Each fold's held-out observations have then
already influenced the data the model trains on, and the performance estimate
that follows is optimistic. This happens even for transformations that never
touch the outcome labels [@moscovich2022preprocessing; @kaufman2012leakage]. One
survey found the problem across seventeen scientific fields and in 294 papers
[@kapoor2023leakage], and in at least one domain it has been shown to inflate
published results [@rosenblatt2024leakage].

`fastml` is an R package that trains, tunes and compares machine-learning models
through a single call. In `fastml`, *fold-local* preprocessing is the default
execution path and not a convention the user has to remember to follow. Every
data-dependent transformation is re-estimated inside each resampling split from
that split's analysis data alone, then applied to the corresponding assessment
data. The package covers classification, regression and survival tasks, and it
offers resampling designs that respect grouping and time order alongside ordinary
cross-validation. The evaluation design stays explicit and under the user's
control.

# Statement of need

Modular machine-learning frameworks break modelling into resampling,
preprocessing, model specification and evaluation. Modularity is useful, but it
also puts the burden of correct composition on the user, who has to apply the
steps in the right order and keep training and assessment data strictly
separated. Experienced practitioners still get this wrong in grouped, nested or
time-ordered designs, and no software check catches it. A leaky pipeline is a
perfectly valid pipeline: it runs, it produces a number, and nothing in the
output suggests that anything is wrong.

We wrote `fastml` for applied researchers who need a defensible performance
estimate more than they need a maximally flexible pipeline. For that audience a
silent error is expensive, and the freedom to compose a pipeline in any order is
worth little. Every framework discussed below can express a safe workflow. The
harder question is how easily each one can also express an unsafe workflow by
accident.

`fastml` answers that by narrowing the interface. Its preprocessing arguments
compile to untrained specifications that are estimated only inside the resampling
loop. A recipe already processed with `prep()` may contain parameters estimated 
outside the training process. The package therefore rejects it before training 
begins. We accepted this trade-off deliberately. The restricted interface prevents 
unsafe combinations but also rules out arbitrary pipeline structures, preprocessing 
parameter tuning, and stacking. Two limitations remain. The package cannot undo 
preprocessing performed before the call. Also, a custom step in a user-supplied 
recipe can still access values computed elsewhere.

# State of the field

Four R frameworks are the obvious points of comparison. `caret` [@kuhn2008caret]
offers a legacy unified training interface. `tidymodels` [@kuhn2020tidymodels]
emphasises explicit composition of recipes, model specifications, workflows and
resampling. `mlr3` [@lang2019mlr3] provides an object-oriented task and learner
design with a strong benchmarking culture, and `h2o` [@fryda2020h2o] supplies an
integrated AutoML system with its own training engine.

The comparison that matters most is with `mlr3pipelines`
[@binder2021mlr3pipelines], because it couples preprocessing to evaluation in
much the same way. Its `GraphLearner` composes preprocessing and a learner into a
single object that is re-fitted on the training part of every fold, so fold-local
preprocessing is already structural there and no separate guard is needed.
`tidymodels` and `mlr3` can express leakage-safe workflows too. They can also
express leakage-prone ones, just as easily and with no diagnostic.

`fastml` was developed not because existing frameworks cannot support safe workflows, 
but because their use often depends on user configuration and expertise. Its main 
distinctions are the default workflow and the constraints it enforces. The package 
restricts supported pipeline structures to ensure that preprocessing remains 
within the resampling loop. It rejects unsupported designs rather than executing 
an approximation that may compromise their validity. Survival modelling requires 
a more qualified comparison. Specialised frameworks such as mlr3proba `mlr3proba`
[@sonabend2021mlr3proba] offer comparable or broader capabilities. In this setting, 
the contribution of `fastml` lies primarily in providing a consistent interface 
across heterogeneous learners, rather than greater methodological depth.

# Software design

Under the guarded path, `fastml` works through each fold $k$ in turn
(\autoref{fig:placement}). It builds the split, fits a fresh preprocessing
specification on the analysis data $D^{(k)}_{\text{analysis}}$ alone, applies
that fold-trained specification to both $D^{(k)}_{\text{analysis}}$ and
$D^{(k)}_{\text{assess}}$, and only then fits and evaluates the model. The
assessment set is never used to estimate the transformation, only to be
transformed by it. Tuning runs inside the same loop, so hyperparameter selection
inherits the separation instead of undermining it.

![Where preprocessing is estimated. **(A)** Estimating a data-dependent
transformation on the whole dataset before resampling lets each fold's assessment
observations influence the data the model trains on, so the resulting score is
optimistic. **(B)** The guarded path that `fastml` follows. The transformation is
re-estimated from each fold's analysis part alone and then applied to both parts,
so the assessment part is only ever transformed, never used to estimate the
transformation itself.\label{fig:placement}](paper-figures/preprocessing-placement.png)

The second design consideration concerned which requests the software should reject. 
Fold-local preprocessing addresses one source of leakage, but valid evaluation also 
requires a resampling design that accounts for dependencies in the data. For 
example, correlated observations from the same entity may be assigned to different 
folds [@roberts2017crossvalidation], or training observations may occur after the 
assessment period [@bergmeir2012crossvalidation]. `fastml` supports grouped, 
blocked, and rolling resampling designs to address these settings. It returns an 
error when a requested design cannot be executed as specified. In particular, 
execution stops if `blocked_cv` is used without an ordering column, `grouped_cv` is 
used without grouping columns, or a design requiring ordered data receives 
observations that are not sorted in ascending order of the ordering variable.

The initial holdout split follows the same principles. When `data` and `test_size` 
are supplied together, the split is determined by the user-specified dependence 
structure. If grouping columns are provided, entire groups are assigned to the 
holdout set. If an ordering column is provided, the final proportion of observations 
in temporal order is held out. Otherwise, the package uses a stratified row-wise split. 
This ensures that grouped cross-validation is not combined with a row-wise holdout.

# Core functionality

A single `fastml()` call trains and compares models from 15 classification, 14 
regression, and 11 survival model families using established engines within the 
`tidymodels` ecosystem. The package supports seven resampling designs: ordinary 
and repeated cross-validation, bootstrap, `grouped_cv`, `blocked_cv`, `rolling_origin`, 
and `nested_cv`. The `none` option provides holdout evaluation without resampling. 
Grouped, blocked, and rolling designs accommodate dependent data, whereas nested 
cross-validation separates hyperparameter selection from performance estimation. 
This separation reduces the selection bias that arises when the same folds are 
used for both purposes [@varma2006bias].

Survival modelling is supported through both `parsnip`-compatible engines and 
native implementations. These include the XGBoost accelerated failure time model 
with interval bounds [@barnwal2020aft] and a piecewise-exponential model implemented 
through `flexsurv` [@jackson2016flexsurv]. The package also provides utilities to 
screen user-supplied recipes for dependencies on external data and the global 
environment. Additional features include exploratory diagnostics and model 
interpretation using permutation importance, accumulated local effects (ALE), 
LIME, individual conditional expectation (ICE) curves, surrogate trees, and 
counterfactual explanations.

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

Preprocessing options, including `impute_method`, `scaling_methods`, and 
`balance_method`, are specified in the same call. Each option defines an 
untrained preprocessing step that is estimated using only the training portion 
of each fold. Data dependencies are also specified within this call: `group_cols` 
identifies grouping variables, while `block_col` defines the ordering variable 
for `blocked_cv` or `rolling_origin`. These settings determine both the initial 
holdout split and the resampling folds.


# Research impact statement

`fastml` has been available on CRAN since November 2024 [@fastml], with 
approximately 108,700 downloads. Over 22 months of public development, the package 
has had 15 CRAN releases, and its repository has received 109 stars and 8 forks.

Feedback from users outside the development group has informed package development. 
Public issue reports include feedback on the handling of `future` backends from a 
maintainer of widely used R parallelisation infrastructure, as well as reports 
from other users on visualisation behaviour. A separate methodological manuscript 
uses Monte Carlo simulations and applied case studies to quantify performance 
inflation attributable to preprocessing placement [@korkmaz2026fastmlpaper]. 
That manuscript presents the empirical evaluation, while the present article 
focuses on the software.


# AI usage disclosure

We used Anthropic’s Claude models through Claude Code during the development of 
`fastml` and the preparation of this manuscript, most recently Claude Opus 5. 
For software development, AI assistance supported defect identification, diagnosis, 
and the drafting of code and tests. The authors wrote the manuscript and used 
AI solely to improve language clarity and concision. All package design decisions 
were made by the authors. Every AI-assisted software change was reviewed, tested, 
and approved before inclusion, and all AI-assisted manuscript edits were checked
by the authors. The authors take full responsibility for the software and the 
manuscript, including any remaining errors.


# Conflicts of interest and funding

We declare no financial conflicts of interest. The work received no external
funding.

# Acknowledgements

We thank the users who reported issues against the package. Their feedback shaped
its interface and its resampling behaviour.

# References
