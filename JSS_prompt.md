# Prompt: Drafting the JSS Article for \pkg{fastml}

You are completing a full Journal of Statistical Software (JSS) manuscript for the R package \pkg{fastml}. The LaTeX source already contains metadata (title, authors, abstract) and the preamble. **Do not modify any metadata or preamble elements.** Replace all placeholder text with polished, publication-ready prose.

## Structural Freedom with Title Discipline
You may reorganize the manuscript as needed:
- Add, remove, or rename section and subsection titles (including deeper levels such as `\subsubsection{}`),
- Reorder content where logical,
- Introduce additional hierarchical levels when helpful.

Titles should be concise, descriptive, and typical of JSS articles (e.g., “Methodology,” “Software Architecture,” “Case Study,” “Related Software,” “Conclusion”). Avoid marketing language or overly informal wording in headings.

## Required Conceptual Components
Ensure the manuscript contains the following elements (titles may differ):
1. Introduction
2. Statistical and computational methodology
3. Software design, architecture, and data flow
4. Description of core functions: `\code{fast_explore()}`, `\code{fastml()}`, `\code{fastexplain()}`
5. Illustrative example with real R code
6. Comparison with related software (`\pkg{caret}`, `\pkg{tidymodels}`, `\pkg{mlr3}`)
7. Summary / Conclusions

## Suggested Outline for Strong Section Titles
Consider adopting or adapting a structure such as:
- Introduction (context, motivation, contributions)
- Design Philosophy and Statistical Framework (leakage-guarded resampling, fold-isolated preprocessing)
- Methodology and Algorithms (cross-validation, nested CV, grouped/blocked resampling, tuning, multi-model workflows)
- Software Architecture and Object Model (pipelines, data flow, standardized result objects)
- Core User-Facing Functions (`\code{fast_explore()}`, `\code{fastml()}`, `\code{fastexplain()}`)
- Reproducible Workflow Example (end-to-end R code with commentary)
- Explainability via \pkg{DALEX} (SHAP, permutation importance, model profiles)
- Parallelization and Reproducibility (options, seeds, logging)
- Related Software (comparison with `\pkg{caret}`, `\pkg{tidymodels}`, `\pkg{mlr3}` including a comparison table)
- Discussion and Conclusion (limitations, future work, summary)

## fastml-Specific Content
Use information from the vignette and `DESCRIPTION`. The paper must describe:
- \pkg{fastml} as a unified ML framework in R,
- Leakage-guarded resampling design,
- Fold-isolated preprocessing,
- Cross-validation, nested CV, and optionally grouped/blocked resampling,
- Multi-model workflow and hyperparameter tuning,
- Parallelization and reproducibility features,
- Explainability via \pkg{DALEX} (SHAP, permutation importance, model profiles),
- Standardized result objects, internal pipelines, and object structure,
- A reproducible example.

## Writing Style
- Academic, clear tone consistent with JSS style; avoid hype or marketing language.
- Use `\pkg{}` for packages and `\code{}` for functions; escape underscores.
- Figures may appear as LaTeX placeholders with captions.
- Use `\citep{}` placeholders where citations are needed.
- Keep examples concise—no long raw output dumps.

## Illustrative Example
Include a complete workflow (e.g., on `iris`):
1. `\code{fast_explore()}`
2. `\code{fastml()}` with multiple model choices
3. Performance summaries
4. `\code{fastexplain()}` with interpretation
5. Commentary on results

## Comparison Requirements
Provide a substantive comparison with `\pkg{caret}`, `\pkg{tidymodels}`, and `\pkg{mlr3}`. Include at least one comparison table covering:
- API design
- Leakage protection
- Availability of nested CV
- Explainability support
- Ease of automation
- Output structure

## Freedom to Expand
You may add diagrams (placeholders), conceptual clarifications, additional examples, or subsections such as “Design Philosophy,” “Guarded Resampling Framework,” or “Internal API Contracts,” provided they support the manuscript.

## Final Sections
Retain `\section*{Acknowledgments}` and `\bibliography{fastml}` unchanged.
