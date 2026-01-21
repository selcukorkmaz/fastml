utils::globalVariables(c(
  ".", "..density..",
  "Var1", "Var2", "Correlation",
  "variable", "Missing",
  "Model", "Engine",
  "truth", "estimate",
  "id",  "valid_model"
))

.onLoad <- function(libname, pkgname) {
  # Validate parsnip defaults registry on package load (in interactive sessions only)
  if (interactive()) {
    mismatches <- tryCatch(
      validate_defaults_registry(),
      error = function(e) list()
    )

    if (length(mismatches) > 0) {
      packageStartupMessage(
        sprintf(
          "Note: fastml's parsnip defaults registry may be out of date. Found %d mismatch(es).\n",
          length(mismatches)
        ),
        "Run validate_defaults_registry() for details."
      )
    }
  }
}
