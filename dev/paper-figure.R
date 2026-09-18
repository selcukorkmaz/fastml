# Regenerates the figure used by paper.md.
#
#   Rscript dev/paper-figure.R
#
# Base graphics only, so the figure is reproducible from a clean R installation
# with no packages beyond those shipped with R. The figure is a redrawn,
# simplified version of the schematic in the accompanying methodological
# manuscript: it carries the same argument, but is sized for a two-page paper
# and names the two parts of a fold correctly rather than calling both of them
# the full dataset.

figure_path <- file.path("paper-figures", "preprocessing-placement.png")

CEX_BOX <- 0.85
CEX_SUB <- 0.80
HL      <- "grey85"   # the box whose position is the whole point

draw_figure <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  grDevices::png(path, width = 9.2, height = 5.8, units = "in", res = 300,
                 bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit(graphics::par(op), add = TRUE)

  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 104), ylim = c(0, 100))

  # Geometry ----------------------------------------------------------------
  xA <- 24; xA_l <- 13; xA_r <- 35
  xB <- 77; xB_l <- 62; xB_r <- 92
  w_wide_A <- 42; w_wide_B <- 50; w_col <- 20; h <- 8

  L1 <- 86; L2 <- 75; L3 <- 64; L4 <- 53; L5 <- 42; L6 <- 30; L7 <- 19

  node <- function(x, y, w, label, fill = "white") {
    graphics::rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2,
                   col = fill, border = "black", lwd = 1)
    graphics::text(x, y, label, cex = CEX_BOX)
  }
  down <- function(x, y_from, y_to) {
    graphics::arrows(x, y_from - h / 2, x, y_to + h / 2,
                     length = 0.07, angle = 20, lwd = 1)
  }
  branch <- function(x, y_from, x_left, x_right, y_to) {
    ym <- (y_from - h / 2 + y_to + h / 2) / 2
    graphics::segments(x, y_from - h / 2, x, ym)
    graphics::segments(x_left, ym, x_right, ym)
    graphics::arrows(x_left, ym, x_left, y_to + h / 2, length = 0.07, angle = 20)
    graphics::arrows(x_right, ym, x_right, y_to + h / 2, length = 0.07, angle = 20)
  }
  merge_in <- function(x_left, x_right, y_from, x, y_to) {
    ym <- (y_from - h / 2 + y_to + h / 2) / 2
    graphics::segments(x_left, y_from - h / 2, x_left, ym)
    graphics::segments(x_right, y_from - h / 2, x_right, ym)
    graphics::segments(x_left, ym, x_right, ym)
    graphics::arrows(x, ym, x, y_to + h / 2, length = 0.07, angle = 20)
  }

  # Panel A: preprocessing before resampling ---------------------------------
  graphics::text(xA, 97, "A", font = 2, cex = 1.15)
  graphics::text(xA, 93, "Preprocessing estimated before resampling",
                 cex = CEX_SUB, font = 3)

  node(xA, L1, w_wide_A, "Full dataset (X, Y)")
  node(xA, L2, w_wide_A,
       "Estimate preprocessing on all rows\n(scaling, imputation, feature construction)",
       fill = HL)
  node(xA, L3, w_wide_A, "Transformed dataset")
  node(xA, L4, 26, "Split into folds")
  node(xA_l, L5, w_col, "Analysis part\nof fold k")
  node(xA_r, L5, w_col, "Assessment part\nof fold k")
  node(xA, L6, w_wide_A, "Fit on analysis, score on assessment")
  node(xA, L7, w_wide_A, "Optimistically biased estimate")

  down(xA, L1, L2); down(xA, L2, L3); down(xA, L3, L4)
  branch(xA, L4, xA_l, xA_r, L5)
  merge_in(xA_l, xA_r, L5, xA, L6)
  down(xA, L6, L7)

  # Panel B: preprocessing inside each fold ----------------------------------
  graphics::text(xB, 97, "B", font = 2, cex = 1.15)
  graphics::text(xB, 93, "Preprocessing estimated inside each fold",
                 cex = CEX_SUB, font = 3)

  node(xB, L1, w_wide_B, "Full dataset (X, Y)")
  node(xB, L2, 26, "Split into folds")
  node(xB_l, L3, w_col, "Analysis part\nof fold k")
  node(xB_r, L3, w_col, "Assessment part\nof fold k")
  node(xB_l, L4, w_col, "Estimate preprocessing\non analysis part only",
       fill = HL)
  node(xB_l, L5, w_col, "Transformed\nanalysis part")
  node(xB_r, L5, w_col, "Transformed\nassessment part")
  node(xB, L6, w_wide_B, "Fit on analysis, score on assessment")
  node(xB, L7, w_wide_B, "Estimate without preprocessing leakage")

  down(xB, L1, L2)
  branch(xB, L2, xB_l, xB_r, L3)
  down(xB_l, L3, L4)
  down(xB_l, L4, L5)
  # The assessment part is only transformed, never used to estimate the
  # transformation: its data flows straight down, the parameters come in
  # sideways from the analysis part.
  graphics::arrows(xB_r, L3 - h / 2, xB_r, L5 + h / 2,
                   length = 0.07, angle = 20)
  x_gap <- (xB_l + w_col / 2 + xB_r - w_col / 2) / 2
  graphics::segments(xB_l + w_col / 2, L4, x_gap, L4)
  graphics::segments(x_gap, L4, x_gap, L5)
  graphics::arrows(x_gap, L5, xB_r - w_col / 2, L5, length = 0.07, angle = 20)
  graphics::text(x_gap, L4 + 2.6, "fold-trained\nparameters", cex = 0.62)
  merge_in(xB_l, xB_r, L5, xB, L6)
  down(xB, L6, L7)

  # Caveat -------------------------------------------------------------------
  graphics::rect(3, 3, 101, 11, border = "black", lwd = 1)
  graphics::text(52, 7,
    paste0("Fold-local preprocessing addresses one leakage mechanism. ",
           "Records that share an entity, or training data\n",
           "that postdates the assessment period, still need a resampling ",
           "design that respects them."),
    cex = CEX_SUB)

  invisible(path)
}

draw_figure(figure_path)
cat("wrote", figure_path, "\n")
