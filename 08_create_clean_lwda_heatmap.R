#!/usr/bin/env Rscript

# Create a clean, native-cell Excel heatmap from Data_LWDA_V2 and save a
# matching PNG/SVG for Quarto or R Markdown.
#
# Usage:
#   Rscript 08_create_clean_lwda_heatmap.R \
#     iowa_ai_workforce_atlas_all_editable_figures.xlsx \
#     iowa_ai_workforce_atlas_with_clean_lwda_heatmap.xlsx \
#     figures/public

required_packages <- c(
  "openxlsx2", "readxl", "dplyr", "tidyr", "ggplot2", "scales"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install these packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

`%||%` <- function(x, y) {
  if (length(x) == 0L || is.na(x) || !nzchar(x)) y else x
}

args <- commandArgs(trailingOnly = TRUE)
input_xlsx <- args[1] %||% "iowa_ai_workforce_atlas_all_editable_figures.xlsx"
output_xlsx <- args[2] %||% sub(
  "\\.xlsx$", "_with_clean_lwda_heatmap.xlsx", input_xlsx,
  ignore.case = TRUE
)
figure_dir <- args[3] %||% file.path(dirname(output_xlsx), "figures", "public")

if (!file.exists(input_xlsx)) {
  stop("Input workbook not found: ", input_xlsx, call. = FALSE)
}
dir.create(dirname(output_xlsx), recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

source_sheet <- "Data_LWDA_V2"
figure_sheet <- "Fig_03_LWDA_Heatmap_Clean"

# Data_LWDA_V2 has two descriptive rows before the actual field names.
raw <- readxl::read_excel(
  input_xlsx,
  sheet = source_sheet,
  skip = 2,
  .name_repair = "unique"
)

pick_col <- function(candidates, description) {
  found <- candidates[candidates %in% names(raw)]
  if (!length(found)) {
    stop(
      "Missing ", description, ". Expected one of: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  found[[1]]
}

cols <- list(
  lwda = pick_col(c("lwda_combined", "combined_lwda", "lwda"), "LWDA field"),
  jobs = pick_col(c("estimated_jobs.x", "estimated_jobs", "jobs"), "jobs field"),
  automation = pick_col(c("automation_exposure_estimate", "automation_exposure"), "automation field"),
  augmentation = pick_col(c("augmentation_potential_estimate", "augmentation_potential"), "augmentation field"),
  expertise = pick_col(c("expertise_leveling_potential_estimate", "expertise_leveling_potential"), "expertise field"),
  new_task = pick_col(c("new_task_opportunity_estimate", "new_task_opportunity"), "new-task field"),
  pro_worker = pick_col(c("pro_worker_opportunity_estimate", "pro_worker_opportunity"), "pro-worker field")
)

lwda <- raw |>
  dplyr::transmute(
    lwda = as.character(.data[[cols$lwda]]),
    estimated_jobs = as.numeric(.data[[cols$jobs]]),
    automation_exposure = as.numeric(.data[[cols$automation]]),
    augmentation_potential = as.numeric(.data[[cols$augmentation]]),
    expertise_leveling_potential = as.numeric(.data[[cols$expertise]]),
    new_task_opportunity = as.numeric(.data[[cols$new_task]]),
    pro_worker_opportunity = as.numeric(.data[[cols$pro_worker]])
  ) |>
  dplyr::filter(!is.na(.data$lwda))

wanted_order <- c(
  "Central Iowa LWDA",
  "Iowa Plains LWDA",
  "East Central Iowa LWDA",
  "South Central Iowa LWDA",
  "Northeast Iowa LWDA"
)
if (all(wanted_order %in% lwda$lwda)) {
  lwda <- lwda |>
    dplyr::mutate(lwda = factor(.data$lwda, levels = wanted_order)) |>
    dplyr::arrange(.data$lwda) |>
    dplyr::mutate(lwda = as.character(.data$lwda))
}

score_cols <- c(
  "automation_exposure",
  "augmentation_potential",
  "expertise_leveling_potential",
  "new_task_opportunity",
  "pro_worker_opportunity"
)
if (nrow(lwda) != 5L) {
  stop("Expected five combined LWDAs; found ", nrow(lwda), ".", call. = FALSE)
}
score_matrix <- as.matrix(lwda[score_cols])
if (any(!is.finite(score_matrix)) || any(score_matrix < 0 | score_matrix > 1)) {
  stop("Score values must be finite 0-1 proportions.", call. = FALSE)
}

metric_labels <- c(
  automation_exposure = "Automation\nexposure",
  augmentation_potential = "Augmentation\npotential",
  expertise_leveling_potential = "Expertise-leveling\npotential",
  new_task_opportunity = "New-task\nopportunity",
  pro_worker_opportunity = "Pro-worker\nopportunity"
)

# -----------------------------------------------------------------------------
# R figure for Quarto / R Markdown
# -----------------------------------------------------------------------------
plot_data <- lwda |>
  dplyr::mutate(
    row_label = paste0(
      .data$lwda, "  |  ", scales::comma(.data$estimated_jobs), " jobs"
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(score_cols),
    names_to = "metric",
    values_to = "score"
  ) |>
  dplyr::mutate(
    metric = factor(
      .data$metric,
      levels = score_cols,
      labels = unname(metric_labels)
    ),
    row_label = factor(.data$row_label, levels = rev(unique(.data$row_label))),
    label_color = dplyr::if_else(.data$score >= 0.12, "white", "black")
  )

palette <- c("#C8E253", "#E3D22A", "#F0B400", "#B48BA8", "#0668C6")
stops <- scales::rescale(
  c(0.05, 0.075, 0.10, 0.15, 0.20),
  from = c(0.045, 0.205)
)

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = .data$metric, y = .data$row_label, fill = .data$score)
) +
  ggplot2::geom_tile(
    color = "white", linewidth = 1.5, width = 0.98, height = 0.92
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::percent(.data$score, accuracy = 0.1),
      color = .data$label_color
    ),
    fontface = "bold", size = 4.3, show.legend = FALSE
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_gradientn(
    colours = palette,
    values = stops,
    limits = c(0.045, 0.205),
    oob = scales::squish,
    breaks = c(0.10, 0.15),
    labels = scales::label_percent(accuracy = 1),
    name = "Employment-weighted score"
  ) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      color = "#111111", lineheight = 0.95,
      margin = ggplot2::margin(t = 8)
    ),
    axis.text.y = ggplot2::element_text(
      color = "#111111", hjust = 1,
      margin = ggplot2::margin(r = 8)
    ),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    legend.key.width = grid::unit(2.2, "cm"),
    plot.margin = ggplot2::margin(12, 18, 12, 18)
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5)
  )

png_path <- file.path(figure_dir, "lwda_ai_channel_heatmap_clean.png")
svg_path <- file.path(figure_dir, "lwda_ai_channel_heatmap_clean.svg")
ggplot2::ggsave(
  png_path, p, width = 12.5, height = 6.4, dpi = 320, bg = "white"
)
grDevices::svg(svg_path, width = 12.5, height = 6.4, bg = "white")
print(p)
grDevices::dev.off()

# -----------------------------------------------------------------------------
# Native Excel-cell figure
# -----------------------------------------------------------------------------
wb <- openxlsx2::wb_load(input_xlsx)
if (figure_sheet %in% wb$sheet_names) {
  wb <- openxlsx2::wb_remove_worksheet(wb, sheet = figure_sheet)
}
wb <- openxlsx2::wb_add_worksheet(
  wb, sheet = figure_sheet, grid_lines = FALSE, zoom = 90
)

# Title and narrative.
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet,
  x = "The strongest statewide signal is capability, not replacement",
  dims = "A1", col_names = FALSE
)
wb <- openxlsx2::wb_merge_cells(wb, figure_sheet, dims = "A1:F1")

subtitle <- paste(
  "Across Iowa's five combined Local Workforce Development Areas, average scores are remarkably similar.",
  "The figure compares employment-weighted planning indicators; it does not report employer adoption or predict layoffs."
)
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet, x = subtitle, dims = "A2", col_names = FALSE
)
wb <- openxlsx2::wb_merge_cells(wb, figure_sheet, dims = "A2:F3")

# Visible table. These are native values; rerunning the script refreshes them.
headers <- c("Combined LWDA | modeled jobs", unname(metric_labels))
row_labels <- paste0(
  lwda$lwda, " | ", scales::comma(lwda$estimated_jobs), " jobs"
)
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet, x = matrix(headers, nrow = 1),
  dims = "A5:F5", col_names = FALSE, enforce = TRUE
)
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet, x = matrix(row_labels, ncol = 1),
  dims = "A6:A10", col_names = FALSE, enforce = TRUE
)
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet, x = score_matrix,
  dims = "B6:F10", col_names = FALSE, enforce = TRUE
)

# Base alignment and number formats.
wb <- openxlsx2::wb_add_numfmt(
  wb, figure_sheet, dims = "B6:F10", numfmt = "0.0%"
)
wb <- openxlsx2::wb_add_cell_style(
  wb, figure_sheet, dims = "A1:F18",
  vertical = "center", wrap_text = TRUE
)
wb <- openxlsx2::wb_add_cell_style(
  wb, figure_sheet, dims = "B5:F10",
  horizontal = "center", vertical = "center", wrap_text = TRUE
)
wb <- openxlsx2::wb_add_cell_style(
  wb, figure_sheet, dims = "A6:A10",
  horizontal = "right", vertical = "center", wrap_text = TRUE
)

# Fonts.
wb <- openxlsx2::wb_add_font(
  wb, figure_sheet, dims = "A1:F1",
  name = "Aptos Display", size = 22, bold = TRUE, color = "FF003B63"
)
wb <- openxlsx2::wb_add_font(
  wb, figure_sheet, dims = "A2:F3",
  name = "Aptos", size = 11, color = "FF202020"
)
wb <- openxlsx2::wb_add_font(
  wb, figure_sheet, dims = "A5:F5",
  name = "Aptos", size = 10, bold = TRUE, color = "FF1A1A1A"
)
wb <- openxlsx2::wb_add_font(
  wb, figure_sheet, dims = "A6:A10",
  name = "Aptos", size = 10, color = "FF111111"
)

# Header and row labels.
wb <- openxlsx2::wb_add_fill(
  wb, figure_sheet, dims = "A5:F5", color = "FFE8EEF4"
)
wb <- openxlsx2::wb_add_border(
  wb, figure_sheet, dims = "A5:F5",
  left_border = NULL, right_border = NULL, top_border = NULL,
  bottom_border = "medium", bottom_color = "FF8EA5B8"
)
wb <- openxlsx2::wb_add_fill(
  wb, figure_sheet, dims = "A6:A10", color = "FFF7F9FB"
)
wb <- openxlsx2::wb_add_border(
  wb, figure_sheet, dims = "A6:A10",
  top_border = "thin", bottom_border = "thin",
  left_border = "thin", right_border = "thin",
  top_color = "FFD8E0E7", bottom_color = "FFD8E0E7",
  left_color = "FFD8E0E7", right_color = "FFD8E0E7",
  inner_hgrid = "thin", inner_hcolor = "FFD8E0E7"
)

# Heatmap colors. All cells remain individually editable in Excel.
score_fill <- function(x) {
  if (x < 0.065) {
    "FFC8E253"
  } else if (x < 0.090) {
    "FFE3D22A"
  } else if (x < 0.160) {
    "FFB48BA8"
  } else {
    "FF0668C6"
  }
}
score_font <- function(x) if (x >= 0.12) "FFFFFFFF" else "FF111111"

excel_cols <- LETTERS[2:6]
for (i in seq_len(nrow(score_matrix))) {
  for (j in seq_len(ncol(score_matrix))) {
    cell <- paste0(excel_cols[j], 5L + i)
    wb <- openxlsx2::wb_add_fill(
      wb, figure_sheet, dims = cell,
      color = score_fill(score_matrix[i, j])
    )
    wb <- openxlsx2::wb_add_font(
      wb, figure_sheet, dims = cell,
      name = "Aptos", size = 12, bold = TRUE,
      color = score_font(score_matrix[i, j])
    )
  }
}
wb <- openxlsx2::wb_add_border(
  wb, figure_sheet, dims = "B6:F10",
  top_border = "medium", bottom_border = "medium",
  left_border = "medium", right_border = "medium",
  top_color = "FFFFFFFF", bottom_color = "FFFFFFFF",
  left_color = "FFFFFFFF", right_color = "FFFFFFFF",
  inner_hgrid = "medium", inner_vgrid = "medium",
  inner_hcolor = "FFFFFFFF", inner_vcolor = "FFFFFFFF"
)

# Legend.
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet, x = "Employment-weighted score",
  dims = "B12", col_names = FALSE
)
wb <- openxlsx2::wb_merge_cells(wb, figure_sheet, dims = "B12:E12")
wb <- openxlsx2::wb_add_font(
  wb, figure_sheet, dims = "B12:E12",
  name = "Aptos", size = 11, bold = TRUE, color = "FF111111"
)
wb <- openxlsx2::wb_add_cell_style(
  wb, figure_sheet, dims = "B12:E13",
  horizontal = "center", vertical = "center"
)
legend_values <- c(0.05, 0.08, 0.12, 0.19)
wb <- openxlsx2::wb_add_data(
  wb, figure_sheet, x = matrix(legend_values, nrow = 1),
  dims = "B13:E13", col_names = FALSE, enforce = TRUE
)
wb <- openxlsx2::wb_add_numfmt(
  wb, figure_sheet, dims = "B13:E13", numfmt = "0%"
)
for (j in seq_along(legend_values)) {
  cell <- paste0(LETTERS[j + 1L], "13")
  wb <- openxlsx2::wb_add_fill(
    wb, figure_sheet, dims = cell,
    color = score_fill(legend_values[j])
  )
  wb <- openxlsx2::wb_add_font(
    wb, figure_sheet, dims = cell,
    name = "Aptos", size = 10, bold = TRUE,
    color = score_font(legend_values[j])
  )
}

# Source, caveat, and editing note.
notes <- c(
  paste(
    "Source: Iowa AI Workforce Atlas calculations using BLS OEWS, the BLS National Employment Matrix,",
    "Census QWI/LODES and ACS, O*NET, and AIOE as an external benchmark."
  ),
  paste(
    "Interpretation: These are employment-weighted planning indicators, not employer adoption rates,",
    "job-loss probabilities, productivity forecasts, or displacement dates."
  ),
  paste(
    "Editing: Every label, value, fill, font, border, row height, and column width is native Excel content.",
    "Rerun this script to refresh the sheet from Data_LWDA_V2."
  )
)
for (k in seq_along(notes)) {
  row <- 15L + k
  wb <- openxlsx2::wb_add_data(
    wb, figure_sheet, x = notes[k],
    dims = paste0("A", row), col_names = FALSE
  )
  wb <- openxlsx2::wb_merge_cells(
    wb, figure_sheet, dims = paste0("A", row, ":F", row)
  )
}
wb <- openxlsx2::wb_add_font(
  wb, figure_sheet, dims = "A16:F18",
  name = "Aptos", size = 9, color = "FF4F6373"
)

# Layout.
wb <- openxlsx2::wb_set_col_widths(
  wb, figure_sheet, cols = 1:6,
  widths = c(38, 17, 17, 19, 17, 17)
)
wb <- openxlsx2::wb_set_row_heights(
  wb, figure_sheet, rows = 1, heights = 31
)
wb <- openxlsx2::wb_set_row_heights(
  wb, figure_sheet, rows = 2:3, heights = 22
)
wb <- openxlsx2::wb_set_row_heights(
  wb, figure_sheet, rows = 5, heights = 42
)
wb <- openxlsx2::wb_set_row_heights(
  wb, figure_sheet, rows = 6:10, heights = 45
)
wb <- openxlsx2::wb_set_row_heights(
  wb, figure_sheet, rows = 12:13, heights = c(20, 24)
)
wb <- openxlsx2::wb_set_row_heights(
  wb, figure_sheet, rows = 16:18, heights = 25
)

openxlsx2::wb_save(wb, output_xlsx, overwrite = TRUE)

message("Created workbook: ", normalizePath(output_xlsx, winslash = "/", mustWork = FALSE))
message("Created PNG:      ", normalizePath(png_path, winslash = "/", mustWork = FALSE))
message("Created SVG:      ", normalizePath(svg_path, winslash = "/", mustWork = FALSE))
message("New worksheet:    ", figure_sheet)
