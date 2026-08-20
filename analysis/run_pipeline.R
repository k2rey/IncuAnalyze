# Run from the repository root:
#   Rscript analysis/run_pipeline.R

if (!file.exists("R/incucyte_tools.R")) {
  stop("Run this script from the repository root.", call. = FALSE)
}

source("R/incucyte_tools.R")
check_incucyte_dependencies()

# -----------------------------------------------------------------------------
# Analysis settings

# The repository runs immediately with synthetic data. Change this to
# "data/raw" after adding your own exports there.
raw_dir <- "data/example"
output_dir <- "output"
timezone <- "UTC"

# NULL enables automatic detection. This covers dot-separated day-first dates,
# ISO dates, and slash-separated dates when their order can be established from
# the files. For a standalone ambiguous US export, use:
#   "%m/%d/%Y %I:%M:%S %p"
datetime_format <- NULL

# "auto" supports decimal-comma and decimal-point exports and checks the values
# in each file independently.
decimal_mark <- "auto"

# NULL records focus information without flagging observations. Set a positive
# threshold only after validating it for the instrument and experiment.
focus_threshold_um <- NULL

# Baseline correction is off by default because it changes interpretation of
# absolute signals. When enabled, a new column is added; raw values are retained.
apply_baseline_correction <- FALSE

# NULL disables interpolation. Otherwise, supply a positive time interval in
# hours. The interpolated table remains separate from measured observations.
interpolation_interval_hours <- NULL

# -----------------------------------------------------------------------------
# Import and prepare

imported <- read_incucyte_dir(
  raw_dir,
  decimal_mark = decimal_mark,
  timezone = timezone,
  datetime_format = datetime_format
)
print(imported)

plate_map <- if (file.exists("config/plate_map.csv")) {
  read_plate_map("config/plate_map.csv")
} else {
  NULL
}

analysis_data <- build_analysis_table(
  imported,
  plate_map = plate_map,
  focus_threshold_um = focus_threshold_um
)

exclusions <- if (file.exists("config/exclusions.csv")) {
  read_exclusions("config/exclusions.csv")
} else {
  NULL
}
analysis_data <- flag_exclusions(analysis_data, exclusions)

value_column <- "value"
if (apply_baseline_correction) {
  analysis_data <- baseline_correct_incucyte(analysis_data)
  value_column <- "value_baseline_corrected"
}

# Filtering is explicit here for plotting.
plotting_input <- analysis_data |>
  dplyr::filter(!excluded, !focus_flag)

# -----------------------------------------------------------------------------
# Export

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(imported$file_metadata, file.path(output_dir, "file_metadata.csv"))
readr::write_csv(imported$observations, file.path(output_dir, "raw_observations.csv"))
readr::write_csv(analysis_data, file.path(output_dir, "analysis_data.csv"))
saveRDS(imported, file.path(output_dir, "incucyte_import.rds"))

if (!is.null(interpolation_interval_hours)) {
  interpolated_data <- interpolate_incucyte(
    plotting_input,
    interval_hours = interpolation_interval_hours,
    value_col = value_column
  )
  readr::write_csv(
    interpolated_data,
    file.path(output_dir, "interpolated_data.csv")
  )
}

overview <- plot_incucyte_curves(
  plotting_input,
  value_col = value_column,
  label_endpoints = TRUE,
  show_legend = FALSE
)
ggplot2::ggsave(
  filename = file.path(output_dir, "curve_overview.pdf"),
  plot = overview,
  width = 10,
  height = 7,
  units = "in",
  bg = "white"
)
ggplot2::ggsave(
  filename = file.path(output_dir, "curve_overview.png"),
  plot = overview,
  width = 10,
  height = 7,
  units = "in",
  dpi = 180,
  bg = "white"
)

message("Finished. Results written to: ", normalizePath(output_dir))
