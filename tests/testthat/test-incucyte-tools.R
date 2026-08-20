example_dir <- testthat::test_path("..", "..", "data", "example")
plate_map_file <- testthat::test_path("..", "..", "config", "plate_map.csv")

testthat::test_that("file content and optional condition filename are parsed", {
  parsed <- read_incucyte_file(
    file.path(example_dir, "C1_B1_R1_P1.txt")
  )

  testthat::expect_equal(
    parsed$file_metadata$metric,
    "Phase Object Confluence (%)"
  )
  testthat::expect_equal(parsed$file_metadata$header_row, 8L)
  testthat::expect_equal(parsed$file_metadata$decimal_mark, ",")
  testthat::expect_equal(parsed$file_metadata$condition_id, 1L)
  testthat::expect_equal(parsed$file_metadata$batch_id, 1L)
  testthat::expect_equal(parsed$file_metadata$replicate_id, 1L)
  testthat::expect_equal(parsed$file_metadata$plate_id, 1L)
  testthat::expect_equal(nrow(parsed$observations), 15L)

  sample_a_at_24h <- parsed$observations |>
    dplyr::filter(sample == "Sample A", elapsed_hours == 24)
  testthat::expect_equal(sample_a_at_24h$value, 34)
  testthat::expect_equal(sample_a_at_24h$value_sd, 1.3)
})

testthat::test_that("directory import assigns ordered file identifiers from F001", {
  imported <- read_incucyte_dir(example_dir)

  testthat::expect_s3_class(imported, "incucyte_import")
  testthat::expect_equal(
    imported$file_metadata$file_id,
    sprintf("F%03d", seq_len(nrow(imported$file_metadata)))
  )
  testthat::expect_equal(imported$file_metadata$file_id[[1L]], "F001")
  testthat::expect_equal(nrow(imported$file_metadata), 4L)
})

testthat::test_that("focus exports are identified by metadata and matched by vessel", {
  focus_file <- read_incucyte_file(
    file.path(example_dir, "focus_R1.txt")
  )
  testthat::expect_equal(focus_file$file_metadata$metric_id, "focus_position")
  testthat::expect_equal(focus_file$file_metadata$filename_scheme, "unparsed")

  imported <- read_incucyte_dir(example_dir)
  plate_map <- read_plate_map(plate_map_file)
  analysis <- build_analysis_table(
    imported,
    plate_map = plate_map,
    focus_threshold_um = 5
  )

  testthat::expect_equal(nrow(analysis), 30L)
  testthat::expect_false(any(analysis$metric_id == "focus_position"))
  testthat::expect_true(all(analysis$condition == "example_treatment"))

  flagged <- analysis |>
    dplyr::filter(
      source_file == "C1_B1_R1_P1.txt",
      sample == "Sample A",
      elapsed_hours == 36
    )
  testthat::expect_true(flagged$focus_flag)
  testthat::expect_equal(flagged$focus_source_file, "focus_R1.txt")

  replicate_two <- analysis |>
    dplyr::filter(
      source_file == "C1_B1_R2_P1.txt",
      sample == "Sample A",
      elapsed_hours == 36
    )
  testthat::expect_false(replicate_two$focus_flag)
})

testthat::test_that("manual exclusions add audit flags without deleting data", {
  imported <- read_incucyte_dir(example_dir)
  analysis <- build_analysis_table(imported)
  rules <- tibble::tibble(
    source_file = "C1_B1_R1_P1.txt",
    vessel_name = NA_character_,
    metric_id = NA_character_,
    sample = "Sample B",
    start_hours = 24,
    end_hours = 24,
    reason = "Synthetic test exclusion"
  )

  flagged <- flag_exclusions(analysis, rules)
  testthat::expect_equal(nrow(flagged), nrow(analysis))
  testthat::expect_equal(sum(flagged$excluded), 1L)
  testthat::expect_equal(
    flagged$exclusion_reason[flagged$excluded],
    "Synthetic test exclusion"
  )
})

testthat::test_that("baseline correction and interpolation preserve source curves", {
  imported <- read_incucyte_dir(example_dir)
  analysis <- build_analysis_table(imported)
  corrected <- baseline_correct_incucyte(analysis)

  first_values <- corrected |>
    dplyr::group_by(file_id, sample, series_index) |>
    dplyr::slice_min(elapsed_hours, n = 1L, with_ties = FALSE) |>
    dplyr::pull(value_baseline_corrected)
  testthat::expect_equal(first_values, rep(0, length(first_values)))
  testthat::expect_true("value" %in% names(corrected))

  interpolated <- interpolate_incucyte(analysis, interval_hours = 6)
  testthat::expect_equal(nrow(interpolated), 54L)

  sample_a_at_6h <- interpolated |>
    dplyr::filter(
      source_file == "C1_B1_R1_P1.txt",
      sample == "Sample A",
      elapsed_hours == 6
    )
  testthat::expect_equal(sample_a_at_6h$value_interpolated, 11.5)
  testthat::expect_false(sample_a_at_6h$is_observed)

  sample_a_at_12h <- interpolated |>
    dplyr::filter(
      source_file == "C1_B1_R1_P1.txt",
      sample == "Sample A",
      elapsed_hours == 12
    )
  testthat::expect_true(sample_a_at_12h$is_observed)
})

testthat::test_that("plotting keeps replicates separate and labels endpoints", {
  imported <- read_incucyte_dir(example_dir)
  plate_map <- read_plate_map(plate_map_file)
  analysis <- build_analysis_table(imported, plate_map = plate_map)
  overview <- plot_incucyte_curves(analysis)

  testthat::expect_s3_class(overview, "ggplot")
  testthat::expect_equal(nrow(overview$data), nrow(analysis))
  testthat::expect_equal(dplyr::n_distinct(overview$data$.curve_id), 6L)

  labels <- unique(overview$data$.curve_label)
  testthat::expect_true(
    all(c(
      "Sample A · R1",
      "Sample A · R2"
    ) %in% labels)
  )
})
