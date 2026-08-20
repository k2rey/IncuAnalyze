# Reusable tools for importing and preparing tab-delimited Incucyte exports.
#
# The functions in this file deliberately avoid changing the working directory,
# attaching packages, modifying global options, or overwriting raw values.

check_incucyte_dependencies <- function() {
  required <- c(
    "dplyr", "ggplot2", "ggrepel", "purrr", "readr", "rlang", "stringr",
    "tibble", "tidyr"
  )
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0L) {
    stop(
      "Install the required packages before running the pipeline: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.first_non_empty <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) NA_character_ else trimws(x[[1L]])
}

.metadata_value <- function(metadata, key) {
  value <- unname(metadata[tolower(names(metadata)) == tolower(key)])
  .first_non_empty(value)
}

.detect_decimal_mark <- function(x, source_file) {
  x <- x[!is.na(x) & nzchar(trimws(x))]
  comma_decimal <- sum(stringr::str_detect(x, "[0-9],[0-9]"))
  point_decimal <- sum(stringr::str_detect(x, "[0-9]\\.[0-9]"))

  if (comma_decimal > 0L && point_decimal > 0L && comma_decimal == point_decimal) {
    stop(
      "Could not determine the decimal mark in '", source_file,
      "'. Set decimal_mark to ',' or '.' explicitly.",
      call. = FALSE
    )
  }

  if (comma_decimal > point_decimal) "," else "."
}

.parse_export_number <- function(x, decimal_mark, field, source_file) {
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "N/A")] <- NA_character_
  grouping_mark <- if (identical(decimal_mark, ",")) "." else ","

  parsed <- suppressWarnings(
    readr::parse_double(
      x,
      na = c("", "NA", "NaN", "N/A"),
      locale = readr::locale(
        decimal_mark = decimal_mark,
        grouping_mark = grouping_mark
      )
    )
  )

  bad <- which(!is.na(x) & is.na(parsed))
  if (length(bad) > 0L) {
    examples <- unique(x[bad])
    examples <- examples[seq_len(min(length(examples), 3L))]
    stop(
      "Could not parse ", field, " in '", source_file, "'. Example value(s): ",
      paste(shQuote(examples), collapse = ", "),
      call. = FALSE
    )
  }

  parsed
}

.detect_export_datetime_format <- function(x, source_file) {
  values <- trimws(x)
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0L) {
    stop("Date Time contains no values in '", source_file, "'.", call. = FALSE)
  }

  dot_day_first <- paste0(
    "^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}",
    "[[:space:]]+[0-9]{1,2}:[0-9]{2}:[0-9]{2}$"
  )
  iso_year_first <- paste0(
    "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}",
    "[[:space:]]+[0-9]{1,2}:[0-9]{2}:[0-9]{2}$"
  )

  if (all(grepl(dot_day_first, values))) {
    return("%d.%m.%Y %H:%M:%S")
  }
  if (all(grepl(iso_year_first, values))) {
    return("%Y-%m-%d %H:%M:%S")
  }

  slash_parts <- stringr::str_match(
    values,
    stringr::regex(
      paste0(
        "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})",
        "[[:space:]]+([0-9]{1,2}):([0-9]{2}):([0-9]{2})",
        "(?:[[:space:]]+(AM|PM))?$"
      ),
      ignore_case = TRUE
    )
  )

  if (all(!is.na(slash_parts[, 1L]))) {
    first <- as.integer(slash_parts[, 2L])
    second <- as.integer(slash_parts[, 3L])
    meridiem <- slash_parts[, 8L]
    has_meridiem <- !is.na(meridiem)

    if (any(has_meridiem) && !all(has_meridiem)) {
      stop(
        "Date Time mixes 12-hour and 24-hour clocks in '", source_file, "'.",
        call. = FALSE
      )
    }

    if (any(first > 12L & second > 12L)) {
      stop("Date Time contains an invalid slash date in '", source_file, "'.", call. = FALSE)
    }

    day_first_evidence <- any(first > 12L)
    month_first_evidence <- any(second > 12L)
    clock_format <- if (all(has_meridiem)) "%I:%M:%S %p" else "%H:%M:%S"
    candidate_formats <- c(
      paste("%m/%d/%Y", clock_format),
      paste("%d/%m/%Y", clock_format)
    )

    if (day_first_evidence && month_first_evidence) {
      stop(
        "Date Time contains conflicting slash-date orders in '", source_file, "'.",
        call. = FALSE
      )
    }
    if (month_first_evidence) return(candidate_formats[[1L]])
    if (day_first_evidence) return(candidate_formats[[2L]])

    rlang::abort(
      paste0(
        "The slash-separated date order in '", source_file, "' is ambiguous. ",
        "Supply datetime_format explicitly (for example, ",
        "'%m/%d/%Y %I:%M:%S %p')."
      ),
      class = "incucyte_ambiguous_datetime",
      source_file = source_file,
      candidate_formats = candidate_formats,
      call = NULL
    )
  }

  stop(
    "Could not detect the Date Time format in '", source_file,
    "'. Supply datetime_format explicitly.",
    call. = FALSE
  )
}

.parse_export_datetime <- function(x, timezone, datetime_format, source_file) {
  x <- trimws(x)

  if (is.null(datetime_format)) {
    datetime_format <- .detect_export_datetime_format(x, source_file)
  }

  parsed <- suppressWarnings(as.POSIXct(x, format = datetime_format, tz = timezone))
  bad <- which(!is.na(x) & nzchar(x) & is.na(parsed))

  if (length(bad) > 0L) {
    examples <- unique(x[bad])
    examples <- examples[seq_len(min(length(examples), 3L))]
    stop(
      "Could not parse date/time values in '", source_file,
      "' with format ", shQuote(datetime_format), ". Example value(s): ",
      paste(shQuote(examples), collapse = ", "),
      call. = FALSE
    )
  }

  parsed
}

normalise_metric_id <- function(metric) {
  ascii <- iconv(metric, from = "", to = "ASCII//TRANSLIT")
  ascii[is.na(ascii)] <- metric[is.na(ascii)]
  id <- tolower(gsub("[^A-Za-z0-9]+", "_", ascii))
  id <- gsub("^_+|_+$", "", id)
  id
}

.extract_metric_units <- function(metric) {
  match <- stringr::str_match(metric, "\\s*\\(([^()]*)\\)\\s*$")
  units <- match[, 2L]
  units[is.na(units) | !nzchar(units)] <- NA_character_
  units
}

# Parse supported filename conventions. Content metadata remains the source of
# truth; unmatched filenames are valid and receive NA design fields.
parse_incucyte_filename <- function(path) {
  filename <- basename(path)
  stem <- tools::file_path_sans_ext(filename)
  # Browsers and operating systems may append a collision suffix such as "(1)".
  clean_stem <- sub("\\([0-9]+\\)$", "", stem)

  full <- stringr::str_match(
    clean_stem,
    stringr::regex(
      "^C([0-9]+)_B([0-9]+)_R([0-9]+)_P([0-9]+)$",
      ignore_case = TRUE
    )
  )
  short <- stringr::str_match(
    clean_stem,
    stringr::regex("^(P[0-9]+)_([0-9]+)$", ignore_case = TRUE)
  )
  replicate_plate <- stringr::str_match(
    clean_stem,
    stringr::regex("^rep(?:licate)?([0-9]+)_p(?:late)?([0-9]+)$", ignore_case = TRUE)
  )

  if (!is.na(full[1L, 1L])) {
    return(tibble::tibble(
      filename_scheme = "condition_batch_replicate_plate",
      condition_id = as.integer(full[1L, 2L]),
      batch_id = as.integer(full[1L, 3L]),
      replicate_id = as.integer(full[1L, 4L]),
      plate_id = as.integer(full[1L, 5L]),
      plate_code = paste0("P", as.integer(full[1L, 5L]))
    ))
  }

  if (!is.na(short[1L, 1L])) {
    return(tibble::tibble(
      filename_scheme = "plate_replicate",
      condition_id = NA_integer_,
      batch_id = NA_integer_,
      replicate_id = as.integer(short[1L, 3L]),
      plate_id = as.integer(sub("^[Pp]", "", short[1L, 2L])),
      plate_code = toupper(short[1L, 2L])
    ))
  }

  if (!is.na(replicate_plate[1L, 1L])) {
    return(tibble::tibble(
      filename_scheme = "replicate_plate",
      condition_id = NA_integer_,
      batch_id = NA_integer_,
      replicate_id = as.integer(replicate_plate[1L, 2L]),
      plate_id = as.integer(replicate_plate[1L, 3L]),
      plate_code = paste0("P", as.integer(replicate_plate[1L, 3L]))
    ))
  }

  tibble::tibble(
    filename_scheme = "unparsed",
    condition_id = NA_integer_,
    batch_id = NA_integer_,
    replicate_id = NA_integer_,
    plate_id = NA_integer_,
    plate_code = NA_character_
  )
}

#' Read one Incucyte text export
#'
#' The metadata block and table header are detected from file content. The
#' result keeps file-level metadata separate from tidy observations.
#'
#' @param path Path to one tab-delimited Incucyte export.
#' @param source_file Provenance label stored in the output. Defaults to the
#'   basename and should usually be relative to the raw-data directory.
#' @param decimal_mark One of "auto", ",", or ".".
#' @param timezone Time zone used to parse the Date Time column.
#' @param datetime_format Optional base-R date format. Dot-separated day-first,
#'   ISO year-first, and unambiguous slash-separated dates are detected
#'   automatically.
#' @return A list with `file_metadata` and `observations` tibbles.
read_incucyte_file <- function(
    path,
    source_file = basename(path),
    decimal_mark = "auto",
    timezone = "UTC",
    datetime_format = NULL) {
  check_incucyte_dependencies()

  if (!file.exists(path) || isTRUE(file.info(path)$size == 0L)) {
    stop("The input file is missing or empty: ", path, call. = FALSE)
  }
  if (!decimal_mark %in% c("auto", ",", ".")) {
    stop("decimal_mark must be one of 'auto', ',', or '.'.", call. = FALSE)
  }

  lines <- readr::read_lines(path, progress = FALSE)
  lines <- sub("^\ufeff", "", lines)
  header_row <- which(
    stringr::str_detect(
      lines,
      stringr::regex("^\\s*Date Time\\tElapsed(?:\\t|$)", ignore_case = TRUE)
    )
  )[1L]

  if (is.na(header_row)) {
    stop(
      "No Incucyte table header ('Date Time<TAB>Elapsed') was found in '",
      source_file, "'.",
      call. = FALSE
    )
  }

  metadata_lines <- if (header_row > 1L) lines[seq_len(header_row - 1L)] else character()
  metadata_match <- stringr::str_match(
    metadata_lines,
    "^\\s*([^:\\t]+):\\s*(.*?)\\s*$"
  )
  keep <- !is.na(metadata_match[, 2L])
  metadata <- stats::setNames(metadata_match[keep, 3L], metadata_match[keep, 2L])

  vessel_name <- .metadata_value(metadata, "Vessel Name")
  metric <- .metadata_value(metadata, "Metric")

  if (is.na(vessel_name)) {
    stop("Missing 'Vessel Name' metadata in '", source_file, "'.", call. = FALSE)
  }
  if (is.na(metric)) {
    stop("Missing 'Metric' metadata in '", source_file, "'.", call. = FALSE)
  }

  wide <- readr::read_tsv(
    path,
    skip = header_row - 1L,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "NaN", "N/A"),
    trim_ws = TRUE,
    name_repair = "minimal",
    progress = FALSE,
    show_col_types = FALSE
  )

  if (ncol(wide) < 3L) {
    stop(
      "Expected Date Time, Elapsed, and at least one measurement column in '",
      source_file, "'.",
      call. = FALSE
    )
  }

  datetime_text <- wide[[1L]]
  elapsed_text <- wide[[2L]]
  measurement_names <- names(wide)[-(1:2)]

  if (any(is.na(measurement_names) | !nzchar(trimws(measurement_names)))) {
    stop("One or more measurement columns are unnamed in '", source_file, "'.", call. = FALSE)
  }

  series_keys <- sprintf(".series_%04d", seq_along(measurement_names))
  statistic_suffix <- stringr::regex(
    paste0(
      "\\s*\\(",
      "((?:(?:Std|Standard)\\s+Dev(?:iation)?|SD)",
      "(?:\\s+(?:Well|Image|Field))?|Mean)",
      "\\)\\s*$"
    ),
    ignore_case = TRUE
  )
  statistic_match <- stringr::str_match(
    measurement_names,
    statistic_suffix
  )
  statistic_label <- statistic_match[, 2L]
  statistic <- ifelse(
    !is.na(statistic_label) &
      stringr::str_detect(
        statistic_label,
        stringr::regex("^(Std|Standard|SD)", ignore_case = TRUE)
      ),
    "sd",
    "value"
  )
  sample <- trimws(
    stringr::str_remove(
      measurement_names,
      statistic_suffix
    )
  )
  if (any(!nzchar(sample))) {
    stop("A measurement column has no sample name in '", source_file, "'.", call. = FALSE)
  }

  series_map <- tibble::tibble(
    series_key = series_keys,
    source_column = measurement_names,
    sample = sample,
    statistic = statistic
  ) |>
    dplyr::group_by(sample, statistic) |>
    dplyr::mutate(series_index = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(sample) |>
    dplyr::mutate(max_series = max(series_index)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      series_id = dplyr::if_else(
        max_series > 1L,
        paste0(sample, "__", series_index),
        sample
      )
    ) |>
    dplyr::select(-max_series)

  names(wide)[-(1:2)] <- series_keys
  numeric_candidates <- unlist(wide[-1L], use.names = FALSE)
  detected_decimal_mark <- if (identical(decimal_mark, "auto")) {
    .detect_decimal_mark(numeric_candidates, source_file)
  } else {
    decimal_mark
  }

  resolved_datetime_format <- if (is.null(datetime_format)) {
    .detect_export_datetime_format(datetime_text, source_file)
  } else {
    datetime_format
  }
  datetime <- .parse_export_datetime(
    datetime_text,
    timezone = timezone,
    datetime_format = resolved_datetime_format,
    source_file = source_file
  )
  elapsed_hours <- .parse_export_number(
    elapsed_text,
    decimal_mark = detected_decimal_mark,
    field = "Elapsed values",
    source_file = source_file
  )
  if (any(is.na(datetime))) {
    stop("Date Time contains missing values in '", source_file, "'.", call. = FALSE)
  }
  if (any(is.na(elapsed_hours) | !is.finite(elapsed_hours))) {
    stop("Elapsed contains missing or non-finite values in '", source_file, "'.", call. = FALSE)
  }

  value_long <- dplyr::bind_cols(
    tibble::tibble(datetime = datetime, elapsed_hours = elapsed_hours),
    wide[-(1:2)]
  ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(series_keys),
      names_to = "series_key",
      values_to = "value_text"
    ) |>
    dplyr::left_join(series_map, by = "series_key") |>
    dplyr::mutate(
      parsed_value = .parse_export_number(
        value_text,
        decimal_mark = detected_decimal_mark,
        field = "measurement values",
        source_file = source_file
      )
    )

  observation_keys <- c("datetime", "elapsed_hours", "sample", "series_index", "series_id")
  means <- value_long |>
    dplyr::filter(statistic == "value") |>
    dplyr::select(dplyr::all_of(observation_keys), value = parsed_value)
  standard_deviations <- value_long |>
    dplyr::filter(statistic == "sd") |>
    dplyr::select(dplyr::all_of(observation_keys), value_sd = parsed_value)

  if (nrow(means) == 0L) {
    stop("No primary measurement columns were found in '", source_file, "'.", call. = FALSE)
  }
  duplicate_means <- means |>
    dplyr::group_by(dplyr::across(dplyr::all_of(observation_keys))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L)
  if (nrow(duplicate_means) > 0L) {
    stop("Duplicate observation keys were found in '", source_file, "'.", call. = FALSE)
  }

  if (nrow(standard_deviations) > 0L) {
    orphan_sd <- dplyr::anti_join(
      standard_deviations,
      means,
      by = observation_keys
    )
    if (nrow(orphan_sd) > 0L) {
      warning(
        "Ignoring ", nrow(orphan_sd), " standard-deviation value(s) without a ",
        "matching primary series in '", source_file, "'.",
        call. = FALSE
      )
    }
    observations <- dplyr::left_join(means, standard_deviations, by = observation_keys)
  } else {
    observations <- dplyr::mutate(means, value_sd = NA_real_)
  }

  observations <- observations |>
    dplyr::mutate(source_file = source_file, .before = 1L) |>
    dplyr::arrange(sample, series_index, elapsed_hours)

  filename_metadata <- parse_incucyte_filename(source_file)
  file_metadata <- dplyr::bind_cols(
    tibble::tibble(
      source_file = source_file,
      vessel_name = vessel_name,
      metric = metric,
      metric_id = normalise_metric_id(metric),
      metric_units = .extract_metric_units(metric),
      analysis_name = .metadata_value(metadata, "Analysis"),
      cell_type = .metadata_value(metadata, "Cell Type"),
      passage = .metadata_value(metadata, "Passage"),
      notes = .metadata_value(metadata, "Notes"),
      header_row = header_row,
      decimal_mark = detected_decimal_mark,
      datetime_format = resolved_datetime_format,
      has_standard_deviation = nrow(standard_deviations) > 0L,
      n_timepoints = dplyr::n_distinct(observations$elapsed_hours),
      n_series = dplyr::n_distinct(observations$series_id),
      time_start_hours = min(observations$elapsed_hours, na.rm = TRUE),
      time_end_hours = max(observations$elapsed_hours, na.rm = TRUE)
    ),
    filename_metadata
  )

  list(file_metadata = file_metadata, observations = observations)
}

#' Read all Incucyte exports in a directory
#'
#' @return An `incucyte_import` list containing one file table and one raw
#'   observation table. Invalid files cause an actionable error; none are
#'   silently skipped.
read_incucyte_dir <- function(
    path,
    recursive = FALSE,
    decimal_mark = "auto",
    timezone = "UTC",
    datetime_format = NULL) {
  check_incucyte_dependencies()

  if (!dir.exists(path)) {
    stop("Input directory does not exist: ", path, call. = FALSE)
  }

  files <- sort(list.files(
    path,
    pattern = "\\.txt$",
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  ))
  if (length(files) == 0L) {
    stop("No .txt exports were found in: ", path, call. = FALSE)
  }

  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  relative_files <- vapply(
    files,
    function(file) {
      full <- normalizePath(file, winslash = "/", mustWork = TRUE)
      if (startsWith(full, paste0(root, "/"))) {
        substring(full, nchar(root) + 2L)
      } else {
        basename(full)
      }
    },
    character(1)
  )

  if (!is.null(datetime_format)) {
    parsed <- purrr::map2(
      files,
      relative_files,
      ~ read_incucyte_file(
        .x,
        source_file = .y,
        decimal_mark = decimal_mark,
        timezone = timezone,
        datetime_format = datetime_format
      )
    )
  } else {
    parsed <- vector("list", length(files))
    ambiguous <- vector("list", length(files))

    for (i in seq_along(files)) {
      result <- tryCatch(
        read_incucyte_file(
          files[[i]],
          source_file = relative_files[[i]],
          decimal_mark = decimal_mark,
          timezone = timezone,
          datetime_format = NULL
        ),
        incucyte_ambiguous_datetime = function(condition) condition
      )

      if (inherits(result, "incucyte_ambiguous_datetime")) {
        ambiguous[[i]] <- result
      } else {
        parsed[[i]] <- result
      }
    }

    resolved <- !vapply(parsed, is.null, logical(1))
    known_formats <- if (any(resolved)) {
      unique(vapply(
        parsed[resolved],
        function(result) result$file_metadata$datetime_format[[1L]],
        character(1)
      ))
    } else {
      character()
    }

    for (i in which(!vapply(ambiguous, is.null, logical(1)))) {
      candidates <- ambiguous[[i]]$candidate_formats
      inferred <- intersect(candidates, known_formats)

      if (length(inferred) != 1L) {
        context <- if (length(inferred) > 1L) {
          "Other exports use conflicting slash-date orders."
        } else {
          "No other export establishes the slash-date order."
        }
        stop(
          conditionMessage(ambiguous[[i]]), " ", context,
          call. = FALSE
        )
      }

      parsed[[i]] <- read_incucyte_file(
        files[[i]],
        source_file = relative_files[[i]],
        decimal_mark = decimal_mark,
        timezone = timezone,
        datetime_format = inferred[[1L]]
      )
    }
  }

  file_metadata <- purrr::map_dfr(parsed, "file_metadata") |>
    dplyr::mutate(file_id = sprintf("F%03d", dplyr::row_number()), .before = 1L)
  file_ids <- file_metadata |>
    dplyr::select(file_id, source_file)
  observations <- purrr::map_dfr(parsed, "observations") |>
    dplyr::left_join(file_ids, by = "source_file") |>
    dplyr::relocate(file_id, .before = source_file)

  structure(
    list(file_metadata = file_metadata, observations = observations),
    class = c("incucyte_import", "list")
  )
}

print.incucyte_import <- function(x, ...) {
  cat(
    "<incucyte_import>\n",
    "  files:        ", nrow(x$file_metadata), "\n",
    "  vessels:      ", dplyr::n_distinct(x$file_metadata$vessel_name), "\n",
    "  metrics:      ", dplyr::n_distinct(x$file_metadata$metric_id), "\n",
    "  observations: ", nrow(x$observations), "\n",
    sep = ""
  )
  invisible(x)
}

read_plate_map <- function(path) {
  check_incucyte_dependencies()
  required <- c("plate_code", "condition", "display_name")

  plate_map <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
  missing <- setdiff(required, names(plate_map))
  if (length(missing) > 0L) {
    stop("plate_map is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(plate_map$plate_code)) {
    stop("plate_code must be unique in the plate map.", call. = FALSE)
  }

  plate_map |>
    dplyr::select(dplyr::all_of(required))
}

.safe_median <- function(x) {
  if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
}

#' Build an analysis-ready measurement table
#'
#' Focus Position exports are identified by their Metric metadata and matched
#' to measurement exports by Vessel Name, sample, series index, and elapsed
#' time. Focus observations become QC columns; they are never removed here.
#'
#' @param imported Result from `read_incucyte_dir()`.
#' @param plate_map Optional result from `read_plate_map()`.
#' @param focus_threshold_um Optional absolute threshold for target-specific
#'   focus-change deviation from the per-scan median. NULL disables flagging.
build_analysis_table <- function(
    imported,
    plate_map = NULL,
    focus_threshold_um = NULL) {
  check_incucyte_dependencies()

  if (!all(c("file_metadata", "observations") %in% names(imported))) {
    stop("imported must be the result of read_incucyte_dir().", call. = FALSE)
  }
  invalid_focus_threshold <- !is.null(focus_threshold_um) && (
    length(focus_threshold_um) != 1L ||
      !is.finite(focus_threshold_um) ||
      focus_threshold_um <= 0
  )
  if (invalid_focus_threshold) {
    stop("focus_threshold_um must be NULL or one positive number.", call. = FALSE)
  }

  joined <- imported$observations |>
    dplyr::left_join(
      imported$file_metadata,
      by = c("file_id", "source_file")
    )

  if (!is.null(plate_map)) {
    joined <- joined |>
      dplyr::left_join(plate_map, by = "plate_code")
  }

  focus <- joined |>
    dplyr::filter(metric_id == "focus_position") |>
    dplyr::select(
      vessel_name,
      sample,
      series_index,
      elapsed_hours,
      focus_source_file = source_file,
      focus_position_um = value,
      focus_position_sd_um = value_sd
    )

  focus_keys <- c("vessel_name", "sample", "series_index", "elapsed_hours")
  if (nrow(focus) > 0L) {
    duplicate_focus <- focus |>
      dplyr::group_by(dplyr::across(dplyr::all_of(focus_keys))) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::filter(n > 1L)
    if (nrow(duplicate_focus) > 0L) {
      stop(
        "Multiple Focus Position values match the same vessel/sample/time. ",
        "Remove duplicate focus exports or split the experiment into separate raw directories.",
        call. = FALSE
      )
    }

    focus <- focus |>
      dplyr::group_by(vessel_name, sample, series_index) |>
      dplyr::arrange(elapsed_hours, .by_group = TRUE) |>
      dplyr::mutate(focus_change_um = focus_position_um - dplyr::lag(focus_position_um)) |>
      dplyr::ungroup() |>
      dplyr::group_by(vessel_name, elapsed_hours) |>
      dplyr::mutate(focus_change_median_um = .safe_median(focus_change_um)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        focus_deviation_um = focus_change_um - focus_change_median_um,
        focus_flag = if (is.null(focus_threshold_um)) {
          FALSE
        } else {
          dplyr::coalesce(abs(focus_deviation_um) >= focus_threshold_um, FALSE)
        }
      )
  } else {
    focus <- tibble::tibble(
      vessel_name = character(),
      sample = character(),
      series_index = integer(),
      elapsed_hours = double(),
      focus_source_file = character(),
      focus_position_um = double(),
      focus_position_sd_um = double(),
      focus_change_um = double(),
      focus_change_median_um = double(),
      focus_deviation_um = double(),
      focus_flag = logical()
    )
  }

  joined |>
    dplyr::filter(metric_id != "focus_position") |>
    dplyr::left_join(focus, by = focus_keys) |>
    dplyr::mutate(focus_flag = dplyr::coalesce(focus_flag, FALSE)) |>
    dplyr::arrange(file_id, sample, series_index, elapsed_hours)
}

read_exclusions <- function(path) {
  check_incucyte_dependencies()
  required <- c(
    "source_file", "vessel_name", "metric_id", "sample",
    "start_hours", "end_hours", "reason"
  )

  exclusions <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
  missing <- setdiff(required, names(exclusions))
  if (length(missing) > 0L) {
    stop("exclusions is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  parse_bound <- function(x, column) {
    parsed <- suppressWarnings(readr::parse_number(x, na = c("", "NA")))
    bad <- !is.na(x) & is.na(parsed)
    if (any(bad)) {
      stop("Could not parse ", column, " in exclusions.csv.", call. = FALSE)
    }
    parsed
  }

  exclusions |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(
      start_hours = parse_bound(start_hours, "start_hours"),
      end_hours = parse_bound(end_hours, "end_hours")
    )
}

#' Add transparent manual-exclusion flags
#'
#' Empty rule fields act as wildcards. Rows are flagged, not deleted.
flag_exclusions <- function(data, exclusions) {
  required_data <- c("source_file", "vessel_name", "metric_id", "sample", "elapsed_hours")
  missing <- setdiff(required_data, names(data))
  if (length(missing) > 0L) {
    stop("data is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  data$excluded <- FALSE
  data$exclusion_reason <- NA_character_
  if (is.null(exclusions) || nrow(exclusions) == 0L) return(data)

  match_or_wildcard <- function(column, value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      rep(TRUE, nrow(data))
    } else {
      !is.na(data[[column]]) & data[[column]] == value
    }
  }

  for (i in seq_len(nrow(exclusions))) {
    rule <- exclusions[i, , drop = FALSE]
    start <- if (is.na(rule$start_hours)) -Inf else rule$start_hours
    end <- if (is.na(rule$end_hours)) Inf else rule$end_hours
    if (start > end) {
      stop("start_hours is greater than end_hours in exclusion rule ", i, ".", call. = FALSE)
    }

    matched <-
      match_or_wildcard("source_file", rule$source_file) &
      match_or_wildcard("vessel_name", rule$vessel_name) &
      match_or_wildcard("metric_id", rule$metric_id) &
      match_or_wildcard("sample", rule$sample) &
      data$elapsed_hours >= start & data$elapsed_hours <= end

    reason <- if (is.na(rule$reason) || !nzchar(trimws(rule$reason))) {
      paste0("Manual exclusion rule ", i)
    } else {
      rule$reason
    }
    existing <- data$exclusion_reason
    data$exclusion_reason[matched] <- ifelse(
      is.na(existing[matched]),
      reason,
      paste(existing[matched], reason, sep = "; ")
    )
    data$excluded[matched] <- TRUE
  }

  data
}

.subtract_first_observation <- function(x) {
  observed <- x[!is.na(x)]
  if (length(observed) == 0L) rep(NA_real_, length(x)) else x - observed[[1L]]
}

#' Add a first-observation baseline-corrected value
baseline_correct_incucyte <- function(
    data,
    group_cols = c("file_id", "sample", "series_index"),
    value_col = "value") {
  required <- c(group_cols, "elapsed_hours", value_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("data is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  value_symbol <- rlang::sym(value_col)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::arrange(elapsed_hours, .by_group = TRUE) |>
    dplyr::mutate(
      value_baseline_corrected = .subtract_first_observation(!!value_symbol)
    ) |>
    dplyr::ungroup()
}

#' Linearly interpolate onto a regular time grid
#'
#' Interpolation is performed only within the observed time range. It does not
#' extrapolate or smooth, and it returns a separate table so raw values remain
#' unchanged. Filter QC and exclusion flags before calling when appropriate.
interpolate_incucyte <- function(
    data,
    interval_hours = 1,
    value_col = "value",
    group_cols = c(
      "file_id", "source_file", "vessel_name", "metric", "metric_id",
      "sample", "series_id", "series_index"
    ),
    tolerance = sqrt(.Machine$double.eps)) {
  if (length(interval_hours) != 1L ||
      !is.finite(interval_hours) ||
      interval_hours <= 0) {
    stop("interval_hours must be one positive number.", call. = FALSE)
  }

  required <- c(group_cols, "elapsed_hours", value_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("data is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  grouped <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::group_split(.keep = TRUE)

  purrr::map_dfr(grouped, function(group) {
    group <- dplyr::ungroup(group)
    keep <- !is.na(group$elapsed_hours) & !is.na(group[[value_col]])
    x <- group$elapsed_hours[keep]
    y <- group[[value_col]][keep]
    order_index <- order(x)
    x <- x[order_index]
    y <- y[order_index]

    group_label <- paste(
      unlist(group[1L, group_cols, drop = FALSE]),
      collapse = " / "
    )
    if (length(x) < 2L) {
      stop(
        "At least two observations are needed to interpolate group: ",
        group_label,
        call. = FALSE
      )
    }
    if (anyDuplicated(x)) {
      stop(
        "Duplicate elapsed times prevent interpolation for group: ",
        group_label,
        call. = FALSE
      )
    }

    grid <- seq(from = min(x), to = max(x), by = interval_hours)
    interpolated <- stats::approx(
      x = x,
      y = y,
      xout = grid,
      method = "linear",
      rule = 1,
      ties = "ordered"
    )$y
    is_observed <- vapply(
      grid,
      function(grid_value) any(abs(x - grid_value) <= tolerance),
      logical(1)
    )

    dplyr::bind_cols(
      group[rep(1L, length(grid)), group_cols, drop = FALSE],
      tibble::tibble(
        elapsed_hours = grid,
        value_interpolated = interpolated,
        is_observed = is_observed,
        source_value_column = value_col
      )
    )
  })
}

#' Plot individual Incucyte time-course curves
#'
#' Each imported series remains a separate curve. Endpoint labels distinguish
#' replicates or source files when the same sample appears more than once.
plot_incucyte_curves <- function(
    data,
    value_col = "value",
    label_endpoints = TRUE,
    show_legend = FALSE,
    title = "Incucyte time course") {
  required <- c(
    "file_id", "elapsed_hours", "sample", "series_index", "metric", value_col
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("data is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  value_symbol <- rlang::sym(value_col)
  finite_values <- is.finite(data$elapsed_hours) & is.finite(data[[value_col]])
  if (!any(finite_values)) {
    stop("data contains no finite time/value pairs to plot.", call. = FALSE)
  }

  plot_data <- data |>
    dplyr::mutate(
      .curve_id = interaction(
        file_id,
        sample,
        series_index,
        drop = TRUE,
        lex.order = TRUE
      )
    )

  source_label <- as.character(plot_data$file_id)
  if ("source_file" %in% names(plot_data)) {
    parsed_source <- tools::file_path_sans_ext(basename(plot_data$source_file))
    use_source <- !is.na(parsed_source) & nzchar(parsed_source)
    source_label[use_source] <- parsed_source[use_source]
  }

  curve_context <- source_label
  if ("replicate_id" %in% names(plot_data)) {
    use_replicate <- !is.na(plot_data$replicate_id)
    curve_context[use_replicate] <- paste0(
      "R",
      plot_data$replicate_id[use_replicate]
    )
  }
  if ("display_name" %in% names(plot_data)) {
    display_name <- as.character(plot_data$display_name)
    use_display <- !is.na(display_name) & nzchar(trimws(display_name))
    if (dplyr::n_distinct(display_name[use_display]) > 1L) {
      curve_context[use_display] <- paste(
        display_name[use_display],
        curve_context[use_display],
        sep = " · "
      )
    }
  }

  plot_data$.curve_context <- curve_context
  plot_data <- plot_data |>
    dplyr::group_by(file_id, metric, sample) |>
    dplyr::mutate(
      .series_count = dplyr::n_distinct(series_index),
      .curve_context = dplyr::if_else(
        .series_count > 1L,
        paste0(.curve_context, " · series ", series_index),
        .curve_context
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(metric, sample) |>
    dplyr::mutate(
      .curve_count = dplyr::n_distinct(.curve_id),
      .curve_label = dplyr::if_else(
        .curve_count > 1L,
        paste(sample, .curve_context, sep = " · "),
        sample
      )
    ) |>
    dplyr::ungroup()

  endpoints <- plot_data |>
    dplyr::filter(is.finite(elapsed_hours), is.finite(!!value_symbol)) |>
    dplyr::group_by(metric, .curve_id) |>
    dplyr::slice_max(elapsed_hours, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  y_label <- if (identical(value_col, "value")) {
    "Value"
  } else {
    tools::toTitleCase(gsub("_", " ", value_col))
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = elapsed_hours,
      y = !!value_symbol,
      group = .curve_id,
      colour = sample
    )
  ) +
    ggplot2::geom_line(linewidth = 0.75, alpha = 0.82, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.35, alpha = 0.9, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(metric), scales = "free_y") +
    ggplot2::scale_colour_viridis_d(option = "D", end = 0.85) +
    ggplot2::labs(
      title = title,
      x = "Elapsed time (hours)",
      y = y_label,
      colour = "Sample",
      caption = "Each line is one imported series."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0),
      legend.position = if (show_legend) "right" else "none"
    )

  if (label_endpoints) {
    x_range <- range(plot_data$elapsed_hours[finite_values], na.rm = TRUE)
    x_nudge <- max(diff(x_range) * 0.025, 0.1)

    plot <- plot +
      ggrepel::geom_text_repel(
        data = endpoints,
        ggplot2::aes(label = .curve_label),
        direction = "y",
        hjust = 0,
        nudge_x = x_nudge,
        min.segment.length = 0,
        segment.alpha = 0.35,
        box.padding = 0.3,
        point.padding = 0.15,
        max.overlaps = 5,
        seed = 42,
        size = 3,
        show.legend = FALSE
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.02, 0.24))
      )
  }

  plot
}
