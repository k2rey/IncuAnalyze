# Data dictionary

The importer stores file-level metadata once and links it to observations with
`file_id`. Analysis and interpolation tables are derived outputs; they do not
replace the raw import tables.

## `file_metadata`

| Column | Type | Purpose |
| --- | --- | --- |
| `file_id` | character | Sequential key assigned after sorting relative file paths; starts at `F001` for each directory import. |
| `source_file` | character | Relative input path for provenance; no absolute personal path is stored. |
| `vessel_name` | character | Exact `Vessel Name` from the export and the main key for matching metrics from one vessel. |
| `metric` | character | Exact Incucyte `Metric` string. |
| `metric_id` | character | Normalized lowercase identifier for filtering and joins. |
| `metric_units` | character | Terminal parenthetical unit text when present; otherwise missing. |
| `analysis_name` | character | Exact analysis-definition name when included in the export. |
| `cell_type`, `passage`, `notes` | character | Optional export metadata retained for provenance. |
| `header_row` | integer | Detected one-based table-header row. |
| `decimal_mark` | character | Decimal mark detected or explicitly selected for the file. |
| `datetime_format` | character | Date-time format detected or explicitly supplied for the file. |
| `has_standard_deviation` | logical | Whether at least one matching standard-deviation series was imported. |
| `n_timepoints`, `n_series` | integer | Import summary counts. |
| `time_start_hours`, `time_end_hours` | double | Observed elapsed-time range. |
| `filename_scheme` | character | Recognized optional naming convention or `unparsed`. |
| `condition_id`, `batch_id`, `replicate_id`, `plate_id` | integer | Optional identifiers parsed from a recognized filename. |
| `plate_code` | character | Optional plate identifier such as `P1`, used to join the plate map. |

## `raw_observations`

| Column | Type | Purpose |
| --- | --- | --- |
| `file_id`, `source_file` | character | Link and provenance fields. |
| `datetime` | date-time | Parsed acquisition time in the selected time zone. |
| `elapsed_hours` | double | Primary time axis from the export. |
| `sample` | character | Exact measurement-column label; neutral across genes, wells, constructs, and conditions. |
| `series_index` | integer | Occurrence number when an export contains repeated sample labels. |
| `series_id` | character | Unique sample/occurrence identifier within a file. |
| `value` | double | Parsed primary measurement; never corrected, smoothed, averaged, or interpolated in this table. |
| `value_sd` | double | Matching standard deviation when exported with a recognized suffix; otherwise missing. |

## `analysis_data`

This table joins non-focus observations to `file_metadata`. When a plate map is
supplied, it also contains `condition` and `display_name` plus the QC fields
below.

| Column | Purpose |
| --- | --- |
| `focus_source_file` | Focus export that supplied the matched value. |
| `focus_position_um`, `focus_position_sd_um` | Matched focus measurement and optional standard deviation. |
| `focus_change_um` | Scan-to-scan change for the same sample series. |
| `focus_change_median_um` | Median change across the vessel at the same scan. |
| `focus_deviation_um` | Sample change minus the vessel median change. |
| `focus_flag` | Optional threshold result; `FALSE` when disabled or unavailable. |
| `excluded` | Whether at least one manual-exclusion rule matched the row. |
| `exclusion_reason` | Reasons from matching rule(s), joined with semicolons. |
| `value_baseline_corrected` | Added only when baseline correction is requested. |

## `interpolated_data`

| Column | Purpose |
| --- | --- |
| Group/provenance columns | Identify the source curve; defaults include file, vessel, metric, sample, and series fields. |
| `elapsed_hours` | Requested regular grid within the observed range. |
| `value_interpolated` | Piecewise-linear value on that grid. |
| `is_observed` | `TRUE` when the grid time coincides with an original observation. |
| `source_value_column` | Records which value column was interpolated. |
