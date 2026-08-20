# IncuAnalyze

[![R tests](https://github.com/k2rey/IncuAnalyze/actions/workflows/test.yml/badge.svg)](https://github.com/k2rey/IncuAnalyze/actions/workflows/test.yml)

IncuAnalyze is a compact R workflow for importing, checking, transforming, and
plotting tab-delimited Incucyte time-course exports. It reads experiment details
from each file, keeps measured values unchanged, and records quality-control
decisions.

This repository contains synthetic example data only. Files placed in
`data/raw/` and results written to `output/` are excluded from version control.

## What it does

1. Detects the table header instead of assuming a fixed metadata-block length.
2. Reads `Vessel Name`, `Metric`, `Analysis`, and other available metadata from
   each export.
3. Converts wide export tables to tidy observations while preserving the source
   file, repeated series, and optional standard deviations.
4. Matches `Focus Position` exports by vessel, sample, series, and elapsed time.
5. Adds focus and manual-exclusion flags without deleting observations.
6. Optionally adds baseline-corrected values and creates a separate regular
   interpolation grid.
7. Plots individual curves without averaging replicates and can label endpoints
   with `ggrepel`.

## Requirements

- R 4.1 or newer
- `dplyr`, `ggplot2`, `ggrepel`, `purrr`, `readr`, `rlang`, `stringr`,
  `tibble`, and `tidyr`
- `testthat` for the automated tests

Install the dependencies once:

```r
install.packages(c(
  "dplyr", "ggplot2", "ggrepel", "purrr", "readr", "rlang",
  "stringr", "tibble", "tidyr", "testthat"
))
```

RStudio is optional. Opening `IncuAnalyze.Rproj` sets sensible project options,
but every command also works from a terminal.

## Quick start

From the repository root, run:

```sh
Rscript analysis/run_pipeline.R
```

The script uses `data/example/` by default, so a fresh clone runs without extra
files. To analyse your own exports:

1. Place unmodified `.txt` exports in `data/raw/`.
2. Set `raw_dir <- "data/raw"` near the top of `analysis/run_pipeline.R`.
3. Review the remaining settings and run the script again.

Results are written to `output/`.

## Import model

`read_incucyte_dir()` returns an `incucyte_import` object with two tables:

- `file_metadata`: one row per export, including the exact metric and optional
  fields parsed from the filename.
- `observations`: one row per file, sample series, and time point. `value`
  remains measured data; it is never silently corrected, smoothed, or averaged.

`build_analysis_table()` joins those tables, converts focus measurements to QC
columns, and optionally adds labels from `config/plate_map.csv`.

```r
source("R/incucyte_tools.R")

imported <- read_incucyte_dir("data/example")
plate_map <- read_plate_map("config/plate_map.csv")
analysis_data <- build_analysis_table(imported, plate_map = plate_map)

plot_incucyte_curves(analysis_data)
```

## Optional filename metadata

File content is always the source of truth. Any filename is accepted when the
export contains the required metadata and a `Date Time`, `Elapsed` table header.

These filename patterns add optional design fields:

- `C3_B2_R1_P4.txt` -> condition 3, batch 2, replicate 1, plate 4
- `P4_1.txt` -> plate 4, replicate 1
- `replicate1_plate4.txt` -> replicate 1, plate 4

The `C`, `B`, `R`, and `P` parts mean condition, batch, replicate, and plate.
Unrecognized filenames still import normally. A collision suffix such as `(1)`
is ignored for filename parsing only.

Files receive sequential `file_id` values (`F001`, `F002`, ...) after sorting by
their relative paths. The IDs link import tables within a run; `source_file`
retains the readable provenance.

## Plotting replicates

`plot_incucyte_curves()` groups by file, sample, and series index. Curves with
the same sample name are therefore shown individually rather than summarized.
When several curves share a sample name, endpoint labels include a replicate or
source-file identifier. Set `label_endpoints = FALSE` to remove labels or
`show_legend = TRUE` to display the colour legend.

The overview caption states explicitly that no averaging is applied.

## Focus quality control

Focus exports remain identifiable as the metric `Focus Position` in the raw
import. In `analysis_data`, they become:

- `focus_position_um`
- `focus_change_um`
- `focus_change_median_um`
- `focus_deviation_um`
- `focus_flag`

The deviation compares each sample's scan-to-scan focus change with the median
change across the vessel at the same scan. No universal threshold is assumed.
The default is `NULL`; set `focus_threshold_um` only after validation for the
instrument, acquisition settings, and assay.

## Baseline correction and interpolation

`baseline_correct_incucyte()` adds `value_baseline_corrected` while retaining
`value`. Use it only when subtracting the first observation is scientifically
appropriate for the selected metric.

`interpolate_incucyte()` performs piecewise-linear interpolation within each
curve's observed time range. It does not extrapolate or fit a biological model.
The result is a separate table with `value_interpolated` and `is_observed`.

## Configuration

`config/plate_map.csv` maps a parsed `plate_code` to optional labels:

| Column | Meaning |
| --- | --- |
| `plate_code` | Parsed code such as `P1`; values must be unique. |
| `condition` | Short machine-readable condition name. |
| `display_name` | Human-readable label used in plots and downstream analysis. |

`config/exclusions.csv` contains auditable exclusion rules. A rule can match
`source_file`, `vessel_name`, `metric_id`, and/or `sample`, plus an inclusive
time range. Empty match fields are wildcards. Rules set `excluded` and
`exclusion_reason`; they never remove rows.

```csv
source_file,vessel_name,metric_id,sample,start_hours,end_hours,reason
C1_B1_R1_P1.txt,,,Sample B,24,24,Documented acquisition issue
```

## Outputs

| File | Contents |
| --- | --- |
| `file_metadata.csv` | One row per imported export and its import diagnostics. |
| `raw_observations.csv` | Parsed observations with file and series provenance. |
| `analysis_data.csv` | Measurement rows with metadata, labels, and QC flags. |
| `interpolated_data.csv` | Optional regular time grid; created only when enabled. |
| `incucyte_import.rds` | Typed R representation of the raw import tables. |
| `curve_overview.pdf`, `curve_overview.png` | Vector and preview versions of the endpoint-labelled plot. |

Column definitions are documented in `docs/data_dictionary.md`.

## Repository layout

| Path | Purpose |
| --- | --- |
| `R/incucyte_tools.R` | Reusable import, QC, transformation, and plotting functions. |
| `analysis/run_pipeline.R` | Configurable end-to-end workflow. |
| `config/` | Plate labels and manual-exclusion rules. |
| `data/example/` | Small synthetic Incucyte-style exports. |
| `data/raw/` | Local research exports; contents are ignored by Git. |
| `docs/` | Output-column documentation. |
| `tests/` | Automated regression tests. |
| `output/` | Generated results; contents are ignored by Git. |
| `.github/workflows/` | GitHub Actions configuration for automatic tests. |

## Tests

Run the complete test suite from the repository root:

```sh
Rscript tests/testthat.R
```

The tests cover filename parsing, import structure, focus matching, exclusions,
baseline correction, interpolation, and replicate-aware plotting. GitHub Actions
runs the same command after every push and pull request.

## Scope

Incucyte export layouts can vary between software versions and analysis
definitions. The importer reports malformed or ambiguous inputs instead of
silently skipping them. Add a synthetic fixture and a test before supporting a
new export layout.

This project is not affiliated with or endorsed by Sartorius. Released under
the MIT License.
