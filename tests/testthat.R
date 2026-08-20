source("R/incucyte_tools.R", local = FALSE)
check_incucyte_dependencies()

testthat::test_dir("tests/testthat", reporter = "summary")