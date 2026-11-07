# Clean the spreadsheet-style schedule into tidy data -----------------------
#
# The planning sheet is laid out for people, not for analysis:
#   row 1        column titles
#   row 2        region numbers (1-10) above the state columns
#   category     a row whose first cell is a heading, not a course
#   course       course code (e.g. "COM-101") + status, then states under regions
#   continuation blank first cell, more states for the course above
#   TOTAL        everything after this row is summary / footnotes
#
# read_schedule() returns one row per course x state with a delivery count.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

COURSE_PATTERN <- "^[A-Z]{3}-[0-9]{3}"

read_schedule <- function(path, state_regions) {
  raw <- read_csv(path, col_names = FALSE, col_types = cols(.default = col_character()),
                  na = c("", "NA"), show_col_types = FALSE)

  # Region columns are the ones labelled 1-10 in the second header row
  region_row  <- unlist(raw[2, ], use.names = FALSE)
  region_cols <- which(region_row %in% as.character(1:10))
  if (length(region_cols) != 10) stop("Expected 10 region columns, found ", length(region_cols))

  body <- raw[-(1:2), ]
  total_at <- which(body[[1]] == "TOTAL")
  if (length(total_at) > 0) body <- body[seq_len(total_at[1] - 1), ]

  names(body)[1:2] <- c("label", "status")
  names(body)[region_cols] <- paste0("region_", region_row[region_cols])

  is_course <- str_detect(coalesce(body$label, ""), COURSE_PATTERN)

  tidy <- body |>
    mutate(
      category = if_else(!is.na(label) & !is_course, label, NA_character_),
      course   = if_else(is_course, label, NA_character_)
    ) |>
    fill(category, .direction = "down") |>
    filter(is.na(label) | is_course) |>            # drop category heading rows
    fill(course, status, .direction = "down") |>   # continuation rows inherit the course
    filter(!is.na(course)) |>
    select(category, course, status, starts_with("region_")) |>
    pivot_longer(starts_with("region_"), names_to = "region", names_prefix = "region_",
                 values_to = "state_abbr", values_drop_na = TRUE) |>
    mutate(region = as.integer(region), state_abbr = str_trim(state_abbr)) |>
    count(category, course, status, region, state_abbr, name = "n_trainings") |>
    mutate(
      course_code = str_extract(course, COURSE_PATTERN)
    )

  # Data-quality check: every state must exist and sit under its own region
  check <- tidy |> left_join(state_regions, by = "state_abbr", suffix = c("", "_lookup"))
  bad <- check |> filter(is.na(region_lookup) | region != region_lookup)
  if (nrow(bad) > 0) {
    warning("Dropping ", nrow(bad), " rows with unknown states or a mismatched region: ",
            paste(unique(bad$state_abbr), collapse = ", "))
  }

  check |>
    filter(!is.na(region_lookup), region == region_lookup) |>
    select(category, course, course_code, status, region, state_abbr,
           state_name, n_trainings)
}

# Courses with no deliveries yet (e.g. "In development") still belong in the
# course list, so read the catalogue separately.
read_course_catalogue <- function(path) {
  raw <- read_csv(path, col_names = FALSE, col_types = cols(.default = col_character()),
                  na = c("", "NA"), show_col_types = FALSE)
  raw[-(1:2), 1:2] |>
    setNames(c("label", "status")) |>
    mutate(category = if_else(!str_detect(coalesce(label, ""), COURSE_PATTERN), label, NA)) |>
    fill(category, .direction = "down") |>
    filter(str_detect(coalesce(label, ""), COURSE_PATTERN)) |>
    transmute(category, course = label, status)
}
