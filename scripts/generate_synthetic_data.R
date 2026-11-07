# Generate the synthetic training schedule ----------------------------------
#
# The original 2023 dashboard read a shared planning spreadsheet. That data is
# not public, so this script creates a synthetic stand-in with the SAME messy
# spreadsheet layout the cleaning code was written for:
#
#   * two header rows (column titles, then region numbers 1-10)
#   * category rows (e.g. "Track A") with no course code
#   * one course per block; extra deliveries spill onto continuation rows
#     where the course name is blank
#   * states listed by two-letter code under their region's column
#   * a TOTAL row followed by free-text footnotes
#
# Courses are identified by code only (COM-101 ...). All statuses, dates and
# notes are invented.
#
# Output: data/training_schedule_raw.csv
# Run from the project root:  Rscript scripts/generate_synthetic_data.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

set.seed(2023)

regions <- read_csv("data/state_regions.csv", show_col_types = FALSE)

courses <- tribble(
  ~category,  ~course,   ~status,          ~deliveries,
  "Track A",  "COM-101", "Completed",      14,
  "Track A",  "COM-102", "Scheduled",      9,
  "Track A",  "COM-103", "Approved",       6,
  "Track B",  "COM-104", "Scheduled",      12,
  "Track B",  "COM-105", "Completed",      8,
  "Track B",  "COM-106", "Approved",       5,
  "Track C",  "COM-107", "Scheduled",      10,
  "Track C",  "COM-108", "Completed",      7,
  "Track C",  "COM-109", "In development", 0,
  "Track D",  "COM-110", "Scheduled",      11,
  "Track D",  "COM-111", "Approved",       4,
  "Track D",  "COM-112", "In development", 0
)

end_dates <- c("Through 3/15/2024", "Through 5/31/2024", "Through 10/15/2024", "Through 1/31/2025")
notes_pool <- c(
  "Bilingual facilitator preferred",
  "Can be combined with a tabletop exercise",
  "Pair with a follow-up workshop where possible",
  "Host provides a projector and seating for 30",
  NA, NA, NA
)

n_regions <- 10
blank_row <- function() rep(NA_character_, 4 + n_regions + 4)

rows <- list(
  c("Training", "Status", "Delivery Window", "Delivery Notes", "Distribution by Region",
    rep(NA, n_regions - 1), "Remaining", "Focus Hosts*", "Waitlist**", "Additional Notes"),
  c(NA, NA, NA, NA, as.character(1:n_regions), "REMAINING", NA, NA, NA)
)

for (cat_name in unique(courses$category)) {
  r <- blank_row(); r[1] <- cat_name
  rows[[length(rows) + 1]] <- r

  for (i in which(courses$category == cat_name)) {
    crs <- courses[i, ]

    # Sample host states (a state can host more than one delivery)
    hosts <- regions |>
      slice_sample(n = crs$deliveries, replace = TRUE) |>
      arrange(region)

    # Lay the states out under their region column, spilling onto extra rows
    by_region <- split(hosts$state_abbr, hosts$region)
    n_rows <- max(1, lengths(by_region))
    block <- replicate(n_rows, blank_row(), simplify = FALSE)

    for (reg in names(by_region)) {
      for (k in seq_along(by_region[[reg]])) {
        block[[k]][4 + as.integer(reg)] <- by_region[[reg]][k]
      }
    }

    first <- block[[1]]
    first[1] <- crs$course
    first[2] <- crs$status
    first[3] <- if (crs$status == "In development") "TBD" else sample(end_dates, 1)
    first[4] <- if (crs$status == "Scheduled") paste("Need", sample(2:4, 1), "more hosts") else NA
    first[4 + n_regions + 1] <- as.character(if (crs$status == "Completed") 0 else sample(1:5, 1))
    first[4 + n_regions + 2] <- as.character(sample(0:3, 1))
    first[4 + n_regions + 3] <- as.character(sample(0:2, 1))
    first[4 + n_regions + 4] <- sample(notes_pool, 1)
    block[[1]] <- first

    rows <- c(rows, block)
  }
}

total <- blank_row(); total[1] <- "TOTAL"; total[5] <- as.character(sum(courses$deliveries))
foot1 <- blank_row(); foot1[1] <- "*Number of hosts in the focus list for this course"
foot2 <- blank_row(); foot2[1] <- "**Hosts waiting for an open session"
rows <- c(rows, list(total, blank_row(), foot1, foot2))

sheet <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
write_csv(sheet, "data/training_schedule_raw.csv", na = "", col_names = FALSE)

message("Wrote ", nrow(sheet), " rows (", sum(courses$deliveries), " deliveries across ",
        nrow(courses), " courses).")
