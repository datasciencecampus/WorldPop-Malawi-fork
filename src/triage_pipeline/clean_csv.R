source("utils.R")
config <- load_config()

ratio_change_data <- read.csv(config$triage_pipeline$ratio_change_csv, stringsAsFactors = FALSE)
growth_factor_data <- read.csv(config$triage_pipeline$hh_estimate_csv, stringsAsFactors = FALSE)

clean_duplicates <- function(df, name) {
  
  # Add a column called ".valid" which shows a count of how many numeric
  # columns have a real, usable value (not NA and not Inf/-Inf)
  df <- df |>
    dplyr::group_by(EA_CODE) |>
    dplyr::mutate(.valid = rowSums(!is.na(dplyr::across(where(is.numeric))) &
                                    is.finite(as.matrix(dplyr::across(where(is.numeric)))))) |>
    dplyr::ungroup()
  
  # Check for ties in duplicate EA_CODE groups - groups where max valid
  # count is shared by more than one row
  ties <- df |>
    dplyr::group_by(EA_CODE) |>
    dplyr::filter(dplyr::n() > 1, .valid == max(.valid)) |>
    dplyr::ungroup()
  
  if (nrow(ties) > 0) {
    # Are tied rows identical?
    identical_ties <- ties |>
      dplyr::group_by(EA_CODE) |>
      dplyr::filter(dplyr::n_distinct(dplyr::across(-c(.valid))) == 1) |>
      dplyr::ungroup()
    
    different_ties <- ties |>
      dplyr::group_by(EA_CODE) |>
      dplyr::filter(dplyr::n_distinct(dplyr::across(-c(.valid))) > 1) |>
      dplyr::ungroup()
    
    if (nrow(different_ties) > 0) {
      message("WARNING: ", dplyr::n_distinct(different_ties$EA_CODE),
              " EA_CODEs in ", name, " have tied rows with DIFFERENT values - manual inspection needed:")
      print(different_ties |> dplyr::select(-.valid) |> dplyr::arrange(EA_CODE))
      stop("Data must be manually inspected and cleaned before continuing.")
    }
    
    if (nrow(identical_ties) > 0) {
      message(dplyr::n_distinct(identical_ties$EA_CODE),
              " EA_CODEs in ", name, " are identical - keeping first row")
    }
  }
  
  # Keep the row with the highest .valid values per EA,
  # if two rows have the same .valid score, just pick the first one
  df |>
    dplyr::group_by(EA_CODE) |>
    dplyr::slice_max(.valid, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-.valid)
}

ratio_change_clean  <- clean_duplicates(ratio_change_data,  "ratio_change_data")
growth_factor_clean <- clean_duplicates(growth_factor_data, "growth_factor_data")

# Result from growth_factor_clean "inspection needed"
# Looking at the rows they look almost identical, decimal points differ
# - will keep first row for now
which(growth_factor_data$EA_CODE == "30399930") # [1]  7117 16176
# - removing row 16176 (duplicate of 7117), keeping first occurrence
growth_factor_data <- growth_factor_data[-16176, ]
growth_factor_clean <- clean_duplicates(growth_factor_data, "growth_factor_data")


# Save clean version alongside original
ratio_change_clean_path  <- sub("\\.csv$", "_clean.csv", config$triage_pipeline$ratio_change_csv)
growth_factor_clean_path <- sub("\\.csv$", "_clean.csv", config$triage_pipeline$hh_estimate_csv)

write.csv(ratio_change_clean,  ratio_change_clean_path,  row.names = FALSE)
write.csv(growth_factor_clean, growth_factor_clean_path, row.names = FALSE)

message("Saved: ", ratio_change_clean_path)
message("Saved: ", growth_factor_clean_path)