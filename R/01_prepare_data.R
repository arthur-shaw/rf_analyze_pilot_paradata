# ------------------------------------------------------------------------------
# Metadata
# ------------------------------------------------------------------------------

# set path to the JSON representation of the questionnaire
json_file_path <- fs::path(here::here("data", "02_metadata", "document.json"))

# ingest
qnr_df <- susometa::parse_questionnaire(path = json_file_path)

# create section-variable table
variables_by_section <- qnr_df |>
  susometa::get_questions_by_section() |>
  tidytable::select(section, variable) |>
  data.table::data.table()

# computed variables
names_computed_variables <- susometa::get_variables(qnr_df = qnr_df) |>
	dplyr::pull(varname)

# questionnaire variable name
qnr_var_name <- jqr::jq(
  x = base::file(description = json_file_path),
  ". | .VariableName"
) |>
  # convert from JSON object to character
	jsonlite::fromJSON()

# clean up environment
rm(qnr_df)

# ------------------------------------------------------------------------------
# Microdata
# ------------------------------------------------------------------------------

# get path(s) to main data frame
main_qnr_data_paths <- here::here("data", "03_microdata", "raw") |>
  fs::dir_info(
    type = "file",
    recurse = TRUE
  ) |>
  dplyr::mutate(file_name = fs::path_file(path)) |>
	dplyr::filter(
    grepl(
      x = file_name,
      pattern = paste0(qnr_var_name, ".dta"),
      fixed = TRUE
    )
  ) |>
	dplyr::pull(path)

# collect all data
main_qnr_df <- main_qnr_data_paths |>
  purrr::map(.f = ~ haven::read_dta(.x)) |>
	purrr::list_rbind()

# get IDs of completed interviews
completed_interview_ids <- main_qnr_df |>
	dplyr::filter(consent == 1) |>
	dplyr::pull(interview__id)

# ------------------------------------------------------------------------------
# Paradata
# ------------------------------------------------------------------------------

# get path of all paradata files
paradata_file_paths <- fs::dir_ls(
  path = here::here("data", "01_paradata"),
  type = "file",
  regexp = "paradata.tab",
  recurse = TRUE
)

# combine raw paradata files
paradata_raw <- tidytable::map_dfr(
  .x = paradata_file_paths,
  .f = ~ data.table::fread(.x)
)

# parse the file into a form needed for analysis
paradata_processed <- susopara::parse_paradata(dt = paradata_raw) |>
	# remove "passive" events of variables being computed
  tidytable::filter(!variable %in% names_computed_variables) |>
	# remove passive events currently not removed by `susopara`
  tidytable::filter(
    !event %in%
    c("InterviewCreated", "InterviewModeChanged", "InterviewerAssigned")
  )

# compute time between events
paradata_w_durations <- susopara::calc_time_btw_active_events(
  dt = paradata_processed
) |>
  # select completed interviews
  tidytable::filter(
    interview__id %in% completed_interview_ids
  ) |>
	# remove extreme durations
  tidytable::filter(
    # less than 99th percentile of duration
    (elapsed_min < quantile(x = elapsed_min, probs = 0.99, na.rm = TRUE)) &
    # non-negative
    (elapsed_min > 0)
  )

# clean up environment, removing large and unused objects
rm(paradata_raw)


# ------------------------------------------------------------------------------
# Amplified paradata
# ------------------------------------------------------------------------------

paradata_w_section <- paradata_w_durations |>
	tidytable::left_join(variables_by_section, by = "variable") |>
	tidytable::filter(!is.na(section))

readr::write_tsv(
  x = paradata_w_section,
  file = here::here("data", "03_created", "paradata_w_section.tsv")
)
