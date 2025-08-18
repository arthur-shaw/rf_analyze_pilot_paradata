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

# clean up environment
rm(qnr_df)

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
paradata_processed <- susopara::calc_time_btw_active_events(
  dt = paradata_processed
)

# clean up environment, removing large and unused objects
rm(paradata_raw)


# ------------------------------------------------------------------------------
# Amplified paradata
# ------------------------------------------------------------------------------

paradata_w_section <- paradata_processed |>
	tidytable::left_join(variables_by_section, by = "variable")

# clean up environment
rm(paradata_processed)

readr::write_tsv(
  x = paradata_w_section,
  file = here::here("data", "03_created", "paradata_w_section.tsv")
)
