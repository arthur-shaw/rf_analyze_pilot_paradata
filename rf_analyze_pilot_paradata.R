# ==============================================================================
# interview length
# ==============================================================================

# ==============================================================================
# module length
# ==============================================================================

# ------------------------------------------------------------------------------
# overall
# ------------------------------------------------------------------------------

duration_by_module <- paradata_w_section |>
  # remove empty section
  tidytable::filter(!is.na(section)) |>
  # compute total by interview
  tidytable::group_by(interview__id, section) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
  tidytable::ungroup() |>
  # compute statistics by section
  tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	# remove module prefilled questions
  dplyr::filter(section != "Cover") |>
	# assign module numbers to facilitate sorting
  dplyr::mutate(
    section_num = dplyr::case_when(
      section == "Cover" ~ 1,
      section == "START" ~ 2,
      section == "SECTION 1: HOUSEHOLD ROSTER" ~ 3,
      section == "SECTION 2: EDUCATION" ~ 4,
      section == "SECTION 3A: HEALTH" ~ 5,
      section == "SECTION 3B: CHILD ANTHROPOMETRY" ~ 6,
      section == "SECTION 3C: FUNCTIONING" ~ 7,
      section == "SECTION 4: LABOR" ~ 8,
      section == "SECTION 5: DIGITAL TECH" ~ 9,
      section == "SECTION 6: FINANCIAL SERVICES" ~ 10,
      section == "SECTION 7: FOOD AWAY FROM HOME" ~ 11,
      section == "SECTION 8: FOOD CONSUMPTION" ~ 12,
      section == "Section 9A: Non-Food Expenditures, 7-Day Recall Period" ~ 13,
      section == "Section 9B: Non-Food Expenditures, 30-Day Recall Period" ~ 14,
      section == "Section 9C: NON-FOOD EXPENDITURES-CLOTHING AND FOOT WEAR - 3 MONTH RECALL PERIOD" ~ 15,
      section == "Section 9D: Non-Food Expenditures, 12-Month Recall Period" ~ 16,
      section == "SECTION 10A: Food Consumption Score (FCS)" ~ 17,
      section == "SECTION 10B: FOOD (FIES)" ~ 18,
      section == "SECTION 11: HOUSING" ~ 19,
      section == "SECTION 12: ENERGY" ~ 20,
      section == "SECTION 13: WASH" ~ 21,
      section == "SECTION 14: DURABLE GOODS" ~ 22,
      section == "SECTION 15A: HOUSEHOLD BUSINESS I" ~ 23,
      section == "SECTION 15B: HOUSEHOLD BUSINESS II" ~ 24,
      section == "SECTION 16A: SHOCKS & COPING I" ~ 25,
      section == "SECTION 16B: SHOCKS & COPING II" ~ 26,
      section == "SECTION 17: ACCESS TO WEATHER INFORMATION" ~ 27,
      section == "SECTION 18: OTHER HOUSEHOLD INCOME" ~ 28,
      section == "SECTION 19: SOCIAL ASSISTANCE AND SOCIAL PROTECTION" ~ 29,
      section == "SECTION 20: AGRICULTURE FILTERS" ~ 30,
      section == "SECTION AG1. CROP FARMING PRACTICES" ~ 31,
      section == "SECTION AG2.A CROP PRODUCTION" ~ 32,
      section == "SECTION AG2.B CROP LABOR - MAIN SEASON" ~ 33,
      section == "SECTION AG3.A CROP PRODUCTION - PERMANENT" ~ 34,
      section == "SECTION AG3.B CROP LABOR - PERMANENT"                                             ~ 35,
      section == "SECTION AG5. CROP INPUTS" ~ 36,
      section == "SECTION AG6A. LIVESTOCK OWNERSHIP" ~ 37,
      section == "SECTION AG6B. LIVESTOCK COSTS" ~ 38,
      section == "SECTION AG6C LIVESTOCK PRODUCTS" ~ 39,
      section == "SECTION - AG6D LIVESTOCK LABOR" ~ 40,
      section == "SECTION AG7A: FISHING & AQUACULTURE PRODUCTION" ~ 41,
      section == "SECTION AG7B: FISHERIES & AQUACULTURE LABOR" ~ 42,
      section == "SECTION AG8: AGRICULTURE EQUIPMENT & ASSETS" ~ 43,
      section == "SECTION AG9. AGRO-PROCESSING & FORESTRY" ~ 44,
      section == "SECTION AG10. OTHER PRODUCTION COSTS" ~ 45,
      section == "[SP1] EARLY CHILDHOOD DEVELOPMENT" ~ 46,
      section == "[SP2] EDUCATION DIGITIAL SKILLS" ~ 47,
      section == "[SP3] CHILDCARE" ~ 48,
      section == "[SP4] MENTAL HEALTH" ~ 49,
      section == "[SP5] JOB HISTORY" ~ 50,
      section == "[SP6] CASUAL LABOUR" ~ 51,
      section == "[SP9] MIGRATION IN THE LAST 12 MONTHS" ~ 52,
      section == "[SP10] MIGRATION - FORMER HOUSEHOLD MEMBERS" ~ 53,
      section == "[SP11] MIGRATION ASPIRATIONS" ~ 54,
      section == "[S12] REMITTANCES" ~ 55,
      section == "[S13] CHRONIC DISEASES" ~ 56,
      section == "SECTION 22: CONTACT" ~ 57,
      .default = 99
    )
  ) |>
	dplyr::arrange(section_num) |>
	dplyr::select(-section_num)

saveRDS(
  object = duration_by_module,
  file = here::here("data", "03_created", "duration_by_module.rds")
)

# ------------------------------------------------------------------------------
# person-level
# ------------------------------------------------------------------------------

person_sections <- c(
  "SECTION 1: HOUSEHOLD ROSTER",
  "SECTION 2: EDUCATION",
  "SECTION 3A: HEALTH",
  "SECTION 3B: CHILD ANTHROPOMETRY",
  "SECTION 3C: FUNCTIONING",
  "SECTION 4: LABOR",
  "SECTION 5: DIGITAL TECH",
  "SECTION 6: FINANCIAL SERVICES",
  "SECTION 7: FOOD AWAY FROM HOME"
)

duration_by_person_module <- paradata_w_section |>
  # filter to person-level sections
  tidytable::filter(section %in% person_sections) |>
  # remove questions not administered to individual people
  tidytable::filter(!is.na(row)) |>
  # transform rows of nested rosters so that they are counted
  # as rows of their parent roster
  # because education and health modules contain nested rosters
  tidytable::mutate(
    row = gsub(
      x = row,
      pattern = ",.+$",
      replacement = ""
    )
  ) |>
  # compute total by row
  tidytable::group_by(interview__id, section, row) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
  tidytable::ungroup() |>
  # compute statistics by section
  tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup()

# ------------------------------------------------------------------------------
# item-level
# ------------------------------------------------------------------------------

item_sections <- c(
  "SECTION 8: FOOD CONSUMPTION",
  "Section 9A: Non-Food Expenditures, 7-Day Recall Period",
  "Section 9B: Non-Food Expenditures, 30-Day Recall Period",
  "Section 9C: NON-FOOD EXPENDITURES-CLOTHING AND FOOT WEAR - 3 MONTH RECALL PERIOD",
  "Section 9D: Non-Food Expenditures, 12-Month Recall Period",
  "SECTION 10A: Food Consumption Score (FCS)",
  "SECTION 14: DURABLE GOODS",
  "SECTION 15A: HOUSEHOLD BUSINESS I",
  "SECTION 15B: HOUSEHOLD BUSINESS II",
  "SECTION 16A: SHOCKS & COPING I",
  "SECTION 16B: SHOCKS & COPING II"
)

duration_by_item <- paradata_w_section |>
  # filter to person-level sections
  tidytable::filter(section %in% item_sections) |>
  # remove questions not administered about items
  tidytable::filter(!is.na(row)) |>
  # transform rows of nested rosters so that they are counted
  # as rows of their parent roster
  # because education and health modules contain nested rosters
  tidytable::mutate(
    row = gsub(
      x = row,
      pattern = ",.+$",
      replacement = ""
    )
  ) |>
  # compute total by row
  tidytable::group_by(interview__id, section, row) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
  tidytable::ungroup() |>
  # compute statistics by section
  tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup()

# ------------------------------------------------------------------------------
# question-level
# ------------------------------------------------------------------------------

duration_by_question <- paradata_w_durations |>
  # remove entries that are not variables
  tidytable::filter(!is.na(variable)) |>
	tidytable::filter(event != "Completed") |>
  # compute statistics by question
  tidytable::group_by(variable) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
  # join section attribute to facilitate filtering
  dplyr::left_join(
    variables_by_section,
    by = "variable"
  ) |>
	dplyr::relocate(section, .before = variable) |>
	# sort in descending order of median duration
  dplyr::arrange(dplyr::desc(med))

# ==============================================================================
# focus on modules with longer lengths
# ==============================================================================

# ------------------------------------------------------------------------------
# education
# ------------------------------------------------------------------------------

duration_for_educ <- paradata_w_section |>
  # filter to education
  tidytable::filter(section == "SECTION 2: EDUCATION") |>
  # remove timestamp questions since they don't rightly belong to either section
  tidytable::filter(!variable %in% c("s2_StartTime", "s2_EndTime")) |>
  # split section into two parts
  tidytable::mutate(
    sub_section = tidytable::case_when(
      grepl(
        x = variable,
        pattern = "^s2q([0-9]$|[0-9]_oth|1[0-7]|1[0-7]_oth)"
      ) ~ "Non-expendtiure",
      .default = "Expenditure"
    )
  ) |>
  # compute sum by interview
  tidytable::group_by(interview__id, sub_section) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
	# compute stats by sub-section
  tidytable::group_by(sub_section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	dplyr::mutate(
    sub_section_order = dplyr::case_when(
      sub_section == "Non-expendtiure" ~ 1,
      sub_section == "Exenditure" ~ 2
    )
  ) |>
	dplyr::arrange(sub_section_order) |>
	dplyr::select(-sub_section_order)

# ------------------------------------------------------------------------------
# labor
# ------------------------------------------------------------------------------

duration_for_labor <- paradata_w_section |>
  # filter to labor
  tidytable::filter(section == "SECTION 4: LABOR") |>
  # split section into two parts
  tidytable::mutate(
    sub_section = tidytable::case_when(
      # through q29
      # main job
      # everything else
      grepl(x = variable, pattern = "^s4q([1-9]|1[0-9]|2[0-9])(_oth)?$") ~
        "Questions 1-29",
      grepl(x = variable, pattern = "^s4q(3[0-9]|4[0-9]|50a)(_oth)?") ~
        "Main job",
      .default = "Everyting else"
    )
  ) |>
  # compute sum by interview
  tidytable::group_by(interview__id, sub_section) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
	# compute stats by sub-section
  tidytable::group_by(sub_section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	dplyr::mutate(
    sub_section_order = dplyr::case_when(
      sub_section == "Questions 1-29" ~ 1, 
      sub_section == "Main job" ~ 2,
      sub_section == "Everyting else" ~ 3,
      .default = NA_integer_
    )
  ) |>
	dplyr::arrange(sub_section_order) |>
	dplyr::select(-sub_section_order)


# ------------------------------------------------------------------------------
# shocks
# ------------------------------------------------------------------------------

duration_for_shocks <- paradata_w_section |>
  # filter to labor
  tidytable::filter(
    section == "SECTION 16A: SHOCKS & COPING I" |
    section == "SECTION 16B: SHOCKS & COPING II"
  ) |>
  # split section into two parts
  tidytable::mutate(
    sub_section = tidytable::case_when(
      # through q2
      variable %in% c("s16a_StartTime", "s16aq0", "s16aq1", "s16aq2") ~
        "Questions from start through q2",
      .default = "Everyting else"
    )
  ) |>
  # compute sum by interview
  tidytable::group_by(interview__id, sub_section) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
	# compute stats by sub-section
  tidytable::group_by(sub_section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	dplyr::mutate(
    sub_section_order = dplyr::case_when(
      sub_section == "Questions from start through q2" ~ 1, 
      sub_section == "Everyting else" ~ 2,
      .default = NA_integer_
    )
  ) |>
	dplyr::arrange(sub_section_order) |>
	dplyr::select(-sub_section_order)

# ==============================================================================
# pauses
# ==============================================================================

paradata_w_pauses_and_sections <- paradata_processed |>
	tidytable::left_join(
    variables_by_section,
    by = "variable"
  )

# number of pauses
# pauses_by_section_med <- paradata_w_pauses_and_sections |>
# 	tidytable::fill(section, .direction = "updown") |>
#   # compute number of pauses per interview-section
#   tidytable::mutate(num_pauses = event == "Paused") |>
#   tidytable::group_by(interview__id, section) |>
#   tidytable::summarise(
#     num_pauses = sum(num_pauses, na.rm = TRUE)
#   ) |>
#   tidytable::ungroup() |>
# 	# compute stats by section
#   tidytable::group_by(section) |>
#   tidytable::summarise(
#     med = median(x = num_pauses, na.rm = TRUE),
#     sd = sd(x = num_pauses, na.rm = TRUE),
#     min = min(num_pauses, na.rm = TRUE),
#     max = max(num_pauses, na.rm = TRUE),
#     n_obs = tidytable::n()
#   ) |>
#   tidytable::ungroup() |>
# 	dplyr::arrange(dplyr::desc(med), dplyr::desc(max))

pauses_by_section_count <- paradata_w_pauses_and_sections |>
	tidytable::fill(section, .direction = "updown") |>
  # compute number of pauses per interview-section
  tidytable::mutate(n_pauses = event == "Paused") |>
  tidytable::group_by(section) |>
  tidytable::summarise(
    n_pauses = sum(n_pauses, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
  dplyr::mutate(pct_pauses = n_pauses/n_obs) |>
	dplyr::arrange(dplyr::desc(n_pauses)) |>
	dplyr::select(section, n_obs, n_pauses, pct_pauses)

# # "nearby" questions
# pause_leads <- paradata_w_pauses_and_sections |>
# 	tidytable::fill(variable, .direction = "down") |>
#   tidytable::mutate(num_pauses = event == "Paused") |>
#   tidytable::filter(num_pauses == TRUE) |>
# 	# compute stats by paused variable
#   tidytable::group_by(variable) |>
#   tidytable::summarise(
#     med = median(x = num_pauses, na.rm = TRUE),
#     sd = sd(x = num_pauses, na.rm = TRUE),
#     min = min(num_pauses, na.rm = TRUE),
#     max = max(num_pauses, na.rm = TRUE),
#     n_obs = tidytable::n()
#   ) |>
#   tidytable::ungroup() |>
# 	dplyr::arrange(dplyr::desc(med))

# ==============================================================================
# answer changes
# ==============================================================================

answer_changes_by_section <- paradata_w_section |>
  tidytable::mutate(n_answer_changes = event == "AnswerRemoved") |>
  tidytable::group_by(interview__id, section) |>
	tidytable::summarise(n_answer_changes = sum(n_answer_changes, na.rm = TRUE)) |>
	tidytable::ungroup() |>
	# compute stats by section
  tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = n_answer_changes, na.rm = TRUE),
    sd = sd(x = n_answer_changes, na.rm = TRUE),
    min = min(n_answer_changes, na.rm = TRUE),
    max = max(n_answer_changes, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	dplyr::arrange(dplyr::desc(med), dplyr::desc(max))

answer_changes_by_question <- paradata_w_section |>
  # remove entries that are not variables
  tidytable::filter(!is.na(variable)) |>
	tidytable::filter(event != "Completed") |>
  # create answer change flag
  tidytable::mutate(n_answer_changes = event == "AnswerRemoved") |>
	# compute stats by question
  tidytable::group_by(variable) |>
  tidytable::summarise(
    n_answer_changes = sum(n_answer_changes, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
  dplyr::mutate(pct_answer_changed = n_answer_changes / n_obs) |>
  dplyr::select(variable, n_obs, n_answer_changes, pct_answer_changed) |>
	dplyr::arrange(dplyr::desc(n_answer_changes))
