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

duration_by_module |>
	reactable::reactable()

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

# ==============================================================================
# module pauses or long durations
# ==============================================================================


# ==============================================================================
# question length
# ==============================================================================

# ==============================================================================
# question answer changes
# ==============================================================================
