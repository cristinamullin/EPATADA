# download individual data profiles from WQX

# construct the WQP web service URL for each profile
  baseurl <- "https://www.waterqualitydata.us"
  profile_station <- "/data/Station"
  profile_result <- "/data/Result"
  profile_result_2 <- "&dataProfile=resultPhysChem"
  profile_project <- "/data/Project"
  filters <- "/search?statecode=US%3A17&characteristicType=Nutrient"
  dates <- "&startDateLo=04-01-2023&startDateHi=11-01-2023"
  type <- "&mimeType=csv&zip=yes"
  providers <- "&providers=NWIS&providers=STEWARDS&providers=STORET"

  stationProfile <- TADA_ReadWQPWebServices(
    paste0(baseurl, profile_station, filters, dates, type, providers)
  )

  physchemProfile <- TADA_ReadWQPWebServices(
    paste0(baseurl, profile_result, filters, dates, type, profile_result_2, providers)
  )

  projectProfile <- TADA_ReadWQPWebServices(
    paste0(baseurl, profile_project, filters, dates, type, providers)
  )

  # Join all three profiles using TADA_JoinWQPProfiles
  TADAProfile <- TADA_JoinWQPProfiles(
    FullPhysChem = physchemProfile,
    Sites = stationProfile,
    Projects = projectProfile
  )


tada.names <- names(TADAProfile)

physchem.names <- names(physchemProfile)

project.names <-  names(projectProfile)

station.names <- names(stationProfile)

wqp.names <- unique(c(physchem.names,
                    project.names,
                    station.names))

check <- intersect(tada.names, wqp.names)

length(check) == length(tada.names)

# in this section use TADA functions to make changes
qaqc_TADAProfile <- TADAProfile |>
  TADA_AutoClean()

autoclean.names <- names(qaqc_TADAProfile)

# helper function to select which columns to retain
prefer_TADA <- function(df, cols, prefix = "TADA.", rename_to_base = FALSE, warn = TRUE) {
  cn <- names(df)
  pref <- paste0(prefix, cols)

  # Choose TADA_<base> if present; otherwise choose <base>
  chosen <- ifelse(pref %in% cn, pref, cols)

  # Keep only those that actually exist
  exists <- chosen %in% cn
  if (warn && any(!exists)) {
    warning("Not found (neither prefixed nor base): ",
            paste(cols[!exists], collapse = ", "))
  }

  chosen_kept <- chosen[exists]
  base_kept   <- cols[exists]

  if (inherits(df, "data.table")) {
    # data.table branch: use .. to evaluate the character vector in j
    out <- df[, ..chosen_kept]
    if (rename_to_base) {
      data.table::setnames(out, old = chosen_kept, new = base_kept)
    }
    return(out)
  } else {
    # data.frame / tibble branch
    out <- df[, chosen_kept, drop = FALSE]
    if (rename_to_base) {
      names(out) <- base_kept
    }
    return(out)
  }
}

df <- data.frame(
  id = 1:3,
  age = c(20, 30, 40),
  TADA.age = c(21, 31, 41),
  height = c(170, 180, 190),
  TADA.height = c(171, 181, 191),
  weight = c(60, 70, 80)
)

cols <- c("age", "height", "weight")

# Call the function to get a result printed in the console
prefer_TADA(df, cols)
# or assign and then print:
res <- prefer_TADA(df, cols, rename_to_base = TRUE)
print(res)


qaqc_physchemProfile <- qaqc_TADAProfile |> prefer_TADA(cols = physchem.names)

qaqc_projectProfile <-TADAProfile |>
  dplyr::select(dplyr::all_of(project.names))

qaqc_stationProfile <- TADAProfile |>
  dplyr::select(dplyr::all_of(station.names))
