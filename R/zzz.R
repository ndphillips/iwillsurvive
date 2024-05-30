.onLoad <- function(libname = find.package("patientjourney"), # nolint
                    pkgname = "patientjourney") {
  utils::globalVariables(
    c(
      "."
    )
  )
}

#' Title
#'
#' @param version package version number
#'
#' @import tibble
#' @import dplyr
#'
get_codename <- function(version) {
  codename_tbl <- tibble::tribble(
    ~version, ~codename,
    "0.1", "Gloria",
    "0.2", "Stu",
    "0.3", "TBD",
    "0.4", "TBD"
  )

  version_minor <- substr(utils::packageVersion("iwillsurvive"), 1, 3)

  codename_tbl %>%
    dplyr::filter(version == version_minor) %>%
    dplyr::pull(codename)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("-----------------------------------------------------")
  packageStartupMessage(paste0(
    "iwillsurvive ",
    utils::packageVersion("iwillsurvive"),
    " '",
    get_codename(utils::packageVersion("iwillsurvive")),
    "'"
  ))
  packageStartupMessage("Intro  : vignette('introduction', 'iwillsurvive')")
  packageStartupMessage("Repo   : https://github.com/ndphillips/iwillsurvive")
  packageStartupMessage(".....................................................")
}
