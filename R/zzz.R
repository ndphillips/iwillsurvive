
.onLoad <- function(libname = find.package("patientjourney"), # nolint
                    pkgname = "patientjourney") {
  utils::globalVariables(
    c(
      "."
    )
  )
}

get_codename <- function(version) {
  codename_tbl <- tibble::tribble(
    ~version, ~codename,
    "0.1", "Gloria",
    "0.2", "TBD",
    "0.3", "TBD",
    "0.4", "TBD"
  )

  version_minor <- substr(utils::packageVersion("iwillsurvive"), 1, 3)

  codename_tbl %>%
    dplyr::filter(version == version_minor) %>%
    dplyr::pull(codename)
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("-----------------------------------------------------------------------")
  packageStartupMessage(paste0(
    "iwillsurvive ",
    utils::packageVersion("iwillsurvive"),
    " '",
    get_codename(utils::packageVersion("iwillsurvive")),
    "'"
  ))
  packageStartupMessage("Intro   : vignette('introduction', 'iwillsurvive')")
  packageStartupMessage("Repo    : https://git.the.flatiron.com/qs_r_packages/iwillsurvive")
  packageStartupMessage("Contact : #TBD")
  packageStartupMessage(".......................................................................")
}
