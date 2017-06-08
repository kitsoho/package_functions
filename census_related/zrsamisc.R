# devtools::use_testthat()
# devtools::test()
# devtools::use_package("rvest")
# devtools::use_package("RpostgreSQL")
# devtools::use_readme_rmd()
# install_github("zevross-spatial/rpackage-zrsamisc", auth_token = "9ab2d1625f8b2fbf4bf5321c563a34e867c37599")
# get_table_insert_pg("2016", "CBSA", reload = TRUE)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("A package to with miscellaneous functions from ZevRoss Spatial Analysis")
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Your name goes here",
    devtools.desc.author = '"First Last <first.last@example.com> [aut, cre]"',
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  get_connection()
  invisible()
}
