setwd("~/Documents/R/bsem")

devtools::load_all()
devtools::document()
devtools::install()
# devtools::install(quick=TRUE)

usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("codecov.yml")
usethis::use_build_ignore(".travis.yml")
usethis::use_build_ignore("_config.yml")
usethis::use_build_ignore("cran-comments.md")
usethis::use_build_ignore("inst/dev.R")
usethis::use_build_ignore("inst/load.R")

# devtools::build_vignettes()
devtools::check()
devtools::check_man()
devtools::missing_s3()
devtools::release_checks()
# devtools::check_win_devel()

# usethis::use_revdep()
# devtools::revdep()

devtools::spell_check()
devtools::test()

# devtools::reload()
t1 <- Sys.time(); devtools::run_examples();
t2 <- Sys.time(); t2-t1
devtools::release()
devtools::build_manual()
covr::codecov(token = "7ab5fe7f-f16a-49fb-8175-b7bc5e5e64b8")
# devtools::submit_cran()
