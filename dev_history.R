usethis::use_package('shiny')
usethis::use_package('shinydashboard')
usethis::use_package('Rshinytemplate')
usethis::use_package('pbapply')
usethis::use_package('EBImage')
usethis::use_package('purrr')
usethis::use_package('stats')
usethis::use_package('OpenImageR')


usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("Rprofile.site")

install.packages("/CM/DEV/ARCHIVE_LIVRABLES/RTE_DRONES/Rshinytemplate_1.0.2.tar.gz", type = "source", repo = NULL)

usethis::use_package('dplyr')
usethis::use_package('data.table')
usethis::use_package('magrittr')
usethis::use_pipe(export = TRUE)
usethis::use_package('measurements')
usethis::use_package('tidync')
