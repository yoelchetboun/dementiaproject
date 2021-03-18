library(Rshinytemplate)
library(shinydashboard)
library(glue)
library(shinyjs)
library(ggplot2)
library(shiny)
library(viridis)
library(data.table)
library(dplyr)
library(dygraphs)
library(purrr)
library(grid)
library(gridExtra)
library(shinycssloaders)
library(tidyverse)
library(cowplot)
library(magick)
library(shinyWidgets)
library(tensorflow)
library(keras)

source("ui.R")
options(shiny.maxRequestSize = 30*1024^2)

print("Lancement du Shiny")
print(getwd())

path_data <- "/srv/OASIS_DATA/data_base_shiny/"
path_root <- "~/GENERIC/dementiaproject/"
path_user_base  <- file.path(path_root, "user_base.rds")

diag_data <- dementiaproject::loadRData(file=file.path(path_root, "//inst/extdata/diag_data_finale.Rdata"))

#
# base_patient <- data.table(id = c(0, 1),
#                            first_name = c("Nouveaux","James"),
#                            last_name = c("Patient", "Bond"),
#                            age_at_diagnosis = c(NA,30),
#                            date_entry = c(NA, as.character(Sys.Date())),
#                            genre = c(NA,"Homme"),
#                            size = c(NA, 1.80),
#                            weight = c(NA, 80),
#                            agit = c(NA,TRUE),
#                            depress = c(NA,FALSE),
#                            anxiety = c(NA,TRUE),
#                            apathy = c(NA,FALSE),
#                            disinhib = c(NA,TRUE),
#                            irr = c(NA,TRUE),
#                            bills = c(NA,0),
#                            taxes = c(NA,0),
#                            shopping = c(NA,0),
#                            games = c(NA,0),
#                            meal = c(NA,0),
#                            event = c(NA,0),
#                            concentration = c(NA,0),
#                            remdates = c(NA,0),
#                            travel = c(NA,0),
#                            independ = c(NA,1))
#
#
# saveRDS(base_patient, file = file.path(path_data, "base_patient.rds"))

#
# dt <- dementiaproject::loadRData("/srv/OASIS_DATA/data_processed_more_cut/dataset.Rdata")
# dt[new_name %in% c("mri_picture_27720.png", "mri_picture_27719.png",
#                    "mri_picture_00006.png", "mri_picture_00009.png", "mri_picture_00017.png",
#                    "mri_picture_25079.png", "mri_picture_25080.png", "mri_picture_25110.png")]

# dimensions of our images.

