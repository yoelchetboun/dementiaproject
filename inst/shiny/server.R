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


source("ui.R")

options(shiny.maxRequestSize = 30*1024^2)

print("Lancement du Shiny")
print(getwd())

#path_root <- "/root/"
path_data <- "/srv/OASIS_DATA/data_base_shiny/"



path_work <- "~/GENERIC/dementiaproject/inst/extdata/shiny_data"
path_root <- "~/GENERIC/dementiaproject/"
path_user_base  <- file.path(path_root, "user_base.rds")


base_patient <- data.table(id = c(0, 1),
                           first_name = c("Nouveaux","Bond"),
                           last_name = c("Patient", "James"),
                           age_at_diagnosis = c(NA,30),
                           date_entry = c(NA, as.character(Sys.Date())),
                           genre = c(NA,"Homme"),
                           size = c(NA, 1.80),
                           weight = c(NA, 80),
                           agit = c(NA,TRUE),
                           depress = c(NA,FALSE),
                           anxiety = c(NA,TRUE),
                           apathy = c(NA,FALSE),
                           disinhib = c(NA,TRUE),
                           irr = c(NA,TRUE),
                           bills = c(NA,0),
                           taxes = c(NA,0),
                           shopping = c(NA,0),
                           games = c(NA,0),
                           meal = c(NA,0),
                           event = c(NA,0),
                           concentration = c(NA,0),
                           remdates = c(NA,0),
                           travel = c(NA,0),
                           independ = c(NA,1))


saveRDS(base_patient, file = file.path(path_data, "base_patient.rds"))



server = (function(input, output, session) {

  filePath = path_user_base
  shinyjs::hide("cal")


  credentials <- callModule(Rshinytemplate::login, "login",
                            filePath = filePath,
                            user_col = user,
                            pwd_col = password,
                            hashed = TRUE,
                            algo = "md5",
                            log_out = reactive(logout_init()))

  logout_init <- callModule(Rshinytemplate::logout, "logout", reactive(credentials()$user_auth))

  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")

    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })


  user_info <- reactive({ credentials()$info})

  output$welcome <- renderText({
    req(credentials()$user_auth)
    glue("Welcome back {user_info()$name}!")

  })


  #switch entre page d'authentification et le shiny Irma
  observe({
    if (credentials()$user_auth == TRUE) {
      output$page <- renderUI({
        print(paste0("Connexion de ", user_info()$name))
        ui2()})
      updateTabItems(session, "tabs", "patient")
      updateTabItems(session, "tabs", "Admin")
      updateTabItems(session, "tabs", "patient")

    } else {
      output$page <- renderUI({
      })
    }
  })

  ### UI admin ###

  output$admin = renderUI({
    req(user_info()$permissions)
    user_auth <- user_info()$permissions
    user_name <- user_info()$name
    user <- user_info()$user


    if (user_auth == "administrator") {
      fluidPage(
        fluidRow(
          column(width = 4,
                 box(title = "New user creation", status = "danger", solidHeader = TRUE, width = NULL, height = 450,
                     #uiOutput("list_user"),
                     textInput("user", "New user ID", ""),
                     textInput("name", "First name / Last name new user", ""),
                     selectInput("permissions", "New user permissions", c("administrator", "standard")),
                     passwordInput("password", "Initial password", ""),
                     actionButton("create_user", "Create"),
                     h5(" "),
                     htmlOutput("show_confirmation_creation")
                 )

          ),

          column(width = 4,
                 box(width = NULL,
                     title = "Users list", status = "danger", solidHeader = TRUE, height = 450,
                     dataTableOutput("user_plot")
                 )
          ),
          column(width = 4,
                 box(title = "User deletion", status = "danger", solidHeader = TRUE,  width = NULL, height = 450,
                     uiOutput("list_user"),
                     actionButton("erase_user", "Delete"),
                     #HTML(paste(textOutput("show_confirmation"), tags$span(style="color:white"))),

                     htmltools::span(textOutput("show_confirmation"), style="color:white"),

                     conditionalPanel(
                       condition = "output.show_confirmation == '1'",
                       h5("Do you really want to delete this user?"),
                       actionButton("erase_user_confirmation_yes", "Yes"),
                       actionButton("erase_user_confirmation_no", "No")
                     )
                 ))





        ),

        fluidRow(
          box(
            title = "User password change", status = "primary", solidHeader = TRUE,
            passwordInput("old_password", "Old password", ""),
            passwordInput("new_password1", "New password", ""),
            passwordInput("new_password2", "New password", ""),
            actionButton("change_password", "Change"),
            h5(" "),
            htmlOutput("change_password_message")

          )
        ),
        fluidRow(
        ),
      )

    } else {

      # if permissions == standard
      fluidRow(

        box(
          title = "User password change", status = "warning", solidHeader = TRUE,
          passwordInput("old_password", "Old password", ""),
          passwordInput("new_password1", "New password", ""),
          passwordInput("new_password2", "New password", ""),
          actionButton("change_password", "Change"),
          h5(" "),
          htmlOutput("change_password_message")

        )
      )
    }

  })


  user_maj <- function() {
    user_base <- readRDS(path_user_base)
    user_base <- user_base[, .(user, name, permissions)]
    return(user_base)
  }

  #initialisation
  output$user_plot <- renderDataTable(user_maj(), options = list(searching = FALSE, paging = FALSE))

  output$list_user <- renderUI({
    user_base <- user_maj()
    users <- user_base[name != user_info()$name]$name
    selectInput('name_to_erase', NULL, users, multiple = FALSE)
  })

  output$show_confirmation <- renderText('0')

  observeEvent(input$erase_user, {
    output$show_confirmation <- renderText('1')

  })

  observeEvent(input$erase_user_confirmation_no, {
    output$show_confirmation <- renderText('0')
  })

  observeEvent(input$erase_user_confirmation_yes, {
    user_to_erase <- user_maj()[name == input$name_to_erase]$user


    if (!is.null(user_to_erase) | user_to_erase != "") {

      user2erase <- user_to_erase
      user_erase <- function(path_user_base, user_to_erase)
      {
        # browser()
        # user <- NULL
        user_base <- base::readRDS(path_user_base)
        if (user_to_erase %in% user_base$user) {
          user_base <- user_base[user != user_to_erase]
          saveRDS(user_base, path_user_base)
        }
      }

      user_erase(path_user_base = filePath,  user_to_erase = user2erase)

      output$user_plot <- renderDataTable(user_maj(), options = list(searching = FALSE, paging = FALSE))

      output$list_user <- renderUI({
        user_base <- user_maj()
        users <- user_base[name != user_info()$name]$name
        selectInput('name_to_erase', NULL, users, multiple = FALSE)
      })

      output$show_confirmation <- renderText('0')

    }
  })

  observeEvent(input$create_user, {

    message_password <- check_password(input$password)
    if (message_password == 0) {
      message <- user_new(path_user_base = filePath, user = input$user, password = input$password, permissions = input$permissions, name = input$name)
      #maj
      output$user_plot <- renderDataTable(user_maj(), options = list(searching = FALSE, paging = FALSE))
      output$list_user <- renderUI({
        user_base <- user_maj()
        users <- user_base[name != user_info()$name]$name
        selectInput('name_to_erase', NULL, users, multiple = FALSE)    })

    } else {
      message <- message_password
    }

    output$show_confirmation_creation <- renderText({ message })

  })

  observeEvent(input$change_password, {

    message_password <- check_password(input$new_password1)

    if (message_password == 0) {
      message <- user_modify_password(path_user_base = filePath, user_to_modify = user_info()$user, old_password = input$old_password, new_password1 = input$new_password1, new_password2 = input$new_password2)
    } else {
      message <- message_password

    }

    output$change_password_message <- renderText({ message })

  })























  #### ICI début du shiny ######


  #Reactives

  input_file_rv <- reactiveValues(data_path = NULL, nb_coupe =NULL)

  #on lit régulierement le fichier base patient pour voir s'il y a des modifs
  base_patient <- reactiveFileReader(1000,
                                     session,
                                     file.path(path_data, "base_patient.rds"),
                                     readRDS)



  #render UI

  observe({
    output$select_patient = renderUI({
      base_patient <- base_patient()
      list_input <- base_patient$id
      names(list_input) <- paste0(base_patient$first_name, " " ,base_patient$last_name)
      selectizeInput("select_patient", "",
                     choices = list_input, selected = "0", multiple = FALSE)
    })
  })


  observeEvent(input$click_patient  , {

    base_patient <- base_patient()
    selected_patient <- base_patient[input$select_patient == id]
    liste_choix <- c("Non", "Difficultés mais a réussi", "A eu besoin d'une aide", "Dépendant d'une tierce personne", "Ne sait pas")


    if (input$select_patient == "0") {
      #on veut ajouter un nouveau patient


      output$info_patient = renderUI({
        useSweetAlert()
        box(
          width = 12,
          status = "primary",  solidHeader = TRUE,
          title = "Nouvelle fiche patient",
          fluidRow(
            column(width = 1, h1(), h1(), tags$b(paste0("ID #", nrow(base_patient)))),
            column(width = 1, h1(), h1(),  prettyToggle( inputId = "genre",  label_on = "Homme",  label_off = "Femme" )),
            column(width = 3, offset = 0, label = "", textInput(inputId = "last_name", label = "", value = "Nom")),
            column(width = 3, textInput(inputId = "first_name", label = "", value = "Prénom")),

            column(width = 4, dateInput("date_admin", label = "", value = Sys.Date()))),
          fluidRow(
            column(width = 4, sliderInput("age_at_diagnosis", label = "Age :", min = 0, max = 120, value = 70, step = 1)),
            column(width = 4, sliderInput("size", label = "Taille [m] :", min = 0.5, max = 2.5, value = 1.75, step = 0.01)),
            column(width = 4, sliderInput("weight", label = "Poids [kg] :", min = 0, max = 200, value = 70, step = 1))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Entêtement : "), tags$i("Le patient s'entête-t-il et refuse-t-il l'aide des autres ?")),
            column(width = 2, prettySwitch(inputId = "agit", label = "Non/Oui", status = "success", fill = TRUE))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Dépression : "), tags$i("Le patient connait-il des épisodes de dépression ou de dysphorie ?")),
            column(width = 2, prettySwitch(inputId = "depress", label = "Non/Oui", status = "success", fill = TRUE))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Anxiété : "), tags$i("Le patient est-il anxieux ?")),
            column(width = 2, prettySwitch(inputId = "anxiety", label = "Non/Oui", status = "success", fill = TRUE))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Apathie : "), tags$i("Le patient est-il apathique ?")),
            column(width = 2, prettySwitch(inputId = "apathy", label = "Non/Oui", status = "success", fill = TRUE))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Désinhibé : "), tags$i("Le patient est-il désinhibé ?")),
            column(width = 2, prettySwitch(inputId = "disinhib", label = "Non/Oui", status = "success", fill = TRUE))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Irritable : "), tags$i("Le patient est-il irritable ?")),
            column(width = 2, prettySwitch(inputId = "irr", label = "Non/Oui", status = "success", fill = TRUE))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Argent : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés ou besoin d'aide pour écrire un chèque, payer avec des billets... ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "bills", label = NULL, choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,  status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Factures : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour gérer ses papiers, payer ses factures, etc... ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "taxes",  label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,  status = "danger",  fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Shopping : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour faire ses courses ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "shopping", label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,  status = "danger",  fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Jeu : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il rencontré des difficultés pour jouer à un jeu de réflexion (bridge, échecs..) ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "games", label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Repas : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés lors de la préparation d'un repas équilibré ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "meal", label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                                       choiceNames = liste_choix,
                                       inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),

          fluidRow(
            column(width = 12,
                   tags$b("Evénements : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se rappeler d'événements courants ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "event",  label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Concentration : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se concentrer et à comprendre un programme TV, un livre ou un magazine ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "concentration",  label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Souvenir dates : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "remdates",   label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,  status = "danger",  fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Déplacements : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se déplacer en dehors de son quartier, à conduire, ou à prendre les transports en commun ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "travel", label = NULL,choiceValues = c(0, 1, 2, 3, 8),
                     choiceNames = liste_choix,
                     inline = TRUE,  status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Autonomie : "), tags$i("Quel est le niveau d'indépendance du patient ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "independ", label = NULL,choiceValues = c(1, 2, 3, 4, 9),
                     choiceNames = c("En capacité de vivre seul", "Requiert une assistance pour des activités particulièrement complexes", "Requiert une assistace pour des activités quotidiennes", "Dépendant", "Ne sait pas"),
                     inline = FALSE,  status = "danger", fill = TRUE
                   ))
          ),

          fluidRow(
            column(width = 12, h4(),
                   div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("click_add_patient", "Ajouter"))))
        )
      })

    } else {
      if (selected_patient$genre == "Homme") {
        genre_toggle <- TRUE }
      else {
        genre_toggle <- FALSE}

      #on veut simplement visualiser la fiche patient ou la modifier
      output$info_patient = renderUI({
        box(
          width = 12,
          status = "primary",  solidHeader = TRUE,
          title = "Fiche patient",
          fluidRow(
            column(width = 1, h1(), h1(),  tags$b(paste0("ID #", selected_patient$id ))),
            column(width = 1, h1(), h1(),  prettyToggle( inputId = "genre_modif",  label_on = "Homme",  label_off = "Femme" , value = genre_toggle)),
            column(width = 3, offset = 0, label = "", textInput(inputId = "last_name_modif", label = "", value = selected_patient$last_name)),
            column(width = 3, textInput(inputId = "first_name_modif", label = "", value = selected_patient$first_name)),

            column(width = 4, dateInput("date_admin_modif", label = "", value = as.Date(selected_patient$date_entry)))),
          fluidRow(
            column(width = 4, sliderInput("age_at_diagnosis_modif", label = "Age :", min = 0, max = 120, value = selected_patient$age_at_diagnosis, step = 1)),
            column(width = 4, sliderInput("size_modif", label = "Taille [m] :", min = 0.5, max = 2.5, value = selected_patient$size, step = 0.01)),
            column(width = 4, sliderInput("weight_modif", label = "Poids [kg] :", min = 0, max = 200, value = selected_patient$weight, step = 1))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Entêtement : "), tags$i("Le patient s'entête-t-il et refuse-t-il l'aide des autres ?")),
            column(width = 2, prettySwitch(inputId = "agit_modif", label = "Non/Oui", status = "success", fill = TRUE, value = selected_patient$agit))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Dépression : "), tags$i("Le patient connait-il des épisodes de dépression ou de dysphorie ?")),
            column(width = 2, prettySwitch(inputId = "depress_modif", label = "Non/Oui", status = "success", fill = TRUE, value = selected_patient$depress))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Anxiété : "), tags$i("Le patient est-il anxieux ?")),
            column(width = 2, prettySwitch(inputId = "anxiety_modif", label = "Non/Oui", status = "success", fill = TRUE, value = selected_patient$anxiety))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Apathie : "), tags$i("Le patient est-il apathique ?")),
            column(width = 2, prettySwitch(inputId = "apathy_modif", label = "Non/Oui", status = "success", fill = TRUE, value = selected_patient$apathy))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Désinhibé : "), tags$i("Le patient est-il désinhibé ?")),
            column(width = 2, prettySwitch(inputId = "disinhib_modif", label = "Non/Oui", status = "success", fill = TRUE, value = selected_patient$disinhib))
          ),
          fluidRow(
            column(width = 10,
                   tags$b("Irritable : "), tags$i("Le patient est-il irritable ?")),
            column(width = 2, prettySwitch(inputId = "irr_modif", label = "Non/Oui", status = "success", fill = TRUE, value = selected_patient$irr))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Argent : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés ou besoin d'aide pour écrire un chèque, payer avec des billets... ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "bills_modif", label = NULL, choiceValues = c(0, 1, 2, 3, 8),  selected = as.numeric(selected_patient$bills),
                                       choiceNames = liste_choix,
                                       inline = TRUE,  status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Factures : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour gérer ses papiers, payer ses factures, etc... ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "taxes_modif",  label = NULL,choiceValues = c(0, 1, 2, 3, 8), selected = as.numeric(selected_patient$taxes),
                                       choiceNames = liste_choix,
                                       inline = TRUE,  status = "danger",  fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Shopping : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour faire ses courses ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "shopping_modif", label = NULL,choiceValues = c(0, 1, 2, 3, 8),selected = as.numeric(selected_patient$shopping),
                                       choiceNames = liste_choix,
                                       inline = TRUE,  status = "danger",  fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Jeu : "), tags$i("Dans les 4 dernières semaines, le patient a-t-il rencontré des difficultés pour jouer à un jeu de réflexion (bridge, échecs..) ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "games_modif", label = NULL,choiceValues = c(0, 1, 2, 3, 8), selected = as.numeric(selected_patient$games),
                                       choiceNames = liste_choix,
                                       inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Repas : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés lors de la préparation d'un repas équilibré ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "meal_modif", label = NULL,choiceValues = c(0, 1, 2, 3, 8),selected = as.numeric(selected_patient$meal),
                                       choiceNames = liste_choix,
                                       inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),

          fluidRow(
            column(width = 12,
                   tags$b("Evénements : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se rappeler d'événements courants ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "event_modif",  label = NULL,choiceValues = c(0, 1, 2, 3, 8),selected = as.numeric(selected_patient$event),
                                       choiceNames = liste_choix,
                                       inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Concentration : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se concentrer et à comprendre un programme TV, un livre ou un magazine ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "concentration_modif",  label = NULL,choiceValues = c(0, 1, 2, 3, 8),selected = as.numeric(selected_patient$concentration),
                                       choiceNames = liste_choix,
                                       inline = TRUE,   status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Souvenir dates : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "remdates_modif",   label = NULL,choiceValues = c(0, 1, 2, 3, 8),selected = as.numeric(selected_patient$remdates),
                                       choiceNames = liste_choix,
                                       inline = TRUE,  status = "danger",  fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Déplacements : "), tags$i("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se déplacer en dehors de son quartier, à conduire, ou à prendre les transports en commun ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "travel_modif", label = NULL,choiceValues = c(0, 1, 2, 3, 8),selected = as.numeric(selected_patient$travel),
                                       choiceNames = liste_choix,
                                       inline = TRUE,  status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12,
                   tags$b("Autonomie : "), tags$i("Quel est le niveau d'indépendance du patient ?"))),
          fluidRow(
            column(width = 12, h5(),
                   prettyRadioButtons( inputId = "independ_modif", label = NULL,choiceValues = c(1, 2, 3, 4, 9),selected = as.numeric(selected_patient$independ),
                                       choiceNames = c("En capacité de vivre seul", "Requiert une assistance pour des activités particulièrement complexes", "Requiert une assistace pour des activités quotidiennes", "Dépendant", "Ne sait pas"),
                                       inline = FALSE,  status = "danger", fill = TRUE
                   ))
          ),
          fluidRow(
            column(width = 12, h4(),
                   div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("click_modify_patient", "Modifier"))))
        )
      })
    }

    shinyjs::show("fiche_patient")
  })


  #ajouter une fiche à la base
  observeEvent(input$click_add_patient  , {
    base_patient <- base_patient()
    if (input$genre == FALSE) {
      genre <-  "Femme"
    } else {
      genre <-  "Homme"
    }
    new_patient <- data.table(id = nrow(base_patient),
                              first_name = input$first_name,
                              last_name = input$last_name,
                              age_at_diagnosis = input$age_at_diagnosis,
                              date_entry = as.character(input$date_admin),
                              genre = genre,
                              size = input$size,
                              weight = input$weight,
                              agit = input$agit,
                              depress = input$depress,
                              anxiety = input$anxiety,
                              apathy = input$apathy,
                              disinhib = input$disinhib,
                              irr = input$irr,
                              bills = input$bills,
                              taxes = input$taxes,
                              shopping =input$shopping,
                              games = input$games,
                              meal =input$meal,
                              event = input$event,
                              concentration = input$concentration,
                              remdates = input$remdates,
                              travel = input$travel,
                              independ = input$independ)
    base_patient <- rbind(base_patient, new_patient)
    saveRDS(base_patient, file = file.path(path_data, "base_patient.rds"))

    sendSweetAlert(
      session = session,
      title = "Succès !",
      text = "Une nouvelle fiche a été ajoutée à la base",
      type = "success"
    )
    shinyjs::hide("fiche_patient")

  })

  #modifier une fiche existante
  observeEvent(input$click_modify_patient  , {

    base_patient <- base_patient()
    selected_patient <- base_patient[input$select_patient == id]

    if (input$genre_modif == FALSE) {
      genre <-  "Femme"
    } else {
      genre <-  "Homme"
    }
    modif_patient <- data.table(id = selected_patient$id,
                              first_name = input$first_name_modif,
                              last_name = input$last_name_modif,
                              age_at_diagnosis = input$age_at_diagnosis_modif,
                              date_entry = as.character(input$date_admin_modif),
                              genre = genre,
                              size = input$size_modif,
                              weight = input$weight_modif,
                              agit = input$agit_modif,
                              depress = input$depress_modif,
                              anxiety = input$anxiety_modif,
                              apathy = input$apathy_modif,
                              disinhib = input$disinhib_modif,
                              irr = input$irr_modif,
                              bills = input$bills_modif,
                              taxes = input$taxes_modif,
                              shopping =input$shopping_modif,
                              games = input$games_modif,
                              meal =input$meal_modif,
                              event = input$event_modif,
                              concentration = input$concentration_modif,
                              remdates = input$remdates_modif,
                              travel = input$travel_modif,
                              independ = input$independ_modif)

    base_patient <- rbind(base_patient[id != selected_patient$id], modif_patient)
    saveRDS(base_patient, file = file.path(path_data, "base_patient.rds"))
    shinyjs::hide("fiche_patient")

    sendSweetAlert(
      session = session,
      title = "Succès !",
      text = "La fiche a été modifiée",
      type = "success"
    )
  })






  output$contents <- renderUI({
    inFile <- input$input_img
    if (is.null(inFile))
      return(NULL)

    #menage dans le dir de destination
    if (!dir.exists(file.path(path_work, "png_img")))
    {
      dir.create(file.path(path_work, "png_img"))
    } else {
      file.remove(list.files(file.path(path_work, "png_img"), pattern = ".png", full.names = TRUE))
    }

    #on convert le nii en png

    withProgress(message = 'Conversion en png...',{
      setProgress(value = 0.3 , message = "Conversion en png...")
      workdir <- getwd()
      setwd(file.path(path_root, "inst/python/"))
      cmd_convertion <- paste0("python3 nii2png.py -i ", inFile$datapath, " -o ", file.path(path_work, "png_img"))
      system(cmd_convertion)
      setwd(workdir)
      nb_png <- length(list.files(file.path(path_work, "png_img"), pattern = ".png", full.names = TRUE))
    })

    #update rv
    input_file_rv$data_path <- inFile$datapath
    input_file_rv$nb_coupe <- nb_png


    shinyjs::show("cut_selection")

    return(valueBox(tags$p(paste0(as.character(nb_png),  " coupes"), style = "font-size: 80%;"), "Nombre de coupes extraites du fichier nifti", icon = icon("brain", lib = "font-awesome"), color = "olive", width = 12))

  })


  observe({

    inFile <- input$input_img
    if (is.null(inFile))
    {
      return(NULL)
    } else {

    }

    output$cut_select_ui <- renderUI({
      list_input <- seq(1, input_file_rv$nb_coupe, 1)
      names(list_input) <- paste0("Coupe numéro ", list_input)
      selectizeInput("cut_list", "Selection des coupes à afficher :",
                     choices = list_input, selected = "140", multiple = TRUE, options = list(maxItems = 6))})

  })


  observeEvent(input$click_visu_cut,{


    withProgress(message = 'Sélection des png à afficher...',{
      dementiaproject::cut_selection_shiny(path_raw = file.path(path_work, "png_img"), path_selected = file.path(path_work, "png_selected"), path_processed = file.path(path_work, "png_processed"), cut_list = as.numeric(input$cut_list), process = input$pre_process)
    })
    if ( input$pre_process == TRUE) {
      read_path <- file.path(path_work, "png_processed")
    } else {
      read_path <- file.path(path_work, "png_selected")
    }

    list_png <- list.files(path = read_path, pattern = ".png", full.names = TRUE)
    plist <- map(list_png, function(x) {ggdraw() + draw_image(x)})

    output$image2 <- renderPlot({
      plot_grid(plotlist = plist, labels = as.character(input$cut_list), label_size = 10, ncol = 4)

    })

    shinyjs::show("cut_visu")
  })


})
