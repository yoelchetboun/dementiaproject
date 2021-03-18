

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

  input_file_rv <- reactiveValues(data_path = NULL, nb_coupe =0, clear = TRUE)

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
      selectizeInput("select_patient", NULL,
                     choices = list_input, selected = "0", multiple = FALSE)
    })
  })


  observeEvent(input$click_patient  , {
    shinyjs::hide("cut_visu")
    base_patient <- base_patient()
    input_file_rv$data_path <- NULL

    selected_patient <- base_patient[input$select_patient == id]
    liste_choix <- c("Non", "Difficultés mais a réussi", "A eu besoin d'une aide", "Dépendant d'une tierce personne", "Ne sait pas")


    if (input$select_patient == "0") {
      #on veut ajouter un nouveau patient
      shinyjs::show("fiche_patient")

      shinyjs::hide("mri_load")
      shinyjs::hide("patient_selected")
      shinyjs::hide("patient_selected_rem")
      shinyjs::hide("patient_selected_rem2")
      shinyjs::hide("patient_selected_rem3")
      shinyjs::hide("cut_selection")
      shinyjs::hide("cut_visu")
      shinyjs::hide("prev_irm")
      shinyjs::hide("ihm_prev")
      shinyjs::hide("ihm_prev_irm")
      shinyjs::hide("ihm_prev_patient")
      shinyjs::hide("prev_coupe")
      shinyjs::hide("prev_coupe")



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
                   div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("click_modify_patient", "Modifier", width = "160px")), " ",
                   div(style="display: inline-block; vertical-align:top; width: 150px; margin-left:30px", actionButton("click_erase_patient", "Supprimer", width = "160px"))))
        )
      })
      shinyjs::show("mri_load")
      shinyjs::show("fiche_patient")
      shinyjs::show("patient_selected")
      shinyjs::show("patient_selected_rem")
      shinyjs::show("patient_selected_rem2")
      shinyjs::show("patient_selected_rem3")
      shinyjs::show("ihm_prev")
      shinyjs::show("ihm_prev_irm")
      shinyjs::show("ihm_prev_patient")
      shinyjs::hide("prev_coupe")


      output$prev_patient = renderUI({
        NULL
        })

      output$prev = renderUI({
        NULL
      })

      path_mri_id <- paste0("mri_id_", base_patient[input$select_patient == id]$id)

      if (dir.exists(file.path(path_data, path_mri_id ))) {
        list_png <- list.files(file.path(path_data, path_mri_id ), pattern = ".png", recursive = FALSE)
        nb_cut <- length(list_png)
        shinyjs::show("cut_selection")

      } else {
        nb_cut <- 0
        shinyjs::hide("cut_selection")
        shinyjs::hide("cut_visu")
        shinyjs::hide("prev_coupe")

      }
      input_file_rv$nb_coupe <- nb_cut


      # output$file_in <- renderUI({fileInput("input_img", "Choisir un fichier .img (format nifti)",
      #                                       accept = c(
      #                                         "text/csv",
      #                                         "text/comma-separated-values,text/plain",
      #                                         ".csv",
      #                                         ".img"), width = NULL,
      # )})

      reset('input_img')
      input_file_rv$clear <- TRUE
      input_file_rv$data_path <- NULL
      output$contents <-  renderUI({NULL})


    }


    output$patient_select = renderUI({
      base_patient <- base_patient()
      selected_patient <- base_patient[input$select_patient == id]

      input$select_var
      select_var = c("Age", "Taille", "Poids", "Entêtement", "Dépression", "Anxiété", "Apathie", "Désinhibé",
                     "Irritable", "Argent", "Factures", "Shopping", "Jeu", "Repas",
                     "Evénements", "Concentration", "Souvenir dates", "Déplacements", "Autonomie")
      dt_var = c("age_at_diagnosis","size", "weight","agit","depress","anxiety","apathy","disinhib",
                "irr","bills","taxes","shopping","games","meal","event","concentration","remdates","travel", "independ")
      dt_corresp <- data.table(select_var = select_var, dt_var=dt_var)
      var <- dt_corresp[select_var == input$select_var]$dt_var

      if (var %in% c("bills", "taxes", "shopping", "games", "meal", "event", "concentration", "remdates","travel", "independ"))
      {
        dt_corresp_value <- data.table(value = c(0, 1, 2, 3, 8),
                                       value_label = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                       "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))
      }

      if (var %in% c("independ"))
      {
        dt_corresp_value <- data.table(value = c(1, 2, 3, 4, 9),
                                       value_label = c("En capacité de vivre seul", "Requiert une assistance pour des activités particulièrement complexes", "Requiert une assistace pour des activités quotidiennes", "Dépendant", "Ne sait pas")
        )
      }
      if (var %in% c("agit", "depress", "anxiety", "apathy", "disinhib", "irr"))
      {
        dt_corresp_value <- data.table(value = c(TRUE, FALSE),
                                       value_label = c("Oui","Non"))
      }


      if (!var %in% c("age_at_diagnosis", "size", "weight")) {
        var_to_display <- dt_corresp_value[value == selected_patient[, .(get(var))]$V1]$value_label
      } else {
        var_to_display <- selected_patient[, .(get(var))]$V1
      }


      box(
        width = 12,
        status = "primary",  solidHeader = TRUE,
        title = "Patient sélectionné",
        #tags$head(tags$style(HTML(".small-box {height: 100px}"))),
        fluidRow (tags$head(tags$style(HTML('.info-box {min-height: 90px;} .info-box-content {padding-top: 15px; padding-bottom: 15px;}'))),

                  infoBox(title = paste0(selected_patient$first_name, "  ", selected_patient$last_name),
                          paste0(selected_patient$genre , " - ", selected_patient$age_at_diagnosis, " ans - ", selected_patient$size , " m"),icon = icon("users", lib = "font-awesome"),
                          color = "light-blue", fill =TRUE, width = 6
                  ),
                  tags$head(tags$style(HTML(".small-box {height: 90px}"))),
                  valueBox(tags$p(paste0(as.character(var_to_display)), style = "font-size: 50%;"), paste0("Variable ", input$select_var, " du patient sélectionné"), icon = icon("question", lib = "font-awesome"), color = "olive", width = 6)
        ))
    })

    output$patient_select_reminder = renderUI({
      base_patient <- base_patient()
      selected_patient <- base_patient[input$select_patient == id]
      path_mri_id <- paste0("mri_id_", selected_patient$id)

      if (dir.exists(file.path(path_data, path_mri_id ))) {
        list_png <- list.files(file.path(path_data, path_mri_id ), pattern = ".png", recursive = FALSE)
        nb_cut <- length(list_png)
      } else {
        nb_cut <- 0
      }

      box(
        width = 12,
        status = "primary",  solidHeader = TRUE,
        title = "Patient sélectionné",
        fluidRow(tags$head(tags$style(HTML('.info-box {min-height: 90px;} .info-box-content {padding-top: 15px; padding-bottom: 15px;}'))),
                 infoBox(title = paste0(selected_patient$first_name, "  ", selected_patient$last_name),
                         paste0(selected_patient$genre , " - ", selected_patient$age_at_diagnosis, " ans - ", selected_patient$size , " m"),icon = icon("users", lib = "font-awesome"),
                         color = "light-blue", fill =TRUE, width = 8
                 ),
                 tags$head(tags$style(HTML(".small-box {height: 90px}"))),
                 valueBox(tags$p(paste0(as.character(input_file_rv$nb_coupe),  " coupes"), style = "font-size: 80%;"), "Nombre de coupes en base pour ce patient", icon = icon("brain", lib = "font-awesome"), color = "olive", width = 4)
        ))
    })

    output$patient_select_reminder2 = renderUI({
      base_patient <- base_patient()
      selected_patient <- base_patient[input$select_patient == id]
      box(
        width = 12,
        status = "primary",  solidHeader = TRUE,
        title = "Patient sélectionné",
        fluidRow(tags$head(tags$style(HTML('.info-box {min-height: 90px;} .info-box-content {padding-top: 15px; padding-bottom: 15px;}'))),
                 infoBox(title = paste0(selected_patient$first_name, "  ", selected_patient$last_name),
                         paste0(selected_patient$genre , " - ", selected_patient$age_at_diagnosis, " ans - ", selected_patient$size , " m"),icon = icon("users", lib = "font-awesome"),
                         color = "light-blue", fill =TRUE, width = 12
                 )))
    })
    output$patient_select_reminder3 = renderUI({
      base_patient <- base_patient()
      selected_patient <- base_patient[input$select_patient == id]
      box(
        width = 12,
        status = "primary",  solidHeader = TRUE,
        title = "Patient sélectionné",
        fluidRow(tags$head(tags$style(HTML('.info-box {min-height: 90px;} .info-box-content {padding-top: 15px; padding-bottom: 15px;}'))),
          infoBox(title = paste0(selected_patient$first_name, "  ", selected_patient$last_name),
                paste0(selected_patient$genre , " - ", selected_patient$age_at_diagnosis, " ans - ", selected_patient$size , " m"),icon = icon("users", lib = "font-awesome"),
                color = "light-blue", fill =TRUE, width = 12
        )))
    })
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
    shinyjs::hide("patient_selected")
    shinyjs::hide("patient_selected_rem")
    shinyjs::hide("patient_selected_rem2")
    shinyjs::hide("patient_selected_rem3")

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

    shinyjs::hide("patient_selected")
    shinyjs::hide("patient_selected_rem")
    shinyjs::hide("patient_selected_rem2")
    shinyjs::hide("patient_selected_rem3")

    sendSweetAlert(
      session = session,
      title = "Succès !",
      text = "La fiche a été modifiée",
      type = "success"
    )
  })



  observeEvent(input$click_erase_patient  , {

    base_patient <- base_patient()
    id_selected_patient <- base_patient[input$select_patient == id]$id
    path_mri_id <- paste0("mri_id_", id_selected_patient)
    unlink(file.path(path_data, path_mri_id), recursive = TRUE)
    base_patient <- base_patient[input$select_patient != id]

    saveRDS(base_patient, file = file.path(path_data, "base_patient.rds"))

    shinyjs::hide("fiche_patient")

    sendSweetAlert(
      session = session,
      title = "Succès !",
      text = "La fiche a été supprimée de la base",
      type = "success"
    )
  })


  observeEvent(input$input_img$datapath, {

    print("Event!")
    print(input$input_img$datapath)

    if(input_file_rv$clear) {

    #browser()
    #inFile <- input$input_img
    input_file_rv$data_path <- input$input_img$datapath


    if (is.null(input_file_rv$data_path )) {
      output$contents <-  renderUI({NULL})
    } else {



      #menage dans le dir de destination
      base_patient <- base_patient()
      id_selected_patient <- base_patient[input$select_patient == id]$id
      path_mri_id <- paste0("mri_id_", id_selected_patient)

      if (!dir.exists(file.path(path_data, path_mri_id )))
      {
        dir.create(file.path(path_data, path_mri_id ))
      } else {
        file.remove(list.files(file.path(path_data, path_mri_id ), pattern = ".png", full.names = TRUE))
      }

      #on convert le nii en png

      withProgress(message = 'Conversion en png...',{
        setProgress(value = 0.3 , message = "Conversion en png...")
        workdir <- getwd()
        setwd(file.path(path_root, "inst/python/"))
        cmd_convertion <- paste0("python3 nii2png.py -i ", input_file_rv$data_path, " -o ", file.path(path_data, path_mri_id))
        system(cmd_convertion)
        setwd(workdir)
        nb_png <- length(list.files(file.path(path_data, path_mri_id), pattern = ".png", full.names = TRUE))
      })

      #update rv
      input_file_rv$nb_coupe <- nb_png
      shinyjs::show("cut_selection")
      input_file_rv$clear <- FALSE


      output$contents <-  renderUI({
        valueBox(tags$p(paste0(as.character(nb_png),  " coupes"), style = "font-size: 80%;"), "Nombre de coupes extraites du fichier nifti", icon = icon("brain", lib = "font-awesome"), color = "olive", width = NULL)
      })

    }

    } else {
      output$contents <-  renderUI({NULL})
    }

  })


  #
  #
  # output$contents <- renderUI({
  #
  #   print(paste0("rv_clear :", input_file_rv$clear))
  #   print(paste0("rv_pathh :", input_file_rv$data_path))
  #   print(paste0("inputfile :", input$input_img$datapath))
  #
  #   if (input_file_rv$clear == TRUE) {
  #
  #     #browser()
  #     #inFile <- input$input_img
  #     input_file_rv$data_path <- input$input_img$datapath
  #
  #
  #     if (is.null(input_file_rv$data_path )) {
  #       return(NULL)
  #     }
  #
  #
  #
  #     #menage dans le dir de destination
  #     base_patient <- base_patient()
  #     id_selected_patient <- base_patient[input$select_patient == id]$id
  #     path_mri_id <- paste0("mri_id_", id_selected_patient)
  #
  #     if (!dir.exists(file.path(path_data, path_mri_id )))
  #     {
  #       dir.create(file.path(path_data, path_mri_id ))
  #     } else {
  #       file.remove(list.files(file.path(path_data, path_mri_id ), pattern = ".png", full.names = TRUE))
  #     }
  #
  #     #on convert le nii en png
  #
  #     withProgress(message = 'Conversion en png...',{
  #       setProgress(value = 0.3 , message = "Conversion en png...")
  #       workdir <- getwd()
  #       setwd(file.path(path_root, "inst/python/"))
  #       cmd_convertion <- paste0("python3 nii2png.py -i ", input_file_rv$data_path, " -o ", file.path(path_data, path_mri_id))
  #       system(cmd_convertion)
  #       setwd(workdir)
  #       nb_png <- length(list.files(file.path(path_data, path_mri_id), pattern = ".png", full.names = TRUE))
  #     })
  #
  #     #update rv
  #     input_file_rv$nb_coupe <- nb_png
  #     shinyjs::show("cut_selection")
  #     input_file_rv$clear <- FALSE
  #
  #     return(valueBox(tags$p(paste0(as.character(nb_png),  " coupes"), style = "font-size: 80%;"), "Nombre de coupes extraites du fichier nifti", icon = icon("brain", lib = "font-awesome"), color = "olive", width = NULL))
  #   } else {
  #     return(NULL)
  #
  #   }
  # })


  observe({

    if (input_file_rv$nb_coupe == 0)
    {
      return(NULL)
    } else {

    }

    output$cut_select_ui <- renderUI({
      list_input <- seq(1, input_file_rv$nb_coupe, 1)
      names(list_input) <- paste0("Coupe numéro ", list_input)
      selectizeInput("cut_list", "Sélection des coupes à afficher :",
                     choices = list_input, selected = c("130", "135", "140", "145"), multiple = TRUE, options = list(maxItems = 4))})

  })


  observeEvent(input$click_visu_cut,{

    withProgress(message = 'Sélection des png à afficher...',{
      base_patient <- base_patient()
      id_selected_patient <- base_patient[input$select_patient == id]$id
      path_mri_id <- paste0("mri_id_", id_selected_patient)
      list_png <- list.files(path = file.path(path_data, path_mri_id), pattern = ".png", full.names = TRUE)
      list_png <- unlist(map(input$cut_list, ~list_png[grepl(pattern = ., list_png)]))
      plist <- map(list_png, function(x) {ggdraw() + draw_image(x)})
    })

    output$image2 <- renderPlot({
      plot_grid(plotlist = plist, labels = as.character(input$cut_list), label_size = 10, ncol = 4)

    })

    shinyjs::show("cut_visu")
  })




  observeEvent(input$launch_prev_2_class_irm,{


    withProgress(message = 'Lancement de la prévision...',{
      base_patient <- base_patient()
      id_selected_patient <- base_patient[input$select_patient == id]$id
      path_mri_id <- paste0("mri_id_", id_selected_patient)

      if (!dir.exists(file.path(path_data, path_mri_id )))
      {
        print("attention il n'y a pas d'irm à prévoir pour ce patient")
        output$prev = renderUI({
          valueBox(tags$p("Attention", style = "font-size: 80%;"), subtitle = "Aucune IRM chargée pour ce patient", icon = icon("exclamation", lib = "font-awesome"), color = "yellow", width = 12)
        })

      } else {
        #on lance la prev avec le bon modele
        #on déplace les coupes
        path_dir_pred <- file.path(path_data, "png_tmp")
        file.remove(list.files(file.path(path_dir_pred, "to_pred"), pattern = ".png", full.names = TRUE, recursive = TRUE))
        setProgress(value = 0.3 , message = "Extraction des png..")

        cut_list<- c(142, 144, 146, 148)
        list_png <- unlist(map(cut_list, ~list.files(file.path(path_data, path_mri_id ), pattern = paste0("z", ., ".png") , full.names = TRUE, recursive = TRUE)))
        map(list_png, ~file.copy(from = ., to = file.path(path_dir_pred, "to_pred")))

        model <- load_model_hdf5(list.files(file.path(path_data, "models/2_class_irm"), pattern = ".h5", full.names = TRUE))

        image_generator <- image_data_generator(rescale=1/255)
        image_test <- flow_images_from_directory(
          path_dir_pred,
          image_generator,
          shuffle = FALSE,
          target_size = c(160, 224),
          color_mode = "rgb",
          class_mode = "binary",
          batch_size = 1
        )

        image_shape <- c(160L, 224L, 3L)
        setProgress(value = 0.6 , message = "Prévision..")

        pred <- predict_generator(
          model,
          image_test,
          steps =  length(list_png),
          verbose = 1)

        mean_pred <- mean(pred)

        if (mean_pred > 0.5) {
          #lepatient est sain
          output$prev = renderUI({
            valueBox(tags$p("Patient sain", style = "font-size: 80%;"), paste0("Probabilité de non démence (moyenne des probabilités des 4 coupes) : ", round(100*mean_pred, 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)

            # output$prev = renderText({
            #   paste("<p style='color:green;'><b>sain</b> - Probabilité moyenne de : ", round(100*mean_pred, 2), "%")
          })
        } else {
          output$prev = renderUI({
            valueBox(tags$p("Patient atteint de démence", style = "font-size: 80%;"), paste0("Probabilité de démence (moyenne des probabilités des 4 coupes) : ", round(100*(1-mean_pred), 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
          })
        }
        #browser()

        #coupe 1
        output$tab1_img <- renderImage({
          list(src = list_png[1], width = "200")
        }, deleteFile = FALSE)

        output$val_tab1 <- renderUI({
          if(pred[1] > 0.5) {
            valueBox(tags$p(paste0(round(100*pred[1], 2),  " %"), style = "font-size: 80%;"), "Probabilité de non démence", icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 8)
          } else {
            valueBox(tags$p(paste0(round(100*(1-pred[1]), 2),  " %"), style = "font-size: 80%;"), "Probabilité de démence", icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 8)

          }
        })

        #coupe 2
        output$tab2_img <- renderImage({
          list(src = list_png[2], width = "200")
        }, deleteFile = FALSE)


        output$val_tab2 <- renderUI({
          if(pred[2] > 0.5) {
            valueBox(tags$p(paste0(round(100*pred[2], 2),  " %"), style = "font-size: 80%;"), "Probabilité de non démence", icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 8)
          } else {
            valueBox(tags$p(paste0(round(100*(1-pred[2]), 2),  " %"), style = "font-size: 80%;"), "Probabilité de démence", icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 8)
          }
        })

        #coupe 3
        output$tab3_img <- renderImage({
          list(src = list_png[3], width = "200")
        }, deleteFile = FALSE)

        output$val_tab3 <- renderUI({
          if(pred[3] > 0.5) {
            valueBox(tags$p(paste0(round(100*pred[3], 2),  " %"), style = "font-size: 80%;"), "Probabilité de non démence", icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 8)
          } else {
            valueBox(tags$p(paste0(round(100*(1-pred[3]), 2),  " %"), style = "font-size: 80%;"), "Probabilité de démence", icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 8)
          }
        })

        #coupe 4
        output$tab4_img <- renderImage({
          list(src = list_png[4], width = "200")
        }, deleteFile = FALSE)

        output$val_tab4 <- renderUI({
          if(pred[4] > 0.5) {
            valueBox(tags$p(paste0(round(100*pred[4], 2),  " %"), style = "font-size: 80%;"), "Probabilité de non démence", icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 8)
          } else {
            valueBox(tags$p(paste0(round(100*(1-pred[4]), 2),  " %"), style = "font-size: 80%;"), "Probabilité de démence", icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 8)

          }
        })

        output$pred_tabs <- renderUI(
          tabsetPanel(
            id = "tabs",
            tabPanel(
              "Coupe 1 (z = 142)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab1_img")
                ),
                column(6,
                       uiOutput("val_tab1")
                )
              )
            ),
            tabPanel(
              "Coupe 2 (z = 144)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab2_img")
                ),
                column(6,
                       uiOutput("val_tab2")
                )
              )
            ),
            tabPanel(
              "Coupe 3 (z = 146)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab3_img")
                ),
                column(6,
                       uiOutput("val_tab3")
                )
              )
            ),
            tabPanel(
              "Coupe 4 (z = 148)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab4_img")
                ),
                column(6,
                       uiOutput("val_tab4")
                )
              )
            )
          )

        )

        shinyjs::show("prev_coupe")

      }
    })


  })

  output$ggplot_var <- renderPlot({


    diag_data$dementia_label<-factor(diag_data$dementia, labels=c("Absence de trouble","Présence de troubles"))
    diag_data$CDR3_label<-factor(diag_data$CDR3, labels=c("Absence de trouble", "Troubles incertains", "Troubles bénins, modérés ou sévères"))


    select_var = c("Age", "Taille", "Poids", "Entêtement", "Dépression", "Anxiété", "Apathie", "Désinhibé",
                   "Irritable", "Argent", "Factures", "Shopping", "Jeu", "Repas",
                   "Evénements", "Concentration", "Souvenir dates", "Déplacements", "Autonomie")
    dt_var = c("age_at_diagnosis","TAILLE", "POIDS","ENTETEMENT","DEPRESS", "ANXIETE", "APATHIE", "DISINHIB",
               "IRRITAB","ARGENT", "FACTURES","SHOPPING","JEU","REPAS","SOUV_EVENT","CONCENTRATION","SOUV_DATES",  "DEPLACEMENT", "AUTONOMIE")

    dt_corresp <- data.table(select_var = select_var, dt_var=dt_var)

    var <- dt_corresp[select_var == input$select_var]$dt_var

    if (input$select_var %in% c("Argent", "Factures", "Shopping", "Jeu", "Repas",
                                "Evénements", "Concentration", "Souvenir dates", "Déplacements")) {
      labels_var <- c("Non","Il a rencontré des difficulté, mais a réussi seul",
                      "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas")
    }

    if (input$select_var %in% c("Entêtement", "Dépression", "Anxiété", "Apathie", "Désinhibé",
                                "Irritable")) {
      labels_var <- c("Non","Oui")
    }

    if (input$select_var == "Autonomie") {
      labels_var <- c("En capacité de vivre seul", "Requiert une assistance pour des activités particulièrement complexes", "Requiert une assistace pour des activités quotidiennes", "Dépendant", "Ne sait pas")
    }

    if (input$select_var == "Age") {
      gg <-  ggplot(diag_data[, .(CDR3_label , age_at_diagnosis )], aes(fill = CDR3_label, x= age_at_diagnosis)) + geom_density(alpha=0.4) + ggtitle("Distribution de l'âge au moment du diagnostic") +
        scale_fill_discrete(name = "Démence :") + labs(x = "Age au moment du diagnostic", y = "Distribution") + theme_minimal() + theme(legend.position="bottom")
    }

    if (input$select_var == "Taille") {
      gg <-  ggplot(diag_data[, .(CDR3_label , TAILLE     )], aes(fill = CDR3_label, x= TAILLE    )) + geom_density(alpha=0.4) + ggtitle("Distribution de la taille des patients") +
        scale_fill_discrete(name = "Démence :") + labs(x = "Taille du patient [cm]", y = "Distribution") + theme_minimal() + theme(legend.position="bottom")
    }
    if (input$select_var == "Poids") {
      gg <-  ggplot(diag_data[, .(CDR3_label , POIDS      )], aes(fill = CDR3_label, x= POIDS     )) + geom_density(alpha=0.4) + ggtitle("Distribution du poids des patients") +
        scale_fill_discrete(name = "Démence :") + labs(x = "Poids du patient [Kg]", y = "Distribution") + theme_minimal() + theme(legend.position="bottom")
    }

    if (!input$select_var %in% c("Age", "Taille", "Poids")) {
      gg_dt <- unique(diag_data[, .(Session  , get(var), CDR3,CDR3_label)])
      setnames(gg_dt, "V2", var)
      gg_dt[,nb_cat := length(.SD$Session), by = c("CDR3",  var)]
      gg_dt[,nb_tot := length(.SD$Session), by = c("CDR3")]
      gg_dt[, prop := nb_cat/nb_tot]
      gg <- ggplot(unique(gg_dt[, .(get(var), CDR3_label, prop)]), aes(x = CDR3_label, y= prop, fill = V1)) + geom_bar(stat="identity", position=position_dodge()) + geom_label(aes(label = round(prop, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5) +
        theme_light() +
        xlab("Démence") +
        ylab("Proportion") +
        labs(fill = "Réponse :") +
        theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        ) +
        scale_fill_brewer(labels = labels_var)+
        theme(legend.position="bottom")

      if (input$select_var == "Entêtement") {
        gg <-gg + ggtitle("Le patient s'entête-t-il et refuse-t-il l'aide des autres ?")
      }
      if (input$select_var == "Dépression") {
        gg <- gg + ggtitle("Le patient connait-il des épisodes de dépression ou de dysphorie ?")
      }
      if (input$select_var == "Anxiété") {
        gg <- gg + ggtitle("Le patient est-il anxieux ?")
      }
      if (input$select_var == "Apathie") {
        gg <-gg + ggtitle("Le patient est-il apathique ?")
      }
      if (input$select_var == "Désinhibé") {
        gg <-gg + ggtitle("Le patient est-il désinhibé ?")
      }
      if (input$select_var == "Irritable") {
        gg <-gg + ggtitle("Le patient est-il irritable ?")
      }
      if (input$select_var == "Argent") {
        gg <-gg + ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés ou besoin d'aide pour écrire un chèque, payer avec des billets... ?")
      }
      if (input$select_var == "Factures") {
        gg <-gg + ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour gérer ses papiers, payer ses factures, etc... ?")
      }
      if (input$select_var == "Shopping") {
        gg <-gg + ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour faire ses courses ?")
      }
      if (input$select_var == "Jeu") {
        gg <-gg + ggtitle("Dans les 4 dernières semaines, le patient a-t-il rencontré des difficultés pour jouer à un jeu de réflexion (bridge, échecs..) ?")
      }
      if (input$select_var == "Repas") {
        gg <-gg + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés lors de la préparation d'un repas équilibré ?")
      }
      if (input$select_var == "Evénements") {
        gg <-gg + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se rappeler d'événements courants ?")
      }
      if (input$select_var == "Concentration") {
        gg <-gg + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se concentrer et à comprendre un programme TV, un livre ou un magazine ?")
      }
      if (input$select_var == "Souvenir dates") {
        gg <-gg + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?")
      }
      if (input$select_var == "Déplacements") {
        gg <-gg + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se déplacer\nen dehors de son quartier, à conduire, ou à prendre les transports en commun ?")
      }
      if (input$select_var == "Autonomie") {
        gg <-gg + ggtitle("Quel est le niveau d'indépendance du patient ?")
      }

    }
    return(gg)

  })



  #prevision base patient


  observeEvent(input$launch_prev_2_class_patient,{


    withProgress(message = 'Lancement de la prévision...',{
      base_patient <- base_patient()
      selected_patient <- base_patient[input$select_patient == id]

      model <- dementiaproject::loadRData(list.files(path = file.path(path_data, "models", "2_class_patient"), pattern = ".Rdata", full.names = TRUE))
      if (selected_patient$genre == "Homme") {
        M_F <- "M"
      } else {
        M_F <- "F"
      }
      dt_2_pred <- data.table(dementia = NA,
                              age_at_diagnosis = selected_patient$age_at_diagnosis,
                              M.F = factor(M_F, levels = c("F", "M")),
                              TAILLE = selected_patient$size,
                              POIDS = selected_patient$weight,
                              AUTONOMIE = factor(selected_patient$independ, levels = as.character(c(1, 2, 3, 4, 9))),
                              ENTETEMENT = factor(as.numeric(selected_patient$agit), levels = as.character(c(0, 1))),
                              DEPRESS = factor(as.numeric(selected_patient$depress), levels = as.character(c(0, 1))),
                              ANXIETE = factor(as.numeric(selected_patient$anxiety), levels = as.character(c(0, 1))),
                              APATHIE = factor(as.numeric(selected_patient$apathy), levels = as.character(c(0, 1))),
                              DISINHIB = factor(as.numeric(selected_patient$disinhib), levels = as.character(c(0, 1))),
                              IRRITAB = factor(as.numeric(selected_patient$irr), levels = as.character(c(0, 1))),
                              ARGENT = factor(as.numeric(selected_patient$bills), levels = as.character(c(0, 1, 2, 3, 8))),
                              FACTURES = factor(as.numeric(selected_patient$taxes), levels = as.character(c(0, 1, 2, 3, 5, 8, 9))),
                              SHOPPING = factor(as.numeric(selected_patient$shopping), levels = as.character(c(0, 1, 2, 3, 8))),
                              JEU = factor(as.numeric(selected_patient$games), levels = as.character(c(0, 1, 2, 3, 8))),
                              REPAS = factor(as.numeric(selected_patient$meal), levels = as.character(c(0, 1, 2, 3, 8))),
                              SOUV_EVENT = factor(as.numeric(selected_patient$event), levels = as.character(c(0, 1, 2, 3, 8))),
                              CONCENTRATION = factor(as.numeric(selected_patient$concentration), levels = as.character(c(0, 1, 2, 3, 8))),
                              SOUV_DATES = factor(as.numeric(selected_patient$remdates), levels = as.character(c(0, 1, 2, 3, 8))),
                              DEPLACEMENT = factor(as.numeric(selected_patient$travel), levels = as.character(c(0, 1, 2, 3, 8))))

      pred <- predict(model,newdata=dt_2_pred)
      pred_prob <- predict(model,newdata=dt_2_pred, type = 'prob')

      if (pred == 0) {
        #le patient est sain
        output$prev_patient = renderUI({
          valueBox(tags$p("Patient sain (Absence de trouble - CDR = 0.0)", style = "font-size: 80%;"), paste0("Probabilité de non démence : ", round(100*pred_prob[,1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
        })
      } else {
        output$prev_patient = renderUI({
          valueBox(tags$p("Patient atteint de démence", style = "font-size: 80%;"), paste0("Probabilité de démence : ", round(100*pred_prob[,2], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
        })
      }
    })
  })




  observeEvent(input$launch_prev_3_class_patient,{


    withProgress(message = 'Lancement de la prévision...',{
      base_patient <- base_patient()
      selected_patient <- base_patient[input$select_patient == id]
      model <- dementiaproject::loadRData(list.files(path = file.path(path_data, "models", "3_class_patient"), pattern = ".Rdata", full.names = TRUE))

      if (selected_patient$genre == "Homme") {
        M_F <- "M"
      } else {
        M_F <- "F"
      }
      dt_2_pred <- data.table(dementia = NA,
                              age_at_diagnosis = selected_patient$age_at_diagnosis,
                              M.F = factor(M_F, levels = c("F", "M")),
                              TAILLE = selected_patient$size,
                              POIDS = selected_patient$weight,
                              AUTONOMIE = factor(selected_patient$independ, levels = as.character(c(1, 2, 3, 4, 9))),
                              ENTETEMENT = factor(as.numeric(selected_patient$agit), levels = as.character(c(0, 1))),
                              DEPRESS = factor(as.numeric(selected_patient$depress), levels = as.character(c(0, 1))),
                              ANXIETE = factor(as.numeric(selected_patient$anxiety), levels = as.character(c(0, 1))),
                              APATHIE = factor(as.numeric(selected_patient$apathy), levels = as.character(c(0, 1))),
                              DISINHIB = factor(as.numeric(selected_patient$disinhib), levels = as.character(c(0, 1))),
                              IRRITAB = factor(as.numeric(selected_patient$irr), levels = as.character(c(0, 1))),
                              ARGENT = factor(as.numeric(selected_patient$bills), levels = as.character(c(0, 1, 2, 3, 8))),
                              FACTURES = factor(as.numeric(selected_patient$taxes), levels = as.character(c(0, 1, 2, 3, 5, 8, 9))),
                              SHOPPING = factor(as.numeric(selected_patient$shopping), levels = as.character(c(0, 1, 2, 3, 8))),
                              JEU = factor(as.numeric(selected_patient$games), levels = as.character(c(0, 1, 2, 3, 8))),
                              REPAS = factor(as.numeric(selected_patient$meal), levels = as.character(c(0, 1, 2, 3, 8))),
                              SOUV_EVENT = factor(as.numeric(selected_patient$event), levels = as.character(c(0, 1, 2, 3, 8))),
                              CONCENTRATION = factor(as.numeric(selected_patient$concentration), levels = as.character(c(0, 1, 2, 3, 8))),
                              SOUV_DATES = factor(as.numeric(selected_patient$remdates), levels = as.character(c(0, 1, 2, 3, 8))),
                              DEPLACEMENT = factor(as.numeric(selected_patient$travel), levels = as.character(c(0, 1, 2, 3, 8))))

      pred <- predict(model,newdata=dt_2_pred)
      pred_prob <- predict(model,newdata=dt_2_pred, type = 'prob')

      if (pred == 0) {
        #le patient est sain
        output$prev_patient = renderUI({
          valueBox(tags$p("Patient sain (Absence de trouble - CDR = 0.0)", style = "font-size: 60%;"), paste0("Probabilité : ", round(100*pred_prob[,1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
        })
      }

      if (pred == 1) {
        output$prev_patient = renderUI({
          valueBox(tags$p("Patient possiblement atteint (Troubles incertains - CDR = 0.5)", style = "font-size: 60%;"), paste0("Probabilité : ", round(100*pred_prob[,2], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "yellow", width = 12)
        })
      }
      if (pred == 2) {

        output$prev_patient = renderUI({
          valueBox(tags$p("Patient atteint (Troubles bénins, modérés ou sévères - CDR >= 1.0)", style = "font-size: 60%;"), paste0("Probabilité : ", round(100*pred_prob[,3], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
        })
      }
    })
  })



  observeEvent(input$launch_prev_3_class_irm,{


    withProgress(message = 'Lancement de la prévision...',{
      base_patient <- base_patient()
      id_selected_patient <- base_patient[input$select_patient == id]$id
      path_mri_id <- paste0("mri_id_", id_selected_patient)

      if (!dir.exists(file.path(path_data, path_mri_id )))
      {
        print("attention il n'y a pas d'irm à prévoir pour ce patient")
        output$prev = renderUI({
          valueBox(tags$p("Attention", style = "font-size: 80%;"), subtitle = "Aucune IRM chargée pour ce patient", icon = icon("exclamation", lib = "font-awesome"), color = "yellow", width = 12)
        })

      } else {
        #on lance la prev avec le bon modele
        #on déplace les coupes
        path_dir_pred <- file.path(path_data, "png_tmp")
        file.remove(list.files(file.path(path_dir_pred, "to_pred"), pattern = ".png", full.names = TRUE, recursive = TRUE))
        setProgress(value = 0.3 , message = "Extraction des png..")
        if (file.exists(file.path(path_data,'/models/3_class_irm/prev.csv'))) {
          file.remove(file.path(path_data, '/models/3_class_irm/prev.csv'))
        }

        cut_list<- c(152, 153, 154, 155)
        list_png <- unlist(map(cut_list, ~list.files(file.path(path_data, path_mri_id ), pattern = paste0("z", ., ".png") , full.names = TRUE, recursive = TRUE)))
        map(list_png, ~file.copy(from = ., to = file.path(path_dir_pred, "to_pred")))

        ### launch prev python ###
        reticulate::source_python(file.path(path_root, "inst/python/launch_3_class_pred.py"), envir = parent.frame(), convert = TRUE)
        pred <- read.csv2(file.path(path_data, "models/3_class_irm/prev.csv"), header = FALSE, sep = "," , dec = ".")
        pred <- as.matrix(pred)

        mean_pred <- c(mean(pred[,1]), mean(pred[,2]), mean(pred[,3]))
        n_max <- which.max(mean_pred)

        if (n_max == 1) {
          output$prev = renderUI({
            valueBox(tags$p("Patient sain (Absence de trouble - CDR = 0.0)", style = "font-size: 60%;"), paste0("Probabilité (moyenne des probabilités des 4 coupes) : ", round(100*mean_pred[n_max], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
          })
        }

        if (n_max == 2) {
          output$prev = renderUI({
            valueBox(tags$p("Patient possiblement atteint (Troubles incertains - CDR = 0.5)", style = "font-size: 60%;"), paste0("Probabilité (moyenne des probabilités des 4 coupes) : ", round(100*mean_pred[n_max], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "yellow", width = 12)
          })
        }

        if (n_max == 3) {
          output$prev = renderUI({
            valueBox(tags$p("Patient atteint de démence (Troubles bénins, modérés ou sévères - CDR >= 1.0)", style = "font-size: 60%;"), paste0("Probabilité (moyenne des probabilités des 4 coupes) : ", round(100*mean_pred[n_max], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
          })
        }

        #coupe 1
        output$tab1_img <- renderImage({
          list(src = list_png[1], width = "200")
        }, deleteFile = FALSE)

        output$val_tab1 <- renderUI({
          max_1 <-  which.max(pred[1,])
          if (max_1 == 1) {
            vb <- valueBox(tags$p("Sain", style = "font-size: 60%;"), paste0("Probabilité (Absence de trouble) : ", round(100*pred[1,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
          }
          if (max_1 == 2) {
            vb <-valueBox(tags$p("Possiblement atteint", style = "font-size: 60%;"), paste0("Probabilité (Troubles incertains) : ", round(100*pred[1,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "yellow", width = 12)
          }
          if (max_1 == 3) {
            vb <-valueBox(tags$p("Atteint de démence", style = "font-size: 60%;"), paste0("Probabilité (Troubles bénins, modérés ou sévères) : ", round(100*pred[1,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
          }
          return(vb)
        })

        output$val_tab2 <- renderUI({
          max_1 <-  which.max(pred[2,])
          if (max_1 == 1) {
            vb <- valueBox(tags$p("Sain", style = "font-size: 60%;"), paste0("Probabilité (Absence de trouble) : ", round(100*pred[2,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
          }
          if (max_1 == 2) {
            vb <- valueBox(tags$p("Possiblement atteint", style = "font-size: 60%;"), paste0("Probabilité (Troubles incertains) : ", round(100*pred[2,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "yellow", width = 12)
          }
          if (max_1 == 3) {
            vb <- valueBox(tags$p("Atteint de démence", style = "font-size: 60%;"), paste0("Probabilité (Troubles bénins, modérés ou sévères) : ", round(100*pred[2,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
          }
          return(vb)
        })

        output$val_tab3 <- renderUI({
          max_1 <-  which.max(pred[3,])
          if (max_1 == 1) {
            vb <- valueBox(tags$p("Sain", style = "font-size: 60%;"), paste0("Probabilité (Absence de trouble) : ", round(100*pred[3,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
          }
          if (max_1 == 2) {
            vb <- valueBox(tags$p("Possiblement atteint", style = "font-size: 60%;"), paste0("Probabilité (Troubles incertains) : ", round(100*pred[3,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "yellow", width = 12)
          }
          if (max_1 == 3) {
            vb <- valueBox(tags$p("Atteint de démence", style = "font-size: 60%;"), paste0("Probabilité (Troubles bénins, modérés ou sévères) : ", round(100*pred[3,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
          }
          return(vb)
        })

        output$val_tab4 <- renderUI({
          max_1 <-  which.max(pred[4,])
          if (max_1 == 1) {
            vb <- valueBox(tags$p("Sain", style = "font-size: 60%;"), paste0("Probabilité (Absence de trouble) : ", round(100*pred[4,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "olive", width = 12)
          }
          if (max_1 == 2) {
            vb <- valueBox(tags$p("Possiblement atteint", style = "font-size: 60%;"), paste0("Probabilité (Troubles incertains) : ", round(100*pred[4,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "yellow", width = 12)
          }
          if (max_1 == 3) {
            vb <- valueBox(tags$p("Atteint de démence", style = "font-size: 60%;"), paste0("Probabilité (Troubles bénins, modérés ou sévères) : ", round(100*pred[4,][max_1], 2),  " %"), icon = icon("stethoscope", lib = "font-awesome"), color = "red", width = 12)
          }
          return(vb)
        })


        #coupe 2
        output$tab2_img <- renderImage({
          list(src = list_png[2], width = "200")
        }, deleteFile = FALSE)


        #coupe 3
        output$tab3_img <- renderImage({
          list(src = list_png[3], width = "200")
        }, deleteFile = FALSE)

        #coupe 4
        output$tab4_img <- renderImage({
          list(src = list_png[4], width = "200")
        }, deleteFile = FALSE)

        output$pred_tabs <- renderUI(
          tabsetPanel(
            id = "tabs",
            tabPanel(
              "Coupe 1 (z = 138)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab1_img")
                ),
                column(6,
                       uiOutput("val_tab1")
                )
              )
            ),
            tabPanel(
              "Coupe 2 (z = 140)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab2_img")
                ),
                column(6,
                       uiOutput("val_tab2")
                )
              )
            ),
            tabPanel(
              "Coupe 3 (z = 142)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab3_img")
                ),
                column(6,
                       uiOutput("val_tab3")
                )
              )
            ),
            tabPanel(
              "Coupe 4 (z = 144)",
              fluidRow(
                br(),
                column(6, align="center",
                       imageOutput("tab4_img")
                ),
                column(6,
                       uiOutput("val_tab4")
                )
              )
            )
          )
        )
        shinyjs::show("prev_coupe")
      }
    })


  })



})
