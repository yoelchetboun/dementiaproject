library(Rshinytemplate)
library(shinydashboard)
library(glue)
library(shinyjs)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(shiny)
library(viridis)
library(data.table)
library(dplyr)
library(tidyr)
library(leaflet)
library(raster)
library(mapview)
library(leafem)
library(leafgl)
library(leaflet.extras)
library(tidync)
library(WNIFRRteDrones)
library(googleVis)
library(dygraphs)
library(purrr)
library(rnaturalearth)
library(rnaturalearthhires)
library(grid)
library(gridExtra)
library(shinycssloaders)
library(tidyverse)
library(cowplot)
library(magick)



source("ui.R")

options(shiny.maxRequestSize = 30*1024^2)

print("Lancement du Shiny")
print(getwd())

#path_root <- "/root/"
path_root <- "/srv/RTE_DRONES_DATA/"



path_work <- "~/GENERIC/dementiaproject/inst/extdata/shiny_data"
path_root <- "~/GENERIC/dementiaproject/"
path_user_base  <- file.path(path_root, "user_base.rds")


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
