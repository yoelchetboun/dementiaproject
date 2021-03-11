library(Rshinytemplate)

ui2 <- function(){
  tagList(
    div(class = "tab-content",

        tabItem(

          ### Tab 1 ###
          tabName = "patient",

          fluidRow(
            box(width = 12, status = 'primary', solidHeader = TRUE,
                title = "Sélection du patient",
                uiOutput('select_patient'),
                div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("click_patient", "Sélectionner"))
            )
          ),


          hidden(
            fluidRow(id = "fiche_patient",
                     uiOutput("info_patient")
            )
          )
        ),



        tabItem(

          ### Tab 2 ###
          tabName = "info",


          hidden(
            fluidRow(id = "patient_selected",
                     uiOutput("patient_select")
            )
          ),

          fluidRow(
            box(width = 12, status = 'primary', solidHeader = TRUE,
                title = "Info",
                column(width = 9),
                column(width = 3)
            )
          )
        ),




        tabItem(


          ### Tab 1 ###

          tabName = "irm",

          hidden(
            fluidRow(id = "patient_selected_rem",
                     uiOutput("patient_select_reminder")
            )
          ),


          hidden(
            fluidRow(id = "mri_load",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         title = "Chargement du fichier IRM",
                         column(width = 8,
                                fileInput("input_img", "Choisir un fichier .img (format nifti)",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv",
                                            ".img"), width = NULL,
                                )),
                         column(width = 4,
                                uiOutput("contents"))
                     )
            )),

          hidden(
            fluidRow(id = "cut_selection",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         title = "Visualisation des coupes",
                         column(width = 12, uiOutput("cut_select_ui"),
                         div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("click_visu_cut", "Sélectionner")))

                     )
            )
          ),

          hidden(
            fluidRow(id = "cut_visu",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         imageOutput("image2")
                     )
            )
          ),

        ),


        tabItem(

          ### Tab 2 ###
          tabName = "prev_base",

          hidden(
            fluidRow(id = "patient_selected_rem2",
                     uiOutput("patient_select_reminder2")
            )
          ),


          fluidRow(
            box(width = 12, status = 'primary', solidHeader = TRUE,
                title = "Lancement de la prévision de démence",
                column(width = 12,
                div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("launch_prev_2_class", "Lancement")))
            )
          )
        ),


        tabItem(

          ### Tab 2 ###
          tabName = "prev_irm",

          hidden(
            fluidRow(id = "patient_selected_rem3",
                     uiOutput("patient_select_reminder3")
            )
          ),

          fluidRow(
            box(width = 12, status = 'primary', solidHeader = TRUE,
                title = "Lancement de la prévision de démence",
                column(width = 12,
                div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("launch_prev_2_class_irm", "Lancement"))),
                br(),
                br(),
                br(),
                uiOutput("prev")
            ),
            box(width = 12, status = 'primary', solidHeader = TRUE,
                title = "Prévisins pour chaque coupe",
                uiOutput("pred_tabs"))
          )
        ),



        ### tab ADMIN ###
        tabItem(
          tabName = "Admin",
          uiOutput('admin')
        )

    )
  )

}




ui <- shinydashboard::dashboardPage(

  shinydashboard::dashboardHeader(title = " - Dementia Project - ",
                                  tags$li(class = "dropdown", style = "padding: 8px;",
                                          Rshinytemplate::logoutUI("logout")) #,
  ),


  shinydashboard::dashboardSidebar(collapsed = TRUE,
                                   div(textOutput("welcome"), style = "padding: 20px"),

                                   #div("Onglets", style = "padding: 15px"),

                                   #onglets
                                   shinydashboard::sidebarMenu(id = "tabs",
                                                               shinydashboard::menuItem("Gestion des patients",tabName = "patient"),
                                                               shinydashboard::menuItem("Informations patient",tabName = "info"),
                                                               shinydashboard::menuItem("Chargement des IRM",tabName = "irm"),
                                                               shinydashboard::menuItem("Aide à la décision (Données Patient)",tabName = "prev_base"),
                                                               shinydashboard::menuItem("Aide à la décision (IRM)",tabName = "prev_irm"),
                                                               shinydashboard::menuItem("Administration",tabName = "Admin"))#,

  ),

  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              includeScript("returnClick.js"),
              tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')
    ),
    Rshinytemplate::loginUI("login"),
    htmlOutput("page"),
    tags$footer(paste0("Copyright Klara Vinceneux & Yoël Chetboun - v",  packageVersion("dementiaproject")), align = "center")

  )
)
