
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
                title = "Informations sur la base de patients",
                fluidRow(column(width = 12, selectInput(inputId = "select_var", label = "Sélectionner une variable d'intérêt :", choices = c("Age", "Taille", "Poids", "Entêtement","Dépression", "Anxiété", "Apathie", "Désinhibé",
                                                                                                                      "Irritable", "Argent", "Factures", "Shopping", "Jeu", "Repas",
                                                                                                                      "Evénements", "Concentration", "Souvenir dates", "Déplacements", "Autonomie") ,multiple = FALSE))),

                fluidRow(column(width = 12, plotOutput("ggplot_var") ))

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
                         fluidRow(
                         column(width = 8,
                                #uiOutput("file_in")),

                                fileInput("input_img", "Choisir un fichier .img (format nifti)",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv",
                                            ".img"), width = NULL,
                                )),
                         column(width = 4,
                                uiOutput("contents")))
                     )
            )),

          hidden(
            fluidRow(id = "cut_selection",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         title = "Visualisation des coupes",
                         fluidRow(

                         column(width = 12, uiOutput("cut_select_ui"),
                         div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("click_visu_cut", "Sélectionner"))))

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

          hidden(
            fluidRow(id = "ihm_prev_patient",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         title = "Lancement de la prévision de démence",
                         fluidRow(
                           column(width = 12,
                                  div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("launch_prev_2_class_patient", "2 Classes", width = "160px")), " ",
                                  div(style="display: inline-block; vertical-align:top; width: 150px; margin-left:30px", actionButton("launch_prev_3_class_patient", "3 Classes", width = "160px")) )),
                         fluidRow(br(), uiOutput("prev_patient"))
                     ))
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

          hidden(
            fluidRow(id = "ihm_prev_irm",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         title = "Lancement de la prévision de démence",
                         fluidRow(
                           column(width = 12,
                                  div(style="display: inline-block; vertical-align:top; width: 150px;", actionButton("launch_prev_2_class_irm", "2 Classes", width = "160px")), " ",
                                  div(style="display: inline-block; vertical-align:top; width: 150px; margin-left:30px", actionButton("launch_prev_3_class_irm", "3 Classes", width = "160px")) )),

                         fluidRow(br(), uiOutput("prev"))
                     ))
          ),



          hidden(
            fluidRow(id = "prev_coupe",
                     box(width = 12, status = 'primary', solidHeader = TRUE,
                         title = "Prévision par coupe",
                         uiOutput("pred_tabs"))
            )
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
                                                               shinydashboard::menuItem("Informations base patients",tabName = "info"),
                                                               shinydashboard::menuItem("Chargement des IRM",tabName = "irm"),
                                                               shinydashboard::menuItem("Aide à la décision (Patient)",tabName = "prev_base"),
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
