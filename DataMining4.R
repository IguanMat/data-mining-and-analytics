# Packages ----------------------------------------------------------------
packs <- c("shiny", "shinydashboard", "shinyWidgets", "DT", "shinycssloaders", "dplyr", "plotly", "ggplot2","magrittr", 
           "ggtext", "tidyverse", "jsonlite", "data.table", "htmltools",
           "plyr","tidyr","stringr","RColorBrewer","Hmisc","factoextra",
           "yardstick","FactoMineR","psych","vtable", "FactoInvestigate")
# lapply() returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X
# require() (and library) load and attach add-on packages
lapply(packs, require, character.only = TRUE)

# Data --------------------------------------------------------------------
data(hobbies)

hobby <- hobbies

for  (i in 1:17){
  hobbies[,i] <- revalue(x = as.character(hobbies[,i]), 
                         replace = c("0" = "NO", 
                                     "1" = "YES"))
}

v1 = hobbies %>% names()

mcahobb <- MCA(hobbies,quali.sup=19:22,quanti.sup=23,method="Burt")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(title = "Hobbies Dataset Analysis - Data Mining 4", 
                    dashboardHeader(title = tags$img(src="https://upload.wikimedia.org/wikipedia/commons/a/a2/Universit%C3%A0-LUMSA-logo.png", height = 45, align = "center"),
                                    titleWidth = 230,
                                    # the <li> tag defines a list item
                                    # the <a> tag defines a hyperlink, which is used to link from one page to another
                                    # _blank	opens the linked document in a new window or tab
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/matteogurrieri/", icon("linkedin"), "Matteo Gurrieri", target = "_blank")),
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/riccardo-bianchi-4928b0251/", icon("linkedin"), "Riccardo Bianchi", target = "_blank")),
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/edoardo-mercuri-b50a04256/", icon("linkedin"), "Edoardo Mercuri", target = "_blank")),
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/paolo-losacco-888278239/", icon("linkedin"), "Paolo Losacco", target = "_blank")),
                                    dropdownMenu(
                                      type = "message",
                                      messageItem(from = "LUMSA", message = "Clicca qui per vedere il nostro corso!", icon("education", lib = "glyphicon"), href = "https://www.lumsa.it/didattica/corsi-di-laurea/roma/triennale/tecniche-informatiche-gestione-dati"))
                    ),
                    dashboardSidebar(
                      sidebarMenu(style = "position: fixed;width: 230px;white-space: nowrap;overflow: visible;",
                        id = "sidebar",
                        menuItem("Dataset", tabName = "dataset", icon = icon("database")),
                        menuItem("Bar charts", tabName = "plots", icon = icon("stats", lib = "glyphicon")),
                        conditionalPanel("input.sidebar == 'plots'",selectInput(inputId = "var1", label = "Seleziona la variabile", choices = v1 )),
                        menuItem("MCA", tabName = "mca", icon = icon("search", lib = "glyphicon")),
                        menuItem("Grafici", tabName = "grafici", icon = icon("picture", lib = "glyphicon"),
                                 menuItem("Categories", tabName = "categories", icon = icon("caret-right")),
                                 menuItem("Contributes", tabName = "contributes", icon = icon("caret-right")),
                                 menuItem("Individuals", tabName = "individuals", icon = icon("caret-right")),
                                 menuItem("OtherDims", tabName = "dim", icon = icon("caret-right")),
                                 menuItem("Ellipses", tabName = "ell", icon = icon("caret-right")),
                                 menuItem("Screeplot", tabName = "scree", icon = icon("caret-right"))),
                        menuItem("Qualità", tabName = "qualita", icon = icon("screenshot", lib = "glyphicon")),
                        menuItem("Contributo", tabName = "contributo", icon = icon("signal", lib = "glyphicon"))
                      )
                    ),
                    dashboardBody(tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #FFFFFF;
                                position: fixed;
                                overflow: visible;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FFFFFF;
                                position: fixed;
                                overflow: visible;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #47352B;
                                /* position: fixed; 
                                overflow: visible;
                                width: 1285px; */
                                }        

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #47352B;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #FF6600;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #47352B;
                                color: #FFFFFF;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #FF781F;
                                }
                                
                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #47352B;
                                }
                                
                                '))),
                                  tabItems(
                                    tabItem(tabName = "dataset",
                                            tabBox(id = "t1", width = 12,
                                                   tabPanel("Info", icon = icon("info-sign", lib = "glyphicon"),
                                                            #  rows are created by the fluidRow() function and include columns defined by the column() function
                                                            fluidRow(
                                                              column(width = 8, tags$img(src="https://i.ibb.co/xSW8LSr/H-O-B-B-I-E-S.png", width = 600, height = 350), align = "center"),
                                                              column(width = 4,
                                                                     box(width = 6, status = "warning", solidHeader = T,tags$h4("Applicazione web Shiny sull'analisi del dataset",
                                                                                                                     tags$a("hobbies", href = "https://rdrr.io/cran/FactoMineR/man/hobbies.html"),br(),
                                                                     ), downloadButton('downloadMCA', 'Vuoi investigare?', icon = icon("search", lib = "glyphicon")),
                                                                     tags$h4("Questi dati sono disponibili installando il pacchetto",
                                                                             tags$a("FactoMineR", href = "https://cran.r-project.org/web/packages/FactoMineR/index.html")),
                                                                      downloadButton('downloadData', 'Download data')))
                                                            )
                                                   ),
                                                   tabPanel(title = "Dati", icon = icon("table"), DT::dataTableOutput("dataT")),
                                                   tabPanel(title = "Struttura", icon = icon("list-alt"), verbatimTextOutput("structure")),
                                                   tabPanel(title = "Statistiche descrittive", icon = icon("chart-pie"), verbatimTextOutput("summary"))
                                            )
                                    ),
                                    tabItem(tabName = "plots",
                                            tabBox(id = "t2", width = 12,
                                                   fluidRow(
                                                     column(width = 12, align = "center", offset = 2,
                                                            box(width = 8, status = "warning", solidHeader = T, title = textOutput("head1"), withSpinner(plotOutput("plotz"))))
                                                   ))),
                                    tabItem(tabName = "mca",
                                            tabBox(id = "t3", width = 12,
                                                            fluidRow(
                                                              column(width = 1,
                                                                     column(width = 1),
                                                                     column(width = 11, radioButtons(inputId = "select_type" , label = "Select: ", choices = c("summary", "description", "plots")))),
                                                                     column(width = 1),
                                                              column(width = 10, withSpinner(verbatimTextOutput("myselect")),
                                                              fluidRow(
                                                                column(width = 6, withSpinner(plotOutput("myplot1"))),
                                                                column(width = 6, withSpinner(plotOutput("myplot3")))))),
                                                              fluidRow( column(width = 12,
                                                                withSpinner(plotOutput("myplot2", height = 600))))
                                                              )
                                                            ),
                                    tabItem(tabName = "categories",
                                            tabBox(id = "t4", width = 12,
                                                   fluidRow(
                                                     column(width = 4, withSpinner(plotOutput("p3", height = 325))),
                                                     column(width = 8, withSpinner(plotOutput("p1", height = 325)))
                                                   ),
                                                   fluidRow(
                                                     column(width = 8, withSpinner(plotOutput("p2", height = 325))),
                                                     column(width = 4, withSpinner(plotOutput("p4", height = 325)))
                                                   ))),
                                    tabItem(tabName = "contributes",
                                            tabBox(id = "t5", width = 12,
                                                   fluidRow(
                                                     column(width = 6, withSpinner(plotOutput("p5", height = 600))),
                                                     column(width = 6, withSpinner(plotOutput("p6", height = 600)))
                                                   ))),
                                    tabItem(tabName = "individuals",
                                            tabBox(id = "t6", width = 12,
                                                   fluidRow(
                                                     column(width = 6, withSpinner(plotOutput("p7", height = 600))),
                                                     column(width = 6, withSpinner(plotOutput("p8", height = 600)))
                                                   ))),
                                    tabItem(tabName = "dim",
                                            tabBox(id = "t7", width = 12,
                                                   fluidRow(
                                                     column(width = 6, withSpinner(plotOutput("p9", height = 600))),
                                                     column(width = 6, withSpinner(plotOutput("p10", height = 300)), withSpinner(plotOutput("p18", height = 300)))
                                                   ))),
                                    tabItem(tabName = "ell",
                                            tabBox(id = "t8", width = 12,
                                                   fluidRow(
                                                     box(selectInput("var2","Seleziona variabile", choices = v1, multiple = FALSE, width = 250),
                                                         width = 12, status = "warning", solidHeader = T, withSpinner(plotOutput("p11", height = 550)))
                                                     #sliderInput("obs",
                                                      #           "Range:",
                                                      #           min = 1,
                                                      #           max = 23,
                                                      #           value = 1),
                                                     #sliderInput("obs2",
                                                      #           "Range:",
                                                      #           min = 1,
                                                      #           max = 23,
                                                      #           value = 6),
                                                   ))),
                                    tabItem(tabName = "scree",
                                            tabBox(id = "t9", width = 12,
                                                   fluidRow(
                                                     box(width = 12, status = "warning", solidHeader = T,withSpinner(plotOutput("p12", height = 600)))
                                                   ))),
                                    tabItem(tabName = "qualita",
                                            tabBox(id = "t10", width = 12,
                                                   fluidRow(
                                                     box(width = 12, status = "warning", solidHeader = T,withSpinner(plotOutput("p13", height = 600)))
                                                   ))),
                                    tabItem(tabName = "contributo",
                                            tabBox(id = "t11", width = 12,
                                                   tabPanel(title = "About", icon = icon("chevron-down"),
                                                            fluidRow(
                                                              column(width = 6, withSpinner(plotOutput("p14", height = 300))),
                                                              column(width = 6, withSpinner(plotOutput("p15", height = 300)))
                                                            ),
                                                            fluidRow(
                                                              column(width = 8, offset = 2, withSpinner(plotOutput("p16", height = 300))))
                                                            ),
                                                   tabPanel(title = "Total", icon = icon("chevron-down"),
                                                            fluidRow(box(width = 12, status = "warning", solidHeader = T,withSpinner(plotOutput("p17", height = 600))))))),
                                    tabItem(tabName = "conclusioni",
                                            tabBox(id = "t12", width = 12,
                                                   ))
                                  )
                    )
)

# SERVER ----------------------------------------------------------------------
server <- function(input, output) {
  
  # DataTable
  output$dataT <- DT::renderDataTable(
    DT::datatable(hobby, options=list(scrollX = T)) 
  )
  
  # Structure
  output$structure <- renderPrint({
    hobby %>%
      str()
  })
  
  # Summary
  output$summary <- renderPrint(
    hobby %>%
      summary()
  )
  
  output$downloadMCA <- downloadHandler(
    filename = function() { 
      paste("mca-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      Investigate(mcahobb)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(hobbies, file)
    })
  
  output$head1 <- renderText(
    paste("Bar chart di ", input$var1)
  )
  
  output$plotz <- renderPlot({
    ggplot(hobbies, aes(get(input$var1), fill = get(input$var1))) +
      geom_bar(show.legend = FALSE) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.2, colour = "white") +
      labs(x = paste(input$var1)) + theme_minimal()
  })
  
  output$mca_summary <- renderPrint(
    summary(mcahobb, ncp=3) 
  )
  
  output$mca_graph <- renderPlot(
    MCA(hobbies,quali.sup=19:22,quanti.sup=23,method="Burt")
  )
  
  output$myselect <- renderPrint({
    if (input$select_type == "summary") {
      summary(mcahobb, ncp=3) 
    } else if (input$select_type == "description") {
      dimdesc(mcahobb)
    }
  })
  
  output$myplot1 <- renderPlot({
    if (input$select_type == "plots") {
      MCA(hobbies,quali.sup=19:22,quanti.sup=23,method="Burt")
    }
  })
  
  output$myplot2 <- renderPlot({
    if (input$select_type == "plots") {
      plot(mcahobb, invisible=c("ind"),autoLab = "y",cex=0.75,title="MCA factor map")
    }
  })
  
  output$myplot3 <- renderPlot({
    if (input$select_type == "plots") {
      plot.MCA(mcahobb,choix = ("var"))
    }
  })
  
  output$p1 <- renderPlot({
    plot(mcahobb,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red"),17),cex = 0.8,"black",rep("red",4)),
         title="Active categories")
  })
  
  output$p2 <- renderPlot({
    plot(mcahobb, invisible=c("ind","var"),autoLab="y",cex=0.8,title="Supplementary categories")
  })
  
  output$p3 <- renderPlot({
    plot(mcahobb, choix="var",xlim=c(0,0.5),ylim=c(0,0.5))
  })
  
  output$p4 <- renderPlot({
    plot(mcahobb, choix="var",xlim=c(0,0.5),ylim=c(0,0.5),invisible=c("quali.sup","quanti.sup"))
  })
  
  output$p5 <- renderPlot({
    plot(mcahobb, invisible="ind",autoLab="y",cex=0.8,selectMod="cos2 10")
  })
  
  output$p6 <- renderPlot({
    plot(mcahobb, invisible="ind",autoLab="y",cex=0.8,selectMod="contrib 20")
  })
  
  output$p7 <- renderPlot({
    plot(mcahobb, invisible=c("var","quali.sup"),autoLab="y",cex=0.7,select="cos2 20")
  })
  
  output$p8 <- renderPlot({
    plot(mcahobb, autoLab="y",cex=0.9, select="cos2 20", selectMod="cos2 10")
  })
  
  output$p9 <- renderPlot({
    plot(mcahobb, invisible="ind",autoLab="y",cex=0.75,selectMod="cos2 20",axes=3:4)
  })
  
  output$p10 <- renderPlot({
    plot(mcahobb,invisible=c("var","quali.sup"),cex=0.75,select="contrib 20",axes=3:4)
  })
  
  output$p11 <- renderPlot({
    fviz_mca_ind(mcahobb, label="none", habillage=as.factor(hobbies[, input$var2]),
                 addEllipses=TRUE, ellipse.level=0.95)
    #plotellipses(mcahobb,keepvar=c(input$obs:input$obs2))
  })
  
  output$p12 <- renderPlot({
    fviz_screeplot(mcahobb, addlabels = TRUE, ylim = c(0, 45))
  })
  
  output$p13 <- renderPlot({
    fviz_mca_var(mcahobb, col.var = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                 repel = TRUE, # Avoid text overlapping
                 ggtheme = theme_minimal())  #separato
  })
  
  output$p14 <- renderPlot({
    fviz_contrib(mcahobb, choice = "var", axes = 1, top = 17)
  })
  
  output$p15 <- renderPlot({
    fviz_contrib(mcahobb, choice = "var", axes = 2, top = 17)
  })
  
  output$p16 <- renderPlot({
    fviz_contrib(mcahobb, choice = "var", axes = 1:2, top = 17)
  })
  
  output$p17 <- renderPlot({
    fviz_mca_var(mcahobb, col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                 repel = TRUE, # avoid text overlapping (slow)
                 ggtheme = theme_minimal()
    )
  })
  
  output$p18 <- renderPlot({
    plot.MCA(mcahobb,choix = "quanti.sup",axes=3:4)
  })
  
}

shinyApp(ui = ui, server = server)

