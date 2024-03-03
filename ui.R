#Application for Unified fracture design

library(shiny)
library(mathjaxr)
library(openxlsx)
library(shinydashboard)
library(noteMD)
library(plotly)


shinyUI(
dashboardPage (
    skin = "green",
    dashboardHeader(title = "Fracture Design"),
    
    dashboardSidebar(
      sidebarMenu(
        
        # Select the language
        menuItem("English", tabName = "English",
                 menuSubItem("Input Data", tabName = "InputDataEng"),
                 menuSubItem("Results", tabName = "ResultsEng")),
        menuItem("Русский", tabName = "Русский",
                 menuSubItem("Входные данные", tabName = "InputDataRU"),
                 menuSubItem("Результаты", tabName = "ResultsRu"))
      )
    ),
    dashboardBody(
      
      # Font style
      tags$style(
        "p, div, h4, h3, h2, h1, pre {
    # color: #27AE60;
     font-family: 'Montserrat';
     
      font-size: 16px;
      
    }"),
      
      # Math
      tags$style(
        "span {
      # color: #27AE60;
     font-family: 'Montserrat';
     letter-spacing: .1rem;
      font-size: 16px;

    }"),
      
      tags$head(tags$style(HTML('
    
    @font-face {  
  src: url(Montserrat-VariableFont_wght.ttf);
  font-family: "Montserrat";
}
      .main-header .logo {
      font-family: "Montserrat";
      
        
        font-weight: bold;
        font-size: 18px;
      }
    '))),
      
      tabItems(
        
        #################################English##################################
        # Input data
        tabItem(tabName = "InputDataEng",
                h3("Input Data"),
                fluidRow(
                  
                  column (4,
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Proppant mass for (two wings), \\(kg\\)',"</p>")),
                                numericInput("M_en", label = NULL, value = 50000, min = 0),
                                HTML(paste("<p>",'Specific gravity of proppant material',"</p>")),
                                numericInput("pprop_en", label = NULL, value = 2.65, min = 0, step = 0.05),
                                HTML(paste("<p>",'Porosity of proppant pack',"</p>")),
                                numericInput("por_en", label = NULL, value = 0.38, min = 0, max = 1, step = 0.05),
                                HTML(paste("<p>",'Proppant pack permeability, \\(mD\\)',"</p>")),
                                numericInput("Kf_en", label = NULL, value = 40000, min = 0),
                                HTML(paste("<p>",'Max propp diameter, \\(mm\\)',"</p>")),
                                numericInput("Dpmax_en", label = NULL, value = 0.3, min = 0, step = 0.05),
                                HTML(paste("<p>",'Rock permeability, \\(mD\\)',"</p>")),
                                numericInput("K_en", label = NULL, value = 2, min = 0),
                                HTML(paste("<p>",'Permeable (leakoff) thickness, \\(m\\)',"</p>")),
                                numericInput("hp_en", label = NULL, value = 25, min = 0)), 
                            width = NULL,height = 600, collapsible = TRUE, collapsed = FALSE)),
                  
                  column (4,
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Well radius, \\(m\\)',"</p>")),
                                numericInput("rw_en", label = NULL, value = 0.1, min = 0, step = 0.01),
                                HTML(paste("<p>",'Well drainage radius, \\(m\\)',"</p>")),
                                numericInput("re_en", label = NULL, value = 200, min = 0),
                                HTML(paste("<p>",'Pre-treatment skin factor',"</p>")),
                                numericInput("skin_en", label = NULL, value = 0),
                                HTML(paste("<p>",'Fracture height, \\(m\\)',"</p>")),
                                numericInput("hf_en", label = NULL, value = 40, min = 0),
                                HTML(paste("<p>",'Plane strain modulus, \\(atm\\)',"</p>")),
                                numericInput("Ep_en", label = NULL, value = 1.4e+5, min = 0, step = 1000),
                                HTML(paste("<p>",'Slurry injection rate, \\(m^3/min\\)',"</p>")),
                                numericInput("qi_en", label = NULL, value = 3.2, min = 0, step = 0.5),
                                HTML(paste("<p>",'Rheology \\(K`\\), \\((N \\cdot s^{n`})/m^2 \\)',"</p>")),
                                numericInput("Kp_en", label = NULL, value = 3.35, step = 0.05)), 
                                width = NULL,height = 600, collapsible = TRUE, collapsed = FALSE)),
                  
                  column (4,
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Rheology, \\(n\\)',"</p>")),
                                numericInput("np_en", label = NULL, value = 0.45, step = 0.05),
                                HTML(paste("<p>",'Leakoff coefficient in permeable layer, \\(m/min^\\frac{1}{2}\\)',"</p>")),
                                numericInput("CL_en", label = NULL, value = 1.52e-3, min = 0, step = 0.0001),
                                HTML(paste("<p>",'Spurt loss coefficient,  \\(m^3/m^2 \\)',"</p>")),
                                numericInput("Sp_en", label = NULL, value = 4.1e-4, min = 0, step = 0.00005),
                                HTML(paste("<p>",'Max possible added proppant concentration, \\(kg/m^3 \\) neat fluid',"</p>")),
                                numericInput("cmax_en", label = NULL, value = 2100, min = 0),
                                HTML(paste("<p>",'Multiply opt length by factor',"</p>")),
                                numericInput("elong_en", label = NULL, value = 1, min = 0, step = 0.1),
                                HTML(paste("<p>",'Multiply Nolte pad by factor',"</p>")),
                                numericInput("Nolte_en", label = NULL, value = 1)),
                            width = NULL,height = 600, collapsible = TRUE, collapsed = FALSE))
        )),
        
        # Results
        tabItem(tabName = "ResultsEng",
                h3("Optimization results"),
                fluidRow(
                  column (6, 
                          box( div(style = 'height: 100%; width: 100%',
                                #Вывод таблиц
                                tabsetPanel(type = "tabs",
                                            tabPanel (
                                              "Optimal placement",
                                              tableOutput("optvalues_en")
                                            ),
                                            tabPanel (
                                              "Actual placement",
                                              tableOutput("actvalues_en")
                                            ),
                                            tabPanel (
                                              "Treatment mode",
                                              tableOutput("mode_en")
                                               )),
                                
                                   verbatimTextOutput("text_en"),br()),
                               width = NULL,  height = 500, collapsible = TRUE, collapsed = FALSE), 
                          
                          
                          box(div(style = 'height: 100%; width: 100%', 
                                  column(12,
                                         helpText("Note: All entered text will be displayed in the report") ),
                                  column(12,
                                         tags$textarea(
                                           "Enter your comment to the calculations",
                                           id    = 'markdowninput_en',
                                           rows  = 3,
                                           style = 'width:100%;')),
                                  helpText("Preview:"),
                                  htmlOutput('htmlmarkdown_en'),br(),br()), width = NULL, height = 250, collapsible = TRUE, collapsed = FALSE)
                  ), 
                  
                  column (6, 
                          box(div (style = 'height: 100%; width: 100%', 
                                   img(src="Economides.png", style = "height: 100%; width: 100%")),
                              width = NULL, height = 500, collapsible = TRUE, collapsed = FALSE),
                          
                          box(div (style = 'height: 100%; width: 100%', 
                                   column (6, 
                                           downloadButton('describe_download_en',"Download Excel Report",class="butt" ), br(),
                                           tags$head(tags$style(".butt{background-color:#27AE60;} .butt{color: #000000;}"))),
                                   
                                   column (6, 
                                           downloadButton('describe_download_test_en',"Download Word Report",class="butt" ), br(), 
                                           tags$head(tags$style(".butt{background-color:##27AE60;} .butt{color: #000000;}")))),
                              width = NULL, height = 250, collapsible = TRUE, collapsed = FALSE)
                  )
                  
                )
        ), 
        
        #################################Русский##################################
        # Входные данные
        tabItem(tabName = "InputDataRU",
                h3("Входные данные"),
                fluidRow(  
                  
                  column (4,
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Масса проппанта (для 2 крыльев трещины), \\(кг\\)',"</p>")),
                                numericInput("M", label = NULL, value = 50000, min = 0),
                                HTML(paste("<p>",'Удельная плотность проппанта, \\(д.ед\\)',"</p>")),
                                numericInput("pprop", label = NULL, value = 2.65, min = 0, step = 0.05),
                                HTML(paste("<p>",'Пористость упаковки проппанта, \\(д.ед\\)',"</p>")),
                                numericInput("por", label = NULL, value = 0.38, min = 0, max = 1, step = 0.05),
                                HTML(paste("<p>",'Проницаемость упаковки проппанта, \\(мД\\)',"</p>")),
                                numericInput("Kf", label = NULL, value = 40000, min = 0),
                                HTML(paste("<p>",'Максимальный диаметр проппанта, \\(мм\\)',"</p>")),
                                numericInput("Dpmax", label = NULL, value = 0.3, min = 0, step = 0.05),
                                HTML(paste("<p>",'Проницаемость породы, \\(мД\\)',"</p>")),
                                numericInput("K", label = NULL, value = 2, min = 0),
                                HTML(paste("<p>",'Проницаемая мощность (утечек), \\(м\\)',"</p>")),
                                numericInput("hp", label = NULL, value = 25, min = 0)), 
                            width = NULL,height = 650, collapsible = TRUE, collapsed = FALSE)),
                  
                  column (4,
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Радиус скважины, \\(м\\)',"</p>")),
                                numericInput("rw", label = NULL, value = 0.1, min = 0, step = 0.01),
                                HTML(paste("<p>",'Радиус контура питания, \\(м\\)',"</p>")),
                                numericInput("re", label = NULL, value = 200, min = 0),
                                HTML(paste("<p>",'Скин-фактор до обработки',"</p>")),
                                numericInput("skin", label = NULL, value = 0),
                                HTML(paste("<p>",'Высота трещины, \\(м\\)',"</p>")),
                                numericInput("hf", label = NULL, value = 40, min = 0),
                                HTML(paste("<p>",'Модуль плоской деформации, \\(атм\\)',"</p>")),
                                numericInput("Ep", label = NULL, value = 1.4e+5, min = 0, step = 1000),
                                HTML(paste("<p>",'Скорость закачки смеси, \\(м^3/мин\\)',"</p>")),
                                numericInput("qi", label = NULL, value = 3.2, min = 0, step = 0.5),
                                HTML(paste("<p>",'Показатель реологии \\(K`\\), \\((Н \\cdot с^{n`})/м^2 \\)',"</p>")),
                                numericInput("Kp", label = NULL, value = 3.35, step = 0.05)), 
                            width = NULL,height = 650, collapsible = TRUE, collapsed = FALSE)),
                  
                  column (4,
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Показатель реологии, \\(n`\\)',"</p>")),
                                numericInput("np", label = NULL, value = 0.45, step = 0.05),
                                HTML(paste("<p>",'Коэффициент утечек в проницаемом слое, \\(м/мин^\\frac{1}{2}\\)',"</p>")),
                                numericInput("CL", label = NULL, value = 1.52e-3, min = 0, step = 0.0001),
                                HTML(paste("<p>",'Коэффициент мгновенной водоотдачи \\(S_p\\),\\(м^3/м^2 \\)',"</p>")),
                                numericInput("Sp", label = NULL, value = 4.1e-4, min = 0, step = 0.00005),
                                HTML(paste("<p>",'Максимально возможная концентрация проппанта, \\(кг/м^3 \\) чистой жидкости',"</p>")),
                                numericInput("cmax", label = NULL, value = 2100, min = 0),
                                HTML(paste("<p>",'Коэффициент увеличения оптимальной длины',"</p>")),
                                numericInput("elong", label = NULL, value = 1, min = 0, step = 0.1),
                                HTML(paste("<p>",'Коэффициент увеличения поправки Нолта',"</p>")),
                                numericInput("Nolte", label = NULL, value = 1)),
                            width = NULL,height = 650, collapsible = TRUE, collapsed = FALSE))
                  
                )),
        
        # Результаты
        tabItem(tabName = "ResultsRu", 
                h3 ("Результаты оптимизации"),
                fluidRow(
                  column (6, 
                          box( div(style = 'height: 100%; width: 100%',
                                   #Вывод таблиц
                                   tabsetPanel(type = "tabs",
                                               tabPanel (
                                                 "Оптимальное размещение",
                                                 tableOutput("optvalues")
                                               ),
                                               tabPanel (
                                                 "Реальное размещение",
                                                 tableOutput("actvalues")
                                               ),
                                               tabPanel (
                                                 "Режим обработки",
                                                 tableOutput("mode")
                                               )),
                                   
                                   verbatimTextOutput("text"),br()),
                               width = NULL,  height = 630, collapsible = TRUE, collapsed = FALSE), 
                          
                          
                          box(div(style = 'height: 100%; width: 100%', 
                                  column(12,
                                         helpText("Note: All entered text will be displayed in the report") ),
                                  column(12,
                                         tags$textarea(
                                           "Enter your comment to the calculations",
                                           id    = 'markdowninput',
                                           rows  = 3,
                                           style = 'width:100%;')),
                                  helpText("Preview:"),
                                  htmlOutput('htmlmarkdown'),br(),br()), width = NULL, height = 250, collapsible = TRUE, collapsed = FALSE)
                  ), 
                  
                  column (6, 
                          box(div (style = 'height: 100%; width: 100%', 
                                   img(src="Economides_Rus.png", style = "height: 100%; width: 100%")),
                              width = NULL, height = 630, collapsible = TRUE, collapsed = FALSE),
                          
                          box(div (style = 'height: 100%; width: 100%', 
                                   column (6, 
                                           downloadButton('describe_download',"Скачать отчет в Excel",class="butt"), br(),
                                           tags$head(tags$style(".butt{background-color:#27AE60;} .butt{color: #000000;}"))),
                                   
                                   column (6, 
                                           downloadButton('describe_download_test',"Скачать отчет в Word",class="butt"), br(), 
                                           tags$head(tags$style(".butt{background-color:##27AE60;} .butt{color: #000000;}")))),
                              width = NULL, height = 250, collapsible = TRUE, collapsed = FALSE)
                  )
                  
                )
        )
      )
    ))

)
