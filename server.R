#Application for Unified fracture design

library(shiny)
library(mathjaxr)
library(openxlsx)
library(shinydashboard)
library(noteMD)
library(plotly)


shinyServer(
  
function(input, output) {
  
  
  #######################################!ENGLISH############################################# 
  ##################################OPTIMAL##################################
  
  rp_en = reactive ({ DLrp1 (input$hp_en, input$hf_en) })
  
  Vf_en <- reactive ({
    rp_en() * input$M_en / 2 / ((1 - input$por_en) * (1000 * input$pprop_en)) 
  }) 
  
  Vr_en <- reactive ({
    input$hp_en * input$re_en ^ 2 * pi 
  }) 
  
  #Dimensional proppant number
  Nprop_en <- reactive ({
    (4 * Vf_en() * input$Kf_en) / (input$K_en * Vr_en())
  })
  
  #Optimal parameters   
  Cfdopt_en <- reactive ({ FracOpt(Nprop_en()) })
  Ixopt_en <- reactive ({ sqrt (Nprop_en() / Cfdopt_en()) })
  Jdopt_en <- reactive ({ Jd(Nprop_en(), Cfdopt_en()) })
  
  xf_en = reactive ({ DLx (Cfdopt_en(), input$Kf_en, Vf_en(), input$K_en, input$hp_en) })
  w_en = reactive ({ DLw (Cfdopt_en(), input$Kf_en, Vf_en(), input$K_en, input$hp_en) })
  
  #Pseudoskin-Factor
  sf_en = reactive ({
    1 / Jdopt_en() - log(input$re_en / input$rw_en) + 3 / 4
  })
  
  #Ratio of productivity increase
  Jdd_en = reactive ({
    (log(input$re_en / input$rw_en) - 3 / 4 + input$skin_en) / (log(input$re_en / input$rw_en) - 3 / 4 + sf_en())
  })
  
  Name_en = as.character (
    c("Proppant number Nprop",
      "Dimensionless productivity index Jdopt",
      "Optimal dimensionless fracture conductivity Cfdopt",
      "Optimal half length xfopt, m",
      "Optimal propped width wopt, m",
      "Post treatment pseudo skin factor sf",
      "Multiplicity of productivity increase")
  )
  Value_en = reactive ({ as.numeric(
    c(Nprop_en(),
      Jdopt_en(),
      Cfdopt_en(),
      xf_en(), 
      w_en(), 
      sf_en(),
      Jdd_en())
  ) })
  
  Value1_en <- reactive ({
    format(Value_en(),digits = 2)
  })
  
  df_en <- reactive({
    data.frame(
      Name_en, Value1_en(), stringsAsFactors = FALSE)
    
  })
  
  output$optvalues_en <- renderTable({
    df_en=df_en()
    colnames(df_en)<-c('Parameter','Value')
    df_en
  })
  
  
  ##################################ACTUAL##########################  
  #Real placement of proppant in the fracture 
  
  xfa_en = reactive ({xf_en() * input$elong_en})
  
  ww0_en= reactive ({ DLw1 (input$Ep_en, input$hf_en, input$Kp_en, input$np, input$qi_en, xf_en())  })
  
  wb_en = reactive ({
    0.628 * ww0_en()
  })
  
  ww_en = reactive ({
    ww0_en() * pi / 4
  })
  
  ce_en = reactive ({
    input$M_en / 2 / (input$hf_en * wb_en() * xfa_en())
  })
  
  #Maximum proppant concentration, kg/m3 of slurry volume
  cemax_en = reactive ({
    input$cmax_en / (1 + input$cmax_en / (1000 * input$pprop_en))
  })
  
  ceswitch_en <- reactive ({
    if (ce_en() > cemax_en()) {
      ceswitch_en = 1 
    } else { 
      ceswitch_en = 0}
  })
  
  Ma_en <- reactive ({
    if (ce_en() > cemax_en()) {
      Ma_en <- input$M_en / 2 * cemax_en() / ce_en()
    } else 
      Ma_en <- input$M_en/2
  })
  
  ce1_en <- reactive ({
    if (ce_en() > cemax_en()) {
      ce1_en = Ma_en() / (input$hf_en * wb_en() * xfa_en())
    } else {
      ce1_en = input$M_en / 2 / (input$hf_en * wb_en() * xfa_en())
    }
  })
  
  Vfa_en = reactive ({
    rp_en() * Ma_en() / ((1 - input$por_en) * (1000 * input$pprop_en))
  }) 
  
  wa_en = reactive ({
    Vfa_en() / (xfa_en() * input$hp_en)
  })
  
  Npropa_en = reactive ({
    (4 * Vfa_en() * input$Kf_en) / (input$K_en * Vr_en())
  })
  
  Cfda_en = reactive ({
    input$Kf_en * Vfa_en() / (xfa_en() * input$hp_en) / (input$K_en * xfa_en())
  })
  
  Ixa_en = 0
  
  #FracPI
  FracPI <- function (Nprop_ena, Cfda, Ixa) {
    if (Nprop_ena == 0) {
      Cfd = Cfda
      Ix = Ixa
      Nprop_en = Cfd * Ix ^ 2
    }
    if (Cfda == 0) {
      Nprop_en = Nprop_ena
      Ix = Ixa
      Cfd = Nprop_en / Ix ^ 2 
    }
    if (Ixa == 0) {
      Nprop_en = Nprop_ena
      Cfd = Cfda
      Ix = sqrt (Nprop_en / Cfd)
    }
    Jda <- Jd(Nprop_en, Cfd)
  }
  
  Jda_en <- reactive ({ 
    FracPI (Npropa_en(), Cfda_en(), Ixa_en)
  })
  
  #Pseudoskin and multiples of productivity increase
  sfa_en = reactive ({
    1 / Jda_en() - log(input$re_en / input$rw_en) + 3 / 4
  })
  
  Jdda_en = reactive ({
    (log(input$re_en / input$rw_en) - 3 / 4 + input$skin_en) / (log(input$re_en / input$rw_en) - 3 / 4 + sfa_en())
  })
  
  #Real placement table
  N_en = as.character (
    c("Proppant mass placed (2 wing), kg",
      "Proppant number Nprop",
      "Dimensionless productivity index Jdopt",
      "Dimensionless fracture conductivity Cfd",
      "Half length xf, m",
      "Propped width w, m",
      "Post treatment pseudo skin factor sf",
      "Multiplicity of productivity increase")
  )
  
  VA_en = reactive ({ as.numeric(
    c(2*Ma_en(),
      Npropa_en(),
      Jda_en(),
      Cfda_en(),
      xfa_en(), 
      wa_en(), 
      sfa_en(),
      Jdda_en())
  ) })
  
  VA1_en <- reactive ({
    format(VA_en(),digits = 2, scientific = F)
  })
  
  dfA_en <- reactive({
    data.frame(
      N_en, VA1_en(), stringsAsFactors = FALSE)
    
  })
  
  output$actvalues_en <- renderTable({
    dfA_en=dfA_en()
    colnames(dfA_en)<-c('Parameter','Value')
    dfA_en
  })
  
  output$text_en <- renderText ({
    
    if (ceswitch_en() == 0 & input$elong_en == 1) {
      paste("Constraints allow optimum placement")
    } else if (ceswitch_en() == 1 & input$elong_en == 1) {
      paste("Suboptimal placement with constraints satisfied \nMass of proppant reduced")
    } else if (ceswitch_en() == 1 & input$elong_en != 1) {
      paste("Suboptimal placement with constraints satisfied \nFracture length changed")
    }
    else{
      paste("...")
    }
  })
  
  
  ######################TREATMENT MODE#########################
  
  te_en <- reactive ({ DLpmp1 (input$qi_en, input$hf_en, xf_en(), wb_en(), input$CL_en, input$Sp_en, rp_en()) })
  
  eta_en <- reactive ({
    input$hf_en * xf_en() * wb_en() / ((input$qi_en/2/60) * te_en())
  })
  
  eps2_en <- reactive ({
    (1 - eta_en()) / (input$Nolte_en + eta_en())
  })
  
  
  eps1_en <- reactive ({
    eps2_en()*input$Nolte_en
  })
  
  tpad_en = reactive ({
    te_en() * eps1_en()
  })
  
  ca_en = reactive ({
    ce1_en() / (1 - ce1_en() / (1000 * input$pprop_en))
  })
  
  pn_en = reactive ({
    input$Ep_en * 98066 * wb_en() / (2 * input$hf_en * 0.628)
  })
  
  ft = 0.3048
  lbm = 0.4535924
  gallon = 0.00378541
  psi = 6894.757
  
  uc_en <- reactive ({
    Ma_en()/(xfa_en()*input$hf_en*wb_en())
  })
  
  ac_en <- reactive ({
    Ma_en() / (xfa_en() * input$hf_en) 
  })
  
  mc_en = reactive ({
    ca_en()
  })
  
  netp_en <- reactive ({
    pn_en() / 98066
  })
  
  
  #Processing mode table
  NR_en = as.character (
    c("Efficiency, eta, %",
      "Pumping time, te,  min",
      "Pad pumping time, te,  min",
      "Exponent of added proppant concentration, eps",
      "Uniform proppant concentration in frac at end, kg/m3",
      "Areal proppant concentration after closure, kg/m2",
      "Max added proppant concentration, kg/m3  neat fluid",
      "Net pressure at end of pumping, atm")
  )
  
  ValueR_en = reactive ({ as.numeric(
    c(eta()*100,
      te()/60,
      tpad()/60,
      eps2(),
      uc(),
      ac(),
      mc(),
      netp()
    )
  ) })
  
  ValueR1_en <- reactive ({
    format(ValueR_en(),digits = 4, scientific = F)
  })
  
  dfR_en <- reactive({
    data.frame(
      NR_en, ValueR1_en(), stringsAsFactors = FALSE)
    
  })
  
  output$mode_en <- renderTable({
    dfR_en=dfR_en()
    colnames(dfR_en)<-c('Parameter','Value')
    dfR_en
  })
  
  
  #Excel file style
  hs_en <- createStyle(
    textDecoration = "BOLD", fontColour = "#000000", fontSize = 12,
    # fontName = "Times New Roman", 
    fgFill = "#9ACD32"
  )
  
  
  #Forming a file for download
  #2 tabs in Excel with separate tables on each tab
  output$describe_download_en <- downloadHandler(
    filename = function() {
      paste('Results', ".xlsx", sep = "")
    },
    
    content = function(file) {
      df_en <- df_en()
      colnames(df_en)<-c('Parameter','Value')
      dfA_en <- dfA_en()
      colnames(dfA_en)<-c('Parameter','Value')
      dfR_en <- dfR_en()
      colnames(dfR_en)<-c('Parameter','Value')
      list_of_datasets_en <- list("Optimal placement" = df_en, "Actual placement" = dfA_en, "Treatmant mode" = dfR_en)
      write.xlsx(list_of_datasets_en, file, halign='center', colWidths = "auto",borders = "all", fontName = "Times New Roman",headerStyle = hs, firstRow=TRUE, firstCol=TRUE)
    }
  )
  
  #Word report
  output$describe_download_test_en = downloadHandler(
    filename<- function(){
      paste("Report",Sys.Date(), '.docx',sep = "") #File name
    },
    
    content = function(file) {
      
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     ## End of progression
                     src <- normalizePath('Report_en.Rmd')
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'Report_en.Rmd', overwrite = TRUE)
                     
                     library(rmarkdown)
                     out <- render('Report_en.Rmd', word_document())
                     file.rename(out, file)
                   })
    })
  
  #Comment output
  output$htmlmarkdown_en = reactive({
    note_in_html(input$markdowninput_en)
  })

  
################################################!РУССКИЙ##############################################
  
#################################################ОПТИМАЛЬНОЕ РАЗМЕЩЕНИЕ#######################################
  
  DLrp1 <- function(hp, hf) {
    
    if (hp <= hf){
      rp <- hp / hf
    } else {
      rp <- 1
    }
  }
  rp = reactive ({ DLrp1 (input$hp, input$hf) })
  
  Vf <- reactive ({
    rp() * input$M / 2 / ((1 - input$por) * (1000 * input$pprop)) 
  }) 
  
  Vr <- reactive ({
    input$hp * input$re ^ 2 * pi 
  }) 
  
  #Безразмерное число проппанта
  Nprop <- reactive ({
    (4 * Vf() * input$Kf) / (input$K * Vr())
  })
  
  #Безразмерная продуктивность 
  Jd <- function (Nprop, Cfd) {
    if (Nprop <= 0.1) {Jd = Jd1 (Nprop, Cfd)}
    else if (Nprop <= 1) {Jd = Jd2 (Nprop, Cfd)}
    else if (Nprop <= 10) {Jd = Jd3 (Nprop, Cfd)}
    else {Jd = Jd3 (Nprop, Cfd)}
  }
  
  #Функция Синко-Лея (f-фактор)
  fCL <- function (Cfd) {
    num = 1.65 - 0.328 * log(Cfd) + 0.116 * (log(Cfd)) ^ 2
    denom = 1 + 0.18 * log(Cfd) + 0.064 * (log(Cfd)) ^ 2 + 0.005 * (log(Cfd)) ^ 3
    fCL = num / denom
  }
  
  #Jd, вычисленное для Ix = 1
  Jd_Ix1 <- function (Nprop) {
    if (Nprop < 241) {
      Jd_Ix1 = 1.13576 - 0.0529836 * log(Nprop) - 0.912619 * atan(0.37403 - 0.494469 * log(Nprop))
    } else {
      Jd_Ix1 = 1.909859
    }
  }
  
  #g1 function 
  g1 <- function (Ix) {
    g1 = 3.357 * Ix ^ 5 - 10.686 * Ix ^ 4 + 10.041 * Ix ^ 3 - 2.1452 * Ix ^ 2 + 0.449 * Ix - 0.01452
  }
  
  #g10 function 
  g10 <- function (Ix) {
    g10 = 2.5016 * Ix ^ 5 - 7.8231 * Ix ^ 4 + 6.7988 * Ix ^ 3 - 1.7905 * Ix ^ 2 + 1.272 * Ix + 0.0346 + 1.44845779338554e-03
  }
  
  #hinf function 
  hinf <- function (Ix) {
    hinf = Ix ^ 2
  }
  
  #Jd для Nprop < 0.1
  Jd1 <- function (Nprop, Cfd) {
    Jd1 = 1 / (-0.629 + 0.5 * log(Cfd / Nprop) + fCL(Cfd))
  }
  
  #Jd для Nprop от 0.1 до 1 
  Jd2 <- function (Nprop, Cfd) {
    Ix = sqrt (Nprop / Cfd)
    delta1 = Jd_Ix1(Nprop) - Jd1(Nprop, Nprop)
    Jd2 = Jd1(Nprop, Cfd) + delta1 * g1(Ix)
  }
  
  #Jd для Nprop от 1 до 10
  Jd3 <- function (Nprop, Cfd) {
    Ix = sqrt (Nprop / Cfd)
    delta2 = Jd_Ix1(Nprop) - Jd2(1, Nprop)
    Jd3 = Jd2(1, Cfd) + delta2 * g10(Ix)
  }
  
  #Jd для Nprop более 10 
  Jd4 <- function (Nprop, Cfd) {
    Ix = sqrt (Nprop / Cfd)
    Cfd10 = 10 / Ix ^ 2
    delta3 = Jd_Ix1(Nprop) - Jd_Ix1(10)
    Jd4 = Jd3(10, Cfd10) + delta3 * hinf(Ix)
  }
  
  
  #Оптимизация трещины гидроразрыва 
  #Нахождение оптимальных параметров (Cfdopt, Ixopt, Jdopt)
  FracOpt <- function (Nprop){
    xu = Nprop/ 0.00000001
    xl=Nprop
    R = (sqrt (5) - 1) / 2
    
    d = R * (xu - xl)
    x1 = xl + d
    x2 = xu - d
    f1 = Jd(Nprop, x1)
    f2 = Jd(Nprop, x2)
    
    while (abs(xu - xl) >= 0.00001) {
      d = R * d 
      if (f1 > f2) {
        xl = x2
        x2 = x1
        f2 = f1
        x1 = xl + d
        f1 = Jd(Nprop, x1)
      } 
      else {
        xu = x1
        x1 = x2
        f1 = f2
        x2 = xu - d
        f2 = Jd(Nprop, x2)
      }
    }
    Cfdopt <- (xl + xu) / 2
  }
  
  #Оптимальные параметры     
  Cfdopt <- reactive ({ FracOpt(Nprop()) })
  Ixopt <- reactive ({ sqrt (Nprop() / Cfdopt()) })
  Jdopt <- reactive ({ Jd(Nprop(), Cfdopt()) })
  
  #Оптимальная длина и ширина трещины 
  DLx <- function (Cfd, Kf, Vf, K, h) {
    xf <- (Kf * Vf / (Cfd * K * h)) ^ 0.5    #оптимальная полудлина, м
  }
  
  DLw <- function (Cfd, Kf, Vf, K, h) {
    w <- (Cfd * K * Vf / (Kf * h)) ^ 0.5     #оптимальная ширина, м
  }
  
  xf = reactive ({ DLx (Cfdopt(), input$Kf, Vf(), input$K, input$hp) })
  w = reactive ({ DLw (Cfdopt(), input$Kf, Vf(), input$K, input$hp) })
  
  #Псевдоскин-фактор
  sf = reactive ({
    1 / Jdopt() - log(input$re / input$rw) + 3 / 4
  })
  
  #Кратность увеличения продуктивности
  Jdd = reactive ({
    (log(input$re / input$rw) - 3 / 4 + input$skin) / (log(input$re / input$rw) - 3 / 4 + sf())
  })
  
  Name = as.character (
    c("Безразмерное число проппанта Nprop",
      "Безразмерный коэффициент продуктивности Jd",
      "Оптимальная безразмерная проводимость трещины Cfdopt",
      "Оптимальная полудлина xfopt, м",
      "Оптимальная фиксированная ширина трещины wopt, м",
      "Псевдоскин-фактор после ГРП sf",
      "Кратность увеличения продуктивности скважины")
  )
  Value = reactive ({ as.numeric(
    c(Nprop(),
      Jdopt(),
      Cfdopt(),
      xf(), 
      w(), 
      sf(),
      Jdd())
  ) })
  
  Value1 <- reactive ({
    format(Value(),digits = 2)
  })
  
  df <- reactive({
    data.frame(
      Name, Value1(), stringsAsFactors = FALSE)
    
  })
  
  output$optvalues <- renderTable({
    df=df()
    colnames(df)<-c('Параметр','Значение')
    df
  })
  
  ########################################РЕАЛЬНОЕ РАЗМЕЩЕНИЕ################################################# 
  #Реальное размещение проппанта в трещине 
  
  xfa = reactive ({xf() * input$elong})
  
  DLw1 <- function (Ep, hf, K, n, qi, xf) {
    c01 = 9.15 ^ (1 / (2 * n + 2)) * 3.98 ^ (n / (2 * n + 2))
    c02 = ((1 + (pi - 1) * n) / n) ^ (n / (2 * n + 2)) * K ^ (1 / (2 * n + 2))
    ww0 <- c01 * c02 * ((qi/2/60) ^ n * hf ^ (1 - n) * xf / (Ep * 98066)) ^ (1 / (2 * n + 2))
  }
  
  ww0 = reactive ({ DLw1 (input$Ep, input$hf, input$Kp, input$np, input$qi, xf())  })
  
  wb = reactive ({
    0.628 * ww0()
  })
  
  ww = reactive ({
    ww0() * pi / 4
  })
  
  ce = reactive ({
    input$M / 2 / (input$hf * wb() * xfa())
  })
  
  #Масимальная концентрация проппанта, кг/м3 объема суспензии
  cemax = reactive ({
    input$cmax / (1 + input$cmax / (1000 * input$pprop))
  })
  
  ceswitch <- reactive ({
    if (ce() > cemax()) {
      ceswitch = 1 
    } else { 
      ceswitch = 0}
  })
  
  Ma <- reactive ({
    if (ce() > cemax()) {
      Ma <- input$M / 2 * cemax() / ce()
    } else 
      Ma <- input$M/2
  })
  
  ce1 <- reactive ({
    if (ce() > cemax()) {
      ce1 = Ma() / (input$hf * wb() * xfa())
    } else {
      ce1 = input$M / 2 / (input$hf * wb() * xfa())
    }
  })
  
  Vfa = reactive ({
    rp() * Ma() / ((1 - input$por) * (1000 * input$pprop))
  }) 
  
  wa = reactive ({
    Vfa() / (xfa() * input$hp)
  })
  
  Npropa = reactive ({
    (4 * Vfa() * input$Kf) / (input$K * Vr())
  })
  
  Cfda = reactive ({
    input$Kf * Vfa() / (xfa() * input$hp) / (input$K * xfa())
  })
  
  Ixa = 0
  
  #FracPI
  FracPI <- function (Npropa, Cfda, Ixa) {
    if (Npropa == 0) {
      Cfd = Cfda
      Ix = Ixa
      Nprop = Cfd * Ix ^ 2
    }
    if (Cfda == 0) {
      Nprop = Npropa
      Ix = Ixa
      Cfd = Nprop / Ix ^ 2 
    }
    if (Ixa == 0) {
      Nprop = Npropa
      Cfd = Cfda
      Ix = sqrt (Nprop / Cfd)
    }
    Jda <- Jd(Nprop, Cfd)
  }
  
  Jda <- reactive ({ 
    FracPI (Npropa(), Cfda(), Ixa)
  })
  
  #Псевдоскин и кратность увеличения продуктивности 
  sfa = reactive ({
    1 / Jda() - log(input$re / input$rw) + 3 / 4
  })
  
  Jdda = reactive ({
    (log(input$re / input$rw) - 3 / 4 + input$skin) / (log(input$re / input$rw) - 3 / 4 + sfa())
  })
  
  #Таблица реального размещения
  N = as.character (
    c("Закачанная масса проппанта (2 крыла), кг",
      "Безразмерное число проппанта Nprop",
      "Безразмерный коэффициент продуктивности Jd",
      "Безразмерная проводимость трещины Cfd",
      "Полудлина xf, м",
      "Фиксированная ширина трещины w, м",
      "Псевдоскин-фактор после ГРП sf",
      "Кратность увеличения продуктивности скважины")
  )
  
  VA = reactive ({ as.numeric(
    c(2*Ma(),
      Npropa(),
      Jda(),
      Cfda(),
      xfa(), 
      wa(), 
      sfa(),
      Jdda())
  ) })
  
  VA1 <- reactive ({
    format(VA(),digits = 2, scientific = F)
  })
  
  dfA <- reactive({
    data.frame(
      N, VA1(), stringsAsFactors = FALSE)
    
  })
  
  output$actvalues <- renderTable({
    dfA=dfA()
    colnames(dfA)<-c('Параметр','Значение')
    dfA
  })
  
  output$text <- renderText ({
    
    if (ceswitch() == 0 & input$elong == 1) {
      paste("Ограничения позволяют оптимальное размещение")
    } else if (ceswitch() == 1 & input$elong == 1) {
      paste("Псевдооптимальное размещение, соответствующее ограничениям \nУменьшена масса проппанта")
    } else if (ceswitch() == 1 & input$elong != 1) {
      paste("Псевдооптимальное размещение, соответствующее ограничениям \nДлина изменена")
    }
    else{
      paste("...")
    }
  })
  
  ####################################РЕЖИМ ОБРАБОТКИ#############################
  
  DLpmp1 <- function (qi, hf, xf, wb, CL, Sp, rp){
    kap = 1.41495
    a = (qi/2/60) / (hf * xf)
    b = -2 * kap * CL / (60 ^ (1/2)) * rp
    c = -(wb + 2 * Sp * rp)
    st = (-b + (b ^ 2 - 4 * a * c) ^ 0.5) / (2 * a)
    te <- st ^ 2
  }
  
  te <- reactive ({ DLpmp1 (input$qi, input$hf, xf(), wb(), input$CL, input$Sp, rp()) })
  
  eta <- reactive ({
    input$hf * xf() * wb() / ((input$qi/2/60) * te())
  })
  
  eps2 <- reactive ({
    (1 - eta()) / (input$Nolte + eta())
  })
  
  
  eps1 <- reactive ({
    eps2()*input$Nolte
  })
  
  tpad = reactive ({
    te() * eps1()
  })
  
  ca = reactive ({
    ce1() / (1 - ce1() / (1000 * input$pprop))
  })
  
  pn = reactive ({
    input$Ep * 98066 * wb() / (2 * input$hf * 0.628)
  })
  
  ft = 0.3048
  lbm = 0.4535924
  gallon = 0.00378541
  psi = 6894.757
  
  uc <- reactive ({
    Ma()/(xfa()*input$hf*wb())
  })
  
  ac <- reactive ({
    Ma() / (xfa() * input$hf) 
  })
  
  mc = reactive ({
    ca()
  })
  
  netp <- reactive ({
    pn() / 98066
  })
  
  
  #Таблица режима обработки
  NR = as.character (
    c("Эффективность, eta, %",
      "Время закачки, te, мин",
      "Время закачки подушки, te,  мин",
      "Показатель степени концентрации закачиваемого проппанта, eps",
      "Средняя концентрация проппанта в трещине в конце обработки, кг/м3",
      "Поверхностная концентрация проппанта после смыкания трещины, кг/м2",
      "Максимальная концентрация проппанта, кг на м3 чистой жидкости",
      "Эффективное давление в конце закачки, атм")
  )
  
  ValueR = reactive ({ as.numeric(
    c(eta()*100,
      te()/60,
      tpad()/60,
      eps2(),
      uc(),
      ac(),
      mc(),
      netp()
    )
  ) })
  
  ValueR1 <- reactive ({
    format(ValueR(),digits = 4, scientific = F)
  })
  
  dfR <- reactive({
    data.frame(
      NR, ValueR1(), stringsAsFactors = FALSE)
    
  })
  
  output$mode <- renderTable({
    dfR=dfR()
    colnames(dfR)<-c('Параметр','Значение')
    dfR
  })
  
  
  #Стиль файла Эксель
  hs <- createStyle(
    textDecoration = "BOLD", fontColour = "#000000", fontSize = 12,
    # fontName = "Times New Roman", 
    fgFill = "#9ACD32"
  )
  
  
  #Формирования файла для скачивания
  #2 вкладки в Excel с отдельными таблицами на каждой
  output$describe_download <- downloadHandler(
    filename = function() {
      paste('Результаты расчета', ".xlsx", sep = "")
    },
    
    content = function(file) {
      df <- df()
      colnames(df)<-c('Параметр','Значение')
      dfA <- dfA()
      colnames(dfA)<-c('Параметр','Значение')
      dfR <- dfR()
      colnames(dfR)<-c('Параметр','Значение')
      list_of_datasets <- list("Оптимальное размещение" = df, "Реальное размещение" = dfA, "Режим обработки" = dfR)
      write.xlsx(list_of_datasets, file, halign='center', colWidths = "auto",borders = "all", fontName = "Times New Roman",headerStyle = hs, firstRow=TRUE, firstCol=TRUE)
    }
  )
  ###Отчет Word
  output$describe_download_test = downloadHandler(
    filename<- function(){
      paste("Отчет",Sys.Date(), '.docx',sep = "") #Название файла
    },
    
    content = function(file) {
      
      withProgress(message = 'Документ загружается',
                   detail = 'Это может занять некоторое время...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     ## End of progression
                     src <- normalizePath('Report_ru.Rmd')
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'Report_ru.Rmd', overwrite = TRUE)
                     
                     library(rmarkdown)
                     out <- render('Report_ru.Rmd', word_document())
                     file.rename(out, file)
                   })
    })
  #Вывод комментария 
  output$htmlmarkdown = reactive({
    note_in_html(input$markdowninput)
  })
  
}

)
