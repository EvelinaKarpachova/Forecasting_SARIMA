###### Aplikācija laikrindu analīzei un prognozēšanai ar SARIMA modeļiem
#                                                      Evelīna Karpačova

### Pakotnes
library(shiny)
library(shinyFiles)
library(readxl)
library(tseries)
library(zoo)
library(forecast)
library(knitr)
library(DT)
library(kableExtra)



###############################    UI    ###################################
ui <- navbarPage(
  title = "Laikrindu prognozēšana ar SARIMA modeļiem.",
  
  ##### Pirmā lapa
  tabPanel("Datu ieladēšana",
           sidebarLayout(
             sidebarPanel(
              h3("Datu augšupielāde un apstrāde"),
              ### Ievades
              br(),
              fileInput("file", "Izvēlieties CSV vai XLSX failu.", accept = c(".csv", ".xlsx")),
              numericInput("start", "Ievadiet sākumu:", 0, min = 0),
              numericInput("frequency", "Ievadiet frekvenci (piem. 12 mēneša datiem):", 1, min = 1),
              strong("Vai Jūsu datos ir datu trūkums?"),
              actionButton("datu_trukums_Poga", "Aizpildīt"),
              br(),
              br(),
              br(),
              helpText("Laikrinda tiek uzzīmēta automatiski.")
              ),
             
             
             mainPanel(
               plotOutput("tsPlot"),
               verbatimTextOutput("ts_dati"),
               tableOutput("ts_summary"),
               textOutput("datu_trukums_izvade"),
               plotOutput("interpolated_plot")
               ))),
  
  
  ##### Otrā lapa
  tabPanel("Analīze",
           sidebarLayout(
             sidebarPanel(
               h3("Laikrindas analīze un modeļa pielāgošana"),
              ### Ievades
               strong("1. Nospiediet pogu, lai sadalītu Jūsu laikrindu uz treniņa un testa kopām:"),
               actionButton("sadale_Poga", "    .    "),
               br(),
               br(),
               br(),
               strong("2. Laikrindas dekompozīcija:"),
               actionButton("decompose_Poga", "Dekompozīcija"),
               br(),
               br(),
               br(),
               radioButtons("stac_Transformacija", "3. Izvēlieties stationarizācijas veidu:",
                           choices = c("Diferencēšana" = "diff", 
                                       "Logaritmiska transformācija" = "log",
                                       "Sezonālā diferencēšana" = "seasonal_diff",
                                       "Slīdošā vidēja filtrs" = "ma"),
                           selected = "diff"),
               actionButton("stac_plot_Poga", "Uzzīmēt"),
               br(),
               br(),
               br(),
               strong("4. Stacionaritātes pārbaude:"),
               actionButton("stac_parbaude_Poga", "Pārbaudīt"),
               br(),
               br(),
               br(),
               strong("5. Atrast atbilstošo SARIMA modeli:"),
               actionButton("sarima_Poga", "Atrast piemēroto SARIMA modeli"),
               br(),
               br(),
               br(),
               strong("6. Atlikumu diagnostika un Boksa-Ļjunga tests modelim:"),
               actionButton("diagnostika_Poga", "Veikt diagnostiku")
               ),
             
             mainPanel(
               textOutput("sadale_rezultats"),
               plotOutput("dekompozicijaPlot"),
               plotOutput("stacionara_ts_Plot"),
               tableOutput("stac_rezultats"),
               verbatimTextOutput("SARIMA"),
               plotOutput("atlikumuPlot"),
               verbatimTextOutput("BL_test_summary"),
               tableOutput("interpretacija")
               ))),
  
  
  ##### Trešā lapa
  tabPanel("Prognozēšana",
           sidebarLayout(
             sidebarPanel(
               h3("Prognozēšana"),
               ### Ievades 
               numericInput("skaits", "Ievadiet prognozējamo vērtību skaitu:", 1, min = 1),
               br(),
               strong("Nospiediet pogu, lai prognozētu laikrindu:"),
               actionButton("prognoze_Poga", "    .    "),
               br(),
               br(),
               br(),
               strong("Vai vēlaties lejupielādēt rezultātus?"),
               downloadButton("download_results", "Lejupielādēt rezultātus")
               ),
             
             
             mainPanel(
               plotOutput("prognozePlot"),
               verbatimTextOutput("prognozes_vertibas"),
               textOutput("pazinojums_1"),
               tableOutput("prognozes_precizitate"),
               textOutput("pazinojums_2"),
               tableOutput("Kriteriji")
               ))),
  
  
  ##### Ceturtā lapa
  tabPanel("Veikt manuālo prognozi",
           sidebarLayout(
             sidebarPanel(
               ### Ievades 
               h3("SARIMA modeļa parametru izvēle"),
               helpText("Šajā lapā jūs varat personīgi izvēlēties prognozēšanas modeļa parametrus.
                         Augšupielādējiet failu un ievadiet informāciju šīs aplikācijas pirmajā lapā.
                        "),
               numericInput("p", "AR parametrs (p):", value = 1, min = 0),
               numericInput("d", "Trenda diference (d):", value = 1, min = 0),
               numericInput("q", "MA parametrs (q):", value = 1, min = 0),
               numericInput("P", "Sezonāls AR parametrs (P):", value = 1, min = 0),
               numericInput("D", "Sezonālā diference (D):", value = 1, min = 0),
               numericInput("Q", "Sezonāls MA parametrs (Q):", value = 1, min = 0),
               numericInput("h", "Prognozēto vērtību skaits (h):", value = 12, min = 1),
               actionButton("Poga", "Prognozēt"),
               br(),
               br(),
               br(),
               strong("Vai vēlaties lejupielādēt rezultātus?"),
               downloadButton("lejupieladet", "Lejupielādēt rezultātus")
               ),
             
             
             mainPanel(
               plotOutput("ACF_PACF"),
               plotOutput("prognoze2_Plot"),
               textOutput("pazinojums_3"),
               tableOutput("kriteriju_tabula2"),
               textOutput("pazinojums_4"),
               tableOutput("Precizitate"),
               tableOutput("prognozes_tabula")
             )))

  
)



###################      Servera funkcija    ################################
server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    
    # Datu ieladēšana
    file_path <- input$file$datapath
    if (endsWith(file_path, ".csv")) {
      data <- read.csv(file_path, header = FALSE)
    } else if (endsWith(file_path, ".xlsx")) {
      data <- read_excel(file_path)
    } else {
      stop("Nepareizs faila formāts. Izvēlieties CSV vai XLSX failu.")}
    
    # Datus konvertē laikrindā
    ts_data <- ts(data, frequency = input$frequency, start = input$start)
    return(ts_data)
    })
  

    # Pārbaude vai ir datu trūkums, ja ir, aizpilda tukšumus, pielietojot splainu interpolāciju
    trukumi_ts <- reactive({
    req(input$datu_trukums_Poga)
    if (any(is.na(data()))) {
      # Splaina interpolācija
      trukumi <- which(is.na(data()))
      data_vektor <- as.vector(data())
      data_vektor_pilns <- data_vektor
      data_vektor_pilns[trukumi] <- splinefun(na.omit(data_vektor), method = "natural")(trukumi)
      ts_data <- ts(data_vektor_pilns, frequency = input$frequency, start = input$start)
      return(ts_data)
      output$datu_trukums_izvade <- renderText({"Trūkstoši dati ir aizpildīti."})
      } else {
      output$datu_trukums_izvade <- renderText({"Datu trūkums nav novērots."})
      return(data)}
      })
    
    # Laikrindas treniņa kopa un testa kopa
    # Treniņa kopa
    trenina_ts <- reactive({
      req(input$sadale_Poga)
      if (any(is.na(data()))){
          ts_index <- round(0.8 * length(trukumi_ts()))
          trenina_kopa <- trukumi_ts()[1:ts_index]
          trenina_laikrinda <- ts(trenina_kopa, frequency = input$frequency, start = input$start)
          return(trenina_laikrinda)
      } else {
        message("Using data()")
          ts_index <- round(0.8 * length(data()))
          trenina_kopa <- data()[1:ts_index]
          trenina_laikrinda <- ts(trenina_kopa, frequency = input$frequency, start = input$start)
          return(trenina_laikrinda)}
      })
    
    # Testa kopa
    testa_ts <- reactive({
      req(input$sadale_Poga)
      if (any(is.na(data()))) {
          index <- round(0.8 * length(trukumi_ts()))
          testa_kopa <- trukumi_ts()[(index + 1):length(trukumi_ts())]
          testa_laikrinda <- ts(testa_kopa, frequency = input$frequency, start = input$start)
          return(testa_laikrinda)
      } else {
          index <- round(0.8 * lenght(data()))
          testa_kopa <- data()[(index):length(data())]
          testa_laikrinda <- ts(testa_kopa, frequency = input$frequency, start = input$start)
          return(testa_laikrinda)}
      })
    
    
    # Laikrindas dekompozīcija
    decompose_ts <- reactive({
      req(input$decompose_Poga)
      decompose(trenina_ts())
      })
    
    
    # Laikrindas stacionārizēšana
    stacionara_ts <- reactive({
      req(input$stac_Transformacija)
      
      if (is.null(input$stac_Transformacija)) {
        return(NULL)  # Return NULL or handle the case when no selection is made
      }
      if (input$stac_Transformacija == "diff") {
        # Diferencēšana
        diff(trenina_ts())
      } else if (input$stac_Transformacija == "log") {
        # Logaritmiskā transformācija
        log(trenina_ts())
      } else if (input$stac_Transformacija == "seasonal_diff") {
        # Sezonālā diferencēšana
        diff(na.omit(trenina_ts()), lag = 48)
      } else if (input$stac_Transformacija == "ma") {
        # Slīdošais vidējais
        ma_ts <- zoo::rollmean(trenina_ts(), k = 3, fill = NA)
        ma_ts[is.na(ma_ts)] <- mean(na.omit(trenina_ts()), na.rm = TRUE)
        return(ma_ts)
      }
      })
    
    
    # Stacionaritātes pārbaude
    stac_parbaude <- reactive({
      req(input$stac_parbaude_Poga)
      # ADF tests stacionaritātes pārbaudei
      result <- adf.test(stacionara_ts())
      return(result)
      })
    
    
    # Meklē SARIMA modeli
    sarima_modelis <- reactive({
      tryCatch({
        auto.arima(stacionara_ts())
      }, error = function(e) {
        return(NULL)
      })
      })
  
    
    # Modeļa atlikumu diagnostika
    sarima_atlikumi <- reactive({
      tryCatch({
        checkresiduals(sarima_modelis())
      }, error = function(e) {
        return(NULL)
      })
      })
    
    
    # Prognoze
    prognoze_ts <- reactive({
      req(input$prognoze_Poga)
      
      # SARIMA modeļa parametri no auto.arima()
      modelis <- auto.arima(stacionara_ts())
      parametri <- modelis$arma
      p1 <- parametri[1] # p
      q1 <- parametri[2] # q
      p2 <- parametri[3] # P
      q2 <- parametri[4] # Q
      s <- parametri[5] # s
      d1 <- parametri[6] # d
      d2 <- parametri[7] # D
        
      # Pielietosim modeli datiem
      if (any(is.na(data()))) {
      beigu_modelis <- Arima(trukumi_ts(), order = c(p1, d1, q1), seasonal = c(p2, d2, q2), include.mean = FALSE, method = "ML")
      prognoze <- forecast(beigu_modelis, h = input$skaits, level = 95)
      return(prognoze)
      } else {
      beigu_modelis <- Arima(data(), order = c(p1, d1, q1), seasonal = c(p2, d2, q2), include.mean = FALSE, method = "ML")
      prognoze <- forecast(beigu_modelis, h = input$skaits, level = 95)
      return(prognoze)
      }
    })

    
    # Ceturtā lapa: SARIMA parametri
    sarima_params <- reactive({
      order <- pmax(c(input$p, input$d, input$q), 0)
      seasonal_order <- pmax(c(input$P, input$D, input$Q), 0)
      
      list(order = order, seasonal_order = seasonal_order, h = input$h
      )
      })
  
    
    # Prognozēšana ar izvēlētiem parametriem
    prognoze_sarima <- function(time_series, order, seasonal_order, h) {
      sarima_model <- Arima(time_series, order = order, seasonal = seasonal_order, include.mean = FALSE, method = "ML")
      forecast_result <- forecast(sarima_model, h = h, level = 95)
      return(forecast_result)
    }
    
    
############################      Izvades      #####################################
  
########### Pirmā lapa:
    
  # Laikrindas grafiks
    output$tsPlot <- renderPlot({
    req(data())
    plot(data(), lwd = 1.5, main = "Laikrindas grafiks", xlab = "Laiks", ylab = "Vērtība")
    points(data(), col = "blue")
    })
    
  # Laikrindas dati
    output$ts_dati <- renderPrint({
    ts(data(), frequency = input$frequency, start = input$start)
    })
    
  # Grafiks pēc trūkumu aizpildes
    output$interpolated_plot <- renderPlot({
      plot(trukumi_ts(), main = "Pilna laikrinda", lwd = 1.5, xlab = "Laiks", ylab = "Vertība")
      points(trukumi_ts(), col = "darkgreen")
      })
    
  # Laikrindas statistiku izvade
    output$ts_summary <- renderTable({
    req(data())
    if (any(is.na(data()))) {
    ts_info <- data.frame(
        Statistika = c("Min", "Max", "Mediāna", "Aritmētiskais vidējais"),
        Vērtība = c(min(trukumi_ts()), max(trukumi_ts()), median(trukumi_ts()), mean(trukumi_ts())))
        ts_info
      } else {
    ts_info <- data.frame(
        Statistika = c("Min", "Max", "Mediāna", "Aritmētiskais vidējais"),
        Vērtība = c(min(data()), max(data()), median(data()), mean(data())))
        ts_info}
      })
    
    
    
########### Otrā lapa:
    
  # Paziņojums par sadali uz treniņa un testa kopām
    observeEvent(input$sadale_Poga, {
      output$sadale_rezultats <- renderText({
        print(" Laikrinda tika sadalīta treniņa kopā un testa kopā (80% un 20%).
              Turpmākās darbības tiks veiktas, izmantojot treniņa kopu.\n")
      })
      })
    
  # Laikrindas dekompozīcijas grafiks
    output$dekompozicijaPlot <- renderPlot({
    req(input$decompose_Poga)
    plot(decompose_ts())
    })  
    
  # Stacionāras laikrindas grafiks
    output$stacionara_ts_Plot <- renderPlot({
    req(input$stac_plot_Poga)
    plot(stacionara_ts(), main = "Stacionarizēta laikrinda", xlab = "Laiks", ylab = "Transformētās vērtības")
    })
    
   # Stacionaritātes pārbaude
    output$stac_rezultats <- renderTable({
      req(input$stac_parbaude_Poga)
      p_value <- stac_parbaude()$p.value
      result_data <- data.frame(
        Tests = "ADF tests",
        H0 = "Laikrinda satur vienības sakni un nav stacionāra.",
        HA = "Laikrinda nesatur vienības sakni un ir stacionāra.",
        "p-vērtība" = p_value,
        "Interpretācija" = "Ja p<0.05, laikrinda ir stacionāra (noraida H0). 
        Ja p>0.05, laikrinda nav stacionāra (nenoraida H0)."
      )
      return(result_data)
    })

   # SARIMA modeļa izvēle
    output$SARIMA <- renderPrint({
    req(input$sarima_Poga)
    if (!is.null(sarima_modelis())) {
      capture.output(print(sarima_modelis()), file = NULL, append = TRUE)
    } else {
      return("Kļūda: Nevar atrast piemēroto SARIMA modeli. Pārbaudiet ievades.")
    }
    })
    
    # Atlikumu grafiks
    output$atlikumuPlot <- renderPlot({
      req(input$diagnostika_Poga)
      if (!is.null(sarima_atlikumi())) {
        capture.output(print(sarima_atlikumi()), file = NULL, append = TRUE)
      } else {
        return("Kļūda: Nav iespējams veikt atlikumu diagnostiku.")
      }})
    
    # Boksa-Ljunga tests
    output$BL_test_summary <- renderPrint({
      req(input$diagnostika_Poga)
      if (!is.null(sarima_atlikumi())) {
        capture.output(print(sarima_atlikumi()), file = NULL, append = TRUE)
      } else {
        return("Kļūda: Nav iespējams veikt atlikumu diagnostiku.")
      }
      })
    
    # Boksa-Ļjunga testa interpretācija
    output$interpretacija <- renderTable({
      req(input$diagnostika_Poga)
      data.frame(
        Tests = "Boksa-Ļjunga tests atlikumiem",
        H0 = "Atlikumos nav autokorelācijas.",
        HA = "Atlikumos ir autokorelācija.",
        "Interpretācija" = "Ja p<0.05, noraida H0. 
        Ja p>0.05, nenoraida H0."
      )
    })
    
    
    
########### Trešā lapa: 
    
    # Laikrindas prognozes grafiks
    output$prognozePlot <- renderPlot({
      req(input$prognoze_Poga, prognoze_ts())
      if (any(is.na(data()))){
      plot(prognoze_ts(), lwd = 1.5, xlab = "Laiks", ylab = "Vērtības", main = "Prognoze")
      points(trukumi_ts(), col = "blue")
      } else {
      plot(prognoze_ts(), lwd = 1.5, xlab = "Laiks", ylab = "Vērtības", main = "Prognoze")
      points(data(), col = "blue")
      }
      })
    
    # Prognozes vērtības
    output$prognozes_vertibas <- renderPrint({
      req(input$prognoze_Poga)
       print(prognoze_ts()$mean)
    })
    
   # Paziņojums
    output$pazinojums_1 <- renderText({
      req(input$prognoze_Poga)
      print("Prognozes precizitātes kļūdas:")
    })
    
    # Prognozes precizitāte
    output$prognozes_precizitate <- renderTable({
      req(input$prognoze_Poga)
      print(accuracy(prognoze_ts()))
    })
    
    # Paziņojums
    output$pazinojums_2 <- renderText({
      req(input$prognoze_Poga)
      print("Informācijas kritēriju tabula:")
    })
    
    # Kritēriju tabula (1)
    output$Kriteriji <- renderTable({
      req(input$prognoze_Poga)
      modelis <- auto.arima(stacionara_ts())
      parametri <- modelis$arma
      p1 <- parametri[1]; q1 <- parametri[2]; p2 <- parametri[3]
      q2 <- parametri[4]; d1 <- parametri[6]; d2 <- parametri[7]
      if (any(is.na(data()))){
        xdx <- diff(trukumi_ts())
        mu <- mean(xdx)
        y <- xdx - mu
        modelis <- Arima(y, order = c(p1,d1,q1), seasonal = c(p2,d2,q2), include.mean = F, method = "ML")
      } else {
        xdx <- diff(data())
        mu <- mean(xdx)
        y <- xdx - mu
        modelis <- Arima(y, order = c(p1,d1,q1), seasonal = c(p2,d2,q2), include.mean = F, method = "ML")
      }
      # Kritēriju tabula
      data.frame(AIC = modelis$aic,
                 BIC = modelis$bic,
                 AICC = modelis$aicc)
      })
    
    # Lejupielādēšanas iespēja
    output$download_results <- downloadHandler(
      filename = function() {
        paste("prognoze.csv", sep = "")
      },
      content = function(file) {
        download_data <- prognoze_ts()$mean
        # Veido CSV failu
        write.csv(download_data, file, row.names = FALSE)
      })
    
    
    
########### Ceturtā lapa:
    
   # Laikrindas ACF un PACF
    output$ACF_PACF <- renderPlot({
      req(input$Poga)
      par(mfrow = c(1,2))
      if (any(is.na(data()))) {
      ts_acf <- Acf(diff(trukumi_ts()), 4*input$h, main = "ACF", family = "A")
      ts_pacf <- Pacf(diff(trukumi_ts()), 4*input$h, main = "PACF", family = "A") 
      }
      ts_acf <- Acf(diff(data()), 4*input$h, main = "ACF", family = "A")
      ts_pacf <- Pacf(diff(data()), 4*input$h, main = "PACF", family = "A") 
    })
    
   # Prognoze ar izvēlētiem SARIMA parametriem
    output$prognoze2_Plot <- renderPlot({
    req(input$Poga)
      if (any(is.na(data()))){
        forecast_result <- prognoze_sarima(trukumi_ts(), sarima_params()$order, sarima_params()$seasonal_order, sarima_params()$h)
        plot(forecast_result, main = "SARIMA prognoze", lwd = 1.5,
             xlab = "Laiks", ylab = "Vērtības")
        points(trukumi_ts(), col = "blue")
          } else {
        forecast_result <- prognoze_sarima(data(), sarima_params()$order, sarima_params()$seasonal_order, sarima_params()$h)
        plot(forecast_result, main = "SARIMA prognoze", lwd=1.5, 
             xlab = "Laiks", ylab = "Vērtības")
        points(data(), col = "blue")
        }
      })
    
    # Paziņojums
    output$pazinojums_3 <- renderText({
      req(input$Poga)
      print("Informācijas kritēriju tabula:")
    })
    
    # Kritēriju tabula (2)
    output$kriteriju_tabula2 <- renderTable({
      req(input$Poga)
      order <- pmax(c(input$p, input$d, input$q), 0)
      seasonal_order <- pmax(c(input$P, input$D, input$Q), 0)
      if (any(is.na(data()))){
        xdx <- diff(trukumi_ts())
      mu <- mean(xdx)
      y <- xdx - mu
      modelis <- Arima(y, order = order, seasonal = seasonal_order, include.mean = F, method = "ML")
      } else {
        xdx <- diff(data())
        mu <- mean(xdx)
        y <- xdx - mu
        modelis <- Arima(y, order = order, seasonal = seasonal_order, include.mean = F, method = "ML")
      }
      # Kritēriju tabula
      data.frame(AIC = modelis$aic,
                   BIC = modelis$bic,
                   AICC = modelis$aicc)
      })
    
    # Paziņojums
    output$pazinojums_4 <- renderText({
      req(input$Poga)
      print("Prognozes precizitātes kļūdas:")
    })
    
    # Prognozes precizitāte
    output$Precizitate <- renderTable({
      req(input$Poga)
      if (any(is.na(data()))){
      forecast_result <- prognoze_sarima(trukumi_ts(), sarima_params()$order, sarima_params()$seasonal_order, sarima_params()$h)
      print(accuracy(forecast_result))
      } else {
      forecast_result <- prognoze_sarima(data(), sarima_params()$order, sarima_params()$seasonal_order, sarima_params()$h)
      print(accuracy(forecast_result)) 
      }
      })
    
    # Lejupielādēšanas iespēja
    output$lejupieladet <- downloadHandler(
      filename = function() {
        paste("sarima.csv", sep = "")},
      content = function(file) {
      if (any(is.na(data()))) {
          forecast_result <- prognoze_sarima(trukumi_ts(), sarima_params()$order, sarima_params()$seasonal_order, sarima_params()$h)
      } else {
          forecast_result <- prognoze_sarima(data(), sarima_params()$order, sarima_params()$seasonal_order, sarima_params()$h)
        }
      download_data <- forecast_result$mean
      # Veido CSV failu
      write.csv(download_data, file, row.names = FALSE)
      })
    
    
    
}
  
# Run the application 
shinyApp(ui = ui, server = server)
