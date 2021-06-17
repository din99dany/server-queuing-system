library(shiny)
library(shinythemes)

# Functie corespunzatoare intensitatii procesului Poisson
LAMBDA <- function(t) {
  if (0 <= t & t < 3 * 60) {
    c <- -t ** 2 - 2 * t + 25
  } else if (3 * 60 <= t & t <= 5 * 60) {
    c <- 14
  } else if (5 * 60 < t & t < 9 * 60) {
    c <- -0.05 * (t ** 2) - 0.2 * t + 12
  } else {
    c <- 11
  }
  return (c)
}

# Functie ce simuleaza momentul primei sosiri a unui client dupa momentul de timp s
# corespunzatoare unui proces Poisson neomogen cu functia de intensitate lambda si
# maximul ei mLambda
GET_TS <- function(s, lambda, mLambda) {
  ts <- t <- s
  
  while (TRUE) {
    # Simuleaza 2 valori uniforme in intervalul [0,1]
    U1 <- runif(1)
    U2 <- runif(1)
    
    # Noul moment de timp t
    t <- t - 1 / mLambda * log(U1) * 60
    
    # Daca valoarea aleatoare uniforma este mai mica decat raportul dintre functia de intensitate
    # in momentul respectiv si valoarea maxima a functiei intensitate, atunci ne oprim
    if (U2 <= lambda(t) / mLambda) {
      ts <- t
      break
    }
  }
  ts
}

# Functie care simuleaza o variabila aleatoare exponentiala de parametru lambda folosind metoda inversa
SIMULATE_EXP <- function(lambda) {
  # Genereaza o variabila aleatoare uniforma pe intervalul [0,1]
  u <- runif(1)
  # Construieste variabila exponentiala de parametru lambda
  x <- -1 / lambda * log(u)
  return (x)
}

# Functie care simuleaza o variabila aleatoare Poisson de parametru lambda = 6
# prin metoda inversa. 
GET_Y1 <- function(lambda = 6) {
  #  Se foloseste de relatia de recurenta p_j+1 = p_j * lambda / (j + 1)
  # Contor
  i <- 0
  # Probabilitatea curenta
  p <-  exp(1) ^ (-lambda)
  # Valoarea functiei de repartitie in punctul curent
  F <- p
  u <- runif(1, 0, 1)
  
  while (u >= F) {
    # Updatam noile valori ale probabilitatilor si functiei de repartitie
    p <- p * lambda / (i + 1)
    F <- F + p
    i <- i + 1
  }
  return (i)
}

# Functie care simuleaza o variabila aleatoare data prin densitatea de probabilitate
# folosind metoda respingeri
GET_Y2 <- function(x) {
  # Am ales g(y) = e^x, x > 0
  # Am calculat maximul functiei h(x) = f(x)/g(x),iar acesta este c = 5
  c <- 5
  while (T) {
    # Simuleaza o variabila aleatoare exponentiala de parametru lambda = 1
    y <- SIMULATE_EXP(1)
    # Simuleaza o variabila aleatoare uniforma cu valori in [0,1]
    u <- runif(1)
    if (u <= 2 / (61 * c) * y * exp(y - y ^ 2 / 61)) {
      return(y)
    }
  }
}

# Functia care simuleaza intregul sistem
# t_max -> timpul (exprimat in minute) in care functioneaza centrul de vaccinare
# lambda -> functia de intensitate a procesului poisson neomogen care simuleaza venirea clientilor
# c_max -> lungimea maxima a cozii de asteptare pentru care un client pleaca
RUN_SIMULATION <- function(t_max = 720, lambda = LAMBDA, c_max = 20) {
  # Vectorii de output
  
  # Momentul de timp in care un client ajunge in sistem
  A1 <- c()
  # Momentul de timp in care un client ajunge la serverul 2
  A2 <- c()
  # momentul de timp in care un client paraseste sistemul 
  D <- c()
  
  # timpul maxim de asteptare dupa care un pacient pleaca
  timp_asteptare_max <- 60
  
  # valoarea maxima a functiei de intensitate
  max_value <- 26
  
  # numarul de doze dintr-o zi
  nr_doze <- 100
  
  # Generam primul moment la care ajunge un client
  ta <- GET_TS(0, lambda, max_value)
  
  # Variabile contor ce retin date despre starea sistemului
  t  <- na <- nd <- n1 <- n2 <- 0
  t1 <- t2 <- Inf
  
  # Variabile indecsi pt fiecare vector ce marcheaza primul pacient care e in asteptare pentru coada de la primul server, respectiv al 2-lea server
  indexA1 <- 1
  indexA2 <- 0
  
  # Numarul de pacienti care au ajuns la fiecare server, in total
  k1 <- 0
  k2 <- 0
  
  # Numarul de clienti pierduti la coada la fiecare server
  serv_1_pierdut <- 0
  serv_2_pierdut <- 0
  
  # Cat timp functioneaza centrul de vaccinare (si mai sunt doze destule)
  while (t < t_max && nr_doze > 0) {
    #Soseste un client nou
    if (ta == min(ta, t1, t2)) {
      t  <- ta
      if (ta < t_max) {
        k1 <- k1 + 1
        
        #A venit un client nou, adaugam in vectorii de output
        A1 <- append(A1, t)
        A2 <- append(A2, 0)
        D <- append(D, 0)
        
        #Punem in coada
        na <- na + 1
        n1 <- n1 + 1
        # Generam urmatorul timp la care vine un client
        ta <- GET_TS(t, lambda, max_value)
        
        #Daca e singurul client genereaza t1 (momentul de timp la care clientul de la serverul 1 termina cu acesta)
        if (n1 == 1) {
          t1 <- t + GET_Y1()
        }
        
        # Daca lungimea cozii e prea mare, clientul pleaca. Marcam in vectorii de output cu valori negative
        if(n1 > c_max) {
          n1 <- n1 - 1
          A1[na] <- -t
          A2[na] <- -t
          D[na] <- -t
          serv_1_pierdut <- serv_1_pierdut + 1
        }
      }
    } else if (t1 < ta && t1 <= t2) { # Daca clientul de la serverul 1 termina primul
      # Verificam daca primul pacient de la coada asteapta de prea mult timp
      t  <- t1
      
      if (n1 > 0 && indexA1 <= k1 && 
          ((t - A1[indexA1]) > timp_asteptare_max)) {
        # Actualizam vectorii de output pentru pacientii care au asteptat prea mult si au plecat.
        # Marcam cu -t in vectorii A2 si D pentru a sti momentul in care un pacient a plecat
        
        A2[indexA1] <- -t
        D[indexA1] <- -t
        indexA1 <- indexA1 + 1
        n1 <- n1 - 1
        
        if (n1 == 0) {
          t1 <- Inf
        }
      } else {
        k2 <- k2 + 1
        # Terminam de procesat un client pa<- ajunge altul in serverul 1
        # Scade numarul de clienti ramasi in coada
        n1 <- n1 - 1
        # Creste in serv2
        n2 <- n2 + 1
        
        # Adaugam timpul la care ajunge clientul la al II-lea server
        A2[indexA1] <-  t
        indexA1 <- indexA1 + 1
        
        if (n1 == 0) {
          # Daca serverul ramane gol reinitializeaza
          t1 <- Inf
        } else {
          # Altfel, pentru urmatorul client din coada, genereaza timpul pentru taskul lui
          t1 <- t + GET_Y1()
        }
        
        if (n2 == 1) {
          # Daca clientul terminat de serverul 1 e singurul in serverul 2 genereaza timpul pentru taskul lui
          t2 <- t + GET_Y2()
        } else if(n2 > c_max) {
          # Daca sunt prea multi clienti la coada, clientul pleaca
          A2[indexA1] <- -t
          D[indexA1] <- -t
          n2 <- n2 - 1
          serv_2_pierdut <- serv_2_pierdut + 1
        }
      }
    } else if (t2 < ta && t2 < t1)  {
      # Serverul 2 termina de procesat client inainte sa primim persoane noi sau sa terminam pe cineva in s1
      t  <- t2
      nd <- nd + 1
      # Scadem numarul de doze ramase
      nr_doze <- nr_doze - 1
      
      # Scade numarul de clienti din coada
      n2 <- n2 - 1
      
      indexA2 <- indexA2 + 1
      
      if (n2 == 0) {
        t2 <- Inf
      }
      
      #adaugam timpul la care pleaca persoana din centrul de vaccinare
      D[nd] <-  t
      
      if (n2 >= 1 ) {
        #daca mai avem persoane de procesat, genereaza timpul pentru taskul lor
        t2 <- t + GET_Y2()
      } 
      
      
    }
  }
  res <- data.frame(A1, A2, D, nr_doze, n1, n2)
  return(res)
}


result <- RUN_SIMULATION()


# Functie corespunzatoare intensitatii procesului Poisson, in momentul in care 
# programul incepe cu o ora mai devreme
LAMBDA_INCEPUT <- function(t) {
  if (0 <= t & t < 4 * 60) {
    c <- -t ** 2 - 2 * t + 25
  } else if (4 * 60 <= t & t <= 6 * 60) {
    c <- 14
  } else if (6 * 60 < t & t < 10 * 60) {
    c <- -0.05 * (t ** 2) - 0.2 * t + 12
  } else {
    c <- 11
  }
  return (c)
}

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  
                  "Simulare vaccinare Covid-19",
                  
                  tabPanel("Pagina pricipala",
                           # Cerinta 1
                           mainPanel(
                             h2("Determinarea timpului minim, maxim si mediu petrecut de o persoana ce asteapta sa se vaccineze\n"),
                             br(),
                             
                             column(10,
                                    tableOutput("table1"))
                           ),
                           br(),
                           # Cerinta 2 
                           mainPanel(
                             h2("Determinarea numarului mediu de persoane vaccinate intr-un interval de timp"),
                             br(),
                             sidebarLayout(
                               
                               # Sidebar with a slider input
                               sidebarPanel(sliderInput("oraSt", "Ora inceput:",
                                                        min = 8, max = 20,         
                                                        value = 8),
                                            sliderInput("oraEnd", "Ora sfarsit:",
                                                        min = 8, max = 20,         
                                                        value = 20),
                                            sliderInput("nrSim2", "Numar simulari:",
                                                        min = 5, max = 20,         
                                                        value = 10)
                               ),
                               
                               # Show a plot of the generated distribution
                               mainPanel(
                                 plotOutput("plotClientiTotal")
                               )
                             ),
                             h3(textOutput("medie2"))
                           ),
                           # Suplimentar nr doze
                           br(),
                           mainPanel(
                             h2("Numarul de doze de vaccin"),
                             h3(textOutput("persNevaccinate")),
                             plotOutput("plotNrDozePierdute"),
                             h3(textOutput("medieDoze"))
                           ),
                           # Cerinta 3
                           mainPanel(
                             h2("Determinarea primului moment de timp la care pleaca o persoana"),
                             h4("Studiul a fost realizat pe 10 simulari"),
                             
                             column(8,
                                    tableOutput("tablePlecare"))
                           ),
                           # Cerinta 4
                           mainPanel(
                             h2("Determinarea numarului mediu de persoane care au plecat datorita timpului de asteptare"),
                             h4("Studiul a fost realizat pe 10 simulari"),
                             
                             column(10,
                                    tableOutput("tablePlecareMediu"))
                           ),
                           # Cerinta 5
                           mainPanel(
                             h2("Determinarea numarului de doze suplimentare administrate"),
                             br(),
                             sidebarLayout(
                               
                               # Sidebar with a slider input
                               sidebarPanel(sliderInput("nrSim5", "Numar simulari:",
                                                        min = 10, max = 50,         #todo: dim minima trebuie sa fie maximul curent
                                                        value = 20),
                                            radioButtons(inputId="choice", label="Ce se modifica?",
                                                         choices=c("Se incepe programul mai devreme cu o ora" = 1,
                                                                   "Se prelungeste programul cu o ora" = 2,
                                                                   "Se mareste dimensiunea maxima a cozii de asteptare" = 3)),
                                            sliderInput("dimCoada", "Dimensiune maxima a cozii de asteptare:",
                                                        min = 20, max = 100,         #todo: dim minima trebuie sa fie maximul curent
                                                        value = 20)),
                               
                               # Show a plot of the generated distribution
                               mainPanel(
                                 plotOutput("plotCastig")
                               )
                             ),
                             h3(textOutput("medie5"))
                             
                           )
                  )
                ) # navbarPage
) # fluidPage

TIMP_IN_SISTEM <- function(ta, td) {
  # Cerinta 1: determinarea timpilor maximi, minimi si medii de asteptare pentru fiecare server
  tmax <- 0
  tmin <-  Inf
  tmed <-  0
  num <- 0
  for(i in 1:length(ta)) {
    if(td[i] > 0 && ta[i] > 0) { # nu luam in calcul persoanele care au plecat inainte sa ajunga la servere
      tmed <- tmed + (td[i] - ta[i])
      num <- num + 1
    }
    if(td[i] > 0 && ta[i] > 0 && td[i] - ta[i] > tmax)
      tmax <-  td[i] - ta[i]
    else if(td[i] > 0 && ta[i] > 0 && td[i] - ta[i] < tmin)
      tmin <-  td[i] - ta[i]
  }
  tmed <- tmed / num
  # print("TIMPI ASTEPTARE")
  # print(tmax)
  # print(tmin)
  # print(tmed)
  res <- c(tmax, tmin, tmed)
  return(res)
}


# Define server function  
server <- function(input, output) {
  
  # Cerinta 1
  res1 <- RUN_SIMULATION()
  server <-  c("Completare fisa", "Vaccinare")
  # Pentru primul server, timpul de asteptare este momentul cand intra in asteptare la serverul 2 - cand intra in sistem
  s1 <- TIMP_IN_SISTEM(res1$A1, res1$A2)
  # Pentru al doilea server, timpul de asteptare este momentul cand iese din sistem(D) - cand intra in al doilea server
  s2 <- TIMP_IN_SISTEM(res1$A2, res1$D)
  timp_maxim <- c(paste0(toString(round(s1[1],2)), " min"), paste0(toString(round(s2[1],2)), " min"))
  timp_minim <- c(paste0(toString(round(s1[2],2)), " min"), paste0(toString(round(s2[2],2)), " min"))
  timp_mediu <- c(paste0(toString(round(s1[3],2)), " min"), paste0(toString(round(s2[3],2)), " min"))
  df1 <- data.frame("Server"=server, "Timp minim"=timp_minim, "Timp maxim"=timp_maxim, "Timp mediu"=timp_mediu)
  
  # Cerinta 1
  output$table1 <- renderTable({df1})
  
  # Cerinta 2
  output$plotClientiTotal <- renderPlot({
    oraSt <- input$oraSt
    oraEnd <- input$oraEnd
    nrSim <- input$nrSim2
    x <- c()
    y <- c()
    med <- 0
    print(paste(oraSt, oraEnd))
    if(oraSt <= oraEnd) {
      for(i in 1 : nrSim) {
        res <- RUN_SIMULATION()
        print(res$D)
        nrPers <- 0
        for(j in 1 : length(res$D)) {
          if(((oraSt - 8) * 60) < res$D[j] && res$D[j] <= ((oraEnd - 8) * 60)) # nr persoane vaccinate intr-un interval de timp
            nrPers <- nrPers + 1
        }
        x <- append(x, i)
        y <- append(y, nrPers)
        med <- med + nrPers
      }
      med <- med / nrSim
      plot(x, y, col = "green",main="Persoane care se vaccineaza", xlab="Indexul simularii", ylab="Numarul persoane", pch = 19)
      abline(h=med, col="magenta")
      legend("topright", legend=c("Numar persoane vaccinate", "Media nr de persoane vaccinate"),
             col=c("green", "magenta"), lty=c(NA, 1), pch=c(19, NA))
      output$medie2 <- renderText({paste0("Numarul mediu de persoane vaccinate este de ", toString(round(med, 2)),".")})
    }
  })
  
  # Cerinta 3
  idx <- c(1:10) # realizam pe 10 simulari
  timp_plecare <- c()
  for(i in 1:10) {
    res <- RUN_SIMULATION()
    t <- Inf
    for(j in 1 : length(res$A2))
      if(res$A1[j] < 0 || res$A2[j] < 0 || res$D[j] < 0) { # a plecat de la una din cele 2 cozi sau nu avea loc la coada
        t <- abs(res$A1[j])
        break
      }
    if(t == Inf) {
      timp_plecare <- append(timp_plecare, " nu pleaca nimeni ") 
      
    } else {
      timp_plecare <- append(timp_plecare, paste0(toString(round(t, 2)), " min")) 
      
    }
  }
  df2 <- data.frame(idx, timp_plecare)
  # Cerinta 3
  output$tablePlecare <- renderTable({df2})
  
  # Suplimentar
  output$persNevaccinate <- renderText({
    res <- RUN_SIMULATION()
    nevaccinati <- 0
    if(res$nr_doze[1] == 0) {
      nevaccinati <- res$n1[1] + res$n2[1]
    } 
    paste0("Dintr-un total de 100 de doze, numarul persoanelor ramase nevaccinate din cauza numarului insuficient de doze este ", toString(nevaccinati), ".")})
  output$plotNrDozePierdute <- renderPlot({
    x <- c(1:10)
    y <- c()
    med <- 0
    for(i in 1:10) {
      res <- RUN_SIMULATION()
      y <- append(y, res$nr_doze[1])
      med <- med + res$nr_doze[1]
    }
    med <- med / 10
    plot(x, y, main="Numar de doze pierdute", xlab="Indexul simularii", ylab="Nr doze pierdute", pch = 19, col="green")
    abline(h=med, col="magenta")
    legend("topright", legend=c("Nr doze pierdute", "Medie nr doze pierdute"),
           col=c("green", "magenta"), lty=c(NA, 1), pch=c(19, NA))
    output$medieDoze <- renderText({paste0("Numarul mediu de doze pierdute este de ", toString(round(med, 2)),".")})
  })
  
  
  # Cerinta 4
  server_1 <- 0
  for(i in 1 : 10) {
    res <- RUN_SIMULATION()
    nr <- 0
    for(j in 1 : length(res$A1)) {
      if(res$A1[j] > 0 && res$A2[j] < 0 && res$D[j] < 0)
        nr <- nr + 1
    }
    server_1 <- server_1 + nr
  }
  server_1 <- server_1 / 10
  server_2 <- 0
  df3 <- data.frame(server_1, server_2)
  
  
  # Cerinta 4
  output$tablePlecareMediu <- renderTable({df3})
  
  # Cerinta 5
  output$plotCastig <- renderPlot({
    nrSim <- input$nrSim5
    dimCoada <- input$dimCoada
    ch <- input$choice
    
    x <- c()
    y <- c()
    med <- 0
    if(ch == 1) {
      
      for(i in 1 : nrSim) {
        res <- RUN_SIMULATION(13 * 60, LAMBDA_INCEPUT)
        x <- append(x, i)
        supl <- 0
        for(j in 1:length(res$D)) {
          if(res$D[j] > 0 && res$D[j] < 60) #s-au vaccinat in prima ora
            supl <- supl + 1
        }
        y <- append(y, supl)
        med <- med + supl
      }
      med <- med / nrSim
      
    }
    else if(ch == 2) {
      for(i in 1 : nrSim) {
        res <- RUN_SIMULATION(13 * 60)
        x <- append(x, i)
        supl <- 0
        for(j in 1:length(res$D)) {
          if(res$D[j] > 0 && res$D[j] <= 780 && res$D[j] >= 720) #s-au vaccinat in ultima ora
            supl <- supl + 1
        }
        y <- append(y, supl)
        med <- med + supl
      }
      med <- med / nrSim
      
    }
    else if(ch == 3) {
      for(i in 1 : nrSim) {
        res1 <- RUN_SIMULATION(c_max=dimCoada)
        res2 <- RUN_SIMULATION()
        nr_doze_res1 <- length(res1$D[res1$D > 0])
        nr_doze_res2 <- length(res2$D[res2$D > 0])
        x <- append(x, i)
        supl <- 0
        if(nr_doze_res1 > nr_doze_res2)
          supl <- nr_doze_res1 - nr_doze_res2
        y <- append(y, supl)
        med <- med + supl
      }
      med <- med / nrSim  
      
    }
    plot(x, y, col = "green",main="Numarul suplimentar de doze administrate obtinute pentru fiecare simulare", xlab="Indexul simularii", ylab="Numarul suplimentar de doze",
         pch = 19)
    abline(h=med, col="magenta")
    legend("topright", legend=c("Numar suplimentar de doze", "Media nr suplimentar de doze"),
           col=c("green", "magenta"), lty=c(NA, 1), pch=c(19, NA))
    output$medie5 <- renderText({paste0("Numarul de doze administrate suplimentar este de ", toString(round(med, 2)),".")})
  })
  
}

# Creeaza serverul Shiny App
shinyApp(ui = ui, server = server)