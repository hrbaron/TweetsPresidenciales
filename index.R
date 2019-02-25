##APP1  TWITTER STREAM
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("RCurl") 
#install.packages("ROAuth")
#install.packages("RJSONIO")
#install.packages("leaflet")
install.packages("streamR")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("DT")
library(ggplot2)
library(ggmap)
library(shiny)
library(shinyWidgets)
library(RCurl)
library(RJSONIO)
library(ROAuth)
library(leaflet)
library(streamR)
library(tm)
library(SnowballC)
library(DT)

ruta <-"C:/Users/hbaronfr/Desktop/CONEXION/"
#ruta <-"D:/Universidad/Diplomado/Rstudio/"
setwd(ruta) 
candidatos <-read.csv("candidatos_presidente.csv",sep = ",",
                      stringsAsFactors = F)
#Se crea el dataFrame
dfCandidatos <-data.frame(candidatos)
candidatosPresi <-unique(dfCandidatos$Nombre)
candidatosPresi <-candidatosPresi[order(candidatosPresi,decreasing=F)]
#setwd("C:/Users/cgordillo/OneDrive - Asesooftware S.A.S/BKASW/Documentos/Cesar/Twitter/")
shinyApp(
  ui <- pageWithSidebar(
    #Titulo de la App
    headerPanel("Analisis Tweets"),
    
    #Crear sideBar panel para Liga
    sidebarPanel(
      
      #Input: Selector de la variable a visualizar
      selectInput("candidato","Candidato: ",
                  candidatosPresi),
      
      #Input: Selector de la variable a visualizar
      textInput("lat", label = "Latitud:", value = 4.57),
      
      #Input 2: radioBox para inclusion
      textInput("long", label = "Longitud:", value = -74.29),
      
      chooseSliderSkin("Modern"),
      sliderInput("tiempoGeneracion", "Tiempo procesamiento (Segundos)",
                  min = 10, max =3000, value = 20)
    ),     
    
    #Crear mainPanel que visualiza resultados
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", h2(textOutput("encabezado")),
                                      leafletOutput("myMap")),
                  tabPanel("Tabla", h3(textOutput("twwts")),
                           DT::dataTableOutput('table')),
                  tabPanel("Graficas", plotOutput("histograma"))
      )
      
      #Salida uno ---> Texto
      #),
      #Salida dos ---> Mapa
      #Salida tres ---> Texto
      #,
      #Salida cuatro ---> Tabla
      
      
    )
  ),
  
  
  server = function(input, output) {
    
    #Declaracion de las credenciales de la API de twitter
    consumer_key <- "9D1mcibkU0zu3oMAESphjkpbK"
    consumer_secret <- "gCRVGbIE85i0kfazJSfWAJBAp30GvK8cQFO6v8uy69g5IJn5mg"
    access_token <- "936771334809575424-5lXALAAynvJhIoAUpZGQWEVP2LQYRTJ"
    access_secret <- "ZFkg8mdVCPfltReSL4rAr8tKxQznyh7Vn6U0vfOn1beYb"
    requestURL <- "https://api.twitter.com/oauth/request_token"
    accessURL <- "https://api.twitter.com/oauth/access_token"
    authURL <- "https://api.twitter.com/oauth/authorize"
    frecuencia_sentimientos <- 0
    parametros_barplot <- 0
    
    options(httr_oauth_cache = TRUE) # enable using a local file to cache OAuth access credentials between R sessions
    my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
                                 consumerSecret = consumer_secret,
                                 requestURL = requestURL,
                                 accessURL = accessURL,
                                 authURL = authURL)
    
    # Ejecuta el protocolo OAUth  (handshake) - se autoriza a la aplicacion mediante el ingreso de un PIN de 7 digitos
    my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    
    
    # Issue search query to Twitter
    observe({
      
      # Create a reactive table 
      output$table <- DT::renderDataTable(
        colnames = c('Registro','Tweet',"Clasificacion Bayes"), 
        options = list(pageLength = 10),
        {
          usuario_twitter <-paste("@",
                                  dfCandidatos$Usuario_twitter[dfCandidatos$Nombre ==input$candidato])
          usuario_twitter <-gsub(" ", "", usuario_twitter)
          filterStream(file.name="tweets_candi.json",
                       locations = c(input$long, input$lat, -73.39, 5.57), 
                       track= c(usuario_twitter), oauth=my_oauth, 
                       timeout = input$tiempoGeneracion,
                       lang="es")
          
          json_candidatos<- parseTweets(tweets='tweets_candi.json', simplify = FALSE)
          texto=json_candidatos$text
          
          #IMPRIMIR MARCADORES SEGUN EL JSON GENERADO
          json_txt <- readLines(paste0("C:/Users/hbaronfr/Desktop/CONEXION/",
                                       "tweets_candi.json"), warn=FALSE)
          
          latitudes <- c(json_candidatos$place_lat)
          longitudes <- c(json_candidatos$place_lon)
          latitudes <- latitudes[latitudes!= "NaN"]
          longitudes <- longitudes[longitudes!= "NaN"]
          
          # Create a reactive leaflet map
          mapTweets <- reactive({
            map = leaflet() %>% addTiles() %>%
              addMarkers(c(longitudes), c(latitudes), popup = "test") %>%
              setView(input$long, input$lat, zoom = 11)
          })
          
          
          output$myMap = renderLeaflet(mapTweets())
          
          # -------------------------------
          # Pre-procesamiento de los datos 
          # -------------------------------
          # Elimina retweets
          unique(texto)
          limpia_texto = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", texto)
          # Elimina puntuacion
          limpia_texto = gsub("[[:punct:]]", "", limpia_texto)
          # Elimina numeros
          limpia_texto = gsub("[[:digit:]]", "", limpia_texto)
          # Elimina URLs
          limpia_texto = gsub("http\\w+", "", limpia_texto)
          limpia_texto = gsub("http[^[:blank:]]+", "", limpia_texto)
          # Elimina a las personas
          limpia_texto = gsub("@\\w+", "", limpia_texto)
          # Elimina espacios innecesarios
          limpia_texto = gsub("[^[:alnum:]]", " ", limpia_texto)
          limpia_texto = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", limpia_texto, perl=TRUE)
          # Elimina emojis o caracteres especiales
          limpia_texto = gsub('<.*>', '', enc2native(limpia_texto))
          # Convierte mayusculas a minusculas
          limpia_texto = tolower(limpia_texto)
          
          # Elimina palabras vacias (Stopwords)
          limpia_texto <- removeWords(limpia_texto, words = stopwords("spanish"))
          # Aplicacion de Stemming: Principalmente recorta las palabras a su raiz
          limpia_texto <- stemDocument (limpia_texto, language = "spanish") 
          #head( limpia_texto, n = input$cantidadTweets )
          
          # ---------------------------------------------
          # Clasificacion del sentimiento
          # ---------------------------------------------
          
          #------------------------------------
          #Clasificador Bayesiano Jose Figueroa
          #------------------------------------
          # instalacion de funciones necesarias para la  clasificacion de la polaridad
          # de la autoria de Jose Cardona Figueroa que implementa un diccionario de 
          # palabras en español para clasificacion segun el sentimiento. Estos archivos
          # se han puesto enla ruta de directorio de trabajo en rstudio.
          source('classify_polarity1.R') 
          source('create_matrix1.R')
          # Funcion de clasificacion del sentimiento
          clasificacion= classify_polarity(limpia_texto, algorithm="bayes", minWordLength = 1)
          
          # Seleccionamos solo la columna 4, que indica la polaridad
          polarity = clasificacion[,4]
          
          frecuencia_sentimientos <- table(polarity)
          parametros_barplot <-c(unique(polarity))
          cantidadTweets <-1:length(limpia_texto)
          
          output$histograma <- renderPlot({
            barplot(as.matrix(frecuencia_sentimientos),
                    beside=TRUE,
                    horiz = F,
                    col=c("darkblue","red"),
                    names.arg=parametros_barplot, 
                    width = 0.2,
                    ylab = "Frecuencia de sentimientos",
                    main="Sentimientos de tweets") 
          })
          
          
          result = as.matrix(cbind(cantidadTweets, limpia_texto,polarity)) 
          
          data <- result
        }
      )
      
      output$twwts<- renderText({
        paste("Consolidado de Tweets para ",input$candidato)
      })
      
      output$encabezado<- renderText({
        paste("Ubicacion de Tweets para ",input$candidato)
      })
    }) 
      
    

    
  }
)
