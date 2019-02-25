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
# Paquete SVM
#install.packages("caTools")
#install.packages("e1071")
#install.packages("plyr")
#install.packages("caret")

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

library(caTools)
library(e1071)
library(plyr)
library(caret)

library(tm)
library(SnowballC)
library(DT)




ruta <-"C:/Tweets_Candidatos/"
setwd(ruta) 
candidatos <-read.csv("candidatos_presidente.csv",sep = ",",
                      stringsAsFactors = F)
#Se crea el dataFrame
dfCandidatos <-data.frame(candidatos)
candidatosPresi <-unique(dfCandidatos$Nombre)
candidatosPresi <-candidatosPresi[order(candidatosPresi,decreasing=F)]
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
                  min = 10, max =3000, value = 30)
    ),     
    
    #Crear mainPanel que visualiza resultados
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", h2(textOutput("encabezado")),
                           leafletOutput("myMap")),
                  tabPanel("Tabla", h3(textOutput("twwts")),
                           DT::dataTableOutput('table')),
                  tabPanel("Grafica Bayes", plotOutput("histograma")),
                  tabPanel("Grafica SVM", plotOutput("histogramaCaret"))
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
        colnames = c('Registro','Tweet',"Clasificacion Bayes",
                     "Clasificacion SVM"), 
        options = list(pageLength = 10),
        {
          usuario_twitter <-paste("@",
                                  dfCandidatos$Usuario_twitter[dfCandidatos$Nombre ==input$candidato])
          usuario_twitter <-gsub(" ", "", usuario_twitter)
          cat(usuario_twitter)
          if (file.exists("tweets_candidatos.json")) 
          {
            cat("eliminar archivo json")
            file.remove("tweets_candidatos.json")
          }
          filterStream(file.name="tweets_candidatos.json",
                       locations = c(input$long, input$lat, -73.39, 5.57), 
                       track= c(usuario_twitter), oauth=my_oauth, 
                       timeout = input$tiempoGeneracion,
                       lang="es")
          
          json_candidatos<- parseTweets(tweets='tweets_candidatos.json', simplify = FALSE)
          texto=json_candidatos$text
          
          #IMPRIMIR MARCADORES SEGUN EL JSON GENERADO
          json_txt <- readLines(paste0("C:/Tweets_Candidatos/",
                                       "tweets_candidatos.json"), warn=FALSE)
          
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
          texto <- limpia_texto
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
          
          sentimientosPolar <-c(polarity)
          sentimientosPolar <-replace(sentimientosPolar, 
                                      sentimientosPolar=="negative",
                                      "Negativo")
          sentimientosPolar <-replace(sentimientosPolar, 
                                        sentimientosPolar=="positive",
                                        "Positivo")
          frecuencia_sentimientos <- table(sentimientosPolar)
          parametros_barplot <-c(unique(sentimientosPolar))
          
          cantidadTweets <-1:length(limpia_texto)
          
          output$histograma <- renderPlot({
            barplot(as.matrix(frecuencia_sentimientos),
                    beside=TRUE,
                    horiz = F,
                    col=c("darkblue","red"),
                    names.arg=parametros_barplot, 
                    width = 0.2,
                    ylab = "Frecuencia de sentimientos",
                    main="Sentimientos Bayes") 
          })
          
          
          #-----------------------------------
          #Clasificador Support Vector Machine
          #-----------------------------------
          
          # Construye un corpus 
          documento <- Corpus(VectorSource(limpia_texto))
          #Convierte el texto a minuscula
          documento <- tm_map(documento, content_transformer(tolower))
          # Elimina palabras vacias en español
          documento <- tm_map(documento, removeWords, stopwords("es"))
          # Reduce las palabras a su raiz
          documento <- tm_map(documento, stemDocument, language = "es")
          
          # SELECCION DE LAS CARACTERISTICAS
          
          #Crea la matriz de documentos
          Frecuencia <- DocumentTermMatrix(documento)
          # Palabras mas frecuentes en los tweets, frecuencia minima (aparicion) de 40 veces
          findFreqTerms(Frecuencia, lowfreq = 40)
          # Crea matriz  que elimina palabras que se mencionan poco
          sparse <- removeSparseTerms(Frecuencia, 0.97)
          # Retorna la matrix de sparse como un dataframe en R para trabajar el modelo SVM
          textoSparse <- as.data.frame(as.matrix(sparse))
          # Asigna los nombres de las palabras a las columnas del dataframe
          colnames(textoSparse)<-make.names(colnames(textoSparse))
          # Agrega la variable de sentimiento previamente clasificada al dataframe textoSparse
          # con el fin de que le enseñe como clasificar los tweets y este aprenda
          elecciones2 <-read.csv("elecciones2.csv",sep = ",",
                                 stringsAsFactors = F)
          #Se crea el dataFrame
          textoSparse$sentimiento<-sample(0,nrow(textoSparse),replace=T)
          if(length(elecciones2$sentimiento)>length(textoSparse$sentimiento))
          {
            for (posicion in 1:nrow(textoSparse)){
              textoSparse$sentimiento[posicion]=elecciones2$sentimiento[posicion]
            }  
          }else
          {
            posicion <- 1
            while(posicion <nrow(textoSparse))
            {
              for (rowArchivo in 1:length(elecciones2$sentimiento))
              {
                textoSparse$sentimiento[posicion]=elecciones2$sentimiento[rowArchivo]
                posicion<-posicion+1
              }
            }
          }
          # establece el mismo nivel de aleatoridad para los datos en Rstudio
          set.seed(12)
          # # Define el 80% de las observaciones para el conjunto de datos de entrenamiento
          split<-sample.split(textoSparse$sentimiento, SplitRatio = 0.8)# Define el 80% de las observaciones para el conjunto de datos de entrenamiento
          # Define un conjunto de entrenamiento
          trainSparse = subset(textoSparse, split==TRUE)
          # Define un conjunto de evaluacion
          testSparse = subset(textoSparse, split==FALSE)
          
          # Identifica como se comporta el sentimiento en el dataframe
          table(textoSparse$sentimiento)
          # Clasificacion con modelo SVM; Necesariamente se debe tratar la variable sentimiento como un factor
          # trabaja sobre la base datos de entrenamiento para enseñarle al clasificador
          trainSparse$sentimiento<-factor(trainSparse$sentimiento)
          require(e1071)
          SVM<-svm(sentimiento~ .,data=trainSparse, kernel = "linear",scale = FALSE)
          # Muestra un resumen del clasificador para observar como esta el modelo, es decir una descripcion del modelo
          summary(SVM)
          # comportamiento del modelo haciendo predicciones, con los nuevos datos
          predictSVM<-predict(SVM, newdata = textoSparse)
          
          sentimientosPredict <-c(predictSVM)
          sentimientosPredict <-replace(sentimientosPredict, 
                                        sentimientosPredict==1, "Negativo")
          sentimientosPredict <-replace(sentimientosPredict, 
                                        sentimientosPredict==2, "Positivo")
          frecuencia_sentim_caret <- table(sentimientosPredict)
          parametros_barp <-c(unique(sentimientosPredict))
          
          output$histogramaCaret <- renderPlot({
            barplot(as.matrix(frecuencia_sentim_caret),
                    beside=TRUE,
                    horiz = F,
                    col=c("darkblue","red"),
                    names.arg=parametros_barp, 
                    width = 0.2,
                    ylab = "Frecuencia de sentimientos Caret",
                    main="Sentimientos SVM") 
          })
          
          result = as.matrix(cbind(texto,sentimientosPolar,sentimientosPredict)) 
          
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
