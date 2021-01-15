library(shiny)
library(ggplot2)
library(zip)
library(httr)
library(rvest)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(rtweet)
library(igraph)


#Leo NewYorkTimes

scraping_wiki <- read_html("https://www.nytimes.com/")

all_text <- scraping_wiki %>%
    html_nodes("div") %>% 
    html_text() %>% 
    strsplit(split = "\n") %>%
    unlist() %>%
    .[. != ""]

all_text %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")

all_text <- as.data.frame(all_text)
all_text <- all_text %>%
    unnest_tokens(word, all_text)

all_text <- cbind(all_text, Pagina='https://www.nytimes.com/')

all_text1 <- all_text



#Leo ABC de Australia

scraping_wiki <- read_html("https://www.abc.net.au/")

all_text <- scraping_wiki %>%
    html_nodes("div") %>%
    html_text() %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    .[. != ""]

all_text %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")

all_text <- as.data.frame(all_text)
all_text <- all_text %>%
    unnest_tokens(word, all_text)

all_text <- cbind(all_text, Pagina='https://www.abc.net.au/')

all_text2 <- all_text



#Leo BBC de Londres

scraping_wiki <- read_html("https://www.bbc.com/")

all_text <- scraping_wiki %>%
    html_nodes("div") %>%
    html_text() %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    .[. != ""]

all_text %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")

all_text <- as.data.frame(all_text)
all_text <- all_text %>%
    unnest_tokens(word, all_text)

all_text <- cbind(all_text, Pagina='https://www.bbc.com/')

all_text3 <- all_text

all_text <- rbind(all_text1, all_text2, all_text3)




#Extraigo las src de cada imagen

DWIU <- function(variable,ip) {
    
    unlink("Zip", recursive = TRUE)
    
    print(variable)
    x <- toString(variable) 
    x <- paste("https://weheartit.com/", x, sep="")
    
    ipt <- read_html(x) %>%
        html_nodes("#content") %>%
        html_attr("data-infinite-scroll-count")
    
    print(paste(ipt,"paginastot",sep="."))
    ipt <- as.numeric(ipt)
    
    ipt <<- ipt
    
    print(paste(ipt,"paginastot",sep="."))
    
    if(ip==0) {
        
        ip <- 1
        
    } else {
        
    }
    
    if(ip > ipt) {
        
        ip <- ipt
        
    } else {
        
    }
    
    ipp <- 1
    
    for(i in 1:ip) {
        
        pagen <- x
        pagen <- paste(pagen,"?page=",sep="")
        pagen <- paste(pagen,ipp,sep="")
        
        if(ipp==1) {
            wh <- read_html(pagen) %>%
                html_nodes(".entry-thumbnail") %>%
                html_attr("src")
            
            wh <- as.data.frame(wh)
            colnames(wh)[1] = "durl"
            
        } else {
            
            wh1 <- read_html(pagen) %>%
                html_nodes(".entry-thumbnail") %>%
                html_attr("src")
            
            wh1 <- as.data.frame(wh1)
            colnames(wh1)[1] = "durl"
            wh <- rbind(wh, wh1)
        }
        
        ipp <- ipp +1
        
    }
    
    dir.create("Zip")
    
    ii <- 1
    iii <- 1
    
    #Descargo las imagenes
    
    for(i in 1:nrow(wh)) {
        
        iii <- iii +1
        
        st <- wh[ii,1]
        
        st <- substring(st, first = 1, last = 40)
        st <- paste(st,"/original.jpg",sep="")
        destf <- paste("Zip/Imagen",ii,sep="")
        destf <- paste(destf,".jpg",sep="")
        
        result <- "Error?"
        result = tryCatch({
            download.file(url = st, destfile = destf)
        }, warning = function(w) {
            print(paste("MY_WARNING:  ",w))
            w <- ""
        }, error = function(e) {
            print(paste("MY_ERROR:  ",e))
            e <- ""
        }, finally = {
            print(paste("result =",result))
            
        })
        
        ii <- ii +1
        
    }
}

DWIUZ <- function(variable) {
    
    print(variable)
    z <- toString(variable)
    print(z)
    z <- paste("Zip/", z, ".zip", sep="")
    zip(z, c("Zip"))
    
}




#Creo los datos para los dropdown

Pagina <- c('https://www.nytimes.com/','https://www.abc.net.au/','https://www.bbc.com/')
Tipo <- c("Noticias", "Noticias", "Noticias")
Lugar <- c("US", "Australia", "England")

shai <- data.frame(Pagina, Tipo, Lugar)

Archivo <- c('NYT.csv','abc.csv','bbc.csv')
Tipo <- c("Noticias", "Noticias", "Noticias")
Lugar <- c("US", "Australia", "England")

shaii <- data.frame(Archivo, Tipo, Lugar)




ui <- fluidPage(
    
    titlePanel("WordCloud & Tweets"),
    div(style="display: inline-block;", h6(style="display: inline-block;", "By: "), a(href="https://lucashrossi.github.io/portfolio.github.io/", target="_blank", "Hernan")),
    br(),
    
    tabsetPanel(
        tabPanel('Palabras en la pagina',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput('graficar',
                                     label='Pagina',
                                     choices = unique(shai$Pagina),
                                     selected = unique(shai$Pagina)[1],
                                     multiple = FALSE
                         ),
                         sliderInput('bins','Cantidad de palabras',
                                     min=50,max=1000, value=100),
                         width = 4
                     ),
                     
                     mainPanel(
                         fluidRow(column(12, plotOutput('WC')))
                     )
                 )
        ),
        
        tabPanel('Nodos, Fruchterman Reigold, Walktrap',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput('graficarr',
                                     label='Archivo:',
                                     choices = unique(shaii$Archivo),
                                     selected = unique(shaii$Archivo)[1],
                                     multiple = FALSE
                         ),
                         sliderInput('binss','Cantidad de nodos',
                                     min=50,max=1000, value=100),
                         width = 4
                     ),
                     
                     mainPanel(
                         fluidRow(column(12, plotOutput('ND'))),
                         fluidRow(column(12, plotOutput('NDF'))),
                         fluidRow(column(12, plotOutput('NDW')))
                     )
                 )
        ),
        
        tabPanel('Descarga',
                 sidebarLayout(
                     sidebarPanel(
                         textInput("n", "User Name of WeHeartIt:"),
                         br(),
                         sliderInput('binssd','Cantidad de paginas:',
                                     min=1,max=50, value=1),
                         br(),
                         br(),
                         actionButton("goButton", "Go!"),
                         br(),
                         br(),
                         br(),
                         downloadButton("downloadData", label = "Download"),
                         width = 4
                         
                     ),
                     mainPanel(
                         verbatimTextOutput("nText"),
                         verbatimTextOutput("nTextf")
                     ),
                 )
        )
    )
    
)

server <- function(input, output, session) {
    
    ntext <- eventReactive(input$goButton, {
        
        bool <<- TRUE
        input$n
    })
    
    ntextf <- eventReactive(input$goButton, {
        
        bool <<- TRUE
        input$n
    })
    
    output$nText <- renderText({
        
        bool <<- TRUE
        
    })
    
    output$nTextf <- renderText({
        
        nub<-ntextf()
        
        if(bool==FALSE || nub==""){
            
            print("Elija usuario")
            
        } else {
            
            DWIU(ntextf(),ip=input$binssd)
            
            "Finalizado!"
            
            DWIUZ(ntextf())
            
            nub <- ""
            updateTextInput(session, "n", value = "")
            print("")
            bool <<- FALSE
            paste("Downloaded: ", ntext(), ". Finalizado Zip!", sep="")
            
        }
        
    })
    
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste(ntext(), "zip", sep=".")
        },
        
        content = function(file) {
            file.copy(paste("Zip/", ntext(), ".zip", sep=""), file)
        },
        contentType = "application/zip"
    )
    
    dds <- reactive({
        shai <- shai[shai$Pagina==input$graficar,]
        shai
    })
    
    ddss <- reactive({
        shaii <- shaii[shaii$Archivo==input$graficarr,]
        shaii
    })
    
    output$WC <- renderPlot({
        
        #Grafico la WordCloud seleccionada
        
        all_text %>%
            filter(Pagina == input$graficar) %>%
            anti_join(stop_words) %>%
            count(word, sort=T) %>%
            with(wordcloud(word, n, scale=c(1.7,0.25), max.words = input$bins, random.order=FALSE, color = brewer.pal(8, "Dark2")))
        
    })
    
    output$ND <- renderPlot({
        
        #Grafico los nodos seleccionados
        
        tw_toblerone <- read.csv(input$graficarr,nrows=input$binss)
        drop <- c("X")
        tw_toblerone = tw_toblerone[,!(names(tw_toblerone) %in% drop)]
        
        nodos <- gather(data = tw_toblerone, key = "tipo", value = "identificacion", c(1,54))
        nodos$retweet_count <- as.numeric(nodos$retweet_count)
        nodos$favorite_count <- as.numeric(nodos$favorite_count)
        
        which.duplicates<-rownames(nodos[duplicated(nodos$identificacion),])
        nodos <- nodos[-c(as.integer(which.duplicates)),]
        
        nodos <- nodos %>% select(identificacion, screen_name, is_retweet, favorite_count, retweet_count, verified)  %>% rename( name = screen_name)
        
        links <- tw_toblerone %>% group_by(user_id, retweet_user_id) %>% summarise(cantidad = n()) %>%
            rename(from = user_id,
                   to = retweet_user_id,
                   friendship = cantidad)
        
        print(input$binss)
        ndg <- as.numeric(input$binss)
        print(ndg)
        
        g <- graph_from_data_frame(links, directed=TRUE, vertices=nodos)
        V(g)$label.cex <- seq(0.01,1,length.out=500)        
        V(g)$size      <- seq(1,10,length.out=500)
        
        plot(g, vertex.label = V(g)$name,
             vertex.shape="circle",
             vertex.color="red")
        
    })
    
    output$NDF <- renderPlot({
        
        #Grafico fruchterman.reingold seleccionado
        
        tw_toblerone <- read.csv(input$graficarr,nrows=input$binss)
        drop <- c("X")
        tw_toblerone = tw_toblerone[,!(names(tw_toblerone) %in% drop)]
        
        nodos <- gather(data = tw_toblerone, key = "tipo", value = "identificacion", c(1,54))
        nodos$retweet_count <- as.numeric(nodos$retweet_count)
        nodos$favorite_count <- as.numeric(nodos$favorite_count)
        
        which.duplicates<-rownames(nodos[duplicated(nodos$identificacion),])
        nodos <- nodos[-c(as.integer(which.duplicates)),]
        
        nodos <- nodos %>% select(identificacion, screen_name, is_retweet, favorite_count, retweet_count, verified)  %>% rename( name = screen_name)
        
        links <- tw_toblerone %>% group_by(user_id, retweet_user_id) %>% summarise(cantidad = n()) %>%
            rename(from = user_id,
                   to = retweet_user_id,
                   friendship = cantidad)
        
        gf <- graph_from_data_frame(links, directed=TRUE, vertices=nodos)
        gf.outd <- degree(gf, mode = c("out"))
        gf.ind <- degree(gf, mode = c("in"))
        my.label<- names(gf.ind)
        my.label[which(log(gf.ind+1) < 2.1)]<- "" 
        my.label2<- my.label
        l <- layout.fruchterman.reingold(gf, grid = c("nogrid"))
        plot(gf, vertex.label = my.label2,  vertex.size = 5, layout = l)
        
    })
    
    output$NDW <- renderPlot({
        
        #Grafico walktrap.community seleccionado
        
        tw_toblerone <- read.csv(input$graficarr,nrows=input$binss)
        drop <- c("X")
        tw_toblerone = tw_toblerone[,!(names(tw_toblerone) %in% drop)]
        
        nodos <- gather(data = tw_toblerone, key = "tipo", value = "identificacion", c(1,54))
        nodos$retweet_count <- as.numeric(nodos$retweet_count)
        nodos$favorite_count <- as.numeric(nodos$favorite_count)
        
        which.duplicates<-rownames(nodos[duplicated(nodos$identificacion),])
        nodos <- nodos[-c(as.integer(which.duplicates)),]
        
        nodos <- nodos %>% select(identificacion, screen_name, is_retweet, favorite_count, retweet_count, verified)  %>% rename( name = screen_name)
        
        links <- tw_toblerone %>% group_by(user_id, retweet_user_id) %>% summarise(cantidad = n()) %>%
            rename(from = user_id,
                   to = retweet_user_id,
                   friendship = cantidad)
        
        gw <- graph_from_data_frame(links, directed=TRUE, vertices=nodos)
        ll <- layout.fruchterman.reingold(gw, grid = c("nogrid"))
        wc <- walktrap.community(gw)
        users_wc <- membership(wc)
        new.color<-data.frame(t(col2rgb(wc$membership)/255))
        new.color<-rgb(new.color, alpha=.6)
        gw.ind <- degree(gw, mode = c("in"))
        plot(wc, gw, vertex.label = NA,  vertex.size=log(gw.ind+1), vertex.color=new.color, edge.size = 0.5, edge.color = "grey",  layout = ll)
        
    })
    
}


shinyApp(ui = ui, server = server)
