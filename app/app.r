library(shiny)
library(DT)
library(shinythemes)
library(openxlsx)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyr)

ui = shinyUI(navbarPage(title = "Mk",
                   tabPanel("Analises",
                            fluidPage( theme = shinytheme("spacelab"),
                                       titlePanel("Previsao de jogos utilizando Poisson"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("anovigente","Selecione a temporada",
                                                       c("ALL",
                                                         "2001",
                                                         "2002",
                                                         "2003",
                                                         "2004",
                                                         "2005",
                                                         "2006",
                                                         "2007",
                                                         "2008",
                                                         "2009",
                                                         "2010",
                                                         "2011",
                                                         "2012",
                                                         "2013",
                                                         "2014",
                                                         "2015",
                                                         "2016",
                                                         "2017",
                                                         "2018",
                                                         "2019"), selected = "2019", selectize = TRUE
                                           ),
                                           selectInput("teamhome","Selecione o time da casa",
                                                       c("Sao Paulo",
                                                         "Atletico-MG",
                                                         "Chapecoense-SC",
                                                         "Flamengo RJ",
                                                         "Gremio",
                                                         "Athletico-PR",
                                                         "Bahia",
                                                         "Ceara",
                                                         "Fluminense",
                                                         "Palmeiras",
                                                         "Corinthians",
                                                         "CSA",
                                                         "Internacional",
                                                         "Avai",
                                                         "Cruzeiro",
                                                         "Fortaleza",
                                                         "Goias",
                                                         "Vasco",
                                                         "Santos",
                                                         "Botafogo RJ"), selected = "Palmeiras", selectize = TRUE
                                           ),
                                           selectInput(
                                             "teamaway","Selecione o time de fora",
                                             c("Sao Paulo",
                                               "Atletico-MG",
                                               "Chapecoense-SC",
                                               "Flamengo RJ",
                                               "Gremio",
                                               "Athletico-PR",
                                               "Bahia",
                                               "Ceara",
                                               "Fluminense",
                                               "Palmeiras",
                                               "Corinthians",
                                               "CSA",
                                               "Internacional",
                                               "Avai",
                                               "Cruzeiro",
                                               "Fortaleza",
                                               "Goias",
                                               "Vasco",
                                               "Santos",
                                               "Botafogo RJ"), selected = "Corinthians", selectize = TRUE
                                           )
                                         ),
                                         
                                         mainPanel(
                                           tabsetPanel(type = "tab",
                                                       tabPanel("Geral",textOutput("text1"),tableOutput("poissongeral"),plotOutput(outputId = "linegraph")),
                                                       #h2("main panel",align = "center"),    
                                                       tabPanel("Times",
                                                                tableOutput("mediasgerais"),
                                                                #fluidRow(htmlOutput("tmod1")), # Header, I presume ?
                                                                fluidRow(
                                                                  column(width = 2, uiOutput("img1")),
                                                                  column(width = 1,h2(""),h2(""),tags$img(src = "X.jpg",height = 50, width = 50)),
                                                                  column(width = 2, uiOutput("img2"))),
                                                                #uiOutput("img1"),
                                                                #tags$img(src = "X.jpg",height = 50, width = 50),
                                                                #uiOutput("img2"),
                                                                tableOutput("infostimes") ))
                                           
                                         )
                                       )
                            )
                   )
)
)

resultados = read.xlsx("E:\\1 - Estudos\\1 - Projetos\\Hobby\\Futebol\\FutebolPoissonExcel\\Poisson Brasileiro.xlsx",sheet = "BRA")

server = shinyServer(function(input,output){
  
  output$img1 <- renderUI({
    if(input$teamhome == "Palmeiras"){            
      img(height = 100, width = 100, src = "palmeiras.png")
    }                                        
    else if(input$teamhome == "Corinthians"){
      img(height = 100, width = 100, src = "corinthians.png")
    }
    else if(input$teamhome == "Fluminense"){
      img(height = 100, width = 100, src = "fluminense.png")
    }
  })
  
  output$img2 <- renderUI({
    if(input$teamaway == "Palmeiras"){            
      img(height = 100, width = 100, src = "palmeiras.png")
    }                                        
    else if(input$teamaway == "Corinthians"){
      img(height = 100, width = 100, src = "corinthians.png")
    }
    else if(input$teamaway == "Fluminense"){
      img(height = 100, width = 100, src = "fluminense.png")
    }
  })
  
  output$text1 <- renderText({ 
    paste("Media de gols - Temporada:", input$anovigente)
  })
  
  output$poissongeral <- renderTable({
    
    if (input$anovigente == "ALL") {
      
      output_gols_geral = data.frame(MGG = (sum(resultados$HG) + sum(resultados$AG))/nrow(resultados),
                                     MGGH = sum(resultados$HG)/nrow(resultados),
                                     MGGA = sum(resultados$AG)/nrow(resultados))
    } else {
      
      output_gols_geral = resultados %>% filter(Season == input$anovigente) %>% 
        summarise(MGG = (sum(HG) + sum(AG))/n(),
                  MGGH = mean(HG),
                  MGGA = mean(AG))
    }
    
  })
  
  output$linegraph <- renderPlot({
    
    resultados_graph = resultados %>% group_by(Season) %>% summarise(MGG = (sum(HG) + sum(AG))/n(),
                                                                     MGGH = mean(HG),
                                                                     MGGA = mean(AG)) %>% data.frame()
    resultados_graph = resultados_graph %>% gather(Medias, Valor, 2:4)
    
    ggplot(data=resultados_graph, aes(x=Season, y=Valor, group=Medias)) +
      geom_line(aes(color=Medias))+
      ylab("Media de gols") +
      xlab("Temporada") +
      geom_point(aes(color=Medias)) +
      scale_x_continuous(breaks = seq(2001,2019,1)) +
      theme_classic(base_size = 18) + 
      theme(legend.position="bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
  }, height = 400, width = 600)
  
  output$mediasgerais <- renderTable({
    
    mgh        = resultados %>% filter(Season == "2019") %>% summarise(mgh = sum(HG)/n())
    mga        = resultados %>% filter(Season == "2019") %>% summarise(mga = sum(AG)/n())
    golshome   = resultados %>% filter(Home == input$teamhome & Season == "2019") %>% summarise(GH = sum(HG)/n())
    golsaway   = resultados %>% filter(Away == input$teamaway & Season == "2019" ) %>% summarise(GA = sum(AG)/n())
    fahome     = golshome/mgh
    faaways    = golsaway/mga
    gchome     = resultados %>% filter(Home == input$teamhome & Season == "2019") %>% summarise(GCH = sum(AG)/n())
    gcaway     = resultados %>% filter(Away == input$teamaway & Season == "2019" ) %>% summarise(GCA = sum(HG)/n())
    fdhome     = gchome/mga
    fdaways    = gcaway/mgh
    
    output_geral = data.frame(MGH = mgh$mgh,
                              MGA = mga$mga)
  })
  
  output$infostimes <- renderTable({
    
    mgh        = resultados %>% filter(Season == "2019") %>% summarise(mgh = sum(HG)/n())
    mga        = resultados %>% filter(Season == "2019") %>% summarise(mga = sum(AG)/n())
    golshome   = resultados %>% filter(Home == input$teamhome & Season == "2019") %>% summarise(GH = sum(HG)/n())
    golsaway   = resultados %>% filter(Away == input$teamaway & Season == "2019" ) %>% summarise(GA = sum(AG)/n())
    fahome     = golshome/mgh
    faaways    = golsaway/mga
    gchome     = resultados %>% filter(Home == input$teamhome & Season == "2019") %>% summarise(GCH = sum(AG)/n())
    gcaway     = resultados %>% filter(Away == input$teamaway & Season == "2019" ) %>% summarise(GCA = sum(HG)/n())
    fdhome     = gchome/mga
    fdaways    = gcaway/mgh
    
    output_infos = data.frame(Time = c(input$teamhome,input$teamaway),
                              MGF = c(golshome$GH,golsaway$GA),
                              FA = c(fahome$GH,faaways$GA),
                              MGC = c(gchome$GCH,gcaway$GCA),
                              FD = c(fdhome$GCH,fdaways$GCA))
    
  })
  
})

shinyApp(ui, server)