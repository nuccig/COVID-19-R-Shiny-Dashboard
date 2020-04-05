#librarys
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(colorspace))
suppressMessages(library(rsconnect))
suppressMessages(library(shinyWidgets))
suppressMessages(library(plotly))
suppressMessages(library(DT))
suppressMessages(library(geobr))
suppressMessages(library(leaflet))
suppressMessages(library(ggiraph))
suppressMessages(library(sf))
suppressMessages(library(datacovidbr))

#leitura dos dados
WRL_Pais <- CSSEGISandData(silent = T) %>% ungroup()
WRL_Regiao <- CSSEGISandData(by_country = F, silent = T) %>% ungroup()
BR_Cidade <- brasilio(silent = T) %>% filter(place_type == "city") %>% select(-c("place_type", "is_last", "estimated_population_2019", "confirmed_per_100k_inhabitants", "death_rate"))
BR_Estado <- brasilio(silent = T) %>% filter(place_type == "state") %>% select(-c("place_type", "is_last", "estimated_population_2019", "confirmed_per_100k_inhabitants", "death_rate"))
Continentes <- read_delim('auxiliar/countryContinent.csv', col_types = cols(), delim = ";")
UFs <- read_delim('auxiliar/estadosUF.csv', col_types = cols(), delim = ";")

#pre-processing
BR_Estado %>% 
  filter(date %in% c(BR_Estado %>% select(date) %>% distinct() %>% head(1), BR_Estado %>% select(date) %>% distinct() %>% head(1) - 1)) %>% 
  pivot_wider(names_from = date, values_from = c(confirmed, deaths)) %>% 
  transmute("Province.State" = state, "Country.Region" = "Brazil", "casosNovos" = .[[4]] - .[[5]], "obitosNovos" = .[[6]] - .[[7]]) -> BR_Estado_Hoje

WRL_Pais %>% 
  filter(data %in% c(WRL_Pais %>% select(data) %>% distinct() %>% tail(1), WRL_Pais %>% select(data) %>% distinct() %>% tail(1) - 1)) %>% 
  pivot_wider(names_from = data, values_from = c(casosAcumulados, obitosAcumulado, recuperadosAcumulado)) %>% 
  transmute("Country.Region" = Country.Region, "casosNovos" = .[[5]] - .[[4]], "obitosNovos" = .[[7]] - .[[6]], "recuperadosNovos" = .[[9]] - .[[8]]) -> WRL_Pais_Hoje

WRL_Regiao %>% 
  filter(data %in% c(WRL_Regiao %>% select(data) %>% distinct() %>% tail(1), WRL_Regiao %>% select(data) %>% distinct() %>% tail(1) - 1)) %>% 
  pivot_wider(names_from = data, values_from = c(casosAcumulados, obitosAcumulado, recuperadosAcumulado)) %>% 
  transmute("Province.State" = Province.State, "Country.Region" = Country.Region, "casosNovos" = .[[6]] - .[[5]], "obitosNovos" = .[[8]] - .[[7]], "recuperadosNovos" = .[[10]] - .[[9]]) -> WRL_Regiao_Hoje

#Listas
escolhasUF <- setNames(UFs$UF, UFs$Estado)
escolhas2 <- setNames(UFs$UF, UFs$UF)
paises <- setNames(unique(WRL_Pais$Country.Region), unique(WRL_Pais$Country.Region))

#Shiny
ui <- dashboardPage(
  dashboardHeader(title = "Corona Vírus"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "global", icon = icon("globe")),
      menuItem("Brasil", tabName = "bras", icon = icon("line-chart"),
       menuSubItem("Situação", tabName = "br", icon = icon("line-chart")),
       menuSubItem("Brasil (Por Estado)", tabName = "estado", icon = icon("line-chart")),
       menuSubItem("Brasil (Por Cidade) - Em breve", tabName = "cidade", icon = icon("line-chart")),
       menuSubItem("Mapas", tabName = "mapBR", icon = icon("map"))),
      menuItem("Mundo",icon = icon("line-chart"),
        menuItem("Por Continente", tabName = "cont", icon = icon("line-chart")),
        menuItem("Por País", tabName = "paises", icon = icon("line-chart")),
        menuItem("Mapas", tabName = "mapWRL", icon = icon("map")))
      #,menuItem("Modelos Preditivos - Em breve", tabName = "pred", icon = icon("desktop"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("global",
              fluidRow(
                box(width = 12,
                    footer = paste("Última atualização em:", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S")),
                    status = "info",
                    h2("COVID-19 Dashboard")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Números do Corona Vírus no Mundo:",
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    
                    valueBoxOutput("TotalCasos", width = 3),
                    valueBoxOutput("TotalMortes", width = 3),
                    valueBoxOutput("PMaisCasos", width = 3),
                    valueBoxOutput("PMaisMortes", width = 3)
                    
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Números do Corona Vírus Hoje:",
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    
                    valueBoxOutput("NCasos", width = 3),
                    valueBoxOutput("NMortes", width = 3),
                    valueBoxOutput("MaisCasos", width = 3),
                    valueBoxOutput("MaisMortos", width = 3)
                    
                )
              ),
              
              fixedRow(
                tabBox(
                  title = "Gráficos",
                  id = "grafTab",
                  tabPanel("Evolução", 
                           plotlyOutput("Evolucao")),
                  tabPanel("Evolução (Log10)", 
                           plotlyOutput("EvoLog"))
                ),
                tabBox(
                  title = "Ranking",
                  id = "rankingtab",
                  tabPanel("Global",
                           DTOutput("tabGlobal")
                  ),
                  tabPanel("Brasil",
                           DTOutput("tabBrasil")
                  )
                )
              )
      ),
      
      tabItem("br",
                fluidRow(
                  box(width = 12,
                      title = "Números no Brasil",status = "danger", solidHeader = T,
                      valueBoxOutput("BRCasos", width = 3),
                      valueBoxOutput("BRMortes", width = 3),
                      valueBoxOutput("NBRCasos", width = 3),
                      valueBoxOutput("NBRMortes", width = 3)
                  )
                ),
              column(6,
                     fluidRow(
                         tabBox(id = 'grafsBR',title = "Gráficos",width = 12,height = 400,
                          tabPanel("Escala Normal",
                            plotlyOutput("plot", height = 380)
                            ),
                          tabPanel("Escala Logarítimica",
                            plotlyOutput("plotLog", height = 380)
                            )
                          )
                         )
                       ),
              column(6,
                     fluidRow(
                        DTOutput("BRDash",height = 380)
                     )
                      )
                     ),

      tabItem("estado",
              box(
                title = "Estado", status = "success", solidHeader = TRUE, width = 600,  
                fluidPage(
                  pickerInput(
                    inputId = "UF", 
                    label = "", 
                    choices = escolhasUF, 
                    options = list(
                      `actions-box` = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 3"
                    ), 
                    multiple = FALSE
                  )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         title = "Tipo de dados",status = "warning", solidHeader = T, width = 12,
                         "Selecione os tipos de dados:",
                         checkboxGroupInput("StatusUF", label = "", 
                                            choiceNames = list("Casos", "Mortes"),
                                            choiceValues = list("confirmed", "deaths"),
                                            selected = "confirmed")
                       )
                ),
                column(6,
                       valueBoxOutput("Mortos", width = 6),
                       valueBoxOutput("CConf", width = 6))
              ),
              fluidPage(
                box(
                  title = "Gráfico", status = "info", solidHeader = TRUE, width = 600, 
                  column(12,
                     fluidRow(
                       tabBox(id = 'grafsBR',width = 12,height = 400,
                              tabPanel("Escala Normal",
                                       plotlyOutput("plotUF", height = 380)
                              ),
                              tabPanel("Escala Logarítimica",
                                       plotlyOutput("plotUFLog", height = 380)
                              )
                       )
                     )
                  )
                )
              )
      ),
      tabItem("cont",
              box(
                title = "Continente", status = "success", solidHeader = TRUE, width = 600,  
                fluidPage(
                  selectInput("Cont", "", choices = list("Americas", "Asia", "Europe", "Africa", "Oceania"), selected = "Americas")
                )
              ),
              fluidRow(
                column(4,
                       box(
                         title = "Tipo de dados",status = "warning", solidHeader = T, width = 12,
                         "Selecione os tipos de dados:",
                         checkboxGroupInput("StatusConf", label = "", 
                                            choiceNames = list("Casos", "Mortes", "Recuperados"),
                                            choiceValues = list("Casos", "Mortes", "Recuperados"),
                                            selected = "Casos")
                       )
                ),
                column(8,
                       valueBoxOutput("ContCasos", width = 4),
                       valueBoxOutput("ContMortos", width = 4),
                       valueBoxOutput("ContRecuperados", width = 4)
                )
              ),
              fluidPage(
                box(
                  title = "Gráfico", status = "info", solidHeader = TRUE, width = 600, 
                  column(12,
                         tabBox(width = 12,
                                tabPanel("Escala Convencional",
                           plotlyOutput("plotCont", height = 380)
                                ),
                           tabPanel("Log 10",
                                    plotlyOutput("plotContLog", height = 380)
                           )
                         )
                  )
                )
              )
      ),
      tabItem("paises",
              box(
                title = "País", status = "success", solidHeader = TRUE, width = 600,  
                fluidPage(
                  pickerInput(
                    inputId = "Pais", 
                    label = "", 
                    choices = paises, 
                    options = list(
                      `actions-box` = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 3"
                    ), 
                    multiple = TRUE,
                    selected = "Brazil"
                  )
                )
              ),
              fluidRow(
                column(4,
                       box(
                         title = "Tipo de dados",status = "warning", solidHeader = T, width = 12,
                         "Selecione os tipos de dados:",
                         radioButtons("StatusPais", label = "", 
                                      choiceNames = list("Casos", "Mortes", "Recuperados"),
                                      choiceValues = list("Casos", "Mortes", "Recuperados"),
                                      selected = "Casos")
                       )
                ),
                column(8,
                       valueBoxOutput("CasosPais", width = 4),
                       valueBoxOutput("MortosPais", width = 4),
                       valueBoxOutput("RecuperadosPais", width = 4)
                )
              ),
              fluidPage(
                box(
                  title = "Gráfico", status = "info", solidHeader = TRUE, width = 600, 
                  column(12,
                         tabBox(width = 12,
                           tabPanel("Escala Convencional", plotlyOutput("plotPais", height = 380)),
                           tabPanel("Log 10", plotlyOutput("plotPaisLog", height = 380))
                         )
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) { 
  
  output$NCasos <- renderValueBox({
    valueBox(paste("+", WRL_Pais_Hoje %>% 
                     select(casosNovos) %>%
                     sum(na.rm = T)
    ),
    "Casos",
    color = "yellow", icon = icon("medkit")
    )
  })
  
  output$NMortes <- renderValueBox({
    valueBox(paste("+", WRL_Pais_Hoje %>% 
                     select(obitosNovos) %>% 
                     sum(na.rm = T)
    ),
    "Mortes", icon = icon("heartbeat"),
    color = "red"
    
    )
  })
  
  output$MaisMortos <- renderValueBox({
    valueBox(value = WRL_Pais_Hoje %>% 
               group_by(Country.Region) %>% 
               summarise(casos = max(casosNovos, na.rm = T), mortes = max(obitosNovos, na.rm = T))  %>% 
               arrange(desc(mortes)) %>% 
               select(mortes) %>% 
               head(1),
             subtitle = paste0("País com mais Mortos Hoje (",
                               WRL_Pais_Hoje %>% 
                                 group_by(Country.Region) %>% 
                                 summarise(casos = max(casosNovos, na.rm = T), mortes = max(obitosNovos, na.rm = T))  %>% 
                                 arrange(desc(mortes)) %>% 
                                 select(Country.Region) %>% 
                                 head(1), ")"),
             color = "red", icon = icon("heartbeat")
    )
  })
  
  output$MaisCasos <- renderValueBox({
    valueBox(value = WRL_Pais_Hoje %>% 
               group_by(Country.Region) %>% 
               summarise(casos = max(casosNovos, na.rm = T), mortes = max(obitosNovos, na.rm = T))  %>% 
               arrange(desc(casos)) %>% 
               select(mortes) %>% 
               head(1),
             subtitle = paste0("País com mais Mortos Hoje (",
                               WRL_Pais %>% 
                                 group_by(Country.Region) %>% 
                                 summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T))  %>% 
                                 arrange(desc(mortes)) %>% 
                                 select(Country.Region) %>% 
                                 head(1), ")"),
             color = "yellow", icon = icon("medkit"))
  })
  
  output$TotalCasos <- renderValueBox({
    valueBox(WRL_Pais %>%
               group_by(Country.Region) %>% 
               summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T)) %>% 
               ungroup() %>%
               select(casos) %>% 
               sum(na.rm = T),
             "Total de Casos",
             color = "navy", icon = icon("medkit"))
  })
  
  output$TotalMortes <- renderValueBox({
    valueBox(WRL_Pais %>%
               group_by(Country.Region) %>% 
               summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T)) %>% 
               ungroup() %>%
               select(mortes) %>% 
               sum(na.rm = T),
             "Total de Mortes", icon = icon("heartbeat"),
             color = "blue"
             
    )
  })
  
  output$PMaisMortes <- renderValueBox({
    valueBox(value = WRL_Pais %>%
               group_by(Country.Region) %>% 
               summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T)) %>% 
               ungroup() %>% 
               arrange(desc(mortes)) %>% 
               head(1) %>% 
               select(mortes),
             subtitle = paste0("País com mais Mortos (",
                               WRL_Pais %>%
                                 group_by(Country.Region) %>% 
                                 summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T)) %>% 
                                 ungroup() %>% 
                                 arrange(desc(mortes)) %>% 
                                 head(1) %>% 
                                 select(Country.Region), ")"),
             color = "purple", icon = icon("heartbeat")
    )
  })
  
  output$PMaisCasos <- renderValueBox({
    valueBox(value = WRL_Pais %>%
               group_by(Country.Region) %>% 
               summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T)) %>% 
               ungroup() %>% 
               arrange(desc(casos)) %>% 
               head(1) %>% 
               select(casos),
             subtitle = paste0("País com mais Casos (",
                               WRL_Pais %>%
                                 group_by(Country.Region) %>% 
                                 summarise(casos = max(casosAcumulados, na.rm = T), mortes = max(obitosAcumulado, na.rm = T)) %>% 
                                 ungroup() %>% 
                                 arrange(desc(casos)) %>% 
                                 head(1) %>% 
                                 select(Country.Region), ")"),
             color = "yellow", icon = icon("medkit"))
  })
  
  output$tabGlobal <- renderDT(options = list(pageLength = 10, lengthMenu = ''),{
    suppressWarnings(WRL_Pais %>% 
      group_by(Country.Region) %>% 
      summarize(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
      ungroup())
  })
  
  output$tabBrasil <- renderDT(options = list(pageLength = 10, lengthMenu = ''),{
    BR_Estado %>% 
      left_join(UFs, by = c("state" = "UF")) %>% 
      group_by(Estado) %>% 
      summarize(Casos = max(confirmed, na.rm = T), Mortes =  max(deaths, na.rm = T))
  })
  
  output$Evolucao <- renderPlotly({    
    WRL_Pais %>% 
      group_by(data) %>%
      summarise(Casos = sum(casosAcumulados, na.rm = T), Mortes = sum(obitosAcumulado, na.rm = T),Recuperados = sum(recuperadosAcumulado, na.rm = T)) %>%
      pivot_longer(-data,names_to = "status", values_to = "val") %>% 
      ggplot(aes(x = data, y = val, color = status)) +
      geom_point() + 
      geom_line() +
      theme_bw() +
      ylab("Número") +
      xlab("Data") +
      scale_x_date(date_breaks = "3 days") +
      labs(title = paste("Evolução do COVID-19")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> Evo
    
    ggplotly(Evo, tooltip = c("data", "val"), dynamicTicks = TRUE) %>%
      layout(hoverlabel = "x",
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = TRUE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: John Hopkins CSSE"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  })
  
  output$EvoLog <- renderPlotly({    
    WRL_Pais %>% 
      group_by(data) %>%
      summarise(Casos = sum(casosAcumulados, na.rm = T), Mortes = sum(obitosAcumulado, na.rm = T),Recuperados = sum(recuperadosAcumulado, na.rm = T)) %>%
      pivot_longer(-data,names_to = "status", values_to = "val") %>% 
      ggplot(aes(x = data, y = val, color = status)) +
      geom_point() + 
      geom_line() +
      theme_bw() +
      ylab("Número") +
      xlab("Data") +
      scale_y_log10() +
      scale_x_date(date_breaks = "3 days") +
      labs(title = paste("Evolução do COVID-19")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> EvoLog
    
    ggplotly(EvoLog, tooltip = c("data", "val"), dynamicTicks = TRUE) %>%
      layout(hoverlabel = "x",
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = TRUE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: John Hopkins CSSE"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black"))) })
  
  output$plot <- renderPlotly({
    
    BR_Estado %>% 
      group_by(date) %>% 
      summarise(confirmed = sum(confirmed, na.rm = T), deaths = sum(deaths, na.rm = T)) %>% 
      pivot_longer(-date, names_to = "status", values_to = "val") %>% 
      ggplot(aes(x = date, y = val, color = status, pch = status)) + 
      geom_point() + 
      theme_bw() +
      geom_line() +
      ylim(0,10000) +
      xlab("Data") +
      ylab("Número") +
      labs(title = "Casos e Confirmados") +
      scale_x_date(date_breaks = "3 days") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> pTY
    
    ggplotly(pTY, tooltip = c("date", "val"), dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x", 
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = FALSE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: Secretarias de Saúde (brasil.io)"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  }) 
  
  output$plotLog <- renderPlotly({
    
    BR_Estado %>% 
      group_by(date) %>% 
      summarise(confirmed = sum(confirmed, na.rm = T), deaths = sum(deaths, na.rm = T)) %>% 
      pivot_longer(-date, names_to = "status", values_to = "val") %>% 
      ggplot(aes(x = date, y = val, color = status, pch = status)) + 
      geom_point() + 
      theme_bw() + 
      scale_y_log10() +
      geom_line() +
      ylab("Log10") +
      xlab("Data") +
      labs(title = paste("Casos e Confirmados")) +
      scale_x_date(date_breaks = "3 days") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> pLogTY
    
    ggplotly(pLogTY, tooltip = c("date", "val"), dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x", 
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = TRUE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: Secretarias de Saúde (brasil.io)"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  }) 
  
  output$BRCasos <- renderValueBox({
    valueBox(BR_Estado %>%
               group_by(date) %>% 
               summarise(confirmed = sum(confirmed, na.rm = T), deaths = sum(deaths, na.rm = T)) %>% 
               pivot_longer(-date, names_to = "status", values_to = "val") %>% 
               group_by(status) %>% 
               summarise(val = max(val, na.rm = T)) %>% 
               filter(status == "confirmed") %>% 
               ungroup() %>% 
               select(val),
             "Casos",
             color = "purple", icon = icon("medkit")
    )
  })
  
  output$BRMortes <- renderValueBox({
    valueBox(BR_Estado %>%
               group_by(date) %>% 
               summarise(confirmed = sum(confirmed, na.rm = T), deaths = sum(deaths, na.rm = T)) %>% 
               pivot_longer(-date, names_to = "status", values_to = "val") %>% 
               group_by(status) %>% 
               summarise(val = max(val, na.rm = T)) %>% 
               filter(status == "deaths") %>% 
               ungroup() %>% 
               select(val),
             "Mortes",
             color = "blue", icon = icon("heartbeat")
    )
  })
  
  output$NBRCasos <- renderValueBox({
    valueBox(value = paste0(" + ",
                            BR_Estado_Hoje %>%
                              select(casosNovos) %>% 
                              sum(na.rm = T)),
             "Casos Hoje",
             color = "yellow", icon = icon("medkit")
    )
  })
  
  output$NBRMortes <- renderValueBox({
    valueBox(value = paste0(" + ", 
                            BR_Estado_Hoje %>%
                              select(obitosNovos) %>% 
                              sum(na.rm = T)),
             "Mortes Hoje",
             color = "navy", icon = icon("heartbeat")
    )
  })
  
  output$BRDash <- renderDT(options = list(pageLength = 9, lengthMenu = ''),{
    BR_Estado %>% 
      left_join(UFs, by = c("state" = "UF")) %>% 
      group_by(Estado) %>% 
      summarise(confirmed = max(confirmed, na.rm = T), deaths = max(deaths, na.rm = T))
  })
  
  output$plotUF <- renderPlotly({
    BR_Estado %>% 
      group_by(state,date) %>% 
      summarise("confirmed" = confirmed, "deaths" = deaths) %>% 
      pivot_longer(c(-state,-date),names_to = "status",values_to = "val") %>% 
      filter(state == input$UF, status %in% input$StatusUF) %>% 
      ggplot(aes(x = date, y = val, color = state, pch = status)) + 
      geom_point() + 
      geom_line() +
      theme_bw() +
      ylab("Número") +
      ylim(0,5000) +
      labs(title = paste("Quantidade de Casos (Por Tipo) -", input$UF)) +
      scale_x_date(date_breaks = "3 days") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> UFTY
    
    ggplotly(UFTY, tooltip = c("date", "val"), dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x", 
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = FALSE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: Secretarias de Saúde (brasil.io)"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  })
  
  output$plotUFLog <- renderPlotly({
    BR_Estado %>% 
      group_by(state,date) %>% 
      summarise("confirmed" = confirmed, "deaths" = deaths) %>% 
      pivot_longer(c(-state,-date),names_to = "status",values_to = "val") %>% 
      filter(state == input$UF, status %in% input$StatusUF) %>% 
      ggplot(aes(x = date, y = val, color = state, pch = status)) +
      geom_point() + 
      geom_line() +
      ylab("Log10") +
      scale_y_log10() +
      labs(title = paste("Quantidade de Casos (Por Tipo) -", input$UF)) +
      scale_x_date(date_breaks = "3 days") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> LogUFTY
    
    ggplotly(LogUFTY, tooltip = c("date", "val"), dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x", 
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = TRUE),
             annotations = list(x = 1, y = -0.35, 
                                text =  paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: Secretarias de Saúde (brasil.io)"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  })
  
  output$CConf <- renderValueBox({
    valueBox(subtitle = "Casos Confirmados",
            value = BR_Estado %>% 
              group_by(state) %>% 
              summarize(confirmed = max(confirmed, na.rm = T), deaths = max(deaths, na.rm = T)) %>%
              filter(state == input$UF) %>% 
              select(confirmed),
            color = "blue",
            icon = icon("medkit"))
  })
  
  output$Mortos <- renderValueBox({
    valueBox(subtitle = "Mortos",
            value = BR_Estado %>% 
              group_by(state) %>% 
              summarize(confirmed = max(confirmed, na.rm = T), deaths = max(deaths, na.rm = T)) %>%
              filter(state == input$UF) %>% 
              select(deaths),
            color = "red",
            icon = icon("heartbeat"))
  })
  
  output$plotCont <- renderPlotly({
    WRL_Pais %>%
      full_join(Continentes, by = c("Country.Region" = "Pais")) %>%
      group_by(Continente, data) %>% 
      summarise(Casos = sum(casosAcumulados, na.rm = T), Mortes = sum(obitosAcumulado, na.rm = T), Recuperados = sum(recuperadosAcumulado, na.rm = T)) %>% 
      pivot_longer(c(-Continente,-data),names_to = "status", values_to = "val") %>% 
      filter(Continente == input$Cont, status %in% input$StatusConf) %>% 
      ggplot(aes(x = data, y = val, color = status, pch = status)) + 
      geom_point() + 
      geom_line() +
      theme_bw() +
      ylim(0,5e5) +
      ylab("Número") +
      xlab("Data") +
      scale_x_date(date_breaks = "3 days") +
      labs(title = paste("Número de", input$status)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> ContTY
    
    ggplotly(ContTY, tooltip = c("data", "val"), dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x", 
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = FALSE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: John Hopkins CSSE"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  })
  
  output$plotContLog <- renderPlotly({
    WRL_Pais %>%
      full_join(Continentes, by = c("Country.Region" = "Pais")) %>%
      group_by(Continente, data) %>% 
      summarise(Casos = sum(casosAcumulados, na.rm = T), Mortes = sum(obitosAcumulado, na.rm = T), Recuperados = sum(recuperadosAcumulado, na.rm = T)) %>% 
      pivot_longer(c(-Continente,-data),names_to = "status", values_to = "val") %>% 
      filter(Continente == input$Cont, status %in% input$StatusConf) %>% 
      ggplot(aes(x = data, y = val, color = status, pch = status)) + 
      geom_point() + 
      geom_line() +
      theme_bw() +
      scale_y_log10() +
      ylab("Número") +
      xlab("Data") +
      scale_x_date(date_breaks = "3 days") +
      labs(title = paste("Número de", input$status)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> ContLogTY
    
    ggplotly(ContLogTY, tooltip = c("data", "val"), dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x", 
             legend = list(x = 0.1, y = 0.9), 
             yaxis = list(autorange = TRUE),
             annotations = list(x = 1, y = -0.35, 
                                text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: John Hopkins CSSE"),
                                showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
  })
  
  output$ContCasos <- renderValueBox({
    valueBox(subtitle = "Total de Casos Confirmados",
            value = WRL_Pais %>%
              full_join(Continentes, by = c("Country.Region" = "Pais")) %>%
              group_by(Country.Region, Continente) %>% 
              summarise(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                        Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                        Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
              group_by(Continente) %>% 
              summarise(Casos = sum(Casos), Mortes = sum(Mortes), Recuperados = sum(Recuperados)) %>% 
              filter(Continente == input$Cont) %>% 
              select(Casos) %>% 
              sum(),
            color = "purple",
            icon = icon("medkit"))
  })
  
  output$ContRecuperados <- renderValueBox({
    valueBox(subtitle = "Total de Casos Recuperados",
            value = WRL_Pais %>%
              full_join(Continentes, by = c("Country.Region" = "Pais")) %>%
              group_by(Country.Region, Continente) %>% 
              summarise(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                        Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                        Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
              group_by(Continente) %>% 
              summarise(Casos = sum(Casos), Mortes = sum(Mortes), Recuperados = sum(Recuperados)) %>% 
              filter(Continente == input$Cont) %>% 
              select(Recuperados) %>% 
              sum(),
            color = "blue",
            icon = icon("heart"))
  })
  
  output$ContMortos <- renderValueBox({
    valueBox(subtitle = "Total de Mortos",
            value = WRL_Pais %>%
              full_join(Continentes, by = c("Country.Region" = "Pais")) %>%
              group_by(Country.Region, Continente) %>% 
              summarise(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                        Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                        Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
              group_by(Continente) %>% 
              summarise(Casos = sum(Casos), Mortes = sum(Mortes), Recuperados = sum(Recuperados)) %>% 
              filter(Continente == input$Cont) %>% 
              select(Mortes) %>% 
              sum(),
            color = "red",
            icon = icon("heartbeat"))
  })
  
  output$CasosPais <- renderValueBox({
    valueBox(subtitle = "Total de Casos Confirmados",
            value = WRL_Pais %>% 
              filter(Country.Region %in% input$Pais) %>% 
              group_by(Country.Region) %>% 
              summarise(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                        Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                        Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
              select(Casos) %>% 
              sum(na.rm = T),
            color = "purple",
            icon = icon("medkit"))
  })
  
  output$RecuperadosPais <- renderValueBox({
    valueBox(subtitle = "Total de Recuperados",
             value = WRL_Pais %>% 
               filter(Country.Region %in% input$Pais) %>% 
               group_by(Country.Region) %>% 
               summarise(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                         Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                         Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
               select(Recuperados) %>% 
               sum(na.rm = T),
            color = "blue",
            icon = icon("heart"))
  })
  
  output$MortosPais <- renderValueBox({
    valueBox("Total de Mortos",
             value = WRL_Pais %>% 
               filter(Country.Region %in% input$Pais) %>% 
               group_by(Country.Region) %>% 
               summarise(Casos = if(max(casosAcumulados, na.rm = T) >= 0){max(casosAcumulados, na.rm = T)}else{0}, 
                         Mortes = if(max(obitosAcumulado, na.rm = T) >= 0){max(obitosAcumulado, na.rm = T)}else{0},
                         Recuperados = if(max(recuperadosAcumulado, na.rm = T) >= 0){max(recuperadosAcumulado, na.rm = T)}else{0}) %>% 
               select(Mortes) %>% 
               sum(na.rm = T),
            color = "red",
            icon = icon("heartbeat"))
    
  })
    
    output$plotPais <- renderPlotly({
      WRL_Pais %>% 
        transmute("data" = data, "Country.Region" = Country.Region, "Casos" = casosAcumulados, "Mortes" = obitosAcumulado, "Recuperados" = recuperadosAcumulado) %>% 
        pivot_longer(c(-data,-Country.Region), names_to = "status", values_to = "val") %>% 
        filter(Country.Region %in% input$Pais, status == input$StatusPais) %>%
        ggplot(aes(x = data, y = val, color = Country.Region)) + 
        geom_point() + 
        geom_line() +
        theme_bw() +
        ylab("Número") +
        xlab("Data") +
        scale_x_date(date_breaks = "3 days") +
        labs(title = paste("Número de", input$StatusPais)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> PaisTY
      
      ggplotly(PaisTY, tooltip = c("data", "val"), dynamicTicks = TRUE) %>%
        rangeslider() %>%
        layout(hovermode = "x", 
               legend = list(x = 0.1, y = 0.9),
               yaxis = list(autorange = TRUE),
               annotations = list(x = 1, y = -0.35, 
                                  text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: John Hopkins CSSE"),
                                  showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                  yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
    })
    
    output$plotPaisLog <- renderPlotly({
      WRL_Pais %>% 
        transmute("data" = data, "Country.Region" = Country.Region, "Casos" = casosAcumulados, "Mortes" = obitosAcumulado, "Recuperados" = recuperadosAcumulado) %>% 
        pivot_longer(c(-data,-Country.Region), names_to = "status", values_to = "val") %>% 
        filter(Country.Region %in% input$Pais, status == input$StatusPais) %>%
        ggplot(aes(x = data, y = val, color = Country.Region)) + 
        geom_point() + 
        geom_line() +
        theme_bw() +
        scale_y_log10() +
        ylab("Número") +
        xlab("Data") +
        scale_x_date(date_breaks = "3 days") +
        labs(title = paste("Número de", input$StatusPais)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> PaisLogTY
      
      ggplotly(PaisLogTY, tooltip = c("data", "val"), dynamicTicks = TRUE) %>%
        rangeslider() %>%
        layout(hovermode = "x", 
               legend = list(x = 0.1, y = 0.9),
               yaxis = list(autorange = TRUE),
               annotations = list(x = 1, y = -0.35, 
                                  text = paste("Atualizado em", format(now("Brazil/East"), "%d/%m/%Y - %H:%M:%S"), "-", "Fonte: John Hopkins CSSE"),
                                  showarrow = F, xref='paper', yref='paper', xanchor='right', 
                                  yanchor='auto', xshift=0, yshift=0, font=list(size=8, color="black")))
    })
    
}

shinyApp(ui, server)
  
