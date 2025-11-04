library(bs4Dash)
library(shiny)
library(ggplot2)
library(fresh)
library(plotly)
library(DT)

tema <- create_theme(
  bs4dash_status(
    "info" = "#004699",
    "primary" = "#E1EDED"
  ),
  bs4dash_color(
    "olive" = "#004699"
  )
)

dados <- diamonds

ui <- dashboardPage(
  freshTheme = tema,
  scrollToTop = T,
  fullscreen = T,
  dark = NULL,
  help = NULL,
  title = "PET - Estatística",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "PET - Estatística",
      image = "https://pet.leg.ufpr.br/images/logo-pet.png"
    )
  ),
  sidebar = dashboardSidebar(
    status = "info",
    sidebarMenu(
      menuItem(
        "Início",
        tabName = "inicio",
        icon = icon("home")
      ),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("bar-chart")
      )
    )
  ),
  footer = dashboardFooter(
    left = "PET - Estatística",
    right = "2025"
  ),
  body = dashboardBody(
    tags$head(
      tags$link(
        rel = "icon", href = "https://pet.leg.ufpr.br/images/logo-pet.png"
      )
    ),
    tabItems(
      tabItem(
        tabName = "inicio",
        
        jumbotron(
          title = "Tutorial para Criação de Dashboards",
          status = "primary",
          lead = "Utilizando o pacote bs4dash",
          btnName = "Site - PET",
          href = "https://pet.leg.ufpr.br/",
          "Clique no link abaixo para acessar o site:"
          
        ),
        
        fluidRow(
          userBox(
            collapsible = TRUE,
            status = "info",
            title = userDescription(
              title = "PET - Estatística",
              subtitle = "UFPR",
              image = "https://pet.leg.ufpr.br/images/logo-pet.png",
              type = 1
            ),
            br(),
            
            "O PET Estatística UFPR é o Programa de Educação Tutorial do curso de Estatística
da Universidade Federal do Paraná, um programa de longa duração que visa integrar ensino, 
pesquisa e extensão, oferecendo aos alunos oportunidades de aprofundar seus conhecimentos 
e participar de projetos."
          ),
          box(
            collapsible = T,
            title = "Citação",
            blockQuote(
              "Frase qualquer",
              color = "olive"
            )
          )
        )
      ),
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Total de diamantes observados",
              value = format(nrow(dados), big.mark = ".", decimal.mark = ","),
              icon = icon("gem"),
              color = "primary"
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Peso médio dos diamantes",
              value = paste0(format(round(mean(dados$carat), 2), big.mark = ".", decimal.mark = ","), " (em quilates)"),
              icon = icon("weight-scale"),
              color = "primary"
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Preço médio dos diamantes",
              value = paste0("U$",format(round(mean(dados$price), 2), big.mark = ".", decimal.mark = ",")),
              icon = icon("dollar"),
              color = "primary"
            )
          )
        ),
        fluidRow(
          sortable(
            width = 6,
            
            box(
              collapsible = T,
              width = 12,
              title = "Gráfico de Dispersão",
              plotOutput("dispersao")
              
            ),
            box(
              ribbon(
                text = "NOVO",
                color = "info"
              ),
              width = 12,
              title = "Histograma",
              plotlyOutput("histograma")
            )
          ),
          sortable(
            width = 6,
            box(
              maximizable = T,
              width = 12,
              title = "Boxplot",
              plotlyOutput("boxplot")
            ),
            box(
              elevation = 3,
              width = 12,
              title = "Gráfico de Barras",
              plotlyOutput("barras")
            )
          )
        ),
        fluidRow(
          sortable(
            width = 12,
            box(
              width = 12,
              title = "Tabela",
              DTOutput("tabela_resumo")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output){
  output$dispersao <- renderPlot(
    ggplot(dados, aes(x = carat, y = price, color = clarity)) +
      geom_point(alpha = 0.5) +
      xlab("Quilate (Carat)") +
      ylab("Preço (USD)")+
      theme_minimal()
  )
  
  output$histograma <- renderPlotly({
    p_hist <- ggplot(dados, aes(x = price)) +
      geom_histogram(binwidth = 500, fill = "navy", color = "white") +
      xlab("Preço (USD)") +
      ylab("Contagem")+
      theme_minimal()
    
    ggplotly(p_hist)
  })
  
  
  output$barras <- renderPlotly({
    p_bar <- ggplot(dados, aes(x = cut)) +
      geom_bar(fill = "#004699") +
      xlab("Qualidade do Corte") +
      ylab("Contagem")+
      theme_minimal()
    
    ggplotly(p_bar)
  })
  
  output$boxplot <- renderPlotly({
    p_box <- ggplot(dados, aes(x = cut, y = price, fill = cut)) +
      geom_boxplot() +
      xlab("Qualidade do Corte") +
      ylab("Preço (USD)")+
      theme_minimal()
    
    ggplotly(p_box) %>%  hide_legend()
    
  })
  
  output$tabela_resumo <- DT::renderDataTable({
    
    DT::datatable(
      diamonds, 
      options = list(
        pageLength = 15, 
        
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')
      ),
      rownames = T
    )
    
  })
}

shinyApp(ui, server)