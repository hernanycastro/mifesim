library(shiny)
library(mifebpc)
library(ggplot2)
library(tibble)
library(dplyr)

# Função para formatar números no padrão brasileiro, ignorando colunas de datas/anos/identificadores
fmt_br <- function(x, nome_coluna = "") {
  colunas_ignoradas <- c("Ano", "ano", "Data", "data", "mes_index", "competencia", "mes", "AnoMes", "periodo")
  if (nome_coluna %in% colunas_ignoradas) {
    if (is.numeric(x) && x > 1e8) {
      return(format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m"))
    }
    if (is.numeric(x) && x >= 1990 && x <= 2100) {
      return(as.character(as.integer(x)))
    }
    return(as.character(x))
  }
  
  if (is.numeric(x)) {
    format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE)
  } else {
    x
  }
}

# 1. Definição da Interface do Utilizador (UI)
ui <- fluidPage(
  titlePanel("Simulador Atuarial e Operacional do BPC (mifebpc v0.1.0)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Configure os parâmetros de projeção integrados ao motor oficial do BPC."),
      
      sliderInput("horizonte", "Horizonte de Projeção (Meses):", 
                  min = 12L, max = 84L, value = 60L, step = 12L),
      
      selectInput("especie_filtro", "Espécie para Auditoria Prévia:",
                  choices = c("PCD (87)" = "87", "Idoso (88)" = "88"),
                  selected = "87"),
      
      actionButton("executar", "Executar Simulação Completa", class = "btn-primary", style = "width: 100%;"),
      hr(),
      tags$ul(
        tags$li("Fase 0: Auditoria prévia de histórico, parâmetros e valores executados."),
        tags$li("Fase 1: Obtenção dinâmica de indicadores (IPCA, desemprego, informalidade)."),
        tags$li("Fase 2-3: Projeção de fila por Random Forest."),
        tags$li("Fase 4-6: Motor atuarial (matriz benefício-mês) e decomposição financeira.")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Evolução do Estoque Ativo", 
                 br(),
                 plotOutput("plotEstoque", height = "480px")
        ),
        tabPanel("Impactos e Decomposição Financeira", 
                 br(),
                 h4("Tabela de Variações e Impactos Anuais"),
                 tableOutput("tabelaImpactosVar"),
                 br(),
                 h4("Resumo Mensal Consolidado (Horizonte Completo)"),
                 dataTableOutput("tabelaResumoMensal")
        ),
        tabPanel("Auditoria de Fila (Fase 0)", 
                 br(),
                 h4("Histórico de Fila Processada"),
                 dataTableOutput("tabelaHistFila")
        ),
        tabPanel("Valores Executados", 
                 br(),
                 h4("Valores Financeiros Executados Históricos (a partir de 2026)"),
                 dataTableOutput("tabelaExecutados")
        )
      )
    )
  )
)

# 2. Lógica do Servidor (Server)
server <- function(input, output, session) {
  
  simulacao_reativa <- eventReactive(input$executar, {
    req(input$horizonte)
    
    withProgress(message = 'A executar pipeline do mifebpc...', value = 0, {
      
      incProgress(0.1, detail = "A consultar histórico de fila e executados...")
      df_hist_fila <- tryCatch({
        consultar_historico_fila(especie = input$especie_filtro, data_inicio = "2024-01-01")
      }, error = function(e) { NULL })
      
      df_executados <- tryCatch({
        consultar_valores_executados(data_inicio = "2026-01-01")
      }, error = function(e) { NULL })
      
      incProgress(0.3, detail = "A recolher indicadores macroeconómicos...")
      dt_macro <- obter_dados_macro()
      
      incProgress(0.5, detail = "A projetar fila de requerimentos (Random Forest)...")
      df_fila <- projetar_fila_bpc(
        dt_macro = dt_macro, 
        horizonte_meses = as.integer(input$horizonte),
        data_inicio_projecao = NULL
      )
      
      incProgress(0.7, detail = "A executar motor atuarial (matriz benefício-mês)...")
      df_atuarial <- executar_motor_atuarial(df_final_engine = df_fila)
      
      incProgress(0.9, detail = "A calcular impactos e decomposição financeira...")
      tabela_sm_vigente <- tibble::tibble(
        Ano = 2026:2033,
        SM = c(1621, 1741, 1854, 1956, 2066, 2180, 2300, 2420)
      )
      
      res_fin <- calcular_impactos_financeiros(
        df_atuarial, 
        tabela_sm = tabela_sm_vigente
      )
      
      list(
        atuarial = df_atuarial, 
        financeiro = res_fin,
        hist_fila = df_hist_fila,
        executados = df_executados
      )
    })
  })
  
  output$plotEstoque <- renderPlot({
    res <- simulacao_reativa()
    req(res)
    
    df_plot <- res$atuarial
    possiveis_colunas_tempo <- c("mes_proj", "mes", "competencia", "data", "AnoMes", "periodo")
    coluna_x <- intersect(possiveis_colunas_tempo, names(df_plot))
    x_var <- if(length(coluna_x) > 0) sym(coluna_x[1]) else sym(names(df_plot)[1])
    
    ggplot(df_plot, aes(x = !!x_var, y = Estoque_Ativo, color = especie, group = especie)) +
      geom_line(linewidth = 1.3) +
      theme_minimal(base_size = 14) +
      labs(title = "Evolução Projetada do Estoque Ativo por Espécie",
           subtitle = "Simulação executada via pacote mifebpc (v0.1.0)",
           x = "Período / Mês", y = "Estoque Ativo", color = "Espécie") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  aplicar_form_tabela <- function(df) {
    if (is.list(df) && !is.data.frame(df)) df <- as.data.frame(df)
    for (col_nome in names(df)) {
      df[[col_nome]] <- sapply(df[[col_nome]], function(val) fmt_br(val, nome_coluna = col_nome))
    }
    df
  }
  
  output$tabelaImpactosVar <- renderTable({
    res <- simulacao_reativa()
    req(res)
    aplicar_form_tabela(res$financeiro$impactos_var)
  }, striped = TRUE, bordered = TRUE, spacing = "m", rownames = FALSE, na = "-")
  
  output$tabelaResumoMensal <- renderDataTable({
    res <- simulacao_reativa()
    req(res)
    df_mensal <- res$financeiro$resumo_mensal
    
    col_data <- intersect(c("Data", "data", "mes", "competencia"), names(df_mensal))
    if(length(col_data) > 0) {
      c_nome <- col_data[1]
      if (is.numeric(df_mensal[[c_nome]]) && df_mensal[[c_nome]][1] > 10000) {
        df_mensal[[c_nome]] <- format(as.Date(df_mensal[[c_nome]], origin = "1970-01-01"), "%Y-%m")
      }
    }
    
    aplicar_form_tabela(df_mensal)
  }, options = list(pageLength = 12, lengthMenu = c(12, 24, 60), language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')))
  
  output$tabelaHistFila <- renderDataTable({
    res <- simulacao_reativa()
    req(res)
    df_hf <- res$hist_fila
    if (!is.null(df_hf)) {
      aplicar_form_tabela(df_hf)
    } else {
      data.frame(Mensagem = "Sem dados históricos carregados para esta seleção.")
    }
  }, options = list(pageLength = 12, lengthMenu = c(12, 24, 60), language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')))
  
  output$tabelaExecutados <- renderDataTable({
    res <- simulacao_reativa()
    req(res)
    df_ex <- res$executados
    if (!is.null(df_ex)) {
      aplicar_form_tabela(df_ex)
    } else {
      data.frame(Mensagem = "Sem dados de valores executados disponíveis.")
    }
  }, options = list(pageLength = 12, lengthMenu = c(12, 24, 60), language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')))
}

shinyApp(ui = ui, server = server)