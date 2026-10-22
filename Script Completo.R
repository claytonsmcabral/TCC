# ==============================================================================
# PIPELINE COMPLETO BACI (CEPII) - PROCESSAMENTO, CONCORDÂNCIA E SHINY APP
# ==============================================================================

library(tidyverse)
library(data.table)
library(WDI)
library(scales)
library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(ggplot2)
library(plotly)

# ------------------------------------------------------------------------------
# 1. MAPEAMENTO DE PARÂMETROS E PARCEIROS
# ------------------------------------------------------------------------------

caminho_pasta <- "G:/Meu Drive/Organizar/Meu Mundo/UFPI/TCC/RStudio/BACI_HS07_V202601"
anos_analise  <- 2011:2024

paises_dic <- fread(file.path(caminho_pasta, "country_codes_V202601.csv"))

brics_iso <- c("BRA", "RUS", "IND", "CHN", "ZAF")
codigos_brics <- c(76, 643, 699, 156, 710)
code_bra <- 76
brics_parceiros_codes <- c(643, 699, 156, 710)

# ------------------------------------------------------------------------------
# 2. CARREGAR E AGREGAR EXPORTAÇÕES GLOBAIS (BACI)
# ------------------------------------------------------------------------------

cat("A carregar e consolidar ficheiros do BACI (2011-2024)...\n")

lista_arquivos <- list.files(
  path = caminho_pasta, 
  pattern = "BACI_HS07_Y(201[1-9]|202[0-4])_V202601\\.csv$", 
  full.names = TRUE
)

exportacoes_hs6 <- map_dfr(lista_arquivos, function(file) {
  cat("  -> Processando:", basename(file), "...\n")
  dt <- fread(file, select = c("t", "i", "k", "v"))
  dt_agg <- dt[, .(val_export_k = sum(v, na.rm = TRUE)), by = .(ano = t, pais_code = i, hs6 = k)]
  return(dt_agg)
})

cat("\nCalculando somatórios globais e por país no nível HS6...\n")

totais_pais <- exportacoes_hs6 %>%
  group_by(ano, pais_code) %>%
  summarise(val_pais_total = sum(val_export_k, na.rm = TRUE), .groups = "drop")

totais_mundo_k <- exportacoes_hs6 %>%
  group_by(ano, hs6) %>%
  summarise(val_mundo_k = sum(val_export_k, na.rm = TRUE), .groups = "drop")

totais_mundo_geral <- exportacoes_hs6 %>%
  group_by(ano) %>%
  summarise(val_mundo_total = sum(val_export_k, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# 3. BASE CONSOLIDADA COM IVCR EM HS6
# ------------------------------------------------------------------------------

cat("\nUnindo denominadores e calculando o IVCR em HS6...\n")

base_ivcr_hs6 <- exportacoes_hs6 %>%
  left_join(totais_pais, by = c("ano", "pais_code")) %>%
  left_join(totais_mundo_k, by = c("ano", "hs6")) %>%
  left_join(totais_mundo_geral, by = "ano") %>%
  mutate(
    share_pais = val_export_k / val_pais_total,
    share_mundo = val_mundo_k / val_mundo_total,
    ivcr = share_pais / share_mundo
  )

# ------------------------------------------------------------------------------
# 4. CLASSIFICAÇÃO DA INTENSIDADE TECNOLÓGICA (A PARTIR DA PLANILHA)
# ------------------------------------------------------------------------------

cat("\nCarregando a tabela de intensidade tecnológica dos 5.050 produtos...\n")

# Caminho completo para a planilha de intensidade tecnológica no Google Drive
caminho_intensidade <- "G:/Meu Drive/Organizar/Meu Mundo/UFPI/TCC/RStudio/HS06_2007_5050_produtos_intensidade.csv"

tabela_intensidade_raw <- read.csv2(caminho_intensidade, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Padronização e limpeza da tabela de intensidade
codigos_hs_classificados <- tabela_intensidade_raw %>%
  mutate(
    hs6 = as.integer(gsub("[^0-9]", "", `Código.HS.2007..HS06.`)),
    intensidade_tecnologica = case_when(
      `Intensidade.tecnológica..OCDE...Morceiro..2019.` %in% c("baixa", "Baixa") ~ "Baixa / Produtos Primários",
      `Intensidade.tecnológica..OCDE...Morceiro..2019.` %in% c("Média-Baixa", "Média-baixa") ~ "Média-baixa",
      `Intensidade.tecnológica..OCDE...Morceiro..2019.` == "Média" ~ "Média",
      `Intensidade.tecnológica..OCDE...Morceiro..2019.` == "Média-alta" ~ "Média-alta",
      `Intensidade.tecnológica..OCDE...Morceiro..2019.` == "Alta" ~ "Alta",
      TRUE ~ "Baixa / Produtos Primários"
    )
  ) %>%
  select(hs6, intensidade_tecnologica) %>%
  distinct(hs6, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 5. OBTER DADOS DO PIB PER CAPITA E CÁLCULO DO PRODY (MUNDO TODO)
# ------------------------------------------------------------------------------

cat("\nBaixando dados do PIB per capita (PPP) do Banco Mundial...\n")

pib_pc_raw <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.PP.KD",
  start = 2011,
  end = 2024
)

paises_dic_unico <- paises_dic %>%
  select(country_code, country_iso3) %>%
  filter(!is.na(country_iso3) & country_iso3 != "") %>%
  distinct(country_iso3, .keep_all = TRUE)

pib_pc_prep <- pib_pc_raw %>%
  filter(!is.na(NY.GDP.PCAP.PP.KD)) %>%
  select(iso3c = iso3c, ano = year, pib_pc = NY.GDP.PCAP.PP.KD) %>%
  inner_join(paises_dic_unico, by = c("iso3c" = "country_iso3")) %>%
  select(ano, pais_code = country_code, pib_pc) %>%
  distinct(ano, pais_code, .keep_all = TRUE)

base_prody_prep <- base_ivcr_hs6 %>%
  inner_join(pib_pc_prep, by = c("ano", "pais_code"))

soma_shares_k <- base_prody_prep %>%
  group_by(ano, hs6) %>%
  summarise(soma_share_mundo_k = sum(share_pais, na.rm = TRUE), .groups = "drop")

tabela_prody <- base_prody_prep %>%
  inner_join(soma_shares_k, by = c("ano", "hs6")) %>%
  mutate(peso_pais_prody = share_pais / soma_share_mundo_k) %>%
  group_by(ano, hs6) %>%
  summarise(
    prody_usd = sum(peso_pais_prody * pib_pc, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 6. SHINY APP ATUALIZADO
# ------------------------------------------------------------------------------

arquivo_dicionario_hs <- list.files(
  path = caminho_pasta, 
  pattern = "product_codes_HS07_V202601", 
  full.names = TRUE
)

if (length(arquivo_dicionario_hs) > 0) {
  dicionario_produtos <- fread(arquivo_dicionario_hs[1])
  if ("code" %in% names(dicionario_produtos) && "description" %in% names(dicionario_produtos)) {
    dicionario_produtos <- dicionario_produtos %>% select(hs6 = code, descricao = description)
  } else {
    names(dicionario_produtos)[1:2] <- c("hs6", "descricao")
    dicionario_produtos <- dicionario_produtos %>% select(hs6, descricao)
  }
  dicionario_produtos$hs6 <- as.integer(dicionario_produtos$hs6)
} else {
  dicionario_produtos <- data.frame(hs6 = integer(), descricao = character())
}

ui <- fluidPage(
  titlePanel("Análise Dinâmica da Pauta Comercial e Intensidade Tecnológica (BACI)"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "exp_sel",
        label = "País(es) Exportador(es):",
        choices = c("Países do BRICS", paises_dic$country_name),
        selected = "Brazil",
        multiple = TRUE
      ),
      
      selectizeInput(
        inputId = "imp_sel",
        label = "País(es) Importador(es) / Destino:",
        choices = c("Mundo (Total)", "Países do BRICS", "Resto do Mundo", paises_dic$country_name),
        selected = "Mundo (Total)",
        multiple = TRUE
      ),
      
      sliderInput(
        inputId = "ano_limite",
        label = "Ano Limite / Ano de Análise:",
        min = 2011,
        max = 2024,
        value = 2024,
        step = 1,
        sep = ""
      ),
      helpText("Nota: O ano selecionado define o término da série histórica e a referência das tabelas e cálculos do EXPY."),
      
      hr(),
      actionButton("btn_limpar_cliques", "Limpar Seleção de Pontos", class = "btn-danger btn-sm"),
      br(), br(),
      uiOutput("painel_variacao_calculada")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela Consolidada (Ano Final)", 
                 br(),
                 DTOutput("tabela_resultado")),
        tabPanel("Detalhamento por Produto (HS6)",
                 br(),
                 p(tags$small("ℹ️ Exibe os itens exportados no ano selecionado no painel lateral, com nomes e descrições oficiais.")),
                 br(),
                 DTOutput("tabela_detalhada_produtos")),
        tabPanel("Balança Comercial Bilateral",
                 br(),
                 p(tags$small("ℹ️ Demonstra a balança comercial entre o(s) país(es) exportador(es) e o(s) país(es) importador(es) selecionados no ano de referência.")),
                 br(),
                 DTOutput("tabela_balanca_comercial")),
        tabPanel("Evolução Histórica (Gráfico Interativo)",
                 br(),
                 p(tags$small("💡 **Como usar a variação:** Clique no **primeiro ponto** e depois no **segundo ponto** da mesma linha para calcular a variação percentual.")),
                 plotlyOutput("grafico_evolucao", height = "520px")),
        tabPanel("Evolução por Produto (HS6)",
                 br(),
                 fluidRow(
                   column(12,
                          selectizeInput(
                            inputId = "produtos_sel",
                            label = "Selecione por Código ou por Descrição do Produto HS6:",
                            choices = NULL, 
                            multiple = TRUE,
                            options = list(placeholder = "Digite o código ou nome do produto (ex: Soja, 120100)...")
                          )
                   )
                 ),
                 p(tags$small("💡 **Como usar a variação:** Clique no 1º ponto e depois no 2º ponto da mesma linha no gráfico abaixo.")),
                 plotlyOutput("grafico_evolucao_produto", height = "500px")),
        tabPanel("Evolução do EXPY",
                 br(),
                 p(tags$small("ℹ️ Exibe a evolução do índice EXPY (Sofisticação da Pauta Exportadora) para cada combinação entre país exportador e país importador selecionados.")),
                 p(tags$small("💡 **Padrão da Legenda:** 'País Exportador - País Importador'. O PRODY é sempre calculado com base no mundo todo.")),
                 plotlyOutput("grafico_expy_evolucao", height = "520px")),
        tabPanel("Resumo da Seleção",
                 br(),
                 verbatimTextOutput("texto_resumo"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  cods_brics_vetor <- c(76, 156, 356, 643, 699, 710)
  
  observe({
    df_hist <- dados_filtrados_historico()
    if (nrow(df_hist) > 0) {
      unicos_hs6 <- sort(unique(df_hist$hs6))
      
      df_opcoes <- data.frame(hs6 = unicos_hs6) %>%
        left_join(dicionario_produtos, by = "hs6") %>%
        mutate(
          descricao = ifelse(is.na(descricao) | descricao == "", "Descrição não disponível", descricao),
          rotulo = paste0(str_pad(as.character(hs6), 6, side = "left", pad = "0"), " - ", descricao)
        )
      
      opcoes_vector <- setNames(df_opcoes$hs6, df_opcoes$rotulo)
      updateSelectizeInput(session, "produtos_sel", choices = opcoes_vector, server = TRUE)
    } else {
      updateSelectizeInput(session, "produtos_sel", choices = c(), server = TRUE)
    }
  })
  
  dados_filtrados_tabela <- reactive({
    ano_alvo <- input$ano_limite
    if ("Países do BRICS" %in% input$exp_sel) {
      cods_exp <- cods_brics_vetor
    } else {
      cods_exp <- paises_dic %>% filter(country_name %in% input$exp_sel) %>% pull(country_code)
    }
    
    quer_mundo <- "Mundo (Total)" %in% input$imp_sel
    quer_brics_imp <- "Países do BRICS" %in% input$imp_sel
    
    cods_imp <- NULL
    if (!quer_mundo && !quer_brics_imp) {
      cods_imp <- paises_dic %>% filter(country_name %in% input$imp_sel) %>% pull(country_code)
    }
    
    arquivo_ano <- list.files(path = caminho_pasta, pattern = paste0("BACI_HS07_Y", ano_alvo, "_V202601\\.csv$"), full.names = TRUE)
    if (length(arquivo_ano) == 0) return(data.frame())
    
    dt_bruto <- fread(arquivo_ano, select = c("t", "i", "j", "k", "v"))
    if (quer_mundo) {
      dt_bruto <- dt_bruto[t == ano_alvo & i %in% cods_exp & v > 0]
    } else if (quer_brics_imp) {
      dt_bruto <- dt_bruto[t == ano_alvo & i %in% cods_exp & j %in% cods_brics_vetor & v > 0]
      dt_bruto <- dt_bruto[i != j]
    } else {
      dt_bruto <- dt_bruto[t == ano_alvo & i %in% cods_exp & j %in% cods_imp & v > 0]
    }
    
    if (nrow(dt_bruto) == 0) return(data.frame())
    names(dt_bruto) <- c("ano", "pais_code", "destino_code", "hs6", "val_export_k")
    df_processado <- dt_bruto %>% left_join(codigos_hs_classificados %>% select(hs6, intensidade_tecnologica), by = "hs6")
    return(df_processado)
  })
  
  dados_filtrados_historico <- reactive({
    anos_alvo <- 2011:input$ano_limite
    if ("Países do BRICS" %in% input$exp_sel) {
      cods_exp <- cods_brics_vetor
    } else {
      cods_exp <- paises_dic %>% filter(country_name %in% input$exp_sel) %>% pull(country_code)
    }
    
    quer_mundo <- "Mundo (Total)" %in% input$imp_sel
    quer_brics_imp <- "Países do BRICS" %in% input$imp_sel
    
    cods_imp <- NULL
    if (!quer_mundo && !quer_brics_imp) {
      cods_imp <- paises_dic %>% filter(country_name %in% input$imp_sel) %>% pull(country_code)
    }
    
    lista_arquivos_anos <- list.files(path = caminho_pasta, pattern = paste0("BACI_HS07_Y(", paste(anos_alvo, collapse="|"), ")_V202601\\.csv$"), full.names = TRUE)
    if (length(lista_arquivos_anos) == 0) return(data.frame())
    
    dt_bruto <- map_dfr(lista_arquivos_anos, function(file) {
      dt <- fread(file, select = c("t", "i", "j", "k", "v"))
      if (quer_mundo) {
        dt <- dt[t %in% anos_alvo & i %in% cods_exp & v > 0]
      } else if (quer_brics_imp) {
        dt <- dt[t %in% anos_alvo & i %in% cods_exp & j %in% cods_brics_vetor & v > 0]
        dt <- dt[i != j]
      } else {
        dt <- dt[t %in% anos_alvo & i %in% cods_exp & j %in% cods_imp & v > 0]
      }
      return(dt)
    })
    
    if (nrow(dt_bruto) == 0) return(data.frame())
    names(dt_bruto) <- c("ano", "pais_code", "destino_code", "hs6", "val_export_k")
    df_processado <- dt_bruto %>% left_join(codigos_hs_classificados %>% select(hs6, intensidade_tecnologica), by = "hs6")
    return(df_processado)
  })
  
  # Tabela Consolidada
  output$tabela_resultado <- renderDT({
    df_f <- dados_filtrados_tabela()
    if (nrow(df_f) == 0) {
      return(datatable(data.frame(Aviso = "Nenhum registo comercial encontrado para os filtros selecionados no ano escolhido."), options = list(dom = 't'), rownames = FALSE))
    }
    
    df_agrupado <- df_f %>%
      group_by(intensidade_tecnologica) %>%
      summarise(
        qtd_produtos_hs6   = n_distinct(hs6),
        valor_milhares_usd = sum(val_export_k, na_rm = TRUE),
        .groups = "drop"
      )
    
    total_qtd_prod  <- n_distinct(df_f$hs6)
    total_valor_usd <- sum(df_agrupado$valor_milhares_usd) * 1000
    
    tabela <- df_agrupado %>%
      mutate(
        valor_usd          = valor_milhares_usd * 1000,
        participacao_valor = (valor_usd / total_valor_usd) * 100,
        participacao_prod  = (qtd_produtos_hs6 / total_qtd_prod) * 100
      ) %>%
      arrange(desc(valor_usd)) %>%
      mutate(
        `Intensidade Tecnológica` = intensidade_tecnologica,
        `Qtd. Produtos (HS6)`     = format(qtd_produtos_hs6, big.mark = ",", decimal.mark = "."),
        `Participação Diversidade`= paste0(format(round(participacao_prod, 2), nsmall = 2, decimal.mark = "."), "%"),
        `Valor Exportado (USD)`   = dollar(valor_usd, prefix = "US$ ", big.mark = ",", decimal.mark = "."),
        `Participação no Valor`   = paste0(format(round(participacao_valor, 2), nsmall = 2, decimal.mark = "."), "%")
      ) %>%
      select(`Intensidade Tecnológica`, `Qtd. Produtos (HS6)`, `Participação Diversidade`, `Valor Exportado (USD)`, `Participação no Valor`)
    
    linha_total <- data.frame(
      `Intensidade Tecnológica` = "TOTAL GERAL",
      `Qtd. Produtos (HS6)`     = format(total_qtd_prod, big.mark = ",", decimal.mark = "."),
      `Participação Diversidade`= "100.00%",
      `Valor Exportado (USD)`   = dollar(total_valor_usd, prefix = "US$ ", big.mark = ",", decimal.mark = "."),
      `Participação no Valor`   = "100.00%",
      check.names = FALSE
    )
    
    tabela_final <- bind_rows(tabela, linha_total)
    
    datatable(tabela_final, options = list(pageLength = 15, dom = 't'), rownames = FALSE) %>%
      formatStyle(
        'Intensidade Tecnológica',
        target = 'row',
        backgroundColor = styleEqual("TOTAL GERAL", "#f2f2f2"),
        fontWeight = styleEqual("TOTAL GERAL", "bold")
      )
  })
  
  # Detalhamento por Produto HS6
  output$tabela_detalhada_produtos <- renderDT({
    ano_detalhe_val <- input$ano_limite
    
    if ("Países do BRICS" %in% input$exp_sel) {
      cods_exp <- cods_brics_vetor
    } else {
      cods_exp <- paises_dic %>% filter(country_name %in% input$exp_sel) %>% pull(country_code)
    }
    
    quer_mundo <- "Mundo (Total)" %in% input$imp_sel
    quer_brics_imp <- "Países do BRICS" %in% input$imp_sel
    
    cods_imp <- NULL
    if (!quer_mundo && !quer_brics_imp) {
      cods_imp <- paises_dic %>% filter(country_name %in% input$imp_sel) %>% pull(country_code)
    }
    
    arquivo_ano <- list.files(path = caminho_pasta, pattern = paste0("BACI_HS07_Y", ano_detalhe_val, "_V202601\\.csv$"), full.names = TRUE)
    if (length(arquivo_ano) == 0) return(datatable(data.frame(Aviso = "Ficheiro BACI não encontrado para o ano selecionado.")))
    
    dt_bruto <- fread(arquivo_ano, select = c("t", "i", "j", "k", "v"))
    
    if (quer_mundo) {
      dt_bruto <- dt_bruto[t == ano_detalhe_val & i %in% cods_exp & v > 0]
    } else if (quer_brics_imp) {
      dt_bruto <- dt_bruto[t == ano_detalhe_val & i %in% cods_exp & j %in% cods_brics_vetor & v > 0]
      dt_bruto <- dt_bruto[i != j]
    } else {
      dt_bruto <- dt_bruto[t == ano_detalhe_val & i %in% cods_exp & j %in% cods_imp & v > 0]
    }
    
    if (nrow(dt_bruto) == 0) {
      return(datatable(data.frame(Aviso = "Nenhum registo encontrado para os filtros e ano selecionados."), options = list(dom = 't'), rownames = FALSE))
    }
    
    names(dt_bruto) <- c("ano", "pais_code", "destino_code", "hs6", "val_export_k")
    
    df_produtos <- dt_bruto %>%
      group_by(hs6) %>%
      summarise(valor_milhares_usd = sum(val_export_k, na_rm = TRUE), .groups = "drop") %>%
      mutate(valor_usd = valor_milhares_usd * 1000)
    
    total_geral_usd <- sum(df_produtos$valor_usd)
    total_qtd_hs6   <- nrow(df_produtos)
    
    df_produtos_classificado <- df_produtos %>%
      left_join(dicionario_produtos, by = "hs6") %>%
      left_join(codigos_hs_classificados %>% select(hs6, intensidade_tecnologica), by = "hs6") %>%
      mutate(
        hs6_str = str_pad(as.character(hs6), width = 6, side = "left", pad = "0"),
        descricao = ifelse(is.na(descricao) | descricao == "", "Descrição não disponível", descricao),
        participacao = (valor_usd / total_geral_usd) * 100
      ) %>%
      arrange(desc(valor_usd)) %>%
      mutate(
        `Código HS07 (6 dígitos)`  = hs6_str,
        `Descrição do Produto`     = descricao,
        `Valor Exportado (USD)`    = dollar(valor_usd, prefix = "US$ ", big.mark = ",", decimal.mark = "."),
        `Participação nas Exportações` = paste0(format(round(participacao, 2), nsmall = 2, decimal.mark = "."), "%"),
        `Intensidade Tecnológica`  = intensidade_tecnologica
      ) %>%
      select(`Código HS07 (6 dígitos)`, `Descrição do Produto`, `Valor Exportado (USD)`, `Participação nas Exportações`, `Intensidade Tecnológica`)
    
    linha_total <- data.frame(
      `Código HS07 (6 dígitos)`  = "TOTAL",
      `Descrição do Produto`     = paste0(format(total_qtd_hs6, big.mark = ",", decimal.mark = "."), " produtos distintos"),
      `Valor Exportado (USD)`    = dollar(total_geral_usd, prefix = "US$ ", big.mark = ",", decimal.mark = "."),
      `Participação nas Exportações` = "100.00%",
      `Intensidade Tecnológica`  = "-",
      check.names = FALSE
    )
    
    tabela_final_produtos <- bind_rows(df_produtos_classificado, linha_total)
    
    datatable(tabela_final_produtos, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(
        'Código HS07 (6 dígitos)',
        target = 'row',
        backgroundColor = styleEqual("TOTAL", "#f2f2f2"),
        fontWeight = styleEqual("TOTAL", "bold")
      )
  })
  
  # Balança Comercial Bilateral (Com paginação, pesquisa e controles do DataTables)
  output$tabela_balanca_comercial <- renderDT({
    ano_alvo <- input$ano_limite
    
    if ("Países do BRICS" %in% input$exp_sel) {
      cods_exp <- cods_brics_vetor
    } else {
      cods_exp <- paises_dic %>% filter(country_name %in% input$exp_sel) %>% pull(country_code)
    }
    
    arquivo_ano <- list.files(path = caminho_pasta, pattern = paste0("BACI_HS07_Y", ano_alvo, "_V202601\\.csv$"), full.names = TRUE)
    if (length(arquivo_ano) == 0) return(datatable(data.frame(Aviso = "Ficheiro BACI não encontrado para o ano selecionado.")))
    
    dt_bruto <- fread(arquivo_ano, select = c("t", "i", "j", "k", "v"))
    names(dt_bruto) <- c("ano", "pais_exp", "pais_imp", "hs6", "val_export_k")
    
    dt_exp <- dt_bruto[ano == ano_alvo & pais_exp %in% cods_exp & val_export_k > 0]
    if (nrow(dt_exp) == 0) {
      return(datatable(data.frame(Aviso = "Nenhum dado comercial encontrado para os exportadores selecionados."), options = list(dom = 't'), rownames = FALSE))
    }
    
    total_exp_mundo_bi <- sum(dt_exp$val_export_k) / 1e6
    dt_imp_mundo <- dt_bruto[ano == ano_alvo & pais_imp %in% cods_exp & val_export_k > 0]
    total_imp_mundo_bi <- sum(dt_imp_mundo$val_export_k) / 1e6
    
    # Determina todos os parceiros comerciais que tiveram transação com o exportador no ano
    cods_destinos_unicos <- unique(c(dt_exp$pais_imp, dt_bruto[ano == ano_alvo & pais_exp %in% cods_exp & pais_imp %in% cods_exp, pais_imp]))
    destinos_selecionados <- paises_dic %>% filter(country_code %in% cods_destinos_unicos)
    cods_dest_vetor <- destinos_selecionados$country_code
    
    linhas_paises <- lapply(cods_dest_vetor, function(d_code) {
      nome_pais <- paises_dic %>% filter(country_code == d_code) %>% pull(country_name)
      if (length(nome_pais) == 0) nome_pais <- as.character(d_code)
      
      exp_bi <- sum(dt_exp[pais_imp == d_code, val_export_k], na.rm = TRUE) / 1e6
      imp_bi <- sum(dt_bruto[ano == ano_alvo & pais_exp == d_code & pais_imp %in% cods_exp, val_export_k], na.rm = TRUE) / 1e6
      
      part_exp <- ifelse(total_exp_mundo_bi > 0, (exp_bi / total_exp_mundo_bi) * 100, 0)
      part_imp <- ifelse(total_imp_mundo_bi > 0, (imp_bi / total_imp_mundo_bi) * 100, 0)
      saldo_bi <- exp_bi - imp_bi
      
      data.frame(
        `País Importador` = nome_pais,
        `Exportação (Bi US$)` = exp_bi,
        `Part. Exp. (%)` = part_exp,
        `Importação (Bi US$)` = imp_bi,
        `Part. Imp. (%)` = part_imp,
        `Saldo Comercial (Bi US$)` = saldo_bi,
        check.names = FALSE
      )
    })
    
    df_paises <- bind_rows(linhas_paises) %>% arrange(desc(`Exportação (Bi US$)`))
    
    tot_exp_sel <- sum(df_paises$`Exportação (Bi US$)`, na.rm = TRUE)
    tot_imp_sel <- sum(df_paises$`Importação (Bi US$)`, na.rm = TRUE)
    tot_part_exp_sel <- ifelse(total_exp_mundo_bi > 0, (tot_exp_sel / total_exp_mundo_bi) * 100, 0)
    tot_part_imp_sel <- ifelse(total_imp_mundo_bi > 0, (tot_imp_sel / total_imp_mundo_bi) * 100, 0)
    tot_saldo_sel <- tot_exp_sel - tot_imp_sel
    
    linha_ultima <- data.frame(
      `País Importador` = "TOTAL COM O MUNDO",
      `Exportação (Bi US$)` = total_exp_mundo_bi,
      `Part. Exp. (%)` = 100.00,
      `Importação (Bi US$)` = total_imp_mundo_bi,
      `Part. Imp. (%)` = 100.00,
      `Saldo Comercial (Bi US$)` = total_exp_mundo_bi - total_imp_mundo_bi,
      check.names = FALSE
    )
    
    df_balanca_final <- bind_rows(df_paises, linha_ultima)
    
    df_exibicao <- df_balanca_final %>%
      mutate(
        `Exportação (Bi US$)` = format(round(`Exportação (Bi US$)`, 2), nsmall = 2, big.mark = ",", decimal.mark = "."),
        `Part. Exp. (%)`      = paste0(format(round(`Part. Exp. (%)`, 2), nsmall = 2, decimal.mark = "."), "%"),
        `Importação (Bi US$)` = format(round(`Importação (Bi US$)`, 2), nsmall = 2, big.mark = ",", decimal.mark = "."),
        `Part. Imp. (%)`      = paste0(format(round(`Part. Imp. (%)`, 2), nsmall = 2, decimal.mark = "."), "%"),
        `Saldo Comercial (Bi US$)` = format(round(`Saldo Comercial (Bi US$)`, 2), nsmall = 2, big.mark = ",", decimal.mark = ".")
      )
    
    n_linhas <- nrow(df_exibicao)
    
    # Removemos o dom = 't' restritivo para liberar a caixa de busca, seletor de páginas e numeração
    datatable(df_exibicao, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(
        columns = 1:6,
        target = 'row',
        backgroundColor = styleRow(n_linhas, '#f2f2f2'),
        fontWeight = styleRow(n_linhas, 'bold')
      )
  })
  
  # Gráfico 1: Evolução Histórica (Intensidade Tecnológica)
  output$grafico_evolucao <- renderPlotly({
    df_hist <- dados_filtrados_historico()
    if (nrow(df_hist) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Sem dados suficientes.", size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    df_agrupado_ano <- df_hist %>%
      group_by(ano, intensidade_tecnologica) %>%
      summarise(valor_total_usd = sum(val_export_k, na_rm = TRUE) * 1000, .groups = "drop") %>%
      mutate(valor_bi = valor_total_usd / 1e9)
    
    g <- ggplot(df_agrupado_ano, aes(x = ano, y = valor_bi, color = intensidade_tecnologica, group = intensidade_tecnologica,
                                     text = paste0("<b>Categoria:</b> ", intensidade_tecnologica,
                                                   "<br><b>Ano:</b> ", ano,
                                                   "<br><b>Valor:</b> US$ ", format(round(valor_total_usd, 2), big.mark = ",", decimal.mark = ".", scientific = FALSE)))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = 2011:input$ano_limite) +
      labs(title = paste("Evolução Histórica por Intensidade (2011–", input$ano_limite, ")"),
           x = "Ano", y = "Valor Total (USD Bilhões)", color = "Intensidade Tecnológica") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    p_plotly <- ggplotly(g, tooltip = "text", source = "grafico_serie") %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      config(displayModeBar = TRUE)
    
    event_register(p_plotly, "plotly_click")
    return(p_plotly)
  })
  
  # Gráfico 2: Evolução por Código HS6 Específico
  output$grafico_evolucao_produto <- renderPlotly({
    df_hist <- dados_filtrados_historico()
    
    if (nrow(df_hist) == 0 || is.null(input$produtos_sel) || length(input$produtos_sel) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Selecione pelo menos um código HS6 no filtro acima.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    cods_selecionados <- as.integer(input$produtos_sel)
    
    df_produto_filtrado <- df_hist %>%
      filter(hs6 %in% cods_selecionados) %>%
      group_by(ano, hs6) %>%
      summarise(valor_total_usd = sum(val_export_k, na_rm = TRUE) * 1000, .groups = "drop") %>%
      left_join(dicionario_produtos, by = "hs6") %>%
      mutate(
        hs6_str = str_pad(as.character(hs6), width = 6, side = "left", pad = "0"),
        descricao = ifelse(is.na(descricao) | descricao == "", "", paste0(" - ", descricao)),
        rotulo_produto = paste0(hs6_str, descricao)
      )
    
    if (nrow(df_produto_filtrado) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Nenhum registo encontrado para os produtos selecionados no período.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    g_prod <- ggplot(df_produto_filtrado, aes(x = ano, y = valor_total_usd, color = rotulo_produto, group = rotulo_produto,
                                              text = paste0("<b>Produto:</b> ", rotulo_produto,
                                                            "<br><b>Ano:</b> ", ano,
                                                            "<br><b>Valor:</b> US$ ", format(round(valor_total_usd, 2), big.mark = ",", decimal.mark = ".", scientific = FALSE)))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = 2011:input$ano_limite) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "US$ ", big.mark = ",", decimal.mark = ".")) +
      labs(title = paste("Evolução das Exportações por Produto HS6 (2011–", input$ano_limite, ")"),
           x = "Ano", y = "Valor Exportado (USD)", color = "Produto (HS6)") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    p_plotly_prod <- ggplotly(g_prod, tooltip = "text", source = "grafico_produto") %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      config(displayModeBar = TRUE)
    
    event_register(p_plotly_prod, "plotly_click")
    return(p_plotly_prod)
  })
  
  # ==============================================================================
  # GRÁFICO 3: EVOLUÇÃO DO EXPY (Múltiplos Exportadores x Múltiplos Importadores)
  # ==============================================================================
  output$grafico_expy_evolucao <- renderPlotly({
    anos_alvo <- 2011:input$ano_limite
    
    # 1. Mapeia os exportadores selecionados
    if ("Países do BRICS" %in% input$exp_sel) {
      df_exp_alvos <- paises_dic %>% filter(country_code %in% cods_brics_vetor) %>% select(country_code, country_name)
    } else {
      df_exp_alvos <- paises_dic %>% filter(country_name %in% input$exp_sel) %>% select(country_code, country_name)
    }
    
    if (nrow(df_exp_alvos) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Selecione pelo menos um país exportador.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    # 2. Mapeia os importadores selecionados
    quer_mundo <- "Mundo (Total)" %in% input$imp_sel
    quer_brics_imp <- "Países do BRICS" %in% input$imp_sel
    quer_resto_mundo <- "Resto do Mundo" %in% input$imp_sel
    
    lista_arquivos_anos <- list.files(path = caminho_pasta, pattern = paste0("BACI_HS07_Y(", paste(anos_alvo, collapse="|"), ")_V202601\\.csv$"), full.names = TRUE)
    if (length(lista_arquivos_anos) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Arquivos do BACI não encontrados.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    # Carrega o bruto do BACI para os anos limite
    dt_bruto_expy <- map_dfr(lista_arquivos_anos, function(file) {
      dt <- fread(file, select = c("t", "i", "j", "k", "v"))
      dt[t %in% anos_alvo & i %in% df_exp_alvos$country_code & v > 0]
    })
    
    if (nrow(dt_bruto_expy) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Sem registros comerciais para os filtros de EXPY.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    names(dt_bruto_expy) <- c("ano", "pais_exp", "pais_imp", "hs6", "val_export_k")
    
    # Cria uma lista/tabela expandida para cada importador selecionado
    lista_importadores_processados <- list()
    
    if (quer_mundo) {
      df_mundo <- dt_bruto_expy %>%
        mutate(rotulo_importador = "Mundo")
      lista_importadores_processados[["Mundo"]] <- df_mundo
    }
    
    if (quer_brics_imp) {
      df_brics <- dt_bruto_expy %>%
        filter(pais_imp %in% cods_brics_vetor) %>%
        mutate(rotulo_importador = "Países do BRICS")
      lista_importadores_processados[["BRICS"]] <- df_brics
    }
    
    if (quer_resto_mundo) {
      df_resto <- dt_bruto_expy %>%
        filter(!(pais_imp %in% cods_brics_vetor)) %>%
        mutate(rotulo_importador = "Resto do Mundo")
      lista_importadores_processados[["Resto"]] <- df_resto
    }
    
    # Outros países específicos selecionados individualmente
    outros_imps <- setdiff(input$imp_sel, c("Mundo (Total)", "Países do BRICS", "Resto do Mundo"))
    if (length(outros_imps) > 0) {
      cods_outros <- paises_dic %>% filter(country_name %in% outros_imps) %>% pull(country_code)
      for (c_imp in cods_outros) {
        n_imp <- paises_dic %>% filter(country_code == c_imp) %>% pull(country_name)
        df_esp <- dt_bruto_expy %>%
          filter(pais_imp == c_imp) %>%
          mutate(rotulo_importador = n_imp)
        lista_importadores_processados[[as.character(c_imp)]] <- df_esp
      }
    }
    
    if (length(lista_importadores_processados) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Selecione pelo menos um país importador ou agregado.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    dt_combinado_importadores <- bind_rows(lista_importadores_processados)
    
    # 3. Calcula os shares dinâmicos para cada par (Exportador - Importador) em cada ano
    df_shares_dinamicos <- dt_combinado_importadores %>%
      group_by(ano, pais_exp, rotulo_importador, hs6) %>%
      summarise(val_k = sum(val_export_k, na_rm = TRUE), .groups = "drop_last") %>%
      mutate(share_dinamico = val_k / sum(val_k, na.rm = TRUE)) %>%
      ungroup()
    
    # 4. Cruza com a tabela global de PRODY (calculada com base no mundo todo)
    df_expy_calculado <- df_shares_dinamicos %>%
      inner_join(tabela_prody, by = c("ano", "hs6")) %>%
      group_by(ano, pais_exp, rotulo_importador) %>%
      summarise(expy_usd = sum(share_dinamico * prody_usd, na.rm = TRUE), .groups = "drop") %>%
      left_join(paises_dic %>% select(country_code, country_name), by = c("pais_exp" = "country_code")) %>%
      mutate(serie_nome = paste0(country_name, " - ", rotulo_importador))
    
    if (nrow(df_expy_calculado) == 0) {
      p <- ggplot() + annotate("text", x = 4, y = 25, label = "Sem dados suficientes para gerar as linhas de EXPY.", size = 4) + theme_void()
      return(ggplotly(p))
    }
    
    g_expy <- ggplot(df_expy_calculado, aes(x = ano, y = expy_usd, color = serie_nome, group = serie_nome,
                                            text = paste0("<b>Combinação:</b> ", serie_nome,
                                                          "<br><b>Ano:</b> ", ano,
                                                          "<br><b>EXPY:</b> US$ ", format(round(expy_usd, 2), big.mark = ",", decimal.mark = ".")))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = 2011:input$ano_limite) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "US$ ", big.mark = ",", decimal.mark = ".")) +
      labs(
        title = paste0("Evolução da Sofisticação da Pauta Exportadora - EXPY (2011–", input$ano_limite, ")"),
        x = "Ano",
        y = "EXPY (USD PPP)",
        color = "Exportador - Importador"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    p_plotly_expy <- ggplotly(g_expy, tooltip = "text", source = "grafico_expy") %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      config(displayModeBar = TRUE)
    
    event_register(p_plotly_expy, "plotly_click")
    return(p_plotly_expy)
  })
  
  # Gestão unificada de cliques para cálculo da variação percentual
  ponto_clique_1 <- reactiveVal(NULL)
  ponto_clique_2 <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "grafico_serie"), {
    clique <- event_data("plotly_click", source = "grafico_serie")
    if (is.null(clique)) return()
    
    df_hist <- dados_filtrados_historico()
    df_agrupado_ano <- df_hist %>%
      group_by(ano, intensidade_tecnologica) %>%
      summarise(valor_total_usd = sum(val_export_k, na_rm = TRUE) * 1000, .groups = "drop") %>%
      mutate(valor_bi = valor_total_usd / 1e9)
    
    categorias_ordenadas <- sort(unique(df_agrupado_ano$intensidade_tecnologica))
    indice_curva <- clique$curveNumber + 1
    
    if (indice_curva <= length(categorias_ordenadas)) {
      categoria_clicada <- categorias_ordenadas[indice_curva]
      ano_clicado <- round(clique$x)
      
      ponto_encontrado <- df_agrupado_ano %>%
        filter(intensidade_tecnologica == categoria_clicada) %>%
        mutate(dif = abs(ano - ano_clicado)) %>%
        arrange(dif) %>%
        slice(1) %>%
        mutate(nome_serie = as.character(intensidade_tecnologica))
      
      if (nrow(ponto_encontrado) > 0) processar_clique_ponto(ponto_encontrado)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "grafico_produto"), {
    clique <- event_data("plotly_click", source = "grafico_produto")
    if (is.null(clique)) return()
    
    df_hist <- dados_filtrados_historico()
    if (is.null(input$produtos_sel)) return()
    
    cods_selecionados <- as.integer(input$produtos_sel)
    df_produto_filtrado <- df_hist %>%
      filter(hs6 %in% cods_selecionados) %>%
      group_by(ano, hs6) %>%
      summarise(valor_total_usd = sum(val_export_k, na_rm = TRUE) * 1000, .groups = "drop") %>%
      left_join(dicionario_produtos, by = "hs6") %>%
      mutate(
        hs6_str = str_pad(as.character(hs6), width = 6, side = "left", pad = "0"),
        descricao = ifelse(is.na(descricao) | descricao == "", "", paste0(" - ", descricao)),
        rotulo_produto = paste0(hs6_str, descricao)
      )
    
    produtos_ordenados <- sort(unique(df_produto_filtrado$rotulo_produto))
    indice_curva <- clique$curveNumber + 1
    
    if (indice_curva <= length(produtos_ordenados)) {
      produto_clicado <- produtos_ordenados[indice_curva]
      ano_clicado <- round(clique$x)
      
      ponto_encontrado <- df_produto_filtrado %>%
        filter(rotulo_produto == produto_clicado) %>%
        mutate(dif = abs(ano - ano_clicado)) %>%
        arrange(dif) %>%
        slice(1) %>%
        rename(intensidade_tecnologica = rotulo_produto)
      
      if (nrow(ponto_encontrado) > 0) processar_clique_ponto(ponto_encontrado)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "grafico_expy"), {
    clique <- event_data("plotly_click", source = "grafico_expy")
    if (is.null(clique)) return()
    
    anos_alvo <- 2011:input$ano_limite
    if ("Países do BRICS" %in% input$exp_sel) {
      df_exp_alvos <- paises_dic %>% filter(country_code %in% cods_brics_vetor) %>% select(country_code, country_name)
    } else {
      df_exp_alvos <- paises_dic %>% filter(country_name %in% input$exp_sel) %>% select(country_code, country_name)
    }
    
    quer_mundo <- "Mundo (Total)" %in% input$imp_sel
    quer_brics_imp <- "Países do BRICS" %in% input$imp_sel
    quer_resto_mundo <- "Resto do Mundo" %in% input$imp_sel
    
    lista_arquivos_anos <- list.files(path = caminho_pasta, pattern = paste0("BACI_HS07_Y(", paste(anos_alvo, collapse="|"), ")_V202601\\.csv$"), full.names = TRUE)
    dt_bruto_expy <- map_dfr(lista_arquivos_anos, function(file) {
      dt <- fread(file, select = c("t", "i", "j", "k", "v"))
      dt[t %in% anos_alvo & i %in% df_exp_alvos$country_code & v > 0]
    })
    
    names(dt_bruto_expy) <- c("ano", "pais_exp", "pais_imp", "hs6", "val_export_k")
    
    lista_importadores_processados <- list()
    if (quer_mundo) lista_importadores_processados[["Mundo"]] <- dt_bruto_expy %>% mutate(rotulo_importador = "Mundo")
    if (quer_brics_imp) lista_importadores_processados[["BRICS"]] <- dt_bruto_expy %>% filter(pais_imp %in% cods_brics_vetor) %>% mutate(rotulo_importador = "Países do BRICS")
    if (quer_resto_mundo) lista_importadores_processados[["Resto"]] <- dt_bruto_expy %>% filter(!(pais_imp %in% cods_brics_vetor)) %>% mutate(rotulo_importador = "Resto do Mundo")
    
    outros_imps <- setdiff(input$imp_sel, c("Mundo (Total)", "Países do BRICS", "Resto do Mundo"))
    if (length(outros_imps) > 0) {
      cods_outros <- paises_dic %>% filter(country_name %in% outros_imps) %>% pull(country_code)
      for (c_imp in cods_outros) {
        n_imp <- paises_dic %>% filter(country_code == c_imp) %>% pull(country_name)
        lista_importadores_processados[[as.character(c_imp)]] <- dt_bruto_expy %>% filter(pais_imp == c_imp) %>% mutate(rotulo_importador = n_imp)
      }
    }
    
    dt_combinado_importadores <- bind_rows(lista_importadores_processados)
    
    df_expy_calculado <- dt_combinado_importadores %>%
      group_by(ano, pais_exp, rotulo_importador, hs6) %>%
      summarise(val_k = sum(val_export_k, na_rm = TRUE), .groups = "drop_last") %>%
      mutate(share_dinamico = val_k / sum(val_k, na.rm = TRUE)) %>%
      ungroup() %>%
      inner_join(tabela_prody, by = c("ano", "hs6")) %>%
      group_by(ano, pais_exp, rotulo_importador) %>%
      summarise(expy_usd = sum(share_dinamico * prody_usd, na.rm = TRUE), .groups = "drop") %>%
      left_join(paises_dic %>% select(country_code, country_name), by = c("pais_exp" = "country_code")) %>%
      mutate(serie_nome = paste0(country_name, " - ", rotulo_importador))
    
    series_ordenadas <- sort(unique(df_expy_calculado$serie_nome))
    indice_curva <- clique$curveNumber + 1
    
    if (indice_curva <= length(series_ordenadas)) {
      serie_clicada <- series_ordenadas[indice_curva]
      ano_clicado <- round(clique$x)
      
      ponto_encontrado <- df_expy_calculado %>%
        filter(serie_nome == serie_clicada) %>%
        mutate(dif = abs(ano - ano_clicado)) %>%
        arrange(dif) %>%
        slice(1) %>%
        rename(intensidade_tecnologica = serie_nome, valor_total_usd = expy_usd)
      
      if (nrow(ponto_encontrado) > 0) processar_clique_ponto(ponto_encontrado)
    }
  })
  
  processar_clique_ponto <- function(ponto_encontrado) {
    if (is.null(ponto_clique_1())) {
      ponto_clique_1(ponto_encontrado)
      ponto_clique_2(NULL)
    } else if (!is.null(ponto_clique_1()) && is.null(ponto_clique_2())) {
      p1 <- ponto_clique_1()
      if (p1$intensidade_tecnologica == ponto_encontrado$intensidade_tecnologica) {
        ponto_clique_2(ponto_encontrado)
      } else {
        ponto_clique_1(ponto_encontrado)
        ponto_clique_2(NULL)
      }
    } else {
      ponto_clique_1(ponto_encontrado)
      ponto_clique_2(NULL)
    }
  }
  
  observeEvent(input$btn_limpar_cliques, {
    ponto_clique_1(NULL)
    ponto_clique_2(NULL)
  })
  
  output$painel_variacao_calculada <- renderUI({
    p1 <- ponto_clique_1()
    p2 <- ponto_clique_2()
    
    if (is.null(p1)) {
      return(div(tags$b("Medição de Variação:"), p("Clique em um 1º ponto em qualquer gráfico.")))
    } else if (!is.null(p1) && is.null(p2)) {
      return(div(
        tags$b("Medição de Variação:"),
        p(paste0("1º Ponto: ", p1$intensidade_tecnologica, " (Ano: ", p1$ano, ")")),
        p("👉 Clique no 2º ponto da mesma linha para ver a variação.")
      ))
    } else {
      val_inicial <- p1$valor_total_usd
      val_final <- p2$valor_total_usd
      variacao_pct <- ((val_final - val_inicial) / val_inicial) * 100
      
      cor_texto <- ifelse(variacao_pct >= 0, "green", "red")
      sinal <- ifelse(variacao_pct >= 0, "+", "")
      
      return(div(
        style = "background: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #ddd;",
        tags$b("📊 Resultado da Variação:"),
        tags$ul(
          tags$li(paste0("Série: ", p1$intensidade_tecnologica)),
          tags$li(paste0("De: ", p1$ano, " (", dollar(val_inicial, prefix = "US$ ", big.mark = ",", decimal.mark = "."), ")")),
          tags$li(paste0("Até: ", p2$ano, " (", dollar(val_final, prefix = "US$ ", big.mark = ",", decimal.mark = "."), ")"))
        ),
        tags$h4(style = paste0("color: ", cor_texto, "; font-weight: bold;"),
                paste0("Variação: ", sinal, format(round(variacao_pct, 2), nsmall = 2, decimal.mark = "."), "%"))
      ))
    }
  })
  
  output$texto_resumo <- renderPrint({
    cat("======================================================================\n")
    cat(" RESUMO DA CONSULTA DINÂMICA\n")
    cat("======================================================================\n")
    cat("Países Exportadores:", paste(input$exp_sel, collapse = ", "), "\n")
    cat("Destinos Selecionados:", paste(input$imp_sel, collapse = ", "), "\n")
    cat("Intervalo Temporal da Análise: 2011 a", input$ano_limite, "\n")
    cat("======================================================================\n")
  })
}

shinyApp(ui = ui, server = server)
