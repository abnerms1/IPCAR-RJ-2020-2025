# =============================================================================
# TCC | IPCAR - ESTADO DO RIO DE JANEIRO (2020-2025)
# SCRIPT 05 - GRAFICOS COMPARATIVOS + TABELA NO FORMATO DE CURITIBA
# =============================================================================
# Usa SOMENTE os resultados das etapas 03/04 e as 6 planilhas anuais do IBGE.
# Nao recalcula os pesos e nao baixa dados pela internet (exceto instalacao
# inicial de bibliotecas, se necessaria).
#
# DEFINICOES IMPORTANTES:
#   IPCAR (geral): media dos 7 indices por renda, ponderada pelo gasto
#                  expandido de cada faixa na POF (criterio plutocratico).
#   11 GRUPOS POF: reagrupamento de subitens cujas variacoes sao do IPCA.
#                  Eles NAO sao 11 grupos oficiais publicados pelo IBGE.
#   IPCA OFICIAL RJ: serie mensal da area Rio de Janeiro nas 6 planilhas.
#
# O resultado representa a estrutura de consumo estadual da POF 2017-2018
# submetida aos precos da area pesquisada pelo IPCA regional do Rio.
# =============================================================================

# ---- 1. CONFIGURACAO LOCAL: ALTERE SOMENTE OS DOIS CAMINHOS -----------------
# ============================================================================
# CONFIGURACAO DOS CAMINHOS - EDITE A LINHA `pasta_microdados` ABAIXO
# ============================================================================
# Informe a pasta LOCAL que contem os arquivos .txt oficiais da POF 2017-2018
# e a planilha Tabela_de_correspondencia_despesas_POF_2017_2018.xlsx.
# Use barras / mesmo no Windows (nao use \ isolada).
# Exemplo Windows: "C:/Users/SEU_USUARIO/Documentos/POF/Dados_20230713"
# Exemplo Linux:   "/home/seu_usuario/POF/Dados_20230713"
# ATENCAO: utilize EXATAMENTE a mesma pasta nos Scripts 01, 02, 03, 04 e 05.
# As pastas TCC_RJ_NOVO/01_preparacao ... /05_graficos_comparativos serao
# criadas automaticamente DENTRO da pasta_microdados, como no estudo original.
pasta_microdados <- "C:/ALTERE_AQUI/POF/Dados_20230713"

if (!dir.exists(pasta_microdados)) {
  stop(paste0("Pasta da POF nao encontrada: ", pasta_microdados,
              "\nEdite SOMENTE 'pasta_microdados' no bloco CONFIGURACAO ",
              "do inicio deste script e execute novamente."), call. = FALSE)
}
# ============================================================================
# Informe a pasta LOCAL com EXATAMENTE estes 6 arquivos do IPCA regional:
# "IPCA (2020).xlsx", "IPCA (2021).xlsx", ... , "IPCA (2025).xlsx".
# Os arquivos devem conservar o formato e as abas utilizadas na monografia.
# Exemplo Windows: "C:/Users/SEU_USUARIO/Documentos/IPCA"
pasta_ipca <- "C:/ALTERE_AQUI/IPCA"
if (!dir.exists(pasta_ipca)) {
  stop(paste0("Pasta do IPCA nao encontrada: ", pasta_ipca,
              "\nEdite SOMENTE 'pasta_ipca' no bloco CONFIGURACAO ",
              "do inicio do Script 05 e execute novamente."), call. = FALSE)
}
ano_foco <- 2025  # MANTER 2025 para os mesmos graficos da monografia (2020-2025)
# -----------------------------------------------------------------------------

pasta_03 <- file.path(pasta_microdados, "TCC_RJ_NOVO", "03_pesos_11_tipos")
pasta_04 <- file.path(pasta_microdados, "TCC_RJ_NOVO", "04_inflacao_faixa")
saida <- file.path(pasta_microdados, "TCC_RJ_NOVO", "05_graficos_comparativos")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

pacotes <- c("readxl", "readr", "dplyr", "tidyr", "ggplot2", "stringr",
             "tibble", "scales", "RColorBrewer", "writexl")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) install.packages(faltantes, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(readxl); library(readr); library(dplyr); library(tidyr)
  library(ggplot2); library(stringr); library(tibble); library(scales)
  library(RColorBrewer)
})

for (arq in c(file.path(pasta_03, "03_02_PESOS_11_TIPOS_POR_FAIXA.csv"),
              file.path(pasta_04, "04_03_INDICES_MENSAIS_FAIXA.csv"),
              file.path(pasta_04, "04_06_INDICES_11_TIPOS_MENSAIS.csv"),
              file.path(pasta_04, "04_01_VARIACOES_MENSAIS_POR_SUBITEM.csv"),
              file.path(pasta_04, "04_00_MAPA_CODIGOS_SNIPC_IDS_SIDRA.csv"),
              file.path(pasta_ipca, sprintf("IPCA (%d).xlsx", 2020:2025)))) {
  if (!file.exists(arq)) stop("Arquivo necessario nao encontrado: ", arq)
}

faixas <- c("Até 2 SM", "2-3 SM", "3-6 SM", "6-10 SM", "10-15 SM",
            "15-25 SM", "Mais 25 SM")
nomes_grupos <- c(
  "01" = "Alimentação", "02" = "Habitação", "03" = "Vestuário",
  "04" = "Transporte", "05" = "Higiene e cuidados pessoais",
  "06" = "Assistência à saúde", "07" = "Educação",
  "08" = "Recreação e cultura", "09" = "Fumo",
  "10" = "Serviços pessoais", "11" = "Despesas diversas")
ordem_grupos <- unname(nomes_grupos)
cores_grupos <- stats::setNames(grDevices::hcl.colors(11, "Dark 3"), ordem_grupos)
cores_faixas <- stats::setNames(RColorBrewer::brewer.pal(8, "Dark2")[1:7], faixas)
cor_ipcar <- "#173F68"
cor_ipca <- "#CA3D3D"
cor_max <- "#B4522D"
cor_min <- "#0C817F"
fmt_pct <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")
limpo_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))

# ---- 2. LEITURA DOS RESULTADOS DOS SCRIPTS 03 E 04 ----------------------------
gastos <- readr::read_csv(file.path(pasta_03, "03_02_PESOS_11_TIPOS_POR_FAIXA.csv"), show_col_types = FALSE) |>
  mutate(FAIXA_LABEL = dplyr::recode(as.character(.data$FAIXA_RENDA),
           "1 - Ate 2 SM" = "Até 2 SM", "2 - Mais de 2 a 3 SM" = "2-3 SM",
           "3 - Mais de 3 a 6 SM" = "3-6 SM", "4 - Mais de 6 a 10 SM" = "6-10 SM",
           "5 - Mais de 10 a 15 SM" = "10-15 SM", "6 - Mais de 15 a 25 SM" = "15-25 SM",
           "7 - Mais de 25 SM" = "Mais 25 SM"),
         GRUPO = unname(nomes_grupos[substr(.data$TIPO_POF_11, 1, 2)]))
if (nrow(gastos) != 77 || anyNA(gastos$GRUPO) ||
    !setequal(gastos$FAIXA_LABEL, faixas))
  stop("A tabela de gastos da Etapa 03 nao contem as 7 faixas e 11 grupos esperados.")

cesta_faixas <- gastos |>
  group_by(.data$FAIXA_LABEL) |>
  summarise(GASTO_BASE = sum(.data$DESPESA_EXPANDIDA), .groups = "drop") |>
  mutate(PESO_FAIXA = .data$GASTO_BASE / sum(.data$GASTO_BASE))

ipcar_faixas <- readr::read_csv(file.path(pasta_04, "04_03_INDICES_MENSAIS_FAIXA.csv"), show_col_types = FALSE) |>
  mutate(FAIXA_LABEL = as.character(.data$FAIXA_LABEL),
         PERIODO = as.character(.data$PERIODO),
         DATA = as.Date(paste0(.data$PERIODO, "01"), "%Y%m%d"))

if (!setequal(unique(ipcar_faixas$FAIXA_LABEL), faixas) ||
    nrow(ipcar_faixas) != 7 * 72) stop("Indices mensais da Etapa 04 incompletos.")

ipcar_geral <- ipcar_faixas |>
  left_join(cesta_faixas, by = "FAIXA_LABEL") |>
  group_by(.data$PERIODO, .data$DATA) |>
  summarise(INDICE = sum(.data$INDICE_DEZ2019_100 * .data$PESO_FAIXA),
            .groups = "drop") |>
  arrange(.data$PERIODO) |>
  mutate(INFLACAO_ACUM_PCT = .data$INDICE - 100,
         ANO = as.integer(substr(.data$PERIODO, 1, 4)))

ipcar_11_faixa <- readr::read_csv(file.path(pasta_04, "04_06_INDICES_11_TIPOS_MENSAIS.csv"), show_col_types = FALSE) |>
  mutate(FAIXA_LABEL = as.character(.data$FAIXA_LABEL),
         PERIODO = as.character(.data$PERIODO),
         GRUPO = unname(nomes_grupos[substr(.data$TIPO_POF_11, 1, 2)]))
if (anyNA(ipcar_11_faixa$GRUPO) || nrow(ipcar_11_faixa) != 7 * 11 * 72)
  stop("Indices dos 11 grupos da Etapa 04 incompletos.")

# Dentro de cada grupo, a media geral pondera cada faixa pelo gasto total
# expandido NAQUELE grupo: os grupos tem representatividade diferente.
ipcar_11_geral <- ipcar_11_faixa |>
  left_join(gastos |> select(.data$FAIXA_LABEL, .data$GRUPO, .data$DESPESA_EXPANDIDA),
            by = c("FAIXA_LABEL", "GRUPO")) |>
  group_by(.data$GRUPO, .data$PERIODO) |>
  summarise(INDICE = sum(.data$INDICE_TIPO * .data$DESPESA_EXPANDIDA) /
                      sum(.data$DESPESA_EXPANDIDA), .groups = "drop") |>
  mutate(ANO = as.integer(substr(.data$PERIODO, 1, 4)),
         ACUM_PCT = .data$INDICE - 100)

# ---- 3. LEITURA DO IPCA OFICIAL RJ, NAS DUAS ESTRUTURAS DE EXCEL --------------
# 2020-2024: sheet 1, meses na linha 4, categorias na linha 5, RJ na linha 19.
# 2025: sheet 'Tabela', meses na linha 3, regioes na linha 4, medidas na
#       linha 5, categorias nas linhas 6 em diante.
# O codigo encontra esses elementos pelo texto, em vez de confiar em letras
# de coluna predefinidas; o formato exportado pelo SIDRA mudou em 2025.

preencher_direita <- function(x) {
  y <- as.character(x)
  ultimo <- NA_character_
  for (j in seq_along(y)) {
    if (!is.na(y[j]) && nzchar(trimws(y[j]))) ultimo <- y[j]
    y[j] <- ultimo
  }
  y
}
extrair_mes <- function(x, ano) {
  pref <- tolower(substr(trimws(as.character(x)), 1, 3))
  nomes <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  n <- match(pref, nomes)
  ifelse(is.na(n), NA_character_, sprintf("%d%02d", ano, n))
}
cat_cod <- function(x) {
  x <- as.character(x)
  ifelse(grepl("^Índice geral$", x), "GERAL",
         ifelse(grepl("^[1-9]\\.", x), sub("\\..*$", "", x), NA_character_))
}

ler_ano_ipca <- function(ano) {
  arq <- file.path(pasta_ipca, sprintf("IPCA (%d).xlsx", ano))
  aba <- if (ano == 2025) "Tabela" else 1
  tab <- as.matrix(readxl::read_excel(arq, sheet = aba, col_names = FALSE,
                                     col_types = "text", .name_repair = "minimal"))
  if (ano < 2025) {
    linha_rj <- which(grepl("^Rio de Janeiro \\(RJ\\)$", as.character(tab[, 1])))
    if (length(linha_rj) != 1) stop("Nao achei a linha RJ na planilha ", ano)
    mes <- extrair_mes(preencher_direita(tab[4, ]), ano)
    cod <- cat_cod(tab[5, ])
    col <- which(!is.na(mes) & !is.na(cod))
    dado <- tibble(PERIODO = mes[col], CODIGO = cod[col],
                   VARIACAO_PCT = limpo_num(tab[linha_rj, col]))
  } else {
    mes <- extrair_mes(preencher_direita(tab[3, ]), ano)
    regiao <- preencher_direita(tab[4, ])
    medida <- as.character(tab[5, ])
    col <- which(!is.na(mes) & regiao == "Rio de Janeiro (RJ)" &
                 medida == "IPCA - Variação mensal (%)")
    linhas <- which(!is.na(cat_cod(tab[, 1])))
    lista <- lapply(col, function(j) tibble(
      PERIODO = rep(mes[j], length(linhas)),
      CODIGO = cat_cod(tab[linhas, 1]),
      VARIACAO_PCT = limpo_num(tab[linhas, j])
    ))
    dado <- bind_rows(lista)
  }
  dado <- dado |> filter(!is.na(.data$VARIACAO_PCT)) |>
    distinct(.data$PERIODO, .data$CODIGO, .keep_all = TRUE)
  if (nrow(dado) != 12 * 10 ||
      !setequal(unique(dado$CODIGO), c("GERAL", as.character(1:9))))
    stop("Planilha ", ano, " nao gerou as 12 variacoes para o indice geral e 9 grupos. Verifique se e a tabela 7060 e se contem 12 meses.")
  dado
}

message("Extraindo o IPCA regional dos 6 arquivos Excel do IBGE...")
ipca_mensal <- bind_rows(lapply(2020:2025, ler_ano_ipca)) |>
  arrange(.data$CODIGO, .data$PERIODO) |>
  group_by(.data$CODIGO) |>
  mutate(INDICE = 100 * cumprod(1 + .data$VARIACAO_PCT / 100),
         ACUM_PCT = .data$INDICE - 100) |>
  ungroup()
if (nrow(ipca_mensal) != 720) stop("A serie IPCA regional nao contem 72 meses e 10 categorias.")

ipca_geral <- ipca_mensal |> filter(.data$CODIGO == "GERAL") |>
  transmute(PERIODO = .data$PERIODO, DATA = as.Date(paste0(.data$PERIODO, "01"), "%Y%m%d"),
            INDICE_IPCA = .data$INDICE, ACUM_PCT_IPCA = .data$ACUM_PCT)

# Fonte e verificacao: diferenca de arredondamento acumulado de taxas mensais
# divulgadas com duas casas decimais pode ocorrer.
comparacao <- ipcar_geral |>
  left_join(ipca_geral, by = c("PERIODO", "DATA")) |>
  rename(INDICE_IPCAR = .data$INDICE, ACUM_PCT_IPCAR = .data$INFLACAO_ACUM_PCT) |>
  mutate(DIF_PONTOS_PCT = .data$ACUM_PCT_IPCAR - .data$ACUM_PCT_IPCA)
write_csv(comparacao, file.path(saida, "05_00_IPCAR_GERAL_VS_IPCA_RJ_MENSAL.csv"))
write_csv(ipca_mensal, file.path(saida, "05_00_IPCA_OFICIAL_RJ_10_CATEGORIAS.csv"))

# ---- 4. FUNCOES PARA TAXAS ACUMULADAS E ANUAIS -------------------------------
serie_dezembro <- function(df, nome_indice = "INDICE") {
  df |>
    filter(substr(.data$PERIODO, 5, 6) == "12") |>
    mutate(ANO = as.integer(substr(.data$PERIODO, 1, 4))) |>
    arrange(.data$ANO)
}
geral_dez <- serie_dezembro(comparacao) |>
  mutate(ACUM_IPCAR = .data$INDICE_IPCAR - 100,
         ACUM_IPCA = .data$INDICE_IPCA - 100,
         ANUAL_IPCAR = 100 * (.data$INDICE_IPCAR /
                    dplyr::lag(.data$INDICE_IPCAR, default = 100) - 1),
         ANUAL_IPCA = 100 * (.data$INDICE_IPCA /
                    dplyr::lag(.data$INDICE_IPCA, default = 100) - 1))

# 11 grupos - acumulada desde dez/2019 e anual calendario
onze_dez <- ipcar_11_geral |>
  filter(substr(.data$PERIODO, 5, 6) == "12") |>
  group_by(.data$GRUPO) |>
  arrange(.data$ANO, .by_group = TRUE) |>
  mutate(ANUAL_PCT = 100 * (.data$INDICE /
          dplyr::lag(.data$INDICE, default = 100) - 1)) |>
  ungroup()

# ---- 5. GRAFICO 1 - IPCAR GERAL VS IPCA OFICIAL RJ ----------------------------
tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(12, 24, 12, 12),
        panel.grid.minor = element_blank())
comparacao_longa <- comparacao |>
  select(.data$DATA, .data$ACUM_PCT_IPCAR, .data$ACUM_PCT_IPCA) |>
  pivot_longer(-.data$DATA, names_to = "SERIE", values_to = "INFLACAO") |>
  mutate(SERIE = dplyr::recode(.data$SERIE,
      "ACUM_PCT_IPCAR" = "IPCAR – RJ (geral)",
      "ACUM_PCT_IPCA" = "IPCA – RJ (oficial)"))

p1 <- ggplot(comparacao_longa, aes(x = .data$DATA, y = .data$INFLACAO, color = .data$SERIE)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("IPCAR – RJ (geral)" = cor_ipcar,
                                "IPCA – RJ (oficial)" = cor_ipca)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPCAR e IPCA regional do Rio de Janeiro | 2020–2025",
       subtitle = "Inflação acumulada desde dezembro de 2019 · base 100",
       x = "Ano", y = "Variação acumulada (%)") + tema

ggsave(file.path(saida, "05_01_IPCAR_GERAL_VS_IPCA_OFICIAL_RJ.png"), p1,
       width = 12.5, height = 7, dpi = 190, bg = "white")

# Opcional: as 7 classes frente ao IPCA geral (mantido separado para evitar
# sobrecarga no grafico principal).
p1b_df <- bind_rows(
  ipcar_faixas |>
    transmute(DATA = .data$DATA, SERIE = .data$FAIXA_LABEL,
              INFLACAO = .data$INFLACAO_ACUMULADA_PCT),
  ipca_geral |>
    transmute(DATA = .data$DATA, SERIE = "IPCA – RJ (oficial)",
              INFLACAO = .data$ACUM_PCT_IPCA))

p1b <- ggplot(p1b_df, aes(.data$DATA, .data$INFLACAO, color = .data$SERIE)) +
  geom_line(linewidth = 0.85) +
  scale_color_manual(values = c(cores_faixas, "IPCA – RJ (oficial)" = "#222222")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPCAR por faixa de renda e IPCA regional | 2020–2025",
       subtitle = "Sete cestas da POF 2017–2018 e referência oficial",
       x = "Ano", y = "Inflação acumulada (%)") + tema +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
ggsave(file.path(saida, "05_01B_FAIXAS_IPCAR_VS_IPCA_RJ.png"), p1b,
       width = 13, height = 8, dpi = 190, bg = "white")

# ---- 6. GRAFICO 2 - IPCAR E OS DOIS SUBITENS EXTREMOS -------------------------
# So usa subitens com variacao propria observada em TODOS os 72 meses.
# Nao inclui indices imputados de item/subgrupo/grupo nessa selecao.
precos <- readr::read_csv(file.path(pasta_04, "04_01_VARIACOES_MENSAIS_POR_SUBITEM.csv"),
                           col_types = cols(COD_SNIPC = col_character(),
                                            PERIODO = col_character(),
                                            NIVEL_PRECO = col_character(),
                                            VARIACAO_MENSAL_PCT = col_double())) |>
  mutate(COD_SNIPC = str_pad(.data$COD_SNIPC, 8, pad = "0"))

extremos <- precos |>
  group_by(.data$COD_SNIPC) |>
  filter(n() == 72, all(.data$NIVEL_PRECO == "SUBITEM"),
         all(is.finite(.data$VARIACAO_MENSAL_PCT))) |>
  arrange(.data$PERIODO, .by_group = TRUE) |>
  mutate(INDICE_SUBITEM = 100 * cumprod(1 + .data$VARIACAO_MENSAL_PCT / 100),
         ACUMULADO = .data$INDICE_SUBITEM - 100) |>
  ungroup()
fim_extremos <- extremos |> filter(.data$PERIODO == "202512") |>
  arrange(.data$ACUMULADO)
if (nrow(fim_extremos) < 2) stop("Nao ha dois subitens com 72 variacoes proprias para o grafico 2.")
ids_ex <- c(fim_extremos$COD_SNIPC[1], dplyr::last(fim_extremos$COD_SNIPC))

mapa_sidra <- read_csv(file.path(pasta_04, "04_00_MAPA_CODIGOS_SNIPC_IDS_SIDRA.csv"),
                       col_types = cols(.default = col_character())) |>
  mutate(COD_SNIPC = str_pad(.data$CODIGO_SNIPC_SIDRA, 8, pad = "0"),
         NOME = sub("^[0-9]+\\.", "", .data$DESCRICAO)) |>
  distinct(.data$COD_SNIPC, .keep_all = TRUE)
nome_ext <- mapa_sidra$NOME[match(ids_ex, mapa_sidra$COD_SNIPC)]
nome_ext[is.na(nome_ext)] <- ids_ex[is.na(nome_ext)]
rotulos_ext <- c(paste0("Menor: ", nome_ext[1]), paste0("Maior: ", nome_ext[2]))

p2df <- bind_rows(
  ipcar_geral |>
    transmute(DATA = .data$DATA, SERIE = "IPCAR – RJ (geral)",
              INFLACAO = .data$INFLACAO_ACUM_PCT),
  extremos |>
    filter(.data$COD_SNIPC %in% ids_ex) |>
    transmute(DATA = as.Date(paste0(.data$PERIODO, "01"), "%Y%m%d"),
              SERIE = ifelse(.data$COD_SNIPC == ids_ex[1], rotulos_ext[1], rotulos_ext[2]),
              INFLACAO = .data$ACUMULADO)
)
cores_ext <- stats::setNames(c(cor_ipcar, cor_min, cor_max),
                    c("IPCAR – RJ (geral)", rotulos_ext))
p2 <- ggplot(p2df, aes(.data$DATA, .data$INFLACAO, color = .data$SERIE)) +
  geom_line(linewidth = 1.15) +
  scale_color_manual(values = cores_ext) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPCAR e subitens de maior e menor inflação | 2020–2025",
       subtitle = "Extremos entre subitens com série própria completa de 72 meses no RJ",
       x = "Ano", y = "Inflação acumulada (%)") + tema

ggsave(file.path(saida, "05_02_IPCAR_E_SUBITENS_EXTREMOS.png"), p2,
       width = 13, height = 7.5, dpi = 190, bg = "white")
write_csv(fim_extremos |> filter(.data$COD_SNIPC %in% ids_ex) |>
            left_join(mapa_sidra |> select(.data$COD_SNIPC, .data$NOME), by = "COD_SNIPC") |>
            select(.data$COD_SNIPC, .data$NOME, INFLACAO_2020_2025_PCT = .data$ACUMULADO),
          file.path(saida, "05_02_SUBITENS_EXTREMOS_SELECIONADOS.csv"))

# ---- 6B. GRAFICO 2B - IPCAR E OS DOIS GRUPOS EXTREMOS -----------------------
# NOVO: seleciona os grupos de menor e maior inflacao acumulada 2020-2025
# entre os 11 tipos de despesa da POF (nao os 9 grupos oficiais do IPCA).
# Mantem INALTERADO o grafico anterior, que compara subitens individuais.
# Usa as series mensais de indice de grupo ja montadas acima; nao precisa
# consultar o SIDRA novamente, nem recalcular pesos das etapas 03 e 04.

ranking_grupos <- onze_dez |>
  filter(.data$ANO == 2025, is.finite(.data$ACUM_PCT)) |>
  arrange(.data$ACUM_PCT)
if (nrow(ranking_grupos) != 11L) {
  stop("Nao foi possivel selecionar extremos: esperados 11 grupos no fechamento de 2025.")
}
grupo_min <- as.character(ranking_grupos$GRUPO[1])
grupo_max <- as.character(ranking_grupos$GRUPO[nrow(ranking_grupos)])
if (identical(grupo_min, grupo_max)) stop("Maior e menor grupo coincidem.")

rotulo_min <- paste0("Menor: ", grupo_min)
rotulo_max <- paste0("Maior: ", grupo_max)
rotulo_ipcar <- "IPCAR – RJ (geral)"

p2b_df <- bind_rows(
  ipcar_geral |>
    transmute(DATA = .data$DATA,
              SERIE = rotulo_ipcar,
              INFLACAO = .data$INFLACAO_ACUM_PCT),
  ipcar_11_geral |>
    filter(.data$GRUPO %in% c(grupo_min, grupo_max)) |>
    transmute(DATA = as.Date(paste0(.data$PERIODO, "01"), "%Y%m%d"),
              SERIE = if_else(.data$GRUPO == grupo_min, rotulo_min, rotulo_max),
              INFLACAO = .data$ACUM_PCT)
)

if (nrow(p2b_df) != 3L * 72L) {
  stop("Grafico 2B incompleto: esperadas 72 observacoes para cada uma das tres series.")
}
cores_grupos_extremos <- stats::setNames(
  c(cor_ipcar, cor_min, cor_max),
  c(rotulo_ipcar, rotulo_min, rotulo_max)
)

p2b <- ggplot(p2b_df, aes(x = .data$DATA, y = .data$INFLACAO,
                          color = .data$SERIE)) +
  geom_hline(yintercept = 0, color = "#A3A3A3", linewidth = 0.35) +
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = cores_grupos_extremos, breaks = names(cores_grupos_extremos)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPCAR e grupos de maior e menor inflação | 2020–2025",
       subtitle = "Grupos extremos dentre os 11 tipos de despesa da POF · base dez/2019 = 100",
       x = "Ano", y = "Inflação acumulada (%)") + tema +
  theme(legend.position = "bottom", legend.text = element_text(size = 11))

ggsave(file.path(saida, "05_02B_IPCAR_E_GRUPOS_EXTREMOS.png"), p2b,
       width = 13, height = 7.5, dpi = 190, bg = "white")

write_csv(
  ranking_grupos |>
    filter(.data$GRUPO %in% c(grupo_min, grupo_max)) |>
    mutate(CLASSIFICACAO = if_else(.data$GRUPO == grupo_min, "Menor", "Maior")) |>
    transmute(CLASSIFICACAO = .data$CLASSIFICACAO,
              GRUPO_POF_11 = .data$GRUPO,
              INFLACAO_2020_2025_PCT = .data$ACUM_PCT),
  file.path(saida, "05_02B_GRUPOS_EXTREMOS_SELECIONADOS.csv")
)

# ---- 7. GRAFICO 3 - 11 GRUPOS POF, E 9 GRUPOS OFICIAIS EM ANEXO ---------------
# O IBGE divulga 9 grupos oficiais, nao 11. O grafico pedido com 11
# categorias e um reagrupamento proprio de subitens com pesos da POF.
g3dados <- onze_dez |>
  filter(.data$ANO == ano_foco) |>
  mutate(GRUPO = factor(.data$GRUPO, levels = rev(ordem_grupos)))

p3 <- ggplot(g3dados, aes(.data$GRUPO, .data$ANUAL_PCT, fill = .data$GRUPO)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = fmt_pct(.data$ANUAL_PCT)), hjust = ifelse(g3dados$ANUAL_PCT >= 0, -0.10, 1.10),
            size = 3.4) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = cores_grupos) +
  scale_y_continuous(expand = expansion(mult = c(.12, .16))) +
  labs(title = paste0("Inflação dos 11 grupos da POF | ", ano_foco),
       subtitle = "Estimativa IPCAR-RJ: variações do IPCA reagrupadas e ponderadas pela POF",
       x = NULL, y = "Inflação no ano (%)") + tema

ggsave(file.path(saida, "05_03_INFLACAO_11_GRUPOS_POF_ANO_FOCO.png"), p3,
       width = 12, height = 8, dpi = 190, bg = "white")

nomes_ipca9 <- c("1"="Alimentação e bebidas", "2"="Habitação",
                 "3"="Artigos de residência", "4"="Vestuário", "5"="Transportes",
                 "6"="Saúde e cuidados pessoais", "7"="Despesas pessoais",
                 "8"="Educação", "9"="Comunicação")

ipca9_ano <- ipca_mensal |>
  filter(.data$CODIGO != "GERAL", .data$PERIODO == paste0(ano_foco, "12")) |>
  left_join(ipca_mensal |>
     filter(.data$CODIGO != "GERAL", .data$PERIODO == paste0(ano_foco - 1, "12")) |>
     select(.data$CODIGO, INDICE_ANT = .data$INDICE), by = "CODIGO") |>
  mutate(INDICE_ANT = coalesce(.data$INDICE_ANT, 100),
         INFLACAO_ANUAL_PCT = 100 * (.data$INDICE / .data$INDICE_ANT - 1),
         GRUPO_OFICIAL = unname(nomes_ipca9[.data$CODIGO]),
         GRUPO_OFICIAL = factor(.data$GRUPO_OFICIAL, levels = rev(unname(nomes_ipca9))))
p3b <- ggplot(ipca9_ano, aes(.data$GRUPO_OFICIAL, .data$INFLACAO_ANUAL_PCT,
                              fill = .data$GRUPO_OFICIAL)) +
  geom_col(width = .72, show.legend = FALSE) +
  geom_text(aes(label = fmt_pct(.data$INFLACAO_ANUAL_PCT)),
            hjust = ifelse(ipca9_ano$INFLACAO_ANUAL_PCT >= 0, -.10, 1.10), size = 3.4) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = stats::setNames(grDevices::hcl.colors(9, "Dark 3"),
                         unname(nomes_ipca9))) +
  scale_y_continuous(expand = expansion(mult = c(.13, .15))) +
  labs(title = paste0("IPCA regional do RJ | 9 grupos oficiais | ", ano_foco),
       subtitle = "Variações divulgadas pelo IBGE (acumuladas das taxas mensais)",
       x = NULL, y = "Inflação no ano (%)") + tema
ggsave(file.path(saida, "05_03B_IPCA_RJ_9_GRUPOS_OFICIAIS.png"), p3b,
       width = 12, height = 7, dpi = 190, bg = "white")

# ---- 8. GRAFICO 4 - INFLACAO ACUMULADA DOS 11 GRUPOS AO LONGO DOS ANOS --------
# Em cada ano, acumulado desde dez/2019. Barras: 11 grupos. Linha pontilhada:
# IPCAR RJ geral naquele dezembro.
g4df <- onze_dez |>
  mutate(ANO = factor(.data$ANO, levels = 2020:2025),
         GRUPO = factor(.data$GRUPO, levels = rev(ordem_grupos)))
g4base <- geral_dez |>
  transmute(ANO = factor(.data$ANO, levels = 2020:2025),
            IPCAR = .data$ACUM_IPCAR)

p4 <- ggplot(g4df, aes(.data$GRUPO, .data$ACUM_PCT, fill = .data$GRUPO)) +
  geom_col(width = .76, show.legend = FALSE) +
  geom_hline(data = g4base, aes(yintercept = .data$IPCAR),
             color = cor_ipcar, linewidth = .75, linetype = "dashed", inherit.aes = FALSE) +
  coord_flip() +
  facet_wrap(~ ANO, ncol = 3) +
  scale_fill_manual(values = cores_grupos) +
  labs(title = "Inflação acumulada por grupo de despesa POF | 2020–2025",
       subtitle = "Acumulada desde dez/2019 · tracejado azul = IPCAR geral de cada dezembro",
       x = NULL, y = "Inflação acumulada (%)") + tema +
  theme(strip.text = element_text(face = "bold"), axis.text.y = element_text(size = 8))
ggsave(file.path(saida, "05_04_INFLACAO_ACUMULADA_11_GRUPOS_POR_ANO.png"), p4,
       width = 18, height = 13.3, dpi = 190, bg = "white")

# ---- 9. GRAFICO 5 - GRUPOS VS IPCAR E IPCA GERAIS NO PERIODO TODO -------------
ultimo <- geral_dez |> filter(.data$ANO == 2025)
p5dados <- onze_dez |>
  filter(.data$ANO == 2025) |>
  mutate(GRUPO = factor(.data$GRUPO, levels = rev(ordem_grupos)))
linhas5 <- tibble(SERIE = c("IPCAR geral", "IPCA oficial RJ"),
                  VALOR = c(ultimo$ACUM_IPCAR, ultimo$ACUM_IPCA))

p5 <- ggplot(p5dados, aes(.data$GRUPO, .data$ACUM_PCT, fill = .data$GRUPO)) +
  geom_col(width = .72, show.legend = FALSE) +
  geom_hline(data = linhas5, aes(yintercept = .data$VALOR, color = .data$SERIE),
             linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
  coord_flip() +
  scale_fill_manual(values = cores_grupos) +
  scale_color_manual(values = c("IPCAR geral" = cor_ipcar, "IPCA oficial RJ" = cor_ipca)) +
  labs(title = "Inflação dos 11 grupos POF vs IPCAR e IPCA | 2020–2025",
       subtitle = "Acumulada desde dez/2019 · barras reagrupadas · linhas = índices gerais",
       x = NULL, y = "Inflação acumulada (%)", color = "Referências") + tema +
  theme(legend.title = element_text(face = "bold"))
ggsave(file.path(saida, "05_05_GRUPOS_VS_IPCAR_E_IPCA_2020_2025.png"), p5,
       width = 12.8, height = 8.7, dpi = 190, bg = "white")

# ---- 10. TABELA 6 - MESMO MODELO DA TABELA 2 DO ARTIGO DE CURITIBA ------------
# TOTAL: IPCAR estadual (pesos de despesas da POF).
# Nas 11 linhas de grupo, a taxa e dada pelo indice daquele grupo, em cada
# faixa e no total, nao pela contribuicao (pontos percentuais) ao indice geral.
faixa_periodo_final <- ipcar_faixas |>
  filter(.data$PERIODO == "202512") |>
  transmute(FAIXA_LABEL = .data$FAIXA_LABEL,
            GRUPO = "IPCAR RJ (total)",
            INFLACAO_PCT = .data$INDICE_DEZ2019_100 - 100)

onze_faixas_fim <- ipcar_11_faixa |>
  filter(.data$PERIODO == "202512") |>
  transmute(FAIXA_LABEL = .data$FAIXA_LABEL,
            GRUPO = .data$GRUPO,
            INFLACAO_PCT = .data$INDICE_TIPO - 100)

onze_geral_fim <- onze_dez |>
  filter(.data$ANO == 2025) |>
  transmute(GRUPO = .data$GRUPO, TOTAL_RJ = .data$ACUM_PCT)

linhas_tabela <- c("IPCAR RJ (total)", ordem_grupos)
tabela <- bind_rows(faixa_periodo_final, onze_faixas_fim) |>
  mutate(FAIXA_LABEL = factor(.data$FAIXA_LABEL, levels = faixas)) |>
  pivot_wider(id_cols = .data$GRUPO, names_from = .data$FAIXA_LABEL,
              values_from = .data$INFLACAO_PCT) |>
  left_join(bind_rows(tibble(GRUPO = "IPCAR RJ (total)",
                               TOTAL_RJ = ultimo$ACUM_IPCAR), onze_geral_fim),
            by = "GRUPO") |>
  mutate(GRUPO = factor(.data$GRUPO, levels = linhas_tabela)) |>
  arrange(.data$GRUPO) |>
  mutate(GRUPO = as.character(.data$GRUPO)) |>
  select(.data$GRUPO, .data$TOTAL_RJ, all_of(faixas))

if (nrow(tabela) != 12 || anyNA(tabela)) stop("Tabela final incompleta.")
write_csv(tabela, file.path(saida, "05_06_TABELA_IPCAR_RJ_POR_FAIXA_E_GRUPO_2020_2025.csv"))

# Tabelas equivalentes PARA CADA ANO (inflacao anual, nao acumulada desde 2019).
lista_tabelas_ano <- list()
for (ano in 2020:2025) {
  periodo <- paste0(ano, "12")
  periodo_anterior <- paste0(ano - 1, "12")
  grupos_atual <- ipcar_11_faixa |> filter(.data$PERIODO == periodo)
  grupos_anterior <- ipcar_11_faixa |> filter(.data$PERIODO == periodo_anterior) |>
    select(.data$FAIXA_LABEL, .data$GRUPO, INDICE_ANT = .data$INDICE_TIPO)
  grupos_ano <- grupos_atual |>
    left_join(grupos_anterior, by = c("FAIXA_LABEL", "GRUPO")) |>
    mutate(INDICE_ANT = coalesce(.data$INDICE_ANT, 100),
           TAXA = 100 * (.data$INDICE_TIPO / .data$INDICE_ANT - 1)) |>
    select(.data$FAIXA_LABEL, .data$GRUPO, .data$TAXA)
  idx_faixas_ano <- ipcar_faixas |>
    filter(.data$PERIODO %in% c(periodo, periodo_anterior)) |>
    select(.data$FAIXA_LABEL, .data$PERIODO, .data$INDICE_DEZ2019_100) |>
    pivot_wider(names_from = .data$PERIODO, values_from = .data$INDICE_DEZ2019_100)
  atual <- as.character(idx_faixas_ano[[periodo]])
  anterior <- if (ano == 2020) rep(100, nrow(idx_faixas_ano)) else as.numeric(idx_faixas_ano[[periodo_anterior]])
  taxa_geral_faixa <- tibble(FAIXA_LABEL = idx_faixas_ano$FAIXA_LABEL,
                            GRUPO = "IPCAR RJ (total)",
                            TAXA = 100 * (as.numeric(atual) / anterior - 1))
  indice_geral_ano <- geral_dez |> filter(.data$ANO == ano)
  indices_11_ano <- onze_dez |>
    filter(.data$ANO == ano) |>
    transmute(GRUPO = .data$GRUPO, TOTAL_RJ = .data$ANUAL_PCT)
  tab_ano <- bind_rows(taxa_geral_faixa, grupos_ano) |>
    mutate(FAIXA_LABEL = factor(.data$FAIXA_LABEL, levels = faixas)) |>
    pivot_wider(id_cols = .data$GRUPO, names_from = .data$FAIXA_LABEL,
                values_from = .data$TAXA) |>
    left_join(bind_rows(tibble(GRUPO = "IPCAR RJ (total)",
                                 TOTAL_RJ = indice_geral_ano$ANUAL_IPCAR), indices_11_ano),
              by = "GRUPO") |>
    mutate(GRUPO = factor(.data$GRUPO, levels = linhas_tabela)) |>
    arrange(.data$GRUPO) |>
    mutate(GRUPO = as.character(.data$GRUPO)) |>
    select(.data$GRUPO, .data$TOTAL_RJ, all_of(faixas))
  if (nrow(tab_ano) != 12 || anyNA(tab_ano)) stop("Tabela anual incompleta: ", ano)
  write_csv(tab_ano, file.path(saida, paste0("05_06_TABELA_ANUAL_", ano, ".csv")))
  lista_tabelas_ano[[as.character(ano)]] <- tab_ano
}

# Uma planilha Excel com a tabela acumulada e as 6 tabelas anuais.
writexl::write_xlsx(c(list(`Acumulado 2020-2025` = tabela),
                     stats::setNames(lista_tabelas_ano, paste0("Ano ", names(lista_tabelas_ano)))),
                   path = file.path(saida, "05_06_TABELAS_IPCAR_RJ.xlsx"))

# Para a tabela ficar legivel inclusive sem Excel: uma versao em imagem.
tab_longa <- tabela |>
  pivot_longer(cols = -c(GRUPO, TOTAL_RJ),
               names_to = "FAIXA", values_to = "TAXA") |>
  bind_rows(tabela |>
     transmute(GRUPO = .data$GRUPO, FAIXA = "Total RJ", TAXA = .data$TOTAL_RJ)) |>
  mutate(GRUPO = factor(.data$GRUPO, levels = rev(linhas_tabela)),
         FAIXA = factor(.data$FAIXA, levels = c("Total RJ", faixas)))

p6 <- ggplot(tab_longa, aes(.data$FAIXA, .data$GRUPO, fill = .data$TAXA)) +
  geom_tile(color = "white", linewidth = .6) +
  geom_text(aes(label = formatC(.data$TAXA, digits = 1, format = "f", decimal.mark = ",")),
            size = 3.05) +
  scale_fill_gradient2(low = "#80B8CF", mid = "#F5F8FB", high = "#E4A078", midpoint = 0,
                       name = "Inflação (%)") +
  labs(title = "Inflação acumulada 2020–2025 | IPCAR-RJ",
       subtitle = "Por faixa de renda e grupo de despesas POF (no padrão da tabela de Curitiba)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text.y = element_text(size = 10), legend.position = "bottom")
ggsave(file.path(saida, "05_06_TABELA_VISUAL_IPCAR_FAIXA_E_GRUPO.png"), p6,
       width = 15, height = 8.3, dpi = 190, bg = "white")

# ---- 11. NOVOS GRAFICOS: EXTREMOS DE RENDA VS IPCA OFICIAL, POR 9 GRUPOS -----
# ATENCAO METODOLOGICA:
# Os 11 grupos da POF nao coincidem com os 9 grupos do IPCA publicados pelo IBGE.
# Para COMPARAR por grupo as faixas ate 2 SM e >25 SM ao IPCA oficial RJ,
# aqui reagrupamos os mesmos subitens das cestas IPCAR NOS 9 GRUPOS SNIPC.
# Os pesos relativos dentro de cada grupo continuam especificos por renda.
# Os dois IPCAR por grupo sao experimentais; a terceira serie e oficial.

arquivo_subitens_pof <- file.path(pasta_03, "03_01_PESOS_SUBITENS_POF_11.csv")
if (!file.exists(arquivo_subitens_pof)) stop("Falta o arquivo da Etapa 03: ", arquivo_subitens_pof)

legendas_extremos <- c("Até 2 SM", "Mais 25 SM", "IPCA oficial RJ")
cores_comparacao <- c("Até 2 SM" = "#087C83", "Mais 25 SM" = "#E09A35",
                     "IPCA oficial RJ" = "#334D75")

# Em vez de cadastrar as faixas manualmente, reaproveitamos a legenda de renda
# ja conferida nos dados do proprio Script 05.
tradutor_faixas <- gastos |>
  distinct(.data$FAIXA_RENDA, .data$FAIXA_LABEL)

pesos_subitens_pof <- readr::read_csv(
  arquivo_subitens_pof,
  col_types = readr::cols(COD_SNIPC = readr::col_character(),
                          .default = readr::col_guess()),
  show_col_types = FALSE
) |>
  mutate(COD_SNIPC = stringr::str_pad(.data$COD_SNIPC, 8, pad = "0"),
         CODIGO_GRUPO_IPCA = as.character(as.integer(substr(.data$COD_SNIPC, 1, 2))),
         GRUPO_IPCA = unname(nomes_ipca9[.data$CODIGO_GRUPO_IPCA]),
         GRUPO_POF = unname(nomes_grupos[substr(.data$TIPO_POF_11, 1, 2)])) |>
  left_join(tradutor_faixas, by = "FAIXA_RENDA")
if (anyNA(pesos_subitens_pof$GRUPO_IPCA) ||
    anyNA(pesos_subitens_pof$GRUPO_POF) ||
    anyNA(pesos_subitens_pof$FAIXA_LABEL))
  stop("Existem grupos ou faixas nao identificados nos pesos por subitem da Etapa 03.")

# Mesmos precos do Script 04 (inclusive as aproximacoes hierarquicas documentadas).
if (anyNA(precos$VARIACAO_MENSAL_PCT) ||
    any(!is.finite(precos$VARIACAO_MENSAL_PCT)) ||
    nrow(precos) != 72L * dplyr::n_distinct(precos$COD_SNIPC))
  stop("Serie de variacoes de subitens da Etapa 04 incompleta ou invalida.")

indices_subitens <- precos |>
  group_by(.data$COD_SNIPC) |>
  arrange(.data$PERIODO, .by_group = TRUE) |>
  mutate(INDICE_PRECO = 100 * cumprod(1 + .data$VARIACAO_MENSAL_PCT / 100)) |>
  ungroup() |>
  select(.data$COD_SNIPC, .data$PERIODO, .data$INDICE_PRECO)

if (!setequal(unique(pesos_subitens_pof$COD_SNIPC), unique(indices_subitens$COD_SNIPC)))
  stop("Os codigos dos pesos POF nao correspondem aos codigos de precos da Etapa 04.")

# IPCAR de cada faixa no nivel dos NOVE grupos comparaveis ao IPCA.
# Importante: os pesos de um grupo sao normalizados DENTRO do grupo;
# portanto calculamos inflacao DO GRUPO, nao contribuicao para o total.
indices_9_por_renda <- pesos_subitens_pof |>
  group_by(.data$FAIXA_LABEL, .data$GRUPO_IPCA, .data$COD_SNIPC) |>
  summarise(PESO_SUBITEM_PCT = sum(.data$PESO_SUBITEM_PCT), .groups = "drop") |>
  group_by(.data$FAIXA_LABEL, .data$GRUPO_IPCA) |>
  mutate(PESO_NO_GRUPO = .data$PESO_SUBITEM_PCT / sum(.data$PESO_SUBITEM_PCT)) |>
  ungroup() |>
  inner_join(indices_subitens, by = "COD_SNIPC") |>
  group_by(.data$FAIXA_LABEL, .data$GRUPO_IPCA, .data$PERIODO) |>
  summarise(INDICE = sum(.data$PESO_NO_GRUPO * .data$INDICE_PRECO), .groups = "drop")

if (nrow(indices_9_por_renda) != 7L * 9L * 72L ||
    any(!is.finite(indices_9_por_renda$INDICE)))
  stop("Indices IPCAR por 9 grupos incompletos.")

# Series para as duas rendas extremas + IPCA oficial dos mesmos nove grupos.
comparacao_9_grupos <- bind_rows(
  indices_9_por_renda |>
    filter(.data$FAIXA_LABEL %in% legendas_extremos[1:2]) |>
    transmute(GRUPO = .data$GRUPO_IPCA, PERIODO = .data$PERIODO,
              SERIE = .data$FAIXA_LABEL, INDICE = .data$INDICE),
  ipca_mensal |>
    filter(.data$CODIGO != "GERAL") |>
    transmute(GRUPO = unname(nomes_ipca9[.data$CODIGO]),
              PERIODO = .data$PERIODO,
              SERIE = "IPCA oficial RJ", INDICE = .data$INDICE)
) |>
  mutate(SERIE = factor(.data$SERIE, levels = legendas_extremos))

if (nrow(comparacao_9_grupos) != 3L * 9L * 72L || anyNA(comparacao_9_grupos))
  stop("As tres series comparaveis por grupo nao estao completas.")

comparacao_9_dez <- comparacao_9_grupos |>
  filter(substr(.data$PERIODO, 5, 6) == "12") |>
  group_by(.data$GRUPO, .data$SERIE) |>
  arrange(.data$PERIODO, .by_group = TRUE) |>
  mutate(ANO = as.integer(substr(.data$PERIODO, 1, 4)),
         INFLACAO_ACUM_PCT = .data$INDICE - 100,
         INFLACAO_ANUAL_PCT = 100 * (.data$INDICE /
                               dplyr::lag(.data$INDICE, default = 100) - 1)) |>
  ungroup() |>
  mutate(GRUPO = factor(.data$GRUPO, levels = rev(unname(nomes_ipca9))))

readr::write_csv(comparacao_9_dez,
                 file.path(saida, "05_07_E_08_COMPARACAO_EXTREMOS_9_GRUPOS.csv"))

# GRAFICO 07: acumulado em 2020-2025, tres colunas por grupo.
g7 <- ggplot(comparacao_9_dez |> filter(.data$ANO == 2025),
             aes(x = .data$GRUPO, y = .data$INFLACAO_ACUM_PCT,
                 fill = .data$SERIE)) +
  geom_hline(yintercept = 0, color = "#8E969E", linewidth = .35) +
  geom_col(position = position_dodge(width = .78), width = .72) +
  coord_flip() +
  scale_fill_manual(values = cores_comparacao, breaks = legendas_extremos) +
  scale_y_continuous(expand = expansion(mult = c(.08, .13))) +
  labs(title = "Rio de Janeiro | Inflação acumulada por grupo e renda",
       subtitle = "2020–2025 · IPCAR das faixas extremas × IPCA regional oficial · nove grupos SNIPC",
       x = NULL, y = "Inflação acumulada (%)", fill = "Série") + tema +
  theme(axis.text.y = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave(file.path(saida, "05_07_EXTREMOS_RENDA_VS_IPCA_9_GRUPOS_ACUMULADO.png"),
       g7, width = 13.6, height = 8.5, dpi = 190, bg = "white")

# GRAFICO 08: taxa do ano civil, com um painel para cada ano (2020 a 2025).
g8 <- ggplot(comparacao_9_dez,
             aes(x = .data$GRUPO, y = .data$INFLACAO_ANUAL_PCT,
                 fill = .data$SERIE)) +
  geom_hline(yintercept = 0, color = "#A0A8AF", linewidth = .3) +
  geom_col(position = position_dodge(width = .80), width = .74) +
  coord_flip() +
  facet_wrap(~ ANO, ncol = 2) +
  scale_fill_manual(values = cores_comparacao, breaks = legendas_extremos) +
  scale_y_continuous(expand = expansion(mult = c(.07, .12))) +
  labs(title = "Rio de Janeiro | Inflação anual dos grupos por renda",
       subtitle = "Até 2 SM × mais de 25 SM × IPCA regional oficial | cada painel corresponde ao ano civil",
       x = NULL, y = "Inflação anual (%)", fill = "Série") + tema +
  theme(axis.text.y = element_text(size = 9.2),
        strip.text = element_text(face = "bold", size = 12),
        panel.spacing = grid::unit(0.9, "cm"))
ggsave(file.path(saida, "05_08_EXTREMOS_RENDA_VS_IPCA_9_GRUPOS_ANO_A_ANO.png"),
       g8, width = 17.8, height = 16.5, dpi = 190, bg = "white")

# ---- 12. GRAFICO EXTRA 09 - CONTRIBUICAO DOS 11 GRUPOS PARA A DIFERENCA ------
# Diferente da taxa de inflacao de um grupo, a contribuicao considera seu PESO
# na cesta total. Em p.p., para cada faixa r e grupo g:
#   contrib(r,g) = soma_i [peso_base_i_em_pontos_percentuais *
#                         (indice_precos_i / 100 - 1)]
# A soma das contribuicoes dos grupos equivale a inflacao total da faixa.
# A diferenca (ate 2 SM - >25 SM) mostra quais grupos aproximam/afastam os IPCAR.
contribuicoes_extremos <- pesos_subitens_pof |>
  filter(.data$FAIXA_LABEL %in% legendas_extremos[1:2]) |>
  inner_join(indices_subitens |>
               filter(.data$PERIODO == "202512") |>
               select(.data$COD_SNIPC, .data$INDICE_PRECO),
             by = "COD_SNIPC") |>
  group_by(.data$GRUPO_POF, .data$FAIXA_LABEL) |>
  summarise(CONTRIB_PONTOS_PCT = sum(.data$PESO_SUBITEM_PCT *
                                    (.data$INDICE_PRECO / 100 - 1)),
            .groups = "drop") |>
  tidyr::pivot_wider(names_from = .data$FAIXA_LABEL,
                     values_from = .data$CONTRIB_PONTOS_PCT) |>
  mutate(DIFERENCA_PP = .data[["Até 2 SM"]] - .data[["Mais 25 SM"]]) |>
  arrange(desc(.data$DIFERENCA_PP))

# Verificacao matematica contra os indices ja produzidos no Script 04.
valores_2025_extremos <- ipcar_faixas |>
  filter(.data$PERIODO == "202512", .data$FAIXA_LABEL %in% legendas_extremos[1:2])
dif_total_esperada <- with(valores_2025_extremos,
                          INDICE_DEZ2019_100[FAIXA_LABEL == "Até 2 SM"] -
                          INDICE_DEZ2019_100[FAIXA_LABEL == "Mais 25 SM"])
if (length(dif_total_esperada) != 1L ||
    abs(sum(contribuicoes_extremos$DIFERENCA_PP) - dif_total_esperada) > 1e-5)
  stop("A decomposicao da diferenca de inflacao nao fechou com os indices do Script 04.")

readr::write_csv(contribuicoes_extremos,
                 file.path(saida, "05_09_CONTRIBUICOES_DIFERENCA_EXTREMOS_11_GRUPOS.csv"))

contrib_plot <- contribuicoes_extremos |>
  mutate(GRUPO = factor(.data$GRUPO_POF,
                        levels = .data$GRUPO_POF[order(.data$DIFERENCA_PP)]),
         SENTIDO = if_else(.data$DIFERENCA_PP >= 0,
                           "Eleva relativamente a inflação até 2 SM",
                           "Eleva relativamente a inflação acima de 25 SM"))
cores_contrib <- c("Eleva relativamente a inflação até 2 SM" = "#B94E3B",
                   "Eleva relativamente a inflação acima de 25 SM" = "#276E99")

g9 <- ggplot(contrib_plot,
             aes(x = .data$GRUPO, y = .data$DIFERENCA_PP, fill = .data$SENTIDO)) +
  geom_hline(yintercept = 0, color = "#444444", linewidth = .65) +
  geom_col(width = .70) +
  coord_flip() +
  scale_fill_manual(values = cores_contrib) +
  scale_y_continuous(expand = expansion(mult = c(.12, .13))) +
  labs(title = "O que explica a diferença de inflação entre as faixas extremas?",
       subtitle = sprintf("Contribuições por grupo POF | 2020–2025 · diferença total = %+.2f p.p.",
                          dif_total_esperada),
       caption = "Contribuição = peso inicial do grupo e evolução de seus preços; barras são diferenças em pontos percentuais, não taxas de inflação.",
       x = NULL, y = "Contribuição até 2 SM menos contribuição mais de 25 SM (p.p.)",
       fill = "Sentido da contribuição") + tema +
  theme(axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.caption = element_text(size = 9))
ggsave(file.path(saida, "05_09_CONTRIBUICAO_GRUPOS_DIFERENCA_EXTREMOS.png"),
       g9, width = 14.6, height = 9, dpi = 190, bg = "white")

# ---- 13. GRAFICO EXTRA 10 - MAPA DE CALOR DAS DIFERENCAS ANUAIS --------------
# Cada celula e a taxa anual DO GRUPO na faixa ate 2 SM menos a mesma taxa
# na faixa acima de 25 SM (pontos percentuais). NAO e contribuicao ao IPCAR.
dif_ano_grupos <- ipcar_11_faixa |>
  filter(.data$FAIXA_LABEL %in% legendas_extremos[1:2],
         substr(.data$PERIODO, 5, 6) == "12") |>
  group_by(.data$FAIXA_LABEL, .data$GRUPO) |>
  arrange(.data$PERIODO, .by_group = TRUE) |>
  mutate(ANO = as.integer(substr(.data$PERIODO, 1, 4)),
         TAXA_ANO_PCT = 100 * (.data$INDICE_TIPO /
                         dplyr::lag(.data$INDICE_TIPO, default = 100) - 1)) |>
  ungroup() |>
  select(.data$GRUPO, .data$ANO, .data$FAIXA_LABEL, .data$TAXA_ANO_PCT) |>
  pivot_wider(names_from = .data$FAIXA_LABEL, values_from = .data$TAXA_ANO_PCT) |>
  mutate(DIFERENCA_PP = .data[["Até 2 SM"]] - .data[["Mais 25 SM"]]) |>
  arrange(.data$GRUPO, .data$ANO)
if (nrow(dif_ano_grupos) != 11L * 6L || anyNA(dif_ano_grupos$DIFERENCA_PP))
  stop("A serie de diferencas anuais por grupo POF esta incompleta.")
readr::write_csv(dif_ano_grupos,
                 file.path(saida, "05_10_DIFERENCA_INFLACAO_ANUAL_11_GRUPOS.csv"))

p10 <- ggplot(dif_ano_grupos |>
                mutate(GRUPO = factor(.data$GRUPO, levels = rev(ordem_grupos)),
                       ANO = factor(.data$ANO, levels = 2020:2025),
                       ROTULO = gsub("\\.", ",", sprintf("%+.1f", .data$DIFERENCA_PP)),
                       COR_ROTULO = if_else(abs(.data$DIFERENCA_PP) >=
                                               0.55 * max(abs(.data$DIFERENCA_PP)),
                                             "#FFFFFF", "#202B35")),
              aes(x = .data$ANO, y = .data$GRUPO, fill = .data$DIFERENCA_PP)) +
  geom_tile(color = "white", linewidth = 1.15) +
  geom_text(aes(label = .data$ROTULO, color = .data$COR_ROTULO), size = 3.25) +
  scale_color_identity() +
  scale_fill_gradient2(low = "#2F76A5", mid = "#F9FAFB", high = "#C85540",
                       midpoint = 0, name = "Diferença (p.p.)") +
  labs(title = "Quem teve maior inflação em cada grupo, ano a ano?",
       subtitle = "IPCAR até 2 SM menos IPCAR acima de 25 SM · vermelho: maior na baixa renda; azul: maior na alta renda",
       caption = "Diferenças de taxas de inflação dos grupos POF, não contribuições ao índice geral.",
       x = "Ano", y = NULL) + tema +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 10),
        legend.position = "right", plot.caption = element_text(size = 9))
ggsave(file.path(saida, "05_10_MAPA_CALOR_DIFERENCAS_ANUAIS_11_GRUPOS.png"),
       p10, width = 14.2, height = 8.9, dpi = 190, bg = "white")

# ---- 14. RELATORIO DE VALIDACAO E LIMITACOES ---------------------------------
taxa_2025 <- geral_dez |> filter(.data$ANO == 2025)
relatorio <- c(
  "SCRIPT 05 - IPCAR / IPCA - RJ - 2020-2025",
  "",
  sprintf("IPCAR geral 2020-2025: %.4f%%", ultimo$ACUM_IPCAR),
  sprintf("IPCA oficial RJ 2020-2025 (produto das variacoes mensais): %.4f%%", ultimo$ACUM_IPCA),
  sprintf("IPCAR RJ no ano 2025: %.4f%%", taxa_2025$ANUAL_IPCAR),
  sprintf("IPCA RJ no ano 2025 (produto das variacoes mensais): %.4f%%", taxa_2025$ANUAL_IPCA),
  "",
  paste("Extremo de menor variacao:", nome_ext[1], ids_ex[1]),
  paste("Extremo de maior variacao:", nome_ext[2], ids_ex[2]),
  paste("Grupo POF com menor inflacao:", grupo_min),
  paste("Grupo POF com maior inflacao:", grupo_max),
  "",
  "METODOLOGIA:",
  "1. IPCAR geral pondera os 7 IPCAR por gastos expandidos de cada faixa (POF).",
  "2. Inflacao dos 11 grupos POF usa os subitens IPCA reagrupados e ponderados pela POF.",
  "3. Somente os 9 grupos publicados pelo IBGE sao indicadores oficiais de grupo.",
  "4. Extremos do grafico 02: somente subitens com variacao propria nos 72 meses, sem fallback.",
  "4B. Extremos do grafico 02B: grupos POF de menor e maior inflacao acumulada em 2025.",
  "5. Indices base: dezembro de 2019 = 100; taxa anual compara cada dezembro com o anterior.",
  "6. IPCA oficial recuperado dos arquivos Excel 2020-2025; pequena diferenca de",
  "   arredondamento frente ao acumulado oficial pode surgir ao compor taxas mensais.",
  "7. Precos IPCA da area regional Rio aplicados a despesas POF do estado inteiro.",
  "8. O IPCAR reusa os subitens aproximados no Script 04 quando nao havia cotacao propria.",
  "9. Graficos 07 e 08 reagrupam os subitens IPCAR em 9 grupos SNIPC para",
  "   comparacao HOMOGENEA com os 9 grupos do IPCA oficial.",
  "10. Grafico 09 decompõe a diferenca dos IPCAR totais em contribuicoes POF (p.p.).",
  "11. Grafico 10 mostra diferencas de taxas anuais por grupo POF (p.p.).",
  "",
  "ARQUIVOS GRAFICOS:",
  "05_01_IPCAR_GERAL_VS_IPCA_OFICIAL_RJ.png",
  "05_01B_FAIXAS_IPCAR_VS_IPCA_RJ.png",
  "05_02_IPCAR_E_SUBITENS_EXTREMOS.png",
  "05_02B_IPCAR_E_GRUPOS_EXTREMOS.png",
  "05_03_INFLACAO_11_GRUPOS_POF_ANO_FOCO.png",
  "05_03B_IPCA_RJ_9_GRUPOS_OFICIAIS.png",
  "05_04_INFLACAO_ACUMULADA_11_GRUPOS_POR_ANO.png",
  "05_05_GRUPOS_VS_IPCAR_E_IPCA_2020_2025.png",
  "05_06_TABELA_VISUAL_IPCAR_FAIXA_E_GRUPO.png",
  "05_07_EXTREMOS_RENDA_VS_IPCA_9_GRUPOS_ACUMULADO.png",
  "05_08_EXTREMOS_RENDA_VS_IPCA_9_GRUPOS_ANO_A_ANO.png",
  "05_09_CONTRIBUICAO_GRUPOS_DIFERENCA_EXTREMOS.png",
  "05_10_MAPA_CALOR_DIFERENCAS_ANUAIS_11_GRUPOS.png",
  "",
  "TABELAS: 05_06_TABELAS_IPCAR_RJ.xlsx e CSVs por ano."
)
writeLines(relatorio, file.path(saida, "05_RESUMO_E_LIMITACOES.txt"), useBytes = TRUE)
message("SCRIPT 05 FINALIZADO. Arquivos em: ", saida)
print(geral_dez |> select(.data$ANO, .data$ANUAL_IPCAR, .data$ANUAL_IPCA, .data$ACUM_IPCAR, .data$ACUM_IPCA), n = Inf)
