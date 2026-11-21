# =============================================================================
# TCC UFRJ | POF 2017-2018 | ESTADO DO RIO DE JANEIRO
# SCRIPT 04: APLICACAO DO IPCA AOS PESOS E GRAFICOS DE INFLACAO
# Entrada: pesos por subitem produzidos pelo Script 03.
# Objetivo: calcular a inflacao por faixa de renda (2020-2025) e gerar graficos.
# Observacao: este script tambem REFAZ o grafico da etapa 03 com legenda legivel,
# cores mais distintas e nomes completos no eixo horizontal.
# =============================================================================

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
pasta_03 <- file.path(pasta_microdados, "TCC_RJ_NOVO", "03_pesos_11_tipos")
saida <- file.path(pasta_microdados, "TCC_RJ_NOVO", "04_inflacao_faixa")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

pacotes <- c("dplyr", "readr", "tidyr", "ggplot2", "jsonlite", "stringr", "scales", "RColorBrewer")
faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltam)) install.packages(faltam, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
  library(jsonlite); library(stringr); library(scales); library(RColorBrewer)
})
options(timeout = max(240, getOption("timeout")))

entrada_rds <- file.path(pasta_03, "03_PESOS_SUBITENS_POF_11.rds")
entrada_grupos <- file.path(pasta_03, "03_02_PESOS_11_TIPOS_POR_FAIXA.csv")
if (!file.exists(entrada_rds)) stop("Arquivo ausente: ", entrada_rds)
if (!file.exists(entrada_grupos)) stop("Arquivo ausente: ", entrada_grupos)

# Os rotulos abaixo sao EXATAMENTE os que o Script 03 gravou nos arquivos.
faixas_origem <- c("1 - Ate 2 SM", "2 - Mais de 2 a 3 SM", "3 - Mais de 3 a 6 SM",
                   "4 - Mais de 6 a 10 SM", "5 - Mais de 10 a 15 SM", "6 - Mais de 15 a 25 SM",
                   "7 - Mais de 25 SM")
faixas_rotulos <- c("Até 2 SM", "2-3 SM", "3-6 SM", "6-10 SM",
                    "10-15 SM", "15-25 SM", "Mais 25 SM")
map_faixa <- stats::setNames(faixas_rotulos, faixas_origem)

# -----------------------------------------------------------------------------
# 1) LEITURA DOS PESOS E REFACCAO DO GRAFICO DA ETAPA 03
# -----------------------------------------------------------------------------
subitens <- readRDS(entrada_rds)
req <- c("FAIXA_RENDA", "TIPO_POF_11", "COD_SNIPC", "PESO_SUBITEM_PCT")
if (!all(req %in% names(subitens))) stop("Faltam colunas em 03_PESOS_SUBITENS_POF_11.rds")
subitens <- subitens |>
  mutate(
    FAIXA_RENDA = as.character(.data$FAIXA_RENDA),
    FAIXA_LABEL = dplyr::recode(.data$FAIXA_RENDA, !!!map_faixa),
    FAIXA_LABEL = factor(.data$FAIXA_LABEL, levels = faixas_rotulos),
    COD_SNIPC = stringr::str_pad(as.character(.data$COD_SNIPC), width = 8, side = "left", pad = "0"),
    ID_SUBITEM = sub("^0", "", .data$COD_SNIPC),
    ID_ITEM = substr(.data$ID_SUBITEM, 1, 4),
    ID_SUBGRUPO = substr(.data$ID_SUBITEM, 1, 2),
    ID_GRUPO = sub("^0", "", substr(.data$COD_SNIPC, 1, 2))
  )
if (anyNA(subitens$FAIXA_LABEL)) stop("Faixas de renda nao reconhecidas no arquivo do Script 03.")

pesos11 <- readr::read_csv(entrada_grupos, show_col_types = FALSE) |>
  mutate(
    FAIXA_LABEL = dplyr::recode(as.character(.data$FAIXA_RENDA), !!!map_faixa),
    FAIXA_LABEL = factor(.data$FAIXA_LABEL, levels = faixas_rotulos),
    TIPO_POF_11 = factor(.data$TIPO_POF_11, levels = sort(unique(.data$TIPO_POF_11)))
  )

paleta11 <- RColorBrewer::brewer.pal(12, "Paired")[1:11]

# Grafico revisado da etapa 03: legenda completa e eixo com nomes pedidos pelo usuario
peso_plot <- ggplot(pesos11, aes(x = .data$FAIXA_LABEL, y = .data$PESO_GRUPO_PCT, fill = .data$TIPO_POF_11)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = paleta11, name = "11 grupos da POF") +
  labs(
    title = "RJ | POF 2017-2018: composição das cestas por faixa de renda",
    subtitle = "11 grupos, como no artigo de Curitiba",
    x = "Faixa de renda",
    y = "Participação na cesta (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

ggsave(file.path(saida, "04_00_GRAFICO_PESOS_11_TIPOS_REVISADO.png"), peso_plot,
       width = 14, height = 9, dpi = 180)

# -----------------------------------------------------------------------------
# 2) METADADOS E CONSULTA SIDRA / IBGE - TABELA 7060
# -----------------------------------------------------------------------------
# CUIDADO: o codigo hierarquico SNIPC (ex.: 1101002, arroz) NAO E o identificador
# de categoria da API do SIDRA (para arroz, por exemplo, 7173). O metadado 315
# fornece essa correspondencia. Consultar a API apenas com os IDs das categorias!
periodos <- format(seq(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "month"), "%Y%m")
cache_metadados <- file.path(saida, "04_CACHE_7060_METADADOS.rds")
# Nome NOVO: impede aproveitar resultados de consultas incorretas da versao anterior.
cache_variacoes <- file.path(saida, "04_CACHE_7060_VARIACOES_IDS_CORRETOS.rds")

num_sidra <- function(x) {
  z <- trimws(as.character(x))
  ok <- !is.na(z) & grepl("^[+-]?[0-9]+([.,][0-9]+)?$", z)
  suppressWarnings(as.numeric(ifelse(ok, gsub(",", ".", z, fixed = TRUE), NA_character_)))
}

achar_coluna <- function(df, tipo = c("codigo", "nome"), padrao = NULL, possiveis = NULL) {
  tipo <- match.arg(tipo)
  cand <- grep(if (tipo == "codigo") "^D[0-9]+C$" else "^D[0-9]+N$",
               names(df), value = TRUE)
  if (!length(cand)) stop("Formato inesperado de colunas do SIDRA.")
  if (!is.null(padrao)) {
    scores <- vapply(cand, function(x) sum(grepl(padrao, as.character(df[[x]]),
                                                  ignore.case = TRUE), na.rm = TRUE), numeric(1))
    if (max(scores) > 0) return(names(scores)[which.max(scores)])
  }
  if (!is.null(possiveis)) {
    scores <- vapply(cand, function(x) sum(as.character(df[[x]]) %in% possiveis,
                                                  na.rm = TRUE), numeric(1))
    if (max(scores) > 0) return(names(scores)[which.max(scores)])
  }
  stop("Nao foi possivel identificar a coluna SIDRA de tipo ", tipo, ".")
}

ler_metadados_7060 <- function() {
  if (file.exists(cache_metadados)) return(readRDS(cache_metadados))
  url <- "https://servicodados.ibge.gov.br/api/v3/agregados/7060/metadados"
  meta <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
                   error = function(e) stop("Falha nos metadados 7060: ", conditionMessage(e)))
  saveRDS(meta, cache_metadados)
  meta
}

mapa_classificacao_315 <- function() {
  meta <- ler_metadados_7060()
  cls <- Filter(function(x) as.character(x$id) == "315", meta$classificacoes)
  if (length(cls) != 1 || is.null(cls[[1]]$categorias)) {
    stop("Classificacao 315 ausente dos metadados do SIDRA 7060.")
  }
  tibble::tibble(
    ID_CATEGORIA = vapply(cls[[1]]$categorias, function(x) as.character(x$id), character(1)),
    DESCRICAO = vapply(cls[[1]]$categorias, function(x) as.character(x$nome), character(1))
  ) |>
    filter(grepl("^[0-9]+\\.", .data$DESCRICAO)) |>
    mutate(CODIGO_SNIPC_SIDRA = sub("\\..*$", "", .data$DESCRICAO)) |>
    distinct(.data$ID_CATEGORIA, .keep_all = TRUE)
}

codigo_rio_7060 <- function() {
  # 7169 = indice geral: categoria que existe na classificacao 315.
  url <- "https://apisidra.ibge.gov.br/values/t/7060/n7/all/v/63/p/202001/c315/7169"
  resp <- tryCatch(jsonlite::fromJSON(url, simplifyDataFrame = TRUE),
                   error = function(e) stop("Falha ao identificar regiao RJ: ", conditionMessage(e)))
  if (!is.data.frame(resp) || nrow(resp) < 2) {
    stop("Consulta de identificacao da regiao Rio retornou sem dados.")
  }
  d <- resp[-1, , drop = FALSE]
  coluna_nome <- achar_coluna(d, "nome", padrao = "Rio de Janeiro")
  coluna_id <- sub("N$", "C", coluna_nome)
  candidatos <- unique(as.character(d[[coluna_id]][
    grepl("Rio de Janeiro", as.character(d[[coluna_nome]]), ignore.case = TRUE)
  ]))
  if (length(candidatos) != 1) stop("Regiao Rio de Janeiro nao identificada univocamente no SIDRA.")
  candidatos[[1]]
}

consultar_7060 <- function(ids_categorias, codigo_rio) {
  ids_categorias <- sort(unique(stats::na.omit(as.character(ids_categorias))))
  if (!length(ids_categorias)) stop("Nenhuma categoria numerica SIDRA para consulta.")
  if (file.exists(cache_variacoes)) {
    cache <- readRDS(cache_variacoes)
    if (is.list(cache) && identical(cache$ID_SOLICITADOS, ids_categorias) &&
        identical(cache$PERIODOS, periodos) && identical(cache$RIO, codigo_rio)) {
      message("Usando cache valido de consultas por IDs internos do SIDRA.")
      return(cache$DADOS)
    }
  }
  lotes <- split(ids_categorias, ceiling(seq_along(ids_categorias) / 35))
  partes <- vector("list", length(lotes))
  for (i in seq_along(lotes)) {
    url <- paste0(
      "https://apisidra.ibge.gov.br/values/t/7060/n7/", codigo_rio,
      "/v/63/p/", min(periodos), "-", max(periodos),
      "/c315/", paste(lotes[[i]], collapse = ",")
    )
    resp <- tryCatch(jsonlite::fromJSON(url, simplifyDataFrame = TRUE),
                     error = function(e) stop("Erro na consulta 7060, lote ", i, ": ", conditionMessage(e)))
    if (!is.data.frame(resp) || nrow(resp) < 2 || !"V" %in% names(resp)) {
      stop("Resposta vazia ou inesperada do SIDRA no lote ", i)
    }
    d <- resp[-1, , drop = FALSE]
    col_categoria <- achar_coluna(d, "codigo", possiveis = lotes[[i]])
    col_mes <- achar_coluna(d, "codigo", possiveis = periodos)
    if (identical(col_categoria, col_mes)) stop("Nao foi possivel separar categoria e periodo.")
    partes[[i]] <- tibble::tibble(
      ID_CATEGORIA = as.character(d[[col_categoria]]),
      PERIODO = as.character(d[[col_mes]]),
      VARIACAO = num_sidra(d$V)
    )
    if (i %% 4 == 0 || i == length(lotes))
      message("SIDRA: lote ", i, "/", length(lotes), " consultado.")
    Sys.sleep(0.25)
  }
  dados <- bind_rows(partes) |> distinct(.data$ID_CATEGORIA, .data$PERIODO, .keep_all = TRUE)
  saveRDS(list(ID_SOLICITADOS = ids_categorias, PERIODOS = periodos,
               RIO = codigo_rio, DADOS = dados), cache_variacoes)
  dados
}

# -----------------------------------------------------------------------------
# 3) CORRESPONDENCIA: CODIGOS DOS SUBITENS <-> IDs INTERNOS DA TABELA 7060
# -----------------------------------------------------------------------------
cat("Consultando o SIDRA e compatibilizando IDs internos da classificacao 315...\n")
mapa315 <- mapa_classificacao_315()
subitens_ids <- subitens |>
  distinct(.data$COD_SNIPC, .data$ID_SUBITEM, .data$ID_ITEM,
           .data$ID_SUBGRUPO, .data$ID_GRUPO)

# A entrada COD_SNIPC vem no formato 8 digitos (ex.: 01101002).
# No SIDRA a hierarquia aparece sem zero inicial: 1101002, 1101, 11 e 1.
codigos_procurados <- unique(c(subitens_ids$ID_SUBITEM, subitens_ids$ID_ITEM,
                                subitens_ids$ID_SUBGRUPO, subitens_ids$ID_GRUPO))
categorias_ativas <- mapa315 |>
  filter(.data$CODIGO_SNIPC_SIDRA %in% codigos_procurados)
if (!nrow(categorias_ativas)) stop("Nenhum codigo POF-SNIPC encontrado no metadado 315.")
if (!all(as.character(1:9) %in% categorias_ativas$CODIGO_SNIPC_SIDRA)) {
  stop("Os nove grupos principais nao foram reconhecidos no metadado 315. Verificar metadados.")
}
write_csv(categorias_ativas,
          file.path(saida, "04_00_MAPA_CODIGOS_SNIPC_IDS_SIDRA.csv"))

rio <- codigo_rio_7060()
precos_raw <- consultar_7060(categorias_ativas$ID_CATEGORIA, rio)
precos_cod <- precos_raw |>
  inner_join(categorias_ativas, by = "ID_CATEGORIA") |>
  group_by(.data$CODIGO_SNIPC_SIDRA, .data$PERIODO) |>
  summarise(VARIACAO = {
    v <- .data$VARIACAO[is.finite(.data$VARIACAO)]
    if (length(v)) v[[1]] else NA_real_
  }, .groups = "drop")

# Reconstruir a taxa mensal usando o subitem; em falta, item -> subgrupo -> grupo.
# Nao atribuir inflacao 0% a variaveis ausentes.
base_preco <- tidyr::crossing(subitens_ids, PERIODO = periodos) |>
  left_join(precos_cod |> rename(VAR_SUBITEM = VARIACAO),
            by = c("ID_SUBITEM" = "CODIGO_SNIPC_SIDRA", "PERIODO")) |>
  left_join(precos_cod |> rename(VAR_ITEM = VARIACAO),
            by = c("ID_ITEM" = "CODIGO_SNIPC_SIDRA", "PERIODO")) |>
  left_join(precos_cod |> rename(VAR_SUBGRUPO = VARIACAO),
            by = c("ID_SUBGRUPO" = "CODIGO_SNIPC_SIDRA", "PERIODO")) |>
  left_join(precos_cod |> rename(VAR_GRUPO = VARIACAO),
            by = c("ID_GRUPO" = "CODIGO_SNIPC_SIDRA", "PERIODO")) |>
  mutate(
    NIVEL_PRECO = case_when(
      is.finite(.data$VAR_SUBITEM) ~ "SUBITEM",
      is.finite(.data$VAR_ITEM) ~ "ITEM",
      is.finite(.data$VAR_SUBGRUPO) ~ "SUBGRUPO",
      is.finite(.data$VAR_GRUPO) ~ "GRUPO",
      TRUE ~ "SEM_PRECO"
    ),
    VARIACAO_MENSAL_PCT = dplyr::coalesce(.data$VAR_SUBITEM, .data$VAR_ITEM,
                                          .data$VAR_SUBGRUPO, .data$VAR_GRUPO)
  )

# Relatorio de ausencias permite localizar problemas reais, se persistirem.
sem_preco <- base_preco |>
  filter(.data$NIVEL_PRECO == "SEM_PRECO") |>
  group_by(.data$COD_SNIPC, .data$ID_GRUPO, .data$ID_SUBGRUPO,
           .data$ID_ITEM, .data$ID_SUBITEM) |>
  summarise(N_MESES_SEM_PRECO = n(), PRIMEIRO_MES = min(.data$PERIODO),
            ULTIMO_MES = max(.data$PERIODO), .groups = "drop")
write_csv(sem_preco, file.path(saida, "04_SUBITENS_SEM_PRECO_DIAGNOSTICO.csv"))
if (nrow(sem_preco)) {
  stop("Ainda ha ", nrow(sem_preco),
       " codigos sem inflacao de grupo. Envie 04_SUBITENS_SEM_PRECO_DIAGNOSTICO.csv.")
}
if (any(!is.finite(base_preco$VARIACAO_MENSAL_PCT))) stop("Inflacao mensal possui valores NA inesperados.")
write_csv(base_preco |>
    select(.data$COD_SNIPC, .data$PERIODO, .data$NIVEL_PRECO,
           .data$VARIACAO_MENSAL_PCT),
    file.path(saida, "04_01_VARIACOES_MENSAIS_POR_SUBITEM.csv"))

painel_cob <- subitens |>
  select(.data$FAIXA_LABEL, .data$COD_SNIPC, .data$PESO_SUBITEM_PCT) |>
  inner_join(base_preco |>
               select(.data$COD_SNIPC, .data$PERIODO, .data$NIVEL_PRECO),
             by = "COD_SNIPC") |>
  group_by(.data$FAIXA_LABEL, .data$PERIODO, .data$NIVEL_PRECO) |>
  summarise(PESO_NIVEL_PCT = sum(.data$PESO_SUBITEM_PCT), .groups = "drop") |>
  group_by(.data$FAIXA_LABEL, .data$PERIODO) |>
  mutate(SOMA = sum(.data$PESO_NIVEL_PCT)) |>
  ungroup()
if (any(abs(painel_cob$SOMA - 100) > 1e-6)) stop("Cobertura por nivel nao fecha 100%.")
write_csv(painel_cob, file.path(saida, "04_02_COBERTURA_NIVEIS_PRECO.csv"))

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 4) INDICES MENSAIS E ANUAIS POR FAIXA DE RENDA
# -----------------------------------------------------------------------------
relativos <- base_preco |>
  group_by(.data$COD_SNIPC) |>
  arrange(.data$PERIODO, .by_group = TRUE) |>
  mutate(RELATIVO = cumprod(1 + .data$VARIACAO_MENSAL_PCT / 100)) |>
  ungroup() |>
  select(.data$COD_SNIPC, .data$PERIODO, .data$NIVEL_PRECO, .data$RELATIVO)

painel <- subitens |>
  select(.data$FAIXA_LABEL, .data$TIPO_POF_11, .data$COD_SNIPC, .data$PESO_SUBITEM_PCT) |>
  inner_join(relativos, by = "COD_SNIPC")

indices <- painel |>
  group_by(.data$FAIXA_LABEL, .data$PERIODO) |>
  summarise(INDICE_DEZ2019_100 = sum(.data$PESO_SUBITEM_PCT * .data$RELATIVO), .groups = "drop") |>
  group_by(.data$FAIXA_LABEL) |>
  arrange(.data$PERIODO, .by_group = TRUE) |>
  mutate(
    INFLACAO_MENSAL_PCT = 100 * (.data$INDICE_DEZ2019_100 / dplyr::lag(.data$INDICE_DEZ2019_100, default = 100) - 1),
    INFLACAO_ACUMULADA_PCT = .data$INDICE_DEZ2019_100 - 100,
    ANO = substr(.data$PERIODO, 1, 4)
  ) |>
  ungroup()

if (nrow(indices) != length(faixas_rotulos) * length(periodos)) stop("A serie mensal esta incompleta.")
write_csv(indices, file.path(saida, "04_03_INDICES_MENSAIS_FAIXA.csv"))

inflacao_anual <- indices |>
  group_by(.data$FAIXA_LABEL, .data$ANO) |>
  summarise(INDICE_FIM_ANO = dplyr::last(.data$INDICE_DEZ2019_100), MESES = n(), .groups = "drop") |>
  group_by(.data$FAIXA_LABEL) |>
  arrange(.data$ANO, .by_group = TRUE) |>
  mutate(INFLACAO_ANO_PCT = 100 * (.data$INDICE_FIM_ANO / dplyr::lag(.data$INDICE_FIM_ANO, default = 100) - 1)) |>
  ungroup()
if (any(inflacao_anual$MESES != 12)) stop("A inflacao anual nao possui 12 meses em todos os anos.")
write_csv(inflacao_anual, file.path(saida, "04_04_INFLACAO_ANUAL_FAIXA.csv"))

inflacao_final <- indices |>
  filter(.data$PERIODO == "202512") |>
  select(.data$FAIXA_LABEL, .data$INFLACAO_ACUMULADA_PCT) |>
  rename(INFLACAO_2020_2025_PCT = .data$INFLACAO_ACUMULADA_PCT) |>
  arrange(.data$FAIXA_LABEL)
write_csv(inflacao_final, file.path(saida, "04_05_INFLACAO_ACUMULADA_2020_2025.csv"))

# Indice dos 11 grupos por faixa (auxiliar para interpretacao, nao obrigatorio ao artigo)
indices_11 <- painel |>
  group_by(.data$FAIXA_LABEL, .data$TIPO_POF_11, .data$PERIODO) |>
  summarise(
    PESO_TIPO_PCT = sum(.data$PESO_SUBITEM_PCT),
    INDICE_TIPO = sum(.data$PESO_SUBITEM_PCT * .data$RELATIVO) / sum(.data$PESO_SUBITEM_PCT) * 100,
    .groups = "drop"
  )
write_csv(indices_11, file.path(saida, "04_06_INDICES_11_TIPOS_MENSAIS.csv"))

# -----------------------------------------------------------------------------
# 5) GRAFICOS DA ETAPA 04
# -----------------------------------------------------------------------------
pal_faixas <- RColorBrewer::brewer.pal(8, "Dark2")[1:7]

g1 <- indices |>
  mutate(DATA = as.Date(paste0(.data$PERIODO, "01"), "%Y%m%d")) |>
  ggplot(aes(x = .data$DATA, y = .data$INFLACAO_ACUMULADA_PCT, color = .data$FAIXA_LABEL)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = pal_faixas, name = "Faixa de renda") +
  labs(
    title = "RJ | Inflação acumulada por faixa de renda",
    subtitle = "Cestas da POF 2017-2018 aplicadas ao IPCA regional do Rio | 2020-2025",
    x = "Mês",
    y = "Inflação acumulada (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(saida, "04_07_GRAFICO_INFLACAO_ACUMULADA_2020_2025.png"), g1,
       width = 12, height = 7, dpi = 180)

g2 <- inflacao_anual |>
  mutate(ANO = factor(.data$ANO, levels = sort(unique(.data$ANO)))) |>
  ggplot(aes(x = .data$ANO, y = .data$INFLACAO_ANO_PCT, fill = .data$FAIXA_LABEL)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = pal_faixas, name = "Faixa de renda") +
  labs(
    title = "RJ | Inflação anual por faixa de renda",
    subtitle = "2020 a 2025",
    x = "Ano",
    y = "Inflação anual (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(saida, "04_08_GRAFICO_INFLACAO_ANUAL_2020_2025.png"), g2,
       width = 13, height = 7, dpi = 180)

g3 <- inflacao_final |>
  ggplot(aes(x = .data$FAIXA_LABEL, y = .data$INFLACAO_2020_2025_PCT, fill = .data$FAIXA_LABEL)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = pal_faixas) +
  labs(
    title = "RJ | Inflação acumulada no período 2020-2025",
    subtitle = "Resultado final por faixa de renda",
    x = "Faixa de renda",
    y = "Inflação acumulada (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(size = 10))

ggsave(file.path(saida, "04_09_GRAFICO_FINAL_2020_2025.png"), g3,
       width = 11, height = 6.5, dpi = 180)

# -----------------------------------------------------------------------------
# 6) RESUMO E ORIENTACAO PARA ENVIO
# -----------------------------------------------------------------------------
resumo <- c(
  "SCRIPT 04 - INFLACAO POR FAIXA DE RENDA (2020-2025)",
  "",
  "O que este script fez:",
  "1) Refez o grafico da etapa 03 com legenda legivel e eixo horizontal padronizado.",
  "2) Leu os pesos por subitem do Script 03.",
  "3) Consultou a tabela 7060 do SIDRA para a area Rio de Janeiro.",
  "4) Aplicou as variacoes mensais de 2020-2025 as cestas das 7 faixas.",
  "5) Gerou indices mensais, inflacao anual e acumulada.",
  "",
  "Arquivos principais para me enviar depois:",
  "- 04_04_INFLACAO_ANUAL_FAIXA.csv",
  "- 04_05_INFLACAO_ACUMULADA_2020_2025.csv",
  "- 04_07_GRAFICO_INFLACAO_ACUMULADA_2020_2025.png",
  "- 04_08_GRAFICO_INFLACAO_ANUAL_2020_2025.png",
  "- 04_09_GRAFICO_FINAL_2020_2025.png",
  "- se houver erro: copie o console inteiro ou envie a pasta da etapa 04 compactada.",
  "",
  "Limitacao aceita nesta monografia:",
  "- Quando um subitem nao possui serie numerica, o script usa item, subgrupo e grupo, nesta ordem.",
  "- Isso e uma aproximacao documentada; nao e necessario resolver cada caso isoladamente agora."
)
writeLines(resumo, file.path(saida, "04_RESUMO.txt"), useBytes = TRUE)

cat("\nSCRIPT 04 CONCLUIDO. Pasta de saida:\n", saida,
    "\nEnvie a pasta 04_inflacao_faixa compactada ou os arquivos principais listados em 04_RESUMO.txt\n")
print(inflacao_final, n = Inf)
