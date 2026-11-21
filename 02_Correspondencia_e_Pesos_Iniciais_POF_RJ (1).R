# =============================================================================
# MONOGRAFIA | POF 2017-2018 | RIO DE JANEIRO (UF 33)
# SCRIPT 02 - CORRIGIR TRADUTOR POF-SNIPC E CALCULAR PESOS INICIAIS
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
pasta_01 <- file.path(pasta_microdados, "TCC_RJ_NOVO", "01_preparacao")
saida <- file.path(pasta_microdados, "TCC_RJ_NOVO", "02_pesos")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

pacotes <- c("dplyr", "readr", "tibble")
nao_instalados <- setdiff(pacotes, rownames(installed.packages()))
if (length(nao_instalados)) install.packages(nao_instalados, repos="https://cloud.r-project.org")
invisible(lapply(pacotes, library, character.only=TRUE))

arqs <- c(despesas=file.path(pasta_01, "02_DESPESAS_POF_RJ_PARA_CLASSIFICAR.rds"),
          dicionario=file.path(pasta_01, "02_DICIONARIO_OFICIAL_POF_SNIPC.csv"),
          faixas=file.path(pasta_01, "01_FAIXAS_DE_RENDA_RJ.csv"))
if (any(!file.exists(arqs))) stop("Arquivos ausentes:\n",
                                 paste(arqs[!file.exists(arqs)], collapse="\n"))

desp <- readRDS(arqs[["despesas"]])
dic <- readr::read_csv(arqs[["dicionario"]], show_col_types=FALSE,
   col_types=readr::cols(CODIGO_POF=readr::col_character(),
                          COD_SNIPC=readr::col_character(), .default=readr::col_character()))
faixas <- readr::read_csv(arqs[["faixas"]], show_col_types=FALSE)
if (nrow(faixas)!=7) stop("As sete faixas de renda precisam estar no Script 01.")
if (anyDuplicated(dic$CODIGO_POF)) stop("A correspondencia tem codigo POF duplicado.")
necessarias <- c("FONTE_POF", "TIPO_FONTE", "FAIXA_RENDA", "CODIGO_POF",
                 "QUADRO", "FORMA_OBTENCAO", "DESPESA_EXPANDIDA_ANUAL")
if (!all(necessarias %in% names(desp))) {
 stop("Campos ausentes no RDS: ", paste(setdiff(necessarias,names(desp)),collapse=", "))
}
if (anyNA(desp$FAIXA_RENDA) || any(!is.finite(desp$DESPESA_EXPANDIDA_ANUAL)) ||
    any(desp$DESPESA_EXPANDIDA_ANUAL < 0))
 stop("Despesa invalida ou falta de faixa de renda: conferir Script 01.")

# Manter uma unica linha por despesa; corrigir a chave, SEM alterar V9001 original.
n_despesas_antes <- nrow(desp)
desp <- desp |>
  dplyr::select(-dplyr::any_of(c("DESCRICAO_POF", "COD_SNIPC", "DESCRICAO_SNIPC",
                                  "GRUPO_SNIPC_9", "SITUACAO_MAPA"))) |>
  dplyr::mutate(
     CODIGO_PRODUTO_POF = dplyr::if_else(
        !is.na(.data$CODIGO_POF) & nchar(.data$CODIGO_POF)>=3,
        substr(.data$CODIGO_POF, 1, pmax(nchar(.data$CODIGO_POF)-2, 1)), NA_character_)) |>
  dplyr::left_join(dic, by=c("CODIGO_PRODUTO_POF"="CODIGO_POF")) |>
  dplyr::mutate(
    GRUPO_SNIPC_9 = substr(.data$COD_SNIPC,1,2),
    STATUS_MAPA = dplyr::case_when(
       is.na(.data$COD_SNIPC) ~ "SEM_MAPA",
       .data$GRUPO_SNIPC_9 == "10" ~ "CODIGO_AUXILIAR",
       .data$GRUPO_SNIPC_9 %in% sprintf("%02d",1:9) ~ "MAPEADO_NOVE_GRUPOS",
       TRUE ~ "REVISAR_CODIGO"),
    FORMA_OBTENCAO = trimws(as.character(.data$FORMA_OBTENCAO)),
    MODALIDADE = dplyr::case_when(
       .data$FORMA_OBTENCAO %in% c("1","3","5") ~ "MONETARIA_PROPRIA",
       .data$FORMA_OBTENCAO %in% c("2","4","6") ~ "MONETARIA_OUTRA_UC",
       .data$FORMA_OBTENCAO %in% c("7","8","9","10") ~ "NAO_MONETARIA",
       .data$FORMA_OBTENCAO == "11" ~ "OUTRA",
       TRUE ~ "VERIFICAR_FORMA"))

if (anyDuplicated(dic$CODIGO_POF) || nrow(desp) == 0L)
 stop("Erro na correspondencia de codigos.")
# Uma chave POF so pode ter um destino; LEFT JOIN nao pode duplicar linhas.
if (nrow(desp) != n_despesas_antes)
 stop("O join aumentou o numero de despesas; conferir dicionario.")

# Diagnostico por fonte: a cobertura dos arquivos COMPLEMENTARES e informada
# separadamente; ela nao se confunde com o consumo elegivel ao SNIPC.
cobertura <- desp |>
  dplyr::group_by(.data$FONTE_POF, .data$TIPO_FONTE, .data$STATUS_MAPA) |>
  dplyr::summarise(N_REGISTROS=dplyr::n(),
                   DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA_ANUAL), .groups="drop") |>
  dplyr::group_by(.data$FONTE_POF) |>
  dplyr::mutate(PCT_DESPESA_NA_FONTE = 100*.data$DESPESA_EXPANDIDA /
                   sum(.data$DESPESA_EXPANDIDA)) |>
  dplyr::ungroup()
readr::write_csv(cobertura, file.path(saida,"02_01_COBERTURA_CORRIGIDA.csv"))

# A estrutura de peso eh calculada por subitem com o total de despesas DIRETAMENTE
# correspondidas; este denominador AINDA NAO E o total definitivo de consumo.
# A selecao das formas de aquisicao e apresentada como sensibilidade.
principal <- desp |> dplyr::filter(.data$TIPO_FONTE=="PRINCIPAL")
if (any(principal$MODALIDADE=="VERIFICAR_FORMA"))
 warning("Existem formas de aquisicao desconhecidas: incluiremos no diagnostico.")

base_pesos <- list(
  TODAS_AS_FORMAS = principal,
  MONETARIAS = principal |>
    dplyr::filter(.data$MODALIDADE %in% c("MONETARIA_PROPRIA","MONETARIA_OUTRA_UC")),
  MONETARIAS_PROPRIA_UC = principal |>
    dplyr::filter(.data$MODALIDADE=="MONETARIA_PROPRIA"))

pesos <- dplyr::bind_rows(lapply(names(base_pesos), function(cenario) {
  b <- base_pesos[[cenario]]
  b |> dplyr::filter(.data$STATUS_MAPA=="MAPEADO_NOVE_GRUPOS") |>
    dplyr::group_by(.data$FAIXA_RENDA, .data$COD_SNIPC, .data$DESCRICAO_SNIPC) |>
    dplyr::summarise(DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA_ANUAL),
                     .groups="drop") |>
    dplyr::group_by(.data$FAIXA_RENDA) |>
    dplyr::mutate(PESO_PRELIMINAR_PCT = 100*.data$DESPESA_EXPANDIDA /
                    sum(.data$DESPESA_EXPANDIDA)) |>
    dplyr::ungroup() |>
    dplyr::mutate(CENARIO=cenario)
}))
if (!nrow(pesos) || any(!is.finite(pesos$PESO_PRELIMINAR_PCT)))
 stop("Nao foi possivel calcular pesos preliminares.")
fechamento <- pesos |>
  dplyr::group_by(.data$CENARIO, .data$FAIXA_RENDA) |>
  dplyr::summarise(PESO_TOTAL_PCT=sum(.data$PESO_PRELIMINAR_PCT), .groups="drop")
if (nrow(fechamento)!=21 || any(abs(fechamento$PESO_TOTAL_PCT-100)>1e-7))
 stop("Os pesos preliminares nao fecharam para todas as faixas/cenarios.")
readr::write_csv(pesos,file.path(saida,"02_02_PESOS_PRELIMINARES_SUBITENS.csv"))
readr::write_csv(fechamento,file.path(saida,"02_03_FECHAMENTO_PESOS.csv"))

# Quanto da despesa fica fora: NAO assumir que toda despesa sem mapa eh elegivel.
pendencias <- principal |>
  dplyr::filter(.data$STATUS_MAPA!="MAPEADO_NOVE_GRUPOS") |>
  dplyr::group_by(.data$FONTE_POF,.data$QUADRO,.data$CODIGO_PRODUTO_POF,
                  .data$STATUS_MAPA) |>
  dplyr::summarise(N_REGISTROS=dplyr::n(),
                   DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA_ANUAL),
                   .groups="drop") |>
  dplyr::arrange(dplyr::desc(.data$DESPESA_EXPANDIDA))
readr::write_csv(pendencias,file.path(saida,"02_04_CODIGOS_PENDENTES.csv"))

# Catálogo que conserva QUADRO + codigo original para a classificação nos
# ONZE TIPOS de despesas da POF. Nao colocar cada item automaticamente no
# grupo de nove do IPCA, pois os sistemas de categorias diferem.
catalogo11 <- desp |>
  dplyr::group_by(.data$FONTE_POF,.data$QUADRO,.data$CODIGO_PRODUTO_POF,
    .data$DESCRICAO_POF,.data$COD_SNIPC,.data$DESCRICAO_SNIPC,.data$STATUS_MAPA) |>
  dplyr::summarise(DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA_ANUAL),
                   N_REGISTROS=dplyr::n(),.groups="drop") |>
  dplyr::arrange(dplyr::desc(.data$DESPESA_EXPANDIDA)) |>
  dplyr::mutate(TIPO_POF_11 = NA_character_)
readr::write_csv(catalogo11,file.path(saida,"02_05_CATALOGO_11_TIPOS_POF.csv"))
saveRDS(desp,file.path(saida,"02_DESPESAS_COM_TRADUTOR_CORRIGIDO.rds"),compress="gzip")

resumo <- c(
  "TCC RJ - SCRIPT 02: CORRESPONDENCIA POF-SNIPC CORRIGIDA",
  paste0("Registros lidos: ",nrow(desp)),
  paste0("Registros na base principal: ",nrow(principal)),
  paste0("Codigos POF no dicionario oficial: ",nrow(dic)),
  "Chave corrigida: V9001 SEM os DOIS digitos finais, mantendo zeros internos.",
  "O arquivo 02_01 mede a cobertura nas bases principais e complementares.",
  "O arquivo 02_02 traz pesos INICIAIS para sete faixas e tres selecoes de despesas.",
  "Valores sem correspondencia e codigos auxiliares NAO foram realocados.",
  "Os pesos ainda nao sao uma versao definitiva nem uma cesta de 11 tipos POF.",
  "A classificacao em ONZE TIPOS da POF exige um dicionario proprio validado.",
  "Proximo trabalho: validar 11 tipos, tratar as pendencias de maior valor e",
  "aplicar as variacoes de precos do IPCA para os graficos."
)
writeLines(resumo,file.path(saida,"02_RESUMO_METODOLOGICO.txt"))
cat("\n",paste(resumo,collapse="\n"),"\nArquivos em: ",saida,"\n",sep="")
