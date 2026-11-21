# =============================================================================
# MONOGRAFIA | POF 2017–2018, ESTADO DO RIO DE JANEIRO
# SCRIPT 01 - PREPARACAO DOS MICRODADOS, RENDA E CATALOGO DE DESPESAS
# INICIO DO PROJETO REFEITO. NAO UTILIZA SCRIPTS ANTERIORES.
# =============================================================================
# BASE: programa de leitura oficial da POF enviado pelo pesquisador.
# LEITURA: mesma posicao dos campos do read.fwf, mas so das colunas essenciais.
# FONTE: POF 2017-2018, IBGE; tabela oficial POF -> SNIPC.
# OBJETIVO: 7 faixas em R$ de janeiro/2018 e preparar 11 tipos de despesa POF.
# NOTA: os 11 tipos POF NAO se deduzem automaticamente dos 9 grupos do IPCA.
#       A classificacao sera validada com o catalogo deste script.
#       ESTE SCRIPT NAO CALCULA AINDA PESOS OU INFLACAO.
# =============================================================================

# 0. CONFIGURACAO PARA EXECUCAO LOCAL ---------------------------------------
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
# A rotina de procura de arquivos continua intacta (ver secao 2).

# 1. BIBLIOTECAS -------------------------------------------------------------
necessarios <- c("dplyr", "readr", "readxl", "tibble")
faltantes <- setdiff(necessarios, rownames(installed.packages()))
if (length(faltantes)) install.packages(faltantes, repos = "https://cloud.r-project.org")
invisible(lapply(necessarios, library, character.only = TRUE))

# Evita nomes de arquivos oriundos de projetos anteriores.
saida <- file.path(pasta_microdados, "TCC_RJ_NOVO", "01_preparacao")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

chaves_uc <- c("UF", "ESTRATO_POF", "COD_UPA", "NUM_DOM", "NUM_UC")
nome_arquivos <- c("MORADOR", "DESPESA_COLETIVA", "CADERNETA_COLETIVA",
                   "DESPESA_INDIVIDUAL", "ALUGUEL_ESTIMADO",
                   "SERVICO_NAO_MONETARIO_POF2", "SERVICO_NAO_MONETARIO_POF4")
complementares <- c("ALUGUEL_ESTIMADO", "SERVICO_NAO_MONETARIO_POF2",
                    "SERVICO_NAO_MONETARIO_POF4")

# Os intervalos abaixo foram extraidos do PROGRAMA OFICIAL DE LEITURA em R
# (arquivo Leitura dos Microdados - R(1).R). Nao sao inferencias sobre os dados.
# Eles permitem selecionar UF=33 ainda durante a leitura, poupando memoria.
layouts <- list(
  MORADOR = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(106, 115), PESO_FINAL = c(92, 105), COD_INFORMANTE = c(20, 21)
  ),
  DESPESA_COLETIVA = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(127, 136), PESO_FINAL = c(113, 126), QUADRO = c(20, 21), SEQ = c(22, 23), V9001 = c(24, 30), V9002 = c(31, 32), V8000_DEFLA = c(75, 84), FATOR_ANUALIZACAO = c(97, 98)
  ),
  CADERNETA_COLETIVA = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(97, 106), PESO_FINAL = c(83, 96), QUADRO = c(20, 21), SEQ = c(22, 24), V9001 = c(25, 31), V9002 = c(32, 33), V8000_DEFLA = c(56, 65), FATOR_ANUALIZACAO = c(67, 68)
  ),
  DESPESA_INDIVIDUAL = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(105, 114), PESO_FINAL = c(91, 104), QUADRO = c(22, 23), SEQ = c(24, 25), COD_INFORMANTE = c(20, 21), V9001 = c(26, 32), V9002 = c(33, 34), V8000_DEFLA = c(64, 73), FATOR_ANUALIZACAO = c(75, 76)
  ),
  ALUGUEL_ESTIMADO = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(98, 107), PESO_FINAL = c(84, 97), QUADRO = c(20, 21), V9001 = c(22, 28), V9002 = c(29, 30), V8000_DEFLA = c(57, 66), FATOR_ANUALIZACAO = c(68, 69)
  ),
  SERVICO_NAO_MONETARIO_POF2 = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(121, 130), PESO_FINAL = c(107, 120), QUADRO = c(20, 21), SEQ = c(22, 23), V9001 = c(24, 30), V9002 = c(31, 32), V8000_DEFLA = c(70, 79), FATOR_ANUALIZACAO = c(91, 92)
  ),
  SERVICO_NAO_MONETARIO_POF4 = list(
    UF = c(1, 2), ESTRATO_POF = c(3, 6), COD_UPA = c(8, 16), NUM_DOM = c(17, 18), NUM_UC = c(19, 19), TIPO_SITUACAO_REG = c(7, 7), RENDA_TOTAL = c(104, 113), PESO_FINAL = c(90, 103), QUADRO = c(22, 23), SEQ = c(24, 25), COD_INFORMANTE = c(20, 21), V9001 = c(26, 32), V9002 = c(33, 34), V8000_DEFLA = c(63, 72), FATOR_ANUALIZACAO = c(74, 75)
  )
)

# 2. LEITURA DA POF: SOMENTE RJ, SEM RESTRICAO URBANA/METROPOLITANA ----------
procurar_arquivo <- function(nome) {
  candidatos <- c(file.path(pasta_microdados, nome),
                  file.path(pasta_microdados, "Dados_20230713", nome),
                  file.path(dirname(pasta_microdados), nome))
  candidatos <- candidatos[file.exists(candidatos)]
  if (length(candidatos)) return(candidatos[[1]])
  NA_character_
}

ler_registro_rj <- function(base, obrigatorio = TRUE) {
  caminho <- procurar_arquivo(paste0(base, ".txt"))
  if (is.na(caminho)) {
    if (obrigatorio) stop("Arquivo necessario nao encontrado: ", base, ".txt",
                           "\nConfira a pasta_microdados no inicio do script.")
    message("Registro complementar ausente (seguiremos): ", base)
    return(NULL)
  }
  message("Lendo o estado do RJ: ", base)
  conexao <- file(caminho, "r")
  on.exit(close(conexao), add = TRUE)
  pedacos <- list()
  k <- 0L
  repeat {
    bloco <- readLines(conexao, n = 50000L, warn = FALSE)
    if (!length(bloco)) break
    selecionado <- bloco[substr(bloco, 1, 2) == "33"]
    if (length(selecionado)) {
      k <- k + 1L
      pedacos[[k]] <- selecionado
    }
  }
  linhas <- unlist(pedacos, use.names = FALSE)
  rm(pedacos)
  if (!length(linhas)) stop("Nenhuma linha UF=33 em ", basename(caminho))
  campos <- layouts[[base]]
  resultado <- lapply(campos, function(lim) {
    x <- trimws(substr(linhas, lim[[1]], lim[[2]]))
    x[x == ""] <- NA_character_
    x
  })
  rm(linhas)
  tibble::as_tibble(resultado)
}

normalizar_codigo <- function(x) {
  z <- trimws(as.character(x))
  z[which(!is.na(z) & z == "")] <- NA_character_
  z <- sub("^0+", "", z)
  z[which(!is.na(z) & z == "")] <- "0"
  z
}

numeric_pof <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

morador <- ler_registro_rj("MORADOR")
for (ch in chaves_uc) {
  if (anyNA(morador[[ch]])) stop("Chave sem valor em MORADOR: ", ch)
}
morador <- morador |>
  dplyr::mutate(RENDA_TOTAL = numeric_pof(.data$RENDA_TOTAL),
                PESO_FINAL = numeric_pof(.data$PESO_FINAL))

# A mesma RENDA_TOTAL se repete entre moradores da unidade de consumo.
# Usa-se UMA informacao por UC; nunca somar a coluna entre todos os moradores.
uc <- morador |>
  dplyr::group_by(dplyr::across(dplyr::all_of(chaves_uc))) |>
  dplyr::summarise(
    MORADORES_REGISTRADOS = dplyr::n(),
    N_RENDAS_DIFERENTES = dplyr::n_distinct(.data$RENDA_TOTAL, na.rm = TRUE),
    N_PESOS_DIFERENTES = dplyr::n_distinct(.data$PESO_FINAL, na.rm = TRUE),
    RENDA_UC = if (all(is.na(.data$RENDA_TOTAL))) NA_real_
      else dplyr::first(.data$RENDA_TOTAL[!is.na(.data$RENDA_TOTAL)]),
    PESO_UC = if (all(is.na(.data$PESO_FINAL))) NA_real_
      else dplyr::first(.data$PESO_FINAL[!is.na(.data$PESO_FINAL)]),
    .groups = "drop")

if (any(uc$N_RENDAS_DIFERENTES > 1) || any(uc$N_PESOS_DIFERENTES > 1)) {
  readr::write_csv(uc |> dplyr::filter(.data$N_RENDAS_DIFERENTES > 1 |
                                         .data$N_PESOS_DIFERENTES > 1),
                   file.path(saida, "ERRO_UC_DIVERGENTE.csv"))
  stop("RENDA_TOTAL ou PESO_FINAL divergente entre moradores da mesma UC.")
}

# Limites do Quadro 2 da POF, SM jan/2018=R$954; renda nao positiva inclui-se
# na primeira faixa, quando a UC tem rendimento observado.
limites <- c(1908, 2862, 5724, 9540, 14310, 23850)
rotulos <- c("1 - Ate 2 SM", "2 - Mais de 2 a 3 SM", "3 - Mais de 3 a 6 SM",
             "4 - Mais de 6 a 10 SM", "5 - Mais de 10 a 15 SM",
             "6 - Mais de 15 a 25 SM", "7 - Mais de 25 SM")
uc <- uc |>
  dplyr::mutate(FAIXA_RENDA = as.character(cut(.data$RENDA_UC,
    breaks = c(-Inf, limites, Inf), labels = rotulos,
    include.lowest = TRUE, right = TRUE)))

# Exportar inconsistencias: NA nao entra forcosamente na primeira faixa.
problemas_uc <- uc |> dplyr::filter(is.na(.data$FAIXA_RENDA) |
                                       !is.finite(.data$PESO_UC) | .data$PESO_UC <= 0)
readr::write_csv(problemas_uc, file.path(saida, "01_UC_SEM_CLASSIFICACAO.csv"))
uc_validas <- uc |> dplyr::filter(!is.na(.data$FAIXA_RENDA),
                                   is.finite(.data$PESO_UC), .data$PESO_UC > 0)

faixas <- uc_validas |>
  dplyr::group_by(.data$FAIXA_RENDA) |>
  dplyr::summarise(UC_NA_AMOSTRA = dplyr::n(),
                   UC_EXPANDIDAS = sum(.data$PESO_UC),
                   .groups = "drop") |>
  dplyr::mutate(FAIXA_RENDA = factor(.data$FAIXA_RENDA, levels = rotulos),
                PCT_UC_EXPANDIDAS = 100 * .data$UC_EXPANDIDAS /
                                           sum(.data$UC_EXPANDIDAS)) |>
  dplyr::arrange(.data$FAIXA_RENDA)
readr::write_csv(faixas, file.path(saida, "01_FAIXAS_DE_RENDA_RJ.csv"))
saveRDS(uc_validas, file.path(saida, "01_UNIDADES_CONSUMO_RJ.rds"))
if (nrow(faixas) != 7L) warning("Nem todas as 7 faixas foram encontradas.")
message("Unidades de consumo do RJ com renda valida: ", nrow(uc_validas))

# 3. IMPORTAR TABELA OFICIAL DE CORRESPONDENCIA POF -> SNIPC -----------------
arquivo_mapa <- procurar_arquivo(
  "Tabela_de_correspondencia_despesas_POF_2017_2018.xlsx")
if (is.na(arquivo_mapa)) {
  arquivo_mapa <- procurar_arquivo(
    "Tabela_de_correspondencia_despesas_POF_2017_2018(1).xlsx")
}
if (is.na(arquivo_mapa)) {
  stop("Planilha oficial de correspondencia POF-SNIPC nao encontrada na pasta.",
       "\nCopie-a para pasta_microdados e execute novamente.")
}
mapa <- readxl::read_excel(arquivo_mapa, sheet = "tab correspondência",
   skip = 4, col_names = c("CODIGO_POF", "DESCRICAO_POF",
                            "COD_SNIPC", "DESCRICAO_SNIPC"),
   col_types = "text") |>
  dplyr::mutate(CODIGO_POF = normalizar_codigo(.data$CODIGO_POF),
                COD_SNIPC = trimws(.data$COD_SNIPC),
                COD_SNIPC = vapply(.data$COD_SNIPC, function(z) {
                  if (is.na(z) || !nzchar(z)) return(NA_character_)
                  paste0(strrep("0", max(0, 8 - nchar(z))), z)
                }, character(1))) |>
  dplyr::filter(!is.na(.data$CODIGO_POF), !is.na(.data$COD_SNIPC))

if (nrow(mapa) == 0L || anyDuplicated(mapa$CODIGO_POF)) {
  stop("A tabela POF-SNIPC esta vazia ou um codigo POF tem mais de um destino.")
}
readr::write_csv(mapa, file.path(saida, "02_DICIONARIO_OFICIAL_POF_SNIPC.csv"))
message("Correspondencias oficiais importadas: ", nrow(mapa))

# 4. LER DESPESAS DOS QUESTIONARIOS PRINCIPAIS -------------------------------
bases_despesa <- nome_arquivos[nome_arquivos != "MORADOR"]
conjuntos <- list()
for (base in bases_despesa) {
  tab <- ler_registro_rj(base, obrigatorio = !(base %in% complementares))
  if (is.null(tab)) next
  if (!"COD_INFORMANTE" %in% names(tab)) tab$COD_INFORMANTE <- NA_character_
  if (!"SEQ" %in% names(tab)) tab$SEQ <- NA_character_
  tab <- tab |>
    dplyr::transmute(dplyr::across(dplyr::all_of(chaves_uc)),
      FONTE_POF = base,
      TIPO_FONTE = if (base %in% complementares) "COMPLEMENTAR" else "PRINCIPAL",
      QUADRO = .data$QUADRO,
      SEQ = .data$SEQ,
      COD_INFORMANTE = .data$COD_INFORMANTE,
      CODIGO_POF = normalizar_codigo(.data$V9001),
      FORMA_OBTENCAO = .data$V9002,
      VALOR_DEFLACIONADO = numeric_pof(.data$V8000_DEFLA),
      FATOR_ANUALIZACAO = numeric_pof(.data$FATOR_ANUALIZACAO),
      PESO_REGISTRO = numeric_pof(.data$PESO_FINAL))
  conjuntos[[base]] <- tab
  message(base, ": ", nrow(tab), " linhas UF=33")
}
despesas <- dplyr::bind_rows(conjuntos)
if (!nrow(despesas)) stop("Nenhuma despesa carregada.")

# A faixa da UC vem SOMENTE da base de moradores: nunca recodificar renda
# separadamente por registro de despesa.
n_antes <- nrow(despesas)
despesas <- despesas |>
  dplyr::left_join(uc_validas |>
    dplyr::select(dplyr::all_of(chaves_uc), "FAIXA_RENDA", "PESO_UC"),
    by = chaves_uc) |>
  dplyr::left_join(mapa, by = "CODIGO_POF") |>
  dplyr::mutate(
    GRUPO_SNIPC_9 = substr(.data$COD_SNIPC, 1, 2),
    SITUACAO_MAPA = dplyr::case_when(
      is.na(.data$COD_SNIPC) ~ "SEM_CORRESPONDENCIA",
      .data$GRUPO_SNIPC_9 == "10" ~ "AUXILIAR_REQUER_TRATAMENTO",
      .data$GRUPO_SNIPC_9 %in% sprintf("%02d", 1:9) ~ "MAPEADA_SNIPC",
      TRUE ~ "CODIGO_SNIPC_A_VERIFICAR"),
    DESPESA_EXPANDIDA_ANUAL = .data$VALOR_DEFLACIONADO *
                               .data$FATOR_ANUALIZACAO * .data$PESO_UC)
if (nrow(despesas) != n_antes) stop("Uniao duplicou linhas: revisar chaves.")

# Aviso em vez de excluir sistematicamente despesas sem mapa, valores nulos,
# formas nao monetarias ou bases complementares. Analise primeiro as tabelas.
if (any(is.na(despesas$FAIXA_RENDA))) {
  warning("Ha despesas de UCs sem renda/expansao valida; relatorio mostrara ausencias.")
}

# Base completa para o proximo script: mantem TODAS as situacoes identificadas.
saveRDS(despesas, file.path(saida, "02_DESPESAS_POF_RJ_PARA_CLASSIFICAR.rds"),
        compress = "gzip")

cobertura <- despesas |>
  dplyr::group_by(.data$FONTE_POF, .data$TIPO_FONTE, .data$SITUACAO_MAPA) |>
  dplyr::summarise(N_LINHAS = dplyr::n(),
    N_SEM_VALOR_CALCULAVEL = sum(!is.finite(.data$DESPESA_EXPANDIDA_ANUAL)),
    DESPESA_ANUAL_EXPANDIDA = sum(.data$DESPESA_EXPANDIDA_ANUAL, na.rm = TRUE),
    .groups = "drop")
readr::write_csv(cobertura, file.path(saida, "03_COBERTURA_DO_TRADUTOR.csv"))

# Uma linha por (arquivo, quadro, codigo). Serve de dicionario operacional
# para a NOVA classificacao em 11 GRUPOS POF, a validar no passo seguinte.
# A POF tem: Alimentacao; Habitacao; Vestuario; Transporte; Higiene e cuidados
# pessoais; Assistencia a saude; Educacao; Recreacao e cultura; Fumo;
# Servicos pessoais; Despesas diversas. Nao supor equivalencia com grupos IPCA.
catalogo <- despesas |>
  dplyr::group_by(.data$FONTE_POF, .data$TIPO_FONTE, .data$QUADRO,
                  .data$CODIGO_POF, .data$DESCRICAO_POF,
                  .data$COD_SNIPC, .data$DESCRICAO_SNIPC,
                  .data$SITUACAO_MAPA) |>
  dplyr::summarise(N_REGISTROS = dplyr::n(),
    VALOR_EXPANDIDO = sum(.data$DESPESA_EXPANDIDA_ANUAL, na.rm = TRUE),
    .groups = "drop") |>
  dplyr::arrange(dplyr::desc(.data$VALOR_EXPANDIDO)) |>
  dplyr::mutate(GRUPO_POF_11_A_VALIDAR = NA_character_)
readr::write_csv(catalogo, file.path(saida, "04_CATALOGO_PARA_11_GRUPOS.csv"))

quadros <- despesas |>
  dplyr::group_by(.data$FONTE_POF, .data$TIPO_FONTE, .data$QUADRO) |>
  dplyr::summarise(N_CODIGOS_POF = dplyr::n_distinct(.data$CODIGO_POF),
    N_REGISTROS = dplyr::n(),
    DESPESA_EXPANDIDA = sum(.data$DESPESA_EXPANDIDA_ANUAL, na.rm = TRUE),
    .groups = "drop") |>
  dplyr::arrange(.data$FONTE_POF, .data$QUADRO)
readr::write_csv(quadros, file.path(saida, "05_QUADROS_POF_PARA_CLASSIFICACAO.csv"))

# 5. RESUMO PARA ENVIAR NA CONVERSA -----------------------------------------
resumo <- c(
  "TCC - POF 2017-2018 - ESTADO RJ (UF 33)",
  paste0("UCs com renda e peso validos: ", nrow(uc_validas)),
  paste0("Unidades com problemas: ", nrow(problemas_uc)),
  paste0("Codigos POF -> SNIPC na planilha oficial: ", nrow(mapa)),
  paste0("Registros de despesas UF=33: ", nrow(despesas)),
  paste0("Codigos observados no catalogo: ", nrow(catalogo)),
  paste0("Arquivos de despesas processados: ", paste(names(conjuntos), collapse = ", ")),
  "Os 11 tipos POF ainda NAO foram atribuidos. Sua validacao usa os arquivos 04 e 05.",
  "Nao foram aplicadas exclusoes nem as regras de redistribuicao da aba oficial.",
  "Nao foram calculados ainda pesos de consumo, nem indices de inflacao.")
writeLines(resumo, file.path(saida, "00_RESUMO_EXECUCAO.txt"))
cat("\n", paste(resumo, collapse = "\n"), "\n\n", sep = "")
cat("CONCLUIDO. Para prosseguir, envie:\n",
    "00_RESUMO_EXECUCAO.txt, 01_FAIXAS_DE_RENDA_RJ.csv, ",
    "03_COBERTURA_DO_TRADUTOR.csv e 05_QUADROS_POF_PARA_CLASSIFICACAO.csv.\n",
    "Se necessario, enviaremos depois o catalogo 04.\n", sep = "")
