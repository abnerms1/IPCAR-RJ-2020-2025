# =============================================================================
# TCC UFRJ | POF 2017-2018 | ESTADO DO RIO DE JANEIRO
# SCRIPT 03: PESOS NOS 11 TIPOS DE DESPESAS DA POF, COMO EM CURITIBA
# Entrada: SOMENTE o RDS produzido pelo Script 02 (nao rele microdados .txt).
# Objetivo: estimar a cesta de cada uma das 7 faixas; SEM calcular IPCA ainda.
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
pasta_02 <- file.path(pasta_microdados, "TCC_RJ_NOVO", "02_pesos")
saida <- file.path(pasta_microdados, "TCC_RJ_NOVO", "03_pesos_11_tipos")
dir.create(saida, recursive=TRUE, showWarnings=FALSE)

for (p in c("dplyr", "readr", "tidyr")) {
  if (!requireNamespace(p, quietly=TRUE))
    install.packages(p, repos="https://cloud.r-project.org")
}
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr)})

entrada <- file.path(pasta_02,"02_DESPESAS_COM_TRADUTOR_CORRIGIDO.rds")
if (!file.exists(entrada)) stop("Arquivo ausente: ",entrada)
dados <- readRDS(entrada)
req <- c("FONTE_POF","FAIXA_RENDA","QUADRO","CODIGO_PRODUTO_POF",
         "DESCRICAO_POF","COD_SNIPC","DESCRICAO_SNIPC", "STATUS_MAPA",
         "DESPESA_EXPANDIDA_ANUAL")
if (!all(req %in% names(dados)))
  stop("Faltam colunas: ",paste(setdiff(req,names(dados)),collapse=", "))

# Todos os cinco registros de aquisicoes (coletivas, individuais e de servicos
# nao monetarios) sao disjuntos. O aluguel estimado esta em arquivo separado.
fontes_consumo <- c("DESPESA_COLETIVA","CADERNETA_COLETIVA",
                    "DESPESA_INDIVIDUAL","SERVICO_NAO_MONETARIO_POF2",
                    "SERVICO_NAO_MONETARIO_POF4")
base <- dados |>
  filter(.data$FONTE_POF %in% fontes_consumo,
         is.finite(.data$DESPESA_EXPANDIDA_ANUAL),
         .data$DESPESA_EXPANDIDA_ANUAL > 0) |>
  mutate(QUADRO=as.character(.data$QUADRO),
         CODIGO_PRODUTO_POF=as.character(.data$CODIGO_PRODUTO_POF),
         COD_SNIPC=as.character(.data$COD_SNIPC),
         GRUPO_IPCA=substr(.data$COD_SNIPC,1,2),
         TIPO_REGISTRO=case_when(
           .data$FONTE_POF=="CADERNETA_COLETIVA" ~ "CADERNETA",
           .data$FONTE_POF %in% c("DESPESA_COLETIVA", "SERVICO_NAO_MONETARIO_POF2") ~ "COLETIVA",
           TRUE ~ "INDIVIDUAL"))

# Classificacao por finalidade na POF, nao por simples substituicao de
# nomes dos nove grupos SNIPC. Quadro 41 (viagens), 32 (papelaria),
# 37 (tecidos e enxoval) e 44 (telefonia) necessitam de separacoes.
# Rotulos numerados mantem a ordem nas tabelas e nos graficos.
base <- base |>
  mutate(TIPO_POF_11=case_when(
    .data$TIPO_REGISTRO=="CADERNETA" & .data$GRUPO_IPCA=="01" ~ "01 Alimentacao",
    .data$TIPO_REGISTRO=="CADERNETA" & .data$GRUPO_IPCA %in% c("02","03") ~ "02 Habitacao",
    .data$TIPO_REGISTRO=="CADERNETA" & .data$GRUPO_IPCA=="06" ~ "05 Higiene e cuidados pessoais",
    .data$TIPO_REGISTRO=="CADERNETA" & .data$GRUPO_IPCA=="07" ~ "11 Despesas diversas",
    .data$TIPO_REGISTRO=="COLETIVA" & .data$QUADRO=="13" ~ "11 Despesas diversas",
    .data$TIPO_REGISTRO=="COLETIVA" ~ "02 Habitacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="21" ~ "09 Fumo",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO %in% c("22","25","26","40","45","47","48") ~ "11 Despesas diversas",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO %in% c("23","33","50","51") ~ "04 Transporte",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="24" ~ "01 Alimentacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO %in% c("27","28","43") ~ "08 Recreacao e cultura",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO %in% c("29","42") ~ "06 Assistencia a saude",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="30" ~ "05 Higiene e cuidados pessoais",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="31" ~ "10 Servicos pessoais",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="32" &
      .data$CODIGO_PRODUTO_POF %in% c("32017","32018") ~ "08 Recreacao e cultura",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="32" ~ "07 Educacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="37" & .data$GRUPO_IPCA=="03" ~ "02 Habitacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO %in% c("34","35","36","37","38","46") ~ "03 Vestuario",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="39" ~ "02 Habitacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="41" & .data$GRUPO_IPCA=="01" ~ "01 Alimentacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="41" & .data$GRUPO_IPCA=="05" ~ "04 Transporte",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="41" ~ "08 Recreacao e cultura",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="44" &
      .data$CODIGO_PRODUTO_POF %in% c("44004","44005","44006") ~ "08 Recreacao e cultura",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="44" ~ "02 Habitacao",
    .data$TIPO_REGISTRO=="INDIVIDUAL" & .data$QUADRO=="49" ~ "07 Educacao",
    TRUE ~ NA_character_))

# Apenas subitens oficiais 01..09 entrarao diretamente. Outros codigos
# permanecem identificados; nao tratamos codigo auxiliar 10 como 10o grupo.
diretos <- base |>
  filter(.data$STATUS_MAPA=="MAPEADO_NOVE_GRUPOS",
         !is.na(.data$TIPO_POF_11)) |>
  group_by(.data$FAIXA_RENDA,.data$FONTE_POF,.data$QUADRO,
           .data$COD_SNIPC,.data$DESCRICAO_SNIPC,.data$TIPO_POF_11) |>
  summarise(DESPESA_DIRETA=sum(.data$DESPESA_EXPANDIDA_ANUAL), .groups="drop")
if (!nrow(diretos)) stop("Nenhuma despesa diretamente mapeada.")

# Redistribuicao SIMPLIFICADA dos registros explicitamente denominados AGREGADO
# e marcados como CODIGO_AUXILIAR: dentro da MESMA faixa, arquivo e quadro,
# proporcional ao gasto observado nos subitens ja mapeados naquele quadro.
# Isto NAO reproduz integralmente o algoritmo oficial de redistribuicao do IBGE.
aux <- base |>
  filter(.data$STATUS_MAPA=="CODIGO_AUXILIAR",
         grepl("AGREGADO",toupper(dplyr::coalesce(.data$DESCRICAO_POF,"")))) |>
  group_by(.data$FAIXA_RENDA,.data$FONTE_POF,.data$QUADRO) |>
  summarise(VALOR_AGREGADO=sum(.data$DESPESA_EXPANDIDA_ANUAL), .groups="drop")

# A tabela de subitens tem diversas linhas por quadro; usamos o total direto
# desse quadro como denominador para distribuir UMA VEZ o gasto agregado.
com_agregados <- diretos |>
  group_by(.data$FAIXA_RENDA,.data$FONTE_POF,.data$QUADRO) |>
  mutate(TOTAL_QUADRO=sum(.data$DESPESA_DIRETA)) |>
  ungroup() |>
  left_join(aux,by=c("FAIXA_RENDA","FONTE_POF","QUADRO")) |>
  mutate(DESPESA_AGREGADA=if_else(.data$TOTAL_QUADRO>0,
      .data$DESPESA_DIRETA/.data$TOTAL_QUADRO *
        dplyr::coalesce(.data$VALOR_AGREGADO,0), 0),
      DESPESA_FINAL=.data$DESPESA_DIRETA+.data$DESPESA_AGREGADA)

# Aluguel estimado da POF: despesa nao monetaria; tipo Habitacao.
# Mantem o subitem residencial no nivel mais especifico disponivel.
alugueis <- dados |>
  filter(.data$FONTE_POF=="ALUGUEL_ESTIMADO",
         is.finite(.data$DESPESA_EXPANDIDA_ANUAL),
         .data$DESPESA_EXPANDIDA_ANUAL>0) |>
  group_by(.data$FAIXA_RENDA) |>
  summarise(DESPESA_FINAL=sum(.data$DESPESA_EXPANDIDA_ANUAL), .groups="drop") |>
  mutate(FONTE_POF="ALUGUEL_ESTIMADO", QUADRO="00", COD_SNIPC="02101001",
         DESCRICAO_SNIPC="Aluguel residencial (estimado)",
         TIPO_POF_11="02 Habitacao", DESPESA_DIRETA=0, DESPESA_AGREGADA=0)

subitens <- bind_rows(
  com_agregados |>
    select(.data$FAIXA_RENDA,.data$COD_SNIPC,.data$DESCRICAO_SNIPC,
           .data$TIPO_POF_11,.data$DESPESA_FINAL,.data$DESPESA_DIRETA,
           .data$DESPESA_AGREGADA),
  alugueis |>
    select(.data$FAIXA_RENDA,.data$COD_SNIPC,.data$DESCRICAO_SNIPC,
           .data$TIPO_POF_11,.data$DESPESA_FINAL,.data$DESPESA_DIRETA,
           .data$DESPESA_AGREGADA)) |>
  group_by(.data$FAIXA_RENDA,.data$TIPO_POF_11,.data$COD_SNIPC) |>
  summarise(DESPESA_EXPANDIDA=sum(.data$DESPESA_FINAL),
            PARCELA_DIRETA=sum(.data$DESPESA_DIRETA),
            PARCELA_AGREGADA=sum(.data$DESPESA_AGREGADA),.groups="drop") |>
  group_by(.data$FAIXA_RENDA) |>
  mutate(PESO_SUBITEM_PCT=100*.data$DESPESA_EXPANDIDA / sum(.data$DESPESA_EXPANDIDA)) |>
  ungroup()

if (!nrow(subitens) || anyNA(subitens$PESO_SUBITEM_PCT) ||
    any(subitens$PESO_SUBITEM_PCT<0)) stop("Erro no calculo de pesos dos subitens.")

pesos11 <- subitens |>
  group_by(.data$FAIXA_RENDA,.data$TIPO_POF_11) |>
  summarise(DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA),
            PESO_GRUPO_PCT=sum(.data$PESO_SUBITEM_PCT),.groups="drop") |>
  arrange(.data$FAIXA_RENDA,.data$TIPO_POF_11)

fechamento <- pesos11 |>
  group_by(.data$FAIXA_RENDA) |>
  summarise(N_TIPOS=n(), PESO_TOTAL_PCT=sum(.data$PESO_GRUPO_PCT), .groups="drop")
if (nrow(fechamento)!=7 || any(fechamento$N_TIPOS!=11) ||
    any(abs(fechamento$PESO_TOTAL_PCT-100)>1e-7))
  stop("Nao existem 11 grupos positivos em todas as faixas ou pesos nao fecham.")

# Cobertura: proporcao da despesa pesquisada nos arquivos utilizados que
# entrou em alguma classe da cesta (nao confundir com elegibilidade IPCA!).
base_total <- base |>
  group_by(.data$FAIXA_RENDA) |>
  summarise(DESPESAS_AQUISICOES=sum(.data$DESPESA_EXPANDIDA_ANUAL),.groups="drop")
extra_total <- alugueis |>
  select(.data$FAIXA_RENDA, ALUGUEL_ESTIMADO=.data$DESPESA_FINAL)
cobertura <- base_total |>
  left_join(extra_total,by="FAIXA_RENDA") |>
  left_join(pesos11 |>
    group_by(.data$FAIXA_RENDA) |>
    summarise(DESPESA_INCLUIDA=sum(.data$DESPESA_EXPANDIDA),.groups="drop"),
    by="FAIXA_RENDA") |>
  mutate(ALUGUEL_ESTIMADO=coalesce(.data$ALUGUEL_ESTIMADO,0),
         DESPESA_OBSERVADA=.data$DESPESAS_AQUISICOES+.data$ALUGUEL_ESTIMADO,
         NAO_INCLUIDA=.data$DESPESA_OBSERVADA-.data$DESPESA_INCLUIDA,
         COBERTURA_BRUTA_PCT=100*.data$DESPESA_INCLUIDA/.data$DESPESA_OBSERVADA)

pendentes <- base |>
  filter(.data$STATUS_MAPA!="MAPEADO_NOVE_GRUPOS" |
           is.na(.data$TIPO_POF_11)) |>
  group_by(.data$FONTE_POF,.data$QUADRO,.data$CODIGO_PRODUTO_POF,
           .data$DESCRICAO_POF,.data$STATUS_MAPA) |>
  summarise(DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA_ANUAL),.groups="drop") |>
  arrange(desc(.data$DESPESA_EXPANDIDA))

# Este relatorio descreve possiveis erros de classificar codigos oficiais
# mapeados: esperado ZERO. Nao confundir com pendencias nao mapeadas.
sem_tipo <- base |>
  filter(.data$STATUS_MAPA=="MAPEADO_NOVE_GRUPOS",is.na(.data$TIPO_POF_11)) |>
  group_by(.data$FONTE_POF,.data$QUADRO,.data$COD_SNIPC) |>
  summarise(DESPESA_EXPANDIDA=sum(.data$DESPESA_EXPANDIDA_ANUAL),.groups="drop")

write_csv(subitens,file.path(saida,"03_01_PESOS_SUBITENS_POF_11.csv"))
write_csv(pesos11,file.path(saida,"03_02_PESOS_11_TIPOS_POR_FAIXA.csv"))
write_csv(fechamento,file.path(saida,"03_03_FECHAMENTO.csv"))
write_csv(cobertura,file.path(saida,"03_04_COBERTURA_BRUTA.csv"))
write_csv(head(pendentes,80),file.path(saida,"03_05_PENDENCIAS_RELEVANTES.csv"))
write_csv(sem_tipo,file.path(saida,"03_06_MAPEADOS_SEM_TIPO.csv"))
saveRDS(subitens,file.path(saida,"03_PESOS_SUBITENS_POF_11.rds"))

# Um grafico de PESOS (nao de inflacao). A inflacao entra no proximo script.
nomes_faixas <- c("1 <=2","2 2-3","3 3-6","4 6-10","5 10-15","6 15-25","7 >25")
mat <- pesos11 |>
  mutate(F=as.integer(sub(" .*","",.data$FAIXA_RENDA)),
         G=as.integer(sub(" .*","",.data$TIPO_POF_11))) |>
  select(.data$F,.data$G,.data$PESO_GRUPO_PCT) |>
  tidyr::pivot_wider(names_from=.data$G,values_from=.data$PESO_GRUPO_PCT,
                     values_fill=0) |>
  arrange(.data$F)
if (nrow(mat)==7 && ncol(mat)==12) {
 m <- as.matrix(mat[,as.character(1:11)])
 png(file.path(saida,"03_07_GRAFICO_PESOS_11_TIPOS.png"),width=1750,height=1050,res=150)
 op <- par(mar=c(5,5,4,16),xpd=TRUE)
 barplot(t(m),names.arg=nomes_faixas,col=grDevices::hcl.colors(11,"Set 3"),
         border=NA,ylim=c(0,105),xlab="Faixa de renda em SM de 2018",
         ylab="Participacao na cesta (%)",main="POF 2017-2018 | RJ: pesos por faixa de renda")
 legend("topright",inset=c(-0.64,0),legend=sort(unique(pesos11$TIPO_POF_11)),
        fill=grDevices::hcl.colors(11,"Set 3"),cex=.66,bty="n")
 par(op);dev.off()
}

resumo <- c(
  "TCC RJ | SCRIPT 03 | PESOS DAS SETE FAIXAS NOS ONZE TIPOS POF",
  paste("Linhas de despesas dos 5 registros considerados:",nrow(base)),
  paste("Despesas diretamente mapeadas (R$):",round(sum(diretos$DESPESA_DIRETA),2)),
  paste("Gastos agregados redistribuidos (R$):",round(sum(com_agregados$DESPESA_AGREGADA),2)),
  paste("Aluguel estimado incluido (R$):",round(sum(alugueis$DESPESA_FINAL),2)),
  paste("Cobertura BRUTA minima por faixa:",round(min(cobertura$COBERTURA_BRUTA_PCT),1),"%; maxima:",round(max(cobertura$COBERTURA_BRUTA_PCT),1),"%"),
  paste("Codigos mapeados sem classificacao:",nrow(sem_tipo)),
  "Cada faixa possui 11 pesos somando 100%.",
  "LIMITACOES: regras proporcionais locais apenas para registros AGREGADO, nao replica integral das regras do IBGE; codigos sem mapa/auxiliares nao agregados nao sao incluidos; mudancas de cesta ao longo dos anos nao sao medidas; preços regionais do IPCA nao cobrem todo o Estado.",
  "Os 11 tipos seguem uma classificacao economica explicita por quadro/produto POF. Casos mistos foram repartidos por produto/SNIPC; nao equivalem aos 9 grupos do IPCA.",
  "PROXIMO PASSO: aplicar variacoes mensais do IPCA regional a esses pesos para 2020-2025, depois fazer graficos de inflacao."
)
writeLines(resumo,file.path(saida,"03_RESUMO.txt"))
cat(paste(resumo,collapse="\n"),"\nResultados em: ",saida,"\n",sep="")
