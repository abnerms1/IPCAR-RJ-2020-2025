# IPCAR-RJ | Inflação por faixas de renda no Estado do Rio de Janeiro (2020–2025)

Este repositório reúne **cinco scripts em R**, executados em sequência, empregados na construção de um índice experimental de preços ao consumidor por sete faixas de renda para o Estado do Rio de Janeiro (IPCAR-RJ). As cestas de referência foram estimadas com os microdados da **POF 2017–2018**, e a evolução dos preços considera as séries mensais do **IPCA regional do Rio de Janeiro** entre janeiro de 2020 e dezembro de 2025.

> **IMPORTANTE:** Os scripts disponibilizados aqui preservam as escolhas e operações da versão utilizada na monografia. As adaptações de publicação se restringem às indicações de caminhos e a verificações iniciais de existência das pastas. Esta disponibilização não converte o IPCAR-RJ em um índice oficial do IBGE e não elimina as limitações metodológicas discutidas na monografia.

## 1. Preparação do computador

- Instale **R 4.1 ou superior** (necessário para o operador nativo `|>`) e, preferencialmente, o RStudio. Os scripts instalam automaticamente os pacotes R ausentes quando há internet.
- Obtenha diretamente do **IBGE** os microdados da POF 2017–2018 no formato de arquivos `.txt` compatíveis com o programa de leitura oficial utilizado no estudo. Os arquivos esperados pelo Script 01 são:
  - `MORADOR.txt`
  - `DESPESA_COLETIVA.txt`
  - `CADERNETA_COLETIVA.txt`
  - `DESPESA_INDIVIDUAL.txt`
  - `ALUGUEL_ESTIMADO.txt`
  - `SERVICO_NAO_MONETARIO_POF2.txt`
  - `SERVICO_NAO_MONETARIO_POF4.txt`
- Na **mesma pasta** dos arquivos `.txt`, coloque a planilha oficial **`Tabela_de_correspondencia_despesas_POF_2017_2018.xlsx`**. O Script 01 também reconhece o nome alternativo terminado em `(1).xlsx`; as abas e o conteúdo precisam seguir o arquivo original (aba `tab correspondência`).
- Para executar o **Script 05**, coloque em outra pasta os **seis arquivos anuais** `IPCA (2020).xlsx`, `IPCA (2021).xlsx`, ..., `IPCA (2025).xlsx`, **no mesmo formato e com as mesmas abas utilizados no estudo**. Não basta renomear uma planilha distinta: a rotina faz a leitura de posições e abas específicas desses arquivos. Consulte a origem/documentação da pesquisa para obter as versões utilizadas.

Para **reproduzir a monografia**, não omita os registros de aluguel estimado e serviços não monetários. O Script 01 admite ausência de alguns arquivos complementares para fins exploratórios, mas isso produziria uma cesta **diferente** da cesta do estudo.

## 2. Onde editar os caminhos

**Em cada um dos cinco scripts**, procure, perto do início, o bloco de comentários **`CONFIGURACAO DOS CAMINHOS`**. Substitua **somente** o valor entre aspas desta linha pelo caminho real da sua pasta de microdados:

```r
pasta_microdados <- "C:/ALTERE_AQUI/POF/Dados_20230713"
```

Exemplos:

```r
# Windows (use / no lugar de \\):
pasta_microdados <- "C:/Users/seu_usuario/Documentos/POF/Dados_20230713"

# Linux:
pasta_microdados <- "/home/seu_usuario/POF/Dados_20230713"
```

Use **o mesmo caminho** nos Scripts 01, 02, 03, 04 e 05. Não é necessário editar os demais `file.path(...)`, pois eles localizam os resultados das etapas anteriores automaticamente.

No **Script 05**, configure também:

```r
pasta_ipca <- "C:/ALTERE_AQUI/IPCA"
```

Este caminho deve apontar para a pasta das **seis planilhas anuais do IPCA**, **não** para um arquivo `.xlsx` individual.

Se o caminho não existir, o script interromperá a execução com uma mensagem solicitando sua correção.

## 3. Ordem de execução (não pule etapas)

| Ordem | Script | Resultado principal |
| --- | --- | --- |
| 01 | `01_Preparacao_POF_RJ_Renda_Dicionario_11_Grupos.R` | Identifica UCs, sete faixas e despesas; importa tabela POF–SNIPC. |
| 02 | `02_Correspondencia_e_Pesos_Iniciais_POF_RJ.R` | Corrige a chave de correspondência entre despesas e códigos do SNIPC; organiza diagnósticos. |
| 03 | `03_Pesos_11_Tipos_POF_RJ.R` | Reagrupa as despesas em onze categorias e calcula os pesos por faixa e subitem. |
| 04 | `04_Inflacao_por_Faixa_POF_RJ_2020_2025_CORRIGIDO_v2.R` | Consulta o SIDRA/IBGE, aplica as variações mensais de preço e gera os índices. |
| 05 | `05_IPCAR_RJ_Graficos_Tabela_2020_2025_AMPLIADO.R` | Compara os indicadores com as planilhas regionais oficiais, gera tabelas, gráficos e decomposições. |

No **RStudio**, abra cada arquivo e clique em **Source** para executar o script inteiro. Alternativamente, a partir de um terminal na pasta deste repositório, utilize `Rscript <nome-do-script.R>`, respeitando a ordem acima.

O **Script 04 requer acesso à internet** para baixar dados da API do SIDRA/IBGE na primeira execução, salvo quando já existir um cache apropriado gerado anteriormente. As respostas da API podem sofrer alterações/revisões ao longo do tempo; a reprodução numérica exata depende das mesmas versões das bases de entrada.

A versão **AMPLIADO** do Script 05 é a versão final utilizada, incluindo os gráficos de faixas extremas e as contribuições por grupo. A versão anterior **ATUALIZADO** não integra o conjunto de cinco scripts disponibilizado aqui.

## 4. Onde os resultados são gravados

Os scripts preservam a estrutura original e criam automaticamente estas pastas **dentro da pasta indicada em `pasta_microdados`**:

```text
Dados_20230713/
  MORADOR.txt
  DESPESA_COLETIVA.txt
  CADERNETA_COLETIVA.txt
  DESPESA_INDIVIDUAL.txt
  ALUGUEL_ESTIMADO.txt
  SERVICO_NAO_MONETARIO_POF2.txt
  SERVICO_NAO_MONETARIO_POF4.txt
  Tabela_de_correspondencia_despesas_POF_2017_2018.xlsx
  TCC_RJ_NOVO/
    01_preparacao/
    02_pesos/
    03_pesos_11_tipos/
    04_inflacao_faixa/
    05_graficos_comparativos/
```

Os scripts subsequentes leem os arquivos intermediários dessas pastas, por isso os nomes dos arquivos e subdiretórios **não devem ser modificados** sem uma revisão conjunta de todos os scripts.

## 5. Conferências dos resultados

Os seguintes números são referências **aproximadas**, de acordo com os resultados da monografia, para ajudar a identificar erros de configuração ou divergências nas fontes:

- **Script 01:** **3.191 unidades de consumo** válidas para o RJ; **144.444 registros** de despesas carregados.
- **Script 03:** **sete faixas de renda**, **onze grupos em cada faixa**, pesos somando **100% por faixa**. A cobertura das despesas incluídas varia aproximadamente entre **85,48% e 97,37%** por faixa.
- **Script 04:** **72 meses**, de janeiro de 2020 a dezembro de 2025; IPCAR acumulado até dezembro de 2025 de aproximadamente **38,70%** para até 2 salários mínimos e **38,51%** para mais de 25 salários mínimos.
- **Script 05:** IPCAR geral acumulado **aproximadamente 38,21%**; IPCA regional recomposto a partir das planilhas mensais do estudo **aproximadamente 36,09%**. O Script 05 contém as comparações com o IPCA e as decomposições por grupo.

As pequenas diferenças de arredondamento podem ser esperadas. Divergências grandes exigem revisar **versões dos arquivos, disponibilização e cobertura de microdados, planilhas do IPCA, execução completa das etapas e os logs de pendências e cobertura**. A soma de pesos igual a 100% **não** indica cobertura integral dos registros da POF.

## 6. Advertências metodológicas

Este projeto estima a inflação de **cestas de referência da POF 2017–2018** e não acompanha as mesmas famílias ao longo dos anos. As despesas contemplam todo o Estado do Rio de Janeiro (UF = 33), enquanto os preços vêm da **área regional pesquisada pelo IPCA**. O código também utiliza registros **não monetários e aluguel estimado**, redistribuição proporcional de certos agregados e aproximações por item/subgrupo/grupo quando uma série direta de subitem não está disponível.

A faixa superior possui apenas **67 unidades de consumo observadas** no recorte estudado, menor cobertura das despesas e maior necessidade de aproximações de preços; suas comparações com a faixa inferior devem ser interpretadas com cautela. O exercício é empírico-descritivo, não um teste causal nem uma réplica integral do método oficial do IBGE.

## 7. Publicação e preservação

O repositório contém somente os scripts e esta documentação. Não publique arquivos locais temporários do R nem microdados brutos sem verificar suas condições de redistribuição. Para tornar a monografia **numericamente reprodutível ao longo do tempo**, é recomendável arquivar junto da versão publicada do repositório as seis planilhas oficiais exatas utilizadas e uma cópia dos arquivos intermediários e/ou caches usados na execução, observadas as permissões de redistribuição. É possível criar uma versão marcada (`v1.0`) e arquivá-la no Zenodo para obter um DOI persistente.
