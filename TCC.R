################################################################################
# UNIVERSIDADE FEDERAL DE SÃO CARLOS
# CENTRO DE CIÊNCIAS EXATAS E DE TECNOLOGIA
# DEPARTAMENTO DE ESTATÍSTICA
#
# SCRIPT MESTRE CONSOLIDADO DE PRODUÇÃO - MONOGRAFIA FINAL DE TCC
# Autor: Gustavo Marques de Matos Baggi
# Orientador: Rafael Izbicki
# Data: Maio de 2026
#
# Estrutura do Script:
#   0.0. Preparação, Dependências e Configurações Globais
#   2.0. Engenharia de Coleta de Dados (Web Scraping Estável via HTML)
#   3.0. Análise Descritiva e Exploratória dos Dados (Capítulo 3)
#   4.0. Pipeline de Modelagem Preditiva de Regressão Discreta (Capítulo 4)
#   5.0. Avaliação de Desempenho e Diagnósticos Visuais (Capítulo 5)
#   6.0. Métodos de Aprimoramento Preditivo: Stacking Ensemble (Capítulo 6)
#   7.0. Modelagem Binária: Classificação de Ocorrência de Lesão (Capítulo 7)
#   8.0. Validação Externa Temporal: Projeção de Janela Futura 24/25 (Capítulo 8)
################################################################################

# ==============================================================================
# --- 0.0. PREPARAÇÃO, DEPENDÊNCIAS E CONFIGURAÇÕES GLOBAIS
# ==============================================================================

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  # 1. Manipulação, Estruturação e Raspagem Web
  tidyverse, rvest, httr, lubridate, readxl, writexl, forcats, stringr, countrycode,
  
  # 2. Análise Descritiva e Geração de Gráficos Vetoriais
  skimr, ggcorrplot, patchwork, gridExtra, grid, scales, cowplot,
  
  # 3. Modelagem Estatística e Machine Learning (Framework Tidymodels)
  tidymodels, vip,
  
  # 4. Motores de Ajuste Algorítmico (Engines)
  poissonreg, MASS, glmnet, rpart, ranger, xgboost, kernlab, kknn, nnet,
  
  # 5. Infraestrutura de Combinação Meta-Preditiva
  stacks
)

# Configuração Estética Global de Temas - Padrão de Identidade Visual da Monografia
theme_set(theme_minimal(base_size = 14) + 
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(size = 21),
              axis.text = element_text(size = 18),
              legend.title = element_text(face = "bold", size = 21),
              legend.text = element_text(size = 18)
            ))

# Definição dos Vetores Institucionais de Cores do Trabalho
cor_azul     <- "#619cff"
cor_vermelho <- "#f87067"

# Chave de Fluxo Operacional: TRUE para Web Scraping ativo / FALSE para Leitura Local
RODAR_SCRAPING <- FALSE 


# ==============================================================================
# --- CAPÍTULO 2: COLETA E TRATAMENTO DOS DADOS (WEB SCRAPING ESTÁVEL VIA HTML)
# ==============================================================================

print("--- Iniciando Capítulo 2: Engenharia de Extração e Tratamento ---")

# --- 2.1. Funções Especialistas de Extração Estrutural Baseadas em Tags HTML ---

extrair_dados_perfil <- function(link) {
  perfil <- tryCatch(read_html(link), error = function(e) return(NULL))
  if (is.null(perfil)) {
    return(tibble(altura = NA_real_, idade = NA_integer_, pe_preferido = NA_character_, 
                  posicao = NA_character_, nacionalidade = NA_character_))
  }
  
  altura <- perfil %>%
    html_elements(xpath = "//span[contains(text(), 'Height')]/following-sibling::span[1]") %>%
    html_text(trim = TRUE) %>% str_replace(",", ".") %>% str_replace_all("[[:space:]]", "") %>% str_remove("m$") %>% as.numeric()
  
  idade <- perfil %>%
    html_elements(xpath = "//span[contains(text(), 'Date of birth')]/following-sibling::span[1]") %>%
    html_text(trim = TRUE) %>% str_extract("\\(\\d+\\)") %>% str_remove_all("[\\(\\)]") %>% as.integer()
  
  pe_preferido <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Foot')]/following-sibling::span[1]") %>% html_text(trim = TRUE)
  posicao <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Position')]/following-sibling::span[1]") %>% html_text(trim = TRUE)
  
  nacionalidade <- perfil %>%
    html_elements(xpath = "//span[contains(text(), 'Citizenship')]/following-sibling::span[1]//img") %>%
    html_attr("title") %>% paste(collapse = ", ")
  
  tibble(altura = altura, idade = idade, pe_preferido = pe_preferido, posicao = posicao, nacionalidade = nacionalidade)
}

extrair_lesoes_jogador <- function(link, temporada) {
  link_lesoes <- str_replace(link, "/profil/", "/verletzungen/")
  pagina_lesoes <- tryCatch(read_html(link_lesoes), error = function(e) return(NULL))
  if (is.null(pagina_lesoes)) return(tibble(n_lesoes = NA_integer_, dias_lesionado = NA_real_, jogos_perdidos = NA_integer_))
  
  tabela_node <- pagina_lesoes %>% html_element(xpath = '//table[contains(@class, "items")]')
  if (is.na(tabela_node) || is.null(tabela_node)) return(tibble(n_lesoes = 0, dias_lesionado = 0, jogos_perdidos = 0))
  
  tabela_lesoes <- tryCatch(html_table(tabela_node, fill = TRUE), error = function(e) return(NULL))
  if (is.null(tabela_lesoes)) return(tibble(n_lesoes = 0, dias_lesionado = 0, jogos_perdidos = 0))
  
  colnames(tabela_lesoes) <- make.unique(make.names(colnames(tabela_lesoes)))
  temporada_str <- paste0(substr(temporada, 3, 4), "/", substr(temporada + 1, 3, 4))
  
  tabela_filtrada <- tabela_lesoes %>%
    filter(grepl(temporada_str, Season)) %>%
    mutate(
      Days = as.numeric(str_extract(Days, "\\d+")),
      GamesMissed = as.numeric(str_extract(Games.missed, "\\d+"))
    )
  
  tibble(
    n_lesoes = nrow(tabela_filtrada),
    dias_lesionado = sum(tabela_filtrada$Days, na.rm = TRUE),
    jogos_perdidos = sum(tabela_filtrada$GamesMissed, na.rm = TRUE)
  )
}

extrair_dados_jogador_estatisticas <- function(link, temporada) {
  nome_jogador <- stringr::str_extract(link, "com/(.*?)/") %>% stringr::str_remove_all("com/|/")
  id_jogador <- stringr::str_extract(link, "spieler/\\d+") %>% stringr::str_remove("spieler/")
  
  extrair_stats <- function(season) {
    url_stats <- paste0("https://www.transfermarkt.com/", nome_jogador, "/leistungsdaten/spieler/", id_jogador, "/plus/0?saison=", season)
    pagina_stats <- tryCatch(read_html(url_stats), error = function(e) return(NULL))
    if (is.null(pagina_stats)) return(list(jogos = 0, minutos = 0))
    
    tabela_node <- pagina_stats %>% html_element(xpath = "//table[contains(@class, 'items')]")
    if (is.null(tabela_node) || inherits(tabela_node, "xml_missing")) return(list(jogos = 0, minutos = 0))
    
    tabela_stats <- tryCatch(html_table(tabela_node, fill = TRUE), error = function(e) return(NULL))
    if (is.null(tabela_stats)) return(list(jogos = 0, minutos = 0))
    
    colnames(tabela_stats) <- make.unique(make.names(colnames(tabela_stats)))
    linha_total <- tabela_stats %>% filter(stringr::str_detect(tabela_stats[[1]], regex("Total", ignore_case = TRUE)))
    
    if (nrow(linha_total) > 0) {
      list(jogos = as.numeric(gsub("[^0-9]", "", linha_total[[4]][1])), minutos = as.numeric(gsub("[^0-9]", "", linha_total[[ncol(linha_total)]][1])))
    } else {
      list(jogos = 0, minutos = 0)
    }
  }
  
  stats_ant <- extrair_stats(temporada - 1)
  stats_atual <- extrair_stats(temporada)
  
  tibble(
    jogos_temporada_anterior = replace_na(stats_ant$jogos, 0), minutos_temporada_anterior = replace_na(stats_ant$minutos, 0),
    jogos_temporada_atual = replace_na(stats_atual$jogos, 0), minutos_temporada_atual = replace_na(stats_atual$minutos, 0)
  )
}

extrair_n_lesoes_ano_passado <- function(link, temporada) {
  temporada_anterior <- temporada - 1
  link_lesoes <- stringr::str_replace(link, "/profil/", "/verletzungen/")
  pagina_lesoes <- tryCatch(xml2::read_html(link_lesoes), error = function(e) return(NULL))
  if (is.null(pagina_lesoes)) return(NA_integer_)
  
  tabela_node <- pagina_lesoes %>% rvest::html_element(xpath = '//table[contains(@class, "items")]')
  if (is.null(tabela_node)) return(0)
  
  tabela_lesoes <- tryCatch(rvest::html_table(tabela_node, fill = TRUE), error = function(e) return(NULL))
  if (is.null(tabela_lesoes)) return(0)
  
  colnames(tabela_lesoes) <- make.unique(make.names(colnames(tabela_lesoes)))
  temporada_str <- paste0(substr(temporada_anterior, 3, 4), "/", substr(temporada_anterior + 1, 3, 4))
  
  filtrada <- tabela_lesoes %>% dplyr::filter(grepl(temporada_str, Season))
  return(nrow(filtrada))
}

extrair_dados_elenco <- function(team_id, season, team_name) {
  url <- paste0("https://www.transfermarkt.com/", team_name, "/kader/verein/", team_id, "/saison_id/", season, "/plus/1")
  page <- read_html(url)
  
  players <- page %>%
    html_nodes(xpath = "//table[contains(@class, 'items')]/tbody/tr") %>%
    map_df(function(row) {
      node <- row %>% html_node(".hauptlink a")
      if(is.null(node)) return(NULL)
      tibble(nome = node %>% html_text(trim = TRUE), link = paste0("https://www.transfermarkt.com", node %>% html_attr("href")))
    })
  
  dados_completos <- players %>% mutate(
    perfil = map(link, function(x) { Sys.sleep(5); extrair_dados_perfil(x) }),
    lesoes = map(link, function(x) { Sys.sleep(2); extrair_lesoes_jogador(x, season) }),
    stats  = map(link, function(x) { Sys.sleep(2); extrair_dados_jogador_estatisticas(x, season) }),
    n_lesoes_ano_passado = map_int(link, function(x) { Sys.sleep(2); extrair_n_lesoes_ano_passado(x, season) })
  ) %>% 
    unnest(c(perfil, lesoes, stats, n_lesoes_ano_passado)) %>% 
    mutate(team = team_name, season = season)
  
  return(dados_completos)
}

corrigir_lesoes_zeradas <- function(df, temporada) {
  idx <- which(df$n_lesoes == 0 | is.na(df$n_lesoes))
  for (i in idx) {
    lesoes <- extrair_lesoes_jogador(df$link[i], temporada)
    if (nrow(lesoes) == 1) df[i, c("n_lesoes", "dias_lesionado", "jogos_perdidos")] <- lesoes
    Sys.sleep(5)
  }
  return(df)
}

# --- 2.2. Execução da Pipeline de ETL e Tratamento de Variáveis ---

if (RODAR_SCRAPING) {
  print("--- Iniciando Coleta Ativa dos Elencos de Forma Sequencial (20 Clubes) ---")
  # d_arsenal   <- extrair_dados_elenco(11, 2023, "arsenal-fc") %>% corrigir_lesoes_zeradas(2023)
  # d_mancity   <- extrair_dados_elenco(281, 2023, "manchester-city") %>% corrigir_lesoes_zeradas(2023)
  # [Os códigos de raspagem por equipe mantêm-se parametrizados para evitar concorrência e bloqueios]
} else {
  print("Ignorando Scraping ativo: Carregando base unificada histórica do TCC...")
  dados_brutos <- read_excel("C:/Users/bagGi/OneDrive/Documentos/TCC/dados_completos_2023_atualizado.xlsx")
}

print("Executando Higienização de Dados e Mapeamento Geográfico/Tático...")

dados <- dados_brutos %>%
  dplyr::select(
    altura, idade, nacionalidade, posicao, pe_preferido,
    jogos_temporada_anterior, minutos_temporada_anterior,
    n_lesoes_ano_passado, n_lesoes
  ) %>%
  mutate(
    nacionalidade = str_trim(str_split_fixed(nacionalidade, ",", 2)[, 1]),
    posicao = case_when(
      posicao == "Goalkeeper" | posicao == "Goleiro" ~ "Goleiro",
      posicao == "Defender - Centre-Back" | posicao == "Zagueiro" ~ "Zagueiro",
      posicao %in% c("Defender - Left-Back", "Defender - Right-Back", "Lateral") ~ "Lateral",
      str_detect(posicao, "Midfield|Meio") ~ "Meio-Campo",
      str_detect(posicao, "Attack|Atacante") ~ "Atacante",
      TRUE ~ "Outro"
    ),
    nacionalidade = case_when(
      nacionalidade %in% c("England", "Scotland", "Wales", "Northern Ireland") ~ "United Kingdom",
      nacionalidade == "Ivory Coast" ~ "Cote d'Ivoire",
      nacionalidade == "DR Congo" ~ "Democratic Republic of the Congo",
      TRUE ~ nacionalidade
    ),
    continente = countrycode(nacionalidade, origin = "country.name", destination = "continent"),
    continente = recode(continente, "Americas" = "América", "Europe" = "Europa", "Africa" = "África", "Asia" = "Ásia", "Oceania" = "Oceania"),
    continente = tidyr::replace_na(continente, "Europa"),
    continente = as.factor(continente), posicao = as.factor(posicao), pe_preferido = as.factor(pe_preferido)
  ) %>%
  mutate(continente = fct_collapse(continente, "Ásia/Oceania" = c("Ásia", "Oceania"))) %>%
  rename(n_lesoes_temporada_anterior = n_lesoes_ano_passado)

write_xlsx(dados, "dados_completos_2023_selecionados.xlsx")


# ==============================================================================
# --- CAPÍTULO 3: ANÁLISE DESCRITIVA E EXPLORATÓRIA DOS DADOS
# ==============================================================================

print("--- Iniciando Capítulo 3: Geração dos Gráficos Exploratórios ---")

# 3.2.1. Distribuição da Variável Resposta (Figura 3.1)
p_resp <- ggplot(dados, aes(x = factor(n_lesoes), fill = factor(n_lesoes))) +
  geom_bar(show.legend = FALSE, width = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 5) +
  labs(x = "Número de Lesões na Temporada 2023/24", y = "Número de Jogadores")

# 3.2.2. Distribuição das Variáveis Preditivas Numéricas (Figura 3.2)
vars_num_hist <- c("altura", "idade", "jogos_temporada_anterior", "minutos_temporada_anterior", "n_lesoes_temporada_anterior")

h1 <- make_hist("altura", "Metros")
h2 <- make_hist("idade", "Anos")
h3 <- make_hist("jogos_temporada_anterior", "Jogos")
h4 <- make_hist("minutos_temporada_anterior", "Minutos (milhares)", breaks = c(1000, 2000, 3000, 4000, 5000), labels = c("1", "2", "3", "4", "5"))
h5 <- make_hist("n_lesoes_temporada_anterior", "Lesões")
p_num <- plot_grid(plot_grid(h1, h2, h3, ncol = 3), plot_grid(NULL, h4, h5, NULL, ncol = 4, rel_widths = c(0.5, 1, 1, 0.5)), ncol = 1, nrow = 2)

# 3.2.3. Variáveis Categóricas (Painel Combinado Figura 3.3)
p1 <- dados %>% count(posicao) %>% mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(posicao, n, .desc = TRUE), y = n, fill = posicao)) + geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.1)), vjust = 1.5, color = "white", size = 3.5) + labs(x = "Posição", y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- dados %>% count(pe_preferido) %>% mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(pe_preferido, n, .desc = TRUE), y = n, fill = pe_preferido)) + geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.1)), vjust = 1.5, color = "white", size = 3.5) + labs(x = "Pé Preferido", y = NULL)

p4 <- dados %>% count(continente) %>% mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(continente, n, .desc = TRUE), y = n, fill = continente)) + geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.1)), vjust = 1.5, color = "white", size = 3.5) + labs(x = "Continente", y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

shared_y_axis <- textGrob("Número de Jogadores", rot = 90, vjust = 0.5, gp = gpar(fontsize = 18, fontface = "bold"))
g_categ <- grid.arrange(p1, p2, p4, ncol = 3, left = shared_y_axis)

# 3.2.4. Matriz de Correlação Linear (Figura 3.4)
cor_matrix <- dados %>% dplyr::select(all_of(vars_num_hist)) %>% cor(use = "complete.obs")
colnames(cor_matrix) <- rownames(cor_matrix) <- c("Altura", "Idade", "Jogos (Anterior)", "Minutos (Anterior)", "Lesões (Anterior)")
p_corr <- ggcorrplot(cor_matrix, method = "square", type = "lower", lab = TRUE, lab_size = 5, colors = c(cor_vermelho, "white", cor_azul))

# 3.2.5. Relações entre Preditores e Resposta (Figura 3.5, 3.6 e 3.7)
s1 <- make_scatter("altura", "Metros"); s2 <- make_scatter("idade", "Anos"); s3 <- make_scatter("jogos_temporada_anterior", "Jogos")
s4 <- make_scatter("minutos_temporada_anterior", "Minutos (milhares)"); s5 <- make_scatter("n_lesoes_temporada_anterior", "Lesões")
p_scatter <- plot_grid(plot_grid(s1, s2, s3, ncol = 3), plot_grid(NULL, s4, s5, NULL, ncol = 4, rel_widths = c(0.5, 1, 1, 0.5)), ncol = 1, nrow = 2)

p_box_hist <- ggplot(dados, aes(x = factor(n_lesoes_temporada_anterior), y = n_lesoes, fill = factor(n_lesoes_temporada_anterior))) + geom_boxplot(show.legend = FALSE, width = 0.6) + labs(x = "Número de Lesões Temporada Anterior", y = "Número de Lesões")
p_box_pos  <- ggplot(dados, aes(x = fct_reorder(posicao, n_lesoes, .fun = median), y = n_lesoes, fill = posicao)) + geom_boxplot(show.legend = FALSE, width = 0.6) + labs(x = "Posição", y = "Número de Lesões") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Geração de PDFs Vetoriais de Alta Resolução para Embutir no Word/LaTeX
if (!dir.exists("imagens_tcc")) dir.create("imagens_tcc")
ggsave("imagens_tcc/fig1_distribuicao_lesoes.pdf", plot = p_resp, width = 8, height = 6)
ggsave("imagens_tcc/fig2_distribuicao_numericas.pdf", plot = p_num, width = 10, height = 8)
ggsave("imagens_tcc/fig3_painel_categoricas.pdf", plot = g_categ, width = 12, height = 6)
ggsave("imagens_tcc/fig4_correlacao.pdf", plot = p_corr, width = 8, height = 8)
ggsave("imagens_tcc/fig5_scatter_numericas.pdf", plot = p_scatter, width = 10, height = 8)
ggsave("imagens_tcc/fig6_boxplot_historico.pdf", plot = p_box_hist, width = 7, height = 6)
ggsave("imagens_tcc/fig7_boxplot_posicao.pdf", plot = p_box_pos, width = 10, height = 6)


# ==============================================================================
# --- CAPÍTULO 4 & 5: WORKFLOW SETS, TUNAGEM E AVALIAÇÃO DE REGRESSÃO DISCRETA
# ==============================================================================

print("--- Iniciando Capítulo 4 e 5: Ajuste de Modelos de Regressão ---")

set.seed(793127) 
split_inicial <- initial_split(dados, prop = 3/4, strata = n_lesoes)
dados_treino <- training(split_inicial); dados_teste  <- testing(split_inicial)
folds_cv     <- vfold_cv(dados_treino, v = 10, strata = n_lesoes)

# --- 4.2. Regularização Lasso (Poisson Base) ---
recipe_lasso <- recipe(n_lesoes ~ ., data = dados_treino) %>%
  step_other(continente, threshold = 0.05) %>%
  step_mutate(n_lesoes_temporada_anterior = as.integer(n_lesoes_temporada_anterior)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors())

spec_lasso <- poisson_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")
wf_lasso   <- workflow() %>% add_recipe(recipe_lasso) %>% add_model(spec_lasso)
res_lasso  <- tune_grid(wf_lasso, resamples = folds_cv, grid = 25, metrics = metric_set(rmse))

vars_selecionadas_lasso <- finalize_workflow(wf_lasso, select_best(res_lasso, metric = "rmse")) %>% 
  fit(data = dados_treino) %>% extract_fit_parsnip() %>% tidy() %>%
  filter(estimate != 0, term != "(Intercept)") %>% pull(term)

# --- 4.3. Instanciação dos Modelos Concorrentes de Regressão ---
recipe_restrita <- recipe_lasso %>% step_select(all_outcomes(), any_of(vars_selecionadas_lasso), matches("^idade$"), starts_with("posicao"))
recipe_completa <- recipe_lasso

mod_poisson <- poisson_reg() %>% set_engine("glm") %>% set_mode("regression")
mod_bn      <- poisson_reg() %>% set_engine("glm", family = MASS::negative.binomial(theta = 1)) %>% set_mode("regression")
mod_tree    <- decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% set_engine("rpart") %>% set_mode("regression")
mod_rf      <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% set_engine("ranger", importance = "permutation") %>% set_mode("regression")
mod_xgb     <- boost_tree(tree_depth = tune(), learn_rate = tune(), trees = 1000) %>% set_engine("xgboost") %>% set_mode("regression")
mod_svm     <- svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>% set_engine("kernlab") %>% set_mode("regression")
mod_knn     <- nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>% set_engine("kknn") %>% set_mode("regression")
mod_neural  <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% set_engine("nnet", MaxNWts = 10000) %>% set_mode("regression")

wf_set <- bind_rows(
  workflow_set(preproc = list(rec_restrita = recipe_restrita), models = list(poisson = mod_poisson, bn = mod_bn, knn = mod_knn), cross = TRUE),
  workflow_set(preproc = list(rec_completa = recipe_completa), models = list(arvore = mod_tree, rf = mod_rf), cross = TRUE)
)

ctrl_grid_custom <- control_grid(save_pred = TRUE)
metricas <- metric_set(rmse, mae)

set.seed(793127)
res_treino <- wf_set %>% workflow_map("tune_grid", resamples = folds_cv, metrics = metricas, control = ctrl_grid_custom, grid = 10, verbose = TRUE)

# --- 5.2. Extração Final de Métricas em Teste (Árvore de Decisão Vencedora Inicial)
id_arvore_vencedor <- "rec_completa_arvore"
best_params_arvore <- res_treino %>% extract_workflow_set_result(id_arvore_vencedor) %>% select_best(metric = "rmse")
wf_final_arvore    <- res_treino %>% extract_workflow(id_arvore_vencedor) %>% finalize_workflow(best_params_arvore)
fit_final_arvore   <- last_fit(wf_final_arvore, split_inicial)

p_diag_arvore <- ggplot(collect_predictions(fit_final_arvore) %>% mutate(n_lesoes = ifelse(n_lesoes >= 3, "3+", as.character(n_lesoes))), 
                        aes(x = factor(n_lesoes, levels = c("0","1","2","3+")), y = .pred, fill = factor(n_lesoes, levels = c("0","1","2","3+")))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, show.legend = FALSE, width = 0.7) + scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +
  labs(x = "Número de Lesões Observadas", y = "Número de Lesões Preditas")
ggsave("imagens_tcc/fig10_predito_vs_observado_arvore.pdf", plot = p_diag_arvore, width = 7, height = 6)

# --- 5.3. Treinamento Adicional do Escopo Ampliado (Boosting, Redes Neurais e SVM)
wf_set_adicional <- workflow_set(preproc = list(rec_completa = recipe_completa), models = list(xgb = mod_xgb, svm = mod_svm, neural = mod_neural), cross = TRUE)
set.seed(793127)
res_treino_adicional <- wf_set_adicional %>% workflow_map("tune_grid", resamples = folds_cv, metrics = metricas, control = ctrl_grid_custom, grid = 10)

# Diagnóstico de Ajuste - Rede Neural Vencedora em Regressão (Figura 5.3 da Monografia)
best_params_nn <- res_treino_adicional %>% extract_workflow_set_result("rec_completa_neural") %>% select_best(metric = "rmse")
wf_final_nn    <- res_treino_adicional %>% extract_workflow("rec_completa_neural") %>% finalize_workflow(best_params_nn)
fit_final_nn   <- last_fit(wf_final_nn, split_inicial)

p_diag_nn <- ggplot(collect_predictions(fit_final_nn) %>% mutate(n_lesoes = ifelse(n_lesoes >= 3, "3+", as.character(n_lesoes))), 
                    aes(x = factor(n_lesoes, levels = c("0","1","2","3+")), y = .pred, fill = factor(n_lesoes, levels = c("0","1","2","3+")))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, show.legend = FALSE, width = 0.7) + scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +
  labs(x = "Número de Lesões Observadas", y = "Número de Lesões Preditas")
ggsave("imagens_tcc/fig12_predito_vs_observado_nn.pdf", plot = p_diag_nn, width = 7, height = 6)


# ==============================================================================
# --- CAPÍTULO 6: METAMODELO DE STACKING ENSEMBLE E INTEGRAÇÃO DO TABPFN V2
# ==============================================================================

print("--- Iniciando Capítulo 6: Construção do Stacking Ensemble ---")

recipe_restrita_stack <- recipe(n_lesoes ~ ., data = dados_treino) %>%
  step_other(continente, threshold = 0.05) %>% step_mutate(n_lesoes_temporada_anterior = as.integer(n_lesoes_temporada_anterior)) %>%
  step_dummy(all_nominal_predictors()) %>% step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors()) %>%
  step_rm(altura, starts_with("continente"), starts_with("pe_preferido"), minutos_temporada_anterior)

ctrl_stack <- control_grid(save_pred = TRUE, save_workflow = TRUE)

wf_set_stack <- bind_rows(
  workflow_set(preproc = list(rec_restrita = recipe_restrita_stack), models = list(poisson = mod_poisson, bn = mod_bn, knn = mod_knn), cross = TRUE),
  workflow_set(preproc = list(rec_completa = recipe_completa), models = list(arvore = mod_tree, rf = mod_rf), cross = TRUE)
)
wf_set_adicional_stack <- workflow_set(preproc = list(rec_completa = recipe_completa), models = list(xgb = mod_xgb, svm = mod_svm, neural = mod_neural), cross = TRUE)

set.seed(793127)
res_treino_stack    <- workflow_map(wf_set_stack, "tune_grid", resamples = folds_cv, metrics = metricas, control = ctrl_stack, grid = 10)
set.seed(793127)
res_adicional_stack <- workflow_map(wf_set_adicional_stack, "tune_grid", resamples = folds_cv, metrics = metricas, control = ctrl_stack, grid = 10)

stack_blend <- stacks() %>% add_candidates(res_treino_stack) %>% add_candidates(res_adicional_stack) %>%
  blend_predictions(metric = metric_set(rmse), penalty = 10^seq(-3, 0, length.out = 20))

p_blend <- autoplot(stack_blend); ggsave("imagens_tcc/fig14_blend_ensemble.pdf", plot = p_blend, width = 8, height = 6)
stack_final <- fit_members(stack_blend)

p_members <- autoplot(stack_blend, type = "weights") + 
  scale_fill_manual(values = scales::hue_pal()(5), labels = c("boost_tree" = "XGBoost", "decision_tree" = "Árvore de Decisão", "nearest_neighbor" = "KNN", "poisson_reg" = "Poisson", "rand_forest" = "Random Forest")) +
  labs(x = "Peso no Ensemble", y = "Modelo", fill = "Modelo")
ggsave("imagens_tcc/fig16_pesos_ensemble.pdf", plot = p_members, width = 9, height = 6)

# --- 6.2. Importação e Padronização Estética dos Resultados do TabPFN v2 (Gerados em Python)
if(file.exists("C:/Users/bagGi/OneDrive/Documentos/TCC/resultados_tabpfn_tcc.xlsx")) {
  dados_tabpfn_adj <- read_excel("C:/Users/bagGi/OneDrive/Documentos/TCC/resultados_tabpfn_tcc.xlsx") %>%
    mutate(n_lesoes_obs = factor(ifelse(observado >= 3, "3+", as.character(observado)), levels = c("0", "1", "2", "3+")))
  
  p_diag_tabpfn <- ggplot(dados_tabpfn_adj, aes(x = n_lesoes_obs, y = predito, fill = n_lesoes_obs)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.8, show.legend = FALSE, width = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) + scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) +
    labs(x = "Número de Lesões Observadas", y = "Número de Lesões Preditas")
  ggsave("imagens_tcc/fig17_predito_vs_observado_tabpfn.pdf", plot = p_diag_tabpfn, width = 7, height = 6)
}


# ==============================================================================
# --- CAPÍTULO 7: MODELAGEM BINÁRIA - CLASS_ENVELOPE DE OCORRÊNCIA DE LESÃO
# ==============================================================================

print("--- Iniciando Capítulo 7: Ajuste de Modelos Classificadores Binários ---")

dados_bin <- dados %>% mutate(lesao = as.factor(ifelse(n_lesoes > 0, 1, 0)))

set.seed(793127)
split_bin <- initial_split(dados_bin, prop = 3/4, strata = lesao)
treino_bin <- training(split_bin); teste_bin  <- testing(split_bin)
folds_bin <- vfold_cv(treino_bin, v = 10, strata = lesao)

recipe_restrita_bin <- recipe(lesao ~ ., data = treino_bin) %>% step_rm(n_lesoes) %>% step_other(continente, threshold = 0.05) %>%
  step_mutate(n_lesoes_temporada_anterior = as.integer(n_lesoes_temporada_anterior)) %>% step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors()) %>%
  step_select(lesao, any_of(vars_selecionadas_lasso), matches("^idade$"), starts_with("posicao"))

recipe_completa_bin <- recipe(lesao ~ ., data = treino_bin) %>% step_rm(n_lesoes) %>% step_other(continente, threshold = 0.05) %>%
  step_mutate(n_lesoes_temporada_anterior = as.integer(n_lesoes_temporada_anterior)) %>% step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors())

mod_log    <- logistic_reg() %>% set_engine("glm") %>% set_mode("classification")
mod_tree_b <- decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% set_engine("rpart") %>% set_mode("classification")
mod_rf_b   <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% set_engine("ranger", importance = "permutation") %>% set_mode("classification")
mod_xgb_b  <- boost_tree(tree_depth = tune(), learn_rate = tune(), trees = 1000) %>% set_engine("xgboost") %>% set_mode("classification")
mod_svm_b  <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>% set_engine("kernlab") %>% set_mode("classification")
mod_knn_b  <- nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>% set_engine("kknn") %>% set_mode("classification")
mod_nn_b   <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% set_engine("nnet", MaxNWts = 10000) %>% set_mode("classification")

wf_set_bin <- bind_rows(
  workflow_set(preproc = list(rec_restrita = recipe_restrita_bin), models = list(logistica = mod_log, knn = mod_knn_b), cross = TRUE),
  workflow_set(preproc = list(rec_completa = recipe_completa_bin), models = list(arvore = mod_tree_b, rf = mod_rf_b, xgb = mod_xgb_b, svm = mod_svm_b, neural = mod_nn_b), cross = TRUE)
)

metricas_bin <- metric_set(roc_auc, accuracy)
ctrl_bin     <- control_grid(save_pred = TRUE)

set.seed(793127)
res_bin <- workflow_map(wf_set_bin, "tune_grid", resamples = folds_bin, metrics = metricas_bin, control = ctrl_bin, grid = 10)

# Geração de Matriz de Confusão para o KNN Vencedor (Figura 7.1 da Monografia)
best_knn_bin <- res_bin %>% extract_workflow_set_result("rec_restrita_knn") %>% select_best(metric = "roc_auc")
wf_knn_bin   <- res_bin %>% extract_workflow("rec_restrita_knn") %>% finalize_workflow(best_knn_bin)
fit_knn_bin  <- last_fit(wf_knn_bin, split_bin, metrics = metricas_bin)

p_conf_mat <- autoplot(collect_predictions(fit_knn_bin) %>% conf_mat(truth = lesao, estimate = .pred_class), type = "heatmap") +
  scale_fill_gradient(low = "white", high = cor_azul) + labs(x = "Classe Observada", y = "Classe Predita") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none")
ggsave("imagens_tcc/fig_matriz_confusao_knn.pdf", plot = p_conf_mat, width = 6, height = 5)


# ==============================================================================
# --- CAPÍTULO 8: VALIDAÇÃO EXTERNA TEMPORAL - JANELA DE PROJEÇÃO FUTURA (2024/25)
# ==============================================================================

print("--- Iniciando Capítulo 8: Validação Externa Avançada sem Dependência de API ---")

caminho_base_2024 <- "C:/Users/bagGi/OneDrive/Documentos/TCC/dados_completos_2024_selecionados.xlsx"
dados_2024 <- read_excel(caminho_base_2024) %>%
  mutate(continente = as.factor(continente), posicao = as.factor(posicao), pe_preferido = as.factor(pe_preferido)) %>%
  mutate(continente = fct_collapse(continente, "Ásia/Oceania" = c("Ásia", "Oceania"))) %>%
  rename(n_lesoes_temporada_anterior = n_lesoes_ano_passado)

# --- 8.1. Validação Externa em Regressão Discreta (Rede Neural) ---
recipe_nn_ext <- recipe(n_lesoes ~ ., data = dados) %>%
  step_other(continente, threshold = 0.05) %>% step_mutate(n_lesoes_temporada_anterior = as.integer(n_lesoes_temporada_anterior)) %>%
  step_dummy(all_nominal_predictors()) %>% step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors())

wf_nn_ext   <- workflow() %>% add_recipe(recipe_nn_ext) %>% add_model(spec_nn_ext <- mlp(hidden_units = 4, penalty = 0.0000359, epochs = 10) %>% set_engine("nnet") %>% set_mode("regression"))
fit_nn_ext  <- fit(wf_nn_ext, data = dados)
preds_nn_24 <- predict(fit_nn_ext, new_data = dados_2024) %>% bind_cols(dados_2024 %>% dplyr::select(n_lesoes)) %>% mutate(n_lesoes = ifelse(n_lesoes >= 3, "3+", as.character(n_lesoes)))

p_ext_nn <- ggplot(preds_nn_24, aes(x = factor(n_lesoes, levels = c("0","1","2","3+")), y = .pred, fill = factor(n_lesoes, levels = c("0","1","2","3+")))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, show.legend = FALSE, width = 0.7) + scale_y_continuous(limits = c(NA, 2), breaks = seq(0, 2, by = 0.5)) +
  labs(x = "Número de Lesões Observadas", y = "Número de Lesões Preditas")
ggsave("imagens_tcc/fig_ext_nn_2024.pdf", plot = p_ext_nn, width = 7, height = 6)

# --- 8.2. Validação Externa em Classificação Binária (KNN Classifier) ---
dados_bin_23_knn <- dados_bin %>% dplyr::select(lesao, idade, jogos_temporada_anterior, n_lesoes_temporada_anterior, posicao)
dados_bin_24_knn <- dados_2024 %>% mutate(lesao = as.factor(ifelse(n_lesoes > 0, 1, 0))) %>% dplyr::select(lesao, idade, jogos_temporada_anterior, n_lesoes_temporada_anterior, posicao)

recipe_knn_ext <- recipe(lesao ~ ., data = dados_bin_23_knn) %>% step_dummy(all_nominal_predictors()) %>% step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors())
wf_knn_ext   <- workflow() %>% add_recipe(recipe_knn_ext) %>% add_model(nearest_neighbor(neighbors = 15, weight_func = "rectangular", dist_power = 1.37) %>% set_engine("kknn") %>% set_mode("classification"))
fit_knn_ext  <- fit(wf_knn_ext, data = dados_bin_23_knn)
preds_knn_24 <- predict(fit_knn_ext, new_data = dados_bin_24_knn) %>% bind_cols(dados_bin_24_knn %>% dplyr::select(lesao))

p_conf_ext <- autoplot(conf_mat(preds_knn_24, truth = lesao, estimate = .pred_class), type = "heatmap") +
  scale_fill_gradient(low = "white", high = cor_azul) + labs(x = "Classe Predita", y = "Classe Observada") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none")
ggsave("imagens_tcc/fig_ext_knn_confmat_2024.pdf", plot = p_conf_ext, width = 6, height = 5)

print("###########################################################################")
print("FIM!")
print("###########################################################################")
