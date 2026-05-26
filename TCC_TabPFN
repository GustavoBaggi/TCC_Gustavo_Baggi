################################################################################
# UNIVERSIDADE FEDERAL DE SÃO CARLOS
# CENTRO DE CIÊNCIAS EXATAS E DE TECNOLOGIA
# DEPARTAMENTO DE ESTATÍSTICA
#
# SCRIPT MESTRE PYTHON - FLUXO DE INFERÊNCIA DO MODELO TABPFN V2.5
# Autor: Gustavo Marques de Matos Baggi
# Orientador: Rafael Izbicki
# Data: Maio de 2026
#
# Descrição: Instalação, autenticação e execução do TabPFNRegressor para as
#            temporadas de treino (2023/24) e validação externa (2024/25).
################################################################################

# ==============================================================================
# --- 1. INSTALAÇÃO DAS DEPENDÊNCIAS OFICIAIS (TABPFN V2.5)
# ==============================================================================
print("--- Passo 1: Instalando pacotes da Prior-Labs ---")
!pip install -q tabpfn tabpfn-extensions openpyxl scikit-learn pandas numpy matplotlib seaborn

# ==============================================================================
# --- 2. CONFIGURAÇÃO DE AMBIENTE E AUTENTICAÇÃO VIA TOKEN
# ==============================================================================
import os
import typing
import numpy as np
import pandas as pd
import torch
from pathlib import Path
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split

# Importação oficial dos pesos estáveis do modelo fundacional v2.5
from tabpfn import TabPFNRegressor
from tabpfn.errors import TabPFNLicenseError

# --- CRUCIAL: Substitua pela chave copiada de https://ux.priorlabs.ai ---
os.environ["TABPFN_TOKEN"] = "SUA_API_KEY_AQUI"

# Fixação de Parâmetros Metodológicos alinhados com os Capítulos 4 e 5 do R
TEST_SIZE = 0.25
RANDOM_STATE = 793127  # Mesma semente utilizada no Tidymodels
TARGET_COLUMN = "n_lesoes"
FEATURE_COLUMNS = ["n_lesoes_temporada_anterior", "jogos_temporada_anterior", "posicao", "idade"]

# Forçar o uso de GPU caso esteja ativa no ambiente do Colab
device = "cuda" if torch.cuda.is_available() else "cpu"
print(f"Ambiente de hardware mapeado. Executando em: {device.upper()}")

# ==============================================================================
# --- 3. PIPELINE DE CARREGAMENTO E ENGENHARIA DE ATRIBUTOS
# ==============================================================================
def carregar_e_preparar_dados(caminho_excel: str, treino_cols: list = None):
    """Carrega o arquivo do TCC, isola os preditores selecionados pelo Lasso
    e aplica a codificação Dummy/One-Hot correspondente."""
    if not os.path.exists(caminho_excel):
        raise FileNotFoundError(f"Erro: O arquivo {caminho_excel} não foi encontrado no ambiente.")
        
    df = pd.read_excel(caminho_excel)
    
    # Padronização de nomenclatura de colunas (caso venha do arquivo bruto do R)
    if "n_lesoes_ano_passado" in df.columns:
        df = df.rename(columns={"n_lesoes_ano_passado": "n_lesoes_temporada_anterior"})
        
    # Extração e Isolamento do Alvo Numérico Contínuo (Regressão)
    y = pd.to_numeric(df[TARGET_COLUMN], errors="coerce").fillna(0).astype(int).to_numpy()
    
    # Tratamento dos Preditores
    X = df[FEATURE_COLUMNS].copy()
    X = pd.get_dummies(X, drop_first=False, dtype=float)
    
    # Alinhamento de Colunas Dummy para Validação Externa (Garantir matriz idêntica)
    if treino_cols is not None:
        X = X.reindex(columns=treino_cols, fill_value=0.0)
        
    return X, y

def imprimir_metricas_oficiais(y_true, y_pred, escopo="Base de Teste"):
    """Calcula e imprime as métricas formais do TCC."""
    rmse = float(np.sqrt(mean_squared_error(y_true, y_pred)))
    mae = mean_absolute_error(y_true, y_pred)
    r2 = r2_score(y_true, y_pred)
    print(f"\n--- Resultados TabPFN - {escopo} ---")
    print(f"RMSE : {rmse:.4f}")
    print(f"MAE  : {mae:.4f}")
    print(f"R²   : {r2:.4f}")

# ==============================================================================
# --- 4. EXECUÇÃO 1: TREINAMENTO E VALIDAÇÃO INTERNA (TEMPORADA 2023/24)
# ==============================================================================
print("\n--- Passo 4: Processando Temporada Corrente 2023/24 ---")

caminho_2023 = "dados_completos_2023_selecionados.xlsx"
X_2023, y_2023 = carregar_e_preparar_dados(caminho_2023)

# Criação do rótulo artificial de estratificação para manter split estável no R e Python
y_strat = np.where(y_2023 >= 3, 3, y_2023)

X_train, X_test, y_train, y_test = train_test_split(
    X_2023, y_2023,
    test_size=TEST_SIZE,
    random_state=RANDOM_STATE,
    stratify=y_strat
)

# Inicialização do Regressor Fundacional
model_tabpfn = TabPFNRegressor(device=device)

try:
    print("Ajustando pesos do TabPFNRegressor...")
    model_tabpfn.fit(X_train, y_train)
except TabPFNLicenseError as license_error:
    print("\n❌ Erro de Autenticação na Licença da Prior-Labs.")
    print("Verifique se o seu TABPFN_TOKEN foi preenchido corretamente no início do script.")
    raise license_error

# Predição com Clipping Metodológico (Impedir valores menores que zero)
preds_test = np.clip(model_tabpfn.predict(X_test).astype(float), 0.0, None)

# Cálculo do resíduo estrutural para exportação
residuals_test = y_test.astype(float) - preds_test

# Geração do Excel de Saída (Exigido pelo Script do R para montar a Fig 17 da Monografia)
df_resultados_r = pd.DataFrame({
    "observado": y_test.astype(float),
    "predito": preds_test,
    "residuo": residuals_test
})

caminho_saida_r = "resultados_tabpfn_tcc.xlsx"
df_resultados_r.to_excel(caminho_saida_r, index=False)
print(f"✅ Arquivo '{caminho_saida_r}' gerado e pronto para o R!")

imprimir_metricas_oficiais(y_test.astype(float), preds_test, escopo="Base de Teste (2023/24)")

# ==============================================================================
# --- 5. EXECUÇÃO 2: VALIDAÇÃO EXTERNA TEMPORAL (TEMPORADA FUTEBOL 2024/25)
# ==============================================================================
caminho_2024 = "dados_completos_2024_selecionados.xlsx"

if os.path.exists(caminho_2024):
    print("\n--- Passo 5: Executando Validação Externa Temporal 2024/25 ---")
    
    # Carregamento alinhando as colunas dummy do treino (2023) para evitar quebras estruturais
    X_2024, y_2024 = carregar_e_preparar_dados(caminho_2024, treino_cols=X_train.columns)
    
    # Predição sobre a série futura completa utilizando o modelo fundacional pré-treinado
    preds_2024 = np.clip(model_tabpfn.predict(X_2024).astype(float), 0.0, None)
    
    imprimir_metricas_oficiais(y_2024.astype(float), preds_2024, escopo="Validação Externa (2024/25)")
    
    # Exportação opcional para auditoria cruzada temporal no R
    df_ext_2024 = pd.DataFrame({
        "observado_2024": y_2024.astype(float),
        "predito_tabpfn_2024": preds_2024
    })
    df_ext_2024.to_excel("resultados_tabpfn_validacao_externa_2024.xlsx", index=False)
    print("✅ Resultados de Validação Externa salvos com sucesso!")
else:
    print("\n⚠️ Arquivo de 2024/25 não mapeado na pasta local. Pulando etapa de Validação Externa.")

print("\n###########################################################################")
print(" Pipeline do TabPFN executada e integrada ao ecossistema do seu TCC!")
print("###########################################################################")
