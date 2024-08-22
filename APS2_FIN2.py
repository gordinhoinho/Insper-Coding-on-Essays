import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize

# Carregar os dados da planilha Excel
file_path = '/Users/pedro/Downloads/aps.xlsx'  # Substitua pelo caminho correto do seu arquivo
df = pd.read_excel(file_path, sheet_name='Consolidado')
df = df.drop('Data', axis=1)

# Calcular os retornos esperados e a matriz de covariância
ret_esp = df.mean()
cov_matrix = df.cov()
std_dev = df.std()

# Taxa de retorno livre de risco (SELIC)
risk_free_rate = 11.65/100
rf = (1 + risk_free_rate)**(1/12) - 1

# Função para calcular o retorno do portfólio
def rcarteira(weights, returns):
    weights = np.array(weights).flatten()
    returns = np.array(returns).flatten()

    return np.dot(weights, returns)

# Função para calcular a variância do portfólio seguindo Markowitz
def desvcarteira(weights, cov_matrix):
    return np.sqrt(np.dot(weights.T, np.dot(cov_matrix, weights)))

# Função para calcular o índice de Sharpe negativo
def sharpe(weights, returns, cov_matrix, risk_free_rate):
    p_ret = rcarteira(weights, returns)
    p_vol = desvcarteira(weights, cov_matrix)

    return -(p_ret - risk_free_rate) / p_vol

# Número de ativos
num_assets = len(ret_esp)

# Parâmetros para o otimizador
n = (ret_esp, cov_matrix, rf)
restricoes = ({'type': 'eq', 'fun': lambda x: np.sum(x) - 1})  # Soma dos pesos igual a 1
limite = tuple((0, 1) for asset in range(num_assets))  # Pesos entre 0 e 1

# Chute inicial (distribuição igualitária)
init_guess = num_assets * [1. / num_assets]

# Otimização
opt_results = minimize(sharpe, init_guess, args=n, method='SLSQP', bounds=limite, constraints=restricoes)

# Pesos ótimos
potimo = opt_results.x
p0=pd.DataFrame([potimo], columns=df.columns)

# Imprimindo os pesos ótimos
print("Pesos Ótimos para a Carteira de Risco ótimo:")
for col in p0.columns:
    print(f"{col}: {p0[col].values[0]*100:.2f}%")

# Desvio padrão da carteira com os pesos ótimos seguindo Markowitz
dpcartdpotimo = desvcarteira(potimo, cov_matrix)
print("Desvio padrão da carteira com desvio padrão ótimo: {:.2f}%".format(dpcartdpotimo * 100))

# Calculando o retorno do portfólio ótimo com base nos pesos ótimos
retorno_portfolio_desvio_otimo = rcarteira(potimo, ret_esp)
print("Retorno da carteira com desvio Ótimo:, {:.2f}%".format(retorno_portfolio_desvio_otimo * 100))


A = [2,6,12]
aversao=np.array(A)

wcarto=[(retorno_portfolio_desvio_otimo-rf)/(a*dpcartdpotimo**2) for a in aversao]
wcartot=pd.DataFrame([wcarto])
colunainv = ['Agressivo','Moderado','Conservador']
wcartot.columns=colunainv

wcartotpct = wcartot.copy()

for col in wcartotpct.columns:
    wcartotpct[col] = wcartotpct[col].map(lambda x: f'{x*100:.2f}%')

print('W da carteira para cada investidor:')
print(wcartotpct)

wcarto_array = np.array(wcarto).reshape(1, -1)
retornocarteirotim = retorno_portfolio_desvio_otimo*wcarto_array + (1-wcarto_array)*rf
retornocarteirotima=pd.DataFrame(retornocarteirotim)
retornocarteirotima.columns = colunainv
retcopia=retornocarteirotima.copy()

for col in retcopia.columns:
    retcopia[col] = retcopia[col].map(lambda x: f'{x*100:.2f}%')

print('Retorno esperado para cada investidor:')
print(retcopia)

raiz = np.sqrt([i**2 * dpcartdpotimo**2 for i in wcarto])
dpinv=pd.DataFrame([raiz])
dpinv.columns=colunainv
dpinvpct = dpinv.copy()

for col in dpinvpct.columns:
    dpinvpct[col] = dpinvpct[col].map(lambda x: f'{x*100:.2f}%')

print('Desvio padrão para cada investidor:')
print(dpinvpct)

# Gerando uma sequência de muitos pesos entre 0 e 1 para combinações do ativo livre de risco e do portfólio de mercado
pesos_lac = np.linspace(0, 1.5, num=2000)

# Aqui, modificamos a linha para calcular os retornos esperados da LAC
retornos_lac = rf + pesos_lac * (retorno_portfolio_desvio_otimo - rf)

volatilidades_lac = pesos_lac * dpcartdpotimo 

def portfolio_variance(weights, cov_matrix):
    return np.dot(weights.T, np.dot(cov_matrix, weights))

# Função para encontrar os pesos que minimizam a variância para um dado retorno esperado
def minimize_variance(target_return, returns, cov_matrix):
    num_assets = len(returns)
    args = (cov_matrix,)
    constraints = (
        {'type': 'eq', 'fun': lambda x: rcarteira(x, returns) - target_return},  # Retorno esperado
        {'type': 'eq', 'fun': lambda x: np.sum(x) - 1}  # Soma dos pesos igual a 1
    )
    bounds = tuple((0, 1) for asset in range(num_assets))
    init_guess = num_assets * [1. / num_assets]
    result = minimize(portfolio_variance, init_guess, args=args, method='SLSQP', bounds=bounds, constraints=constraints)
    return result.x

# Gerando a fronteira de variância mínima
target_returns = np.linspace(ret_esp.min(), ret_esp.max(), 50)
min_var_weights = [minimize_variance(t_ret, ret_esp, cov_matrix) for t_ret in target_returns]
min_var_portfolios = [rcarteira(w, ret_esp) for w in min_var_weights]
min_var_vols = [np.sqrt(portfolio_variance(w, cov_matrix)) for w in min_var_weights]

# Plotando a LAC e a fronteira de variância mínima

tamanho_marcador_carteira = 150
tamanho_marcador_investidor = 150 

# Plotando a LAC e a fronteira de variância mínima
plt.figure(figsize=(12, 8))
plt.plot(min_var_vols, min_var_portfolios, color='blue', label='Fronteira de Variância Mínima', linewidth=1.3)
plt.plot(volatilidades_lac, retornos_lac, color='black', label='LAC', linewidth=1.3)
plt.scatter(dpcartdpotimo, retorno_portfolio_desvio_otimo, marker='o', color='red', label='Carteira com desvio ótimo', s=tamanho_marcador_carteira)

# Plotando os pontos dos investidores
for i, perfil in enumerate(['Agressivo', 'Moderado', 'Conservador']):
    plt.scatter(dpinv[perfil][0], retornocarteirotima[perfil][0], label=perfil, s=tamanho_marcador_investidor)

# Melhorias visuais
plt.title('Linha de Alocação de Capital (LAC) com Fronteira de Variância Mínima', fontsize=16)
plt.xlabel('Volatilidade (σ)', fontsize=14)
plt.ylabel('Retorno Esperado (E[r])', fontsize=14)
plt.scatter(0, rf, label='Risk-Free', color='red')
plt.legend(fontsize=12)
plt.grid(True)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)
plt.xlim(0, 2 * dpcartdpotimo)
plt.ylim(0, max(retornos_lac) * 1.4)
plt.show()