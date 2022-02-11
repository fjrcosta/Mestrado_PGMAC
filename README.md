# Codigos_mestrado
Códigos usados na modelagem realizada 

a) "script_fonte_modelagem_final.R": entrada dos dados, manipulações necessárias, investigações preliminares, ajuste do modelo, diagnóstico, visualização/extração dos efeitos das covariáveis incorporadas aos preditores dos parâmetros distribucionais; 

b) "script_geracao_imagens_amimacoes.R": demonstra como associar variáveis levantadas pelo IBGE em seus censos aos setores censitários (RENDA, na situação específica e caso essa variável fosse utilizada), criação de grid para estimação em toda a extensão do perímetro urbano de Londrina, geração de imagens de estimações feitas em datas arbitradas (no terminal, em html e em kml), animações (renderização) de imagens de estimações  feitas em datas arbitradas em sucessivos períodos (animações em kml), criação de objetos espaço-temporais;

c) "bootstrap_n_parametrico.R": ): No bootstrap não paramétrico, os valores originais da resposta e das covariáveis são reamostrados com reposição e o modelo original
é ajustado aos novos dados;

d) "bootstrap_semiparametrico.R": Os resíduos e os valores estimdos para os parâmetros distribucionais pelo modelo proposto são salvos e novas respostas são simuladas por reamostragem com reposição dos resíduos, transformando-os usando o função distribuição acumulada da distribuição normal (os resíduos são normalizados) para valores entre 0 e 1 (probabilidade) e então usando a função distribuição acumulada inversa da distribuição BCTo, sob os valores estimados dos parâmetros;

e) "bootstrap_parametrico.R": a partir do modelo proposto são simuladas amostras boostrap da resposta sob uma distribuição BCTo 
