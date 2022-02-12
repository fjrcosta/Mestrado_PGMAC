Material auxiliar e complementar relativo à dissertação de mestrado produzida no Programa de pós-graduação em matemárica aplicada e computacional junto à Universidade Estadual de Londrina, sob o título "GAMLSS Espaço Temporal para Engenharia de Avaliações" (2020-2022). 


## Apêndices

Material didático complementar à dissertação GAMLSS Espaço Temporal para Engenharia de Avaliações.

## Bairros

Arquivos digitais com a delimitação e denominação dos bairros de Londrina.


## BCTo

Uma aplicação iterativa para demonstrar a flexibilidade da distribuição teórica Box-Cox t sob diferentes valores de parâmetros (ref: https://towardsdatascience.com/).


## Codigos_mestrado
Códigos usados na modelagem realizada 

a) "modelagem_final.R": entrada dos dados, manipulações necessárias, investigações preliminares, ajuste do modelo, diagnóstico, visualização/extração dos efeitos das covariáveis incorporadas aos preditores dos parâmetros distribucionais; 

b) "geracao_imagens_animacoes.R": demonstra como associar variáveis levantadas pelo IBGE em seus censos aos setores censitários (RENDA, na situação específica e caso essa variável fosse utilizada), criação de grid para estimação em toda a extensão do perímetro urbano de Londrina, geração de imagens de estimações feitas em datas arbitradas (no terminal, em html e em kml), animações (renderização) de imagens de estimações  feitas em datas arbitradas em sucessivos períodos (animações em kml), criação de objetos espaço-temporais;

c) "bootstrap_n_parametrico.R": ): No bootstrap não paramétrico, os valores originais da resposta e das covariáveis são reamostrados com reposição. O modelo original
é ajustado aos novos dados;

d) "bootstrap_semiparametrico.R": Os resíduos e os valores estimados para os parâmetros distribucionais pelo modelo proposto são salvos e novas respostas são simuladas por reamostragem com reposição dos resíduos, transformando-os usando o função distribuição acumulada da distribuição normal (os resíduos são normalizados) para valores entre 0 e 1 (probabilidade) e então usando a função distribuição acumulada inversa da distribuição BCTo, sob os valores estimados dos parâmetros. O modelo original é ajustado aos novos dados;

e) "bootstrap_parametrico.R": a partir dos valores estimados para os parâmetros distribucionais pelo modelo proposto modelo são geradas respostas sob uma distribuição BCTo e reamostradas com reposição. O modelo original é ajustado aos novos dados; 


## Dados

Planilha em formato MS Excel apresentando os dados analisados (por razões de sigilo, as colunas que apresentam informações sobre o nome e telefone dos informantes foram suprimidas). 

## Modelo

Modelo ajustado aos dados (objeto: mod_teste).

## Perímetro urbano

Arquivos digitais com a delimitação do perímetro urbano de Londrina.


## Renda

Planilhas em formato MS Excel apresentando os resultados tabulados pelo IBGE referentes à renda média familiar mensal de cada setor censitário de Londrina (2000 e 2010).


## Setores censitários

Arquivo em formato KML (zipado) para ser aberto pelo Google Earth, com os setores censitários de todos os municípios do estado do Paraná.
