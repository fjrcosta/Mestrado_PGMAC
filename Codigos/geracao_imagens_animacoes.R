library(librarian)

shelf(Hmisc,gamlss,gamlss.add,geobr,readxl,sf,rgdal,spatialEco,
      mapview,RSAGA,plotKML,inlabru,RColorBrewer,rayshader,av,
      rgl,maps,GISTools,geoR,spacetime,zoo,ggplot2,hnp,fields,
      cowplot,rosm,ggmap,spData,tmap,tmaptools,raster, fields,
      raster, spData, ceramic, plotGoogleMaps, dplyr, tibble)

# Para mapas do Google Earth usar a função 
# register_google para a chave da API, a qual requer a versão 
# do mantenedor do ggmap: 
# devtools::install_github("dkahle/ggmap", ref = "tidyup")


########################################################################################################################
# Carregando resultados salvos
########################################################################################################################

setwd("<...>/dissertacao/dados")
load("mod_teste")
load("dados_treino_terrenos")

# Arquivos necessários (e auxiliares):
# a planilha com os dados: Terrenos_R_GIT.xlsx;
# planilhas auxiliares (xlsx) com informações (já resumidas) sobre o rendimento médio por setor censitário nos censos de 2000 e 2010;
# arquivos digitais (shp) auxiliares com o perimetro_urbano da cidade de Londrina, Pr.;
# arquivos digitais (shp) auxiliares com os bairros da cidade de Londrina, Pr.;
# arquivo digital (kmz) com os setores censitários do estado do Paraná, para ser lido no Google Earth;
# modelo ajustado;
# estimações já feitas sobre os pontos do grid;

# Criar os diretórios onde se localizam os arquivos necessários
# diretorio para os dados ='<...>dissertacao/dados'
# diretorio para as planilhas de rendimentos ='<...>dissertacao/renda'
# diretorio para os shp do perímetro urbano ='<...>dissertacao/perimetro_urbano'
# diretorio para os shp dos bairros ='<...>dissertacao/bairros'
# diretorio para as estimações feitas do grid em datas diferentes ='<...>dissertacao/estimados'



########################################################################################################################
# OBJETOS E VARIÁVEIS ESPACIAIS
# shapefiles com o perímetro urbano e também os bairros de Londrina
# As rotinas para se associar as  rendas médias mensais os setores censitários,
# caso seja de interesse usar essa variável (RENDA)
# planilhas com a variável "RENDA"  (renda média do setor censitário ref. ao censo de 2010)
# arquivos com os cógidos de cada setor censitário (2010)
########################################################################################################################

# Carregando objetos espaciais relacionados aos setores censitários (códigos: code_tract)

setores_2010.LDA=read_census_tract(code_tract =4113700, year = 2010, zone="urban", simplified = FALSE)
setores_2000.LDA=read_census_tract(code_tract =4113700, year = 2000, zone="urban", simplified = FALSE)

# Associando a variável "RENDA" (renda média do setor censitário ref. ao censo de 2010 ou 2000)
# ao objeto anterior

diretorio.2='<...>/renda/Renda_Domiciliar_Censo_2010.xlsx'
diretorio.2='<...>/renda/Renda_Domiciliar_Censo_2000.xlsx'


renda_pr=read_xlsx(diretorio.2,'1')
renda_pr=renda_pr[, -c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)] #Se censo 2010
#renda_pr=renda_pr[, -c(2,3,4)] #Se censo 2000

renda_pr=cbind(renda_pr[,1], renda_pr[,2])
colnames(renda_pr)=c('code_tract','RENDA') 
setores_2010.LDA=merge(setores_2010.LDA, renda_pr, by.y="code_tract")
#setores_2000.LDA=merge(setores_2000.LDA, renda_pr, by.y="code_tract")


# Agora o objeto possui uma informação a mais em cada setor censitário: "RENDA 2010" ou "RENDA 2000"
# Reprojetando o objeto
setores_2010.LDA=st_transform(setores_2010.LDA, crs= 29192)

# Carregando objetos espaciais relacionados ao limite urbano municipal (para recortar o grid)

diretorio.3='<...>perimetro_urbano/LIM_Perimetro_Urbano_V3.shp'

shape_londrina_urbano=readOGR(diretorio.3, encoding="UTF-8")
shape_londrina_urbano=spTransform(shape_londrina_urbano, CRS("+init=epsg:29192"))

# Carregando objeto espacial relacionado aos bairros municipais (para imagens diversas)

diretorio.4='<...>bairros/LIM_Bairros_V1.shp'

shape_londrina_bairros=readOGR(diretorio.4, encoding="UTF-8")
shape_londrina_bairros=spTransform(shape_londrina_bairros, CRS("+init=epsg:29192"))


# Criando um grid denso (ponts distantes 50m x 50m) para estimação em datas específicas por todo a área urbana 

loci= expand.grid(
  UTM_Y=seq(from=shape_londrina_urbano@bbox[2,1]-1000, to=shape_londrina_urbano@bbox[2,2]+1000,by=50),
  UTM_X=seq(from=shape_londrina_urbano@bbox[1,1]-1000, to=shape_londrina_urbano@bbox[1,2]+1000,by=50))
ref=seq(1, length(loci$UTM_Y), 1)

grid=data.frame (x=loci$UTM_X, y=loci$UTM_Y, ref=ref)
coordinates(grid)=~x+y
class(grid)
proj4string(grid)=CRS('+init=epsg:29192') #Atribuindo o CRS sem reprojetar (não precisa)

# Selecionando apenas os pontos de grid que estão dentro do polígono urbano de Londrina
# cf. http://www2.uaem.mx/r-mirror/web/packages/sp/vignettes/over.pdf p. 3

grid_loci<- grid[shape_londrina_urbano,]


# Criando um dataframe com a variável "RENDA" em cada ponto do grid criado, de acordo com o setor que pertence 
# caso a variável "RENDA" (renda média do setor censitário ref. ao censo de 2010) fosse utilizada no modelo

pts_poly_shape=point.in.poly(grid_loci, setores_2010.LDA)
tab=data.frame(pts_poly_shape$ref, pts_poly_shape@data$RENDA, pts_poly_shape@coords[,1], pts_poly_shape@coords[,2])
colnames(tab)=c("REF GRID", "RENDA", "UTM_X", "UTM_Y")
tab$RENDA[is.na(tab$RENDA)]=609.4 # caso haja algum setor com NA, adotei a renda mínima para ele

# Criando um objeto espacial com os dados amostrais 

amostra=data.frame(x=dados_treino_terrenos$UTM_X, y=dados_treino_terrenos$UTM_Y,Ref=dados_treino_terrenos$REF)
coordinates(amostra)=~x+y
proj4string(amostra)=CRS('+init=epsg:29192')

# Criando dataframes com as informações dos shapefiles dos bairros e do perímetro urbano 

shape_londrina_bairros$id = rownames(as.data.frame(shape_londrina_bairros))
bairros.pts=fortify(reproject(shape_londrina_bairros), region="id")
bairros.df= merge(bairros.pts, shape_londrina_bairros, by="id", type='left') 
bairros=aggregate(cbind(long, lat) ~ bairros, data=bairros.df,FUN=function(x)mean(range(x)))

shape_londrina_urbano$id = rownames(as.data.frame(shape_londrina_urbano))
urbano.pts=fortify(reproject(shape_londrina_urbano), region="id")
urbano.df= merge(urbano.pts, shape_londrina_urbano, by="id", type='left') 


########################################################################################################################
# Estimação dos pontos do grid nas datas de 01/06/2000 e 01/06/2021
# Criação de objetos espaciais com os resultados
########################################################################################################################


# Dataframe com dados na data de 01/07/2000 

novos_dados2000=data.frame(
  UTM_X=tab$UTM_X,
  UTM_Y=tab$UTM_Y,
  DATA=as.numeric(as.Date("01/07/2000", format="%d/%m/%Y"),tz="UTC", origin="1970-01-01"),
  AT=377, 
  RELEVO="Plano",
  PAVIMENTACAO="Pav. asfáltica",
  #RENDA=tab$RENDA,  #a variável RENDA não foi utilizada no modelo
  NATUREZA="Transação", 
  IMPLANTACAO="Isolado")

# Dataframe com dados na data de 01/07/2021 

novos_dados2021=data.frame(
  UTM_X=tab$UTM_X,
  UTM_Y=tab$UTM_Y,
  DATA=as.numeric(as.Date("01/07/2021", format="%d/%m/%Y"),tz="UTC", origin="1970-01-01"),
  AT=377, 
  RELEVO="Plano",
  PAVIMENTACAO="Pav. asfáltica",
  #RENDA=tab$RENDA, #a variável RENDA não foi utilizada no modelo
  NATUREZA="Transação", 
  IMPLANTACAO="Isolado")

# Estimando (mu, sigma, nu e tau) nas datas de 01/07/2000 e 01/07/2021

fit2000=predictAll(object=mod_teste, newdata = novos_dados2000, type = 'response')
fit2021=predictAll(object=mod_teste, newdata = novos_dados2021, type = 'response')

# Salvando os resultados
# saveRDS(object=fit2000, file="<...>/dissertacao/estimados/fit2000")
# saveRDS(object=fit2021, file="<...>/dissertacao/estimados/fit2021")


# Construindo um objeto espacial com as estimativas da média  nas datas de 01/07/2000 e 01/07/2021


# Na escala original em 01/07/2000

#Dataframe
pred_loci2000=
  data.frame( 
    x=tab$UTM_X,
    y=tab$UTM_Y,
    V_UNIT=fit2000$mu)

#Sp
pred_loci2000.sp=pred_loci2000
coordinates(pred_loci2000.sp)=~x+y
class(pred_loci2000.sp)
proj4string(pred_loci2000.sp)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_loci2000.sp)=TRUE

# Na escala original em 01/07/2021

#Dataframe
pred_loci2021=
  data.frame( 
    x=tab$UTM_X,
    y=tab$UTM_Y,
    V_UNIT=fit2021$mu)

#Sp
pred_loci2021.sp=pred_loci2021
coordinates(pred_loci2021.sp)=~x+y
class(pred_loci2021.sp)
proj4string(pred_loci2021.sp)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_loci2021.sp)=TRUE


# Escala logaritmica em 01/07/2000
# (a transformação para essa escala visa apenas dar um destaque visual maior às variações entre as áreas urbanas)

#Dataframe
pred_loci2000.log=
  data.frame( 
    x=tab$UTM_X,
    y=tab$UTM_Y,
    V_UNIT=log(fit2000$mu))

#Sp
pred_loci2000.sp.log=pred_loci2000.log
coordinates(pred_loci2000.sp.log)=~x+y
class(pred_loci2000.sp.log)
proj4string(pred_loci2000.sp.log)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_loci2000.sp.log)=TRUE



# Escala logaritmica em 01/07/2021
# (a transformação para essa escala visa apenas dar um destaque visual maior às variações entre as áreas urbanas)

#Dataframe
pred_loci2021.log=
  data.frame( 
    x=tab$UTM_X,
    y=tab$UTM_Y,
    V_UNIT=log(fit2021$mu))

#Sp
pred_loci2021.sp.log=pred_loci2021.log
coordinates(pred_loci2021.sp.log)=~x+y
class(pred_loci2021.sp.log)
proj4string(pred_loci2021.sp.log)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_loci2021.sp.log)=TRUE


# Criando dataframes (em WSG84) com as estimativas para criar linhas de contorno com stat_contour

pred_loci2000.df.WSG84=as.data.frame(reproject(pred_loci2000.sp))
colnames(pred_loci2000.df.WSG84)=c('V_UNIT', 'x', 'y')

pred_loci2000.df.log.WSG84=as.data.frame(reproject(pred_loci2000.sp.log))
colnames(pred_loci2000.df.log.WSG84)=c('V_UNIT', 'x', 'y')

pred_loci2021.df.WSG84=as.data.frame(reproject(pred_loci2021.sp))
colnames(pred_loci2021.df.WSG84)=c('V_UNIT', 'x', 'y')

pred_loci2021.df.log.WSG84=as.data.frame(reproject(pred_loci2021.sp.log))
colnames(pred_loci2021.df.log.WSG84)=c('V_UNIT', 'x', 'y')


# Manipulações para converter da classse:
# SpatialPixelsDataFrame >>  SpatialGridDataFrame >> list >> list >> SpatialLinesDataFrame
# Para possibilitar a justaposição de várias camadas no KML base (amostra, linhas e contorno...)

# Na escala original
pred_loci2000.sldf=as(pred_loci2000.sp, "SpatialGridDataFrame")
pred_loci2000.sldf=as.image.SpatialGridDataFrame(pred_loci2000.sldf)
pred_loci2000.sldf=contourLines(pred_loci2000.sldf,nlevels=50)
pred_loci2000.sldf=ContourLines2SLDF(pred_loci2000.sldf)
proj4string(pred_loci2000.sldf)=CRS('+init=epsg:29192')

# Na escala logaritmica
pred_loci2000.sldf.log=as(pred_loci2000.sp.log, "SpatialGridDataFrame")
pred_loci2000.sldf.log=as.image.SpatialGridDataFrame(pred_loci2000.sldf.log)
pred_loci2000.sldf.log=contourLines(pred_loci2000.sldf.log,nlevels=50)
pred_loci2000.sldf.log=ContourLines2SLDF(pred_loci2000.sldf.log)
proj4string(pred_loci2000.sldf.log)=CRS('+init=epsg:29192')


# Manipulações para converter da classse:
# SpatialPixelsDataFrame >>  SpatialGridDataFrame >> list >> list >> SpatialLinesDataFrame

# Na escala original
pred_loci2021.sldf=as(pred_loci2021.sp, "SpatialGridDataFrame")
pred_loci2021.sldf=as.image.SpatialGridDataFrame(pred_loci2021.sldf)
pred_loci2021.sldf=contourLines(pred_loci2021.sldf,nlevels=50)
pred_loci2021.sldf=ContourLines2SLDF(pred_loci2021.sldf)
proj4string(pred_loci2021.sldf)=CRS('+init=epsg:29192')

# Na escala logaritmica
pred_loci2021.sldf.log=as(pred_loci2021.sp.log, "SpatialGridDataFrame")
pred_loci2021.sldf.log=as.image.SpatialGridDataFrame(pred_loci2021.sldf.log)
pred_loci2021.sldf.log=contourLines(pred_loci2021.sldf.log,nlevels=50)
pred_loci2021.sldf.log=ContourLines2SLDF(pred_loci2021.sldf.log)
proj4string(pred_loci2021.sldf.log)=CRS('+init=epsg:29192')



# Salvando os resultados

# saveRDS(object=pred_loci2000, file="<...>/dissertacao/estimados/pred_loci2000")
# 
# saveRDS(object=pred_loci2000.log, file="<...>/dissertacao/estimados/pred_loci2000.log")
# 
# saveRDS(object=pred_loci2000.sp, file="<...>/dissertacao/estimados/pred_loci2000.sp")
# 
# saveRDS(object=pred_loci2000.sp.log, file="<...>/dissertacao/estimados/pred_loci2000.sp.log")
# 
# saveRDS(object=pred_loci2021, file="<...>/dissertacao/estimados/pred_loci2021")
# 
# saveRDS(object=pred_loci2021.log, file="<...>/dissertacao/estimados/pred_loci2021.log")
# 
# saveRDS(object=pred_loci2021.sp, file="<...>/dissertacao/estimados/pred_loci2021.sp")
# 
# saveRDS(object=pred_loci2021.sp.log, file="<...>/dissertacao/estimados/pred_loci2021.sp.log")



########################################################################################################################
# Criando variadas paletas de cores
########################################################################################################################


pal1=colorRampPalette(brewer.pal(9, name="Blues"))
pal2=colorRampPalette(rev(brewer.pal(11, "Spectral"))) 
pal3=SAGA_pal[[22]]
pal4=brewer.pal(11, "BrBG")
pal5=colorRampPalette(c("blue", "cyan", "yellow", "red"), bias=1)
pal6=SAGA_pal[[1]]
pal7=colorRampPalette(c('#0B1F23','#0E2629','#112E30','#133637','#163E3D',
                        '#194644','#1C4E4A','#20574F','#255F54','#2A6859',
                        '#2F715E','#367A62','#3D8366','#458C69','#4E956C',
                        '#579E6F','#61A771','#6CB073','#78B975','#84C276',
                        '#91CB77','#9ED378','#ACDC79','#BBE57A','#CAED7B',
                        '#DAF57C'), bias=1)
pal8=colorRampPalette(brewer.pal(11, "Spectral"))
pal9=colorRampPalette(c('#18232F','#1C2936','#1F2F3E','#233545','#263C4D',
                        '#294254','#2C495C','#2E5064','#31576B','#335E73',
                        '#35667B','#376D83','#39758B','#3A7C92','#3C849A',
                        '#3D8CA2','#3E94A9','#3E9CB1','#3FA4B9','#3FACC0',
                        '#40B5C7','#40BDCF','#40C5D6','#40CEDD','#3FD6E4',
                        '#3FDFEB','#3FE8F1','#3FF1F8'), bias=1)
pal10=colorRampPalette(c('#53341F','#583920','#5D3F22','#614423','#654A25','#695027',
                         '#6C5729','#6F5D2C','#72642F','#746A32','#767136','#77783A',
                         '#787F3F','#798645','#798D4B','#799452','#799B59','#78A261',
                         '#77A96A','#76B073','#75B77D','#74BE87','#72C592','#71CC9E',
                         '#70D3A9','#70D9B6','#70E0C2','#72E6CF','#75ECDC','#79F2E8'),
                       bias=1)
pal11=colorRampPalette(c('#522C2A','#593031','#5F3538','#66393F','#6C3E46','#72434E',
                         '#774856','#7D4E5E','#815367','#85596F','#896078','#8C6681',
                         '#8F6D8A','#917493','#927B9C','#9383A4','#938AAD','#9292B5',
                         '#919ABD','#90A2C4','#8EAACB','#8BB2D1','#89BAD7','#86C2DD',
                         '#83CAE1','#80D2E6','#7EDAE9','#7CE2EC','#7BEAEE','#7CF2EF'),
                       bias=1)
pal12=colorRampPalette(c('#580507','#5D080A','#620B0C','#670E0F','#6C1111','#711413','#761815','#7A1B17','#7F1E19','#84221C',
                         '#89251E','#8E2921','#922D24','#973026','#9B3429','#A0382C','#A43C2F','#A84032','#AD4435','#B14838',
                         '#B54C3B','#B9503F','#BD5442','#C15946','#C45D49','#C8624D','#CB6651','#CF6B55','#D26F59','#D5745D',
                         '#D87961','#DB7D65','#DE8269','#E1876D','#E38C72','#E69176','#E8967B','#EA9B80','#ECA084','#EEA589',
                         '#EFAB8E','#F1B093','#F2B598','#F3BA9E','#F4C0A3','#F5C5A8'), bias=1)
pal13=col2kml(two.colors(n=1024, start="blue", end="red", middle="green",alpha=0.5))
pal14=tim.colors(1000)
pal15=larry.colors()
pal16=colorRampPalette(pal14[], bias=1)
pal17=colorRampPalette(pal15[], bias=1)
pal18=colorRampPalette(brewer.pal(11, "Spectral")) 
pal19=theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())
pal20=scale_color_distiller(palette='Spectral')
pal21=scale_fill_distiller(palette='Spectral')


#########################################
# Geração de imagens em html
#########################################


# Gerando uma visualização em html do período arbitrado (exposto no 'viewer')


# Escala original
# 01/07/2000 

mapview(
  pred_loci2000.sp, 
  col.regions=pal14,
  alpha.regions=0.6,
  legend.opacity=0.8,
  layer.name="VUnit 07/2000",
  at = seq(5, 200, 10),
  burst=TRUE,
  legend=TRUE,
  homebutton=FALSE,
  hide=TRUE)

# 01/07/2021 

mapview(
  pred_loci2021.sp, 
  col.regions=pal14,
  alpha.regions=0.6,
  legend.opacity=0.8,
  layer.name="VUnit 07/2021",
  at = seq(50, 3850, 100),
  burst=TRUE,
  legend=TRUE,
  homebutton=FALSE,
  hide=TRUE)


# Escala logarítimica
# 01/07/2000 

mapview(
  pred_loci2000.sp.log, 
  col.regions=pal14,
  alpha.regions=0.6,
  legend.opacity=0.8,
  layer.name="VUnit 06/2000",
  at = seq(1,6,0.1),
  burst=TRUE,
  legend=TRUE,
  homebutton=FALSE,
  hide=TRUE)

# 01/07/2021 

mapview(
  pred_loci2021.sp.log, 
  col.regions=pal14,
  alpha.regions=0.6,
  legend.opacity=0.8,
  layer.name="VUnit 06/2021",
  at = seq(3,9,0.2),
  burst=TRUE,
  legend=TRUE,
  homebutton=FALSE,
  hide=TRUE)




#########################################
# Gerando uma visualização em kml 
# dos períodos arbitrados e sobrepondo os dados amostrais
#
# Gerando e salvando uma arquivo .kml com chamada automática do GoogleEarth
# (atribuir nomes para os arquivos)
#########################################


# Manipulações para converter da classse:
# SpatialPixelsDataFrame >>  SpatialGridDataFrame >> list >> list >> SpatialLinesDataFrame
# Para possibilitar a justaposição de várias camadas no KML base (amostra, linhas e contorno...)

# Na escala original
pred_loci2000.sldf=as(pred_loci2000.sp, "SpatialGridDataFrame")
pred_loci2000.sldf=as.image.SpatialGridDataFrame(pred_loci2000.sldf)
pred_loci2000.sldf=contourLines(pred_loci2000.sldf,nlevels=50)
pred_loci2000.sldf=ContourLines2SLDF(pred_locdev.offi2000.sldf)
proj4string(pred_loci2000.sldf)=CRS('+init=epsg:29192')

# Na escala logaritmica
pred_loci2000.sldf.log=as(pred_loci2000.sp.log, "SpatialGridDataFrame")
pred_loci2000.sldf.log=as.image.SpatialGridDataFrame(pred_loci2000.sldf.log)
pred_loci2000.sldf.log=contourLines(pred_loci2000.sldf.log,nlevels=50)
pred_loci2000.sldf.log=ContourLines2SLDF(pred_loci2000.sldf.log)
proj4string(pred_loci2000.sldf.log)=CRS('+init=epsg:29192')


# Manipulações para converter da classse:
# SpatialPixelsDataFrame >>  SpatialGridDataFrame >> list >> list >> SpatialLinesDataFrame

# Na escala original
pred_loci2021.sldf=as(pred_loci2021.sp, "SpatialGridDataFrame")
pred_loci2021.sldf=as.image.SpatialGridDataFrame(pred_loci2021.sldf)
pred_loci2021.sldf=contourLines(pred_loci2021.sldf,nlevels=50)
pred_loci2021.sldf=ContourLines2SLDF(pred_loci2021.sldf)
proj4string(pred_loci2021.sldf)=CRS('+init=epsg:29192')

# Na escala logaritmica
pred_loci2021.sldf.log=as(pred_loci2021.sp.log, "SpatialGridDataFrame")
pred_loci2021.sldf.log=as.image.SpatialGridDataFrame(pred_loci2021.sldf.log)
pred_loci2021.sldf.log=contourLines(pred_loci2021.sldf.log,nlevels=50)
pred_loci2021.sldf.log=ContourLines2SLDF(pred_loci2021.sldf.log)
proj4string(pred_loci2021.sldf.log)=CRS('+init=epsg:29192')


# 01/07/2000
# escala original

plotKML(pred_loci2000.sp,
        alpha=0.40, 
        colour_scale = pal14,
        z.lim=c(0,200),
        file.name = "Modelo_proposto_julho_2000.kml", 
        open.kml = TRUE)

# Justapondo as camadas no arquivo base
kml_open("Modelo_proposto_julho_2000.kml")
kml_layer(pred_loci2000.sp, alpha=0.40, colour_scale = pal14, z.lim=c(0,200))
kml_layer(amostra.WSG84)  
kml_layer(pred_loci2000.sldf, colour="red", z.lim=c(0,200), name= pred_loci2000.sldf$level )
kml_close("Modelo_proposto_julho_2000.kml")


# escala logaritimica (editar o png com a escala cromática)

plotKML(pred_loci2000.sp.log,
        alpha=0.40,
        colour_scale=pal14,
        z.lim=c(1,9),
        file.name = "Modelo_proposto_julho_2000_log.kml",
        open.kml = TRUE)

kml_open("Modelo_proposto_julho_2000_log.kml")
kml_layer(pred_loci2000.sp.log, alpha=0.40, colour_scale = pal14, z.lim=c(1,9))
kml_layer(amostra.WSG84)  
kml_layer(pred_loci2000.sldf.log, colour="red", z.lim=c(1,9))
kml_close("Modelo_proposto_julho_2000_log.kml")


# 01/07/2021

# escala natural
plotKML(pred_loci2021.sp,
        alpha=0.40, 
        colour_scale = pal14,
        z.lim=c(0,5000),
        file.name = "Modelo_proposto_julho_2021.kml", open.kml = TRUE)

# Justapondo as camadas no arquivo base
kml_open("Modelo_proposto_julho_2021.kml")
kml_layer(pred_loci2021.sp, alpha=0.40, colour_scale = pal14, z.lim=c(0,5000))
kml_layer(amostra.WSG84)  
kml_layer(pred_loci2021.sldf, colour="red", z.lim=c(0,5000))
kml_close("Modelo_proposto_julho_2021.kml")



# escala logaritimica (editar o png com a escala cromática)
plotKML(pred_loci2021.sp.log,
        alpha=0.40,
        colour_scale=pal14,
        z.lim=c(1,9),
        file.name = "Modelo_proposto_julho_2021_log.kml",
        open.kml = TRUE)

# Justapondo as camadas no arquivo base
kml_open("Modelo_proposto_julho_2021_log.kml")
kml_layer(pred_loci2021.sp.log, alpha=0.40, colour_scale = pal14, z.lim=c(1,9))
kml_layer(amostra.WSG84)  
kml_layer(pred_loci2021.sldf.log, colour="red", z.lim=c(0,5000), labels=TRUE)
kml_close("Modelo_proposto_julho_2021_log.kml")



# Para justapor a amostra sobre o kml escolhido
# abrir esse layer: Amostra de terrenos.kml junto com o grid estimado desejado

amostra.WSG84=reproject(amostra)            #amostra em wsg84 9GoogleEarth
kmlPoints(amostra.WSG84, "Amostra de terrenos.kml",icon="http://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_orange.png",
          name=paste("Ref:", dados_treino_terrenos$REF))  



#########################################
# Gerando imagens no terminal do RStudio
#########################################


# 01/07/2000 A posição dos elementos está apropriada para visualização em zoom

image(pred_loci2000.sp, val=pred_loci2000.sp@data$V_UNIT, col= pal14,
      ylab="UTM Y (EPSG 29192)", 
      xlab="UTM X (EPSG 29192)", axes=T)
legend.krige(x.leg=c(506000, 508000),y.leg=c(7415000, 7425000),
             val=pred_loci2000.sp@data$V_UNIT, 
             col= pal14, ndivs=30,vertical =TRUE, cex=1 )
title("Valores unitários medianos estimados base: jul. 2000 (R$/m2)")	
north.arrow(xb=461000, yb=7427000, len=500, lab="N", cex.lab=1, tcol="black") 
map.scale(xc=461000, yc=7415000, len=9000, units="km", ndivs=5, tcol="black", scol="black", sfcol="black")
contour(pred_loci2000.sp, nlevels=20, col= "red", cex=1.5, axes=T, add=TRUE, lty=1)

# 01/07/2021 A posição dos elementos está apropriada para visualização em zoom

image(pred_loci2021.sp, val=pred_loci2021.sp@data$V_UNIT, col= pal14,
      ylab="UTM Y (EPSG 29192)", 
      xlab="UTM X (EPSG 29192)", axes=T)
legend.krige(x.leg=c(504000, 506000),y.leg=c(7415000, 7425000),val=pred_loci2021.sp@data$V_UNIT, 
             col= pal14, ndivs=30,vertical =TRUE, cex=1 )
title("Valores unitários medianos estimados base: jul. 2021 (R$/m2)")	
north.arrow(xb=461000, yb=7427000, len=500, lab="N", cex.lab=1, tcol="black") 
map.scale(xc=461000, yc=7415000, len=9000, units="km", ndivs=5, tcol="black", scol="black", sfcol="black")
contour(pred_loci2021.sp, nlevels=20, col= "red", cex=1.5, axes=T, add=TRUE, lty=1)



#########################################
# Gerando imagens com várias bibliotecas
#########################################

# Ggplot

# Escala original
# 01/07/2000

plot_2000=ggplot()+
  gg(reproject(pred_loci2000.sp))+
  scale_fill_gradientn(colours=pal18(26),  n.breaks=20) +
  ggtitle("Valores unitários medianos estimados \nbase: jul. 2000 \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.5, 'cm'),legend.spacing.y = unit(0.5, 'cm'))+
  guides(fill = guide_legend(title = "R$/m2",
                             label.position = "right",
                             title.position = "top", title.vjust = 1)) 
plot_2000

# Nessa imagem foram arbitrados os limites da escala e os intervalos
# de modo a que ela possa ser posta ao lado de outra (plot_2021b) 
# compartilhando a mesma escala cromática

# Os valores estimados foram log - transformados para que a escala de cores 
# exibisse diferenças mais pronunciadas   

# escala logaritmica

breaks=seq(1,9,0.5)
labs=paste("R$",formatC(exp(breaks), digits=2, format='f'),"/m2")

plot_2000b=ggplot()+
  gg(reproject(pred_loci2000.sp.log))+
  scale_fill_gradientn(colours=pal16(44), breaks=seq(1,9,0.5),
                       labels = labs,
                       limits=c(1,9))+
  ggtitle("Valores unitários medianos estimados \nbase: jul. 2000 \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'))+
  guides(fill = guide_legend(title = "R$/m2",
                             label.position = "right",
                             title.position = "top", title.vjust = 1)) 

plot_2000b

# Com os bairros

plot_2000b_bairros=ggplot(bairros.df)+
  geom_polygon(aes(x=long, y=lat, group=group), colour="black", size=0.3, fill=NA)+
  geom_text(data=bairros, aes(long, lat, label = bairros), size=2.5, col='black') +
  coord_map()+
  gg(reproject(pred_loci2000.sp.log), alpha=0.6)+
  scale_fill_gradientn(colours=pal16(44), breaks=seq(1,9,0.5),
                       labels = labs,
                       limits=c(1,9))+
  ggtitle("Valores unitários medianos estimados \nbase: jul. 2000 \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'))+
  guides(fill = guide_legend(title = "R$/m2",
                             label.position = "right",
                             title.position = "top", title.vjust = 1))+
  geom_polygon(data=urbano.df, aes(x=long, y=lat, group=group), colour="red", size=0.3, fill=NA)


plot_2000b_bairros


# 01/07/2021

plot_2021=ggplot()+
  gg(reproject(pred_loci2021.sp))+
  scale_fill_gradientn(colours=pal18(26),  n.breaks=20) +
  ggtitle("Valores unitários medianos estimados \nbase: jul. 2021 \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.5, 'cm'),legend.spacing.y = unit(0.5, 'cm'))+
  guides(fill = guide_legend(title = "R$/m2",
                             title.position = "top", title.vjust = 1)) 
plot_2021

# Nessa imagem foram arbitrados os limites da escala e os intervalos
# de modo a que ela possa ser posta ao lado de outra (plot_2021b) compartilhando
# a mesma escala de cores/valores

# Os valores estimados foram log - transformados para que a escala de cores 
# exibisse diferenças mais pronunciadas   


plot_2021b=ggplot()+
  gg(reproject(pred_loci2021.sp.log))+
  scale_fill_gradientn(colours=pal16(44), breaks=seq(1,9,0.5),
                       labels = labs,
                       limits=c(1,9))+
  ggtitle("Valores unitários medianos estimados \nbase: jul. 2021 \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'))+
  guides(fill = guide_legend(title = "R$/m2",
                             label.position = "right",
                             title.position = "top", title.vjust = 1)) 

plot_2021b


# Com os bairros

plot_2021b_bairros=ggplot(bairros.df)+
  geom_polygon(aes(x=long, y=lat, group=group), colour="black", size=0.3, fill=NA)+
  geom_text(data=bairros, aes(long, lat, label = bairros), size=2.5, col='black') +
  coord_map()+
  gg(reproject(pred_loci2021.sp.log), alpha=0.6)+
  scale_fill_gradientn(colours=pal16(44), breaks=seq(1,9,0.5),
                       labels = labs,
                       limits=c(1,9))+
  ggtitle("Valores unitários medianos estimados \nbase: jul. 2021 \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'))+
  guides(fill = guide_legend(title = "R$/m2",
                             label.position = "right",
                             title.position = "top", title.vjust = 1))+
  geom_polygon(data=urbano.df, aes(x=long, y=lat, group=group), colour="red", size=0.3, fill=NA)


plot_2021b_bairros


# As duas imagens lado a lado com uma única escala cromática de valores (via cowplot)

plot_combinado=plot_grid(plot_2000b+theme(legend.position = "none"),
                         plot_2021b+theme(legend.position = "none"))
legend=get_legend(plot_2000b)
plot1=plot_grid(plot_combinado,legend, ncol = 2, rel_widths  = c(1, 0.3))
plot(plot1) 

# plot_grid(plot_2000b, plot_2021b, labels = "")


plot_combinado_bairros=plot_grid(plot_2000b_bairros+theme(legend.position = "none"),
                                 plot_2021b_bairros+theme(legend.position = "none"))
legend=get_legend(plot_2000b_bairros)
plot2=plot_grid(plot_combinado_bairros,legend, ncol = 2, rel_widths  = c(1, 0.3))
plot(plot2) 

plot_grid(plot_2000b_bairros, plot_2021b_bairros, labels = "")




# As duas imagens lado a lado com uma única escala cromática de valores (via spplot)
# Adicionar contour=TRUE para linhas de contorno

p1 = spplot(pred_loci2000.sp.log, zcol = "V_UNIT", at = seq(1,9,0.5),
            colorkey = list(space="bottom"), scales = list(draw = TRUE),
            xlab=list(label="Coordenada métrica X (ESPG 29192)", vjust = -.2),
            ylab="Coordenada métrica Y (ESPG 29192)",
            col.regions =pal14)


p2 = spplot(pred_loci2021.sp.log, zcol = "V_UNIT",  at = seq(1,9,0.5),
            xlab=list(label="Coordenada métrica X (ESPG 29192)", vjust = -.2),
            ylab="Coordenada métrica Y (ESPG 29192)",
            col.regions=pal14)

update(c(p1, p2), layout = c(2, 1), as.table = TRUE) # 1 column, 2 rows


#########################################
# Proporção valor unitário 07/2000/valor unitário 07/2021
#########################################

pred_diff=data.frame(pred_loci2000$x,pred_loci2000$y, abs(pred_loci2021$V_UNIT-pred_loci2000$V_UNIT)/abs(pred_loci2000$V_UNIT))
colnames(pred_diff)=c("x", "y", "V_UNIT")
pred_diff.sp=pred_diff
coordinates(pred_diff.sp)=~x+y
proj4string(pred_diff.sp)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_diff.sp)=TRUE

plot_diff=ggplot()+
  gg(reproject(pred_diff.sp))+
  scale_fill_gradientn(colours=pal16(100), breaks=seq(0,45,2.5), limits=c(0,45)  )+
  ggtitle("Variação proporcional relativa entre valores unitários medianos estimados \n(Londrina, PR)")+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(plot.title=element_text(color="black", size=12, face="italic"))+
  theme(legend.position = "right", legend.justification = "center", legend.direction = "vertical",
        legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'))+
  guides(fill = guide_legend(title = "Razão: \nv. unit.2021/\nv. unit. 2000",
                             label.position = "right",
                             title.position = "top", title.vjust = 1)) 

plot_diff


#########################################
# Renderizando as imagens para a geração de uma maquete em 3d
#########################################


# Para 01/07/2000 
# Escala logarítimica

plot_2000b=ggplot(bairros.df)+
  gg(reproject(pred_loci2000.sp.log))+
  scale_fill_gradientn(colours=pal16(44), breaks=breaks,
                       labels = labs,
                       limits=c(1,9))+
  stat_contour(data = pred_loci2000.df.log.WSG84, aes(x=x, y=y, z = as.vector(V_UNIT)), alpha=0.5, color="red", bins=30)+
  ggtitle("Valores unitários medianos estimados (base: jul. 2000) \n(Londrina, PR)")+
  theme(plot.title=element_text(color="black", size=16, face="italic"))+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(legend.position = "right", legend.justification = "top", legend.direction = "vertical",
        legend.spacing.x = unit(0.5, 'cm'),legend.spacing.y = unit(0.5, 'cm'),  legend.key.size = unit(1.5, "cm"))+
  labs(fill="R$/m2")+
  geom_polygon(data=urbano.df, aes(x=long, y=lat, group=group), colour="red", size=0.3, fill=NA)+
  geom_polygon(aes(x=long, y=lat, group=group), colour="black", size=0.3, fill=NA)+
  geom_text(data=bairros, aes(long, lat, label = bairros), size=2.5, col='black') 


plot_gg(plot_2000b,
        multicore = FALSE,
        width = 8, 
        height = 9, 
        scale = 300, 
        zoom = 0.6, 
        background = "#afceff",
        shadowcolor = "#3a4f70")

# Salvando 

render_snapshot(filename = "<...>/dissertacao/estimados/3D_londrina.2000.log")


# Gerando uma animação da imagem que está na tela
# >>>não fechar a janela da imagem 3d<<<

render_movie("<...>/dissertacao/estimados/3D_londrina_2000_log.mp4", frames = 720, fps=30,zoom=0.6,fov = 30)

# Para 01/07/2021
# Escala logarítimica

plot_2021b=ggplot(bairros.df)+
  gg(reproject(pred_loci2021.sp.log))+
  scale_fill_gradientn(colours=pal16(44), breaks=breaks,
                       labels = labs,
                       limits=c(1,9))+
  stat_contour(data = pred_loci2021.df.log.WSG84, aes(x=x, y=y, z = as.vector(V_UNIT)), alpha=0.5, color="red", bins=30)+
  ggtitle("Valores unitários medianos estimados (base: jul. 2021) \n(Londrina, PR)")+
  theme(plot.title=element_text(color="black", size=16, face="italic"))+
  theme(axis.title.x = element_text(color="black", size=10, face="italic"))+
  theme(axis.title.y = element_text(color="black", size=10, face="italic"))+
  theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0))+
  theme(axis.text.y = element_text(face="plain", color="black", size=10, angle=0))+
  scale_y_continuous(name="Latitude")+
  scale_x_continuous(name="Longitude")+
  coord_fixed(ratio=1)+
  theme(legend.position = "right", legend.justification = "top", legend.direction = "vertical",
        legend.spacing.x = unit(0.5, 'cm'),legend.spacing.y = unit(0.5, 'cm'),  legend.key.size = unit(1.5, "cm"))+
  labs(fill="R$/m2")+
  geom_polygon(data=urbano.df, aes(x=long, y=lat, group=group), colour="red", size=0.3, fill=NA)+
  geom_polygon(aes(x=long, y=lat, group=group), colour="black", size=0.3, fill=NA)+
  geom_text(data=bairros, aes(long, lat, label = bairros), size=2.5, col='black') 


plot_gg(plot_2021b,
        multicore = FALSE,
        width = 8, 
        height = 9, 
        scale = 300, 
        zoom = 0.6, 
        background = "#afceff",
        shadowcolor = "#3a4f70")

# Salvando 

render_snapshot(filename = "<...>/dissertacao/3D_londrina.2021.log")


# Gerando uma animação da imagem que está na tela
# >>>não fechar a janela da imagem 3d<<<

render_movie("<...>/dissertacao/3D_londrina_2021_log.mp4", frames = 720, fps=30,zoom=0.6,fov = 30)



########################################################################################################################
# Estimação dos pontos do grid nas datas de 01/01/2000 a 01/07/2021 (a cada 6 meses)
# Criação de objetos espaciais com os resultados
########################################################################################################################

# Função para auxiliar a estimação do grid em todas as datas escolhidas

estime=function(at, relevo, pavimentacao, natureza, implantacao) {  
  names=c("jan/2000","jul/2000","jan/2001","jul/2001","jan/2002","jul/2002",
          "jan/2003","jul/2003","jan/2004","jul/2004","jan/2005","jul/2005",
          "jan/2006","jul/2006","jan/2007","jul/2007","jan/2008","jul/2008",
          "jan/2009","jul/2009","jan/2010","jul/2010","jan/2011","jul/2011",
          "jan/2012","jul/2012","jan/2013","jul/2013","jan/2014","jul/2014",
          "jan/2015","jul/2015","jan/2016","jul/2016","jan/2017","jul/2017",
          "jan/2018","jul/2018","jan/2019","jul/2019","jan/2020","jul/2020",
          "jan/2021","jul/2021")
  datas=as.numeric(seq(as.Date("01/01/2000", format="%d/%m/%Y"), by="6 months",length.out=44),tz="UTC", origin="1970-01-01")
  fit=data.frame()
  dados=data.frame(
    UTM_X=tab$UTM_X,
    UTM_Y=tab$UTM_Y,
    DATA=0,
    AT=at, 
    RELEVO=relevo,
    PAVIMENTACAO=pavimentacao,
    #RENDA=tab$RENDA, #a variável RENDA não foi utilizada no modelo
    NATUREZA=natureza, 
    IMPLANTACAO=implantacao)
  for(i in 1: length(datas)){
    dados$DATA=as.numeric(datas[i])
    fit_temp=predict(object=mod_teste, what="mu", newdata = dados, type = 'response')
    fit=rbind(fit, fit_temp)
  }
  fit=t(fit)
  fit=data.frame(fit)
  rownames(fit)=NULL
  colnames(fit)=names
  return(fit) 
}

# Qual área escolher???
# summary(dados_treino_terrenos$AT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 157.0   290.8   377.0  1617.1   610.0 43088.0 
# Mediana= 377m2

# Estimando o terreno na situação paradigma usando a função estime

result_pred_estime_2000_2021 = estime(at=377, relevo="Plano", natureza="Transação", pavimentacao="Pav. asfáltica", implantacao="Isolado")

# Salvando 
# saveRDS(object=result_pred_estime_2000_2021, file="<...>/dissertacao/estimados/result_pred_estime_2000_2021")


# Criando um geo-objeto 
# da estimação do grid  
# nas 44 datas (renomeei as colunas para adequar a legenda do spplot)

# Dataframe na escala original

pred_geral=data.frame(x=tab$UTM_X, y=tab$UTM_Y, result_pred_estime_2000_2021)
class(pred_geral)
rownames(pred_geral)
colnames(pred_geral)


# SpatialPointsDataframe

pred_geral.sp=pred_geral
coordinates(pred_geral.sp)=~x+y
class(pred_geral.sp)
proj4string(pred_geral.sp)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_geral.sp)=TRUE

# Dataframe na escala logaritmica
# (a transformação para essa escala visa apenas dar um destaque visual maior às variações entre as áreas urbanas)

pred_geral.log=data.frame(x=tab$UTM_X, y=tab$UTM_Y, log(result_pred_estime_2000_2021))
class(pred_geral.log)
rownames(pred_geral.log)
colnames(pred_geral.log)

# SpatialPointsDataframe

pred_geral.sp.log=pred_geral.log
coordinates(pred_geral.sp.log)=~x+y
class(pred_geral.sp.log)
proj4string(pred_geral.sp.log)=CRS('+init=epsg:29192')  #Atribuindo o CRS sem reprojetar (não precisa)
gridded(pred_geral.sp.log)=TRUE


#########################################
# Geração de imagens no terminal 
# recortes temporais 
#########################################

# Escala original
# Adicionar <contour=TRUE> para linhas de contorno

spplot(pred_geral.sp, c('jan.2000','jul.2000', 'jan.2001','jul.2001','jan.2002','jul.2002','jan.2003','jul.2003',
                        'jan.2004','jul.2004','jan.2005','jul.2005','jan.2006','jul.2006','jan.2007','jul.2007',
                        'jan.2008','jul.2008','jan.2009','jul.2009','jan.2010','jul.2010','jan.2011','jul.2011',
                        'jan.2012','jul.2012','jan.2013','jul.2013','jan.2014','jul.2014','jan.2015','jul.2015',
                        'jan.2016','jul.2016','jan.2017','jul.2017','jan.2018','jul.2018','jan.2019','jul.2019',
                        'jan.2020','jul.2020','jan.2021','jul.2021'),as.table=TRUE,
       col.regions=(pal16(44)), colorkey=list(space="bottom"),
       main="Alterações do valor unitário \n Londrina, PR \n (jan. 2000 ~ jul. 2021)",
       cex=1)

# Escala logaritmica MAS com a legenda já transformada
# Adicionar <contour=TRUE> para linhas de contorno

spplot(pred_geral.sp.log, c('jan.2000','jul.2000', 'jan.2001','jul.2001','jan.2002','jul.2002','jan.2003','jul.2003',
                            'jan.2004','jul.2004','jan.2005','jul.2005','jan.2006','jul.2006','jan.2007','jul.2007',
                            'jan.2008','jul.2008','jan.2009','jul.2009','jan.2010','jul.2010','jan.2011','jul.2011',
                            'jan.2012','jul.2012','jan.2013','jul.2013','jan.2014','jul.2014','jan.2015','jul.2015',
                            'jan.2016','jul.2016','jan.2017','jul.2017','jan.2018','jul.2018','jan.2019','jul.2019',
                            'jan.2020','jul.2020','jan.2021','jul.2021'),as.table=TRUE,
       col.regions=pal16(44), 
       main="Alterações do valor unitário \n Londrina, PR \n (jan. 2000 ~ jul. 2021)",
       cex=1, colorkey = list(space="bottom", labels = list( labels = c("R$ 7/m2","R$ 20/m2","R$ 50/m2","R$ 150/m2","R$ 400/m2","R$ 1090/m2","R$ 2980/m2"),        
                                                             width = 1, cex = 1)))

#########################################
# Com contourlines
# por exemplo, para jan.2000
#########################################


spplot(pred_geral.sp, 'jan.2000',col.regions=pal16(44), region=TRUE,
       contour=TRUE,  main="Alterações do valor unitário \n (Londrina, PR) ",
       cex=1, labels = list(cex = 0.5))


#########################################
# Geração de um kml de um período específico
# usando pred_geral existente
# e uma versão ainda existente do plotGoogleMaps que gera e abre um html
# por exemplo, para jan.2000
#########################################


plotKML(pred_geral.sp['jan.2000'],alpha=0.30, colour_scale=pal14, contour=TRUE,
        file.name ='Modelo_proposto.kml')

plotGoogleMaps(pred_geral.sp['jan.2000'], colPalette=pal3, at=seq(1,201,10))

mapview(pred_geral.sp['jan.2000'], col.regions=tim.colors(1000))



#########################################
# Imagens usando a biblioteca 'tmaps' e as 
# estimativas no pred_geral.sp/pred_geral.sp.log 
#########################################

#Instale as últimas versões dos desenvolvedores
#install_github("r-tmap/tmaptools")
#install_github("r-tmap/tmap")

# Imagens lado a lado de jun.2000 e jun.2021
# escala cromática 

datas=names(pred_geral.sp.log)
datas1=c("jul.2000", "jul.2021")
legend_title=expression("Valor unitário mediano (R$/m"^2*")")
seq=seq(1,9,0.5)
rotulos=seq
rot_log=round(exp(rotulos),2)

# Imagens de fundo (Mapbox, GEarth, Stamen)

# Obtendo uma imagem do Mapbox (a API key é gratuita e não exige cc)

Sys.setenv(MAPBOX_API_KEY='< sua api.key >')
lda_mbox=cc_location(cbind(med_lon, med_lat), buffer = 17000, type = "mapbox.satellite")
lda_mbox=cc_location(cbind(med_lon, med_lat), buffer = 17000, type = "mapbox.satellite")


# Via Google Earth: a API key não é gratuita e exige um CC para debitar, 
# no caso de grandes volumes de acesso, como em uso comercial
# Para mapas do Google Earth usar a função 
# register_google para a chave da API, a qual requer a versão 
# do mantenedor do ggmap: 
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

register_google(key = "< sua api.key >") 
lda_google=get_map(location=c(lon=mean(lon),lat=mean(lat)), zoom=11,maptype="satellite",source="google")


# Convertendo para raster
# Função obtida em https://gis.stackexchange.com/questions/155334/ggmap-clip-a-map

ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

lda_google=ggmap_rast(lda_google)


# Obtendo uma imagem do Stamen

lda_stamen=get_map(c(-51.3,-23.45,-51.05,-23.2), 
                   zoom=12, 
                   maptype = "terrain",
                   source = "stamen")
lda_stamen=ggmap_rast(lda_stamen)


# IMAGEM 'amostra_tmaps' (sobre MapBox ou GoogleEarth)

tm_shape(lda_mbox,
         raster.downsample=FALSE,
         projection ='+init=epsg:4326')+
  tm_rgb()+
  tm_add_legend(type="fill",
                labels=c("Perímetro urbano", 
                         "Nome do bairro", 
                         "Amostra"),
                col=c("blue", "yellow", "red"))+
  tm_shape(shape_londrina_urbano)+
  tm_borders(col="blue", lwd=2)+
  tm_shape(shape_londrina_bairros)+
  tm_borders(col=NULL, lty=2, lwd=0.5)+
  tm_text('bairros',
          col="yellow",
          size=0.5)+
  tm_grid(labels.size = 0.7)+
  tm_shape(amostra)+
  tm_symbols(shape=1,
             size=0.2,
             scale=0.5,
             col="red")+
  tm_compass(type = "4star", 
             size = 1.8, 
             text.color = "white",
             position = c("right", "top"))+
  tm_scale_bar(text.size = 0.5, 
               text.color = "white",
               position = c("left", "bottom"))+
  tm_layout(main.title = "Amostra \nLondrina, PR (23,3197S; 51,1662W) \nimagem de fundo: Mapbox",
            main.title.position ="left",
            main.title.size = 1,
            main.title.fontface = "bold",
            legend.text.color = "white")


# IMAGEM 'amostra_tmaps2' (sobre Stamen Map)

tm_shape(lda_stamen)+
  tm_rgb(alpha = 1)+
  tm_shape(shape_londrina_urbano)+
  tm_borders(col="blue", lwd=2)+
  tm_shape(shape_londrina_bairros)+
  tm_borders(col=NULL, lty=2, lwd=0.5)+
  tm_text('bairros', col="darkgreen",size=0.5)+
  tm_grid(labels.size = 0.7)+
  tm_shape(amostra)+
  tm_symbols(shape=1,
             size=0.2,
             scale=0.5,
             col="red")+
  tm_compass(type = "4star", 
             size = 1.8, 
             text.color = "black",
             position = c("RIGHT", "TOP"))+
  tm_scale_bar(text.size = 0.5, 
               text.color = "black",
               position = c("LEFT", "BOTTOM"))+
  tm_layout(main.title = "Amostra \nLondrina, PR (23,3197S; 51,1662W) \nimagem de fundo: Stamen Maps, 2021",
            main.title.position ="left",
            main.title.size = 1,
            main.title.fontface = "bold",
            legend.text.color = "black",
            legend.text.fontface = "bold",
            fontface = "bold")+
  tm_add_legend(type="fill",
                labels=c("Perímetro urbano", 
                         "Nome do bairro", 
                         "Amostra"),
                col=c("blue", "darkgreen", "red"))



#########################################
# IMAGEM 'imagem_tmmap_combinada'

tm_shape(pred_geral.sp.log,
         projection ='+init=epsg:4326')+
  tm_raster(datas1,
            alpha = 0.7,
            palette = pal14, 
            breaks = seq,
            title = legend_title,
            style = "cont")+
  tm_grid(labels.size = 0.8,
          labels.inside.frame = FALSE,
          n.x = 5, n.y = 8, labels.rot = c(0,0))+
  tm_shape(shape_londrina_urbano)+
  tm_borders(col="blue",
             lwd=2)+
  tm_shape(shape_londrina_bairros)+
  tm_borders(col="red",
             lty=2)+
  tm_text('bairros',
          col="darkgreen",
          size=0.6)+
  tm_add_legend(type="fill",
                labels=c("Fora do per. urbano","Perímetro urbano", "Nome do bairro"),
                col=c("white", "blue", "darkgreen"),
                size = 1)+
  tm_layout(between.margin=0.1,
            main.title = "",
            main.title.position = "center",
            attr.outside = TRUE,
            attr.outside.position = "BOTTOM",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1.5,
            legend.text.size = 1,
            scale = 1,
            frame.lwd = 5,
            outer.bg.color = "white",
            bg.color="white",
            inner.margins=c(0.05,0.05,0.05,0.05),
            outer.margins = c(0.05,0.05,0.05,0.05),
            panel.labels = c("base: jul. 2000", "base: jul. 2021"),
            panel.label.size = 1,
            panel.label.color = "black",
            panel.label.height = 1.1,
            panel.label.rot = c(0,0),
            legend.format=list(fun=function(x) {
              ifelse(x %in% seq, rot_log, "")
            }))+
  tm_compass(type = "4star", size = 1.5)+
  tm_scale_bar(text.size = 0.7)


#########################################
# IMAGEM 'tm_map_2000_2021'

dev.off()
dev.new()
graphics.off()

tm_shape(pred_geral.sp.log)+
  tm_raster(datas,
            alpha = 0.7,
            palette = pal14, 
            breaks = seq,
            title = legend_title,
            style="cont",
            legend.is.portrait=FALSE)+
  tm_facets(free.scales = FALSE)+
  tm_shape(shape_londrina_urbano)+
  tm_borders(col="blue",
             lwd=1)+
  tm_layout(between.margin=0.1,
            outer.bg.color = "white",
            bg.color="white",
            legend.stack = "horizontal",
            legend.just = "center",
            legend.title.size = 1,
            legend.format=list(fun=function(x) {
              ifelse(x %in% seq, rot_log, "")
            }),
            main.title = "",
            main.title.position = "center",
            attr.outside = TRUE,
            legend.outside.position = "bottom",
            panel.labels = datas,
            panel.label.size = 1,
            panel.label.color = "black",
            panel.label.height = 1.1,
            panel.label.rot = c(0,0),
            scale=0.95,
            legend.outside.size = 0.15)



#########################################
# Geração de um html de todo o período 
#########################################

html_2000_2021=mapview(
  pred_geral.sp,
  alpha.regions=0.6,
  legend.opacity=0.8,
  layer.name=c("jan.2000","jul.2000","jan.2001","jul.2001","jan.2002","jul.2002",
               "jan.2003","jul.2003","jan.2004","jul.2004","jan.2005","jul.2005",
               "jan.2006","jul.2006","jan.2007","jul.2007","jan.2008","jul.2008",
               "jan.2009","jul.2009","jan.2010","jul.2010","jan.2011","jul.2011",
               "jan.2012","jul.2012","jan.2013","jul.2013","jan.2014","jul.2014",
               "jan.2015","jul.2015","jan.2016","jul.2016","jan.2017","jul.2017",
               "jan.2018","jul.2018","jan.2019","jul.2019","jan.2020","jul.2020",
               "jan.2021","jul.2021"),
  burst=TRUE,
  legend=TRUE,
  homebutton=FALSE,
  hide=TRUE)

# Salvando

mapshot(html_2000_2021, url = paste0(getwd(), "/Modelo proposto 2000_2021.html"))



#########################################
# Criação de um objeto espaço-temporal 
#########################################

#cf. Spatio-temporal Statistics with R (Cressie, Wikle e Mangion p. 59)

spat_part=SpatialPoints(coords = tab[, c("UTM_X", "UTM_Y")])
spat_part=as(spat_part, "SpatialPixels")
proj4string(spat_part)=CRS('+init=epsg:29192')

temp_part=as.Date(c('2000/01/01','2000/07/01','2001/01/01','2001/07/01','2002/01/01','2002/07/01','2003/01/01','2003/07/01',
                    '2004/01/01','2004/07/01','2005/01/01','2005/07/01','2006/01/01','2006/07/01','2007/01/01','2007/07/01',
                    '2008/01/01','2008/07/01','2009/01/01','2009/07/01','2010/01/01','2010/07/01','2011/01/01','2011/07/01',
                    '2012/01/01','2012/07/01','2013/01/01','2013/07/01','2014/01/01','2014/07/01','2015/01/01','2015/07/01',
                    '2016/01/01','2016/07/01','2017/01/01','2017/07/01','2018/01/01','2018/07/01','2019/01/01','2019/07/01',
                    '2020/01/01','2020/07/01','2021/01/01','2021/07/01'))

temp_part=as.POSIXct(temp_part, format="%Y/%m/%dT%H:%M:%SZ")


dados=data.frame(as.vector(c(result_pred_estime_2000_2021$`jan/2000`,result_pred_estime_2000_2021$`jul/2000`,result_pred_estime_2000_2021$`jan/2001`,result_pred_estime_2000_2021$`jul/2001`,result_pred_estime_2000_2021$`jan/2002`,result_pred_estime_2000_2021$`jul/2002`,
                             result_pred_estime_2000_2021$`jan/2003`,result_pred_estime_2000_2021$`jul/2003`,result_pred_estime_2000_2021$`jan/2004`,result_pred_estime_2000_2021$`jul/2004`,result_pred_estime_2000_2021$`jan/2005`,result_pred_estime_2000_2021$`jul/2005`,
                             result_pred_estime_2000_2021$`jan/2006`,result_pred_estime_2000_2021$`jul/2006`,result_pred_estime_2000_2021$`jan/2007`,result_pred_estime_2000_2021$`jul/2007`,result_pred_estime_2000_2021$`jan/2008`,result_pred_estime_2000_2021$`jul/2008`,
                             result_pred_estime_2000_2021$`jan/2009`,result_pred_estime_2000_2021$`jul/2009`,result_pred_estime_2000_2021$`jan/2010`,result_pred_estime_2000_2021$`jul/2010`,result_pred_estime_2000_2021$`jan/2011`,result_pred_estime_2000_2021$`jul/2011`,
                             result_pred_estime_2000_2021$`jan/2012`,result_pred_estime_2000_2021$`jul/2012`,result_pred_estime_2000_2021$`jan/2013`,result_pred_estime_2000_2021$`jul/2013`,result_pred_estime_2000_2021$`jan/2014`,result_pred_estime_2000_2021$`jul/2014`,
                             result_pred_estime_2000_2021$`jan/2015`,result_pred_estime_2000_2021$`jul/2015`,result_pred_estime_2000_2021$`jan/2016`,result_pred_estime_2000_2021$`jul/2016`,result_pred_estime_2000_2021$`jan/2017`,result_pred_estime_2000_2021$`jul/2017`,
                             result_pred_estime_2000_2021$`jan/2018`,result_pred_estime_2000_2021$`jul/2018`,result_pred_estime_2000_2021$`jan/2019`,result_pred_estime_2000_2021$`jul/2019`,result_pred_estime_2000_2021$`jan/2020`,result_pred_estime_2000_2021$`jul/2020`,
                             result_pred_estime_2000_2021$`jan/2021`,result_pred_estime_2000_2021$`jul/2021`)))


kml_2000_2021=STFDF(sp = spat_part, time = temp_part, data = dados) 

# Objeto espaço temporal na escala logaritmica
# (a transformação para essa escala visa apenas dar um destaque visual maior às variações entre as áreas urbanas)

dados_log=log(dados)
kml_2000_2021_log=STFDF(sp = spat_part, time = temp_part, data = dados_log) 


# Geração de imagens lado-a-lado no terminal 
# semelhante ao spplot
# Adicionar contour=TRUE para linhas de contorno

stplot(kml_2000_2021, colorkey=list(space="bottom"), 
       main="Alterações do valor unitário \n Londrina, PR \n Situação paradigma \n (jan. 2000 ~ jul. 2021)",
       cex=1,  col.regions=pal16(44))



#########################################################
# Os dois objetos espaço temporais:
# kml_2000_2021 e
# kml_2000_2021_log
# possuem as estimativas de todo o grid nos 44 períodos temporais arbitrados e podem 
# gerar um KML para ser lido pelo Google Earth, via de regra, usando-se a função:
# plotKML(kml_2000_2021, size=0.1, colour_scale = pal2(44), open.kml = FALSE, time=temp_part)
# Todavia, para essa situação, a função consume muito tempo!!
# O que essa função faz é gerar um arquivo 'kml' que ordena uma sucessão de imagens associadas
# 44 imagens, uma para cada tempo arbitrado), associando sua localização espacial e períodos temporais 
# criando ao fim uma legenda cromática comum a todos os períodos temporais 
#########################################################



#########################################################
# Isso pode ser feito manualmente e muito mais rápido:
#
# i. gere as imagens individualmente com a função plotKML 
# (muito rápida para apenas um período de tempo)
# tendo pré-definido seu argumento z.lim (da legenda cromática) de modo a poder ser comum a todos os 44
# períodos de tempo 
#
# ii. edite o arquivo kml:
#
# <kml xmlns:xsd="https://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" xmlns:xmlns="https://www.opengis.net/kml/2.2/" version="1.0">
#   <Document>
#   <name>kml_teste2</name>
#   <open>1</open>
#   <Folder>
#   <name>RasterBrick</name>
# <GroundOverlay>
#   <name>colour.1</name>
#   <TimeSpan>
#   <begin>2000-01-01T15:00:00Z</begin>
#   <end>2000-06-30T15:00:00Z</end>
#   </TimeSpan>
#   <altitude>0</altitude>
#   <altitudeMode>clampToGround</altitudeMode>
#   <Icon>
#   <href>colour.1.png</href>
#   </Icon>
#   <LatLonBox>
#   <north>-23.23221</north>
#   <south>-23.44284</south>
#   <east>-51.07729</east>
#   <west>-51.25089</west>
#   </LatLonBox>
# </GroundOverlay>
# .
# .
# .
# insira as outras 43 imagens, com seus nomes e períodos temporais 
# .
# .
# .
# </Folder>
#   <ScreenOverlay>
#   <name>Legend</name>
#   <Icon>
#   <href>obj_legend.png</href>
#   </Icon>
#   <overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/>
#   <screenXY x="0" y="1" xunits="fraction" yunits="fraction"/>
#   </ScreenOverlay>
#   </Document>
#   </kml>
#########################################################

# Função para automatizar a geração das 44 imagens:

estime_kml=function() { 
  nomes=c("colour.1.kml","colour.2.kml","colour.3.kml","colour.4.kml","colour.5.kml",
          "colour.6.kml","colour.7.kml","colour.8.kml","colour.9.kml","colour.10.kml",
          "colour.11.kml","colour.12.kml","colour.13.kml","colour.14.kml","colour.15.kml",
          "colour.16.kml","colour.17.kml","colour.18.kml","colour.19.kml","colour.20.kml",
          "colour.21.kml","colour.22.kml","colour.23.kml","colour.24.kml","colour.25.kml",
          "colour.26.kml","colour.27.kml","colour.28.kml","colour.29.kml","colour.30.kml",
          "colour.31.kml","colour.32.kml","colour.33.kml","colour.34.kml","colour.35.kml",
          "colour.36.kml","colour.37.kml","colour.38.kml","colour.39.kml","colour.40.kml",
          "colour.41.kml","colour.42.kml","colour.43.kml","colour.44.kml")
  for(i in 1: 44){
    plotKML(pred_geral.sp.log[i], #aqui usando na escala logaritmica
            alpha=0.40,
            open.kml=FALSE,
            colour_scale=pal14,
            file.name = nomes[i],
            z.lim=c(1,9)) 
  }
}

estime_kml()



#########################################################
# Estimar um elemento novo (pontualmente)
#########################################################


REF=1000  
UTM_X=483732
UTM_Y=7421352
DATA=as.numeric(as.Date("10/01/2022", format="%d/%m/%Y"),tz="UTC", origin="1970-01-01")  #ATUAL
AT=1968.75 
NATUREZA="Transação"
IMPLANTACAO="Isolado"
PAVIMENTACAO="Pav. asfáltica"
RELEVO="Declive"

novo_dado=data.frame(UTM_X, UTM_Y,DATA,AT,NATUREZA,IMPLANTACAO, PAVIMENTACAO, RELEVO)


# Estimando já na escala da variável

estimativa=predictAll(object=mod_teste, newdata = novo_dado, type = 'response')

# O efeito de cada um dos termos pode ser obtido setando-se type="terms"
# somado cada um tomando-se exp(soma)=estimação

estimativa_termos=predictAll(object=mod_teste, newdata = novo_dado, type = 'terms')
estimativa_termos$mu
estimativa_termos$sigma
estimativa_termos$nu
estimativa_termos$tau

#########################################################
# Plotar a curva da densidade da distribuição no terminal (poderá exigir alguns ajustes nas posições dos textos)
#########################################################

lab_vunit_est=expression("Valores unitários medianos estimados" ~ ("R$"~"/"~m^{2}))

est_mu=formatC(estimativa$mu, digits=2, format='f')
est_sigma=formatC(estimativa$sigma, digits=2, format='f')
est_nu=formatC(estimativa$nu, digits=2, format='f')
est_tau=formatC(estimativa$tau, digits=2, format='E')

lab1=paste("Mediana: hat(mu) ==",  est_mu)
lab2=paste("Variabilidade: hat(sigma)==", est_sigma)
lab3=paste("Assimetria: hat(nu)==", est_nu)
lab4=paste("Curtose: hat(tau)==", est_tau)
lab5=paste("p(Valor < Limite inferior) < 0,10 \n Limite inferior=", paste('(R$/m2)',formatC(lim_inf, digits=2, format='f')))
lab6=paste("p(Valor > Limite superior) < 0,10 \n Limite superior=", paste('(R$/m2)',formatC(lim_sup, digits=2, format='f')))
lab7=paste("p(Valor < Valor médio)=p(Valor > Valor mediano)= 0,50 \n Valor médio=",  paste('(R$/m2)',formatC(estimativa$mu, digits=2, format='f')))


d_inf=dBCTo(lim_inf, mu = estimativa$mu, sigma = estimativa$sigma, nu = estimativa$nu, tau = estimativa$tau)
d_med=dBCTo(lim_50, mu = estimativa$mu, sigma = estimativa$sigma, nu = estimativa$nu, tau = estimativa$tau)
d_sup=dBCTo(lim_sup, mu = estimativa$mu, sigma = estimativa$sigma, nu = estimativa$nu, tau = estimativa$tau)

ggplot(data.frame(x = c(1000,10000)), aes(x)) +
  stat_function(fun = function(x)dBCTo(x, mu = estimativa$mu, sigma = estimativa$sigma,
                                       nu = estimativa$nu, tau = estimativa$tau)) +
  stat_function(fun = function(x)dBCTo(x, mu = estimativa$mu, sigma = estimativa$sigma,
                                       nu = estimativa$nu, tau = estimativa$tau), 
                xlim=c(lim_inf,lim_sup),
                geom = "area",
                fill = 'gray', 
                alpha = 0.6)+
  ylab("Densidade")+
  xlab(lab_vunit_est) +
  labs(title=("Função densidade de probabilidade dos valores (BCTo)"),
       subtitle =("(estimada para o imóvel avaliando)"))+
  geom_segment(aes(x = lim_inf, y = 0, xend = lim_inf, yend = d_inf), color="blue", lty=2, lwd=0.3)+
  geom_segment(aes(x = lim_sup, y = 0, xend = lim_sup, yend = d_sup), color="red", lty=2, lwd=0.3)+
  geom_segment(aes(x = lim_50, y = 0, xend = lim_50, yend = d_med), color="darkgreen", lty=2, lwd=0.3)+
  annotate(geom="text", x=lim_inf, y=d_inf, label=lab5, angle=0, vjust=0, hjust=0, color="blue",size=3)+
  annotate(geom="text", x=lim_50, y=d_med, label=lab7, angle=0, vjust=0, hjust=0, color="darkgreen", size=3)+
  annotate(geom="text", x=lim_sup, y=d_sup, label=lab6, angle=0, vjust=0, hjust=0, color="red", size=3)+
  theme_bw()


#########################################################
# Variograma experimental espaço-temporal 
# https://stats.stackexchange.com/questions/496233/interpreting-a-distance-time-3d-variogram-for-variogram-modeling
#########################################################

# Parte espacial

tab2=data.frame(dados_treino_terrenos$UTM_X, dados_treino_terrenos$UTM_Y)
colnames(tab2)=c("UTM_X", "UTM_Y")
spat_part2=SpatialPoints(coords = tab2[, c("UTM_X", "UTM_Y")])
proj4string(spat_part2)=CRS('+init=epsg:29192')

# Parte temporal

temp_part2=as.Date(dados_treino_terrenos$DATA)

# Geo-objeto dos resíduos simples (brutos) do modelo 

res=as.vector(residuals(mod_teste, what='mu', type = 'simple'))
dados2=data.frame(res)

# Objeto espaço temporal

res_esp_temp=STIDF(sp = spat_part2,
                   time = temp_part2,
                   data = dados2)
class(res_esp_temp)


stplot(res_esp_temp, col.regions=pal14, 
       main="Resíduos do modelo proposto jan. 2000 ~ jul. 2021 
       \nSituação paradigma")


str(res_esp_temp)
res_esp_temp@data$res

vv = variogramST(res~1, data=res_esp_temp,cutoff=2000, width=200,
                 tunit='days', tlags=seq(0, 1800, 200), twindow=120,
                 assumeRegular=F,na.omit=T)

cv=bpy.colors(n=6, cutoff.tails = )
palette(cv)

vv=variogramST(formula=res~1, data=res_esp_temp, 
               tlags=seq(0, 3600, 600), tunit='days', twindow=120,
               cutoff=2000, width = 2000/15, boundaries = seq(0, 2000, 200),
               progress = interactive(), 
               pseudo = TRUE, assumeRegular = FALSE, 
               na.omit = TRUE)


plot(vv,map=F, main="Variograma espaço temporal",
     xlab="Distância (m)")


plot(vv,map=T,main="Variograma espaço temporal",xlab="Distância espacial (m)", 
     ylab="Afastamento temporal (dias)") 
plot(vv,wireframe=T, main="Variograma espaçotemporal", xlab="Distância espacial (m)", 
     ylab="Afastamento temporal (dias)", zlab="Semivariância")




