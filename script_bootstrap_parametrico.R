library(librarian)

shelf(Hmisc,gamlss,gamlss.add,geobr,readxl,sf,rgdal,spatialEco,
      mapview,RSAGA,plotKML,inlabru,RColorBrewer,rayshader,av,
      rgl,maps,GISTools,geoR,spacetime,zoo,ggplot2,hnp,fields,
      cowplot,rosm,ggmap,spData,tmap,tmaptools,raster, fields,
      raster, spData, ceramic, plotGoogleMaps, dplyr, tibble)


########################################################################################################################
# Carregando resultados salvos
########################################################################################################################

setwd("<...>/dissertacao/dados")
load("dados_treino_terrenos")
load("mod_teste")


###########################################################################################
# BOOTSTRAP PARAMÉTRICO 
###########################################################################################

# Para que a estimação de cada modelo não seja muito demorada,
# mod_boot_p é uma versão do mod_teste com apenas 20 iterações: method = RS())
# qualquer outra situação, fazer mod_boot_np=mod_teste


mod_boot_p=gamlss(UNIT~ ga(~ti(AT, bs='cr'), method = "REML")+
                     NATUREZA+IMPLANTACAO+RELEVO+PAVIMENTACAO+                   
                     ga(~ti(UTM_X, UTM_Y, DATA, bs=c('tp','cr'), d=c(2,1)), method = "REML")+
                     ga(~ti(UTM_X, UTM_Y, bs='tp', d=2), method = "REML")+
                     ga(~ti(DATA, bs='cr'), method = "REML"),
                   sigma.formula = ~  ga(~ti(AT, bs='cr'), method = "REML")+
                     ga(~ti(UTM_X, UTM_Y,bs='tp'), method = "REML")+
                     ga(~ti(DATA, bs='cr'), method = "REML"),
                   nu.formula = ~ ga(~ti(AT, bs='cr'), method = "REML")+
                     ga(~ti(UTM_X, UTM_Y,bs='tp'), method = "REML")+
                     ga(~ti(DATA, bs='cr'), method = "REML"),
                   tau.formula = ~ ga(~ti(AT, bs='cr'), method = "REML"),
                   family=BCTo,
                   data=dados_treino_terrenos,
                   method = RS())


novos_dados=data.frame(dados_treino_terrenos,
                       fmu=fitted(mod_teste, "mu"),
                       fsigma=fitted(mod_teste, "sigma"),
                       fnu=fitted(mod_teste, "nu"),
                       ftau=fitted(mod_teste, "tau"))

fit_par=NULL
m=100

for(i in 1:m){
  mod_boot=NULL
  boot_data=data.frame()
  boot_data=novos_dados[sample(nrow(novos_dados), replace = TRUE), ]
  boot_data$UNIT=rBCTo(n=length(boot_data$REF), boot_data$fmu, 
                       sigma=boot_data$fsigma, nu=boot_data$fnu, 
                       tau = boot_data$ftau)
  boot_data=boot_data[, -c(1,7,8,9)]
  mod_boot=try(update(mod_boot_p, data=boot_data, what="mu"), silent = TRUE)
  if(isTRUE(class(mod_boot)!="NULL")) {next} else {
    fit_temp1=as.vector(mod_boot$mu.coefficients[1]) #intercepto
    fit_temp2=as.vector(mod_boot$mu.coefficients[3]) #transação
    fit_temp3=as.vector(mod_boot$mu.coefficients[4]) #condomínio
    fit_temp4=as.vector(mod_boot$mu.coefficients[5]) #aclive
    fit_temp5=as.vector(mod_boot$mu.coefficients[6]) #declive
    fit_temp6=as.vector(mod_boot$mu.coefficients[7]) #asfáltica
    fit_temp7=GAIC(mod_boot)                         #GAIC
    fit_temp=cbind(fit_temp1,fit_temp2,fit_temp3,
                   fit_temp4,fit_temp5,fit_temp6,
                   fit_temp7)
    fit_par=rbind(fit_par, fit_temp)}
  print(i)
}

fit_par
names=c("Intercepto", "Natureza (transação)",
        "Implantação (condomínio)", "Relevo (aclive)", 
        "Relevo (declive)", "Pavimentação (asfáltica)", "GAIC") 
colnames(fit_par)=names
fit_par

# write.csv(fit_par,"result_boot_par")
boot_par_mod_q=read.csv("result_boot_par")


#INTERCEPTO

delta_par_int=quantile((boot_par_mod_q$Intercepto-as.vector(mod_teste$mu.coefficients[1])), 
                       probs = c(0.025, 0.975)) 
as.vector(mod_teste$mu.coefficients[1])+delta_par_int

#TRANSACAO

delta_par_nat=quantile((boot_par_mod_q$Natureza..transação.-as.vector(mod_teste$mu.coefficients[3])), 
                       probs = c(0.025, 0.975)) 
as.vector(mod_teste$mu.coefficients[3])+delta_par_nat

#CONDOMINIO

delta_par_imp=quantile((boot_par_mod_q$Implantação..condomínio.-as.vector(mod_teste$mu.coefficients[4])), 
                       probs = c(0.025, 0.975)) 
as.vector(mod_teste$mu.coefficients[4])+delta_par_imp

#ACLIVE

delta_par_aclive=quantile((boot_par_mod_q$Relevo..aclive.-as.vector(mod_teste$mu.coefficients[5])), 
                          probs = c(0.025, 0.975)) 
as.vector(mod_teste$mu.coefficients[5])+delta_par_aclive

#DECLIVE

delta_par_declive=quantile((boot_par_mod_q$Relevo..declive.-as.vector(mod_teste$mu.coefficients[6])), 
                           probs = c(0.025, 0.975)) 
as.vector(mod_teste$mu.coefficients[6])+delta_par_declive

#PAVIMENTACAO

delta_par_pav=quantile((boot_par_mod_q$Pavimentação..asfáltica.-as.vector(mod_teste$mu.coefficients[7])), 
                       probs = c(0.025, 0.975)) 
as.vector(mod_teste$mu.coefficients[7])+delta_par_pav




ggplot(boot_par_mod_q, aes(x = Natureza..transação.))+
  geom_histogram(aes(y = ..density..), bins=50, fill="lightblue", color="black")+
  scale_x_continuous(name = "Estimativas obtidas", limits = c(-0.75,0)) +
  scale_y_continuous(name = "Densidade")+
  ggtitle("Histograma das estimativas ``bootstrapp´´ paramétricas:\ncoeficiente do fator: NATUREZA (se transação) ")+
  stat_function(fun = dnorm, colour="red",args = list(mean = mean(boot_par_mod_q$Natureza..transação.),
                                                      sd = sd(boot_par_mod_q$Natureza..transação.)))+
  theme_bw(base_size = 20)

ggplot(boot_par_mod_q, aes(x=Intercepto))+
  geom_histogram(aes(y = ..density..), bins=50, fill="lightblue", color="black")+
  scale_x_continuous(name = "Estimativas obtidas", limits = c(0,5)) +
  scale_y_continuous(name = "Densidade")+
  ggtitle("Histograma das estimativas ``bootstrapp´´ paramétricas:\nINTERCEPTO ")+
  stat_function(fun = dnorm, colour="red",args = list(mean = mean(boot_par_mod_q$Intercepto ),
                                                      sd = sd(boot_par_mod_q$Intercepto )))+
  theme_bw(base_size = 20)



ggplot(boot_par_mod_q, aes(x=Implantação..condomínio.))+
  geom_histogram(aes(y = ..density..), bins=50, fill="lightblue", color="black")+
  scale_x_continuous(name = "Estimativas obtidas", limits = c(0,1.5)) +
  scale_y_continuous(name = "Densidade")+
  ggtitle("Histograma das estimativas ``bootstrapp´´ paramétricas:\ncoeficiente do fator: IMPLANTACAO (se condomínio) ")+
  stat_function(fun = dnorm, colour="red",args = list(mean = mean(boot_par_mod_q$Implantação..condomínio. ),
                                                      sd = sd(boot_par_mod_q$Implantação..condomínio. )))+
  theme_bw(base_size = 20)


ggplot(boot_par_mod_q, aes(x=Relevo..aclive.))+
  geom_histogram(aes(y = ..density..), bins=80, fill="lightblue", color="black")+
  scale_x_continuous(name = "Estimativas obtidas", limits = c(-0.5, 0.5)) +
  scale_y_continuous(name = "Densidade")+
  ggtitle("Histograma das estimativas ``bootstrapp´´ paramétricas:\ncoeficiente do fator: RELEVO (se aclive) ")+
  stat_function(fun = dnorm, colour="red",args = list(mean = mean(boot_par_mod_q$Relevo..aclive. ),
                                                      sd = sd(boot_par_mod_q$Relevo..aclive. )))+
  theme_bw(base_size = 20)


ggplot(boot_par_mod_q, aes(x=Relevo..declive.))+
  geom_histogram(aes(y = ..density..), bins=80, fill="lightblue", color="black")+
  scale_x_continuous(name = "Estimativas obtidas", limits = c(-0.5, 0.5)) +
  scale_y_continuous(name = "Densidade")+
  ggtitle("Histograma das estimativas ``bootstrapp´´ paramétricas:\ncoeficiente do fator: RELEVO (se declive) ")+
  stat_function(fun = dnorm, colour="red",args = list(mean = mean(boot_par_mod_q$Relevo..declive. ),
                                                      sd = sd(boot_par_mod_q$Relevo..declive. )))+
  theme_bw(base_size = 20)


ggplot(boot_par_mod_q, aes(x=Pavimentação..asfáltica.))+
  geom_histogram(aes(y = ..density..), bins=20, fill="lightblue", color="black")+
  scale_x_continuous(name = "Estimativas obtidas", limits = c(0,1)) +
  scale_y_continuous(name = "Densidade")+
  ggtitle("Histograma das estimativas ``bootstrapp´´ paramétricas:\ncoeficiente do fator: PAVIMENTACAO (se asfáltica) ")+
  stat_function(fun = dnorm, colour="red",args = list(mean = mean(boot_par_mod_q$Pavimentação..asfáltica. ),
                                                      sd = sd(boot_par_mod_q$Pavimentação..asfáltica. )))+
  theme_bw(base_size = 20)




