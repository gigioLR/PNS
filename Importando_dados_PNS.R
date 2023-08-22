# Instalar pacotes (se nao estiver) -----------------------------------------------

install.packages("PNSIBGE")
install.packages("survey")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("foreign")
install.packages("forcats")
install.packages("tidyverse")
install.packages("data.table")

# Chamar pacotes necessarios ----------------------------------------------

library(PNSIBGE)
library(survey)
library(ggplot2)
library(dplyr)
library(foreign)
library(forcats)
library(tidyverse)
library(data.table)


#apresenta pacote
#help("get_pns")


# Importar microdados -----------------------------------------------------

#vamos baixar os dados brutos da PNS 2019 a partir do pacote PNSIBGE
#fonte: https://rpubs.com/gabriel-assuncao-ibge/pns
#https://github.com/Gabriel-Assuncao/PNSIBGE
#https://cran.r-project.org/web/packages/PNSIBGE/PNSIBGE.pdf

#se quero baixar todas as variaveis
dadosPNS <- get_pns(year=2019, #ano da pesquisa
                    selected = TRUE, #questionario do morador
                    anthropometry = FALSE, #questionario de antropometria
                    labels = FALSE, #categorias
                    design = FALSE) #nao importar como survey


#OBS: se quero baixar somente algumas variaveis (ex: atividade fisica no lazer/tempo livre)
#dadosPNS_teste_bruto <- get_pns(year=2019, vars=c("P034","P035", "P03701", "P03702", "P036"), design=FALSE)

#Selecionando registros validos e calculando peso amostral - summary de verificacao
dadosPNS.1<-dadosPNS %>% filter(V0025A==1) #Selecionado para questionario individual
dadosPNS.1<-dadosPNS.1 %>% mutate(peso_morador_selec=((V00291*(94114/168426190)))) #peso amostral de cada respondente
dadosPNS.1<-dadosPNS.1 %>% filter(!is.na(peso_morador_selec)) #filtra os NA
summary(dadosPNS.1$peso_morador_selec)


#### Definições de abrangências ####

#Sexo
dadosPNS.1 <- dadosPNS.1 %>% rename(Sexo=C006)
dadosPNS.1$Sexo<-factor(dadosPNS.1$Sexo, levels=c(1,2), labels=c("Masculino", "Feminino"))
summary(dadosPNS.1$Sexo)

#Estados - UFs
dadosPNS.1 <- dadosPNS.1 %>% rename(Unidades_da_Federacao=V0001)
dadosPNS.1$Unidades_da_Federacao<-factor(dadosPNS.1$Unidades_da_Federacao, levels=c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                                         label=c("Rondonia","Acre","Amazonas","Roraima","Para","Amapa","Tocantins","Maranhao","Piaui","Ceara",
                                                 "Rio Grande do Norte","Paraiba","Pernambuco","Alagoas","Sergipe","Bahia",
                                                 "Minas Gerais","Espirito Santo","Rio de Janeiro","Sao Paulo",
                                                 "Parana","Santa Catarina","Rio Grande do Sul", 
                                                 "Mato Grosso do Sul","Mato Grosso","Goias","Distrito Federal"))
summary(dadosPNS.1$Unidades_da_Federacao)



#Faixas Etárias
dadosPNS.1 <-  dadosPNS.1 %>% mutate(faixa_idade=cut(C008,
                                                     breaks = c(18,20, 25, 50, 65, Inf),
                                                     labels = c("18 a 19 anos", "20 a 24 anos", "25 a 49 anos", "50 a 64 anos", "65 anos ou mais"), 
                                                     ordered_result = TRUE, right = FALSE))
summary(dadosPNS.1$faixa_idade)

####TRANSFORMANDO EM SURVEY####

dadosPNS.1_survey <-svydesign(id=~UPA_PNS, strat=~V0024, weight=~peso_morador_selec, nest=TRUE, data=dadosPNS.1)
