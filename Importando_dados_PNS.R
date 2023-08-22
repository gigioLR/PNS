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


# Calcular tempo de atividade física semanal ---------------------------------

#tempo livre/lazer
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_lazer=((P035*(P03702+(P03701*60))))) #gerando o tempo de atividade semanal em minutos
dadosPNS.1$tsem_lazer[which(is.na(dadosPNS.1$tsem_lazer))] <- 0 #transforma as respostas vazias de tempo em zero
dadosPNS.1$P036[which(is.na(dadosPNS.1$P036))] <- "00" #insere código de nenhuma atividade

summary(dadosPNS.1$tsem_lazer)

#ponderacao do tempo de atividade por intensidade (atividade vigorosa vale o dobro da moderada)
dadosPNS.1 <- dadosPNS.1 %>%
  mutate(tsem_lazer_pond = case_when(
    P036 == "01"      ~tsem_lazer,
    P036 == "02"      ~tsem_lazer,
    P036 == "03"       ~tsem_lazer*2,
    P036 == "04"       ~tsem_lazer*2,
    P036 == "05"       ~tsem_lazer,
    P036 == "06"       ~tsem_lazer*2,
    P036 == "07"       ~tsem_lazer,
    P036 == "08"       ~tsem_lazer,
    P036 == "09"       ~tsem_lazer,
    P036 == "10"      ~tsem_lazer*2,
    P036 == "11"      ~tsem_lazer*2,
    P036 == "12"      ~tsem_lazer*2,
    P036 == "13"      ~tsem_lazer*2,
    P036 == "14"     ~tsem_lazer,
    P036 == "15"      ~tsem_lazer*2,
    P036 == "16"      ~tsem_lazer,
    P036 == "17"     ~tsem_lazer,
    P036 == "00"       ~0,
    P036 == "99"      ~0
  ))


#trabalho
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_trab=((P03904*(P03906+(P03905*60)))))
dadosPNS.1$tsem_trab[which(is.na(dadosPNS.1$tsem_trab))] <- 0

#deslocamento
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_desloc=((P04001*(P04102+(P04101*60))))+(P042*(P04302+(P04301*60))))
dadosPNS.1$tsem_desloc[which(is.na(dadosPNS.1$tsem_desloc))] <- 0

#total (lazer, trabalho, deslocamento)
dadosPNS.1<-dadosPNS.1 %>% mutate(tsem_total=tsem_lazer_pond+tsem_trab+tsem_desloc)

summary(dadosPNS.1$tsem_total)

# Designar pessoas insuficientemente ativas ---------------------------------
dadosPNS.1 <- dadosPNS.1 %>%
  mutate(ativo = case_when(
    tsem_total < 150      ~0,
    tsem_total >= 150     ~1
    
  ))

dadosPNS.1 <- dadosPNS.1 %>%
  mutate(insuf_ativo = case_when(
    tsem_total < 150 & tsem_total != 0      ~1,
    tsem_total >= 150 | tsem_total == 0    ~0
    
  ))

dadosPNS.1 <- dadosPNS.1 %>%
  mutate(inativo = case_when(
    tsem_total == 0      ~1,
    tsem_total != 0     ~0
    
  ))


summary(dadosPNS.1$ativo)
summary(dadosPNS.1$insuf_ativo)
summary(dadosPNS.1$inativo)

mean(dadosPNS.1$ativo)+mean(dadosPNS.1$insuf_ativo)+mean(dadosPNS.1$inativo)

# Calcular gasto energetico semanal (MET) ---------------------------------


#tempo livre/lazer

#Atribuicao do MET para cada atividade
#Referencia:https://stackoverflow.com/questions/51029322/add-new-columns-and-insert-values-in-columns-based-on-value-in-another-column

dadosPNS.1 <- dadosPNS.1 %>%
  mutate(MET_lazer = case_when(
    P036 == "01"      ~4,
    P036 == "02"      ~4,
    P036 == "14"     ~4,
    P036 == "17"     ~4,
    P036 == "03"       ~8,
    P036 == "04"       ~8,
    P036 == "13"      ~8,
    P036 == "05"       ~6,
    P036 == "09"       ~6,
    P036 == "06"       ~7.3,
    P036 == "15"      ~7.3,
    P036 == "07"       ~5.5,
    P036 == "08"       ~3,
    P036 == "10"      ~10.3,
    P036 == "11"      ~6.8,
    P036 == "12"      ~7.3,
    P036 == "16"      ~5.5,
    P036 == "00"       ~0,
    P036 == 99      ~0
  ))
dadosPNS.1<-dadosPNS.1 %>% mutate(METminsem_lazer=tsem_lazer*MET_lazer)

#teste:
#dadosPNS.1<-dadosPNS.1 %>% mutate(METminsem_lazer_pond=tsem_lazer_pond*4)

summary(dadosPNS.1$METminsem_lazer)
#summary(dadosPNS.1$METminsem_lazer_pond)


#trabalho
dadosPNS.1<-dadosPNS.1 %>% mutate(METminsem_trab=tsem_trab*4)

#deslocamento
dadosPNS.1<-dadosPNS.1 %>% mutate(METminsem_desloc=tsem_desloc*4)

#total (lazer, trabalho, deslocamento)
dadosPNS.1<-dadosPNS.1 %>% mutate(METminsem_total=METminsem_lazer+METminsem_trab+METminsem_desloc)

#lazer + trabalho (para os calculos)
dadosPNS.1<-dadosPNS.1 %>% mutate(METminsem_lazertrab=METminsem_lazer+METminsem_trab)



#Classificando os respondentes por nivel de atividade fisica semanal
dadosPNS.1 <- dadosPNS.1 %>%
  mutate(niveis_AF = case_when(
    METminsem_lazer == 0 ~"Sedentario",
    METminsem_lazer > 0 & METminsem_lazer < 600 ~"Insuficientemente ativo",
    METminsem_lazer >= 600 & METminsem_lazer < 3000 ~"Ativo",
    METminsem_lazer >= 3000 ~"Muito ativo",
  ))


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
