# Inicialização ====
# Loading libraries
load_libraries <- function(){
  if (!require("apaTables"))
    install.packages("apaTables"); library(apaTables)
  if (!require("bootnet"))
    install.packages("bootnet"); library(bootnet)
  if (!require("corrplot"))
    install.packages("corrplot"); library(corrplot)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
  if(!require("lmf"))
    install.packages("lmf"); library(lmf)
  if(!require("magrittr"))
    install.packages("magrittr"); library(magrittr) 
  if(!require("NetworkComparisonTest"))
    install.packages("NetworkComparisonTest"); library(NetworkComparisonTest) 
  if(!require("psych"))
    install.packages("psych"); library(psych)
  if(!require("qgraph"))
    install.packages("qgraph"); library(qgraph)
  if(!require("tidyr"))
    install.packages("tidyr"); library(tidyr)
}
load_libraries()

# Lendo e limpando banco
df <- foreign::read.spss('bancos\\banco.sav',
                         to.data.frame = T,
                         use.value.labels = F)

df %<>% drop_na(MarP01_E:UWES09Ab)

df_antes <- df %>% filter(Antes_Covid == 2)
df_durante <- df %>% filter(Antes_Covid == 1)

# Create a APA correlation table ====
apaTables::apa.cor.table(df, filename = "Correlation_Tables.doc",
                         table.number = 1,
                         show.conf.interval = F,
                         landscape = T)

# Criando Rede 1 - Antes da pandemia ====
grupos <-
  list(
    "Extroversão" = c(1, 6, 11, 16, 21),
    "Amabilidade" = c(2, 7, 12, 17, 22),
    "Conscienciosidade" = c(3, 8, 13, 18, 23),
    "Neuroticismo" = c(4, 9, 14, 19, 24),
    "Abertura" = c(5, 10, 15, 20, 25),
    "Sentido no Trabalho" = c(26:35),
    "Engajamento" = c(36:44)
  )

# Arrumando argumentos iniciais
itens_antes <- df_antes %>% select(MarP01_E:UWES09Ab)
nomes_dos_itens <- c('P1_E', 'P2_S', 'P3_C', 'P4_N', 'P5_A',
                     'P6_E', 'P7_S', 'P8_C', 'P9_N', 'P10_A',
                     'P11_E', 'P12_S', 'P13_C', 'P14_N', 'P15_A',
                     'P16_E', 'P17_S', 'P18_C', 'P19_N', 'P20_A',
                     'P21_E', 'P22_S', 'P23_C', 'P24_N', 'P25_A',
                     'WAMI1', 'WAMI2', 'WAMI3', 'WAMI4', 'WAMI5',
                     'WAMI6', 'WAMI7', 'WAMI8', 'WAMI9', 'WAMI10',
                     'E1_Vi', 'E2_Vi', 'E3_De', 'E4_De', 'E5_Vi',
                     'E6_Ab', 'E7_De', 'E8_Ab', 'E9_Ab')

names(itens_antes) <- nomes_dos_itens

# Estimando rede
network_antes <- estimateNetwork(itens_antes,
                                 default = 'EBICglasso')

# Plot da rede                                 
plot(network_antes,
     layout = "spring",
     groups = grupos,
     theme = 'colorblind',
     labels = colnames(itens_antes),
     filename = 'figuras\\rede_1',
     filetype = 'png',
     width = 1.4 * 5,
     height = 5)

# Investigando acurácia da rede 1 - Pt. 1: BCa for edge weights ====
boot1 <- bootnet(network_antes,
                 type = 'nonparametric', # tipo de bootstrapping
                 nBoots = 2500,
                 nCores = 8)

# Intervalos de confiança para arestas
plot(boot1, labels = F,
     order = "sample")

print(boot1)

# Investigando acurácia da rede 1 - Pt. 2: centrality stability ====
boot2 <- bootnet(network_antes,
                 nBoots = 2500,
                 type = "case",
                 nCores = 8,
                 statistics = c(
                   'strength',
                   'expectedInfluence'))

# Há estabilidade nos índices de estabilidade? (ideal acima de 0.5)
corStability(boot2, statistics = "all", verbose = T)

# Estabilidade dos índices de centralidade (apenas acima de 0.5)
plot(boot2, "all")

centralityPlot(network_antes,
               include = c('Strength', 'ExpectedInfluence'),
               orderBy = 'ExpectedInfluence')

# Criando Rede 2 - Durante a pandemia ====
grupos <-
  list(
    "Extroversão" = c(1, 6, 11, 16, 21),
    "Amabilidade" = c(2, 7, 12, 17, 22),
    "Conscienciosidade" = c(3, 8, 13, 18, 23),
    "Neuroticismo" = c(4, 9, 14, 19, 24),
    "Abertura" = c(5, 10, 15, 20, 25),
    "Sentido no Trabalho" = c(26:35),
    "Engajamento" = c(36:44)
  )

# Arrumando argumentos iniciais
itens_durante <- df_durante %>% select(MarP01_E:UWES09Ab)
nomes_dos_itens <- c('P1_E', 'P2_S', 'P3_C', 'P4_N', 'P5_A',
                     'P6_E', 'P7_S', 'P8_C', 'P9_N', 'P10_A',
                     'P11_E', 'P12_S', 'P13_C', 'P14_N', 'P15_A',
                     'P16_E', 'P17_S', 'P18_C', 'P19_N', 'P20_A',
                     'P21_E', 'P22_S', 'P23_C', 'P24_N', 'P25_A',
                     'WAMI1', 'WAMI2', 'WAMI3', 'WAMI4', 'WAMI5',
                     'WAMI6', 'WAMI7', 'WAMI8', 'WAMI9', 'WAMI10',
                     'E1_Vi', 'E2_Vi', 'E3_De', 'E4_De', 'E5_Vi',
                     'E6_Ab', 'E7_De', 'E8_Ab', 'E9_Ab')

names(itens_durante) <- nomes_dos_itens

# Estimando rede
network_durante <- estimateNetwork(itens_durante,
                                   default = 'EBICglasso')

# Plot da rede
plot(network_durante,
     layout = averageLayout(network_antes), # mesmo layout de network_antes
     groups = grupos,
     theme = 'colorblind',
     labels = colnames(itens_antes),
     #filename = 'figuras\\rede_2',
     #filetype = 'png',
     width = 1.4 * 5,
     height = 5,
     maximum = 0.658676) # valor obtido com max(abs(network_antes$graph))

# Investigando acurácia da rede 2 - Pt. 1: BCa for edge weights ====
boot1 <- bootnet(network_durante,
                 type = 'nonparametric', # tipo de bootstrapping
                 nBoots = 2500,
                 nCores = 8)

# Intervalos de confiança para arestas
plot(boot1, labels = F,
     order = "sample")

print(boot1)

# Investigando acurácia da rede 2 - Pt. 2: centrality stability ====
boot2 <- bootnet(network_durante,
                 nBoots = 2500,
                 type = "case",
                 nCores = 8,
                 statistics = c(
                   'strength',
                   'expectedInfluence'))

# Há estabilidade nos índices de estabilidade? (ideal acima de 0.5)
corStability(boot2, statistics = "all", verbose = T)

# Estabilidade dos índices de centralidade (apenas acima de 0.5)
plot(boot2, "all")

centralityPlot(network_antes,
               include = c('Strength', 'ExpectedInfluence'),
               orderBy = 'ExpectedInfluence')

# Correlacionando matrizes das redes ====
cor(c(network_antes$graph), c(network_durante$graph))

# 0.8395139

# Comparando as redes Pt. 1 - Redes como um todo ====
nct_results <- NCT(network_antes, network_durante, it = 1000)

nct_results

# Plots
plot(nct_results, what = "network")
plot(nct_results, what = "strength")






