# Inicialização ====
# Loading libraries
load_libraries <- function(){
  if (!require("bootnet"))
    install.packages("bootnet"); library(bootnet)
  if (!require("corrplot"))
    install.packages("corrplot"); library(corrplot)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("export"))
    install.packages("export"); library(export) 
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

# Código para comparar centralidade ====
compareCentrality <- function(net1, net2,
                              include = c("Strength",
                                          "Closeness",
                                          "Betweenness",
                                          "ExpectedInfluence",
                                          "all",
                                          "All"),
                              orderBy = c("Strength",
                                          "Closeness",
                                          "Betweenness",
                                          "ExpectedInfluence"),
                              decreasing = T,
                              legendName = '',
                              net1Name = 'Network 1',
                              net2Name = 'Network 2'){
  
  library(ggplot2)
  library(forcats)
  
  if(include == "All" | include == "all"){
    include = c("Strength",
                "Closeness",
                "Betweenness",
                "ExpectedInfluence")
  }
  df <- centralityTable(net1, net2) %>% filter(measure %in% include)
  
  df %>% 
    mutate(graph = case_when(graph == 'graph 1' ~ net1Name,
                             graph == 'graph 2' ~ net2Name),
           graph = as.factor(graph),
           node = as.factor(node)) %>% 
    
    mutate(node = fct_reorder(node, value)) %>% 
    
    ggplot(aes(x = node, y = value, group = graph)) +
    
    geom_line(aes(linetype = graph), size = 1) +
    
    labs(x = '', y = '') +
    
    scale_linetype_discrete(name = legendName) +
    
    coord_flip() +
    
    facet_grid(~measure) +
    
    theme_bw()
  
}

# Criando Rede 1 - Antes da pandemia ====
grupos <-
  list(
    "Extroversao" = c(1, 6, 11, 16, 21),
    "Amabilidade" = c(2, 7, 12, 17, 22),
    "Conscienciosidade" = c(3, 8, 13, 18, 23),
    "Neuroticismo" = c(4, 9, 14, 19, 24),
    "Abertura" = c(5, 10, 15, 20, 25),
    "Trabalho Significativo" = c(26:35),
    "Engajamento" = c(36:44)
  )

# Arrumando argumentos iniciais
itens_antes <- df_antes %>% select(MarP01_E:UWES09Ab)
nomes_dos_itens <- c('Ex1', 'Am1', 'Co1', 'Ne1', 'Ab1',
                     'Ex2', 'Am2', 'Co2', 'Ne2', 'Ab2',
                     'Ex3', 'Am3', 'Co3', 'Ne3', 'Ab3',
                     'Ex4', 'Am4', 'Co4', 'Ne4', 'Ab4',
                     'Ex5', 'Am5', 'Co5', 'Ne5', 'Ab5',
                     'TS1', 'TS2', 'TS3', 'TS4', 'TS5',
                     'TS6', 'TS7', 'TS8', 'TS9', 'TS10',
                     'Eng1', 'Eng2', 'Eng3', 'Eng4', 'Eng5',
                     'Eng6', 'Eng7', 'Eng8', 'Eng9')

names(itens_antes) <- nomes_dos_itens

# Estimando rede
network_antes <- estimateNetwork(itens_antes,
                                 default = 'EBICglasso')

# Plot da rede                                 
plot(network_antes,
     layout = "spring",
     groups = grupos,
     theme = 'gray',
     labels = colnames(itens_antes),
     #filename = 'figuras\\rede_1',
     #filetype = 'png',
     width = 1.4 * 5,
     height = 5)

graph2ppt(file = 'network_antes')

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
    "Extroversao" = c(1, 6, 11, 16, 21),
    "Amabilidade" = c(2, 7, 12, 17, 22),
    "Conscienciosidade" = c(3, 8, 13, 18, 23),
    "Neuroticismo" = c(4, 9, 14, 19, 24),
    "Abertura" = c(5, 10, 15, 20, 25),
    "Trabalho Significativo" = c(26:35),
    "Engajamento" = c(36:44)
  )

# Arrumando argumentos iniciais
itens_durante <- df_durante %>% select(MarP01_E:UWES09Ab)
nomes_dos_itens <- c('Ex1', 'Am1', 'Co1', 'Ne1', 'Ab1',
                     'Ex2', 'Am2', 'Co2', 'Ne2', 'Ab2',
                     'Ex3', 'Am3', 'Co3', 'Ne3', 'Ab3',
                     'Ex4', 'Am4', 'Co4', 'Ne4', 'Ab4',
                     'Ex5', 'Am5', 'Co5', 'Ne5', 'Ab5',
                     'TS1', 'TS2', 'TS3', 'TS4', 'TS5',
                     'TS6', 'TS7', 'TS8', 'TS9', 'TS10',
                     'Eng1', 'Eng2', 'Eng3', 'Eng4', 'Eng5',
                     'Eng6', 'Eng7', 'Eng8', 'Eng9')

names(itens_durante) <- nomes_dos_itens

# Estimando rede
network_durante <- estimateNetwork(itens_durante,
                                   default = 'EBICglasso')

# Plot da rede
plot(network_durante,
     layout = averageLayout(network_antes), # mesmo layout de network_antes
     groups = grupos,
     theme = 'gray',
     labels = colnames(itens_antes),
     #filename = 'figuras\\rede_2',
     #filetype = 'png',
     width = 1.4 * 5,
     height = 5,
     maximum = 0.658676) # valor obtido com max(abs(network_antes$graph))

graph2ppt(file = 'network_durante')

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

# Comparando centralidade ====
compareCentrality(network_antes,
                  network_durante,
                  include = 'ExpectedInfluence',
                  legendName = 'Grafos',
                  net1Name = 'Antes da pandemia',
                  net2Name = 'Durante a pandemia')

graph2ppt(file = 'influencia_esperada')
