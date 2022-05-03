#Trabalho apresentado para obtenção do título de especialista em Data Science e Analytics – 2022
#João Paulo Muner Lenat
#Tema TCC: Quantidade de “aces” em partida de tênis considerando superfície de quadras e altitude da cidade

#Esse script R refere-se a análise realizada para a base de dados de torneios de tênis de 2000 a 2016

#Pacotes utilizados
#tidyverse: Pacote guarda-chuva que consolida uma série de ferramentas que fazem parte do ciclo da ciência de dados.
#kableExtra: Visualização da base de dados
#fastDummies: Pacote para dummizar a variavel de superfície da quadra sendo ela uma variável categórica.
#plotly: Geração de graficos interativos
#overdisp: Pacote para verificação de supersipersão
#MASS: Pacote que auxilia na geração do modelo binomial negativo
#jtools: Pacote para auxilio de geração de resultados dos modelos e comparação entre os mesmos
#reshape2: Pacote para auxilio na geração gráfica dos resultados
#cowplot: Pacote para auxilio na geração gráfica dos resultados

#Código para utilização download e instação dos pacotes necessários para análise.
pacotes <- c("tidyverse", "kableExtra","fastDummies", "plotly", "overdisp", "MASS", "jtools", "reshape2", "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Utilização da base final já com as devidas limpezas e incrementos de dados 
atp_matches <- read.csv(file = 'dataset_atp_matches_final.csv')

#Visualização da base de dados inicial
atp_matches %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#"Dummizando" a variavel categórica de superficie do torneio
#Utilizada o padrão do R, para definição da variavél referencia, sendo a que possui maior frequência (no caso quadra dura)
#Esse padrão é definido pelo argumento remove_most_frequent_dummy = T
#Essa variável foi retirada da base "dummizada" (pelo argumento remove_selected_columns = T)
atp_matches_dummies <- dummy_columns(.data = atp_matches,
                                     select_columns = "superficie_torneio",
                                     remove_selected_columns = T,
                                     remove_most_frequent_dummy = T)

#Visualização da base de dados com acrescímo das variáveis "dummies"
atp_matches_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

################################################################################
#                     MODELO DE REGRESSÃO LINEAR MULTÍPLA                      #
################################################################################
#Modelo de regressão linear utilizando a base de dados atp_matches_dummies que possui as variaveis "dummies'
#Variável dependete: quantidade de aces totais de um torneio (total_aces_torneio)
#Variáveis explicativas: Altituda da cidade sede do torneio (altitude_torneio)
#                        Superfície dos quadras do torneios "dummizada" (superficie_torneio_Carpet, superficie_torneio_Clay, superficie_torneio_Grass)
modelo_regressao_linear_atp_matches_dummies <- lm(total_aces_torneio ~ altitude_torneio + superficie_torneio_Carpet + superficie_torneio_Clay + superficie_torneio_Grass, atp_matches_dummies)

#Visualização dos resultados do modelo de regressão linear
#Summary foi utilizado para gerar resultados apresentados no trabalho escrito
summary(modelo_regressao_linear_atp_matches_dummies)

#Variáveis explicativas: altitude_torneio - Não significante para o modelo
#                        superficie_torneio_Carpet - Não significante para o modelo
#                        superficie_torneio_Clay - Significante para o modelo
#                        superficie_torneio_Grass - Significante para o modelo

################################################################################
#              MODELO DE REGRESSÃO PARA DADOS DE CONTAGEM POISSON              #
################################################################################
#Modelo de regressão para dados de contagem POISSON com a base de dados atp_matches_dummies que possui as variaveis "dummies'
#Variável dependente: quantidade de aces totais de um torneio (total_aces_torneio)
#Variáveis explicativas: Altituda da cidade sede do torneio (altitude_torneio)
#                        Superfície dos quadras do torneios "dummizada" (superficie_torneio_Carpet, superficie_torneio_Clay, superficie_torneio_Grass)
modelo_poisson_atp_matches <- glm(formula = total_aces_torneio ~ altitude_torneio + superficie_torneio_Carpet + superficie_torneio_Clay + superficie_torneio_Grass,
                                  data = atp_matches_dummies,
                                  family = "poisson")

#Visualização dos resultados do modelo de regressão para dados de contagem POISSON
#Summary foi utilizado para gerar resultados apresentados no trabalho escrito
summary(modelo_poisson_atp_matches)

#Variáveis explicativas: altitude_torneio - Significante para o modelo
#                        superficie_torneio_Carpet - Significante para o modelo
#                        superficie_torneio_Clay - Significante para o modelo
#                        superficie_torneio_Grass - Significante para o modelo

################################################################################
#                  VALIDAÇÃO DA UTILIZAÇÃO DO MODELO POISSON                   #
################################################################################
#Identificado que o modelo POISSON não teve um resultado bom
#Portanto foi gerado algumas análises para verificação se a utilização do modelo POISSON é correta para esse cenário
#Diagnóstico preliminar para observação de eventual igualdade entre a média e a variância da variável dependente 'quantidade de aces por torneio'
atp_matches_dummies %>%
  summarise(Média = mean(total_aces_torneio),
            Variância = var(total_aces_torneio)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)


#Sabendo que a média é muito diferente da variância, já sabemos que o modelo Poisson não é ideal
#Mesmo assim, vamos realizar outras validações 
#Histograma da variável dependente 'quantidade de aces por torneio'
ggplotly(
  atp_matches %>%
    ggplot(aes(x = total_aces_torneio,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(atp_matches) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de aces por torneio",
         y = "Frequência") +
    theme_bw()
)

#Histograma mostra uma grande dispersão dos dados, pontato é analise a possibiliadde de superdispersão

################################################################################
#                         VALIDAÇÃO DE SUPERDISPERSÃO                          #
################################################################################
#Validação de superdispersão
overdisp(x = atp_matches_dummies,
         dependent.position = 9,
         predictor.position = 8:12)

#Caso o p-value seja maior que 0.05, verifica-se a existência de equidispersão nos dados.
#Caso contrário, diagnostica-se a existência de superdispersão nos dados, fato que favorece a estimação de um modelo binomial negativo.
#Portanto para o caso de estudo, foi necessário realizar o modelo de contagem binomial negativo

################################################################################
#         MODELO DE REGRESSÃO PARA DADOS DE CONTAGEM BINOMIAL NEGATIVO         #
################################################################################
#Modelo de regressão para dados de contagem binomial negativo com a base de dados atp_matches_dummies que possui as variaveis "dummies'
#Variável dependente: quantidade de aces totais de um torneio (total_aces_torneio)
#Variáveis explicativas: Altituda da cidade sede do torneio (altitude_torneio)
#                        Superfície dos quadras do torneios "dummizada" (superficie_torneio_Carpet, superficie_torneio_Clay, superficie_torneio_Grass)
modelo_bneg_atp_matches <- glm.nb(formula = total_aces_torneio ~ altitude_torneio + superficie_torneio_Carpet + superficie_torneio_Clay + superficie_torneio_Grass,
                                  data = atp_matches_dummies)

#Visualização dos resultados do modelo de regressão para dados de contagem binomial negativo
#Summary foi utilizado para gerar resultados apresentados no trabalho escrito
summary(modelo_bneg_atp_matches)

#Variáveis explicativas: altitude_torneio - Não significante para o modelo
#                        superficie_torneio_Carpet - Não significante para o modelo
#                        superficie_torneio_Clay - Significante para o modelo
#                        superficie_torneio_Grass - Significante para o modelo

################################################################################
#                            COMPARAÇÃO DOS MODELOS                            # 
################################################################################
#Comparando os modelos de regressão linear, Poisson e Binomial Negativo em relação ao AIC
export_summs(modelo_regressao_linear_atp_matches_dummies, modelo_poisson_atp_matches,modelo_bneg_atp_matches, scale = F, digits = 4,
             model.names = c("LINEAR", "POISSON", "BNEG"))

#Comparando os modelos de regressão linear, Poisson e Binomial Negativo em relação ao Log Likelihood
my_plot <-
  data.frame(Bneg = logLik(modelo_bneg_atp_matches),
             Poisson = logLik(modelo_poisson_atp_matches),
             Linear = logLik(modelo_regressao_linear_atp_matches_dummies)) %>%
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = round(value, digits = 3)), 
            color = "black", 
            size = 4, 
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "orange", "#FDE725FF")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot
