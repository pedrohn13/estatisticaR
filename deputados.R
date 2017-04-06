library(dplyr, warn.conflicts = F)
library(readr)
library(ggplot2)
library(ggthemes)

deputados_bruto = read_csv("gastos-cota_atividade_parlamentar.csv")

deputados = deputados_bruto %>% select(-txtCNPJCPF, -vlrGlosa,-nuCarteiraParlamentar,
                                       -nuDeputadoId,-codLegislatura,-numParcela,-txtPassageiro
                                       -numEspecificacaoSubCota,-vlrRestituicao,-txtTrecho,
                                       -txtPassageiro,-numSubCota,-txtDescricaoEspecificacao,
                                       -txtNumero,-numEspecificacaoSubCota,-numLote,-indTipoDocumento,
                                       -ideDocumento,-ideCadastro,-nuLegislatura,-numRessarcimento)



deputados = deputados %>% select(nome = txNomeParlamentar,
                                 estado = sgUF,
                                 partido = sgPartido,
                                 descricao = txtDescricao,
                                 fornecedor = txtFornecedor,
                                 valor = vlrDocumento,
                                 valor_liquido = vlrLiquido,
                                 mes = numMes,
                                 ano = numAno,
                                 data = datEmissao)

######################## EMPRESAS MAIS CONTRATADAS

gastos = deputados %>% select(estado,nome,valor,descricao,fornecedor)
gastos_div_pb = gastos %>% filter(estado=="PB", descricao=="DIVULGAÇÃO DA ATIVIDADE PARLAMENTAR.")
gastos_div_pb = gastos_div_pb %>% select(-descricao,-estado)

empresas_pb = gastos_div_pb %>% group_by(fornecedor) %>% tally()
empresas_pb = empresas_pb %>% arrange(-n)

sumario_empresas_pb = empresas_pb %>% summarise(media = mean(n), mediana=median(n), min = min(n), max = max(n))

empresas_pb %>% ggplot(aes(x = n)) + geom_density()

empresas_pb %>% ggplot(aes(x = fornecedor, y=n)) + 
  geom_bar(stat = "identity") + coord_flip()

head(empresas_pb,30) %>% ggplot(aes(x = reorder(fornecedor,n), y=n)) + 
  geom_bar(stat = "identity") + coord_flip()

empresas_pb %>% 
  ggplot(mapping = aes(x ="Quantidade de vezes contratada",y=n)) + 
  geom_boxplot(width = .1)

########################################## LUCRO EM MILHARES DE REAIS

empresas_lucro_pb = gastos_div_pb %>% select(-nome) %>% group_by(fornecedor) %>% summarise(valor = sum(valor)/1000) %>% arrange(-valor)
sumario_empresas_lucro_pb = empresas_lucro_pb %>% summarise(media = mean(valor), mediana=median(valor), min = min(valor), max = max(valor))

empresas_lucro_pb %>% ggplot(aes(x = valor)) + geom_density()

empresas_lucro_pb %>% ggplot(aes(x = fornecedor, y=valor)) + 
  geom_bar(stat = "identity") + coord_flip()

head(empresas_lucro_pb,30) %>% ggplot(aes(x = reorder(fornecedor,valor), y=valor)) + 
  geom_bar(stat = "identity") + coord_flip()

empresas_lucro_pb %>% 
  ggplot(mapping = aes(x ="Lucro da empresa (Milhares de Reais)",y=valor)) + 
  geom_boxplot(width = .1)


#######################################################

gastos_deputado_despesa = gastos %>% group_by(nome,descricao) %>%  summarise(total = sum(valor))

gastos_somados = gastos %>%  group_by(descricao) %>% summarise(total = sum(valor)/1000)

gastos_somados %>% 
  ggplot(mapping = aes(x =descricao,y=total)) + geom_bar(stat = "identity") + 
  labs(x ="Tipos de Despesa",
       y="Valor Total (em milhares)",
       title="Gastos totais dos deputados por tipo de despesa") + coord_flip() +
  scale_fill_excel() +
  theme_excel()


deputados %>% ggplot(aes(x = "valor", y = valor)) + 
  geom_count()

gastos_deputado_despesa %>% 
  ggplot(mapping = aes(x =descricao,y=total)) + 
  geom_boxplot(width = .3)+ coord_flip()

mean(deputados)

sumario = deputados %>% summarise(media = median(valor), min = min(valor),max = max(valor))

bonzinho = deputados %>% filter(valor == min(valor))
gastador = deputados %>% filter(valor == max(valor))
