library(dplyr, warn.conflicts = F)
library(readr)
library(ggplot2)
library(ggthemes)

deputados_bruto = read_csv("gastos-cota_atividade_parlamentar.csv")

deputados = deputados_bruto %>% select(-txtCNPJCPF, -vlrGlosa,-nuCarteiraParlamentar,
                                       -nuDeputadoId,-codLegislatura,-numParcela,-txtPassageiro
                                       -numEspecificacaoSubCota,-vlrRestituicao,-txtTrecho,
                                       -numSubCota,-txtDescricaoEspecificacao,
                                       -txtNumero,-numEspecificacaoSubCota,-numLote,-indTipoDocumento,
                                       -ideDocumento,-ideCadastro,-nuLegislatura,-numRessarcimento)



deputados = deputados %>% select(nome = txNomeParlamentar,
                                 estado = sgUF,
                                 partido = sgPartido,
                                 descricao = txtDescricao,
                                 passageiro = txtPassageiro,
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

head(empresas_lucro_pb,30) %>% ggplot(aes(x = reorder(fornecedor,valor), y=valor)) + 
  geom_bar(stat = "identity") + coord_flip()

empresas_lucro_pb %>% 
  ggplot(mapping = aes(x ="Lucro da empresa (Milhares de Reais)",y=valor)) + 
  geom_boxplot(width = .1)


#######################################################


#### PASSAGENS AEREAS

gastos_passagens = deputados %>% select(estado,nome,valor,descricao,passageiro)
gastos_passagens$descricao = ifelse(gastos_passagens$descricao == "Emissão Bilhete Aéreo",
                                    "PASSAGENS AÉREAS", gastos_passagens$descricao)
gastos_passagens = gastos_passagens %>% filter(descricao=="PASSAGENS AÉREAS")
gastos_passagens_outros = gastos_passagens %>% filter(nome != passageiro) %>% select(-descricao)
#### base


gastos_passagens_outros_ocorrencias = gastos_passagens_outros %>% group_by(estado,nome,passageiro) %>% tally() %>% arrange(-n)
gastos_passagens_outros_ocorrencias = gastos_passagens_outros_ocorrencias %>% mutate(passageiroInfo = paste(passageiro, nome, estado,sep = '//'))

gastos_passagens_outros_valor = gastos_passagens_outros %>% group_by(estado,nome,passageiro) %>% summarise(total = sum(valor)/1000) %>% arrange(-total)
gastos_passagens_outros_valor = gastos_passagens_outros_valor %>% mutate(passageiroInfo = paste(passageiro, nome, estado,sep = '//'))

mean(gastos_passagens_outros_ocorrencias$n)
median(gastos_passagens_outros_ocorrencias$n)
min(gastos_passagens_outros_ocorrencias$n)
max(gastos_passagens_outros_ocorrencias$n)

mean(gastos_passagens_outros_valor$total)
median(gastos_passagens_outros_valor$total)
min(gastos_passagens_outros_valor$total)
max(gastos_passagens_outros_valor$total)

gastos_passagens_outros_ocorrencias %>% ggplot(aes(x = n)) + geom_density()
gastos_passagens_outros_valor %>% ggplot(aes(x = total)) + geom_density()

head(gastos_passagens_outros_ocorrencias,30) %>% ggplot(aes(x = reorder(passageiroInfo,n), y=n)) + 
  geom_bar(stat = "identity") + xlab("Passageiro//Deputado//UF") + ylab("Total de Passagens Recebidas") + coord_flip() +
 ggtitle("Quantidade de Passagens recebidas por passageiros que não são deputados - TOP 30")

head(gastos_passagens_outros_valor,30) %>% ggplot(aes(x = reorder(passageiroInfo,total), y=total)) + 
  geom_bar(stat = "identity") + xlab("Passageiro//Deputado//UF") + ylab("Total de gastos com passagens (Milhares de Reais)") + coord_flip() +
  ggtitle("Total de gastos com Passagens de passageiros que não são deputados - TOP 30")


gastos_passagens_outros_ocorrencias %>% 
  ggplot(mapping = aes(x ="Quantidade de passagens recebidas",y=n)) + 
  geom_boxplot(width = .3)

gastos_passagens_outros_valor %>% 
  ggplot(mapping = aes(x ="Gastos com passagens",y=total)) + 
  geom_boxplot(width = .3)

### QUAIS OS DEPUTADO MAIS GASTAM COM E DÃO PASSAGENS PARA OUTRAS PESSOAS?

gastos_passagens_deputados_ocorrencia = gastos_passagens_outros %>% select(-passageiro) %>% group_by(estado,nome) %>% tally()  %>% arrange(-n)
gastos_passagens_deputados_ocorrencia = gastos_passagens_deputados_ocorrencia %>% mutate(depInfo = paste(nome, estado,sep = ' - '))

gastos_passagens_deputados_valor = gastos_passagens_outros %>% select(-passageiro) %>% group_by(estado,nome)  %>% summarise(total = sum(valor)/1000) %>% arrange(-total)
gastos_passagens_deputados_valor = gastos_passagens_deputados_valor %>% mutate(depInfo = paste(nome, estado,sep = ' - '))


mean(gastos_passagens_deputados_ocorrencia$n)
median(gastos_passagens_deputados_ocorrencia$n)
min(gastos_passagens_deputados_ocorrencia$n)
max(gastos_passagens_deputados_ocorrencia$n)

mean(gastos_passagens_deputados_valor$total)
median(gastos_passagens_deputados_valor$total)
min(gastos_passagens_deputados_valor$total)
max(gastos_passagens_deputados_valor$total)

gastos_passagens_deputados_ocorrencia %>% ggplot(aes(x = n)) + geom_density()
gastos_passagens_deputados_valor %>% ggplot(aes(x = total)) + geom_density()

head(gastos_passagens_deputados_ocorrencia,30) %>% ggplot(aes(x = reorder(depInfo,n), y=n)) + 
  geom_bar(stat = "identity") + xlab("Passageiro//Deputado//UF") + ylab("Total de Passagens Recebidas") + coord_flip() +
  ggtitle("Quantidade de Passagens dadas por deputados - TOP 30")

head(gastos_passagens_deputados_valor,30) %>% ggplot(aes(x = reorder(depInfo,total), y=total)) + 
  geom_bar(stat = "identity") + xlab("Passageiro//Deputado//UF") + ylab("Total de gastos com passagens (Milhares de Reais)") + coord_flip() +
  ggtitle("Total de gastos com Passagens de passageiros que não são deputados - TOP 30")


gastos_passagens_deputados_ocorrencia %>% 
  ggplot(mapping = aes(x ="Quantidade de passagens dadas",y=n)) + 
  geom_boxplot(width = .3)

gastos_passagens_deputados_valor %>% 
  ggplot(mapping = aes(x ="Gastos com passagens",y=total)) + 
  geom_boxplot(width = .3)

#######################################






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
