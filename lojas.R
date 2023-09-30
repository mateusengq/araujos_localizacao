## Pacotes

library("basedosdados")
library(tidyverse)
library(sf)
library(geobr)

## Dados
base <- read.csv('base_empresas.csv')
head(base)

base_bh <- base %>%
    filter(cidade == 'Belo Horizonte')

## Pergunta1: quantas há em BH?

quant_bh <- nrow(base_bh)
quant_bh

## Pergunta2: quais os bairros com mais Araújos

base_bh %>%
    group_by(bairro) %>%
    summarise(quant = n()) %>%
    mutate(proporcao = quant/sum(quant)*100) %>%
    arrange(desc(quant)) %>%
    top_n(9) %>%
    ggplot(aes(x = reorder(bairro, quant), y = quant)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(x = 'Bairro', y = 'Quantidade')



base_bh %>%
    group_by(bairro) %>%
    summarise(quant = n()) %>%
    mutate(proporcao = quant/sum(quant)*100) %>%
    arrange(desc(quant)) %>% View()
