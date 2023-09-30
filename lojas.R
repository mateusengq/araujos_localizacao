## Pacotes


library("basedosdados")
library(tidyverse)
library(sf)
library(geobr)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(patchwork)
library(showtext)

font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(theme_bw(base_size = 16, base_family = "Roboto Condensed"))

cor_azul <- '#004684'
cor_vermelha <- '#ED1B24'

## Dados
base <- read.csv2('base_empresas.csv')
head(base)

base_bh <- base %>%
    filter(cidade == 'Belo Horizonte')

base_bairros <- readxl::read_xlsx('bairros_bh.xlsx')
head(base_bairros)

quant_bairros_bh <- 323

## Pergunta1: quantas há em BH?

quant_bh <- nrow(base_bh)
quant_bh

media_por_bairro <- nrow(base_bh)/quant_bairros_bh

media_por_bairro_atuacao <- nrow(base_bh)/nrow(table(base_bh$bairro))

## Pergunta2: quais os bairros com mais Araújos

base_bh %>%
    group_by(bairro) %>%
    summarise(quant = n()) %>%
    mutate(proporcao = quant/sum(quant)*100) %>%
    arrange(desc(quant)) %>%
    top_n(9) %>%
    ggplot(aes(x = reorder(bairro, quant), y = quant, fill = bairro)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(x = 'Bairro', y = 'Quantidade',
         title = 'Quantidade de Araújos por bairro',
         subtitle = 'Contagem dos estabelecimentos com CNPJ básico igual a "17256512"',
         caption = 'Data: BaseDosDados') +
    geom_text(aes(label = paste0(quant)), hjust = 1.6, color = 'white',
                  size = 4.5)+
    scale_fill_manual(values = c(cor_azul, cor_vermelha, rep(cor_azul,3),
                                  cor_vermelha, cor_azul, cor_vermelha, cor_azul)) +
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0,
                                  face = 'bold',
                                  margin = margin(5, 0, 5, 0),
                                  size = 16),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)
    )


## Pergunta2: Geograficamente, como estão localizadas as farmácias?


bairros_bh <- sf::st_read('Bairros_CidadeBH_MG/BAIRRO_OFICIAL/BAIRRO_OFICIAL.shp')
nomes_bairros <- bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Centro', 'Barreiro', 'Venda Nova', 'Buritis', 'Lagoa da Pampulha', 'Campus UFMG'),]
nomes_bairros <- nomes_bairros %>%
    group_by(NOME) %>%
    summarise(X = quantile(st_coordinates(geometry)[,1], 0.5),
              Y = quantile(st_coordinates(geometry)[,2], 1))

base_bh_sf <- st_as_sf(base_bh, wkt = "centroide")
st_crs(base_bh_sf) <- st_crs(4326)

ggplot() + 
    geom_sf(data = bairros_POPULAR_bh, fill = cor_azul, color = 'white') +
    geom_sf(data = bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Centro', 'Barreiro', 'Venda Nova', 'Buritis'),], fill = '#1ba185', color = 'white') +
    geom_sf(data = bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Lagoa da Pampulha'),], fill = '#00BFFF', color = 'white') +
    geom_sf(data = bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Campus UFMG'),], fill = '#708090', color = 'white') +
    ##geom_sf(data = bairros_bh, fill = '#1ba185', alpha = .5, color = 'white') +
    geom_sf(data = base_bh_sf, color = 'white', size = 2. , alpha = 0.7) +
    geom_sf(data = base_bh_sf, color = cor_vermelha, size = 1.5 ) +
    geom_sf(data = base_bh_sf, color = 'white', size = 1. , alpha = 0.3) +
    geom_label(data = nomes_bairros, aes(label = NOME, x = X, y = Y),
               color = "black", size = 5, vjust = 1.25, hjust = 0.5, alpha = .6) +
    labs(title = 'Distribuição Geográfica das Drogarias Araújo em Belo Horizonte',
         subtitle = 'CEP cadastrado na Receita Federal foi utilizado para definir a localização') +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    #margin = margin(10, 5, 10, 5),
                                    size = 16),
          plot.subtitle =  element_text(hjust = 0.5,
                                        margin = margin(5, 0, 5, 0),
                                        size = 12))

ggplot() + 
    geom_sf(data = bairros_POPULAR_bh, fill = cor_azul, color = 'white') +
    geom_sf(data = bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Centro', 'Barreiro', 'Venda Nova', 'Buritis'),], fill = '#1ba185', color = 'white') +
    geom_sf(data = bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Lagoa da Pampulha'),], fill = '#00BFFF', color = 'white') +
    geom_sf(data = bairros_POPULAR_bh[bairros_POPULAR_bh$NOME %in% c('Campus UFMG'),], fill = '#708090', color = 'white') +
    ##geom_sf(data = bairros_bh, fill = '#1ba185', alpha = .5, color = 'white') +
    geom_sf(data = base_bh_sf, color = 'white', size = 2. , alpha = 0.7) +
    geom_sf(data = base_bh_sf, color = cor_vermelha, size = 1.5 ) +
    geom_sf(data = base_bh_sf, color = 'white', size = 1. , alpha = 0.3)  +
    theme_void() +
    labs(title = 'Distribuição Geográfica das Drogarias Araújo em Belo Horizonte',
         subtitle = 'CEP cadastrado na Receita Federal foi utilizado para definir a localização') +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    #margin = margin(10, 5, 10, 5),
                                    size = 16),
          plot.subtitle =  element_text(hjust = 0.5,
                                        margin = margin(5, 0, 5, 0),
                                        size = 12))
