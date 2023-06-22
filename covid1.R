# Data from: << https://www.saopaulo.sp.gov.br/planosp/simi/dados-abertos/ >>

# Necessary libraries 
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)

# It is important change the path when you import the data
covid <- read.csv("~/covid_school/20210412_simi.csv", header=TRUE, sep=";")

# Filtering Sars-Cov-2 cases by age (Adults) 
covid2 <- covid %>% 
  filter(Idade >= 19 & Idade <= 59) %>% 
  filter(Municipio == "SÃO PAULO") %>% 
  rename(periodo = "Data.Inicio.Sintomas") %>% 
  select(Genero, Municipio, periodo, Idade, Obito) %>% 
  filter(Genero != "INDEFINIDO")

# Excluding missing cases
covid2 <- covid2[!(!is.na(covid2$periodo) & covid2$periodo==""), ]

# Formating the variable dmy as calendar
covid2 <- covid2 %>% 
  mutate(periodo = lubridate::dmy(periodo))

covid2 <- covid2[order(covid2$periodo),]

# Filtering Sars-Cov-2 cases by age (Children & Teenager) 
covid1 <- covid %>% 
  filter(Idade >= 0 & Idade <= 18) %>% 
  filter(Municipio == "SÃO PAULO") %>% 
  rename(periodo = "Data.Inicio.Sintomas") %>% 
  select(Genero, Municipio, periodo, Idade, Obito) %>% 
  filter(Genero != "INDEFINIDO")

covid1 <- covid1[!(!is.na(covid1$periodo) & covid1$periodo==""), ]

covid1 <- covid1 %>% 
  mutate(periodo = lubridate::dmy(periodo))

covid1 <- covid1[order(covid1$periodo),]

# Filtering children hospital admissions (covid1) by gender + period 
covid1 %>% filter(Genero != "IGNORADO") %>% 
  ggplot() +
  geom_bar(aes(x = periodo, fill=Genero)) +
  labs(title = " ", 
       x = "Período", 
       y = "Internações (n)", 
       fill = "Gênero") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.1, size = 13),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16)) +
  facet_grid(cols=vars(Genero))

# Filtering children hospital admissions (covid1) by age <= 5
infantil <- covid1 %>%
  filter(Genero != "IGNORADO") %>% 
  filter(Idade >= 0 & Idade <= 5) %>% 
  arrange(periodo) %>% 
  subset(periodo > "2021-02-01" & periodo < "2021-03-30")

infantil %>% ggplot() +
  geom_bar(aes(x = periodo, fill=Genero)) +
  labs(title = " ", 
       x = "Período", 
       y = "Internações (n)", 
       fill = "Gênero") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ylim(0, 70) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.1, size = 13),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16)) +
  facet_grid(cols=vars(Genero))

# Filtering children hospital admissions (covid1) by 6 <= age <= 10 
fund1 <- covid1 %>% 
  filter(Idade >= 6 & Idade <= 10) %>% 
  arrange(periodo) %>% 
  subset(periodo > "2021-02-01" & periodo < "2021-03-30")

fund1 %>% ggplot() +
  geom_bar(aes(x = periodo, fill=Genero)) +
  labs(title = " ", 
       x = "Período", 
       y = "Internações (n)", 
       fill = "Gênero") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ylim(0, 70) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.1, size = 13),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16)) +
  facet_grid(cols=vars(Genero))

# Filtering children hospital admissions (covid1) by 11 <= age <= 15 
fund2 <- covid1 %>% 
  filter(Idade >= 11 & Idade <= 15) %>% 
  arrange(periodo) %>% 
  subset(periodo > "2021-02-01" & periodo < "2021-03-30")

fund2 %>% ggplot() +
  geom_bar(aes(x = periodo, fill=Genero)) +
  labs(title = " ", 
       x = "Período", 
       y = "Internações (n)", 
       fill = "Gênero") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ylim(0, 70) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.1, size = 13),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16)) +
  facet_grid(cols=vars(Genero))

# Filtering children hospital admissions (covid1) by age >= 16 
medio <- covid1 %>% 
  filter(Idade >= 16) %>% 
  arrange(periodo) %>% 
  subset(periodo > "2021-02-01" & periodo < "2021-03-30")

medio %>% ggplot() +
  geom_bar(aes(x = periodo, fill=Genero)) +
  labs(title = " ", 
       x = "Período", 
       y = "Internações (n)", 
       fill = "Gênero") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ylim(0, 70) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.1, size = 13),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16)) +
  facet_grid(cols=vars(Genero))

# Filtering children deaths by 0 <= age <= 5 
obitos_inf <- covid1 %>%
  filter(Idade >= 0 & Idade <= 5) %>%
  summarise(sum(Obito))
# 20

# Filtering children deaths by 6 <= age <= 15 
obitos_fund <- covid1 %>%
  filter(Idade >= 6 & Idade <= 15) %>%
  summarise(sum(Obito))
# 19

# Filtering children deaths byage >= 16 
obitos_medio <- covid1 %>%
  filter(Idade >= 16) %>%
  summarise(sum(Obito))
# 14
# total 53

# Filtering adult deaths by age 
obitos_adult <- covid2 %>%
  subset(periodo >= "2021-02-01" & periodo <= "2021-03-30") %>% 
  summarise(sum(Obito))
# 1483

# Hospital admissions taking into account socio-territorial quality among teachers and education workers
# The IDH came from: << https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/Informes_Urbanos/29_Dimensoes_IDH-M.pdf >>

idh_covid <- read_excel("~/covid_school/Layers_idh_covid_escolas2.xlsx")

# Selecting attributes. Especially local Index of Human Development
idh_covid1 <- idh_covid %>% 
  select(idh_idh, eq_classe, 
         eq_distr, eq_subpref, eq_dre, 
         eq_diretor, Apoio, Professore, 
         Total_cont, Mortes) %>% 
  rename(idh = "idh_idh", 
         tipo = "eq_classe", 
         regiao = "eq_distr", 
         subpref = "eq_subpref", 
         dre = "eq_dre", 
         diretoria = "eq_diretor", 
         contagios_total = "Total_cont")

# Deaths and hospital admissions by school, role and by IDH
escolas <- idh_covid1 %>% 
  filter(!is.na(idh_covid1$tipo))

mortes <- idh_covid1 %>% 
  mutate(Apoio = as.numeric(Apoio),
         Professores = as.numeric(Professore),
         contagios_total = as.numeric(contagios_total),
         Mortes = as.numeric(Mortes)) %>% 
  filter(!is.na(idh_covid1$contagios_total)) %>% 
  group_by(subpref)

mortes_graph <- mortes %>%
  summarize(total = sum(contagios_total))

mortes_graph1 <- mortes %>%
  summarize(idh = mean(idh))

mortes_graph1 <- mortes_graph1 %>% 
  left_join(mortes_graph) %>% 
  mutate(idh = ifelse(subpref == "PINHEIROS","0.942",idh),
         idh = ifelse(subpref == "VILA MARIANA","0.938",idh),
         idh = ifelse(subpref == "SANTO AMARO","0.909",idh),
         idh = ifelse(subpref == "LAPA","0.906",idh),
         idh = ifelse(subpref == "SE","0.889",idh),
         idh = ifelse(subpref == "MOOCA","0.869",idh),
         idh = ifelse(subpref == "SANTANA-TUCURUVI","0.869",idh),
         idh = ifelse(subpref == "BUTANTA","0.859",idh),
         idh = ifelse(subpref == "IPIRANGA","0.824",idh),
         idh = ifelse(subpref == "ARICANDUVA-FORMOSA-CARRAO","0.822",idh),
         idh = ifelse(subpref == "JABAQUARA","0.816",idh),
         idh = ifelse(subpref == "PENHA","0.804",idh),
         idh = ifelse(subpref == "CASA VERDE-CACHOEIRINHA","0.799",idh),
         idh = ifelse(subpref == "VILA MARIA-VILA GUILHERME","0.793",idh),
         idh = ifelse(subpref == "PIRITUBA-JARAGUA","0.787",idh),
         idh = ifelse(subpref == "VILA PRUDENTE","0.785",idh),
         idh = ifelse(subpref == "CAMPO LIMPO","0.783",idh),
         idh = ifelse(subpref == "ERMELINO MATARAZZO","0.777",idh),
         idh = ifelse(subpref == "JACANA-TREMEMBE","0.768",idh),
         idh = ifelse(subpref == "FREGUESIA-BRASILANDIA","0.762",idh),
         idh = ifelse(subpref == "ITAQUERA","0.758",idh),
         idh = ifelse(subpref == "CIDADE ADEMAR","0.758",idh),
         idh = ifelse(subpref == "CAPELA DO SOCORRO","0.750",idh),
         idh = ifelse(subpref == "SAO MIGUEL","0.736",idh),
         idh = ifelse(subpref == "SAO MATEUS","0.732",idh),
         idh = ifelse(subpref == "PERUS","0.731",idh),
         idh = ifelse(subpref == "ITAIM PAULISTA","0.725",idh),
         idh = ifelse(subpref == "M'BOI MIRIM","0.716",idh),
         idh = ifelse(subpref == "GUAIANASES","0.713",idh),
         idh = ifelse(subpref == "CIDADE TIRADENTES","0.708",idh),
         idh = ifelse(subpref == "PARELHEIROS","0.68",idh),
         idh = ifelse(subpref == "SAPOPEMBA","0.726",idh)) %>% 
  mutate(idh = as.numeric(idh),
         idh = round(idh, digits = 2))

mortes_graph1 %>%
  ggplot() +
  geom_col(aes(x = subpref, y=total, fill = idh)) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  labs(title = " ",
       x = "Subprefeitura", 
       y = "Número de infectados",
       fill = "IDH") +
  ylim(0, 15000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.1, size = 13),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16))