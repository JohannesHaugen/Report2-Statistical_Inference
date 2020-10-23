library(readr)  #Laster inn pakkene med de ulike funksjonene
library(tidyverse)
library(flextable)
library(rstatix)

download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv")  #Laster ned datafilen

hypertrophy <- read_csv("./data/hypertrophy.csv")  #Laster inn datafilen og kobler den
# til objektet hypertrophy

view(hypertrophy) #Viser datafilene

var_interest <- c("SUB_ID", "CLUSTER", "AGE", "T1_BODY_MASS", 
                  "PERCENT_TYPE_II_T1") # Plukker ut hvilke variabler vi er interesserte i å ha med

hyptable <- hypertrophy %>%
  
  select(all_of(var_interest)) %>%
  
  
  # Denne delen spesifiserer hvilke verdier vi vil ha med og komprimerer datasettet.
  # Navnene kommer inn i "variable" og verdier inn i "value".
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:PERCENT_TYPE_II_T1) %>%
  
 group_by(CLUSTER, variable) %>%
  filter(!is.na(CLUSTER)) %>%  #Filtrerer vekk forsøkspersoner som ikke ble regnet som 
  # HIGH eller LOW responders.
  
  summarise (m = mean(value),
             s = sd(value)) %>%  #Regner ut gjennomsnittet og standardavviket.
  
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1),
                    ")", sep = ""), # Denne delen gjør at standardavviket havner i en parantes
         # med en desimal.
         
         CLUSTER = factor(CLUSTER, levels = c("LOW", "HIGH"), #Justerer navnene på variablene
                          labels = c("LOW (n = 10)",
                                     "HIGH (n = 10)")),
         
         variable = factor(variable,
                           levels = c("AGE", # Denne delen bestemmer rekkefølgen i tabellen
                                      "T1_BODY_MASS", 
                                      "PERCENT_TYPE_II_T1"),
                           
                           labels = c("Age (years)",  # Bestemmer navnene på variablene
                                      "Body mass (kg)",
                                      "Type II fiber (%)"))) %>%
  select(-m, -s) %>%   # Selekterer vekk gjennomsnittet og standardavviket
  
  pivot_wider(names_from = CLUSTER,
              values_from = ms) %>% 
  
  arrange(variable) %>%   # Sorterer tabellen med utgangspunkt i variablene
  select(variable, `LOW (n = 10)`, `HIGH (n = 10)`) %>% # Sorterer rekkefølgen på high og low
  
  print() #Printer resultatet av koden over


# Lag tabellen

hyptable %>%
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(variable = "Variabel") %>%
  
  add_header_row(values = "Tabell 1", colwidths = 3) %>% #Angir tittel på tabellen
  
  add_footer_row(values = "Values are mean and (SD)", colwidths = 3) %>% #Angir en fotnote
# med beskrivelse av tabellen.
  
  autofit() #Gjør tabellen penere
  

########################################
# KAOS

hypbxp <- read_csv("./data/hypertrophy.csv") %>%
  filter(!is.na(CLUSTER))

bxp <- ggboxplot(hypbxp, x = "CLUSTER", y = "T3T1_PERCENT_CHANGE_FAST_CSA", 
                 ylab = "Fiber type II change", xlab = "CLUSTER", add = "jitter")

bxp


df <- read_csv("./data/hypertrophy.csv") %>%
  filter(!is.na(CLUSTER)) %>%
  select(CLUSTER, SUB_ID, T3T1_PERCENT_CHANGE_FAST_CSA) %>%
  group_by(CLUSTER) %>%
  get_summary_stats(T3T1_PERCENT_CHANGE_FAST_CSA, type = "mean_sd") %>%
  print()
  
df %>%
  mutate(variable = factor(variable, levels = c("T3T1_PERCENT_CHANGE_FAST_CSA", 
                                                "T3T1_PERCENT_CHANGE_FAST_CSA"),
                           labels = c("Type II fibre percent change",
                                      "Type II fibre precent change"))) %>%
  
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(CLUSTER = "Cluster",
                    variable = "Variabel", 
                    n = "n",
                    mean = "Gjennomsnitt",
                    sd = "SD") %>%
  add_header_row(values = "Summary statistics", colwidths = 5) %>% #Angir tittel på tabellen
  
  
  autofit()




bxp <- ggboxplot(hypertrophy, x = "CLUSTER", y = "T3T1_PERCENT_CHANGE_FAST_CSA", 
                 ylab = "Fiber type II change", xlab = "CLUSTER", add = "jitter")

bxp


##############################
# T-test

# Tester om dataene er normaltfordelt med en shapiro-test.
hyp %>%
  group_by(CLUSTER) %>%
  shapiro_test(T3T1_PERCENT_CHANGE_FAST_CSA) %>%
  print ()

# Plott som viser normalfordelingen
ggqqplot(hyp, x = "T3T1_PERCENT_CHANGE_FAST_CSA", facet.by = "CLUSTER")

# Test variansen i gruppene. Er variansen tilnærnmet lik skal p være mindre enn 0.05
hyp %>% levene_test(T3T1_PERCENT_CHANGE_FAST_CSA ~ CLUSTER)

# Variansen i gruppene er tilnærmet lik i følge levene_testen. Derfor kjøres t-testen med
# var.equal = TRUE.
stat.test <- hyp %>% 
  t_test(T3T1_PERCENT_CHANGE_FAST_CSA ~ CLUSTER, var.equal = TRUE) %>%
  add_significance()
stat.test

# Kalkulering av Effect size
hyp %>%  cohens_d(T3T1_PERCENT_CHANGE_FAST_CSA ~ CLUSTER, var.equal = TRUE)


stat.test <- stat.test %>% add_xy_position(x = "CLUSTER")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))


##### Tabell med resultater fra t-testen

stat.test %>%
  select(group1, group2, n1, n2, p) %>%
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(group1 = "Gruppe 1",
                    group2 = "Gruppe 2",
                    n1 = "Antall i gruppe 1",
                    n2 = "Antall i gruppe 2",
                    p = "P-verdi") %>% 
  
  add_header_row(values = "Resultater fra t-test", colwidths = 5) %>% # Angir tittel på tabellen
  
  set_table_properties( width = 1, layout = "autofit")


