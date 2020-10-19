library(readr)  #Laster inn pakkene med de ulike funksjonene
library(tidyverse)
library(flextable)

download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv")  #Laster ned datafilen

hypertrophy <- read_csv("./data/hypertrophy.csv")  #Laster inn datafilen og kobler den
# til objektet hypertrophy

view(hypertrophy) #Viser datafilene

var_interest <- c("SUB_ID", "CLUSTER", "AGE", "T1_BODY_MASS", 
                  "Squat_3RM_kg") # Plukker ut hvilke variabler vi er interesserte i å ha med

hyp <- hypertrophy %>%
  
  select(all_of(var_interest)) %>%
  
  
  # Denne delen spesifiserer hvilke verdier vi vil ha med og komprimerer datasettet.
  # Navnene kommer inn i "variable" og verdier inn i "value".
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:Squat_3RM_kg) %>%
  
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
                                      "Squat_3RM_kg"),
                           
                           labels = c("Age (years)",  # Bestemmer navnene på variablene
                                      "Body mass (kg)",
                                      "Squat 3RM (kg)"))) %>%
  select(-m, -s) %>%   # Selekterer vekk gjennomsnittet og standardavviket
  
  pivot_wider(names_from = CLUSTER,
              values_from = ms) %>% 
  
  arrange(variable) %>%   # Sorterer tabellen med utgangspunkt i variablene
  select(variable, `LOW (n = 10)`, `HIGH (n = 10)`) %>% # Sorterer rekkefølgen på high og low
  
  print() #Printer resultatet av koden over


hyp %>%
  flextable() %>% #Lag tabell med Flextable
  
  add_header_row(values = "Table 1", colwidths = 3) %>% #Angir tittel på tabellen
  
  add_footer_row(values = "Values are mean and (SD)", colwidths = 3) %>% #Angir en fotnote
# med beskrivelse av tabellen.
  
  autofit() #Gjør tabellen penere
  
