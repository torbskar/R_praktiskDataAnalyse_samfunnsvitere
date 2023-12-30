# Purpose: reads in data and saves subsample to disc

#install.packages("janitor")
invisible(Sys.setlocale(locale='no_NB.utf8'))
library(haven)
library(tidyverse)
library(janitor)


# Leser inn Ungdata 2010-2023, rydder og lagrer utvalg til fil
bane <- "C:/Users/torbskar/OneDrive - Universitetet i Oslo/Dokumenter/Undervisning/PHS/kvantMetode/data/Ungdata 2010-2023"
fil <- paste0(bane, "/", "NSD3157-V1.dta")

ungdata0 <- read_dta(fil) %>% 
  clean_names() 

ungdata <- ungdata0 %>%
  mutate(kommune_nr = str_pad(as.character(kommune), width = 4, pad = "0"),
         fylke_nr = str_pad(as.character(fylke), width = 2, pad = "0")) %>%
  labelled::unlabelled() 

ungdata %>% 
  select(kommune_nr, kommune, fylke_nr, fylke) %>% 
  head()


#glimpse(ungdata[,1:40])

#detach("package:ggforce", unload = TRUE)

## Data for alkoholbruk ####
ungdata_alko <- ungdata %>% 
  filter(!is.na(klasse), !is.na(alko1), !is.na(kjonn) ) %>% 
  select(klasse,  ar, kjonn, alko1) %>% 
  mutate(drikker = as.numeric(alko1 != "Aldri"))


saveRDS(ungdata_alko, "data/ungdata_alko.rds")

## Data for alkoholbruk og geografi ####
ungdata_geo <- ungdata %>% 
  filter( ar == 2020) %>% 
  filter(!is.na(klasse), !is.na(alko1), !is.na(kjonn) ) %>% 
  mutate(drikker = as.numeric(alko1 != "Aldri")) %>% 
  select(kommune_nr, kommune, fylke_nr, fylke, drikker, klasse, kjonn) %>% 
  group_by(kommune, kjonn) %>% 
  summarise(n = n(), 
            drikker = sum(drikker == 1, na.rm = T)/n()) %>% 
  ungroup() 

head(ungdata_geo)


## Data for kontinuerlige variable (indekser) ####

# Ungdata med kjønn, klasse, og indeks for livskvalitet og atferdsproblemer
ungdata_kont <- ungdata %>% 
  select(kjonn, ar, klasse, 
         starts_with("samftil"),                                         # samfunnstillitt
         skole1:skole6,                                                  # skoletrivsel
         livskval1:livskval7,                                            # livskvalitet 
         paste0("atfpro", c(1, 25, 12, 15, 16, 18, 29, 30, 31, 32))) %>% # utvalgte av atfpro 

  mutate(across(starts_with("samftil"), ~na_if(.x, "Vet ikke"))) %>%     # erstatter "Vet ikke" med NA
  
  mutate(across(starts_with("samftil"), ~as.numeric(fct_rev(.x)))) %>%
  mutate(across(   skole1:skole6,    ~as.numeric(fct_rev(.x)))) %>%
  mutate(across(livskval1:livskval7, ~as.numeric(fct_rev(.x)))) %>%
  mutate(across((starts_with("atfpro")), ~as.numeric(fct_rev(.x)))) %>%
  
  mutate( samfunnstillit = select(., starts_with("samftil")) |> rowMeans(na.rm = T)) %>% 
  mutate( skoletrivsel = select(., starts_with("skole")) |> rowMeans(na.rm = T)) %>% 
  mutate( livskval = select(., starts_with("livskval")) |> rowMeans(na.rm = T)) %>% 
  mutate( atfpro   = select(., starts_with("atfpro")) |> rowMeans(na.rm = T)) %>% 
  select(kjonn, ar, klasse, livskval, atfpro, skoletrivsel, samfunnstillit) %>% 
  filter(complete.cases(.)) 

glimpse(ungdata_kont)

saveRDS(ungdata_kont, "data/ungdata_kont.rds")



ungdata_kont %>% 
  filter(complete.cases(.)) %>%
  select(livskval, atfpro, skoletrivsel, samfunnstillit) %>%
  cor(use = "pairwise.complete.obs")


ungdata_kont %>% 
  select(livskval, atfpro, skoletrivsel, samfunnstillit) %>%
  cor(use = "pairwise.complete.obs")


ggplot(ungdata_kont, aes(x = livskval, y = samfunnstillit))+
  geom_jitter(alpha = .3)+
  geom_smooth(method = "lm", se = F)

ggplot(ungdata_kont, aes(x = livskval))+
  geom_histogram(binwidth = .15) 

ggplot(ungdata_alko, aes(x = alko1)) +
  geom_bar() +
  labs(x = "", y = "Antall", fill = "Drikker") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1)) +
  labs(title = "Antall som drikker alkohol", caption = "Kilde: Ungdata 2010-2020") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
  


levels(ungdata$alko1)

# Bruker Ungdata 2010: alkoholbruk etter klassetrinn over tid
alko1 <- ungdata_alko %>% 
  group_by(klasse, ar, kjonn) %>%
  summarise(n = n(), 
            drikker = sum(drikker == "ja", na.rm = T)/n()) %>%
  ungroup()

alko1 %>% 
  arrange((n))

ggplot(alko1, aes(x = ar, y = drikker, group = klasse, color = klasse)) +
  geom_line() +
  geom_point() +
  ylim(0, NA)+
  labs(x = "År", y = "Andel som drikker", color = "Klasse") +
  theme_bw() +
  theme(legend.position = "bottom")



# ESS-data #### 
ess0 <- read_dta("data/ESS10.dta") %>% 
  clean_names() 

glimpse(ess0)

# ESS-data ####
# basert på https://link.springer.com/article/10.1007/s11205-019-02212-x
ess <- ess0 %>%
  select(cntry, agea, eduyrs, gndr, wkhtot,
         happy, stflife,
         ppltrst, pplfair, pplhlp,
         trstprl, trstlgl, trstplt, trstep, trstun, trstplc
         ) %>% 
  filter(cntry == "NO") %>% 
  zap_missing() %>% 
  filter(complete.cases(.)) %>%
  mutate(happy = as.numeric(happy), 
         stflife = as.numeric(stflife),
         overallhappy = (happy + stflife)/2) %>% 
  mutate(across(starts_with("trst"), ~as.numeric(.x))) %>%
  mutate(trust_pol = select(., starts_with("trst")) |> rowMeans(na.rm = T)) %>%
  mutate(across(starts_with("ppl"), ~as.numeric(.x))) %>%
  mutate(trust_soc = select(., starts_with("ppl")) |> rowMeans(na.rm = T)) %>%
  labelled::unlabelled() %>% 
  select(-starts_with("trst"), -starts_with("ppl"))
  
summary(ess)

saveRDS(ess, "data/ess_trust.rds")  

ggplot(ess, aes(x = trust_soc, y = overallhappy))+
  geom_jitter(alpha = .3)+
  geom_smooth(method = "lm", se = F)


ggplot(ess, aes(x = trust_soc, y = overallhappy)) +
  geom_density2d_filled()+
  theme_minimal()
