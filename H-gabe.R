library(readr)
tbl <- read_csv("C:/Users/Коржик/Downloads/eddypro (1).csv",  skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl
class(tbl)
tbl = tbl[-1,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl <- tbl [ ,c(-6, -7, -9, -10, -12, -13, -15, -16, -18, -19, -21, -22, -78:-130)]
tbl 
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
tbl <- tbl[tbl$DOY>244 & tbl$DOY<334, c(1:ncol(tbl))]
tbl
tbl<- tbl[!tbl$daytime>TRUE,]
tbl
 sapply(tbl,is.numeric)
 tbl_numeric = tbl[,sapply(tbl,is.numeric)]
 tbl_numeric
 tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
 tbl_non_numeric
cor_tbl = cor(tbl_numeric)
cor_tbl
cor_tbl = cor(na.omit(tbl_numeric))
cor_tbl
cor_tbl = cor(na.omit(tbl_numeric)) %>% as.data.frame %>% select(co2_flux)
cor_tbl
vars = row.names(cor_tbl)[cor_tbl$co2_flux^2 > .1] %>% na.exclude
vars
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep = ""))
formula                     
mod1 = lm (co2_flux ~ co2_flux+h2o_molar_density+h2o_molar_density+h2o_mixing_ratio+air_molar_volume+Tdew+air_pressure+water_vapor_density+air_density+e+air_heat_capacity+specific_humidity, data=tbl)
summary(mod1)
mod2 = lm (co2_flux ~ (h2o_molar_density+water_vapor_density+e+air_heat_capacity)^2, data=tbl)
summary(mod2)

