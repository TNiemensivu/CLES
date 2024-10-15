library(tidyverse)
library(readxl)
library(writexl)
setwd("~/roi/koku_s24")

#### 3-luokka ####

excel_sheets("FUNA_Koulukunnossa_s24_matematiikka_3lk.xlsx")
fi_3lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_3lk.xlsx", sheet=15)

fi_3lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_3lk.xlsx", sheet=17)

hist(fi_3lk_matriisit$Time, breaks=100)
hist(fi_3lk_vaaka$Time[fi_3lk_vaaka$Time<20000], breaks=100)


excel_sheets("FUNA_Koulukunnossa_s24_Matematiikka_3lk_ruotsi.xlsx")
sv_3lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_3lk_ruotsi.xlsx", sheet=15)

sv_3lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_3lk_ruotsi.xlsx", sheet=17)

hist(sv_3lk_matriisit$Time, breaks=100)
hist(sv_3lk_vaaka$Time, breaks=100)

koku_matriisit_3lk <- rbind(fi_3lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
  pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
  mutate(lang = "FI"),
  sv_3lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
    pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
    mutate(lang = "SV")) %>% mutate(grade = "3lk")

koku_vaaka_3lk <- rbind(fi_3lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                              pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                              mutate(lang = "FI"),
                            sv_3lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                              pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                              mutate(lang = "SV")) %>% mutate(grade = "3lk")





#### 4-luokka ####

excel_sheets("FUNA_Koulukunnossa_s24_matematiikka_4lk.xlsx")
fi_4lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_4lk.xlsx", sheet=17) %>%
  filter(Time>350)

fi_4lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_4lk.xlsx", sheet=19) %>%
  filter(Time>350)

hist(fi_4lk_matriisit$Time, breaks=100)
hist(fi_4lk_vaaka$Time, breaks=100)


excel_sheets("FUNA_Koulukunnossa_s24_Matematiikka_4lk_ruotsi.xlsx")
sv_4lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_4lk_ruotsi.xlsx", sheet=17)%>%
  filter(Time>350)

sv_4lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_4lk_ruotsi.xlsx", sheet=19)%>%
  filter(Time>350)

hist(sv_4lk_matriisit$Time, breaks=100)
hist(sv_4lk_vaaka$Time, breaks=100)

koku_matriisit_4lk <- rbind(fi_4lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
                              pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                              mutate(lang = "FI"),
                            sv_4lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
                              pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                              mutate(lang = "SV")) %>% mutate(grade = "4lk")

koku_vaaka_4lk <- rbind(fi_4lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                          pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                          mutate(lang = "FI"),
                        sv_4lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                          pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                          mutate(lang = "SV")) %>% mutate(grade = "4lk")


koku_matriisit_34lk <- rbind(koku_matriisit_3lk, koku_matriisit_4lk)
koku_vaaka_34lk <- rbind(koku_vaaka_3lk, koku_vaaka_4lk)



#### 5-6-luokka ####

excel_sheets("FUNA_Koulukunnossa_s24_matematiikka_5-6lk.xlsx")
fi_56lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_5-6lk.xlsx", sheet=17) %>%
  filter(Time>350)

fi_56lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_5-6lk.xlsx", sheet=19) %>%
  filter(Time>350)

hist(fi_56lk_matriisit$Time, breaks=100)
hist(fi_56lk_vaaka$Time, breaks=100)


excel_sheets("FUNA_Koulukunnossa_s24_Matematiikka_5-6_lk_ruotsi.xlsx")
sv_56lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_5-6_lk_ruotsi.xlsx", sheet=17)%>%
  filter(Time>350)

sv_56lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_5-6_lk_ruotsi.xlsx", sheet=19)%>%
  filter(Time>350)

hist(sv_56lk_matriisit$Time, breaks=100)
hist(sv_56lk_vaaka$Time, breaks=100)

koku_matriisit_56lk <- rbind(fi_56lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
                              pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                              mutate(lang = "FI"),
                            sv_56lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
                              pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                              mutate(lang = "SV")) %>% mutate(grade = "5-6lk")

koku_vaaka_56lk <- rbind(fi_56lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                          pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                          mutate(lang = "FI"),
                        sv_56lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                          pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                          mutate(lang = "SV")) %>% mutate(grade = "5-6lk")



#### 7-9-luokka ####

excel_sheets("FUNA_Koulukunnossa_s24_matematiikka_7-9lk.xlsx")
fi_789lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_7-9lk.xlsx", sheet=17) %>%
  filter(Time>350)

fi_789lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_matematiikka_7-9lk.xlsx", sheet=19) %>%
  filter(Time>350)

hist(fi_789lk_matriisit$Time, breaks=100)
hist(fi_789lk_vaaka$Time, breaks=100)


excel_sheets("FUNA_Koulukunnossa_s24_Matematiikka_7-9lk_ruotsi.xlsx")
sv_789lk_matriisit <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_7-9lk_ruotsi.xlsx", sheet=17)%>%
  filter(Time>350)

sv_789lk_vaaka <- read_excel("FUNA_Koulukunnossa_s24_Matematiikka_7-9lk_ruotsi.xlsx", sheet=19)%>%
  filter(Time>350)

hist(sv_789lk_matriisit$Time, breaks=100)
hist(sv_789lk_vaaka$Time, breaks=100)

koku_matriisit_789lk <- rbind(fi_789lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
                               pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                               mutate(lang = "FI"),
                             sv_789lk_matriisit %>% select(IDCode, PreOrd, AnsC) %>%
                               pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                               mutate(lang = "SV")) %>% mutate(grade = "7-9lk")

koku_vaaka_789lk <- rbind(fi_789lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                           pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA) %>%
                           mutate(lang = "FI"),
                         sv_789lk_vaaka %>% select(IDCode, PreOrd, AnsC) %>%
                           pivot_wider(names_from = "PreOrd", values_from = "AnsC", values_fill = NA)%>%
                           mutate(lang = "SV")) %>% mutate(grade = "7-9lk")



write_xlsx(list(`Matriisit 3-4` = koku_matriisit_34lk, `Vaaka 3-4` = koku_vaaka_34lk,
                `Matriisit 5-6` = koku_matriisit_56lk, `Vaaka 5-6` = koku_vaaka_56lk,
                `Matriisit 7-9` = koku_matriisit_789lk, `Vaaka 7-9` = koku_vaaka_789lk),
           "FUNA_KOKU_päättelyt_s24.xlsx")



#### Raakadatan kanssa pelailua ####

matriisit_34 <- reduce(list(fi_3lk_matriisit, fi_4lk_matriisit,
                            sv_3lk_matriisit, sv_4lk_matriisit), rbind)

library(ggplot2)

ggplot(matriisit_34, aes(x=Time))+
  geom_histogram(bins=50)+
  xlim(c(0,20000))+
  facet_wrap(vars(PreOrd))+
  theme_minimal()+
  ggtitle("Matriisipäättely 3-4. lk")



matriisit_56 <- reduce(list(fi_56lk_matriisit, sv_56lk_matriisit), rbind)


ggplot(matriisit_56, aes(x=Time))+
  geom_histogram(bins=50)+
  xlim(c(0,20000))+
  facet_wrap(vars(PreOrd))+
  theme_minimal()+
  ggtitle("Matriisipäättely 5-6. lk")

matriisit_789 <- reduce(list(fi_789lk_matriisit, sv_789lk_matriisit), rbind)


ggplot(matriisit_789, aes(x=Time))+
  geom_histogram(bins=50)+
  xlim(c(0,20000))+
  facet_wrap(vars(PreOrd))+
  theme_minimal()+
  ggtitle("Matriisipäättely 7-9. lk")





vaaka_34<- reduce(list(fi_3lk_vaaka, fi_4lk_vaaka,
                            sv_3lk_vaaka, sv_4lk_vaaka), rbind)

ggplot(vaaka_34, aes(x=Time))+
  geom_histogram(bins=50)+
  xlim(c(0,20000))+
  facet_wrap(vars(PreOrd))+
  theme_minimal()+
  ggtitle("Vaakapäättely 3-4. lk")



vaaka_56 <- reduce(list(fi_56lk_vaaka, sv_56lk_vaaka), rbind)


ggplot(vaaka_56, aes(x=Time))+
  geom_histogram(bins=50)+
  xlim(c(0,20000))+
  facet_wrap(vars(PreOrd))+
  theme_minimal()+
  ggtitle("Vaakapäättely 5-6. lk")

vaaka_789 <- reduce(list(fi_789lk_vaaka, sv_789lk_vaaka), rbind)


ggplot(vaaka_789, aes(x=Time))+
  geom_histogram(bins=50)+
  xlim(c(0,20000))+
  facet_wrap(vars(PreOrd))+
  theme_minimal()+
  ggtitle("Vaakapäättely 7-9. lk")
