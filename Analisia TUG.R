

# Kargartu beharreko paketeak

library(nortest)  # Normaltasun testa burutzeko
library(mgcv)   # GAM ereduak doitzeko
library(CatPredi)  # Mozketa puntu optimoak eskuratzeko metodoa erabiltzeko
library(ggplot2)  # Grafikoak irudikatzeko


# Datu-basearen izena: DataLAC14
# Eantzun aldagaiaren izena: Evento

# Datu-basearen irakurketa

View(DataLAC14)
head(DataLAC14)
summary(DataLAC14) 


# Evento (gertaera) aldagaian NA bezala adierazi Baja_1 edo Baja_2 izan dutenak

DataLAC14$Evento[DataLAC14$Baja_1 == 1] <- NA
DataLAC14$Evento[DataLAC14$Baja_2 == 1] <- NA

# Baja eman duten indibiduoak kendu

DataLAC14 <- subset(DataLAC14, !is.na(DataLAC14$Evento))


# Evento (gertaera) numeric bezela jarri

DataLAC14$Evento <- as.numeric(as.character(DataLAC14$Evento))



# Aldagaien deskribapena sexuaren arabera


# Aldagai JARRAITUA denean batezbestekoa eta desbiderapen estandarraren bitartez
# Adibidez, adina aldagaiarentzako:

# Batezbestekoak
mean(DataLAC14$Edad) # Orokorra
mean(DataLAC14$Edad[DataLAC14$Sexo == 'Mujer']) # Emakumezkoena
mean(DataLAC14$Edad[DataLAC14$Sexo == 'Hombre']) # Gizonezkoena

# Desbiderapen estandarrak
sd(DataLAC14$Edad) # Orokorra
sd(DataLAC14$Edad[DataLAC14$Sexo == 'Mujer']) # Emakumezkoena
sd(DataLAC14$Edad[DataLAC14$Sexo == 'Hombre']) # Gizonezkoena



# Aldagai KATEGORIKOA denean maiztasunen bitartez
# Adibidez, ikusmen arazoa (Sgeria_p1) aldagaiarentzako: (ikusmen arazoa BAI=1/ EZ=2 )

sum(DataLAC14$Sgeria_P1== 1, na.rm=TRUE) # Guztira ikusmen arazoekin
sum(DataLAC14$Sgeria_P1[DataLAC14$Sexo == 'Mujer']== 1,na.rm = TRUE) # Emakumeak ikusmen arazoekin
sum(DataLAC14$Sgeria_P1[DataLAC14$Sexo == 'Hombre']== 1,na.rm=TRUE) # Gizonezkoak ikusmen arazoekin                                                     # Gizonezkoak ikusmen arazoekin=42 %11.57


sum(DataLAC14$Sgeria_P1== 2, na.rm=TRUE) # Guztira ikusmen arazorik gabe
sum(DataLAC14$Sgeria_P1[DataLAC14$Sexo == 'Mujer']== 2,na.rm=TRUE) # Emakumeak ikusmen arazorik gabe
sum(DataLAC14$Sgeria_P1[DataLAC14$Sexo == 'Hombre']== 2,na.rm=TRUE) # Gizonezkoek ikusmen arazorik gabe                                                     # Gizonezkoak ikusmen arazorik gabe=321 %88.43


# Emakumezkoen eta gizonezkoen arteko ezberdintasunak ebaluatzeko

# Aldagai KATEGORIKOA denean:

taula1<-table(DataLAC14$Sexo, DataLAC14$Sgeria_P1) 
taula1
chisq.test(taula1) # Khi-karratu testa

# Aldagai JARRAITUA denean:

# Normaltasuna aztertzeko:

lillie.test(DataLAC14$Edad)  

# Normaltasuna jarraitzen ez bada:

wilcox.test(DataLAC14$Edad ~ DataLAC14$Sexo, data = DataLAC14, correct = FALSE) # Wilcoxon-en testa



# Erregresio logistiko bakunak eraikitzeko:

eredu_sinplea<- glm(Evento~Edad, data=DataLAC14,family=binomial)
anova(eredua_sinplea,test="Chisq") # Egiantz arrazoiaren testa 

# Odds ratioa eta bere konfiantza tartea lolrtzeko:
exp(coefficients(eredu_sinplea))   
exp(confint.default(eredu_sinplea)) 


# Aldagai jarraituen linealtasuna aztertzeko:

Edad_gam <- gam(Evento ~ s(Edad, bs="ps"), method="REML",family=binomial,data=DataLAC14) # GAM eredua eraiki
plot(Edad_gam,main="Adinaren linealtasuna")
summary(Edad_gam)
# bs="ps" idatziz p-splinea erabiltzen ari gara



# Eredu anizkoitza eraikitzeko:
# GAM eredua eraiki TUG ez baita lineala
# s(TUG_seg,by=Sexo, bs="ps"): TUG eta sexuaren interakzioa p-splineak erabiliz
eredu_anizkoitza<- gam(Evento ~ Sgeria_P3 + Salud_3K
                        + Edad 
                        + Polifarmacia
                        + s(TUG_seg,by=Sexo, bs="ps"), data=DataLAC14, family = binomial)

summary(eredu_anizkoitza) 


# Mozketa puntu optimoak kalkulatzeko:

# Eredu SINPLEAN:

# Kasu orokorrerako
#Genetic
sinple_orokorra_genetic<-catpredi(Evento~1,cat.var="TUG_seg",cat.points=1,data=DataLAC14,method="genetic",correct.AUC=TRUE)
plot(sinple_orokorra_genetic)   # Grafikoa ikusteko
sinple_orokorra_genetic
#Addfor
set.seed(500)
sinple_orokorra_addfor<-catpredi(Evento~1,cat.var="TUG_seg",cat.points=1,data=DataLAC14,method="addfor",correct.AUC=TRUE,control = controlcatpredi(addfor.g = 1000))
plot(sinple_orokorra_addfor)  
sinple_orokorra_addfor

# Emakumezkoen kasurako
DataEmakumeak <- subset(DataLAC14, Sexo == "Mujer") # Emakumezkoen datuak
#Genetic
sinple_emakume_genetic<-catpredi(Evento~1,cat.var="TUG_seg",cat.points=1,data=DataEmakumeak,method="genetic",correct.AUC=TRUE)
plot(sinple_emakume_genetic)  
sinple_emakume_genetic
#Addfor
sinple_emakume_addfor<-catpredi(Evento~1,cat.var="TUG_seg",cat.points=1,data=DataEmakumeak,method="addfor",correct.AUC=TRUE,control = controlcatpredi(addfor.g = 1000))
plot(sinple_emakume_addfor) 
sinple_emakume_addfor


# Gizonezkoen kasurako
DataGizonezkoak <- subset(DataLAC14, Sexo == "Hombre") # Gizonezkoen datuak
#Genetic
sinple_gizon_genetic<-catpredi(Evento~1,cat.var="TUG_seg",cat.points=1,data=DataGizonezkoak,method="genetic",correct.AUC=TRUE)
plot(sinple_gizon_genetic)   
sinple_gizon_genetic
#Addfor 
sinple_gizon_addfor<-catpredi(Evento~1,cat.var="TUG_seg",cat.points=1,data=DataGizonezkoak,method="addfor",correct.AUC=TRUE,control = controlcatpredi(addfor.g = 1000))
plot(sinple_gizon_addfor)   
sinple_gizon_addfor


# Eredu ANIZKOITZEAN:

# Kasu orokorrerako
# Genetic
anizkoitza_orokorra_genetic<-catpredi(Evento~Sexo+Sgeria_P3 + Salud_3K + Edad + Polifarmacia,cat.var="TUG_seg",cat.points=1,data=DataLAC14,method="genetic",correct.AUC=TRUE)
plot(anizkoitza_orokorra_genetic)  
anizkoitza_orokorra_genetic
#Addfor
anizkoitza_orokorra_addfor<-catpredi(Evento~Sexo+Sgeria_P3 + Salud_3K + Edad + Polifarmacia,cat.var="TUG_seg",cat.points=1,data=DataLAC14,method="addfor",correct.AUC=TRUE,control=controlcatpredi(addfor.g = 1000))
plot(anizkoitza_orokorra_addfor)  
anizkoitza_orokorra_addfor

# Emakumezkoen kasurako
#Genetic
anizkoitza_emakume_genetic<-catpredi(Evento~Sgeria_P3 + Salud_3K + Edad + Polifarmacia,cat.var="TUG_seg",cat.points=1,data=DataEmakumeak,method="genetic",correct.AUC=TRUE)
plot(anizkoitza_emakume_genetic)  
anizkoitza_emakume_genetic
#Addfor
anizkoitza_emakume_addfor<-catpredi(Evento~Sgeria_P3 + Salud_3K + Edad + Polifarmacia,cat.var="TUG_seg",cat.points=1,data=DataEmakumeak,method="addfor",correct.AUC=TRUE,control=controlcatpredi(addfor.g = 1000))
plot(anizkoitza_emakume_addfor)  
anizkoitza_emakume_addfor

# Gizonezkoen kasurako
#Genetic
anizkoitza_gizon_genetic<-catpredi(Evento~Sgeria_P3 + Salud_3K + Edad + Polifarmacia,cat.var="TUG_seg",cat.points=1,data=DataGizonezkoak,method="genetic",correct.AUC=TRUE)
plot(anizkoitza_gizon_genetic)
anizkoitza_gizon_genetic
#Addfor
anizkoitza_gizon_addfor<-catpredi(Evento~Sgeria_P3 + Salud_3K + Edad + Polifarmacia,cat.var="TUG_seg",cat.points=1,data=DataGizonezkoak,method="addfor",correct.AUC=TRUE,control=controlcatpredi(addfor.g = 1000))
plot(anizkoitza_gizon_addfor)  
anizkoitza_gizon_addfor



# TUG aldagaiaren kategorizazioa lortutako mozketa puntu optimoak erabiliz

# SINPLEAN
DataLAC14$TUG_cat_sinplea_orokorra <- ifelse(DataLAC14$TUG_seg<= 12.12, "Sendoa", "Hauskorra")
DataEmakumeak$TUG_cat_sinplea_emakumeak <- ifelse(DataEmakumeak$TUG_seg<= 12.29, "Sendoa", "Hauskorra")  
DataGizonezkoak$TUG_cat_sinplea_gizonezkoak <- ifelse(DataGizonezkoak$TUG_seg<= 11.27, "Sendoa", "Hauskorra")
# ANIZKOITZEAN
DataLAC14$TUG_cat_anizkoitza_orokorra <- ifelse(DataLAC14$TUG_seg<= 12.85, "Sendoa", "Hauskorra")
DataEmakumeak$TUG_cat_anizkoitza_emakumeak <- ifelse(DataEmakumeak$TUG_seg<= 20.59, "Sendoa", "Hauskorra")
DataGizonezkoak$TUG_cat_anizkoitza_gizonezkoak <- ifelse(DataGizonezkoak$TUG_seg<= 10.88, "Sendoa", "Hauskorra")



# TUG aldagaiaren banaketa kasu bakoitzerako:

# SINPLEAN
table(DataLAC14$TUG_cat_sinlea_orokorra, DataLAC14$Evento) # Orokorra
table(DataEmakumeak$TUG_cat_sinlea_emakumeak, DataEmakumeak$Evento) # Emakumeena
table(DataGizonezkoak$TUG_cat_sinplea_gizonezkoak, DataGizonezkoak$Evento) # Gizonezkoena
# ANIZKOITZEAN
table(DataLAC14$TUG_cat_anizkoitza_orokorra, DataLAC14$Evento) # Orokorra
table(DataEmakumeak$TUG_cat_anizkoitza_emakumeak, DataEmakumeak$Evento) #Emakumeena
table(DataGizonezkoak$TUG_cat_anizkoitza_gizonezkoak, DataGizonezkoak$Evento) # Gizonezkoena




