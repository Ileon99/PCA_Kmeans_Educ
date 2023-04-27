#Automatisation of our cleaning function


library(readr)
library(haven)

library(tidyselect)
library(magrittr) 
library(dplyr)


names_elementary_students<-c("EstudiantesFemeninoPrimerAñoEGB",	"EstudiantesMasculinoPrimerAñoEGB",	"EstudiantesFemeninoSegundoAñoEGB",	"EstudiantesMasculinoSegundoAñoEGB",
                             "EstudiantesFemeninoTercerAñoEGB",	"EstudiantesMasculinoTercerAñoEGB",	"EstudiantesFemeninoCuartoAñoEGB",	"EstudiantesMasculinoCuartoAñoEGB",	
                             "EstudiantesFemeninoQuintoAñoEGB","EstudiantesMasculinoQuintoAñoEGB",	"EstudiantesFemeninoSextoAñoEGB",	"EstudiantesMasculinoSextoAñoEGB",	"EstudiantesFemeninoSeptimoAñoEGB",	"EstudiantesMasculinoSeptimoAñoEGB",
                             "EstudiantesFemeninoOctavoAñoEGB",	"EstudiantesMasculinoOctavoAñoEGB",	"EstudiantesFemeninoNovenoAñoEGB",	"EstudiantesMasculinoNovenoAñoEGB",
                             "EstudiantesFemeninoDecimoAñoEGB",	"EstudiantesMasculinoDecimoAñoEGB")

#INEC_POP

Pop_INEC <- read_csv2("Pop_INEC.csv")
province_codes <- read_csv2("~/PHD/Clean_Enemdu/province_codes.csv")
Pop_INEC_area <- read_csv2("Pop_INEC_area.csv")


#MINEDUC

Inicio2010 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2010-2011Inicio.csv")
Inicio2011 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2011-2012Inicio.csv")
Inicio2012 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2012-2013Inicio.csv")
Inicio2013 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2013-2014Inicio.csv")
Inicio2014 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2014-2015Inicio.csv")
Inicio2015 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2015-2016Inicio.csv")

Inicio2016 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2016-2017_Inicio.csv")
Inicio2017 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2017-2018Inicio.csv")
Inicio2018 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2018-2019Inicio.csv")
Inicio2019 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2019-2020Inicio.csv")
Inicio2020 <- read_csv2("2_MINEDUC_RegistrosAdministrativos_2020-2021Inicio.csv")
Inicio2021 <- read_csv2("2-MINEDUC_RegistrosAdministrativos_2021-2022Inicio.csv")


#ENEMDU

ENEMDU2010 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2010_12_hom.csv")
ENEMDU2011 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2011_12_hom.csv")
ENEMDU2012 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2012_12_hom.csv")
ENEMDU2013 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2013_12_hom.csv")
ENEMDU2014 <- read_csv2("~/PHD/Clean_Enemdu/201412_EnemduBDD_15anios.csv")
ENEMDU2015 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2015_12_hom.csv")
ENEMDU2016 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2016_12_hom.csv")
ENEMDU2017 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2017_12_hom.csv")
ENEMDU2018 <- read_csv2("~/PHD/Clean_Enemdu/ENEMDU_PERSONAS_2018_12_hom.csv")
ENEMDU2019 <- read_csv2("~/PHD/Clean_Enemdu/enemdu_persona_201912.csv")
ENEMDU2020 <- read_csv("~/PHD/Clean_Enemdu/enemdu_persona_2020_12.csv")
ENEMDU2021 <- read_csv("~/PHD/Clean_Enemdu/enemdu_persona_2021_12.csv")

AutoClean_educ_indices <- function(Inicio, ENEMDU, year){
  
  educ_indices <- province_codes
  educ_indices$year <- rep(year, length(educ_indices$province_c))
  
  # Basic education enrolment rate (BEER) = Number of students enroled in primary schools / pop between 5 - 14 years
  
  educ_indices$beer <- rep(0, length(educ_indices$province))
  
  #Sum all the registered students in elementary school that follows regular education
  
  Inicio$EstudiantesEGB <- rowSums(Inicio %>% subset( select = names_elementary_students))
  
  educ_indices$population1_14 <- Pop_INEC$`5  -  9`[Pop_INEC$year==year]+ Pop_INEC$`10 - 14` [Pop_INEC$year==year] + Pop_INEC$`1  -  4`[Pop_INEC$year==year]
 
  Inicio$es_students_ppy <- rep(0, length(Inicio$Periodo))
  
  my_vec <- c()
  prov_c <- c()
  
  for (i in unique(educ_indices$province_c)) {
    new_value <- sum(Inicio$EstudiantesEGB[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular"])
    my_vec <- c(my_vec, new_value)
    prov_c <- c(prov_c, i)
  }
  
  my_vec
  prov_c
  
  educ_indices$es_students_ppy <- my_vec
  
  educ_indices$beer <- educ_indices$es_students_ppy / educ_indices$population1_14
  
  #We note that there are more enrolled students than people in the thoeric age of studiing
  
  #We are going to compute the high scool enrolement rate hser
  
  educ_indices$hser <- rep(0, length(educ_indices$province))
  
  #Take the columns names of the variables of interest
  
  names_hs_students <- c("EstudiantesFemeninoSegundoAñoBACH", "EstudiantesMasculinoSegundoAñoBACH", "EstudiantesFemeninoTercerAñoBACH", "EstudiantesMasculinoTercerAñoBACH")
  
  #Now we sum how many students are enrolled per institution 
  
  Inicio$EstudiantesHS <- rowSums(Inicio %>% subset( select = names_hs_students))
  
  #Now we compute how many enrolled students are per province 
  
  my_vec2 <- c()
  prov_c2 <- c()
  
  for (i in unique(educ_indices$province_c)) {
    new_value2 <- sum(Inicio$EstudiantesHS[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular"])
    my_vec2 <- c(my_vec2, new_value2)
    prov_c2 <- c(prov_c2, i)
  }
  
  my_vec2
  prov_c2
  
  educ_indices$hs_students_ppy <- my_vec2
  
  #We create a variable with the population between 15 and 17 years
  
  educ_indices$population15_19 <- Pop_INEC$`15 - 19`[Pop_INEC$year==year]
  
  educ_indices$hser <- educ_indices$hs_students_ppy / educ_indices$population15_19
  
  
  #Number of students that haev aproved high school
  
  ENEMDU$hsa <- rep(0, length(ENEMDU$p10a))
  ENEMDU$p10a<- as.numeric(ENEMDU$p10a)
  ENEMDU$p10b<- as.numeric(ENEMDU$p10b)
  
  
  #In the Enemdu in year Santo domingo and Santa Elena are not accounted as a province
  #We find the city code of the province by comparing the present codes in year and 2018
  #Should compute province code
  
  ENEMDU$province_c <- floor(as.numeric(ENEMDU$ciudad)/10000)
  ENEMDU$population18_29 <- rep(0,length(ENEMDU$ciudad))
  ENEMDU$p03 <- as.numeric(ENEMDU$p03)
  ENEMDU$population18_29[ENEMDU$p03>=18 & ENEMDU$p03 <=29 ]=1
  ENEMDU$hsa[ENEMDU$p10a >= 6 & ENEMDU$p10b == 6 & ENEMDU$p03>=18 & ENEMDU$p03 <=29]=1
  ENEMDU$ciudad <- as.numeric(ENEMDU$ciudad)
  
  #We find that the code of santo domingo was 1706 before becoming a province
  #Santa Elena is 923 915 917 and 926
  
  #Santo Domingo
  
  ENEMDU$province_c[floor(ENEMDU$ciudad/100) == 1706]<- 23
  
  #Santa Elena
  
  ENEMDU$province_c[floor(ENEMDU$ciudad/100) == 923 | floor(ENEMDU$ciudad/100) == 915| floor(ENEMDU$ciudad/100) == 917| floor(ENEMDU$ciudad/100) == 926 ]<- 24
  
  
  my_vec3 <- c()
  prov_c3 <- c()
  pop18_29 <- c()
  vec_test <- c()
  
  for (i in unique(province_codes$province_c)) {
    new_value3 <- sum(ENEMDU$hsa[ENEMDU$province_c == i])
    my_vec3 <- c(my_vec3, new_value3)
    prov_c3 <- c(prov_c3, i)
    pop18_29nv <- sum(ENEMDU$population18_29[ENEMDU$province_c == i])
    pop18_29 <- c(pop18_29,pop18_29nv)
    sample_size <- sum(ENEMDU$population18_29[ENEMDU$province_c == i])
    if(sample_size > 0){
      test <- prop.test(new_value3/pop18_29nv, sample_size, p = NULL, alternative = "two.sided")
      nv_test <- test$p.value
    } else {
      nv_test <- NA
    }
    vec_test <- c(vec_test, nv_test)
  }
  
  my_vec3
  prov_c3
  pop18_29
  vec_test
  
  educ_indices$hscr <- my_vec3 / pop18_29
  
  #We will now divide the beer and the hser in two variables each for the rural and the urban zone
  
  #We are going to create a variable of population between 1 and 14 year for each province per zone
  
  
  pop1_14_rural <- c()
  pop1_14_urban <- c()
  prov_c4 <- c()
  
  
  for (i in unique(province_codes$province_c)) {
    nv_rur1 <- Pop_INEC_area$`1  -  4`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "RURAL"] + 
      Pop_INEC_area$`5  -  9`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "RURAL"] +
      Pop_INEC_area$`10 - 14`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "RURAL"]
    nv_urb1 <- Pop_INEC_area$`1  -  4`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "URBAN"] + 
      Pop_INEC_area$`5  -  9`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "URBAN"] +
      Pop_INEC_area$`10 - 14`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "URBAN"]
    pop1_14_rural <- c(pop1_14_rural, nv_rur1)
    pop1_14_urban <- c(pop1_14_urban, nv_urb1)
    prov_c4 <- c(prov_c4, i)
  }
  
  pop1_14_rural
  pop1_14_urban 
  prov_c4 
  
  educ_indices$pop_urb1_14 <- pop1_14_urban
  educ_indices$pop_rur1_14 <- pop1_14_rural
  
  #Now we create a variable basic education enrollement rate in the rural area
  
  my_vec4 <- c()
  prov_c <- c()
  
  for (i in unique(educ_indices$province_c)) {
    new_value4 <- sum(Inicio$EstudiantesEGB[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular" & Inicio$Area == "RuralINEC"])
    my_vec4 <- c(my_vec4, new_value4)
    prov_c <- c(prov_c, i)
  }
  
  my_vec4
  prov_c
  
  educ_indices$beer_rur <- my_vec4 / pop1_14_rural
  
  #Now we create a variable basic education enrollement rate in the urban area
  
  my_vec5 <- c()
  prov_c <- c()
  
  for (i in unique(educ_indices$province_c)) {
    new_value5 <- sum(Inicio$EstudiantesEGB[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular" & Inicio$Area == "UrbanaINEC"])
    my_vec5 <- c(my_vec5, new_value5)
    prov_c <- c(prov_c, i)
  }
  
  my_vec5
  prov_c
  
  educ_indices$beer_urb <- my_vec5 / pop1_14_urban
  
  #Same for the high school enrollement
  
  hser_rur <- c()
  hser_urb <- c()
  prov_c <- c()
  
  for (i in unique(educ_indices$province_c)) {
    hser_rurnv <- sum(Inicio$EstudiantesHS[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular" & Inicio$Area == "RuralINEC" ])
    hser_urbnv <- sum(Inicio$EstudiantesHS[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular" & Inicio$Area == "UrbanaINEC" ])
    hser_rur <- c(hser_rur, hser_rurnv)
    hser_urb <- c(hser_urb, hser_urbnv)
    prov_c <- c(prov_c, i)
  }
  
  hser_rur
  hser_urb
  
  pop15_19_rural <- c()
  pop15_19_urban <- c()
  prov_c <- c()
  
  for (i in unique(province_codes$province_c)) {
    nv_rur1 <- Pop_INEC_area$`15 - 19`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "RURAL"]
    nv_urb1 <- Pop_INEC_area$`15 - 19`[Pop_INEC_area$province_c == i & Pop_INEC_area$year == year & Pop_INEC_area$zone == "URBAN"]
    pop15_19_rural <- c(pop15_19_rural, nv_rur1)
    pop15_19_urban <- c(pop15_19_urban, nv_urb1)
    prov_c <- c(prov_c, i)
  }
  
  pop15_19_rural
  pop15_19_urban 
  prov_c 
  
  educ_indices$hser_urb <- hser_urb/pop15_19_urban
  educ_indices$hser_rur <- hser_rur/pop15_19_rural
  
  #We are going to compute the year of study average per province
  #First with the ENEMDU we compute how many year each individual studied
  
  #"año aprobado"
  
  ENEMDU$ap <- as.numeric(ENEMDU$p10b)
  
  
  #Education level
  
  ENEMDU$education <- rep(0, length(ENEMDU$p10a))
  ENEMDU$education <- as.numeric(ENEMDU$p10a)
  ENEMDU$education[is.na(ENEMDU$education)]=0
  
  #Years of education
  ENEMDU$education_y <- rep(0, length(ENEMDU$p10a))
  ENEMDU$education_y[ENEMDU$education == 1]=0
  ENEMDU$education_y[ENEMDU$education == 2]= ENEMDU$ap[ENEMDU$education == 2]
  ENEMDU$education_y[ENEMDU$education == 3]= 1
  ENEMDU$education_y[ENEMDU$education == 4]= ENEMDU$ap[ENEMDU$education == 4]+1
  ENEMDU$education_y[ENEMDU$education == 5]= ENEMDU$ap[ENEMDU$education == 5]
  ENEMDU$education_y[ENEMDU$education == 6]= ENEMDU$ap[ENEMDU$education == 6]+7
  ENEMDU$education_y[ENEMDU$education == 7]= ENEMDU$ap[ENEMDU$education == 7]+10
  ENEMDU$education_y[ENEMDU$education == 8]= ENEMDU$ap[ENEMDU$education == 8]+13
  ENEMDU$education_y[ENEMDU$education == 9]= ENEMDU$ap[ENEMDU$education == 9]+13
  ENEMDU$education_y[ENEMDU$education == 10]= ENEMDU$ap[ENEMDU$education == 10]+17
  
  ENEMDU$tool <- rep(1,length(ENEMDU$area))
  
  
  vec_educ_years <- c()
  prov_c <- c()
  pop <- c()
  vec_test <- c()
  
  for (i in unique(province_codes$province_c)) {
    nv_educ_y <- sum(ENEMDU$education_y[ENEMDU$province_c == i])
    vec_educ_years <- c(vec_educ_years, nv_educ_y)
    prov_c <- c(prov_c, i)
    popnv <- sum(ENEMDU$tool[ENEMDU$province_c == i])
    pop <- c(pop,popnv)
    sample_size <- sum(ENEMDU$tool[ENEMDU$province_c == i])
    if(sample_size > 0){
      test <- prop.test(nv_educ_y/popnv, sample_size, p = NULL, alternative = "two.sided")
      nv_test <- test$p.value
    } else {
      nv_test <- NA
    }
    vec_test <- c(vec_test, nv_test)
  }
  
  educ_indices$educ_years <- vec_educ_years/pop
  
  vec_test_educ_y <- vec_test
  
  #Now we compute the school assistance rate of the population between 3 and 21 years ar
  
  ENEMDU$assis <- rep(0, length(ENEMDU$ciudad))
  ENEMDU$assis[ENEMDU$p07 == 1]=1
  ENEMDU$pop3_18 <- rep(0,length(ENEMDU$ciudad))
  ENEMDU$pop3_18[ENEMDU$p03 >2 & ENEMDU$p03<21]=1
  
  vec_assis <- c()
  prov_c <- c()
  pop3_18 <- c()
  vec_test <- c()
  
  for (i in unique(province_codes$province_c)) {
    nv_assis <- sum(ENEMDU$assis[ENEMDU$province_c == i])
    vec_assis <- c(vec_assis, nv_assis)
    prov_c <- c(prov_c, i)
    popnv3_18 <- sum(ENEMDU$pop3_18[ENEMDU$province_c == i])
    pop3_18 <- c(pop3_18,popnv3_18)
    sample_size <- sum(ENEMDU$pop3_18[ENEMDU$province_c == i])
    nv_test <- 0
    if(sample_size>0){
      test <- prop.test(nv_assis/popnv3_18, sample_size, p = NULL, alternative = "two.sided")
      nv_test <- test$p.value
    }
    vec_test <- c(vec_test, nv_test)
  }
  
  vec_test_assis <- vec_test
  
  educ_indices$ar <- vec_assis/pop3_18
  
  #Now we compute the number of people that are between 3 and 18 years per Education Instituion in each province
  #pop_per_IE
  
  educ_indices$population5_19 <- Pop_INEC$`5  -  9`[Pop_INEC$year==year]+ Pop_INEC$`10 - 14` [Pop_INEC$year==year]  + Pop_INEC$`15 - 19`[Pop_INEC$year==year]
  
  
  vec_IE <- c()
  prov_c <- c()
  
  for (i in unique(province_codes$province_c)) {
    nv_IE <- nrow(Inicio[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular",])
    vec_IE <- c(vec_IE, nv_IE)
    prov_c <- c(prov_c, i)
  }
  
  educ_indices$pop_per_IE <- educ_indices$population5_19/vec_IE
  
  #Students per teacher in IE 
  
  Inicio$Studens_per_teacher <- Inicio$Total_Estudiantes / Inicio$Total_Docentes
  
  vec_spt <- c()
  prov_c <- c()
  
  for (i in unique(province_codes$province_c)) {
    nv_spt <-  sum(Inicio[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular" & Inicio$Total_Docentes != 0 , "Studens_per_teacher"]) / nrow(Inicio[Inicio$Cod_Provincia == i & Inicio$Tipo_Educacion == "Educación Regular" & Inicio$Total_Docentes != 0 ,])
    vec_spt <- c(vec_spt, nv_spt)
    prov_c <- c(prov_c, i)
  }
  
  educ_indices$spt <- vec_spt
  
  educ_indices$row_name <- paste(educ_indices$province_c, educ_indices$year, sep = "_")
  
  row.names(educ_indices) <- educ_indices$row_name
  
  return(educ_indices)
   
}

#educ indices 

educ_indices2010 <- AutoClean_educ_indices(Inicio2010,ENEMDU2010, 2010)
educ_indices2011 <- AutoClean_educ_indices(Inicio2011,ENEMDU2011, 2011)
educ_indices2012 <- AutoClean_educ_indices(Inicio2012,ENEMDU2012, 2012)
educ_indices2013 <- AutoClean_educ_indices(Inicio2013,ENEMDU2013, 2013)
educ_indices2014 <- AutoClean_educ_indices(Inicio2014,ENEMDU2014, 2014)
educ_indices2015 <- AutoClean_educ_indices(Inicio2015,ENEMDU2015, 2015)
educ_indices2016 <- AutoClean_educ_indices(Inicio2016,ENEMDU2016, 2016)
educ_indices2017 <- AutoClean_educ_indices(Inicio2017,ENEMDU2017, 2017)
educ_indices2018 <- AutoClean_educ_indices(Inicio2018,ENEMDU2018, 2018)
educ_indices2019 <- AutoClean_educ_indices(Inicio2019,ENEMDU2019, 2019)
educ_indices2020 <- AutoClean_educ_indices(Inicio2020,ENEMDU2020, 2020)
educ_indices2021 <- AutoClean_educ_indices(Inicio2021,ENEMDU2021, 2021)




