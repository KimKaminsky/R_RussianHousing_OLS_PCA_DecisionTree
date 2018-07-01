###################################
# Install packages and libraries  #
###################################

#install.packages("dummies")
#install.packages(c("plyr", "dplyr", "lubridate", "dummies", "Metrics", "car",
#                   "ggplot", "corrplot", "rpart", "rattle", "dummies"))
library(plyr)
library(dplyr)
library(lubridate)
library(dummies)
library(Metrics)
library(car)
library(ggplot2)
library(doBy)
library(corrplot)
library(rpart)
library(rattle)
library(dummies)

##########################################
# Read in Files and view data structures #
##########################################
Train <- read.csv("train.csv", sep = ",")
Test <- read.csv("Test.csv", sep = ",")
Macro <- read.csv("macro.csv", sep = ",", stringsAsFactors = FALSE)
BAFix <- read.csv("badAddressFix.csv", sep = ',')


# Change rows in Train set to have correct row from BAFix dataset
for (i in 1:nrow(BAFix)) {

    if (BAFix$id[i] <= 30473) {
        for (j in 2:ncol(BAFix)) {
            colName <- noquote(names(BAFix[j]))
            colIndx <- which(colnames(Train) == colName)
            Train[BAFix[i, 1], colIndx] <- BAFix[i, j]
        }
    }
}

for (i in 1:nrow(BAFix)) {

    if (BAFix$id[i] > 30473) {
        for (j in 2:ncol(BAFix)) {
            colName <- noquote(names(BAFix[j]))
            colIndx <- which(colnames(Test) == colName)
            Test[which(Test$id == BAFix[i, 1]), colIndx] <- BAFix[i, j]
        }
    }
}


## 291 predictor variables with response variable = price_doc
## 'data.frame': 30471 obs. of  292 variables
#str(Train, list.len = ncol(Train)) 

## 291 predictor variables
## 'data.frame': 7662 obs. of  291 variables
#str(Test, list.len = ncol(Test)) 

## 100 variables in Macro - data on Russia's macroeconomy and financial sector 
## 'data.frame': 2484 obs. of  100 variables
#str(Macro, list.len = ncol(Macro)) 

## Quick view of the data in each of the datasets
#head(Train)
#head(Test)
#head(Macro)

# Get a list of variables with missing values for Training data
tmv <- function() {
    train.missing.values <- data.frame(matrix(ncol = 2, nrow = 0))
    cols <- c("variable", "numMissing")
    colnames(train.missing.values) <- cols

    for (i in 1:length(Train)) {

        if (sum(is.na(Train[i])) > 0) {
            train.missing.values[nrow(train.missing.values) + 1,] <-
            c(noquote(names(Train[i])), sum(is.na(Train[i])))
        }
    }

    train.missing.values
}

tmv()

#######################################################
# Drop variables that have too many missing variables #
#######################################################
Train$m_state <- 0
Train$m_state[is.na(Train$state)] <- 1
Train$state <- NULL

Test$m_state <- 0
Test$m_state[is.na(Test$state)] <- 1
Test$state <- NULL

Train$m_hospital_beds_raion <- 0
Train$m_hospital_beds_raion[is.na(Train$hospital_beds_raion)] <- 1
Train$hospital_beds_raion <- NULL

Test$m_hospital_beds_raion <- 0
Test$m_hospital_beds_raion[is.na(Test$hospital_beds_raion)] <- 1
Test$hospital_beds_raion <- NULL

Train$m_cafe_sum_500_min_price_avg <- 0
Train$m_cafe_sum_500_min_price_avg[is.na(Train$cafe_sum_500_min_price_avg)] <- 1
Train$cafe_sum_500_min_price_avg <- NULL

Test$m_cafe_sum_500_min_price_avg <- 0
Test$m_cafe_sum_500_min_price_avg[is.na(Test$cafe_sum_500_min_price_avg)] <- 1
Test$cafe_sum_500_min_price_avg <- NULL

Train$m_cafe_sum_500_max_price_avg <- 0
Train$m_cafe_sum_500_max_price_avg[is.na(Train$cafe_sum_500_max_price_avg)] <- 1
Train$cafe_sum_500_max_price_avg <- NULL

Test$m_cafe_sum_500_max_price_avg <- 0
Test$m_cafe_sum_500_max_price_avg[is.na(Test$cafe_sum_500_max_price_avg)] <- 1
Test$cafe_sum_500_max_price_avg <- NULL

Train$m_cafe_avg_price_500 <- 0
Train$m_cafe_avg_price_500[is.na(Train$cafe_avg_price_500)] <- 1
Train$cafe_avg_price_500 <- NULL

Test$m_cafe_avg_price_500 <- 0
Test$m_cafe_avg_price_500[is.na(Test$cafe_avg_price_500)] <- 1
Test$cafe_avg_price_500 <- NULL

Train$m_material <- 0
Train$m_material[is.na(Train$material)] <- 1
Train$material <- NULL

Test$m_material <- 0
Test$m_material[is.na(Test$material)] <- 1
Test$material <- NULL

#################################################
# Impute life_sq using rules from decision tree #
#################################################
Train$m_life_sq <- 0
Train$m_life_sq[is.na(Train$life_sq)] <- 1

Train$imp_life_sq <- Train$life_sq
Train$imp_life_sq[Train$full_sq < 58.5 & Train$full_sq < 42.5 & is.na(Train$imp_life_sq)] <- 21.30
Train$imp_life_sq[Train$full_sq < 58.5 & Train$full_sq >= 42.5 & is.na(Train$imp_life_sq)] <- 32.15
Train$imp_life_sq[Train$full_sq >= 58.5 & Train$full_sq < 122.5 & is.na(Train$imp_life_sq)] <- 49.67
Train$imp_life_sq[Train$full_sq >= 58.5 & Train$full_sq >= 122.5 & is.na(Train$imp_life_sq)] <- 110.96
Train$life_sq <- NULL

Test$m_life_sq <- 0
Test$m_life_sq[is.na(Test$life_sq)] <- 1

Test$imp_life_sq <- Test$life_sq
Test$imp_life_sq[Test$full_sq < 58.5 & Test$full_sq < 42.5 & is.na(Test$imp_life_sq)] <- 21.30
Test$imp_life_sq[Test$full_sq < 58.5 & Test$full_sq >= 42.5 & is.na(Test$imp_life_sq)] <- 32.15
Test$imp_life_sq[Test$full_sq >= 58.5 & Test$full_sq < 122.5 & is.na(Test$imp_life_sq)] <- 49.67
Test$imp_life_sq[Test$full_sq >= 58.5 & Test$full_sq >= 122.5 & is.na(Test$imp_life_sq)] <- 110.96
Test$life_sq <- NULL

##################################################
# Impute kitch_sq using rules from decision tree #
##################################################
Train$m_kitch_sq <- 0
Train$m_kitch_sq[is.na(Train$kitch_sq)] <- 1

Train$imp_kitch_sq <- Train$kitch_sq
Train$imp_kitch_sq[is.na(Train$imp_kitch_sq)] <- 6.44

Train$kitch_sq <- NULL

Test$m_kitch_sq <- 0
Test$m_kitch_sq[is.na(Test$kitch_sq)] <- 1

Test$imp_kitch_sq <- Test$kitch_sq
Test$imp_kitch_sq[is.na(Test$imp_kitch_sq)] <- 6.44

Test$kitch_sq <- NULL


##################
# Impute floor   #
##################
Train$m_floor <- 0
Train$m_floor[is.na(Train$floor)] <- 1

Train$imp_floor <- Train$floor
Train$imp_floor[Train$max_floor < 12.5 & Train$max_floor >= 1.5 & is.na(Train$imp_floor)] <- 4.81
Train$imp_floor[Train$max_floor >= 12.5 & Train$max_floor < 20.5 & is.na(Train$imp_floor)] <- 8.85
Train$imp_floor[Train$max_floor >= 12.5 & Train$max_floor >= 20.5 & is.na(Train$imp_floor)] <- 13.48
Train$imp_floor[Train$max_floor < 12.5 & Train$max_floor < 1.5 & is.na(Train$imp_floor)] <- 8.14
Train$floor <- NULL

Test$m_floor <- 0
Test$m_floor[is.na(Test$floor)] <- 1

Test$imp_floor <- Test$floor
Test$imp_floor[Test$max_floor < 12.5 & Test$max_floor >= 1.5 & is.na(Test$imp_floor)] <- 4.81
Test$imp_floor[Test$max_floor >= 12.5 & Test$max_floor < 20.5 & is.na(Test$imp_floor)] <- 8.85
Test$imp_floor[Test$max_floor >= 12.5 & Test$max_floor >= 20.5 & is.na(Test$imp_floor)] <- 13.48
Test$imp_floor[Test$max_floor < 12.5 & Test$max_floor < 1.5 & is.na(Test$imp_floor)] <- 8.14
Test$floor <- NULL


######################
# Impute max_floor   #
######################
Train$m_max_floor <- 0
Train$m_max_floor[is.na(Train$max_floor)] <- 1

Train$imp_max_floor <- Train$max_floor
Train$imp_max_floor[Train$build_year < 1978 & Train$imp_floor < 9.5 & is.na(Train$imp_max_floor)] <- 8.37

Train$imp_max_floor[Train$build_year >= 1978 & Train$sub_area %in% c("Ajeroport", "Akademicheskoe", "Altuf'evskoe", "Babushkinskoe",
                     "Basmannoe", "Beskudnikovskoe", "Birjulevo Vostochnoe", "Bogorodskoe", "Brateevo", "Butyrskoe", "Caricyno",
                     "Cheremushki", "Chertanovo Central 'noe", "Chertanovo Juzhnoe", "Chertanovo Severnoe", "Donskoe", "Filevskij Park",
                      "Fili Davydkovo", "Gagarinskoe", "Gol ' janovo", "Golovinskoe", "Horoshevo - Mnevniki", "Horoshevskoe",
                      "Hovrino", "Ivanovskoe", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Juzhnoe Tushino", "Juzhnoportovoe",
                      "Kon ' kovo", "Koptevo", "Krylatskoe", "Kuncevo", "Lefortovo", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe",
                      "Marfino", "Mitino", "Molzhaninovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe", "Nagatino - Sadovniki",
                      "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Nizhegorodskoe", "Novo - Peredelkino", "Obruchevskoe",
                      "Ochakovo - Matveevskoe", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe", "Ostankinskoe",
                      "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Poselenie Moskovskij", "Poselenie Shherbinka", "Poselenie Vnukovskoe",
                      "Preobrazhenskoe", "Presnenskoe", "Prospekt Vernadskogo", "Ramenki", "Rjazanskij", "Severnoe", "Severnoe Butovo",
                      "Severnoe Izmajlovo", "Shhukino", "Silino", "Sokol", "Sokolinaja Gora", "Solncevo", "Taganskoe", "Teplyj Stan",
                      "Timirjazevskoe", "Troparevo - Nikulino", "Veshnjaki", "Vostochnoe Degunino", "Zjablikovo", "Zjuzino")
                    & is.na(Train$imp_max_floor)] <- 17.30

Train$imp_max_floor[Train$build_year >= 1978 & Train$sub_area %in% c("Alekseevskoe", "Begovoe", "Bibirevo", "Birjulevo Zapadnoe", "Danilovskoe",
                    "Dmitrovskoe", "Dorogomilovo", "Hamovniki", "Izmajlovo", "Jaroslavskoe", "Juzhnoe Butovo", "Kapotnja", "Kosino - Uhtomskoe",
                    "Kotlovka", "Krasnosel 'skoe", "Krjukovo", "Kurkino", "Kuz' minki", "Ljublino", "Losinoostrovskoe", "Mar 'ina Roshha", "Mar' ino",
                    "Matushkino", "Meshhanskoe", "Metrogorodok", "Novogireevo", "Novokosino", "Otradnoe", "Poselenie Desjonovskoe",
                    "Poselenie Filimonkovskoe", "Poselenie Kievskij", "Poselenie Kokoshkino", "Poselenie Krasnopahorskoe", "Poselenie Marushkinskoe",
                    "Poselenie Mosrentgen", "Poselenie Novofedorovskoe", "Poselenie Pervomajskoe", "Poselenie Rjazanovskoe", "Poselenie Rogovskoe",
                    "Poselenie Shhapovskoe", "Poselenie Sosenskoe", "Poselenie Voronovskoe", "Poselenie Voskresenskoe", "Rostokino", "Savelki",
                    "Savelovskoe", "Severnoe Medvedkovo", "Severnoe Tushino", "Sokol 'niki", "Staroe Krjukovo", "Strogino", "Sviblovo",
                    "Tekstil ' shhiki", "Troickij okrug", "Tverskoe", "Vnukovo", "Vojkovskoe", "Vostochnoe Izmajlovo", "Vyhino - Zhulebino",
                    "Zamoskvorech ' e", "Zapadnoe Degunino")
                    & is.na(Train$imp_max_floor)] <- 13.47
Train$imp_max_floor[Train$build_year < 1978 & Train$imp_floor >= 9.5 & is.na(Train$imp_max_floor)] <- 13.97
Train$max_floor <- NULL

Test$m_max_floor <- 0
Test$m_max_floor[is.na(Test$max_floor)] <- 1

Test$imp_max_floor <- Test$max_floor
Test$imp_max_floor[Test$build_year < 1978 & Test$imp_floor < 9.5 & is.na(Test$imp_max_floor)] <- 8.37

Test$imp_max_floor[Test$build_year >= 1978 & Test$sub_area %in% c("Ajeroport", "Akademicheskoe", "Altuf'evskoe", "Babushkinskoe",
                     "Basmannoe", "Beskudnikovskoe", "Birjulevo Vostochnoe", "Bogorodskoe", "Brateevo", "Butyrskoe", "Caricyno",
                     "Cheremushki", "Chertanovo Central 'noe", "Chertanovo Juzhnoe", "Chertanovo Severnoe", "Donskoe", "Filevskij Park",
                      "Fili Davydkovo", "Gagarinskoe", "Gol ' janovo", "Golovinskoe", "Horoshevo - Mnevniki", "Horoshevskoe",
                      "Hovrino", "Ivanovskoe", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Juzhnoe Tushino", "Juzhnoportovoe",
                      "Kon ' kovo", "Koptevo", "Krylatskoe", "Kuncevo", "Lefortovo", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe",
                      "Marfino", "Mitino", "Molzhaninovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe", "Nagatino - Sadovniki",
                      "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Nizhegorodskoe", "Novo - Peredelkino", "Obruchevskoe",
                      "Ochakovo - Matveevskoe", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe", "Ostankinskoe",
                      "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Poselenie Moskovskij", "Poselenie Shherbinka", "Poselenie Vnukovskoe",
                      "Preobrazhenskoe", "Presnenskoe", "Prospekt Vernadskogo", "Ramenki", "Rjazanskij", "Severnoe", "Severnoe Butovo",
                      "Severnoe Izmajlovo", "Shhukino", "Silino", "Sokol", "Sokolinaja Gora", "Solncevo", "Taganskoe", "Teplyj Stan",
                      "Timirjazevskoe", "Troparevo - Nikulino", "Veshnjaki", "Vostochnoe Degunino", "Zjablikovo", "Zjuzino")
                    & is.na(Test$imp_max_floor)] <- 17.30

Test$imp_max_floor[Test$build_year >= 1978 & Test$sub_area %in% c("Alekseevskoe", "Begovoe", "Bibirevo", "Birjulevo Zapadnoe", "Danilovskoe",
                    "Dmitrovskoe", "Dorogomilovo", "Hamovniki", "Izmajlovo", "Jaroslavskoe", "Juzhnoe Butovo", "Kapotnja", "Kosino - Uhtomskoe",
                    "Kotlovka", "Krasnosel 'skoe", "Krjukovo", "Kurkino", "Kuz' minki", "Ljublino", "Losinoostrovskoe", "Mar 'ina Roshha", "Mar' ino",
                    "Matushkino", "Meshhanskoe", "Metrogorodok", "Novogireevo", "Novokosino", "Otradnoe", "Poselenie Desjonovskoe",
                    "Poselenie Filimonkovskoe", "Poselenie Kievskij", "Poselenie Kokoshkino", "Poselenie Krasnopahorskoe", "Poselenie Marushkinskoe",
                    "Poselenie Mosrentgen", "Poselenie Novofedorovskoe", "Poselenie Pervomajskoe", "Poselenie Rjazanovskoe", "Poselenie Rogovskoe",
                    "Poselenie Shhapovskoe", "Poselenie Sosenskoe", "Poselenie Voronovskoe", "Poselenie Voskresenskoe", "Rostokino", "Savelki",
                    "Savelovskoe", "Severnoe Medvedkovo", "Severnoe Tushino", "Sokol 'niki", "Staroe Krjukovo", "Strogino", "Sviblovo",
                    "Tekstil ' shhiki", "Troickij okrug", "Tverskoe", "Vnukovo", "Vojkovskoe", "Vostochnoe Izmajlovo", "Vyhino - Zhulebino",
                    "Zamoskvorech ' e", "Zapadnoe Degunino")
                    & is.na(Test$imp_max_floor)] <- 13.47
Test$imp_max_floor[Test$build_year < 1978 & Test$imp_floor >= 9.5 & is.na(Test$imp_max_floor)] <- 13.97
Test$max_floor <- NULL

############################################################################################
# There are still missing values for max_floor and floor .Would like to impute build_year  #
# but the data is messy, so decision tree is giving silly recommendations. Take a look at  #
# outliers first.                                                                          #
############################################################################################

# Assuming typos
Train$build_year[Train$build_year >= 20050000] <- 2005
Train$build_year[Train$build_year == 4965] <- 1965
Train$build_year[Train$build_year == 1691] <- 1991

# Assuming how many years ago it was built was entered
Train$build_year[Train$build_year < 1860 & !is.na(Train$build_year)] <-
    as.numeric(substring(Train$timestamp[Train$build_year < 1860 & !is.na(Train$build_year)], 1, 4)) -
    Train$build_year[Train$build_year < 1860 & !is.na(Train$build_year)]


Test$build_year[Test$build_year < 1860 & !is.na(Test$build_year)] <-
    as.numeric(substring(Test$timestamp[Test$build_year < 1860 & !is.na(Test$build_year)], 1, 4)) -
    Test$build_year[Test$build_year < 1860 & !is.na(Test$build_year)]

######################
# Impute build_year  #
######################

Train$m_build_year <- 0
Train$m_build_year[is.na(Train$build_year)] <- 1

Train$imp_build_year <- Train$build_year

Train$imp_build_year[Train$product_type == "Investment" & Train$imp_max_floor < 12.5 & is.na(Train$build_year)] <- 1970
Train$imp_build_year[Train$product_type == "Investment" & Train$imp_max_floor >= 12.5 & is.na(Train$build_year)] <- 1994
Train$imp_build_year[Train$product_type == "OwnerOccupier" & is.na(Train$build_year)] <- 2014

Train$build_year <- NULL

Test$m_build_year <- 0
Test$m_build_year[is.na(Test$build_year)] <- 1

Test$imp_build_year <- Test$build_year

Test$imp_build_year[Test$product_type == "Investment" & Test$imp_max_floor < 12.5 & is.na(Test$build_year)] <- 1970
Test$imp_build_year[Test$product_type == "Investment" & Test$imp_max_floor >= 12.5 & is.na(Test$build_year)] <- 1994
Test$imp_build_year[Test$product_type == "OwnerOccupier" & is.na(Test$build_year)] <- 2014


Test$build_year <- NULL


###############################
# Impute remaining imp_floor  #
###############################

Train$imp_floor[Train$sub_area %in% c("Beskudnikovskoe", "Brateevo", "Chertanovo Central'noe", "Chertanovo Juzhnoe",
                     "Filevskij Park", "Hovrino", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Kosino - Uhtomskoe",
                     "Krylatskoe", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe", "Mar 'ino", "Mitino", "Molzhaninovskoe",
                     "Moskvorech ' e - Saburovo", "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Novo - Peredelkino",
                     "Novokosino", "Obruchevskoe", "Ochakovo - Matveevskoe", "Poselenie Moskovskij", "Poselenie Mosrentgen",
                     "Poselenie Shherbinka", "Poselenie Sosenskoe", "Poselenie Vnukovskoe", "Poselenie Voskresenskoe",
                     "Prospekt Vernadskogo", "Ramenki", "Rostokino", "Severnoe", "Severnoe Butovo", "Silino", "Solncevo",
                     "Strogino", "Troparevo - Nikulino", "Tverskoe") & is.na(Train$imp_floor)] <- 9


Train$imp_floor[!(Train$sub_area %in% c("Beskudnikovskoe", "Brateevo", "Chertanovo Central'noe", "Chertanovo Juzhnoe",
                     "Filevskij Park", "Hovrino", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Kosino - Uhtomskoe",
                     "Krylatskoe", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe", "Mar 'ino", "Mitino", "Molzhaninovskoe",
                     "Moskvorech ' e - Saburovo", "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Novo - Peredelkino",
                     "Novokosino", "Obruchevskoe", "Ochakovo - Matveevskoe", "Poselenie Moskovskij", "Poselenie Mosrentgen",
                     "Poselenie Shherbinka", "Poselenie Sosenskoe", "Poselenie Vnukovskoe", "Poselenie Voskresenskoe",
                     "Prospekt Vernadskogo", "Ramenki", "Rostokino", "Severnoe", "Severnoe Butovo", "Silino", "Solncevo",
                     "Strogino", "Troparevo - Nikulino", "Tverskoe"))
                    & is.na(Train$imp_floor)] <- 6


Test$imp_floor[Test$sub_area %in% c("Beskudnikovskoe", "Brateevo", "Chertanovo Central'noe", "Chertanovo Juzhnoe",
                     "Filevskij Park", "Hovrino", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Kosino - Uhtomskoe",
                     "Krylatskoe", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe", "Mar 'ino", "Mitino", "Molzhaninovskoe",
                     "Moskvorech ' e - Saburovo", "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Novo - Peredelkino",
                     "Novokosino", "Obruchevskoe", "Ochakovo - Matveevskoe", "Poselenie Moskovskij", "Poselenie Mosrentgen",
                     "Poselenie Shherbinka", "Poselenie Sosenskoe", "Poselenie Vnukovskoe", "Poselenie Voskresenskoe",
                     "Prospekt Vernadskogo", "Ramenki", "Rostokino", "Severnoe", "Severnoe Butovo", "Silino", "Solncevo",
                     "Strogino", "Troparevo - Nikulino", "Tverskoe") & is.na(Test$imp_floor)] <- 9


Test$imp_floor[!(Test$sub_area %in% c("Beskudnikovskoe", "Brateevo", "Chertanovo Central'noe", "Chertanovo Juzhnoe",
                     "Filevskij Park", "Hovrino", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Kosino - Uhtomskoe",
                     "Krylatskoe", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe", "Mar 'ino", "Mitino", "Molzhaninovskoe",
                     "Moskvorech ' e - Saburovo", "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Novo - Peredelkino",
                     "Novokosino", "Obruchevskoe", "Ochakovo - Matveevskoe", "Poselenie Moskovskij", "Poselenie Mosrentgen",
                     "Poselenie Shherbinka", "Poselenie Sosenskoe", "Poselenie Vnukovskoe", "Poselenie Voskresenskoe",
                     "Prospekt Vernadskogo", "Ramenki", "Rostokino", "Severnoe", "Severnoe Butovo", "Silino", "Solncevo",
                     "Strogino", "Troparevo - Nikulino", "Tverskoe"))
                        & is.na(Test$imp_floor)] <- 6


###########################################################################################################
# Try imputing the rest of imp_build_year - can't use imp_max_floor as the same rows have missing values  #
###########################################################################################################
length(Train$imp_build_year[is.na(Train$imp_max_floor) & is.na(Train$imp_build_year)])

Train$imp_build_year[Train$product_type == "Investment" & Train$imp_kitch_sq >= 7.5 & is.na(Train$imp_build_year)] <- 1989
Train$imp_build_year[Train$product_type == "Investment" & Train$imp_kitch_sq < 7.5 & is.na(Train$imp_build_year)] <- 1970

Test$imp_build_year[Test$product_type == "Investment" & Test$imp_kitch_sq >= 7.5 & is.na(Test$imp_build_year)] <- 1989
Test$imp_build_year[Test$product_type == "Investment" & Test$imp_kitch_sq < 7.5 & is.na(Test$imp_build_year)] <- 1970


#######################################################################################
# Now that all missing values for build year are resolved create age.house variable   #
#######################################################################################

Train$age.house <- as.numeric(substring(Train$timestamp, 1, 4)) - Train$imp_build_year
Train$age.house[Train$age.house < 0] <- 0

Test$age.house <- as.numeric(substring(Test$timestamp, 1, 4)) - Test$imp_build_year
Test$age.house[Test$age.house < 0] <- 0


#####################################
# Impute the rest of imp_max_floor  #
#####################################

Train$imp_max_floor[Train$imp_build_year < 1976 & Train$imp_build_year >= 1966 & is.na(Train$imp_max_floor)] <- 10

Train$imp_max_floor[Train$imp_build_year >= 1976 & Train$sub_area %in% c("Ajeroport", "Akademicheskoe", "Altuf'evskoe", "Babushkinskoe",
                     "Basmannoe", "Beskudnikovskoe", "Birjulevo Vostochnoe", "Bogorodskoe", "Brateevo", "Butyrskoe", "Caricyno",
                     "Cheremushki", "Chertanovo Central 'noe", "Chertanovo Juzhnoe", "Chertanovo Severnoe", "Donskoe", "Filevskij Park",
                      "Fili Davydkovo", "Gagarinskoe", "Gol ' janovo", "Golovinskoe", "Horoshevo - Mnevniki", "Horoshevskoe",
                      "Hovrino", "Ivanovskoe", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Juzhnoe Tushino", "Juzhnoportovoe",
                      "Kon ' kovo", "Koptevo", "Krylatskoe", "Kuncevo", "Lefortovo", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe",
                      "Marfino", "Mitino", "Molzhaninovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe", "Nagatino - Sadovniki",
                      "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Nizhegorodskoe", "Novo - Peredelkino", "Obruchevskoe",
                      "Ochakovo - Matveevskoe", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe", "Ostankinskoe",
                      "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Poselenie Moskovskij", "Poselenie Shherbinka", "Poselenie Vnukovskoe",
                      "Preobrazhenskoe", "Presnenskoe", "Prospekt Vernadskogo", "Ramenki", "Rjazanskij", "Severnoe", "Severnoe Butovo",
                      "Severnoe Izmajlovo", "Shhukino", "Silino", "Sokol", "Sokolinaja Gora", "Solncevo", "Taganskoe", "Teplyj Stan",
                      "Timirjazevskoe", "Troparevo - Nikulino", "Veshnjaki", "Vostochnoe Degunino", "Zjablikovo", "Zjuzino")
                    & is.na(Train$imp_max_floor)] <- 16

Train$imp_max_floor[Train$imp_build_year >= 1976 & !(Train$sub_area %in% c("Ajeroport", "Akademicheskoe", "Altuf'evskoe", "Babushkinskoe",
                     "Basmannoe", "Beskudnikovskoe", "Birjulevo Vostochnoe", "Bogorodskoe", "Brateevo", "Butyrskoe", "Caricyno",
                     "Cheremushki", "Chertanovo Central 'noe", "Chertanovo Juzhnoe", "Chertanovo Severnoe", "Donskoe", "Filevskij Park",
                      "Fili Davydkovo", "Gagarinskoe", "Gol ' janovo", "Golovinskoe", "Horoshevo - Mnevniki", "Horoshevskoe",
                      "Hovrino", "Ivanovskoe", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Juzhnoe Tushino", "Juzhnoportovoe",
                      "Kon ' kovo", "Koptevo", "Krylatskoe", "Kuncevo", "Lefortovo", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe",
                      "Marfino", "Mitino", "Molzhaninovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe", "Nagatino - Sadovniki",
                      "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Nizhegorodskoe", "Novo - Peredelkino", "Obruchevskoe",
                      "Ochakovo - Matveevskoe", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe", "Ostankinskoe",
                      "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Poselenie Moskovskij", "Poselenie Shherbinka", "Poselenie Vnukovskoe",
                      "Preobrazhenskoe", "Presnenskoe", "Prospekt Vernadskogo", "Ramenki", "Rjazanskij", "Severnoe", "Severnoe Butovo",
                      "Severnoe Izmajlovo", "Shhukino", "Silino", "Sokol", "Sokolinaja Gora", "Solncevo", "Taganskoe", "Teplyj Stan",
                      "Timirjazevskoe", "Troparevo - Nikulino", "Veshnjaki", "Vostochnoe Degunino", "Zjablikovo", "Zjuzino"))
                    & is.na(Train$imp_max_floor)] <- 11

Train$imp_max_floor[Train$imp_build_year < 1976 & Train$imp_build_year < 1966 & is.na(Train$imp_max_floor)] <- 6

Test$imp_max_floor[Test$imp_build_year < 1976 & Test$imp_build_year >= 1966 & is.na(Test$imp_max_floor)] <- 10

Test$imp_max_floor[Test$imp_build_year >= 1976 & Test$sub_area %in% c("Ajeroport", "Akademicheskoe", "Altuf'evskoe", "Babushkinskoe",
                     "Basmannoe", "Beskudnikovskoe", "Birjulevo Vostochnoe", "Bogorodskoe", "Brateevo", "Butyrskoe", "Caricyno",
                     "Cheremushki", "Chertanovo Central 'noe", "Chertanovo Juzhnoe", "Chertanovo Severnoe", "Donskoe", "Filevskij Park",
                      "Fili Davydkovo", "Gagarinskoe", "Gol ' janovo", "Golovinskoe", "Horoshevo - Mnevniki", "Horoshevskoe",
                      "Hovrino", "Ivanovskoe", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Juzhnoe Tushino", "Juzhnoportovoe",
                      "Kon ' kovo", "Koptevo", "Krylatskoe", "Kuncevo", "Lefortovo", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe",
                      "Marfino", "Mitino", "Molzhaninovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe", "Nagatino - Sadovniki",
                      "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Nizhegorodskoe", "Novo - Peredelkino", "Obruchevskoe",
                      "Ochakovo - Matveevskoe", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe", "Ostankinskoe",
                      "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Poselenie Moskovskij", "Poselenie Shherbinka", "Poselenie Vnukovskoe",
                      "Preobrazhenskoe", "Presnenskoe", "Prospekt Vernadskogo", "Ramenki", "Rjazanskij", "Severnoe", "Severnoe Butovo",
                      "Severnoe Izmajlovo", "Shhukino", "Silino", "Sokol", "Sokolinaja Gora", "Solncevo", "Taganskoe", "Teplyj Stan",
                      "Timirjazevskoe", "Troparevo - Nikulino", "Veshnjaki", "Vostochnoe Degunino", "Zjablikovo", "Zjuzino")
                    & is.na(Test$imp_max_floor)] <- 16

Test$imp_max_floor[Test$imp_build_year >= 1976 & !(Test$sub_area %in% c("Ajeroport", "Akademicheskoe", "Altuf'evskoe", "Babushkinskoe",
                     "Basmannoe", "Beskudnikovskoe", "Birjulevo Vostochnoe", "Bogorodskoe", "Brateevo", "Butyrskoe", "Caricyno",
                     "Cheremushki", "Chertanovo Central 'noe", "Chertanovo Juzhnoe", "Chertanovo Severnoe", "Donskoe", "Filevskij Park",
                      "Fili Davydkovo", "Gagarinskoe", "Gol ' janovo", "Golovinskoe", "Horoshevo - Mnevniki", "Horoshevskoe",
                      "Hovrino", "Ivanovskoe", "Jakimanka", "Jasenevo", "Juzhnoe Medvedkovo", "Juzhnoe Tushino", "Juzhnoportovoe",
                      "Kon ' kovo", "Koptevo", "Krylatskoe", "Kuncevo", "Lefortovo", "Levoberezhnoe", "Lianozovo", "Lomonosovskoe",
                      "Marfino", "Mitino", "Molzhaninovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe", "Nagatino - Sadovniki",
                      "Nagatinskij Zaton", "Nagornoe", "Nekrasovka", "Nizhegorodskoe", "Novo - Peredelkino", "Obruchevskoe",
                      "Ochakovo - Matveevskoe", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe", "Ostankinskoe",
                      "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Poselenie Moskovskij", "Poselenie Shherbinka", "Poselenie Vnukovskoe",
                      "Preobrazhenskoe", "Presnenskoe", "Prospekt Vernadskogo", "Ramenki", "Rjazanskij", "Severnoe", "Severnoe Butovo",
                      "Severnoe Izmajlovo", "Shhukino", "Silino", "Sokol", "Sokolinaja Gora", "Solncevo", "Taganskoe", "Teplyj Stan",
                      "Timirjazevskoe", "Troparevo - Nikulino", "Veshnjaki", "Vostochnoe Degunino", "Zjablikovo", "Zjuzino"))
                    & is.na(Test$imp_max_floor)] <- 11

Test$imp_max_floor[Test$build_year < 1976 & Test$build_year < 1966 & is.na(Test$imp_max_floor)] <- 6

###################
# Impute num_room #
###################

Train$m_num_room <- 0
Train$m_num_room[is.na(Train$num_room)] <- 1

Train$imp_num_room <- Train$num_room

Train$imp_num_room[Train$full_sq >= 49.5 & Train$full_sq < 70.5 & is.na(Train$imp_num_room)] <- 2
Train$imp_num_room[Train$full_sq < 49.5 & Train$imp_life_sq < 21.65 & is.na(Train$imp_num_room)] <- 1
Train$imp_num_room[Train$full_sq < 49.5 & Train$imp_life_sq >= 21.65 & is.na(Train$imp_num_room)] <- 2
Train$imp_num_room[Train$full_sq >= 49.5 & Train$full_sq >= 70.5 & is.na(Train$imp_num_room)] <- 3

Train$num_room <- NULL

Test$m_num_room <- 0
Test$m_num_room[is.na(Test$num_room)] <- 1

Test$imp_num_room <- Test$num_room

Test$imp_num_room[Test$full_sq >= 49.5 & Test$full_sq < 70.5 & is.na(Test$imp_num_room)] <- 2
Test$imp_num_room[Test$full_sq < 49.5 & Test$imp_life_sq < 21.65 & is.na(Test$imp_num_room)] <- 1
Test$imp_num_room[Test$full_sq < 49.5 & Test$imp_life_sq >= 21.65 & is.na(Test$imp_num_room)] <- 2
Test$imp_num_room[Test$full_sq >= 49.5 & Test$full_sq >= 70.5 & is.na(Test$imp_num_room)] <- 3

Test$num_room <- NULL

##########################
# Impute preschool_quota #
##########################

Train$m_preschool_quota <- 0
Train$m_preschool_quota[is.na(Train$preschool_quota)] <- 1

Train$imp_preschool_quota <- Train$preschool_quota

Train$imp_preschool_quota[is.na(Train$imp_preschool_quota) & Train$sub_area %in% c("Mitino", "Vyhino-Zhulebino")] <- 7057
Train$imp_preschool_quota[is.na(Train$imp_preschool_quota) & Train$sub_area %in% c("Juzhnoe Butovo", "Mar'ino")] <- 10994
Train$imp_preschool_quota[is.na(Train$imp_preschool_quota) & Train$sub_area %in% c("Basmannoe", "Bibirevo", "Birjulevo Vostochnoe",
                     "Birjulevo Zapadnoe", "Bogorodskoe", "Brateevo", "Caricyno", "Chertanovo Juzhnoe", "Chertanovo Severnoe",
                     "Fili Davydkovo", "Gol 'janovo", "Horoshevo-Mnevniki", "Hovrino", "Jaroslavskoe", "Jasenevo", "Koptevo", "Kosino-Uhtomskoe",
                    "Krjukovo", "Krylatskoe", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe",
                    "Nagatinskij Zaton", "Novo - Peredelkino", "Novokosino", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe",
                    "Otradnoe", "Pechatniki", "Presnenskoe", "Rjazanskij", "Severnoe Butovo", "Severnoe Izmajlovo", "Severnoe Medvedkovo",
                    "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Veshnjaki",
                    "Vostochnoe Degunino", "Zjablikovo", "Zjuzino")] <- 3914
Train$imp_preschool_quota[is.na(Train$imp_preschool_quota) & !(Train$sub_area %in% c("Basmannoe", "Bibirevo", "Birjulevo Vostochnoe",
                     "Birjulevo Zapadnoe", "Bogorodskoe", "Brateevo", "Caricyno", "Chertanovo Juzhnoe", "Chertanovo Severnoe",
                     "Fili Davydkovo", "Gol 'janovo", "Horoshevo-Mnevniki", "Hovrino", "Jaroslavskoe", "Jasenevo", "Koptevo", "Kosino-Uhtomskoe",
                    "Krjukovo", "Krylatskoe", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe",
                    "Nagatinskij Zaton", "Novo - Peredelkino", "Novokosino", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe",
                    "Otradnoe", "Pechatniki", "Presnenskoe", "Rjazanskij", "Severnoe Butovo", "Severnoe Izmajlovo", "Severnoe Medvedkovo",
                    "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Veshnjaki",
                    "Vostochnoe Degunino", "Zjablikovo", "Zjuzino", "Mitino", "Vyhino-Zhulebino", "Juzhnoe Butovo", "Mar'ino"))] <- 1823

Train$preschool_quota <- NULL

Test$m_preschool_quota <- 0
Test$m_preschool_quota[is.na(Test$preschool_quota)] <- 1

Test$imp_preschool_quota <- Test$preschool_quota

Test$imp_preschool_quota[is.na(Test$imp_preschool_quota) & Test$sub_area %in% c("Mitino", "Vyhino-Zhulebino")] <- 7057
Test$imp_preschool_quota[is.na(Test$imp_preschool_quota) & Test$sub_area %in% c("Juzhnoe Butovo", "Mar'ino")] <- 10994
Test$imp_preschool_quota[is.na(Test$imp_preschool_quota) & Test$sub_area %in% c("Basmannoe", "Bibirevo", "Birjulevo Vostochnoe",
                     "Birjulevo Zapadnoe", "Bogorodskoe", "Brateevo", "Caricyno", "Chertanovo Juzhnoe", "Chertanovo Severnoe",
                     "Fili Davydkovo", "Gol 'janovo", "Horoshevo-Mnevniki", "Hovrino", "Jaroslavskoe", "Jasenevo", "Koptevo", "Kosino-Uhtomskoe",
                    "Krjukovo", "Krylatskoe", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe",
                    "Nagatinskij Zaton", "Novo - Peredelkino", "Novokosino", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe",
                    "Otradnoe", "Pechatniki", "Presnenskoe", "Rjazanskij", "Severnoe Butovo", "Severnoe Izmajlovo", "Severnoe Medvedkovo",
                    "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Veshnjaki",
                    "Vostochnoe Degunino", "Zjablikovo", "Zjuzino")] <- 3914
Test$imp_preschool_quota[is.na(Test$imp_preschool_quota) & !(Test$sub_area %in% c("Basmannoe", "Bibirevo", "Birjulevo Vostochnoe",
                     "Birjulevo Zapadnoe", "Bogorodskoe", "Brateevo", "Caricyno", "Chertanovo Juzhnoe", "Chertanovo Severnoe",
                     "Fili Davydkovo", "Gol 'janovo", "Horoshevo-Mnevniki", "Hovrino", "Jaroslavskoe", "Jasenevo", "Koptevo", "Kosino-Uhtomskoe",
                    "Krjukovo", "Krylatskoe", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe", "Moskvorech ' e - Saburovo", "Mozhajskoe",
                    "Nagatinskij Zaton", "Novo - Peredelkino", "Novokosino", "Orehovo - Borisovo Juzhnoe", "Orehovo - Borisovo Severnoe",
                    "Otradnoe", "Pechatniki", "Presnenskoe", "Rjazanskij", "Severnoe Butovo", "Severnoe Izmajlovo", "Severnoe Medvedkovo",
                    "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Veshnjaki",
                    "Vostochnoe Degunino", "Zjablikovo", "Zjuzino", "Mitino", "Vyhino-Zhulebino", "Juzhnoe Butovo", "Mar'ino"))] <- 1823

Test$preschool_quota <- NULL

##########################
# Impute school_quota #
##########################

Train$m_school_quota <- 0
Train$m_school_quota[is.na(Train$school_quota)] <- 1

Train$imp_school_quota <- Train$school_quota

Train$imp_school_quota[is.na(Train$imp_school_quota) & Train$work_female >= 5.733e+04 & Train$children_preschool < 1.358e+04] <- 16777
Train$imp_school_quota[is.na(Train$imp_school_quota) & Train$work_female >= 5.733e+04 & Train$children_preschool >= 1.358e+04] <- 23229
Train$imp_school_quota[is.na(Train$imp_school_quota) & Train$work_female < 5.733e+04 & Train$sub_area %in% c("Akademicheskoe", "Bibirevo",
                     "Birjulevo Vostochnoe", "Chertanovo Juzhnoe", "Gol 'janovo", "Hamovniki", "Horoshevo-Mnevniki", "Ivanovskoe", "Jasenevo",
                     "Kon ' kovo", "Koptevo", "Kosino - Uhtomskoe", "Krjukovo", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe",
                     "Mozhajskoe", "Nagornoe", "Novo - Peredelkino", "Orehovo - Borisovo Juzhnoe", "Otradnoe", "Presnenskoe", "Rjazanskij",
                     "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Zjablikovo")] <- 10165
Train$imp_school_quota[is.na(Train$imp_school_quota) & Train$work_female < 5.733e+04 & !(Train$sub_area %in% c("Akademicheskoe", "Bibirevo",
                     "Birjulevo Vostochnoe", "Chertanovo Juzhnoe", "Gol 'janovo", "Hamovniki", "Horoshevo-Mnevniki", "Ivanovskoe", "Jasenevo",
                     "Kon ' kovo", "Koptevo", "Kosino - Uhtomskoe", "Krjukovo", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe",
                     "Mozhajskoe", "Nagornoe", "Novo - Peredelkino", "Orehovo - Borisovo Juzhnoe", "Otradnoe", "Presnenskoe", "Rjazanskij",
                     "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Zjablikovo"))] <- 5905

Train$school_quota <- NULL


Test$m_school_quota <- 0
Test$m_school_quota[is.na(Test$school_quota)] <- 1

Test$imp_school_quota <- Test$school_quota

Test$imp_school_quota[is.na(Test$imp_school_quota) & Test$work_female >= 5.733e+04 & Test$children_preschool < 1.358e+04] <- 16777
Test$imp_school_quota[is.na(Test$imp_school_quota) & Test$work_female >= 5.733e+04 & Test$children_preschool >= 1.358e+04] <- 23229
Test$imp_school_quota[is.na(Test$imp_school_quota) & Test$work_female < 5.733e+04 & Test$sub_area %in% c("Akademicheskoe", "Bibirevo",
                     "Birjulevo Vostochnoe", "Chertanovo Juzhnoe", "Gol 'janovo", "Hamovniki", "Horoshevo-Mnevniki", "Ivanovskoe", "Jasenevo",
                     "Kon ' kovo", "Koptevo", "Kosino - Uhtomskoe", "Krjukovo", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe",
                     "Mozhajskoe", "Nagornoe", "Novo - Peredelkino", "Orehovo - Borisovo Juzhnoe", "Otradnoe", "Presnenskoe", "Rjazanskij",
                     "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Zjablikovo")] <- 10165
Test$imp_school_quota[is.na(Test$imp_school_quotaa) & Test$work_female < 5.733e+04 & !(Test$sub_area %in% c("Akademicheskoe", "Bibirevo",
                     "Birjulevo Vostochnoe", "Chertanovo Juzhnoe", "Gol 'janovo", "Hamovniki", "Horoshevo-Mnevniki", "Ivanovskoe", "Jasenevo",
                     "Kon ' kovo", "Koptevo", "Kosino - Uhtomskoe", "Krjukovo", "Kuncevo", "Kuz ' minki", "Ljublino", "Lomonosovskoe",
                     "Mozhajskoe", "Nagornoe", "Novo - Peredelkino", "Orehovo - Borisovo Juzhnoe", "Otradnoe", "Presnenskoe", "Rjazanskij",
                     "Severnoe Tushino", "Solncevo", "Strogino", "Taganskoe", "Teplyj Stan", "Troparevo - Nikulino", "Zjablikovo"))] <- 5905

Test$school_quota <- NULL

###############################################
# Impute raion_build_count_with_material_info #
###############################################

Train$m_raion_build_count_with_material_info <- 0
Train$m_raion_build_count_with_material_info[is.na(Train$raion_build_count_with_material_info)] <- 1

Train$imp_raion_build_count_with_material_info <- Train$raion_build_count_with_material_info

Train$imp_raion_build_count_with_material_info[is.na(Train$imp_raion_build_count_with_material_info) & Train$sub_area %in% c("Juzhnoe Butovo",
                                                "Kosino - Uhtomskoe", "Vnukovo")] <- 1491
Train$imp_raion_build_count_with_material_info[is.na(Train$imp_raion_build_count_with_material_info) & Train$sub_area %in% c("Basmannoe",
                         "Hamovniki", "Horoshevo - Mnevniki", "Kuncevo", "Molzhaninovskoe", "Novo - Peredelkino", "Presnenskoe", "Solncevo",
                         "Tverskoe")] <- 730
Train$imp_raion_build_count_with_material_info[is.na(Train$imp_raion_build_count_with_material_info) & Train$sub_area %in% c("Ajeroport",
                        "Akademicheskoe", "Babushkinskoe", "Bogorodskoe", "Caricyno", "Cheremushki", "Danilovskoe", "Filevskij Park",
                        "Fili Davydkovo", "Gol 'janovo", "Golovinskoe", "Horoshevskoe", "Izmajlovo", "Juzhnoe Tushino", "Kon' kovo", "Koptevo",
                        "Krjukovo", "Kurkino", "Kuz 'minki", "Lefortovo", "Lianozovo", "Ljublino", "Mar' ino", "Meshhanskoe", "Mitino", "Mozhajskoe",
                        "Novogireevo", "Ochakovo - Matveevskoe", "Otradnoe", "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Ramenki", "Rjazanskij",
                        "Severnoe", "Severnoe Izmajlovo", "Severnoe Medvedkovo", "Severnoe Tushino", "Shhukino", "Sokol", "Sokolinaja Gora",
                        "Strogino", "Taganskoe", "Tekstil 'shhiki", "Timirjazevskoe", "Vojkovskoe", "Vyhino-Zhulebino", "Zamoskvorech' e",
                        "Zjuzino")] <- 377
Train$imp_raion_build_count_with_material_info[is.na(Train$imp_raion_build_count_with_material_info) & !(Train$sub_area %in% c("Ajeroport",
                        "Akademicheskoe", "Babushkinskoe", "Bogorodskoe", "Caricyno", "Cheremushki", "Danilovskoe", "Filevskij Park",
                        "Fili Davydkovo", "Gol 'janovo", "Golovinskoe", "Horoshevskoe", "Izmajlovo", "Juzhnoe Tushino", "Kon' kovo", "Koptevo",
                        "Krjukovo", "Kurkino", "Kuz 'minki", "Lefortovo", "Lianozovo", "Ljublino", "Mar' ino", "Meshhanskoe", "Mitino", "Mozhajskoe",
                        "Novogireevo", "Ochakovo - Matveevskoe", "Otradnoe", "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Ramenki", "Rjazanskij",
                        "Severnoe", "Severnoe Izmajlovo", "Severnoe Medvedkovo", "Severnoe Tushino", "Shhukino", "Sokol", "Sokolinaja Gora",
                        "Strogino", "Taganskoe", "Tekstil 'shhiki", "Timirjazevskoe", "Vojkovskoe", "Vyhino-Zhulebino", "Zamoskvorech' e",
                        "Zjuzino", "Basmannoe", "Hamovniki", "Horoshevo - Mnevniki", "Kuncevo", "Molzhaninovskoe", "Novo - Peredelkino", "Presnenskoe",
                        "Solncevo", "Tverskoe", "Juzhnoe Butovo", "Kosino - Uhtomskoe", "Vnukovo"))] <- 145

Train$raion_build_count_with_material_info <- NULL


Test$m_raion_build_count_with_material_info <- 0
Test$m_raion_build_count_with_material_info[is.na(Test$raion_build_count_with_material_info)] <- 1

Test$imp_raion_build_count_with_material_info <- Test$raion_build_count_with_material_info

Test$imp_raion_build_count_with_material_info[is.na(Test$imp_raion_build_count_with_material_info) & Test$sub_area %in% c("Juzhnoe Butovo",
                                                "Kosino - Uhtomskoe", "Vnukovo")] <- 1491
Test$imp_raion_build_count_with_material_info[is.na(Test$imp_raion_build_count_with_material_info) & Test$sub_area %in% c("Basmannoe",
                         "Hamovniki", "Horoshevo - Mnevniki", "Kuncevo", "Molzhaninovskoe", "Novo - Peredelkino", "Presnenskoe", "Solncevo",
                         "Tverskoe")] <- 730
Test$imp_raion_build_count_with_material_info[is.na(Test$imp_raion_build_count_with_material_info) & Test$sub_area %in% c("Ajeroport",
                        "Akademicheskoe", "Babushkinskoe", "Bogorodskoe", "Caricyno", "Cheremushki", "Danilovskoe", "Filevskij Park",
                        "Fili Davydkovo", "Gol 'janovo", "Golovinskoe", "Horoshevskoe", "Izmajlovo", "Juzhnoe Tushino", "Kon' kovo", "Koptevo",
                        "Krjukovo", "Kurkino", "Kuz 'minki", "Lefortovo", "Lianozovo", "Ljublino", "Mar' ino", "Meshhanskoe", "Mitino", "Mozhajskoe",
                        "Novogireevo", "Ochakovo - Matveevskoe", "Otradnoe", "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Ramenki", "Rjazanskij",
                        "Severnoe", "Severnoe Izmajlovo", "Severnoe Medvedkovo", "Severnoe Tushino", "Shhukino", "Sokol", "Sokolinaja Gora",
                        "Strogino", "Taganskoe", "Tekstil 'shhiki", "Timirjazevskoe", "Vojkovskoe", "Vyhino-Zhulebino", "Zamoskvorech' e",
                        "Zjuzino")] <- 377
Test$imp_raion_build_count_with_material_info[is.na(Test$imp_raion_build_count_with_material_info) & !(Test$sub_area %in% c("Ajeroport",
                        "Akademicheskoe", "Babushkinskoe", "Bogorodskoe", "Caricyno", "Cheremushki", "Danilovskoe", "Filevskij Park",
                        "Fili Davydkovo", "Gol 'janovo", "Golovinskoe", "Horoshevskoe", "Izmajlovo", "Juzhnoe Tushino", "Kon' kovo", "Koptevo",
                        "Krjukovo", "Kurkino", "Kuz 'minki", "Lefortovo", "Lianozovo", "Ljublino", "Mar' ino", "Meshhanskoe", "Mitino", "Mozhajskoe",
                        "Novogireevo", "Ochakovo - Matveevskoe", "Otradnoe", "Pechatniki", "Perovo", "Pokrovskoe Streshnevo", "Ramenki", "Rjazanskij",
                        "Severnoe", "Severnoe Izmajlovo", "Severnoe Medvedkovo", "Severnoe Tushino", "Shhukino", "Sokol", "Sokolinaja Gora",
                        "Strogino", "Taganskoe", "Tekstil 'shhiki", "Timirjazevskoe", "Vojkovskoe", "Vyhino-Zhulebino", "Zamoskvorech' e",
                        "Zjuzino", "Basmannoe", "Hamovniki", "Horoshevo - Mnevniki", "Kuncevo", "Molzhaninovskoe", "Novo - Peredelkino", "Presnenskoe",
                        "Solncevo", "Tverskoe", "Juzhnoe Butovo", "Kosino - Uhtomskoe", "Vnukovo"))] <- 145

Test$raion_build_count_with_material_info <- NULL

############################
# Impute build_count_block #
############################

Train$m_build_count_block <- 0
Train$m_build_count_block[is.na(Train$build_count_block)] <- 1

Train$imp_build_count_block <- Train$build_count_block

Train$imp_build_count_block[Train$imp_raion_build_count_with_material_info >= 211.5 & Train$shopping_centers_raion < 8.5
                                & is.na(Train$imp_build_count_block)] <- 77
Train$imp_build_count_block[Train$imp_raion_build_count_with_material_info < 211.5 & Train$workplaces_km < 4.33
                                & is.na(Train$imp_build_count_block)] <- 28
Train$imp_build_count_block[Train$imp_raion_build_count_with_material_info >= 211.5 & Train$shopping_centers_raion >= 8.5
                                & is.na(Train$imp_build_count_block)] <- 27
Train$imp_build_count_block[Train$imp_raion_build_count_with_material_info < 211.5 & Train$workplaces_km >= 4.33
                                & is.na(Train$imp_build_count_block)] <- 2
Train$build_count_block <- NULL


Test$m_build_count_block <- 0
Test$m_build_count_block[is.na(Test$build_count_block)] <- 1

Test$imp_build_count_block <- Test$build_count_block

Test$imp_build_count_block[Test$raion_build_count_with_material_info >= 211.5 & Test$shopping_centers_raion < 8.5
                                & is.na(Test$imp_build_count_block)] <- 77
Test$imp_build_count_block[Test$raion_build_count_with_material_info < 211.5 & Test$workplaces_km < 4.33
                                & is.na(Test$imp_build_count_block)] <- 28
Test$imp_build_count_block[Test$raion_build_count_with_material_info >= 211.5 & Test$shopping_centers_raion >= 8.5
                                & is.na(Test$imp_build_count_block)] <- 27
Test$imp_build_count_block[Test$raion_build_count_with_material_info < 211.5 & Test$workplaces_km >= 4.33
                                & is.na(Test$imp_build_count_block)] <- 2

Test$build_count_block <- NULL

############################
# Impute build_count_wood  #
############################

Train$m_build_count_wood <- 0
Train$m_build_count_wood[is.na(Train$build_count_wood)] <- 1

Train$imp_build_count_wood <- Train$build_count_wood

Train$imp_build_count_wood[Train$imp_raion_build_count_with_material_info < 694 & is.na(Train$imp_build_count_wood)] <- 11
Train$imp_build_count_wood[Train$imp_raion_build_count_with_material_info < 851 & Train$imp_raion_build_count_with_material_info >= 694
                                & is.na(Train$imp_build_count_wood)] <- 213
Train$imp_build_count_wood[Train$imp_raion_build_count_with_material_info >= 851 & Train$preschool_quota >= 7887
                                & is.na(Train$imp_build_count_wood)] <- 607
Train$imp_build_count_wood[Train$imp_raion_build_count_with_material_info >= 851 & Train$preschool_quota < 7887
                                & is.na(Train$imp_build_count_wood)] <- 790
Train$build_count_wood <- NULL

Test$m_build_count_wood <- 0
Test$m_build_count_wood[is.na(Test$build_count_wood)] <- 1

Test$imp_build_count_wood <- Test$build_count_wood

Test$imp_build_count_wood[Test$raion_build_count_with_material_info < 694 & is.na(Test$imp_build_count_wood)] <- 11
Test$imp_build_count_wood[Test$raion_build_count_with_material_info < 851 & Test$raion_build_count_with_material_info >= 694
                                & is.na(Test$imp_build_count_wood)] <- 213
Test$imp_build_count_wood[Test$raion_build_count_with_material_info >= 851 & Test$preschool_quota >= 7887
                                & is.na(Test$imp_build_count_wood)] <- 607
Test$imp_build_count_wood[Test$raion_build_count_with_material_info >= 851 & Test$preschool_quota < 7887
                                & is.na(Test$imp_build_count_wood)] <- 790
Test$build_count_wood <- NULL

############################
# Impute build_count_frame #
############################

Train$m_build_count_frame <- 0
Train$m_build_count_frame[is.na(Train$build_count_frame)] <- 1

Train$imp_build_count_frame <- Train$build_count_frame

Train$imp_build_count_frame[Train$imp_build_count_wood < 40.5 & is.na(Train$imp_build_count_frame)] <- 0
Train$imp_build_count_frame[Train$imp_build_count_wood >= 98 & Train$imp_school_quota >= 1.011e+04
                                & is.na(Train$imp_build_count_frame)] <- 24
Train$imp_build_count_frame[Train$imp_build_count_wood >= 98 & Train$imp_school_quota < 1.011e+04
                                & is.na(Train$imp_build_count_frame)] <- 67
Train$imp_build_count_frame[Train$imp_build_count_wood >= 40.5 & is.na(Train$imp_build_count_frame)] <- 14
Train$build_count_frame <- NULL

Test$m_build_count_frame <- 0
Test$m_build_count_frame[is.na(Test$build_count_frame)] <- 1

Test$imp_build_count_frame <- Test$build_count_frame

Test$imp_build_count_frame[Test$build_count_wood < 40.5 & is.na(Test$imp_build_count_frame)] <- 0
Test$imp_build_count_frame[Test$build_count_wood >= 98 & Test$school_quota >= 1.011e+04
                                & is.na(Test$imp_build_count_frame)] <- 24
Test$imp_build_count_frame[Test$build_count_wood >= 98 & Test$school_quota < 1.011e+04
                                & is.na(Test$imp_build_count_frame)] <- 67
Test$imp_build_count_frame[Test$build_count_wood >= 40.5 & is.na(Test$imp_build_count_frame)] <- 14
Test$build_count_frame <- NULL

############################
# Impute build_count_brick #
############################

Train$m_build_count_brick <- 0
Train$m_build_count_brick[is.na(Train$build_count_brick)] <- 1

Train$imp_build_count_brick <- Train$build_count_brick

Train$imp_build_count_brick[Train$office_raion < 78.5 & Train$imp_raion_build_count_with_material_info < 319.5 &
                             is.na(Train$imp_build_count_brick)] <- 45
Train$imp_build_count_brick[Train$office_raion < 78.5 & Train$imp_raion_build_count_with_material_info >= 319.5
                                & is.na(Train$imp_build_count_brick)] <- 24
Train$imp_build_count_brick[Train$office_raion >= 78.5 & is.na(Train$imp_build_count_brick)] <- 67
Train$build_count_brick <- NULL

Test$m_build_count_brick <- 0
Test$m_build_count_brick[is.na(Test$build_count_brick)] <- 1

Test$imp_build_count_brick <- Test$build_count_brick

Test$imp_build_count_brick[Test$office_raion < 78.5 & Test$imp_raion_build_count_with_material_info < 319.5 &
                             is.na(Test$imp_build_count_brick)] <- 45
Test$imp_build_count_brick[Test$office_raion < 78.5 & Test$imp_raion_build_count_with_material_info >= 319.5
                                & is.na(Test$imp_build_count_brick)] <- 24
Test$imp_build_count_brick[Test$office_raion >= 78.5 & is.na(Test$imp_build_count_brick)] <- 67
Test$build_count_brick <- NULL

###############################
# Impute build_count_monolith #
###############################

Train$m_build_count_monolith <- 0
Train$m_build_count_monolith[is.na(Train$build_count_monolith)] <- 1

Train$imp_build_count_monolith <- Train$build_count_monolith

Train$imp_build_count_monolith[Train$imp_raion_build_count_with_material_info < 1457 & Train$sport_objects_raion < 16.5
                                & is.na(Train$imp_build_count_monolith)] <- 8
Train$imp_build_count_monolith[Train$imp_raion_build_count_with_material_info < 1457 & Train$sport_objects_raion >= 16.5
                                & is.na(Train$imp_build_count_monolith)] <- 33
Train$imp_build_count_monolith[Train$imp_raion_build_count_with_material_info >= 1457 & is.na(Train$imp_build_count_monolith)] <- 116
Train$build_count_monolith <- NULL

Test$m_build_count_monolith <- 0
Test$m_build_count_monolith[is.na(Test$build_count_monolith)] <- 1

Test$imp_build_count_monolith <- Test$build_count_monolith

Test$imp_build_count_monolith[Test$imp_raion_build_count_with_material_info < 1457 & Test$sport_objects_raion < 16.5
                                & is.na(Test$imp_build_count_monolith)] <- 8
Test$imp_build_count_monolith[Test$imp_raion_build_count_with_material_info < 1457 & Test$sport_objects_raion >= 16.5
                                & is.na(Test$imp_build_count_monolith)] <- 33
Test$imp_build_count_monolith[Test$imp_raion_build_count_with_material_info >= 1457 & is.na(Test$imp_build_count_monolith)] <- 116
Test$build_count_monolith <- NULL


###############################
# Impute build_count_panel #
###############################

Train$m_build_count_panel <- 0
Train$m_build_count_panel[is.na(Train$build_count_panel)] <- 1

Train$imp_build_count_panel <- Train$build_count_panel

Train$imp_build_count_panel[Train$work_all < 8.412e+04 & Train$work_female >= 2.225e+04
                                & is.na(Train$imp_build_count_panel)] <- 96
Train$imp_build_count_panel[Train$work_all < 8.412e+04 & Train$work_female < 2.225e+04
                                & is.na(Train$imp_build_count_panel)] <- 26
Train$imp_build_count_panel[Train$work_all >= 8.412e+04 & Train$work_all < 1.209e+05
                                & is.na(Train$imp_build_count_panel)] <- 194
Train$imp_build_count_panel[Train$work_all >= 8.412e+04 & Train$work_all >= 1.209e+05
                                & is.na(Train$imp_build_count_panel)] <- 364
Train$build_count_panel <- NULL

Test$m_build_count_panel <- 0
Test$m_build_count_panel[is.na(Test$build_count_panel)] <- 1

Test$imp_build_count_panel <- Test$build_count_panel

Test$imp_build_count_panel[Test$work_all < 8.412e+04 & Test$work_female >= 2.225e+04
                                & is.na(Test$imp_build_count_panel)] <- 96
Test$imp_build_count_panel[Test$work_all < 8.412e+04 & Test$work_female < 2.225e+04
                                & is.na(Test$imp_build_count_panel)] <- 26
Test$imp_build_count_panel[Test$work_all >= 8.412e+04 & Test$work_all < 1.209e+05
                                & is.na(Test$imp_build_count_panel)] <- 194
Test$imp_build_count_panel[Test$work_all >= 8.412e+04 & Test$work_all >= 1.209e+05
                                & is.na(Test$imp_build_count_panel)] <- 364
Test$build_count_panel <- NULL

###############################
# Impute build_count_foam     #
###############################

Train$m_build_count_foam <- 0
Train$m_build_count_foam[is.na(Train$build_count_foam)] <- 1

Train$imp_build_count_foam <- Train$build_count_foam

Train$imp_build_count_foam[Train$imp_build_count_wood >= 98 & Train$imp_build_count_frame >= 22
                                & is.na(Train$imp_build_count_foam)] <- 0
Train$imp_build_count_foam[Train$imp_build_count_wood >= 98 & Train$imp_build_count_frame < 22
                                & is.na(Train$imp_build_count_foam)] <- 1
Train$imp_build_count_foam[Train$imp_build_count_wood < 98 & is.na(Train$imp_build_count_foam)] <- 0

Train$build_count_foam <- NULL

Test$m_build_count_foam <- 0
Test$m_build_count_foam[is.na(Test$build_count_foam)] <- 1

Test$imp_build_count_foam <- Test$build_count_foam

Test$imp_build_count_foam[Test$imp_build_count_wood >= 98 & Test$imp_build_count_frame >= 22
                                & is.na(Test$imp_build_count_foam)] <- 0
Test$imp_build_count_foam[Test$imp_build_count_wood >= 98 & Test$imp_build_count_frame < 22
                                & is.na(Test$imp_build_count_foam)] <- 1
Test$imp_build_count_foam[Test$imp_build_count_wood < 98 & is.na(Test$imp_build_count_foam)] <- 0

Test$build_count_foam <- NULL

###############################
# Impute build_count_slag     #
###############################

Train$m_build_count_slag <- 0
Train$m_build_count_slag[is.na(Train$build_count_slag)] <- 1

Train$imp_build_count_slag <- Train$build_count_slag

Train$imp_build_count_slag[Train$imp_raion_build_count_with_material_info < 851 & Train$imp_build_count_wood < 107.5
                                & is.na(Train$imp_build_count_slag)] <- 1
Train$imp_build_count_slag[Train$imp_raion_build_count_with_material_info < 851 & Train$imp_build_count_wood >= 107.5
                                & is.na(Train$imp_build_count_slag)] <- 16
Train$imp_build_count_slag[Train$imp_raion_build_count_with_material_info >= 85 & Train$area_m >= 2.099e+07
                                & is.na(Train$imp_build_count_slag)] <- 84
Train$imp_build_count_slag[Train$imp_raion_build_count_with_material_info >= 85 & Train$area_m < 2.099e+07
                                & is.na(Train$imp_build_count_slag)] <- 60

Train$build_count_slag <- NULL

Test$m_build_count_slag <- 0
Test$m_build_count_slag[is.na(Test$build_count_slag)] <- 1

Test$imp_build_count_slag <- Test$build_count_slag

Test$imp_build_count_slag[Test$imp_raion_build_count_with_material_info < 851 & Test$imp_build_count_wood < 107.5
                                & is.na(Test$imp_build_count_slag)] <- 1
Test$imp_build_count_slag[Test$imp_raion_build_count_with_material_info < 851 & Test$imp_build_count_wood >= 107.5
                                & is.na(Test$imp_build_count_slag)] <- 16
Test$imp_build_count_slag[Test$imp_raion_build_count_with_material_info >= 85 & Test$area_m >= 2.099e+07
                                & is.na(Test$imp_build_count_slag)] <- 84
Test$imp_build_count_slag[Test$imp_raion_build_count_with_material_info >= 85 & Test$area_m < 2.099e+07
                                & is.na(Test$imp_build_count_slag)] <- 60

Test$build_count_slag <- NULL

###############################
# Impute build_count_mix      #
###############################

Train$m_build_count_mix <- 0
Train$m_build_count_mix[is.na(Train$build_count_mix)] <- 1

Train$imp_build_count_mix <- Train$build_count_mix

Train$imp_build_count_mix[Train$imp_build_count_wood >= 15.5 & Train$imp_build_count_slag < 16.5
                                & is.na(Train$imp_build_count_mix)] <- 1
Train$imp_build_count_mix[Train$imp_build_count_wood >= 15.5 & Train$imp_build_count_slag >= 16.5
                                & is.na(Train$imp_build_count_mix)] <- 5
Train$imp_build_count_mix[Train$imp_build_count_wood < 15.5 & Train$sport_objects_raion >= 16.5
                                & is.na(Train$imp_build_count_mix)] <- 1
Train$imp_build_count_mix[Train$imp_build_count_wood < 15.5 & Train$sport_objects_raion < 16.5
                                & is.na(Train$imp_build_count_mix)] <- 0
Train$build_count_mix <- NULL

Test$m_build_count_mix <- 0
Test$m_build_count_mix[is.na(Test$build_count_mix)] <- 1

Test$imp_build_count_mix <- Test$build_count_mix

Test$imp_build_count_mix[Test$imp_build_count_wood >= 15.5 & Test$imp_build_count_slag < 16.5
                                & is.na(Test$imp_build_count_mix)] <- 1
Test$imp_build_count_mix[Test$imp_build_count_wood >= 15.5 & Test$imp_build_count_slag >= 16.5
                                & is.na(Test$imp_build_count_mix)] <- 5
Test$imp_build_count_mix[Test$imp_build_count_wood < 15.5 & Test$sport_objects_raion >= 16.5
                                & is.na(Test$imp_build_count_mix)] <- 1
Test$imp_build_count_mix[Test$imp_build_count_wood < 15.5 & Test$sport_objects_raion < 16.5
                                & is.na(Test$imp_build_count_mix)] <- 0

Test$build_count_mix <- NULL


####################################################
# Impute raion_build_count_with_builddate_info     #
####################################################

Train$m_raion_build_count_with_builddate_info <- 0
Train$m_raion_build_count_with_builddate_info[is.na(Train$raion_build_count_with_builddate_info)] <- 1

Train$imp_raion_build_count_with_builddate_info <- Train$raion_build_count_with_builddate_info

Train$imp_raion_build_count_with_builddate_info[Train$imp_raion_build_count_with_material_info < 604
                                & Train$imp_raion_build_count_with_material_info < 264.5
                                & is.na(Train$imp_raion_build_count_with_builddate_info)] <- 146
Train$imp_raion_build_count_with_builddate_info[Train$imp_raion_build_count_with_material_info < 604
                                & Train$imp_raion_build_count_with_material_info >= 264.5
                                & is.na(Train$imp_raion_build_count_with_builddate_info)] <- 379
Train$imp_raion_build_count_with_builddate_info[Train$imp_raion_build_count_with_material_info >= 604
                                & Train$imp_raion_build_count_with_material_info < 1035
                                & is.na(Train$imp_raion_build_count_with_builddate_info)] <- 729
Train$imp_raion_build_count_with_builddate_info[Train$imp_raion_build_count_with_material_info >= 604
                                & Train$imp_raion_build_count_with_material_info >= 1035
                                & is.na(Train$imp_raion_build_count_with_builddate_info)] <- 1490
Train$raion_build_count_with_builddate_info <- NULL

Test$m_raion_build_count_with_builddate_info <- 0
Test$m_raion_build_count_with_builddate_info[is.na(Test$raion_build_count_with_builddate_info)] <- 1

Test$imp_raion_build_count_with_builddate_info <- Test$raion_build_count_with_builddate_info

Test$imp_raion_build_count_with_builddate_info[Test$imp_raion_build_count_with_material_info < 604
                                & Test$imp_raion_build_count_with_material_info < 264.5
                                & is.na(Test$imp_raion_build_count_with_builddate_info)] <- 146
Test$imp_raion_build_count_with_builddate_info[Test$imp_raion_build_count_with_material_info < 604
                                & Test$imp_raion_build_count_with_material_info >= 264.5
                                & is.na(Test$imp_raion_build_count_with_builddate_info)] <- 379
Test$imp_raion_build_count_with_builddate_info[Test$imp_raion_build_count_with_material_info >= 604
                                & Test$imp_raion_build_count_with_material_info < 1035
                                & is.na(Test$imp_raion_build_count_with_builddate_info)] <- 729
Test$imp_raion_build_count_with_builddate_info[Test$imp_raion_build_count_with_material_info >= 604
                                & Test$imp_raion_build_count_with_material_info >= 1035
                                & is.na(Test$imp_raion_build_count_with_builddate_info)] <- 1490
Test$raion_build_count_with_builddate_info <- NULL

######################################
# Impute build_count_before_1920     #
######################################

Train$m_build_count_before_1920 <- 0
Train$m_build_count_before_1920[is.na(Train$build_count_before_1920)] <- 1

Train$imp_build_count_before_1920 <- Train$build_count_before_1920

Train$imp_build_count_before_1920[Train$office_raion < 57.5 & Train$imp_build_count_wood < 783.5
                                & is.na(Train$imp_build_count_before_1920)] <- 4
Train$imp_build_count_before_1920[Train$office_raion < 57.5 & Train$imp_build_count_wood >= 783.5
                                & is.na(Train$imp_build_count_before_1920)] <- 298
Train$imp_build_count_before_1920[Train$office_raion >= 57.5 & Train$sport_objects_raion >= 24
                                & is.na(Train$imp_build_count_before_1920)] <- 269
Train$imp_build_count_before_1920[Train$office_raion >= 57.5 & Train$sport_objects_raion < 24
                                & is.na(Train$imp_build_count_before_1920)] <- 175
Train$build_count_before_1920 <- NULL

Test$m_build_count_before_1920 <- 0
Test$m_build_count_before_1920[is.na(Test$build_count_before_1920)] <- 1

Test$imp_build_count_before_1920 <- Test$build_count_before_1920

Test$imp_build_count_before_1920[Test$office_raion < 57.5 & Test$imp_build_count_wood < 783.5
                                & is.na(Test$imp_build_count_before_1920)] <- 4
Test$imp_build_count_before_1920[Test$office_raion < 57.5 & Test$imp_build_count_wood >= 783.5
                                & is.na(Test$imp_build_count_before_1920)] <- 298
Test$imp_build_count_before_1920[Test$office_raion >= 57.5 & Test$sport_objects_raion >= 24
                                & is.na(Test$imp_build_count_before_1920)] <- 269
Test$imp_build_count_before_1920[Test$office_raion >= 57.5 & Test$sport_objects_raion < 24
                                & is.na(Test$imp_build_count_before_1920)] <- 175
Test$build_count_before_1920 <- NULL

####################################
# Impute build_count_1921.1945     #
####################################

Train$m_build_count_1921.1945 <- 0
Train$m_build_count_1921.1945[is.na(Train$build_count_1921.1945)] <- 1

Train$imp_build_count_1921.1945 <- Train$build_count_1921.1945

Train$imp_build_count_1921.1945[Train$imp_build_count_wood < 278 & Train$imp_build_count_before_1920 < 13.5
                                & is.na(Train$imp_build_count_1921.1945)] <- 7
Train$imp_build_count_1921.1945[Train$imp_build_count_wood < 278 & Train$imp_build_count_before_1920 >= 13.5
                                & is.na(Train$imp_build_count_1921.1945)] <- 78
Train$imp_build_count_1921.1945[Train$imp_build_count_wood >= 278 & Train$imp_build_count_slag >= 49.5
                                & is.na(Train$imp_build_count_1921.1945)] <- 329
Train$imp_build_count_1921.1945[Train$imp_build_count_wood >= 278 & Train$imp_build_count_slag < 49.5
                                & is.na(Train$imp_build_count_1921.1945)] <- 182
Train$build_count_1921.1945 <- NULL

Test$m_build_count_1921.1945 <- 0
Test$m_build_count_1921.1945[is.na(Test$build_count_1921.1945)] <- 1

Test$imp_build_count_1921.1945 <- Test$build_count_1921.1945

Test$imp_build_count_1921.1945[Test$imp_build_count_wood < 278 & Test$imp_build_count_before_1920 < 13.5
                                & is.na(Test$imp_build_count_1921.1945)] <- 7
Test$imp_build_count_1921.1945[Test$imp_build_count_wood < 278 & Test$imp_build_count_before_1920 >= 13.5
                                & is.na(Test$imp_build_count_1921.1945)] <- 78
Test$imp_build_count_1921.1945[Test$imp_build_count_wood >= 278 & Test$imp_build_count_slag >= 49.5
                                & is.na(Test$imp_build_count_1921.1945)] <- 329
Test$imp_build_count_1921.1945[Test$imp_build_count_wood >= 278 & Test$imp_build_count_slag < 49.5
                                & is.na(Test$imp_build_count_1921.1945)] <- 182
Test$build_count_1921.1945 <- NULL


####################################
# Impute build_count_1946.1970     #
####################################

Train$m_build_count_1946.1970 <- 0
Train$m_build_count_1946.1970[is.na(Train$build_count_1946.1970)] <- 1

Train$imp_build_count_1946.1970 <- Train$build_count_1946.1970

Train$imp_build_count_1946.1970[Train$imp_build_count_brick >= 27 & Train$imp_build_count_block < 84
                                & is.na(Train$imp_build_count_1946.1970)] <- 164
Train$imp_build_count_1946.1970[Train$imp_build_count_brick >= 27 & Train$imp_build_count_block >= 84
                                & is.na(Train$imp_build_count_1946.1970)] <- 318
Train$imp_build_count_1946.1970[Train$imp_build_count_brick < 27 & Train$imp_build_count_block < 42.5
                                & is.na(Train$imp_build_count_1946.1970)] <- 13
Train$imp_build_count_1946.1970[Train$imp_build_count_brick < 27 & Train$imp_build_count_block >= 42.5
                                & is.na(Train$imp_build_count_1946.1970)] <- 99
Train$build_count_1946.1970 <- NULL

Test$m_build_count_1946.1970 <- 0
Test$m_build_count_1946.1970[is.na(Test$build_count_1946.1970)] <- 1

Test$imp_build_count_1946.1970 <- Test$build_count_1946.1970

Test$imp_build_count_1946.1970[Test$imp_build_count_brick >= 27 & Test$imp_build_count_block < 84
                                & is.na(Test$imp_build_count_1946.1970)] <- 164
Test$imp_build_count_1946.1970[Test$imp_build_count_brick >= 27 & Test$imp_build_count_block >= 84
                                & is.na(Test$imp_build_count_1946.1970)] <- 318
Test$imp_build_count_1946.1970[Test$imp_build_count_brick < 27 & Test$imp_build_count_block < 42.5
                                & is.na(Test$imp_build_count_1946.1970)] <- 13
Test$imp_build_count_1946.1970[Test$imp_build_count_brick < 27 & Test$imp_build_count_block >= 42.5
                                & is.na(Test$imp_build_count_1946.1970)] <- 99
Test$build_count_1946.1970 <- NULL


####################################
# Impute build_count_1971.1995     #
####################################

Train$m_build_count_1971.1995 <- 0
Train$m_build_count_1971.1995[is.na(Train$build_count_1971.1995)] <- 1

Train$imp_build_count_1971.1995 <- Train$build_count_1971.1995

Train$imp_build_count_1971.1995[Train$work_female < 3.686e+04 & Train$raion_popul >= 7.426e+04
                                & is.na(Train$imp_build_count_1971.1995)] <- 70
Train$imp_build_count_1971.1995[Train$work_female < 3.686e+04 & Train$raion_popul < 7.426e+04
                                & is.na(Train$imp_build_count_1971.1995)] <- 19
Train$imp_build_count_1971.1995[Train$work_female >= 3.686e+04 & Train$ekder_all < 4.195e+04
                                & is.na(Train$imp_build_count_1971.1995)] <- 130
Train$imp_build_count_1971.1995[Train$work_female >= 3.686e+04 & Train$ekder_all >= 4.195e+04
                                & is.na(Train$imp_build_count_1971.1995)] <- 197
Train$build_count_1971.1995 <- NULL

Test$m_build_count_1971.1995 <- 0
Test$m_build_count_1971.1995[is.na(Test$build_count_1971.1995)] <- 1

Test$imp_build_count_1971.1995 <- Test$build_count_1971.1995

Test$imp_build_count_1971.1995[Test$work_female < 3.686e+04 & Test$raion_popul >= 7.426e+04
                                & is.na(Test$imp_build_count_1971.1995)] <- 70
Test$imp_build_count_1971.1995[Test$work_female < 3.686e+04 & Test$raion_popul < 7.426e+04
                                & is.na(Test$imp_build_count_1971.1995)] <- 19
Test$imp_build_count_1971.1995[Test$work_female >= 3.686e+04 & Test$ekder_all < 4.195e+04
                                & is.na(Test$imp_build_count_1971.1995)] <- 130
Test$imp_build_count_1971.1995[Test$work_female >= 3.686e+04 & Test$ekder_all >= 4.195e+04
                                & is.na(Test$imp_build_count_1971.1995)] <- 197
Test$build_count_1971.1995 <- NULL

####################################
# Impute build_count_after_1995    #
####################################

Train$m_build_count_after_1995 <- 0
Train$m_build_count_after_1995[is.na(Train$build_count_after_1995)] <- 1

Train$imp_build_count_after_1995 <- Train$build_count_after_1995

Train$imp_build_count_after_1995[Train$imp_build_count_slag < 74 & Train$imp_build_count_frame < 11.5
                                & is.na(Train$imp_build_count_after_1995)] <- 33
Train$imp_build_count_after_1995[Train$imp_build_count_slag < 74 & Train$imp_build_count_frame >= 11.5
                                & is.na(Train$imp_build_count_after_1995)] <- 174
Train$imp_build_count_after_1995[Train$imp_build_count_slag >= 74 & is.na(Train$imp_build_count_after_1995)] <- 799

Train$build_count_after_1995 <- NULL

Test$m_build_count_after_1995 <- 0
Test$m_build_count_after_1995[is.na(Test$build_count_after_1995)] <- 1

Test$imp_build_count_after_1995 <- Test$build_count_after_1995

Test$imp_build_count_after_1995[Test$imp_build_count_slag < 74 & Test$imp_build_count_frame < 11.5
                                & is.na(Test$imp_build_count_after_1995)] <- 33
Test$imp_build_count_after_1995[Test$imp_build_count_slag < 74 & Test$imp_build_count_frame >= 11.5
                                & is.na(Test$imp_build_count_after_1995)] <- 174
Test$imp_build_count_after_1995[Test$imp_build_count_slag >= 74 & is.na(Test$imp_build_count_after_1995)] <- 799

Test$build_count_after_1995 <- NULL

############################
# Impute metro_min_walk    #
############################

Train$m_metro_min_walk <- 0
Train$m_metro_min_walk[is.na(Train$metro_min_walk)] <- 1

Train$imp_metro_min_walk <- Train$metro_min_walk

Train$imp_metro_min_walk[Train$thermal_power_plant_km < 15.2 & Train$metro_km_avto < 3.33
                                & is.na(Train$imp_metro_min_walk)] <- 16
Train$imp_metro_min_walk[Train$thermal_power_plant_km < 15.2 & Train$metro_km_avto >= 3.33
                                & is.na(Train$imp_metro_min_walk)] <- 62
Train$imp_metro_min_walk[Train$thermal_power_plant_km >= 15.2 & Train$nuclear_reactor_km < 35.62
                                & is.na(Train$imp_metro_min_walk)] <- 216
Train$imp_metro_min_walk[Train$thermal_power_plant_km >= 15.2 & Train$nuclear_reactor_km >= 35.62
                                & is.na(Train$imp_metro_min_walk)] <- 558
Train$metro_min_walk <- NULL

Test$m_metro_min_walk <- 0
Test$m_metro_min_walk[is.na(Test$metro_min_walk)] <- 1

Test$imp_metro_min_walk <- Test$metro_min_walk

Test$imp_metro_min_walk[Test$thermal_power_plant_km < 15.2 & Test$metro_km_avto < 3.33
                                & is.na(Test$imp_metro_min_walk)] <- 16
Test$imp_metro_min_walk[Test$thermal_power_plant_km < 15.2 & Test$metro_km_avto >= 3.33
                                & is.na(Test$imp_metro_min_walk)] <- 62
Test$imp_metro_min_walk[Test$thermal_power_plant_km >= 15.2 & Test$nuclear_reactor_km < 35.62
                                & is.na(Test$imp_metro_min_walk)] <- 216
Test$imp_metro_min_walk[Test$thermal_power_plant_km >= 15.2 & Test$nuclear_reactor_km >= 35.62
                                & is.na(Test$imp_metro_min_walk)] <- 558
Test$metro_min_walk <- NULL

############################
# Impute metro_km_walk    #
############################

Train$m_metro_km_walk <- 0
Train$m_metro_km_walk[is.na(Train$metro_km_walk)] <- 1

Train$imp_metro_km_walk <- Train$metro_km_walk

Train$imp_metro_km_walk[Train$imp_metro_min_walk < 135.6 & Train$imp_metro_min_walk < 39.1
                                & is.na(Train$imp_metro_km_walk)] <- 1.33
Train$imp_metro_km_walk[Train$imp_metro_min_walk < 135.6 & Train$imp_metro_min_walk >= 39.1
                                & is.na(Train$imp_metro_km_walk)] <- 5.18
Train$imp_metro_km_walk[Train$imp_metro_min_walk >= 135.6 & Train$workplaces_km < 26.49
                                & is.na(Train$imp_metro_km_walk)] <- 17.97
Train$imp_metro_km_walk[Train$imp_metro_min_walk >= 135.6 & Train$workplaces_km >= 26.49
                                & is.na(Train$imp_metro_km_walk)] <- 46.50
Train$metro_km_walk <- NULL

Test$m_metro_km_walk <- 0
Test$m_metro_km_walk[is.na(Test$metro_km_walk)] <- 1

Test$imp_metro_km_walk <- Test$metro_km_walk

Test$imp_metro_km_walk[Test$imp_metro_min_walk < 135.6 & Test$imp_metro_min_walk < 39.1
                                & is.na(Test$imp_metro_km_walk)] <- 1.33
Test$imp_metro_km_walk[Test$imp_metro_min_walk < 135.6 & Test$imp_metro_min_walk < 39.1
                                & is.na(Test$imp_metro_km_walk)] <- 5.18
Test$imp_metro_km_walk[Test$imp_metro_min_walk >= 135.6 & Test$workplaces_km < 26.49
                                & is.na(Test$imp_metro_km_walk)] <- 17.97
Test$imp_metro_km_walk[Test$imp_metro_min_walk >= 135.6 & Test$workplaces_km >= 26.49
                                & is.na(Test$imp_metro_km_walk)] <- 46.50
Test$metro_km_walk <- NULL


######################################
# Impute railroad_station_walk_km    #
######################################

Train$m_railroad_station_walk_km <- 0
Train$m_railroad_station_walk_km[is.na(Train$railroad_station_walk_km)] <- 1

Train$imp_railroad_station_walk_km <- Train$railroad_station_walk_km

Train$imp_railroad_station_walk_km[Train$railroad_station_avto_km < 8.6276 & Train$railroad_station_avto_km < 3.363
                                & is.na(Train$imp_railroad_station_walk_km)] <- 1.99
Train$imp_railroad_station_walk_km[Train$railroad_station_avto_km < 8.6276 & Train$railroad_station_avto_km >= 3.363
                                & is.na(Train$imp_railroad_station_walk_km)] <- 4.71
Train$imp_railroad_station_walk_km[Train$railroad_station_avto_km >= 8.627 & Train$preschool_km < 4.028
                                & is.na(Train$imp_railroad_station_walk_km)] <- 10.67
Train$imp_railroad_station_walk_km[Train$railroad_station_avto_km >= 8.627 & Train$preschool_km >= 4.028
                                & is.na(Train$imp_railroad_station_walk_km)] <- 16.91
Train$railroad_station_walk_km <- NULL

Test$m_railroad_station_walk_km <- 0
Test$m_railroad_station_walk_km[is.na(Test$railroad_station_walk_km)] <- 1

Test$imp_railroad_station_walk_km <- Test$railroad_station_walk_km

Test$imp_railroad_station_walk_km[Test$railroad_station_avto_km < 8.6276 & Test$railroad_station_avto_km < 3.363
                                & is.na(Test$imp_railroad_station_walk_km)] <- 1.99
Test$imp_railroad_station_walk_km[Test$railroad_station_avto_km < 8.6276 & Test$railroad_station_avto_km >= 3.363
                                & is.na(Test$imp_railroad_station_walk_km)] <- 4.71
Test$imp_railroad_station_walk_km[Test$railroad_station_avto_km >= 8.627 & Test$preschool_km < 4.028
                                & is.na(Test$imp_railroad_station_walk_km)] <- 10.67
Test$imp_railroad_station_walk_km[Test$railroad_station_avto_km >= 8.627 & Test$preschool_km >= 4.028
                                & is.na(Test$imp_railroad_station_walk_km)] <- 16.91
Test$railroad_station_walk_km <- NULL

######################################
# Impute railroad_station_walk_min   #
######################################

Train$m_railroad_station_walk_min <- 0
Train$m_railroad_station_walk_min[is.na(Train$railroad_station_walk_min)] <- 1

Train$imp_railroad_station_walk_min <- Train$railroad_station_walk_min

Train$imp_railroad_station_walk_min[Train$imp_railroad_station_walk_km < 8.152 & Train$imp_railroad_station_walk_km < 3.652
                                & is.na(Train$imp_railroad_station_walk_min)] <- 25.54
Train$imp_railroad_station_walk_min[Train$imp_railroad_station_walk_km < 8.152 & Train$imp_railroad_station_walk_km >= 3.652
                                & is.na(Train$imp_railroad_station_walk_min)] <- 62.11
Train$imp_railroad_station_walk_min[Train$imp_railroad_station_walk_km >= 8.152 & Train$imp_railroad_station_walk_km < 13.95
                                & is.na(Train$imp_railroad_station_walk_min)] <- 126.50
Train$imp_railroad_station_walk_min[Train$imp_railroad_station_walk_km >= 8.152 & Train$imp_railroad_station_walk_km >= 13.95
                                & is.na(Train$imp_railroad_station_walk_min)] <- 207.59
Train$railroad_station_walk_min <- NULL

Test$m_railroad_station_walk_min <- 0
Test$m_railroad_station_walk_min[is.na(Test$railroad_station_walk_min)] <- 1

Test$imp_railroad_station_walk_min <- Test$railroad_station_walk_min

Test$imp_railroad_station_walk_min[Test$imp_railroad_station_walk_km < 8.152 & Test$imp_railroad_station_walk_km < 3.652
                                & is.na(Test$imp_railroad_station_walk_min)] <- 25.54
Test$imp_railroad_station_walk_min[Test$imp_railroad_station_walk_km < 8.152 & Test$imp_railroad_station_walk_km >= 3.652
                                & is.na(Test$imp_railroad_station_walk_min)] <- 62.11
Test$imp_railroad_station_walk_min[Test$imp_railroad_station_walk_km >= 8.152 & Test$imp_railroad_station_walk_km < 13.95
                                & is.na(Test$imp_railroad_station_walk_min)] <- 126.50
Test$imp_railroad_station_walk_min[Test$imp_railroad_station_walk_km >= 8.152 & Test$imp_railroad_station_walk_km >= 13.95
                                & is.na(Test$imp_railroad_station_walk_min)] <- 207.59
Test$railroad_station_walk_min <- NULL

######################################
# Impute ID_railroad_station_walk   #
######################################

Train$m_ID_railroad_station_walk <- 0
Train$m_ID_railroad_station_walk[is.na(Train$ID_railroad_station_walk)] <- 1

Train$imp_ID_railroad_station_walk <- Train$ID_railroad_station_walk

Train$imp_ID_railroad_station_walk[Train$ID_railroad_station_avto < 46.5 & Train$ID_railroad_station_avto >= 18.5
                                & is.na(Train$imp_ID_railroad_station_walk)] <- 32.27
Train$imp_ID_railroad_station_walk[Train$ID_railroad_station_avto < 46.5 & Train$ID_railroad_station_avto < 18.5
                                & is.na(Train$imp_ID_railroad_station_walk)] <- 12.31
Train$imp_ID_railroad_station_walk[Train$ID_railroad_station_avto >= 46.5 & Train$mosque_km < 14.17
                                & is.na(Train$imp_ID_railroad_station_walk)] <- 58.98
Train$imp_ID_railroad_station_walk[Train$ID_railroad_station_avto >= 46.5 & Train$mosque_km >= 14.17
                                & is.na(Train$imp_ID_railroad_station_walk)] <- 110.94
Train$ID_railroad_station_walk <- NULL

Test$m_ID_railroad_station_walk <- 0
Test$m_ID_railroad_station_walk[is.na(Test$ID_railroad_station_walk)] <- 1

Test$imp_ID_railroad_station_walk <- Test$ID_railroad_station_walk

Test$imp_ID_railroad_station_walk[Test$ID_railroad_station_avto < 46.5 & Test$ID_railroad_station_avto >= 18.5
                                & is.na(Test$imp_ID_railroad_station_walk)] <- 32.27
Test$imp_ID_railroad_station_walk[Test$ID_railroad_station_avto < 46.5 & Test$ID_railroad_station_avto < 18.5
                                & is.na(Test$imp_ID_railroad_station_walk)] <- 12.31
Test$imp_ID_railroad_station_walk[Test$ID_railroad_station_avto >= 46.5 & Test$mosque_km < 14.17
                                & is.na(Test$imp_ID_railroad_station_walk)] <- 58.98
Test$imp_ID_railroad_station_walk[Test$ID_railroad_station_avto >= 46.5 & Test$mosque_km >= 14.17
                                & is.na(Test$imp_ID_railroad_station_walk)] <- 110.94
Test$ID_railroad_station_walk <- NULL

######################################
# Impute cafe_sum_1000_min_price_avg   #
######################################

Train$m_cafe_sum_1000_min_price_avg <- 0
Train$m_cafe_sum_1000_min_price_avg[is.na(Train$cafe_sum_1000_min_price_avg)] <- 1

Train$imp_cafe_sum_1000_min_price_avg <- Train$cafe_sum_1000_min_price_avg

Train$imp_cafe_sum_1000_min_price_avg[Train$cafe_count_1000_price_2500 >= 0.5 & Train$cafe_count_1000_price_500 >= 0.5
                                & is.na(Train$imp_cafe_sum_1000_min_price_avg)] <- 768.07
Train$imp_cafe_sum_1000_min_price_avg[Train$cafe_count_1000_price_2500 >= 0.5 & Train$cafe_count_1000_price_500 < 0.5
                                & is.na(Train$imp_cafe_sum_1000_min_price_avg)] <- 1032.87
Train$imp_cafe_sum_1000_min_price_avg[Train$cafe_count_1000_price_2500 < 0.5 & Train$cafe_count_1000_price_1500 >= 0.5
                                & is.na(Train$imp_cafe_sum_1000_min_price_avg)] <- 681.69
Train$imp_cafe_sum_1000_min_price_avg[Train$cafe_count_1000_price_2500 < 0.5 & Train$cafe_count_1000_price_1500 < 0.5
                                & is.na(Train$imp_cafe_sum_1000_min_price_avg)] <- 451.51
Train$cafe_sum_1000_min_price_avg <- NULL

Test$m_cafe_sum_1000_min_price_avg <- 0
Test$m_cafe_sum_1000_min_price_avg[is.na(Test$cafe_sum_1000_min_price_avg)] <- 1

Test$imp_cafe_sum_1000_min_price_avg <- Test$cafe_sum_1000_min_price_avg

Test$imp_cafe_sum_1000_min_price_avg[Test$cafe_count_1000_price_2500 >= 0.5 & Test$cafe_count_1000_price_500 >= 0.5
                                & is.na(Test$imp_cafe_sum_1000_min_price_avg)] <- 768.07
Test$imp_cafe_sum_1000_min_price_avg[Test$cafe_count_1000_price_2500 >= 0.5 & Test$cafe_count_1000_price_500 < 0.5
                                & is.na(Test$imp_cafe_sum_1000_min_price_avg)] <- 1032.87
Test$imp_cafe_sum_1000_min_price_avg[Test$cafe_count_1000_price_2500 < 0.5 & Test$cafe_count_1000_price_1500 >= 0.5
                                & is.na(Test$imp_cafe_sum_1000_min_price_avg)] <- 681.69
Test$imp_cafe_sum_1000_min_price_avg[Test$cafe_count_1000_price_2500 < 0.5 & Test$cafe_count_1000_price_1500 < 0.5
                                & is.na(Test$imp_cafe_sum_1000_min_price_avg)] <- 451.51
Test$cafe_sum_1000_min_price_avg <- NULL

######################################
# Impute cafe_sum_1000_max_price_avg   #
######################################

Train$m_cafe_sum_1000_max_price_avg <- 0
Train$m_cafe_sum_1000_max_price_avg[is.na(Train$cafe_sum_1000_max_price_avg)] <- 1

Train$imp_cafe_sum_1000_max_price_avg <- Train$cafe_sum_1000_max_price_avg

Train$imp_cafe_sum_1000_max_price_avg[Train$imp_cafe_sum_1000_min_price_avg < 777.7 & Train$imp_cafe_sum_1000_min_price_avg >= 497.4
                                & is.na(Train$imp_cafe_sum_1000_max_price_avg)] <- 1081.03
Train$imp_cafe_sum_1000_max_price_avg[Train$imp_cafe_sum_1000_min_price_avg < 777.7 & Train$imp_cafe_sum_1000_min_price_avg < 497.4
                                & is.na(Train$imp_cafe_sum_1000_max_price_avg)] <- 706.64
Train$imp_cafe_sum_1000_max_price_avg[Train$imp_cafe_sum_1000_min_price_avg >= 777.7 & Train$imp_cafe_sum_1000_min_price_avg < 1142
                                & is.na(Train$imp_cafe_sum_1000_max_price_avg)] <- 1520.08
Train$imp_cafe_sum_1000_max_price_avg[Train$imp_cafe_sum_1000_min_price_avg >= 777.7 & Train$imp_cafe_sum_1000_min_price_avg >= 1142
                                & is.na(Train$imp_cafe_sum_1000_max_price_avg)] <- 2194.62
Train$cafe_sum_1000_max_price_avg <- NULL

Test$m_cafe_sum_1000_max_price_avg <- 0
Test$m_cafe_sum_1000_max_price_avg[is.na(Test$cafe_sum_1000_max_price_avg)] <- 1

Test$imp_cafe_sum_1000_max_price_avg <- Test$cafe_sum_1000_max_price_avg

Test$imp_cafe_sum_1000_max_price_avg[Test$imp_cafe_sum_1000_min_price_avg < 777.7 & Test$imp_cafe_sum_1000_min_price_avg >= 497.4
                                & is.na(Test$imp_cafe_sum_1000_max_price_avg)] <- 1081.03
Test$imp_cafe_sum_1000_max_price_avg[Test$imp_cafe_sum_1000_min_price_avg < 777.7 & Test$imp_cafe_sum_1000_min_price_avg < 497.4
                                & is.na(Test$imp_cafe_sum_1000_max_price_avg)] <- 706.64
Test$imp_cafe_sum_1000_max_price_avg[Test$imp_cafe_sum_1000_min_price_avg >= 777.7 & Test$imp_cafe_sum_1000_min_price_avg < 1142
                                & is.na(Test$imp_cafe_sum_1000_max_price_avg)] <- 1520.08
Test$imp_cafe_sum_1000_max_price_avg[Test$imp_cafe_sum_1000_min_price_avg >= 777.7 & Test$imp_cafe_sum_1000_min_price_avg >= 1142
                                & is.na(Test$imp_cafe_sum_1000_max_price_avg)] <- 2194.62
Test$cafe_sum_1000_max_price_avg <- NULL


################################
# Impute cafe_avg_price_1000   #
################################

Train$m_cafe_avg_price_1000 <- 0
Train$m_cafe_avg_price_1000[is.na(Train$cafe_avg_price_1000)] <- 1

Train$imp_cafe_avg_price_1000 <- Train$cafe_avg_price_1000

Train$imp_cafe_avg_price_1000[Train$imp_cafe_sum_1000_min_price_avg < 777.7 & Train$imp_cafe_sum_1000_min_price_avg >= 581.5
                                & is.na(Train$imp_cafe_avg_price_1000)] <- 907.03
Train$imp_cafe_avg_price_1000[Train$imp_cafe_sum_1000_min_price_avg < 777.7 & Train$imp_cafe_sum_1000_min_price_avg < 581.5
                                & is.na(Train$imp_cafe_avg_price_1000)] <- 676.44
Train$imp_cafe_avg_price_1000[Train$imp_cafe_sum_1000_min_price_avg >= 777.7 & Train$imp_cafe_sum_1000_min_price_avg < 1142
                                & is.na(Train$imp_cafe_avg_price_1000)] <- 1221.35
Train$imp_cafe_avg_price_1000[Train$imp_cafe_sum_1000_min_price_avg >= 777.7 & Train$imp_cafe_sum_1000_min_price_avg >= 1142
                                & is.na(Train$imp_cafe_avg_price_1000)] <- 1770.83
Train$cafe_avg_price_1000 <- NULL

Test$m_cafe_avg_price_1000 <- 0
Test$m_cafe_avg_price_1000[is.na(Test$cafe_avg_price_1000)] <- 1

Test$imp_cafe_avg_price_1000 <- Test$cafe_avg_price_1000

Test$imp_cafe_avg_price_1000[Test$imp_cafe_sum_1000_min_price_avg < 777.7 & Test$imp_cafe_sum_1000_min_price_avg >= 581.5
                                & is.na(Test$imp_cafe_avg_price_1000)] <- 907.03
Test$imp_cafe_avg_price_1000[Test$imp_cafe_sum_1000_min_price_avg < 777.7 & Test$imp_cafe_sum_1000_min_price_avg < 581.5
                                & is.na(Test$imp_cafe_avg_price_1000)] <- 676.44
Test$imp_cafe_avg_price_1000[Test$imp_cafe_sum_1000_min_price_avg >= 777.7 & Test$imp_cafe_sum_1000_min_price_avg < 1142
                                & is.na(Test$imp_cafe_avg_price_1000)] <- 1221.35
Test$imp_cafe_avg_price_1000[Test$imp_cafe_sum_1000_min_price_avg >= 777.7 & Test$imp_cafe_sum_1000_min_price_avg >= 1142
                                & is.na(Test$imp_cafe_avg_price_1000)] <- 1770.83
Test$cafe_avg_price_1000 <- NULL

########################################
# Impute cafe_sum_1500_min_price_avg   #
########################################

Train$m_cafe_sum_1500_min_price_avg <- 0
Train$m_cafe_sum_1500_min_price_avg[is.na(Train$cafe_sum_1500_min_price_avg)] <- 1

Train$imp_cafe_sum_1500_min_price_avg <- Train$cafe_sum_1500_min_price_avg

Train$imp_cafe_sum_1500_min_price_avg[Train$imp_cafe_avg_price_1000 < 1093 & Train$imp_cafe_avg_price_1000 >= 778.1
                                & is.na(Train$imp_cafe_sum_1500_min_price_avg)] <- 696.14
Train$imp_cafe_sum_1500_min_price_avg[Train$imp_cafe_avg_price_1000 < 1093 & Train$imp_cafe_avg_price_1000 < 778.1
                                & is.na(Train$imp_cafe_sum_1500_min_price_avg)] <- 549.90
Train$imp_cafe_sum_1500_min_price_avg[Train$imp_cafe_avg_price_1000 >= 1093 & Train$imp_cafe_sum_1000_max_price_avg < 1838
                                & is.na(Train$imp_cafe_sum_1500_min_price_avg)] <- 882.13
Train$imp_cafe_sum_1500_min_price_avg[Train$imp_cafe_avg_price_1000 >= 1093 & Train$imp_cafe_sum_1000_max_price_avg >= 1838
                                & is.na(Train$imp_cafe_sum_1500_min_price_avg)] <- 1078.86
Train$cafe_sum_1500_min_price_avg <- NULL

Test$m_cafe_sum_1500_min_price_avg <- 0
Test$m_cafe_sum_1500_min_price_avg[is.na(Test$cafe_sum_1500_min_price_avg)] <- 1

Test$imp_cafe_sum_1500_min_price_avg <- Test$cafe_sum_1500_min_price_avg

Test$imp_cafe_sum_1500_min_price_avg[Test$imp_cafe_avg_price_1000 < 1093 & Test$imp_cafe_avg_price_1000 >= 778.1
                                & is.na(Test$imp_cafe_sum_1500_min_price_avg)] <- 696.14
Test$imp_cafe_sum_1500_min_price_avg[Test$imp_cafe_avg_price_1000 < 1093 & Test$imp_cafe_avg_price_1000 < 778.1
                                & is.na(Test$imp_cafe_sum_1500_min_price_avg)] <- 549.90
Test$imp_cafe_sum_1500_min_price_avg[Test$imp_cafe_avg_price_1000 >= 1093 & Test$imp_cafe_sum_1000_max_price_avg < 1838
                                & is.na(Test$imp_cafe_sum_1500_min_price_avg)] <- 882.13
Test$imp_cafe_sum_1500_min_price_avg[Test$imp_cafe_avg_price_1000 >= 1093 & Test$imp_cafe_sum_1000_max_price_avg >= 1838
                                & is.na(Test$imp_cafe_sum_1500_min_price_avg)] <- 1078.86
Test$cafe_sum_1500_min_price_avg <- NULL

########################################
# Impute cafe_sum_1500_max_price_avg   #
########################################

Train$m_cafe_sum_1500_max_price_avg <- 0
Train$m_cafe_sum_1500_max_price_avg[is.na(Train$cafe_sum_1500_max_price_avg)] <- 1

Train$imp_cafe_sum_1500_max_price_avg <- Train$cafe_sum_1500_max_price_avg

Train$imp_cafe_sum_1500_max_price_avg[Train$imp_cafe_sum_1000_max_price_avg < 1376 & Train$cafe_count_1500_price_4000 < 0.5
                                & is.na(Train$imp_cafe_sum_1500_max_price_avg)] <- 1055.37
Train$imp_cafe_sum_1500_max_price_avg[Train$imp_cafe_sum_1000_max_price_avg < 1376 & Train$cafe_count_1500_price_4000 >= 0.5
                                & is.na(Train$imp_cafe_sum_1500_max_price_avg)] <- 1303.78
Train$imp_cafe_sum_1500_max_price_avg[Train$imp_cafe_sum_1000_max_price_avg >= 1376 & Train$imp_cafe_avg_price_1000 < 1319
                                & is.na(Train$imp_cafe_sum_1500_max_price_avg)] <- 1410.02
Train$imp_cafe_sum_1500_max_price_avg[Train$imp_cafe_sum_1000_max_price_avg >= 1376 & Train$imp_cafe_avg_price_1000 >= 1319
                                & is.na(Train$imp_cafe_sum_1500_max_price_avg)] <- 1674.35
Train$cafe_sum_1500_max_price_avg <- NULL

Test$m_cafe_sum_1500_max_price_avg <- 0
Test$m_cafe_sum_1500_max_price_avg[is.na(Test$cafe_sum_1500_max_price_avg)] <- 1

Test$imp_cafe_sum_1500_max_price_avg <- Test$cafe_sum_1500_max_price_avg

Test$imp_cafe_sum_1500_max_price_avg[Test$imp_cafe_sum_1000_max_price_avg < 1376 & Test$cafe_count_1500_price_4000 < 0.5
                                & is.na(Test$imp_cafe_sum_1500_max_price_avg)] <- 1055.37
Test$imp_cafe_sum_1500_max_price_avg[Test$imp_cafe_sum_1000_max_price_avg < 1376 & Test$cafe_count_1500_price_4000 >= 0.5
                                & is.na(Test$imp_cafe_sum_1500_max_price_avg)] <- 1303.78
Test$imp_cafe_sum_1500_max_price_avg[Test$imp_cafe_sum_1000_max_price_avg >= 1376 & Test$imp_cafe_avg_price_1000 < 1319
                                & is.na(Test$imp_cafe_sum_1500_max_price_avg)] <- 1410.02
Test$imp_cafe_sum_1500_max_price_avg[Test$imp_cafe_sum_1000_max_price_avg >= 1376 & Test$imp_cafe_avg_price_1000 >= 1319
                                & is.na(Test$imp_cafe_sum_1500_max_price_avg)] <- 1674.35
Test$cafe_sum_1500_max_price_avg <- NULL

################################
# Impute cafe_avg_price_1500   #
################################

Train$m_cafe_avg_price_1500 <- 0
Train$m_cafe_avg_price_1500[is.na(Train$cafe_avg_price_1500)] <- 1

Train$imp_cafe_avg_price_1500 <- Train$cafe_avg_price_1500

Train$imp_cafe_avg_price_1500[Train$imp_cafe_sum_1500_max_price_avg < 1302 & Train$imp_cafe_sum_1500_max_price_avg >= 1034
                                & is.na(Train$imp_cafe_avg_price_1500)] <- 925.27
Train$imp_cafe_avg_price_1500[Train$imp_cafe_sum_1500_max_price_avg < 1302 & Train$imp_cafe_sum_1500_max_price_avg < 1034
                                & is.na(Train$imp_cafe_avg_price_1500)] <- 714.59
Train$imp_cafe_avg_price_1500[Train$imp_cafe_sum_1500_max_price_avg >= 1302 & Train$imp_cafe_sum_1500_max_price_avg < 1634
                                & is.na(Train$imp_cafe_avg_price_1500)] <- 1169.92
Train$imp_cafe_avg_price_1500[Train$imp_cafe_sum_1500_max_price_avg >= 1302 & Train$imp_cafe_sum_1500_max_price_avg >= 1634
                                & is.na(Train$imp_cafe_avg_price_1500)] <- 1448.81
Train$cafe_avg_price_1500 <- NULL

Test$m_cafe_avg_price_1500 <- 0
Test$m_cafe_avg_price_1500[is.na(Test$cafe_avg_price_1500)] <- 1

Test$imp_cafe_avg_price_1500 <- Test$cafe_avg_price_1500

Test$imp_cafe_avg_price_1500[Test$imp_cafe_sum_1500_max_price_avg < 1302 & Test$imp_cafe_sum_1500_max_price_avg >= 1034
                                & is.na(Test$imp_cafe_avg_price_1500)] <- 925.27
Test$imp_cafe_avg_price_1500[Test$imp_cafe_sum_1500_max_price_avg < 1302 & Test$imp_cafe_sum_1500_max_price_avg < 1034
                                & is.na(Test$imp_cafe_avg_price_1500)] <- 714.59
Test$imp_cafe_avg_price_1500[Test$imp_cafe_sum_1500_max_price_avg >= 1302 & Test$imp_cafe_sum_1500_max_price_avg < 1634
                                & is.na(Test$imp_cafe_avg_price_1500)] <- 1169.92
Test$imp_cafe_avg_price_1500[Test$imp_cafe_sum_1500_max_price_avg >= 1302 & Test$imp_cafe_sum_1500_max_price_avg >= 1634
                                & is.na(Test$imp_cafe_avg_price_1500)] <- 1448.81
Test$cafe_avg_price_1500 <- NULL

########################################
# Impute cafe_sum_2000_min_price_avg   #
########################################

Train$m_cafe_sum_2000_min_price_avg <- 0
Train$m_cafe_sum_2000_min_price_avg[is.na(Train$cafe_sum_2000_min_price_avg)] <- 1

Train$imp_cafe_sum_2000_min_price_avg <- Train$cafe_sum_2000_min_price_avg

Train$imp_cafe_sum_2000_min_price_avg[Train$imp_cafe_sum_1500_min_price_avg < 827.1 & Train$swim_pool_km < 9.416
                                & is.na(Train$imp_cafe_sum_2000_min_price_avg)] <- 651.75
Train$imp_cafe_sum_2000_min_price_avg[Train$imp_cafe_sum_1500_min_price_avg < 827.1 & Train$swim_pool_km >= 9.416
                                & is.na(Train$imp_cafe_sum_2000_min_price_avg)] <- 1708.86
Train$imp_cafe_sum_2000_min_price_avg[Train$imp_cafe_sum_1500_min_price_avg >= 827.1 & Train$cafe_sum_3000_max_price_avg < 1465
                                & is.na(Train$imp_cafe_sum_2000_min_price_avg)] <- 850.56
Train$imp_cafe_sum_2000_min_price_avg[Train$imp_cafe_sum_1500_min_price_avg >= 827.1 & Train$cafe_sum_3000_max_price_avg >= 1465
                                & is.na(Train$imp_cafe_sum_2000_min_price_avg)] <- 1033.57
Train$cafe_sum_2000_min_price_avg <- NULL

Test$m_cafe_sum_2000_min_price_avg <- 0
Test$m_cafe_sum_2000_min_price_avg[is.na(Test$cafe_sum_2000_min_price_avg)] <- 1

Test$imp_cafe_sum_2000_min_price_avg <- Test$cafe_sum_2000_min_price_avg

Test$imp_cafe_sum_2000_min_price_avg[Test$imp_cafe_sum_1500_min_price_avg < 827.1 & Test$swim_pool_km < 9.416
                                & is.na(Test$imp_cafe_sum_2000_min_price_avg)] <- 651.75
Test$imp_cafe_sum_2000_min_price_avg[Test$imp_cafe_sum_1500_min_price_avg < 827.1 & Test$swim_pool_km >= 9.416
                                & is.na(Test$imp_cafe_sum_2000_min_price_avg)] <- 1708.86
Test$imp_cafe_sum_2000_min_price_avg[Test$imp_cafe_sum_1500_min_price_avg >= 827.1 & Test$cafe_sum_3000_max_price_avg < 1465
                                & is.na(Test$imp_cafe_sum_2000_min_price_avg)] <- 850.56
Test$imp_cafe_sum_2000_min_price_avg[Test$imp_cafe_sum_1500_min_price_avg >= 827.1 & Test$cafe_sum_3000_max_price_avg >= 1465
                                & is.na(Test$imp_cafe_sum_2000_min_price_avg)] <- 1033.57
Test$cafe_sum_2000_min_price_avg <- NULL

########################################
# Impute cafe_sum_2000_max_price_avg   #
########################################

Train$m_cafe_sum_2000_max_price_avg <- 0
Train$m_cafe_sum_2000_max_price_avg[is.na(Train$cafe_sum_2000_max_price_avg)] <- 1

Train$imp_cafe_sum_2000_max_price_avg <- Train$cafe_sum_2000_max_price_avg

Train$imp_cafe_sum_2000_max_price_avg[Train$imp_cafe_sum_2000_min_price_avg < 809.4 & Train$imp_cafe_sum_2000_min_price_avg < 650.3
                                & is.na(Train$imp_cafe_sum_2000_max_price_avg)] <- 980.88
Train$imp_cafe_sum_2000_max_price_avg[Train$imp_cafe_sum_2000_min_price_avg < 809.4 & Train$imp_cafe_sum_2000_min_price_avg >= 650.3
                                & is.na(Train$imp_cafe_sum_2000_max_price_avg)] <- 1204.60
Train$imp_cafe_sum_2000_max_price_avg[Train$imp_cafe_sum_2000_min_price_avg >= 809.4 & Train$imp_cafe_sum_2000_min_price_avg < 1410
                                & is.na(Train$imp_cafe_sum_2000_max_price_avg)] <- 1542.70
Train$imp_cafe_sum_2000_max_price_avg[Train$imp_cafe_sum_2000_min_price_avg >= 809.4 & Train$imp_cafe_sum_2000_min_price_avg >= 1410
                                & is.na(Train$imp_cafe_sum_2000_max_price_avg)] <- 2910.59
Train$cafe_sum_2000_max_price_avg <- NULL

Test$m_cafe_sum_2000_max_price_avg <- 0
Test$m_cafe_sum_2000_max_price_avg[is.na(Test$cafe_sum_2000_max_price_avg)] <- 1

Test$imp_cafe_sum_2000_max_price_avg <- Test$cafe_sum_2000_max_price_avg

Test$imp_cafe_sum_2000_max_price_avg[Test$imp_cafe_sum_2000_min_price_avg < 809.4 & Test$imp_cafe_sum_2000_min_price_avg < 650.3
                                & is.na(Test$imp_cafe_sum_2000_max_price_avg)] <- 980.88
Test$imp_cafe_sum_2000_max_price_avg[Test$imp_cafe_sum_2000_min_price_avg < 809.4 & Test$imp_cafe_sum_2000_min_price_avg >= 650.3
                                & is.na(Test$imp_cafe_sum_2000_max_price_avg)] <- 1204.60
Test$imp_cafe_sum_2000_max_price_avg[Test$imp_cafe_sum_2000_min_price_avg >= 809.4 & Test$imp_cafe_sum_2000_min_price_avg < 1410
                                & is.na(Test$imp_cafe_sum_2000_max_price_avg)] <- 1542.70
Test$imp_cafe_sum_2000_max_price_avg[Test$imp_cafe_sum_2000_min_price_avg >= 809.4 & Test$imp_cafe_sum_2000_min_price_avg >= 1410
                                & is.na(Test$imp_cafe_sum_2000_max_price_avg)] <- 2910.59
Test$cafe_sum_2000_max_price_avg <- NULL

################################
# Impute cafe_avg_price_2000   #
################################

Train$m_cafe_avg_price_2000 <- 0
Train$m_cafe_avg_price_2000[is.na(Train$cafe_avg_price_2000)] <- 1

Train$imp_cafe_avg_price_2000 <- Train$cafe_avg_price_2000

Train$imp_cafe_avg_price_2000[Train$imp_cafe_sum_2000_min_price_avg < 809.4 & Train$imp_cafe_sum_2000_min_price_avg < 650.3
                                & is.na(Train$imp_cafe_avg_price_2000)] <- 773.08
Train$imp_cafe_avg_price_2000[Train$imp_cafe_sum_2000_min_price_avg < 809.4 & Train$imp_cafe_sum_2000_min_price_avg >= 650.3
                                & is.na(Train$imp_cafe_avg_price_2000)] <- 960.72
Train$imp_cafe_avg_price_2000[Train$imp_cafe_sum_2000_min_price_avg >= 809.4 & Train$imp_cafe_sum_2000_min_price_avg < 1410
                                & is.na(Train$imp_cafe_avg_price_2000)] <- 1245.47
Train$imp_cafe_avg_price_2000[Train$imp_cafe_sum_2000_min_price_avg >= 809.4 & Train$imp_cafe_sum_2000_min_price_avg >= 1410
                                & is.na(Train$imp_cafe_avg_price_2000)] <- 2342.31
Train$cafe_avg_price_2000 <- NULL

Test$m_cafe_avg_price_2000 <- 0
Test$m_cafe_avg_price_2000[is.na(Test$cafe_avg_price_2000)] <- 1

Test$imp_cafe_avg_price_2000 <- Test$cafe_avg_price_2000

Test$imp_cafe_avg_price_2000[Test$imp_cafe_sum_2000_min_price_avg < 809.4 & Test$imp_cafe_sum_2000_min_price_avg < 650.3
                                & is.na(Test$imp_cafe_avg_price_2000)] <- 773.08
Test$imp_cafe_avg_price_2000[Test$imp_cafe_sum_2000_min_price_avg < 809.4 & Test$imp_cafe_sum_2000_min_price_avg >= 650.3
                                & is.na(Test$imp_cafe_avg_price_2000)] <- 960.72
Test$imp_cafe_avg_price_2000[Test$imp_cafe_sum_2000_min_price_avg >= 809.4 & Test$imp_cafe_sum_2000_min_price_avg < 1410
                                & is.na(Test$imp_cafe_avg_price_2000)] <- 1245.47
Test$imp_cafe_avg_price_2000[Test$imp_cafe_sum_2000_min_price_avg >= 809.4 & Test$imp_cafe_sum_2000_min_price_avg >= 1410
                                & is.na(Test$imp_cafe_avg_price_2000)] <- 2342.31
Test$cafe_avg_price_2000 <- NULL


#######################################
# Impute cafe_sum_3000_min_price_avg  #
#######################################

Train$m_cafe_sum_3000_min_price_avg <- 0
Train$m_cafe_sum_3000_min_price_avg[is.na(Train$cafe_sum_3000_min_price_avg)] <- 1

Train$imp_cafe_sum_3000_min_price_avg <- Train$cafe_sum_3000_min_price_avg

Train$imp_cafe_sum_3000_min_price_avg[Train$area_m < 5.988e+07 & Train$imp_cafe_sum_2000_min_price_avg < 833.4
                                & is.na(Train$imp_cafe_sum_3000_min_price_avg)] <- 684.87
Train$imp_cafe_sum_3000_min_price_avg[Train$area_m < 5.988e+07 & Train$imp_cafe_sum_2000_min_price_avg >= 833.4
                                & is.na(Train$imp_cafe_sum_3000_min_price_avg)] <- 903.20
Train$imp_cafe_sum_3000_min_price_avg[Train$area_m >= 5.988e+07 & Train$cafe_count_3000_price_4000 >= 0.5
                                & is.na(Train$imp_cafe_sum_3000_min_price_avg)] <- 1678.61
Train$imp_cafe_sum_3000_min_price_avg[Train$area_m >= 5.988e+07 & Train$cafe_count_3000_price_4000 < 0.5
                                & is.na(Train$imp_cafe_sum_3000_min_price_avg)] <- 964.06
Train$cafe_sum_3000_min_price_avg <- NULL

Test$m_cafe_sum_3000_min_price_avg <- 0
Test$m_cafe_sum_3000_min_price_avg[is.na(Test$cafe_sum_3000_min_price_avg)] <- 1

Test$imp_cafe_sum_3000_min_price_avg <- Test$cafe_sum_3000_min_price_avg

Test$imp_cafe_sum_3000_min_price_avg[Test$area_m < 5.988e+07 & Test$imp_cafe_sum_2000_min_price_avg < 833.4
                                & is.na(Test$imp_cafe_sum_3000_min_price_avg)] <- 684.87
Test$imp_cafe_sum_3000_min_price_avg[Test$area_m < 5.988e+07 & Test$imp_cafe_sum_2000_min_price_avg >= 833.4
                                & is.na(Test$imp_cafe_sum_3000_min_price_avg)] <- 903.20
Test$imp_cafe_sum_3000_min_price_avg[Test$area_m >= 5.988e+07 & Test$cafe_count_3000_price_4000 >= 0.5
                                & is.na(Test$imp_cafe_sum_3000_min_price_avg)] <- 1678.61
Test$imp_cafe_sum_3000_min_price_avg[Test$area_m >= 5.988e+07 & Test$cafe_count_3000_price_4000 < 0.5
                                & is.na(Test$imp_cafe_sum_3000_min_price_avg)] <- 964.06
Test$cafe_sum_3000_min_price_avg <- NULL

#######################################
# Impute cafe_sum_3000_max_price_avg  #
#######################################

Train$m_cafe_sum_3000_max_price_avg <- 0
Train$m_cafe_sum_3000_max_price_avg[is.na(Train$cafe_sum_3000_max_price_avg)] <- 1

Train$imp_cafe_sum_3000_max_price_avg <- Train$cafe_sum_3000_max_price_avg

Train$imp_cafe_sum_3000_max_price_avg[Train$area_m < 5.988e+07 & Train$imp_cafe_sum_2000_min_price_avg < 833.4
                                & is.na(Train$imp_cafe_sum_3000_max_price_avg)] <- 684.87
Train$imp_cafe_sum_3000_max_price_avg[Train$area_m < 5.988e+07 & Train$imp_cafe_sum_2000_min_price_avg >= 833.4
                                & is.na(Train$imp_cafe_sum_3000_max_price_avg)] <- 903.20
Train$imp_cafe_sum_3000_max_price_avg[Train$area_m >= 5.988e+07 & Train$cafe_count_3000_price_4000 >= 0.5
                                & is.na(Train$imp_cafe_sum_3000_max_price_avg)] <- 1678.61
Train$imp_cafe_sum_3000_max_price_avg[Train$area_m >= 5.988e+07 & Train$cafe_count_3000_price_4000 < 0.5
                                & is.na(Train$imp_cafe_sum_3000_max_price_avg)] <- 964.06
Train$cafe_sum_3000_max_price_avg <- NULL

Test$m_cafe_sum_3000_max_price_avg <- 0
Test$m_cafe_sum_3000_max_price_avg[is.na(Test$cafe_sum_3000_max_price_avg)] <- 1

Test$imp_cafe_sum_3000_max_price_avg <- Test$cafe_sum_3000_max_price_avg

Test$imp_cafe_sum_3000_max_price_avg[Test$area_m < 5.988e+07 & Test$imp_cafe_sum_2000_min_price_avg < 833.4
                                & is.na(Test$imp_cafe_sum_3000_max_price_avg)] <- 684.87
Test$imp_cafe_sum_3000_max_price_avg[Test$area_m < 5.988e+07 & Test$imp_cafe_sum_2000_min_price_avg >= 833.4
                                & is.na(Test$imp_cafe_sum_3000_max_price_avg)] <- 903.20
Test$imp_cafe_sum_3000_max_price_avg[Test$area_m >= 5.988e+07 & Test$cafe_count_3000_price_4000 >= 0.5
                                & is.na(Test$imp_cafe_sum_3000_max_price_avg)] <- 1678.61
Test$imp_cafe_sum_3000_max_price_avg[Test$area_m >= 5.988e+07 & Test$cafe_count_3000_price_4000 < 0.5
                                & is.na(Test$imp_cafe_sum_3000_max_price_avg)] <- 964.06
Test$cafe_sum_3000_max_price_avg <- NULL

###############################
# Impute cafe_avg_price_3000  #
###############################

Train$m_cafe_avg_price_3000 <- 0
Train$m_cafe_avg_price_3000[is.na(Train$cafe_avg_price_3000)] <- 1

Train$imp_cafe_avg_price_3000 <- Train$cafe_avg_price_3000

Train$imp_cafe_avg_price_3000[Train$imp_cafe_sum_3000_min_price_avg < 1164 & Train$imp_cafe_sum_3000_max_price_avg < 1271
                                & is.na(Train$imp_cafe_avg_price_3000)] <- 884.63
Train$imp_cafe_avg_price_3000[Train$imp_cafe_sum_3000_min_price_avg < 1164 & Train$imp_cafe_sum_3000_max_price_avg >= 1271
                                & is.na(Train$imp_cafe_avg_price_3000)] <- 1145.95
Train$imp_cafe_avg_price_3000[Train$imp_cafe_sum_3000_min_price_avg >= 1164 & Train$imp_cafe_sum_3000_max_price_avg >= 1550
                                & is.na(Train$imp_cafe_avg_price_3000)] <- 2301.59
Train$imp_cafe_avg_price_3000[Train$imp_cafe_sum_3000_min_price_avg >= 1164 & Train$imp_cafe_sum_3000_max_price_avg < 1550
                                & is.na(Train$imp_cafe_avg_price_3000)] <- 1732.79
Train$cafe_avg_price_3000 <- NULL

Test$m_cafe_avg_price_3000 <- 0
Test$m_cafe_avg_price_3000[is.na(Test$cafe_avg_price_3000)] <- 1

Test$imp_cafe_avg_price_3000 <- Test$cafe_avg_price_3000

Test$imp_cafe_avg_price_3000[Test$imp_cafe_sum_3000_min_price_avg < 1164 & Test$imp_cafe_sum_3000_max_price_avg < 1271
                                & is.na(Test$imp_cafe_avg_price_3000)] <- 884.63
Test$imp_cafe_avg_price_3000[Test$imp_cafe_sum_3000_min_price_avg < 1164 & Test$imp_cafe_sum_3000_max_price_avg >= 1271
                                & is.na(Test$imp_cafe_avg_price_3000)] <- 1145.95
Test$imp_cafe_avg_price_3000[Test$imp_cafe_sum_3000_min_price_avg >= 1164 & Test$imp_cafe_sum_3000_max_price_avg >= 1550
                                & is.na(Test$imp_cafe_avg_price_3000)] <- 2301.59
Test$imp_cafe_avg_price_3000[Test$imp_cafe_sum_3000_min_price_avg >= 1164 & Test$imp_cafe_sum_3000_max_price_avg < 1550
                                & is.na(Test$imp_cafe_avg_price_3000)] <- 1732.79
Test$cafe_avg_price_3000 <- NULL

##########################
# Impute prom_part_5000  #
##########################

Train$m_prom_part_5000 <- 0
Train$m_prom_part_5000[is.na(Train$prom_part_5000)] <- 1

Train$imp_prom_part_5000 <- Train$prom_part_5000

Train$imp_prom_part_5000[Train$prom_part_3000 < 13.07 & Train$cafe_count_5000_price_500 < 24.5
                                & is.na(Train$imp_prom_part_5000)] <- 5.75
Train$imp_prom_part_5000[Train$prom_part_3000 < 13.07 & Train$cafe_count_5000_price_500 >= 24.5
                                & is.na(Train$imp_prom_part_5000)] <- 10.35
Train$imp_prom_part_5000[Train$prom_part_3000 >= 13.07 & Train$prom_part_3000 < 24.57
                                & is.na(Train$imp_prom_part_5000)] <- 14.25
Train$imp_prom_part_5000[Train$prom_part_3000 >= 13.07 & Train$prom_part_3000 >= 24.57
                                & is.na(Train$imp_prom_part_5000)] <- 21.33
Train$prom_part_5000 <- NULL

Test$m_prom_part_5000 <- 0
Test$m_prom_part_5000[is.na(Test$prom_part_5000)] <- 1

Test$imp_prom_part_5000 <- Test$prom_part_5000

Test$imp_prom_part_5000[Test$prom_part_3000 < 13.07 & Test$cafe_count_5000_price_500 < 24.5
                                & is.na(Test$imp_prom_part_5000)] <- 5.75
Test$imp_prom_part_5000[Test$prom_part_3000 < 13.07 & Test$cafe_count_5000_price_500 >= 24.5
                                & is.na(Test$imp_prom_part_5000)] <- 10.35
Test$imp_prom_part_5000[Test$prom_part_3000 >= 13.07 & Test$prom_part_3000 < 24.57
                                & is.na(Test$imp_prom_part_5000)] <- 14.25
Test$imp_prom_part_5000[Test$prom_part_3000 >= 13.07 & Test$prom_part_3000 >= 24.57
                                & is.na(Test$imp_prom_part_5000)] <- 21.33
Test$prom_part_5000 <- NULL

#######################################
# Impute cafe_sum_5000_min_price_avg  #
#######################################

Train$m_cafe_sum_5000_min_price_avg <- 0
Train$m_cafe_sum_5000_min_price_avg[is.na(Train$cafe_sum_5000_min_price_avg)] <- 1

Train$imp_cafe_sum_5000_min_price_avg <- Train$cafe_sum_5000_min_price_avg

Train$imp_cafe_sum_5000_min_price_avg[Train$shopping_centers_km < 3.798 & Train$cafe_count_5000_price_high < 0.5
                                & is.na(Train$imp_cafe_sum_5000_min_price_avg)] <- 701.44
Train$imp_cafe_sum_5000_min_price_avg[Train$shopping_centers_km < 3.798 & Train$cafe_count_5000_price_high >= 0.5
                                & is.na(Train$imp_cafe_sum_5000_min_price_avg)] <- 828.57
Train$imp_cafe_sum_5000_min_price_avg[Train$shopping_centers_km >= 3.798 & Train$green_part_1500 < 50.55
                                & is.na(Train$imp_cafe_sum_5000_min_price_avg)] <- 1172.71
Train$imp_cafe_sum_5000_min_price_avg[Train$shopping_centers_km >= 3.798 & Train$green_part_1500 >= 50.55
                                & is.na(Train$imp_cafe_sum_5000_min_price_avg)] <- 1485.25
Train$cafe_sum_5000_min_price_avg <- NULL

Test$m_cafe_sum_5000_min_price_avg <- 0
Test$m_cafe_sum_5000_min_price_avg[is.na(Test$cafe_sum_5000_min_price_avg)] <- 1

Test$imp_cafe_sum_5000_min_price_avg <- Test$cafe_sum_5000_min_price_avg

Test$imp_cafe_sum_5000_min_price_avg[Test$shopping_centers_km < 3.798 & Test$cafe_count_5000_price_high < 0.5
                                & is.na(Test$imp_cafe_sum_5000_min_price_avg)] <- 701.44
Test$imp_cafe_sum_5000_min_price_avg[Test$shopping_centers_km < 3.798 & Test$cafe_count_5000_price_high >= 0.5
                                & is.na(Test$imp_cafe_sum_5000_min_price_avg)] <- 828.57
Test$imp_cafe_sum_5000_min_price_avg[Test$shopping_centers_km >= 3.798 & Test$green_part_1500 < 50.55
                                & is.na(Test$imp_cafe_sum_5000_min_price_avg)] <- 1172.71
Test$imp_cafe_sum_5000_min_price_avg[Test$shopping_centers_km >= 3.798 & Test$green_part_1500 >= 50.55
                                & is.na(Test$imp_cafe_sum_5000_min_price_avg)] <- 1485.25
Test$cafe_sum_5000_min_price_avg <- NULL

#######################################
# Impute cafe_sum_5000_max_price_avg  #
#######################################

Train$m_cafe_sum_5000_max_price_avg <- 0
Train$m_cafe_sum_5000_max_price_avg[is.na(Train$cafe_sum_5000_max_price_avg)] <- 1

Train$imp_cafe_sum_5000_max_price_avg <- Train$cafe_sum_5000_max_price_avg

Train$imp_cafe_sum_5000_max_price_avg[Train$shopping_centers_km < 3.798 & Train$cafe_count_5000_price_high < 0.5
                                & is.na(Train$imp_cafe_sum_5000_max_price_avg)] <- 1180.64
Train$imp_cafe_sum_5000_max_price_avg[Train$shopping_centers_km < 3.798 & Train$cafe_count_5000_price_high >= 0.5
                                & is.na(Train$imp_cafe_sum_5000_max_price_avg)] <- 1376.32
Train$imp_cafe_sum_5000_max_price_avg[Train$shopping_centers_km >= 3.798 & Train$green_part_1500 < 50.55
                                & is.na(Train$imp_cafe_sum_5000_max_price_avg)] <- 1901.34
Train$imp_cafe_sum_5000_max_price_avg[Train$shopping_centers_km >= 3.798 & Train$green_part_1500 >= 50.55
                                & is.na(Train$imp_cafe_sum_5000_max_price_avg)] <- 2429.67
Train$cafe_sum_5000_max_price_avg <- NULL

Test$m_cafe_sum_5000_max_price_avg <- 0
Test$m_cafe_sum_5000_max_price_avg[is.na(Test$cafe_sum_5000_max_price_avg)] <- 1

Test$imp_cafe_sum_5000_max_price_avg <- Test$cafe_sum_5000_max_price_avg

Test$imp_cafe_sum_5000_max_price_avg[Test$shopping_centers_km < 3.798 & Test$cafe_count_5000_price_high < 0.5
                                & is.na(Test$imp_cafe_sum_5000_max_price_avg)] <- 1180.64
Test$imp_cafe_sum_5000_max_price_avg[Test$shopping_centers_km < 3.798 & Test$cafe_count_5000_price_high >= 0.5
                                & is.na(Test$imp_cafe_sum_5000_max_price_avg)] <- 1376.32
Test$imp_cafe_sum_5000_max_price_avg[Test$shopping_centers_km >= 3.798 & Test$green_part_1500 < 50.55
                                & is.na(Test$imp_cafe_sum_5000_max_price_avg)] <- 1901.34
Test$imp_cafe_sum_5000_max_price_avg[Test$shopping_centers_km >= 3.798 & Test$green_part_1500 >= 50.55
                                & is.na(Test$imp_cafe_sum_5000_max_price_avg)] <- 2429.67
Test$cafe_sum_5000_max_price_avg <- NULL

###############################
# Impute cafe_avg_price_5000  #
###############################

Train$m_cafe_avg_price_5000 <- 0
Train$m_cafe_avg_price_5000[is.na(Train$cafe_avg_price_5000)] <- 1

Train$imp_cafe_avg_price_5000 <- Train$cafe_avg_price_5000

Train$imp_cafe_avg_price_5000[Train$shopping_centers_km < 3.798 & Train$cafe_count_5000_price_high < 0.5
                                & is.na(Train$imp_cafe_avg_price_5000)] <- 941.04
Train$imp_cafe_avg_price_5000[Train$shopping_centers_km < 3.798 & Train$cafe_count_5000_price_high >= 0.5
                                & is.na(Train$imp_cafe_avg_price_5000)] <- 1102.45
Train$imp_cafe_avg_price_5000[Train$shopping_centers_km >= 3.798 & Train$green_part_1500 < 50.55
                                & is.na(Train$imp_cafe_avg_price_5000)] <- 1537.02
Train$imp_cafe_avg_price_5000[Train$shopping_centers_km >= 3.798 & Train$green_part_1500 >= 50.55
                                & is.na(Train$imp_cafe_avg_price_5000)] <- 1957.46
Train$cafe_avg_price_5000 <- NULL

Test$m_cafe_avg_price_5000 <- 0
Test$m_cafe_avg_price_5000[is.na(Test$cafe_avg_price_5000)] <- 1

Test$imp_cafe_avg_price_5000 <- Test$cafe_avg_price_5000

Test$imp_cafe_avg_price_5000[Test$shopping_centers_km < 3.798 & Test$cafe_count_5000_price_high < 0.5
                                & is.na(Test$imp_cafe_avg_price_5000)] <- 941.04
Test$imp_cafe_avg_price_5000[Test$shopping_centers_km < 3.798 & Test$cafe_count_5000_price_high >= 0.5
                                & is.na(Test$imp_cafe_avg_price_5000)] <- 1102.45
Test$imp_cafe_avg_price_5000[Test$shopping_centers_km >= 3.798 & Test$green_part_1500 < 50.55
                                & is.na(Test$imp_cafe_avg_price_5000)] <- 1537.02
Test$imp_cafe_avg_price_5000[Test$shopping_centers_km >= 3.798 & Test$green_part_1500 >= 50.55
                                & is.na(Test$imp_cafe_avg_price_5000)] <- 1957.46
Test$cafe_avg_price_5000 <- NULL


##################################################################
# Impute material  - taking too long to run - come back to this  #
##################################################################

#Train$m_material <- 0
#Train$m_material[is.na(Train$material)] <- 1

#Train$imp_material <- Train$material

#Train$imp_material[Train$product_type == "Investment" & Train$imp_max_floor < 12.5 & is.na(Train$imp_material)] <- 1970
#Train$imp_material[Train$product_type == "Investment" & Train$imp_max_floor >= 12.5 & is.na(Train$imp_material)] <- 1994
#Train$imp_material[Train$product_type == "OwnerOccupier" & is.na(Train$imp_material)] <- 2014

#Train$material <- NULL


#############################################################################
# Create Dummy Variables for sub_area -- create new variables called Neigh* #
#############################################################################

# Need to see which sub_areas would fit in well together.  Try using a boxplot by sub_area
par(mfrow = c(1, 1))
neigh <- ggplot(aes(y = price_doc, x = reorder(sub_area, price_doc, FUN = median)), data = Train) + geom_boxplot(fill = "#3366FF")
neigh + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Outliers are making it hard to differentiate the sub_areas - let's look at it in smaller chunks
median.area <- summaryBy(price_doc ~ sub_area, data = Train, FUN = median)
median.area <- median.area[order(median.area$price_doc.median),]

temp.area <- subset(Train, Train$sub_area %in% c("Poselenie Klenovskoe", "Molzhaninovskoe",
"Poselenie Novofedorovskoe", "Poselenie Shhapovskoe"), select = c(price_doc, sub_area))

neigh <- ggplot(aes(y = price_doc, x = reorder(sub_area, price_doc, FUN = mean)), data = temp.area) +
                                                geom_boxplot(fill = "#3366FF", notch = TRUE)
neigh + theme(axis.text.x = element_text(angle = 90, hjust = 1))


Train$neigh <- as.character(Train$neigh)
Train$neigh[Train$sub_area %in% c("Poselenie Klenovskoe")] <- "Neigh0"
Train$neigh[Train$sub_area %in% c("Molzhaninovskoe")] <- "Neigh1"
Train$neigh[Train$sub_area %in% c("Poselenie Novofedorovskoe", "Poselenie Shhapovskoe")] <- "Neigh2"
Train$neigh[Train$sub_area %in% c("Poselenie Marushkinskoe", "Poselenie Mihajlovo-Jarcevskoe", "Poselenie Krasnopahorskoe",
                                  "Poselenie Filimonkovskoe", "Poselenie Kokoshkino",
                                    "Poselenie Voronovskoe")] <- "Neigh3"
Train$neigh[Train$sub_area %in% c("Poselenie Shherbinka", "Poselenie Desjonovskoe")] <- "Neigh4"
Train$neigh[Train$sub_area %in% c("Poselenie Mosrentgen", "Vostochnoe", "Staroe Krjukovo", "Poselenie Pervomajskoe",
                                  "Vnukovo", "Matushkino", "Troickij okrug", "Birjulevo Zapadnoe", "Kapotnja")] <- "Neigh5"
Train$neigh[Train$sub_area %in% c("Zapadnoe Degunino", "Poselenie Sosenskoe", "Savelki", "Poselenie Voskresenskoe")] <- "Neigh6"
Train$neigh[Train$sub_area %in% c("Nekrasovka", "Poselenie Moskovskij", "Birjulevo Vostochnoe",
                                  "Solncevo", "Poselenie Rjazanovskoe", "Metrogorodok", "Vostochnoe Izmajlovo",
                                  "Pechatniki", "Caricyno", "Dmitrovskoe", "Severnoe Izmajlovo", "Nizhegorodskoe",
                                  "Gol'janovo", "Veshnjaki", "Altuf'evskoe", "Severnoe", "Krjukovo", "Kuz'minki",
                                   "Poselenie Vnukovskoe", "Rjazanskij", "Ljublino", "Perovo",
                                  "Kosino-Uhtomskoe", "Silino", "Juzhnoe Butovo", "Novogireevo", "Bogorodskoe",
                                  "Marfino", "Vyhino-Zhulebino", "Ivanovskoe", "Jaroslavskoe", "Golovinskoe",
                                  "Poselenie Rogovskoe", "Juzhnoe Medvedkovo", "Brateevo", "Lianozovo",
                                  "Preobrazhenskoe", "Tekstil'shhiki", "Beskudnikovskoe", "Vostochnoe Degunino",
                                  "Novo-Peredelkino", "Orehovo-Borisovo Severnoe")] <- "Neigh7"
Train$neigh[Train$sub_area == "Tverskoe"] <- "Neigh8"
Train$neigh[Train$sub_area %in% c("Koptevo", "Juzhnoe Tushino", "Mozhajskoe", "Sokolinaja Gora",
                                  "Zjablikovo", "Bibirevo", "Chertanovo Central'noe", "Savelovskoe", "Kotlovka",
                                  "Orehovo-Borisovo Juzhnoe", "Chertanovo Juzhnoe", "Ochakovo-Matveevskoe",
                                  "Nagatino-Sadovniki", "Timirjazevskoe", "Otradnoe", "Chertanovo Severnoe",
                                  "Rostokino", "Severnoe Medvedkovo", "Babushkinskoe", "Mar'ino", "Lefortovo",
                                  "Severnoe Butovo", "Novokosino", "Losinoostrovskoe")] <- "Neigh9"
Train$neigh[Train$sub_area %in% c("Horoshevo-Mnevniki", "Zjuzino", "Fili Davydkovo", "Hovrino",
                                  "Pokrovskoe Streshnevo", "Severnoe Tushino", "Butyrskoe", "Sviblovo",
                                  "Mar'ina Roshha", "Moskvorech'e-Saburovo", "Nagatinskij Zaton",
                                  "Teplyj Stan", "Kon'kovo", "Ostankinskoe", "Jasenevo", "Juzhnoportovoe",
                                  "Levoberezhnoe", "Filevskij Park", "Alekseevskoe", "Mitino")] <- "Neigh10"
Train$neigh[Train$sub_area %in% c("Izmajlovo", "Kuncevo", "Shhukino", "Cheremushki", "Jakimanka",
                                  "Strogino", "Horoshevskoe", "Vojkovskoe", "Danilovskoe", "Donskoe",
                                  "Ajeroport", "Sokol", "Taganskoe", "Akademicheskoe", "Nagornoe", "Sokol'niki",
                                  "Begovoe", "Poselenie Kievskij", "Meshhanskoe", "Krasnosel'skoe", "Basmannoe",
                                  "Troparevo-Nikulino")] <- "Neigh11"
Train$neigh[Train$sub_area %in% c("Prospekt Vernadskogo", "Presnenskoe", "Ramenki", "Gagarinskoe",
                                  "Krylatskoe", "Arbat", "Dorogomilovo", "Obruchevskoe", "Zamoskvorech'e",
                                  "Lomonosovskoe", "Kurkino", "Hamovniki")] <- "Neigh12"
Train$neigh <- factor(Train$neigh)

neigh <- ggplot(aes(y = price_doc, x = reorder(neigh, price_doc, FUN = mean)), data = Train) +
                        geom_boxplot(fill = "#3366FF")
neigh + theme(axis.text.x = element_text(angle = 90, hjust = 1))


Test$neigh <- as.character(Test$neigh)
Test$neigh[Test$sub_area %in% c("Poselenie Klenovskoe")] <- "Neigh0"
Test$neigh[Test$sub_area %in% c("Molzhaninovskoe")] <- "Neigh1"
Test$neigh[Test$sub_area %in% c("Poselenie Novofedorovskoe", "Poselenie Shhapovskoe")] <- "Neigh2"
Test$neigh[Test$sub_area %in% c("Poselenie Marushkinskoe", "Poselenie Mihajlovo-Jarcevskoe", "Poselenie Krasnopahorskoe",
                                  "Poselenie Filimonkovskoe", "Poselenie Kokoshkino",
                                    "Poselenie Voronovskoe")] <- "Neigh3"
Test$neigh[Test$sub_area %in% c("Poselenie Shherbinka", "Poselenie Desjonovskoe")] <- "Neigh4"
Test$neigh[Test$sub_area %in% c("Poselenie Mosrentgen", "Vostochnoe", "Staroe Krjukovo", "Poselenie Pervomajskoe",
                                  "Vnukovo", "Matushkino", "Troickij okrug", "Birjulevo Zapadnoe", "Kapotnja")] <- "Neigh5"
Test$neigh[Test$sub_area %in% c("Zapadnoe Degunino", "Poselenie Sosenskoe", "Savelki", "Poselenie Voskresenskoe")] <- "Neigh6"
Test$neigh[Test$sub_area %in% c("Nekrasovka", "Poselenie Moskovskij", "Birjulevo Vostochnoe",
                                  "Solncevo", "Poselenie Rjazanovskoe", "Metrogorodok", "Vostochnoe Izmajlovo",
                                  "Pechatniki", "Caricyno", "Dmitrovskoe", "Severnoe Izmajlovo", "Nizhegorodskoe",
                                  "Gol'janovo", "Veshnjaki", "Altuf'evskoe", "Severnoe", "Krjukovo", "Kuz'minki",
                                   "Poselenie Vnukovskoe", "Rjazanskij", "Ljublino", "Perovo",
                                  "Kosino-Uhtomskoe", "Silino", "Juzhnoe Butovo", "Novogireevo", "Bogorodskoe",
                                  "Marfino", "Vyhino-Zhulebino", "Ivanovskoe", "Jaroslavskoe", "Golovinskoe",
                                  "Poselenie Rogovskoe", "Juzhnoe Medvedkovo", "Brateevo", "Lianozovo",
                                  "Preobrazhenskoe", "Tekstil'shhiki", "Beskudnikovskoe", "Vostochnoe Degunino",
                                  "Novo-Peredelkino", "Orehovo-Borisovo Severnoe")] <- "Neigh7"
Test$neigh[Test$sub_area == "Tverskoe"] <- "Neigh8"
Test$neigh[Test$sub_area %in% c("Koptevo", "Juzhnoe Tushino", "Mozhajskoe", "Sokolinaja Gora",
                                  "Zjablikovo", "Bibirevo", "Chertanovo Central'noe", "Savelovskoe", "Kotlovka",
                                  "Orehovo-Borisovo Juzhnoe", "Chertanovo Juzhnoe", "Ochakovo-Matveevskoe",
                                  "Nagatino-Sadovniki", "Timirjazevskoe", "Otradnoe", "Chertanovo Severnoe",
                                  "Rostokino", "Severnoe Medvedkovo", "Babushkinskoe", "Mar'ino", "Lefortovo",
                                  "Severnoe Butovo", "Novokosino", "Losinoostrovskoe")] <- "Neigh9"
Test$neigh[Test$sub_area %in% c("Horoshevo-Mnevniki", "Zjuzino", "Fili Davydkovo", "Hovrino",
                                  "Pokrovskoe Streshnevo", "Severnoe Tushino", "Butyrskoe", "Sviblovo",
                                  "Mar'ina Roshha", "Moskvorech'e-Saburovo", "Nagatinskij Zaton",
                                  "Teplyj Stan", "Kon'kovo", "Ostankinskoe", "Jasenevo", "Juzhnoportovoe",
                                  "Levoberezhnoe", "Filevskij Park", "Alekseevskoe", "Mitino")] <- "Neigh10"
Test$neigh[Test$sub_area %in% c("Izmajlovo", "Kuncevo", "Shhukino", "Cheremushki", "Jakimanka",
                                  "Strogino", "Horoshevskoe", "Vojkovskoe", "Danilovskoe", "Donskoe",
                                  "Ajeroport", "Sokol", "Taganskoe", "Akademicheskoe", "Nagornoe", "Sokol'niki",
                                  "Begovoe", "Poselenie Kievskij", "Meshhanskoe", "Krasnosel'skoe", "Basmannoe",
                                  "Troparevo-Nikulino")] <- "Neigh11"
Test$neigh[Test$sub_area %in% c("Prospekt Vernadskogo", "Presnenskoe", "Ramenki", "Gagarinskoe",
                                  "Krylatskoe", "Arbat", "Dorogomilovo", "Obruchevskoe", "Zamoskvorech'e",
                                  "Lomonosovskoe", "Kurkino", "Hamovniki")] <- "Neigh12"
Test$neigh <- factor(Test$neigh)

neigh <- ggplot(aes(y = price_doc, x = reorder(neigh, price_doc, FUN = mean)), data = Train) +
                        geom_boxplot(fill = "#3366FF")
neigh + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#########################
# Cleanup Macro Data #
#########################

# Get a list of variables with missing values for Training data
mmv <- function() {
    macro.missing.values <- data.frame(matrix(ncol = 2, nrow = 0))
    cols <- c("variable", "numMissing")
    colnames(macro.missing.values) <- cols

    for (i in 1:length(Macro)) {

        if (sum(is.na(Macro[i])) > 0) {
            macro.missing.values[nrow(macro.missing.values) + 1,] <-
            c(noquote(names(Macro[i])), sum(is.na(Macro[i])))
        }
    }

    macro.missing.values
}

mmv()

str(Macro)

###################
# Macro clean-up  #
###################

# Create month variable from timestamp
Macro$month <- month(as.Date(Macro$timestamp), label = TRUE)

#######################################################
# Drop variables that have too many missing variables #
#######################################################
Macro$m_grp_growth <- 0
Macro$m_grp_growth[is.na(Macro$grp_growth)] <- 1
Macro$grp_growth <- NULL

Macro$m_load_of_teachers_preschool_per_teacher <- 0
Macro$m_load_of_teachers_preschool_per_teacher[is.na(Macro$load_of_teachers_preschool_per_teacher)] <- 1
Macro$load_of_teachers_preschool_per_teacher <- NULL

Macro$m_modern_education_share <- 0
Macro$m_modern_education_share[is.na(Macro$modern_education_share)] <- 1
Macro$modern_education_share <- NULL

Macro$m_old_education_build_share <- 0
Macro$m_old_education_build_share[is.na(Macro$old_education_build_share)] <- 1
Macro$old_education_build_share <- NULL

Macro$m_hospital_beds_available_per_cap <- 0
Macro$m_hospital_beds_available_per_cap[is.na(Macro$hospital_beds_available_per_cap)] <- 1
Macro$hospital_beds_available_per_cap <- NULL

Macro$m_hospital_bed_occupancy_per_year <- 0
Macro$m_hospital_bed_occupancy_per_year[is.na(Macro$hospital_bed_occupancy_per_year)] <- 1
Macro$hospital_bed_occupancy_per_year <- NULL

Macro$m_provision_retail_space_sqm <- 0
Macro$m_provision_retail_space_sqm[is.na(Macro$provision_retail_space_sqm)] <- 1
Macro$provision_retail_space_sqm <- NULL

Macro$m_provision_retail_space_modern_sqm <- 0
Macro$m_provision_retail_space_modern_sqm[is.na(Macro$provision_retail_space_modern_sqm)] <- 1
Macro$provision_retail_space_modern_sqm <- NULL

Macro$m_theaters_viewers_per_1000_cap <- 0
Macro$m_theaters_viewers_per_1000_cap[is.na(Macro$theaters_viewers_per_1000_cap)] <- 1
Macro$theaters_viewers_per_1000_cap <- NULL

Macro$m_museum_visitis_per_100_cap <- 0
Macro$m_museum_visitis_per_100_cap[is.na(Macro$museum_visitis_per_100_cap)] <- 1
Macro$museum_visitis_per_100_cap <- NULL

Macro$m_population_reg_sports_share <- 0
Macro$m_population_reg_sports_share[is.na(Macro$population_reg_sports_share)] <- 1
Macro$population_reg_sports_share <- NULL

Macro$m_students_reg_sports_share <- 0
Macro$m_students_reg_sports_share[is.na(Macro$students_reg_sports_share)] <- 1
Macro$students_reg_sports_share <- NULL

#####################
# Impute gdp_quart  #
#####################

Macro$m_gdp_quart <- 0
Macro$m_gdp_quart[is.na(Macro$gdp_quart)] <- 1

Macro$imp_gdp_quart <- Macro$gdp_quart

Macro$imp_gdp_quart[Macro$fixed_basket >= 1.378e+04 & Macro$deposits_value >= 1.687e+07
                                & is.na(Macro$imp_gdp_quart)] <- 19977.82
Macro$imp_gdp_quart[Macro$fixed_basket >= 1.378e+04 & Macro$deposits_value < 1.687e+07
                                & is.na(Macro$imp_gdp_quart)] <- 17478.27
Macro$imp_gdp_quart[Macro$fixed_basket < 1.378e+04 & Macro$average_provision_of_build_contract < 5.7
                                & is.na(Macro$imp_gdp_quart)] <- 11796.89
Macro$imp_gdp_quart[Macro$fixed_basket < 1.378e+04 & Macro$average_provision_of_build_contract >= 5.7
                                & is.na(Macro$imp_gdp_quart)] <- 15525.29
Macro$gdp_quart <- NULL

############################
# Impute gdp_quart_growth  #
############################

Macro$m_gdp_quart_growth <- 0
Macro$m_gdp_quart_growth[is.na(Macro$gdp_quart_growth)] <- 1

Macro$imp_gdp_quart_growth <- Macro$gdp_quart_growth

Macro$imp_gdp_quart_growth[Macro$fixed_basket < 1.466e+04 & Macro$deposits_value < 1.378e+04
                    & is.na(Macro$imp_gdp_quart_growth)] <- 4.35
Macro$imp_gdp_quart_growth[Macro$fixed_basket >= 1.466e+04 & Macro$deposits_value < 1.86e+04
                    & is.na(Macro$imp_gdp_quart_growth)] <- .97
Macro$imp_gdp_quart_growth[Macro$fixed_basket >= 1.466e+04 & Macro$deposits_value >= 1.86e+04
                    & is.na(Macro$imp_gdp_quart_growth)] <- 2.69
Macro$imp_gdp_quart_growth[Macro$fixed_basket < 1.466e+04 & Macro$deposits_value >= 1.378e+04
                    & is.na(Macro$imp_gdp_quart_growth)] <- 2.58
Macro$gdp_quart_growth <- NULL

###############
# Impute cpi  #
###############

Macro$m_cpi <- 0
Macro$m_cpi[is.na(Macro$cpi)] <- 1

Macro$imp_cpi <- Macro$cpi

Macro$imp_cpi[Macro$fixed_basket < 1.678e+04 & Macro$deposits_value < 1.295e+07
                           & is.na(Macro$imp_cpi)] <- 343.43
Macro$imp_cpi[Macro$fixed_basket < 1.678e+04 & Macro$deposits_value >= 1.295e+07
                           & is.na(Macro$imp_cpi)] <- 401.78
Macro$imp_cpi[Macro$fixed_basket >= 1.678e+044 & Macro$fixed_basket >= 1.918e+04
                           & is.na(Macro$imp_cpi)] <- 519.76
Macro$imp_cpi[Macro$fixed_basket >= 1.678e+044 & Macro$fixed_baske < 1.918e+04
                           & is.na(Macro$imp_cpi)] <- 480.77
Macro$cpi <- NULL

###############
# Impute ppi  #
###############

Macro$m_ppi <- 0
Macro$m_ppi[is.na(Macro$ppi)] <- 1

Macro$imp_ppi <- Macro$ppi

Macro$imp_ppi[Macro$imp_cpi < 413.2 & Macro$deposits_value >= 9.869e+06
              & is.na(Macro$imp_ppi)] <- 457.99
Macro$imp_ppi[Macro$imp_cpi < 413.2 & Macro$deposits_value < 9.869e+06
              & is.na(Macro$imp_ppi)] <- 369.35
Macro$imp_ppi[Macro$imp_cpi >= 413.2 & Macro$imp_gdp_quart_growth < -0.1
              & is.na(Macro$imp_ppi)] <- 579.12
Macro$imp_ppi[Macro$imp_cpi >= 413.2 & Macro$imp_gdp_quart_growth >= -0.1
              & is.na(Macro$imp_ppi)] <- 511.44
Macro$ppi <- NULL


######################
# Impute gdp_deflator  #
######################

Macro$m_gdp_deflator <- 0
Macro$m_gdp_deflator[is.na(Macro$gdp_deflator)] <- 1

Macro$imp_gdp_deflator <- Macro$gdp_deflator

Macro$imp_gdp_deflator[Macro$gdp_annual >= 6.897e+04 & Macro$gdp_annual_growth >= 0.00993
              & is.na(Macro$imp_gdp_deflator)] <- 113.47
Macro$imp_gdp_deflator[Macro$gdp_annual >= 6.897e+04 & Macro$gdp_annual_growth < 0.00993
              & is.na(Macro$imp_gdp_deflator)] <- 123.66
Macro$imp_gdp_deflator[Macro$gdp_annual < 6.897e+04 & Macro$gdp_annual >= 5.3e+04
              & is.na(Macro$imp_gdp_deflator)] <- 100
Macro$imp_gdp_deflator[Macro$gdp_annual < 6.897e+04 & Macro$gdp_annual < 5.3e+04
              & is.na(Macro$imp_gdp_deflator)] <- 86.72
Macro$gdp_deflator <- NULL


#########################
# Impute balance_trade  #
#########################

Macro$m_balance_trade <- 0
Macro$m_balance_trade[is.na(Macro$balance_trade)] <- 1

Macro$imp_balance_trade <- Macro$balance_trade

Macro$imp_balance_trade[Macro$imp_cpi < 492.4 & Macro$oil_urals >= 91.65
                       & is.na(Macro$imp_balance_trade)] <- 17.62
Macro$imp_balance_trade[Macro$imp_cpi < 492.4 & Macro$oil_urals < 91.65
                       & is.na(Macro$imp_balance_trade)] <- 14.56
Macro$imp_balance_trade[Macro$imp_cpi >= 492.4 & Macro$fixed_basket >= 1.95e+04
                       & is.na(Macro$imp_balance_trade)] <- 7.84
Macro$imp_balance_trade[Macro$imp_cpi >= 492.4 & Macro$fixed_basket < 1.95e+04
                        & is.na(Macro$imp_balance_trade)] <- 11
Macro$balance_trade <- NULL


################################
# Impute balance_trade_growth  #
################################

Macro$m_balance_trade_growth <- 0
Macro$m_balance_trade_growth[is.na(Macro$balance_trade_growth)] <- 1

Macro$imp_balance_trade_growth <- Macro$balance_trade_growth

Macro$imp_balance_trade_growth[Macro$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
                               & Macro$average_provision_of_build_contract < 6.105
                        & is.na(Macro$imp_balance_trade_growth)] <- 20.05
Macro$imp_balance_trade_growth[Macro$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
                               & Macro$average_provision_of_build_contract >= 6.105
                               & is.na(Macro$imp_balance_trade_growth)] <- 61.97
Macro$imp_balance_trade_growth[!(Macro$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))
                               & Macro$imp_balance_trade < 13.95
                               & is.na(Macro$imp_balance_trade_growth)] <- 1.65
Macro$imp_balance_trade_growth[!(Macro$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))
                               & Macro$imp_balance_trade >= 11.61
                               & is.na(Macro$imp_balance_trade_growth)] <- 1.65
Macro$balance_trade_growth <- NULL


#########################
# Impute usdrub  #
#########################

Macro$m_usdrub <- 0
Macro$m_usdrub[is.na(Macro$usdrub)] <- 1

Macro$imp_usdrub <- Macro$usdrub

Macro$imp_usdrub[Macro$oil_urals >= 68.17 & Macro$imp_cpi < 407.1
                        & is.na(Macro$imp_usdrub)] <- 30.72
Macro$imp_usdrub[Macro$oil_urals >= 68.17 & Macro$imp_cpi >= 407.1
                        & is.na(Macro$imp_usdrub)] <- 37.26
Macro$imp_usdrub[Macro$oil_urals < 68.17 & Macro$oil_urals < 50.99
                        & is.na(Macro$imp_usdrub)] <- 67.09
Macro$imp_usdrub[Macro$oil_urals < 68.17 & Macro$oil_urals >= 50.99
                        & is.na(Macro$imp_usdrub)] <- 56.65
Macro$usdrub <- NULL


##################
# Impute eurrub  #
##################

Macro$m_eurrub <- 0
Macro$m_eurrub[is.na(Macro$eurrub)] <- 1

Macro$imp_eurrub <- Macro$eurrub

Macro$imp_eurrub[Macro$imp_usdrub < 52.59 & Macro$imp_usdrub < 33.7
                 & is.na(Macro$imp_eurrub)] <- 40.89
Macro$imp_eurrub[Macro$imp_usdrub < 52.59 & Macro$imp_usdrub >= 33.7
                 & is.na(Macro$imp_eurrub)] <- 50.29
Macro$imp_eurrub[Macro$imp_usdrub >= 52.59 & Macro$imp_usdrub >= 63.05
                 & is.na(Macro$imp_eurrub)] <- 75.50
Macro$imp_eurrub[Macro$imp_usdrub >= 52.59 & Macro$imp_usdrub >= 63.05
                 & is.na(Macro$imp_eurrub)] <- 65.92
Macro$eurrub <- NULL


##################
# Impute brent   #
##################

Macro$m_brent <- 0
Macro$m_brent[is.na(Macro$brent)] <- 1

Macro$imp_brent <- Macro$brent

Macro$imp_brent[Macro$oil_urals >= 80.23 & Macro$oil_urals >= 98.32
                 & is.na(Macro$imp_brent)] <- 110.65
Macro$imp_brent[Macro$oil_urals >= 80.23 & Macro$oil_urals < 98.32
                 & is.na(Macro$imp_brent)] <- 90.74
Macro$imp_brent[Macro$oil_urals < 80.23 & Macro$imp_usdrub >= 56.26
                 & is.na(Macro$imp_brent)] <- 47.21
Macro$imp_brent[Macro$oil_urals < 80.23 & Macro$imp_usdrub < 56.26
                 & is.na(Macro$imp_brent)] <- 73.74
Macro$brent <- NULL

##############################
# Impute net_capital_export  #
##############################

Macro$m_net_capital_export <- 0
Macro$m_net_capital_export[is.na(Macro$net_capital_export)] <- 1

Macro$imp_net_capital_export <- Macro$net_capital_export

Macro$imp_net_capital_export[Macro$imp_eurrub < 49.92 & Macro$mortgage_rate >= 11.95
                & is.na(Macro$imp_net_capital_export)] <- .038
Macro$imp_net_capital_export[Macro$imp_eurrub < 49.92 & Macro$mortgage_rate < 11.95
                & is.na(Macro$imp_net_capital_export)] <- .41
Macro$imp_net_capital_export[Macro$imp_eurrub >= 49.92 & Macro$imp_balance_trade < 12.05
                & is.na(Macro$imp_net_capital_export)] <- -.41
Macro$imp_net_capital_export[Macro$imp_eurrub >= 49.92 & Macro$imp_balance_trade >= 12.05
                & is.na(Macro$imp_net_capital_export)] <- -.16
Macro$net_capital_export <- NULL


######################################################
# Impute average_provision_of_build_contract_moscow  #
######################################################

Macro$m_average_provision_of_build_contract_moscow <- 0
Macro$m_average_provision_of_build_contract_moscow[is.na(Macro$average_provision_of_build_contract_moscow)] <- 1

Macro$imp_average_provision_of_build_contract_moscow <- Macro$average_provision_of_build_contract_moscow

Macro$imp_average_provision_of_build_contract_moscow[Macro$imp_ppi < 541.8 & Macro$imp_balance_trade >= 15.54
                             & is.na(Macro$imp_average_provision_of_build_contract_moscow)] <- 6.32
Macro$imp_average_provision_of_build_contract_moscow[Macro$imp_ppi < 541.8 & Macro$imp_balance_trade < 15.54
                             & is.na(Macro$imp_average_provision_of_build_contract_moscow)] <- 6.66
Macro$imp_average_provision_of_build_contract_moscow[Macro$imp_ppi >= 541.8 & Macro$average_provision_of_build_contract < 5.955
                             & is.na(Macro$imp_average_provision_of_build_contract_moscow)] <- 6.67
Macro$imp_average_provision_of_build_contract_moscow[Macro$imp_ppi >= 541.8 & Macro$average_provision_of_build_contract >= 5.955
                             & is.na(Macro$imp_average_provision_of_build_contract_moscow)] <- 7.38
Macro$average_provision_of_build_contract_moscow <- NULL


##################
# Impute rts   #
##################

Macro$m_rts <- 0
Macro$m_rts[is.na(Macro$rts)] <- 1

Macro$imp_rts <- Macro$rts

Macro$imp_rts[Macro$imp_eurrub < 48.77 & Macro$imp_usdrub >= 29.94
                & is.na(Macro$imp_rts)] <- 1429.44
Macro$imp_rts[Macro$imp_eurrub < 48.77 & Macro$imp_usdrub < 29.94
                & is.na(Macro$imp_rts)] <- 1761.67
Macro$imp_rts[Macro$imp_eurrub >= 48.77 & Macro$imp_eurrub >= 60.65
                & is.na(Macro$imp_rts)] <- 862.06
Macro$imp_rts[Macro$imp_eurrub >= 48.77 & Macro$imp_eurrub < 60.65
                & is.na(Macro$imp_rts)] <- 1094.97
Macro$rts <- NULL

##########################
# Impute micex_rgbi_tr   #
##########################

Macro$m_micex_rgbi_tr <- 0
Macro$m_micex_rgbi_tr[is.na(Macro$micex_rgbi_tr)] <- 1

Macro$imp_micex_rgbi_tr <- Macro$micex_rgbi_tr

Macro$imp_micex_rgbi_tr[Macro$provision_nurse >= 94.65 & Macro$fixed_basket < 1.378e+04
              & is.na(Macro$imp_micex_rgbi_tr)] <- 131.38
Macro$imp_micex_rgbi_tr[Macro$provision_nurse >= 94.65 & Macro$fixed_basket >= 1.378e+04
              & is.na(Macro$imp_micex_rgbi_tr)] <- 136.11
Macro$imp_micex_rgbi_tr[Macro$provision_nurse < 94.65 & Macro$mortgage_rate >= 12.53
              & is.na(Macro$imp_micex_rgbi_tr)] <- 115.89
Macro$imp_micex_rgbi_tr[Macro$provision_nurse < 94.65 & Macro$mortgage_rate >= 12.53
              & is.na(Macro$imp_micex_rgbi_tr)] <- 126.21
Macro$micex_rgbi_tr <- NULL

##################
# Impute micex   #
##################

Macro$m_micex <- 0
Macro$m_micex[is.na(Macro$micex)] <- 1

Macro$imp_micex <- Macro$micex

Macro$imp_micex[Macro$imp_cpi < 460.7 & Macro$imp_usdrub >= 28.76
                & is.na(Macro$imp_micex)] <- 1451.49
Macro$imp_micex[Macro$imp_cpi < 460.7 & Macro$imp_usdrub < 28.76
                & is.na(Macro$imp_micex)] <- 1709.99
Macro$imp_micex[Macro$imp_cpi >= 460.7 & Macro$fixed_basket < 1.968e+04
                & is.na(Macro$imp_micex)] <- 1701.52
Macro$imp_micex[Macro$imp_cpi >= 460.7 & Macro$fixed_basket >= 1.968e+04
                & is.na(Macro$imp_micex)] <- 1927.63
Macro$micex <- NULL

#########################
# Impute micex_cbi_tr   #
#########################

Macro$m_micex_cbi_tr <- 0
Macro$m_micex_cbi_tr[is.na(Macro$micex_cbi_tr)] <- 1

Macro$imp_micex_cbi_tr <- Macro$micex_cbi_tr

Macro$imp_micex_cbi_tr[Macro$fixed_basket >= 1.466e+04 & Macro$gdp_annual < 7.937e+04
                & is.na(Macro$imp_micex_cbi_tr)] <- 249.42
Macro$imp_micex_cbi_tr[Macro$fixed_basket >= 1.466e+04 & Macro$gdp_annual >= 7.937e+04
                & is.na(Macro$imp_micex_cbi_tr)] <- 301.70
Macro$imp_micex_cbi_tr[Macro$fixed_basket < 1.466e+04 & Macro$imp_ppi >= 420.2
                & is.na(Macro$imp_micex_cbi_tr)] <- 212.04
Macro$imp_micex_cbi_tr[Macro$fixed_basket < 1.466e+04 & Macro$imp_ppi < 420.2
                & is.na(Macro$imp_micex_cbi_tr)] <- 189.49
Macro$micex_cbi_tr <- NULL


############################
# Impute deposits_growth   #
###########################

Macro$m_deposits_growth <- 0
Macro$m_deposits_growth[is.na(Macro$deposits_growth)] <- 1

Macro$imp_deposits_growth <- Macro$deposits_growth

Macro$imp_deposits_growth[Macro$month %in% c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                          & Macro$imp_gdp_quart >= 1.602e+04
                          & is.na(Macro$imp_deposits_growth)] <- 0.00662
Macro$imp_deposits_growth[Macro$month %in% c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                          & Macro$imp_gdp_quart < 1.602e+04
                          & is.na(Macro$imp_deposits_growth)] <- 0.016291
Macro$imp_deposits_growth[Macro$month %in% c("Jan") & is.na(Macro$imp_deposits_growth)] <- 0.05599

Macro$deposits_growth <- NULL


############################
# Impute mortgage_growth   #
############################

Macro$m_mortgage_growth <- 0
Macro$m_mortgage_growth[is.na(Macro$mortgage_growth)] <- 1

Macro$imp_mortgage_growth <- Macro$mortgage_growth

Macro$imp_mortgage_growth[Macro$fixed_basket >= 1.358e+04 & Macro$imp_gdp_quart_growth >= -2
                       & is.na(Macro$imp_mortgage_growth)] <- 0.373965
Macro$imp_mortgage_growth[Macro$fixed_basket >= 1.358e+04 & Macro$imp_gdp_quart_growth < -2
                       & is.na(Macro$imp_mortgage_growth)] <- -0.29026
Macro$imp_mortgage_growth[Macro$fixed_basket < 1.358e+04 & Macro$imp_cpi >= 350.8
                       & is.na(Macro$imp_mortgage_growth)] <- 0.95703
Macro$imp_mortgage_growth[Macro$fixed_basket < 1.358e+04 & Macro$imp_cpi < 350.8
                       & is.na(Macro$imp_mortgage_growth)] <- 1.39625
Macro$mortgage_growth <- NULL


#########################
# Impute deposits_rate   #
#########################

Macro$m_deposits_rate <- 0
Macro$m_deposits_rate[is.na(Macro$deposits_rate)] <- 1

Macro$imp_deposits_rate <- Macro$deposits_rate

Macro$imp_deposits_rate[Macro$imp_cpi < 437.7 & Macro$imp_ppi >= 439.4
                        & is.na(Macro$imp_deposits_rate)] <- 5.53
Macro$imp_deposits_rate[Macro$imp_cpi < 437.7 & Macro$imp_ppi < 439.4
                        & is.na(Macro$imp_deposits_rate)] <- 4.21
Macro$imp_deposits_rate[Macro$imp_cpi >= 437.7 & Macro$fixed_basket >= 1.86e+04
                        & is.na(Macro$imp_deposits_rate)] <- 7.89
Macro$imp_deposits_rate[Macro$imp_cpi >= 437.7 & Macro$fixed_basket < 1.86e+04
                        & is.na(Macro$imp_deposits_rate)] <- 11.74
Macro$deposits_rate <- NULL

################
# Impute grp   #
################

Macro$m_grp <- 0
Macro$m_grp[is.na(Macro$grp)] <- 1

Macro$imp_grp <- Macro$grp

Macro$imp_grp[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                        & is.na(Macro$imp_grp)] <- 10666.87
Macro$imp_grp[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                        & is.na(Macro$imp_grp)] <- 12808.5734
Macro$imp_grp[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                        & is.na(Macro$imp_grp)] <- 9948.77
Macro$imp_grp[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                        & is.na(Macro$imp_grp)] <- 8375.8638
Macro$grp <- NULL

############################
# Impute income_per_cap   #
###########################

Macro$m_income_per_cap <- 0
Macro$m_income_per_cap[is.na(Macro$income_per_cap)] <- 1

Macro$imp_income_per_cap <- Macro$income_per_cap

Macro$imp_income_per_cap[Macro$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
                          & Macro$imp_gdp_quart_growth >= 1.65
                          & is.na(Macro$imp_income_per_cap)] <- 43943.01
Macro$imp_income_per_cap[Macro$month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
                          & Macro$imp_gdp_quart_growth < 1.65
                          & is.na(Macro$imp_income_per_cap)] <- 54563.48
Macro$imp_income_per_cap[Macro$month %in% c("Dec") & is.na(Macro$imp_income_per_cap)] <- 87496.13

Macro$income_per_cap <- NULL

##############################################
# Impute real_dispos_income_per_cap_growth   #
##############################################

Macro$m_real_dispos_income_per_cap_growth <- 0
Macro$m_real_dispos_income_per_cap_growth[is.na(Macro$real_dispos_income_per_cap_growth)] <- 1

Macro$imp_real_dispos_income_per_cap_growth <- Macro$real_dispos_income_per_cap_growth

Macro$imp_real_dispos_income_per_cap_growth[Macro$provision_nurse >= 98.65 & Macro$oil_urals < 91.69
                         & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- 0.038
Macro$imp_real_dispos_income_per_cap_growth[Macro$provision_nurse >= 98.65 & Macro$oil_urals >= 91.69
                         & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- -0.005
Macro$imp_real_dispos_income_per_cap_growth[Macro$provision_nurse < 98.65 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- -0.025
Macro$imp_real_dispos_income_per_cap_growth[Macro$provision_nurse < 98.65 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- -0.079
Macro$real_dispos_income_per_cap_growth <- NULL

Macro$imp_real_dispos_income_per_cap_growth[Macro$imp_mortgage_growth >= 0.959 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- 0.038
Macro$imp_real_dispos_income_per_cap_growth[Macro$imp_mortgage_growth < 0.959 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- -0.005
Macro$imp_real_dispos_income_per_cap_growth[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- -0.025
Macro$imp_real_dispos_income_per_cap_growth[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_real_dispos_income_per_cap_growth)] <- -0.079

####################
# Impute salary   #
###################

Macro$m_salary <- 0
Macro$m_salary[is.na(Macro$salary)] <- 1

Macro$imp_salary <- Macro$salary

Macro$imp_salary[Macro$gdp_annual_growth >= 0.02399 & Macro$gdp_annual_growth < 0.04384
                                            & is.na(Macro$imp_salary)] <- 48830
Macro$imp_salary[Macro$gdp_annual_growth >= 0.02399 & Macro$gdp_annual_growth >= 0.04384
                                            & is.na(Macro$imp_salary)] <- 44899
Macro$imp_salary[Macro$gdp_annual_growth < 0.02399 & Macro$gdp_annual >= 5.491e+04
                                            & is.na(Macro$imp_salary)] <- 61208
Macro$imp_salary[Macro$gdp_annual_growth < 0.02399 & Macro$gdp_annual < 5.491e+04
                                            & is.na(Macro$imp_salary)] <- 38410
Macro$salary <- NULL


##########################
# Impute salary_growth   #
##########################

Macro$m_salary_growth <- 0
Macro$m_salary_growth[is.na(Macro$salary_growth)] <- 1

Macro$imp_salary_growth <- Macro$salary_growth

Macro$imp_salary_growth[Macro$gdp_annual_growth < 0.03891 & Macro$gdp_annual < 7.448e+04
                                            & is.na(Macro$imp_salary_growth)] <- 0.10314
Macro$imp_salary_growth[Macro$gdp_annual_growth < 0.03891 & Macro$gdp_annual >= 7.448e+04
                                            & is.na(Macro$imp_salary_growth)] <- 0.05067
Macro$imp_salary_growth[Macro$gdp_annual_growth >= 0.03891 & Macro$gdp_annual < 5.3e+04
                                            & is.na(Macro$imp_salary_growth)] <- 0.1689
Macro$imp_salary_growth[Macro$gdp_annual_growth >= 0.03891 & Macro$gdp_annual >= 5.3e+04
                                            & is.na(Macro$imp_salary_growth)] <- 0.087568
Macro$salary_growth <- NULL


#########################################
# Impute retail_trade_turnover_per_cap   #
#########################################

Macro$m_retail_trade_turnover_per_cap <- 0
Macro$m_retail_trade_turnover_per_cap[is.na(Macro$retail_trade_turnover_per_cap)] <- 1

Macro$imp_retail_trade_turnover_per_cap <- Macro$retail_trade_turnover_per_cap

Macro$imp_retail_trade_turnover_per_cap[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_retail_trade_turnover_per_cap)] <- 365.089
Macro$imp_retail_trade_turnover_per_cap[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_retail_trade_turnover_per_cap)] <- 286.952
Macro$imp_retail_trade_turnover_per_cap[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_retail_trade_turnover_per_cap)] <- 351.447778
Macro$imp_retail_trade_turnover_per_cap[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_retail_trade_turnover_per_cap)] <- 251.484
Macro$retail_trade_turnover_per_cap <- NULL

##################################
# Impute retail_trade_turnover   #
##################################

Macro$m_retail_trade_turnover <- 0
Macro$m_retail_trade_turnover[is.na(Macro$retail_trade_turnover)] <- 1

Macro$imp_retail_trade_turnover <- Macro$retail_trade_turnover

Macro$imp_retail_trade_turnover[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_retail_trade_turnover)] <- 4436.898
Macro$imp_retail_trade_turnover[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_retail_trade_turnover)] <- 3322.047
Macro$imp_retail_trade_turnover[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_retail_trade_turnover)] <- 4310.1067
Macro$imp_retail_trade_turnover[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                                            & is.na(Macro$imp_retail_trade_turnover)] <- 2882.4169
Macro$retail_trade_turnover <- NULL

########################################
# Impute retail_trade_turnover_growth  #
########################################

Macro$m_retail_trade_turnover_growth <- 0
Macro$m_retail_trade_turnover_growth[is.na(Macro$retail_trade_turnover_growth)] <- 1

Macro$imp_labor_force <- Macro$retail_trade_turnover_growth

Macro$imp_retail_trade_turnover_growth[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_retail_trade_turnover_growth)] <- 102.4
Macro$imp_retail_trade_turnover_growth[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_retail_trade_turnover_growth)] <- 106.6
Macro$imp_retail_trade_turnover_growth[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_retail_trade_turnover_growth)] <- 82.8
Macro$imp_retail_trade_turnover_growth[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                                            & is.na(Macro$imp_retail_trade_turnover_growth)] <- 106.8
Macro$retail_trade_turnover_growth <- NULL

#######################
# Impute labor_force  #
#######################

Macro$m_labor_force <- 0
Macro$m_labor_force[is.na(Macro$labor_force)] <- 1

Macro$imp_labor_force <- Macro$labor_force

Macro$imp_labor_force[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_labor_force)] <- 7087.326
Macro$imp_labor_force[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_labor_force)] <- 6643.626
Macro$imp_labor_force[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_labor_force)] <- 7067.470079
Macro$imp_labor_force[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                                            & is.na(Macro$imp_labor_force)] <- 6436.244
Macro$labor_force <- NULL

#######################
# Impute unemployment  #
#######################

Macro$m_unemployment <- 0
Macro$m_unemployment[is.na(Macro$unemployment)] <- 1

Macro$imp_unemployment <- Macro$unemployment

Macro$imp_unemployment[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_unemployment)] <- 0.015
Macro$imp_unemployment[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_unemployment)] <- 0.014
Macro$imp_unemployment[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_unemployment)] <- 0.017708
Macro$imp_unemployment[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                                            & is.na(Macro$imp_unemployment)] <- 0.018
Macro$unemployment <- NULL

#######################
# Impute employment  #
#######################

Macro$m_employment <- 0
Macro$m_employment[is.na(Macro$employment)] <- 1

Macro$imp_employment <- Macro$employment

Macro$imp_employment[Macro$imp_unemployment >= 0.016 & Macro$gdp_annual >= 5.287e+04
                                            & is.na(Macro$imp_employment)] <- 0.733598
Macro$imp_employment[Macro$imp_unemployment >= 0.016 & Macro$gdp_annual < 5.287e+04
                                            & is.na(Macro$imp_employment)] <- 0.69
Macro$imp_employment[Macro$imp_unemployment < 0.016 & Macro$gdp_annual >= 5.3e+04
                                            & is.na(Macro$imp_employment)] <- 0.74
Macro$imp_employment[Macro$imp_unemployment < 0.016 & Macro$gdp_annual < 5.3e+04
                                            & is.na(Macro$imp_employment)] <- 0.708
Macro$employment <- NULL

########################################
# Impute invest_fixed_capital_per_cap  #
########################################

Macro$m_invest_fixed_capital_per_cap <- 0
Macro$m_invest_fixed_capital_per_cap[is.na(Macro$invest_fixed_capital_per_cap)] <- 1

Macro$imp_invest_fixed_capital_per_cap <- Macro$invest_fixed_capital_per_cap

Macro$imp_invest_fixed_capital_per_cap[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_invest_fixed_capital_per_cap)] <- 126874
Macro$imp_invest_fixed_capital_per_cap[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_invest_fixed_capital_per_cap)] <- 73976
Macro$imp_invest_fixed_capital_per_cap[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_invest_fixed_capital_per_cap)] <- 131403
Macro$imp_invest_fixed_capital_per_cap[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                                            & is.na(Macro$imp_invest_fixed_capital_per_cap)] <- 63932
Macro$invest_fixed_capital_per_cap <- NULL

###############################
# Impute invest_fixed_assets  #
###############################

Macro$m_invest_fixed_assets <- 0
Macro$m_invest_fixed_assets[is.na(Macro$invest_fixed_assets)] <- 1

Macro$imp_invest_fixed_assets <- Macro$invest_fixed_assets

Macro$imp_invest_fixed_assets[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual >= 5.662e+04
                                            & is.na(Macro$imp_invest_fixed_assets)] <- 1542
Macro$imp_invest_fixed_assets[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                                            & is.na(Macro$imp_invest_fixed_assets)] <- 856
Macro$imp_invest_fixed_assets[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                                            & is.na(Macro$imp_invest_fixed_assets)] <- 1612
Macro$imp_invest_fixed_assets[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                                            & is.na(Macro$imp_invest_fixed_assets)] <- 733
Macro$invest_fixed_assets <- NULL

####################################
# Impute profitable_enterpr_share  #
####################################

Macro$m_profitable_enterpr_share <- 0
Macro$m_profitable_enterpr_share[is.na(Macro$profitable_enterpr_share)] <- 1

Macro$imp_profitable_enterpr_share <- Macro$profitable_enterpr_share

Macro$imp_profitable_enterpr_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_profitable_enterpr_share)] <- .708
Macro$imp_profitable_enterpr_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_profitable_enterpr_share)] <- .716
Macro$imp_profitable_enterpr_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_profitable_enterpr_share)] <- .702
Macro$imp_profitable_enterpr_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_profitable_enterpr_share)] <- .646
Macro$profitable_enterpr_share <- NULL

####################################
# Impute unprofitable_enterpr_share  #
####################################

Macro$m_unprofitable_enterpr_share <- 0
Macro$m_unprofitable_enterpr_share[is.na(Macro$unprofitable_enterpr_share)] <- 1

Macro$imp_unprofitable_enterpr_share <- Macro$unprofitable_enterpr_share

Macro$imp_unprofitable_enterpr_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_unprofitable_enterpr_share)] <- .298
Macro$imp_unprofitable_enterpr_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_unprofitable_enterpr_share)] <- .354
Macro$imp_unprofitable_enterpr_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_unprofitable_enterpr_share)] <- .292
Macro$imp_unprofitable_enterpr_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_unprofitable_enterpr_share)] <- .284
Macro$unprofitable_enterpr_share <- NULL

##############################
# Impute share_own_revenues  #
##############################

Macro$m_share_own_revenues <- 0
Macro$m_share_own_revenues[is.na(Macro$share_own_revenues)] <- 1

Macro$imp_share_own_revenues <- Macro$share_own_revenues

Macro$imp_share_own_revenues[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_share_own_revenues)] <- .9421
Macro$imp_share_own_revenues[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_share_own_revenues)] <- 9778
Macro$imp_share_own_revenues[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_share_own_revenues)] <- .9629
Macro$imp_share_own_revenues[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_share_own_revenues)] <- .8915
Macro$share_own_revenues <- NULL

##############################
# Impute overdue_wages_per_cap  #
##############################

Macro$m_overdue_wages_per_cap <- 0
Macro$m_overdue_wages_per_cap[is.na(Macro$overdue_wages_per_cap)] <- 1

Macro$imp_overdue_wages_per_cap <- Macro$overdue_wages_per_cap

Macro$imp_overdue_wages_per_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_overdue_wages_per_cap)] <- 57746
Macro$imp_overdue_wages_per_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_overdue_wages_per_cap)] <- 47563
Macro$imp_overdue_wages_per_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_overdue_wages_per_cap)] <- 53636
Macro$imp_overdue_wages_per_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_overdue_wages_per_cap)] <- 12865
Macro$overdue_wages_per_cap <- NULL


##############################
# Impute fin_res_per_cap  #
##############################

Macro$m_fin_res_per_cap <- 0
Macro$m_fin_res_per_cap[is.na(Macro$fin_res_per_cap)] <- 1

Macro$imp_fin_res_per_cap <- Macro$fin_res_per_cap

Macro$imp_fin_res_per_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_fin_res_per_cap)] <- 96.91
Macro$imp_fin_res_per_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_fin_res_per_cap)] <- 249.28
Macro$imp_fin_res_per_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_fin_res_per_cap)] <- 226.21
Macro$imp_fin_res_per_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_fin_res_per_cap)] <- 233.35
Macro$fin_res_per_cap <- NULL

##################################
# Impute marriages_per_1000_cap  #
##################################

Macro$m_marriages_per_1000_cap <- 0
Macro$m_marriages_per_1000_cap[is.na(Macro$marriages_per_1000_cap)] <- 1

Macro$imp_marriages_per_1000_cap <- Macro$marriages_per_1000_cap

Macro$imp_marriages_per_1000_cap[Macro$gdp_annual_growth >= 0.03891 & Macro$gdp_annual < 5.3e+04
                                            & is.na(Macro$imp_marriages_per_1000_cap)] <- 8.5
Macro$imp_marriages_per_1000_cap[Macro$gdp_annual_growth >= 0.03891 & Macro$gdp_annual >= 5.3e+04
                                            & is.na(Macro$imp_marriages_per_1000_cap)] <- 7.8
Macro$imp_marriages_per_1000_cap[Macro$gdp_annual_growth < 0.03891 & Macro$imp_fin_res_per_cap < 147.5
                                            & is.na(Macro$imp_marriages_per_1000_cap)] <- 8.3
Macro$imp_marriages_per_1000_cap[Macro$gdp_annual_growth < 0.03891 & Macro$imp_fin_res_per_cap >= 147.5
                                            & is.na(Macro$imp_marriages_per_1000_cap)] <- 8
Macro$marriages_per_1000_cap <- NULL

##################################
# Impute divorce_rate  #
##################################

Macro$m_divorce_rate <- 0
Macro$m_divorce_rate[is.na(Macro$divorce_rate)] <- 1

Macro$imp_divorce_rate <- Macro$divorce_rate

Macro$imp_divorce_rate[Macro$gdp_annual_growth < 0.02399 & Macro$gdp_annual < 5.491e+04
                                            & is.na(Macro$imp_divorce_rate)] <- 3.9
Macro$imp_divorce_rate[Macro$gdp_annual_growth < 0.02399 & Macro$gdp_annual >= 5.491e+04
                                            & is.na(Macro$imp_divorce_rate)] <- 3.7
Macro$imp_divorce_rate[Macro$gdp_annual_growth >= 0.02399
                            & Macro$imp_real_dispos_income_per_cap_growth >= -0.015
                                            & is.na(Macro$imp_divorce_rate)] <- 3.8
Macro$imp_divorce_rate[Macro$gdp_annual_growth >= 0.02399
                            & Macro$imp_real_dispos_income_per_cap_growth < -0.015
                                            & is.na(Macro$imp_divorce_rate)] <- 3.6
Macro$divorce_rate <- NULL

##############################
# Impute construction_value  #
##############################

Macro$m_construction_value <- 0
Macro$m_construction_value[is.na(Macro$construction_value)] <- 1

Macro$imp_construction_value <- Macro$construction_value

Macro$imp_construction_value[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_construction_value)] <- 734697.6
Macro$imp_construction_value[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_construction_value)] <- 609729.3
Macro$imp_construction_value[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_construction_value)] <- 549075.8
Macro$imp_construction_value[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_construction_value)] <- 553874.9
Macro$construction_value <- NULL

###################################
# Impute invest_fixed_assets_phys  #
###################################

Macro$m_invest_fixed_assets_phys <- 0
Macro$m_invest_fixed_assets_phys[is.na(Macro$invest_fixed_assets_phys)] <- 1

Macro$imp_invest_fixed_assets_phys <- Macro$invest_fixed_assets_phys

Macro$imp_invest_fixed_assets_phys[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_invest_fixed_assets_phys)] <- 133.1
Macro$imp_invest_fixed_assets_phys[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_invest_fixed_assets_phys)] <- 100.1
Macro$imp_invest_fixed_assets_phys[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_invest_fixed_assets_phys)] <- 106.6
Macro$imp_invest_fixed_assets_phys[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_invest_fixed_assets_phys)] <- 95.8
Macro$invest_fixed_assets_phys <- NULL

###################################
# Impute pop_natural_increase  #
###################################

Macro$m_pop_natural_increase <- 0
Macro$m_pop_natural_increase[is.na(Macro$pop_natural_increase)] <- 1

Macro$imp_pop_natural_increase <- Macro$pop_natural_increase

Macro$imp_pop_natural_increase[Macro$imp_unemployment < 0.016 & Macro$gdp_annual_growth < 0.04384
                                            & is.na(Macro$imp_pop_natural_increase)] <- 1.6
Macro$imp_pop_natural_increase[Macro$imp_unemployment < 0.016 & Macro$gdp_annual_growth >= 0.04384
                                            & is.na(Macro$imp_pop_natural_increase)] <- 1.1
Macro$imp_pop_natural_increase[Macro$imp_unemployment >= 0.016 & Macro$imp_gdp_quart_growth < 2.29
                                            & is.na(Macro$imp_pop_natural_increase)] <- 1.66
Macro$imp_pop_natural_increase[Macro$imp_unemployment >= 0.016 & Macro$imp_gdp_quart_growth >= 2.29
                                            & is.na(Macro$imp_pop_natural_increase)] <- -0.3
Macro$pop_natural_increase <- NULL

##############################
# Impute pop_migration  #
##############################

Macro$m_pop_migration <- 0
Macro$m_pop_migration[is.na(Macro$pop_migration)] <- 1

Macro$imp_pop_migration <- Macro$pop_migration

Macro$imp_pop_migration[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_pop_migration)] <- 5.7
Macro$imp_pop_migration[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_pop_migration)] <- 8.9
Macro$imp_pop_migration[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_pop_migration)] <- 5.1
Macro$imp_pop_migration[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_pop_migration)] <- 14.1
Macro$pop_migration <- NULL

##############################
# Impute pop_total_inc  #
##############################

Macro$m_pop_total_inc <- 0
Macro$m_pop_total_inc[is.na(Macro$pop_total_inc)] <- 1

Macro$imp_pop_total_inc <- Macro$pop_total_inc

Macro$imp_pop_total_inc[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                            & is.na(Macro$imp_pop_total_inc)] <- 7.3
Macro$imp_pop_total_inc[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                            & is.na(Macro$imp_pop_total_inc)] <- 10.3
Macro$imp_pop_total_inc[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_pop_total_inc)] <- 6.2
Macro$imp_pop_total_inc[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_pop_total_inc)] <- 13.8
Macro$pop_total_inc <- NULL

######################
# Impute childbirth  #
######################

Macro$m_childbirth <- 0
Macro$m_childbirth[is.na(Macro$childbirth)] <- 1

Macro$imp_childbirth <- Macro$childbirth

Macro$imp_childbirth[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 7.448e+04
                                            & is.na(Macro$imp_childbirth)] <- 11.55
Macro$imp_childbirth[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 7.448e+04
                                            & is.na(Macro$imp_childbirth)] <- 11.3
Macro$imp_childbirth[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                            & is.na(Macro$imp_childbirth)] <- 10.8
Macro$imp_childbirth[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                            & is.na(Macro$imp_childbirth)] <- 10.7
Macro$childbirth <- NULL

######################
# Impute mortality  #
######################

Macro$m_mortality <- 0
Macro$m_mortality[is.na(Macro$mortality)] <- 1

Macro$imp_mortality <- Macro$mortality

Macro$imp_mortality[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                     & is.na(Macro$imp_mortality)] <- 9.89
Macro$imp_mortality[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                     & is.na(Macro$imp_mortality)] <- 11
Macro$imp_mortality[Macro$imp_salary_growth >= 0.1031 & Macro$imp_real_dispos_income_per_cap_growth < 0.0245
                     & is.na(Macro$imp_mortality)] <- 9.7
Macro$imp_mortality[Macro$imp_salary_growth >= 0.1031 & Macro$imp_real_dispos_income_per_cap_growth >= 0.0245
                     & is.na(Macro$imp_mortality)] <- 9.6
Macro$mortality <- NULL


##############################
# Impute  housing_fund_sqm  #
##############################

Macro$m_housing_fund_sqm <- 0
Macro$m_housing_fund_sqm[is.na(Macro$housing_fund_sqm)] <- 1

Macro$imp_housing_fund_sqm <- Macro$housing_fund_sqm

Macro$imp_housing_fund_sqm[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                        & is.na(Macro$imp_housing_fund_sqm)] <- 234
Macro$imp_housing_fund_sqm[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                        & is.na(Macro$imp_housing_fund_sqm)] <- 231
Macro$imp_housing_fund_sqm[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                        & is.na(Macro$imp_housing_fund_sqm)] <- 218
Macro$imp_housing_fund_sqm[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                        & is.na(Macro$imp_housing_fund_sqm)] <- 2126
Macro$housing_fund_sqm <- NULL


##############################
# Impute lodging_sqm_per_cap  #
##############################

Macro$m_lodging_sqm_per_cap <- 0
Macro$m_lodging_sqm_per_cap[is.na(Macro$lodging_sqm_per_cap)] <- 1

Macro$imp_lodging_sqm_per_cap <- Macro$lodging_sqm_per_cap

Macro$imp_lodging_sqm_per_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                           & is.na(Macro$imp_lodging_sqm_per_cap)] <- 19.19
Macro$imp_lodging_sqm_per_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                           & is.na(Macro$imp_lodging_sqm_per_cap)] <- 19.28
Macro$imp_lodging_sqm_per_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                           & is.na(Macro$imp_lodging_sqm_per_cap)] <- 18.77
Macro$imp_lodging_sqm_per_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                           & is.na(Macro$imp_lodging_sqm_per_cap)] <- 18.72
Macro$lodging_sqm_per_cap <- NULL

##############################
# Impute water_pipes_share  #
##############################

Macro$m_water_pipes_share <- 0
Macro$m_water_pipes_share[is.na(Macro$water_pipes_share)] <- 1

Macro$imp_water_pipes_share <- Macro$water_pipes_share

Macro$imp_water_pipes_share[Macro$gdp_annual < 5.3e+04 & is.na(Macro$imp_water_pipes_share)] <- 99.9
Macro$imp_water_pipes_share[Macro$gdp_annual >= 5.3e+04 & is.na(Macro$imp_water_pipes_share)] <- 99.8

Macro$water_pipes_share <- NULL

#######################
# Impute baths_share  #
#######################

Macro$m_baths_share <- 0
Macro$m_baths_share[is.na(Macro$baths_share)] <- 1

Macro$imp_baths_share <- Macro$baths_share

Macro$imp_baths_share[Macro$gdp_annual < 5.3e+04 & is.na(Macro$imp_baths_share)] <- 99.9
Macro$imp_baths_share[Macro$gdp_annual >= 5.3e+04 & is.na(Macro$imp_baths_share)] <- 98.6

Macro$baths_share <- NULL

##############################
# Impute sewerage_share  #
##############################

Macro$m_sewerage_share <- 0
Macro$m_sewerage_share[is.na(Macro$sewerage_share)] <- 1

Macro$imp_sewerage_share <- Macro$sewerage_share

Macro$imp_sewerage_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                              & is.na(Macro$imp_sewerage_share)] <- 98.1
Macro$imp_sewerage_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                              & is.na(Macro$imp_sewerage_share)] <- 98
Macro$imp_sewerage_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                              & is.na(Macro$imp_sewerage_share)] <- 99.5
Macro$imp_sewerage_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                              & is.na(Macro$imp_sewerage_share)] <- 99.6
Macro$sewerage_share <- NULL

#####################
# Impute gas_share  #
#####################

Macro$m_gas_share <- 0
Macro$m_gas_share[is.na(Macro$gas_share)] <- 1

Macro$imp_gas_share <- Macro$gas_share

Macro$imp_gas_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                         & is.na(Macro$imp_gas_share)] <- 43.3
Macro$imp_gas_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                         & is.na(Macro$imp_gas_share)] <- 44.1
Macro$imp_gas_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                         & is.na(Macro$imp_gas_share)] <- 43.9
Macro$imp_gas_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                         & is.na(Macro$imp_gas_share)] <- 44.4
Macro$gas_share <- NULL

##########################
# Impute hot_water_share  #
##########################

Macro$m_hot_water_share <- 0
Macro$m_hot_water_share[is.na(Macro$hot_water_share)] <- 1

Macro$imp_hot_water_share <- Macro$hot_water_share

Macro$imp_hot_water_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.331e+04
                    & is.na(Macro$imp_hot_water_share)] <- 94.3
Macro$imp_hot_water_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.331e+04
                    & is.na(Macro$imp_hot_water_share)] <- 94.4
Macro$imp_hot_water_share[Macro$gdp_annual < 5.3e+04 & is.na(Macro$imp_hot_water_share)] <- 95.7

Macro$hot_water_share <- NULL

###################################
# Impute child_on_acc_pre_school  #
###################################

Macro$child_on_acc_pre_school <- as.numeric(gsub(",", "", Macro$child_on_acc_pre_school))
str(Macro$child_on_acc_pre_school)

Macro$m_child_on_acc_pre_school <- 0
Macro$m_child_on_acc_pre_school[is.na(Macro$child_on_acc_pre_school)] <- 1

Macro$imp_child_on_acc_pre_school <- Macro$child_on_acc_pre_school

Macro$imp_child_on_acc_pre_school[Macro$imp_real_dispos_income_per_cap_growth >= 0.0065 & Macro$oil_urals < 95.78
                    & is.na(Macro$imp_child_on_acc_pre_school)] <- 45713
Macro$imp_child_on_acc_pre_school[Macro$Macro$imp_real_dispos_income_per_cap_growth >= 0.0065 & Macro$oil_urals >= 95.78
                    & is.na(Macro$imp_child_on_acc_pre_school)] <- 3013
Macro$imp_child_on_acc_pre_school[Macro$imp_real_dispos_income_per_cap_growth < 0.0065 & Macro$gdp_annual_growth < 0.02772
                    & is.na(Macro$imp_child_on_acc_pre_school)] <- 16765
Macro$imp_child_on_acc_pre_school[Macro$imp_real_dispos_income_per_cap_growth < 0.0065 & Macro$gdp_annual_growth >= 0.02772
                    & is.na(Macro$imp_child_on_acc_pre_school)] <- 7311
Macro$child_on_acc_pre_school <- NULL


################################
# Impute electric_stove_share  #
################################

Macro$m_electric_stove_share <- 0
Macro$m_electric_stove_share[is.na(Macro$electric_stove_share)] <- 1

Macro$imp_electric_stove_share <- Macro$electric_stove_share

Macro$imp_electric_stove_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                    & is.na(Macro$imp_electric_stove_share)] <- 55.2
Macro$imp_electric_stove_share[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                    & is.na(Macro$imp_electric_stove_share)] <- 54.1
Macro$imp_electric_stove_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                    & is.na(Macro$imp_electric_stove_share)] <- 55.3
Macro$imp_electric_stove_share[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                    & is.na(Macro$imp_electric_stove_share)] <- 55
Macro$electric_stove_share <- NULL

#######################
# Impute heating_share  #
#######################

Macro$m_heating_share <- 0
Macro$m_heating_share[is.na(Macro$heating_share)] <- 1

Macro$imp_heating_share <- Macro$heating_share

Macro$imp_heating_share[Macro$gdp_annual < 5.3e+04 & is.na(Macro$imp_heating_share)] <- 99.9
Macro$imp_heating_share[Macro$gdp_annual >= 5.3e+04 & is.na(Macro$imp_heating_share)] <- 99.3

Macro$heating_share <- NULL

#######################
# Impute old_house_share  #
#######################

Macro$m_old_house_share <- 0
Macro$m_old_house_share[is.na(Macro$old_house_share)] <- 1

Macro$imp_old_house_share <- Macro$old_house_share

Macro$imp_old_house_share[Macro$gdp_annual < 5.3e+04 & is.na(Macro$imp_old_house_share)] <- .3
Macro$imp_old_house_share[Macro$gdp_annual >= 5.3e+04 & is.na(Macro$imp_old_house_share)] <- .4

Macro$old_house_share <- NULL

###############################################
# Impute seats_theather_rfmin_per_100000_cap  #
###############################################

Macro$m_seats_theather_rfmin_per_100000_cap <- 0
Macro$m_seats_theather_rfmin_per_100000_cap[is.na(Macro$seats_theather_rfmin_per_100000_cap)] <- 1

Macro$imp_seats_theather_rfmin_per_100000_cap <- Macro$seats_theather_rfmin_per_100000_cap

Macro$imp_seats_theather_rfmin_per_100000_cap[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual >= 4.925e+04
                               & is.na(Macro$imp_seats_theather_rfmin_per_100000_cap)] <- .45888
Macro$imp_seats_theather_rfmin_per_100000_cap[Macro$imp_salary_growth < 0.1031 & Macro$gdp_annual < 4.925e+04
                               & is.na(Macro$imp_seats_theather_rfmin_per_100000_cap)] <- .41
Macro$imp_seats_theather_rfmin_per_100000_cap[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                               & is.na(Macro$imp_seats_theather_rfmin_per_100000_cap)] <- .45356
Macro$imp_seats_theather_rfmin_per_100000_cap[Macro$imp_salary_growth >= 0.1031 & Macro$gdp_annual < 5.662e+04
                               & is.na(Macro$imp_seats_theather_rfmin_per_100000_cap)] <- .44784
Macro$seats_theather_rfmin_per_100000_cap <- NULL

############################
# Impute average_life_exp  #
############################

Macro$m_average_life_exp <- 0
Macro$m_average_life_exp[is.na(Macro$average_life_exp)] <- 1

Macro$imp_average_life_exp <- Macro$average_life_exp

Macro$imp_average_life_exp[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual >= 5.287e+04
                                              & is.na(Macro$imp_average_life_exp)] <- 76.7
Macro$imp_average_life_exp[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual < 5.287e+04
                                              & is.na(Macro$imp_average_life_exp)] <- 74.2
Macro$imp_average_life_exp[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual >= 5.3e+04
                                              & is.na(Macro$imp_average_life_exp)] <- 76.77
Macro$imp_average_life_exp[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual < 5.3e+04
                                              & is.na(Macro$imp_average_life_exp)] <- 75.79
Macro$average_life_exp <- NULL

############################
# Impute infant_mortarity_per_1000_cap  #
############################

Macro$m_infant_mortarity_per_1000_cap <- 0
Macro$m_infant_mortarity_per_1000_cap[is.na(Macro$infant_mortarity_per_1000_cap)] <- 1

Macro$imp_infant_mortarity_per_1000_cap <- Macro$infant_mortarity_per_1000_cap

Macro$imp_infant_mortarity_per_1000_cap[Macro$gdp_annual_growth >= 0.02399 & Macro$gdp_annual >= 5.3e+04
                           & is.na(Macro$imp_infant_mortarity_per_1000_cap)] <- 8.1
Macro$imp_infant_mortarity_per_1000_cap[Macro$gdp_annual_growth >= 0.02399 & Macro$gdp_annual < 5.3e+04
                           & is.na(Macro$imp_infant_mortarity_per_1000_cap)] <- 6.2
Macro$imp_infant_mortarity_per_1000_cap[Macro$gdp_annual_growth < 0.02399 & is.na(Macro$imp_infant_mortarity_per_1000_cap)] <- 6.1

Macro$infant_mortarity_per_1000_cap <- NULL


######################################
# Impute perinatal_mort_per_1000_cap  #
######################################

Macro$m_perinatal_mort_per_1000_cap <- 0
Macro$m_perinatal_mort_per_1000_cap[is.na(Macro$perinatal_mort_per_1000_cap)] <- 1

Macro$imp_perinatal_mort_per_1000_cap <- Macro$perinatal_mort_per_1000_cap

Macro$imp_perinatal_mort_per_1000_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                               & is.na(Macro$imp_perinatal_mort_per_1000_cap)] <- 7.61
Macro$imp_perinatal_mort_per_1000_cap[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                               & is.na(Macro$imp_perinatal_mort_per_1000_cap)] <- 10.2
Macro$imp_perinatal_mort_per_1000_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                               & is.na(Macro$imp_perinatal_mort_per_1000_cap)] <- 5.53
Macro$imp_perinatal_mort_per_1000_cap[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                               & is.na(Macro$imp_perinatal_mort_per_1000_cap)] <- 5.87
Macro$perinatal_mort_per_1000_cap <- NULL


###########################
# Impute provision_nurse  #
###########################

Macro$m_provision_nurse <- 0
Macro$m_provision_nurse[is.na(Macro$provision_nurse)] <- 1

Macro$imp_provision_nurse <- Macro$provision_nurse

Macro$imp_provision_nurse[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual >= 5.287e+04
                                      & is.na(Macro$imp_provision_nurse)] <- 93
Macro$imp_provision_nurse[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual < 5.287e+04
                                      & is.na(Macro$imp_provision_nurse)] <- 99
Macro$imp_provision_nurse[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual < 5.3e+04
                                      & is.na(Macro$imp_provision_nurse)] <- 100
Macro$imp_provision_nurse[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual >= 5.3e+04
                                      & is.na(Macro$imp_provision_nurse)] <- 91
Macro$provision_nurse <- NULL


###########################
# Impute power_clinics  #
###########################

Macro$m_power_clinics <- 0
Macro$m_power_clinics[is.na(Macro$power_clinics)] <- 1

Macro$imp_power_clinics <- Macro$power_clinics

Macro$imp_power_clinics[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals < 91.69
                          & is.na(Macro$imp_power_clinics)] <- 163
Macro$imp_power_clinics[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals >= 91.69
                          & is.na(Macro$imp_power_clinics)] <- 376
Macro$imp_power_clinics[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual < 6.897e+04
                          & is.na(Macro$imp_power_clinics)] <- 156
Macro$imp_power_clinics[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual >= 6.897e+04
                          & is.na(Macro$imp_power_clinics)] <- 42
Macro$power_clinics <- NULL

###############################
# Impute incidence_population  #
###############################

Macro$m_incidence_population <- 0
Macro$m_incidence_population[is.na(Macro$incidence_population)] <- 1

Macro$imp_incidence_population <- Macro$incidence_population

Macro$imp_incidence_population[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals < 91.69
                        & is.na(Macro$imp_incidence_population)] <- 697
Macro$imp_incidence_population[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals >= 91.69
                        & is.na(Macro$imp_incidence_population)] <- 715
Macro$imp_incidence_population[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual < 6.897e+04
                        & is.na(Macro$imp_incidence_population)] <- 699
Macro$imp_incidence_population[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual >= 6.897e+04
                        & is.na(Macro$imp_incidence_population)] <- 662
Macro$incidence_population <- NULL

###############################
# Impute rent_price_4.room_bus  #
###############################

Macro$m_rent_price_4.room_bus <- 0
Macro$m_rent_price_4.room_bus[is.na(Macro$rent_price_4.room_bus)] <- 1

Macro$imp_rent_price_4.room_bus <- Macro$rent_price_4.room_bus

Macro$imp_rent_price_4.room_bus[Macro$imp_ppi < 495.1 & Macro$imp_average_provision_of_build_contract_moscow < 6.7
                               & is.na(Macro$imp_rent_price_4.room_bus)] <- 156.77
Macro$imp_rent_price_4.room_bus[Macro$imp_ppi < 495.1 & Macro$imp_average_provision_of_build_contract_moscow >= 6.7
                               & is.na(Macro$imp_rent_price_4.room_bus)] <- 137.81
Macro$imp_rent_price_4.room_bus[Macro$imp_ppi >= 495.1 & Macro$imp_ppi < 569
                               & is.na(Macro$imp_rent_price_4.room_bus)] <- 175.10
Macro$imp_rent_price_4.room_bus[Macro$imp_ppi >= 495.1 & Macro$imp_ppi >= 569
                               & is.na(Macro$imp_rent_price_4.room_bus)] <- 198.06
Macro$rent_price_4.room_bus <- NULL

############################
# Impute rent_price_3room_bus  #
############################

Macro$m_rent_price_3room_bus <- 0
Macro$m_rent_price_3room_bus[is.na(Macro$rent_price_3room_bus)] <- 1

Macro$imp_rent_price_3room_bus <- Macro$rent_price_3room_bus

Macro$imp_rent_price_3room_bus[Macro$imp_power_clinics >= 81.55 & Macro$imp_ppi >= 427
                           & is.na(Macro$imp_rent_price_3room_bus)] <- 88.70
Macro$imp_rent_price_3room_bus[Macro$imp_power_clinics >= 81.55 & Macro$imp_ppi < 427
                           & is.na(Macro$imp_rent_price_3room_bus)] <- 80.67
Macro$imp_rent_price_3room_bus[Macro$imp_power_clinics < 81.55 & Macro$imp_mortgage_growth >= -0.3632
                           & is.na(Macro$imp_rent_price_3room_bus)] <- 101.88
Macro$imp_rent_price_3room_bus[Macro$imp_power_clinics < 81.55 & Macro$imp_mortgage_growth < -0.3632
                           & is.na(Macro$imp_rent_price_3room_bus)] <- 115.97
Macro$rent_price_3room_bus <- NULL

############################
# Impute load_on_doctors  #
############################

Macro$m_load_on_doctors <- 0
Macro$m_load_on_doctors[is.na(Macro$load_on_doctors)] <- 1

Macro$imp_load_on_doctors <- Macro$load_on_doctors

Macro$imp_load_on_doctors[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual >= 5.287e+04
                          & is.na(Macro$imp_load_on_doctors)] <- 7805
Macro$imp_load_on_doctors[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual < 5.287e+04
                          & is.na(Macro$imp_load_on_doctors)] <- 7873
Macro$imp_load_on_doctors[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual >= 5.3e+04
                          & is.na(Macro$imp_load_on_doctors)] <- 6900
Macro$imp_load_on_doctors[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual < 5.3e+04
                          & is.na(Macro$imp_load_on_doctors)] <- 8181
Macro$load_on_doctors <- NULL

################################
# Impute rent_price_2room_bus  #
################################

Macro$m_rent_price_2room_bus <- 0
Macro$m_rent_price_2room_bus[is.na(Macro$rent_price_2room_bus)] <- 1

Macro$imp_rent_price_2room_bus <- Macro$rent_price_2room_bus

Macro$imp_rent_price_2room_bus[Macro$imp_profitable_enterpr_share >= 0.6655 & Macro$deposits_value >= 1.067e+07
                          & is.na(Macro$imp_rent_price_2room_bus)] <- 68.64
Macro$imp_rent_price_2room_bus[Macro$imp_profitable_enterpr_share >= 0.6655 & Macro$deposits_value < 1.067e+07
                          & is.na(Macro$imp_rent_price_2room_bus)] <- 59.68
Macro$imp_rent_price_2room_bus[Macro$imp_profitable_enterpr_share < 0.6655 & Macro$imp_micex_rgbi_tr >= 125.7
                          & is.na(Macro$imp_rent_price_2room_bus)] <- 74.88
Macro$imp_rent_price_2room_bus[Macro$imp_profitable_enterpr_share < 0.6655 & Macro$imp_micex_rgbi_tr < 125.7
                          & is.na(Macro$imp_rent_price_2room_bus)] <- 82.05
Macro$rent_price_2room_bus <- NULL


################################
# Impute rent_price_1room_bus  #
################################

Macro$m_rent_price_1room_bus <- 0
Macro$m_rent_price_1room_bus[is.na(Macro$rent_price_1room_bus)] <- 1

Macro$imp_rent_price_1room_bus <- Macro$rent_price_1room_bus

Macro$imp_rent_price_1room_bus[Macro$imp_retail_trade_turnover < 4374 & Macro$deposits_value >= 1.04e+07
                               & is.na(Macro$imp_rent_price_1room_bus)] <- 52.26
Macro$imp_rent_price_1room_bus[Macro$imp_retail_trade_turnover < 4374 & Macro$deposits_value < 1.04e+07
                               & is.na(Macro$imp_rent_price_1room_bus)] <- 44.32
Macro$imp_rent_price_1room_bus[Macro$imp_retail_trade_turnover >= 4374 & Macro$imp_rent_price_2room_bus < 83.22
                               & is.na(Macro$imp_rent_price_1room_bus)] <- 58.94
Macro$imp_rent_price_1room_bus[Macro$imp_retail_trade_turnover >= 4374 & Macro$imp_rent_price_2room_bus >= 83.22
                               & is.na(Macro$imp_rent_price_1room_bus)] <- 67.17
Macro$rent_price_1room_bus <- NULL


################################
# Impute rent_price_3room_eco  #
################################

Macro$m_rent_price_3room_eco <- 0
Macro$m_rent_price_3room_eco[is.na(Macro$rent_price_3room_eco)] <- 1

Macro$imp_rent_price_3room_eco <- Macro$rent_price_3room_eco

Macro$imp_rent_price_3room_eco[Macro$imp_rent_price_2room_bus >= 66.28 & Macro$imp_retail_trade_turnover_per_cap < 351.4
                               & is.na(Macro$imp_rent_price_3room_eco)] <- 47.21
Macro$imp_rent_price_3room_eco[Macro$imp_rent_price_2room_bus >= 66.28 & Macro$imp_retail_trade_turnover_per_cap >= 351.4
                               & is.na(Macro$imp_rent_price_3room_eco)] <- 50.45
Macro$imp_rent_price_3room_eco[Macro$imp_rent_price_2room_bus < 66.28 & Macro$imp_rent_price_3room_bus >= 79.58
                               & is.na(Macro$imp_rent_price_3room_eco)] <- 43.88
Macro$imp_rent_price_3room_eco[Macro$imp_rent_price_2room_bus < 66.28 & Macro$imp_rent_price_3room_bus < 79.58
                               & is.na(Macro$imp_rent_price_3room_eco)] <- 40.44
Macro$rent_price_3room_eco <- NULL

################################
# Impute rent_price_2room_eco  #
################################

Macro$m_rent_price_2room_eco <- 0
Macro$m_rent_price_2room_eco[is.na(Macro$rent_price_2room_eco)] <- 1

Macro$imp_rent_price_2room_eco <- Macro$rent_price_2room_eco

Macro$imp_rent_price_2room_eco[Macro$imp_rts < 1549 & Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493
                               & is.na(Macro$imp_rent_price_2room_eco)] <- 39.19
Macro$imp_rent_price_2room_eco[Macro$imp_rts < 1549 & Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493
                               & is.na(Macro$imp_rent_price_2room_eco)] <- 41.93
Macro$imp_rent_price_2room_eco[Macro$imp_rts >= 1549 & Macro$imp_income_per_cap < 4.94e+04
                               & is.na(Macro$imp_rent_price_2room_eco)] <- 36.39
Macro$imp_rent_price_2room_eco[Macro$imp_rts >= 1549 & Macro$imp_income_per_cap >= 4.94e+04
                               & is.na(Macro$imp_rent_price_2room_eco)] <- 28.90
Macro$rent_price_2room_eco <- NULL


################################
# Impute rent_price_1room_eco  #
################################

Macro$m_rent_price_1room_eco <- 0
Macro$m_rent_price_1room_eco[is.na(Macro$rent_price_1room_eco)] <- 1

Macro$imp_rent_price_1room_eco <- Macro$rent_price_1room_eco

Macro$imp_rent_price_1room_eco[Macro$imp_rent_price_2room_eco < 40.8 & Macro$imp_micex_rgbi_tr < 134.3
                               & is.na(Macro$imp_rent_price_1room_eco)] <- 30.03
Macro$imp_rent_price_1room_eco[Macro$imp_rent_price_2room_eco < 40.8 & Macro$imp_micex_rgbi_tr >= 134.3
                               & is.na(Macro$imp_rent_price_1room_eco)] <- 22.53
Macro$imp_rent_price_1room_eco[Macro$imp_rent_price_2room_eco >= 40.8 & is.na(Macro$imp_rent_price_1room_eco)] <- 33.65
Macro$rent_price_1room_eco <- NULL

#############################################
# Impute load_of_teachers_school_per_teacher  #
#############################################

Macro$m_load_of_teachers_school_per_teacher <- 0
Macro$m_load_of_teachers_school_per_teacher[is.na(Macro$load_of_teachers_school_per_teacher)] <- 1

Macro$imp_load_of_teachers_school_per_teacher <- Macro$load_of_teachers_school_per_teacher

Macro$imp_load_of_teachers_school_per_teacher[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual >= 5.3e+04
                                              & is.na(Macro$imp_load_of_teachers_school_per_teacher)] <- 1574
Macro$imp_load_of_teachers_school_per_teacher[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual < 5.3e+04
                                              & is.na(Macro$imp_load_of_teachers_school_per_teacher)] <- 1392
Macro$imp_load_of_teachers_school_per_teacher[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual >= 5.287e+04
                                              & is.na(Macro$imp_load_of_teachers_school_per_teacher)] <- 1518
Macro$imp_load_of_teachers_school_per_teacher[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual < 5.287e+04
                                              & is.na(Macro$imp_load_of_teachers_school_per_teacher)] <- 1356
Macro$load_of_teachers_school_per_teacher <- NULL

###################################
# Impute students_state_oneshift  #
###################################

Macro$m_students_state_oneshift <- 0
Macro$m_students_state_oneshift[is.na(Macro$students_state_oneshift)] <- 1

Macro$imp_students_state_oneshift <- Macro$students_state_oneshift

Macro$imp_students_state_oneshift[Macro$imp_lodging_sqm_per_cap >= 19.19 & Macro$gdp_annual < 7.244e+04
                                              & is.na(Macro$imp_students_state_oneshift)] <- 99.2666
Macro$imp_students_state_oneshift[Macro$imp_lodging_sqm_per_cap >= 19.19 & Macro$gdp_annual >= 7.244e+04
                                              & is.na(Macro$imp_students_state_oneshift)] <- 97.8484
Macro$imp_students_state_oneshift[Macro$imp_lodging_sqm_per_cap < 19.19 & Macro$gdp_annual < 5.866e+04
                                              & is.na(Macro$imp_students_state_oneshift)] <- 89.05
Macro$imp_students_state_oneshift[Macro$imp_lodging_sqm_per_cap < 19.19 & Macro$gdp_annual >= 5.866e+04
                                              & is.na(Macro$imp_students_state_oneshift)] <- 85.51
Macro$students_state_oneshift <- NULL

#############################
# Impute provision_doctors  #
#############################

Macro$m_provision_doctors <- 0
Macro$m_provision_doctors[is.na(Macro$provision_doctors)] <- 1

Macro$imp_provision_doctors <- Macro$provision_doctors

Macro$imp_provision_doctors[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals >= 91.69
                                  & is.na(Macro$imp_provision_doctors)] <- 65.9
Macro$imp_provision_doctors[Macro$imp_mortgage_growth >= 0.9592 & Macro$oil_urals < 91.69
                                  & is.na(Macro$imp_provision_doctors)] <- 18
Macro$imp_provision_doctors[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual < 6.897e+04
                                  & is.na(Macro$imp_provision_doctors)] <- 66
Macro$imp_provision_doctors[Macro$imp_mortgage_growth < 0.9592 & Macro$gdp_annual >= 6.897e+04
                                  & is.na(Macro$imp_provision_doctors)] <- 61
Macro$provision_doctors <- NULL

#####################################
# Impute turnover_catering_per_cap  #
####################################

Macro$m_turnover_catering_per_cap <- 0
Macro$m_turnover_catering_per_cap[is.na(Macro$turnover_catering_per_cap)] <- 1

Macro$imp_turnover_catering_per_cap <- Macro$turnover_catering_per_cap

Macro$imp_turnover_catering_per_cap[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual >= 5.287e+04
                            & is.na(Macro$imp_turnover_catering_per_cap)] <- 10311
Macro$imp_turnover_catering_per_cap[Macro$imp_seats_theather_rfmin_per_100000_cap < 0.4493 & Macro$gdp_annual < 5.287e+04
                            & is.na(Macro$imp_turnover_catering_per_cap)] <- 6221
Macro$imp_turnover_catering_per_cap[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual >= 5.3e+04
                            & is.na(Macro$imp_turnover_catering_per_cap)] <- 10805
Macro$imp_turnover_catering_per_cap[Macro$imp_seats_theather_rfmin_per_100000_cap >= 0.4493 & Macro$gdp_annual < 5.3e+04
                            & is.na(Macro$imp_turnover_catering_per_cap)] <- 6943
Macro$turnover_catering_per_cap <- NULL

#############################
# Impute bandwidth_sports  #
############################

Macro$m_bandwidth_sports <- 0
Macro$m_bandwidth_sports[is.na(Macro$bandwidth_sports)] <- 1

Macro$imp_bandwidth_sports <- Macro$bandwidth_sports

Macro$imp_bandwidth_sports[Macro$imp_lodging_sqm_per_cap < 19.19 & Macro$gdp_annual >= 5.866e+04
                                    & is.na(Macro$imp_bandwidth_sports)] <- 398451
Macro$imp_bandwidth_sports[Macro$imp_lodging_sqm_per_cap < 19.19 & Macro$gdp_annual < 5.866e+04
                                    & is.na(Macro$imp_bandwidth_sports)] <- 269768
Macro$imp_bandwidth_sports[Macro$imp_lodging_sqm_per_cap >= 19.19 & Macro$oil_urals < 78.56
                                    & is.na(Macro$imp_bandwidth_sports)] <- 463938
Macro$imp_bandwidth_sports[Macro$imp_lodging_sqm_per_cap >= 19.19 & Macro$oil_urals >= 78.56
                                    & is.na(Macro$imp_bandwidth_sports)] <- 288177
Macro$bandwidth_sports <- NULL

############################
# Impute apartment_build  #
###########################

Macro$m_apartment_build <- 0
Macro$m_apartment_build[is.na(Macro$apartment_build)] <- 1

Macro$imp_apartment_build <- Macro$apartment_build

Macro$imp_apartment_build[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual >= 6.897e+04
                                      & is.na(Macro$imp_apartment_build)] <- 46080
Macro$imp_apartment_build[Macro$gdp_annual >= 5.3e+04 & Macro$gdp_annual < 6.897e+04
                                      & is.na(Macro$imp_apartment_build)] <- 42551
Macro$imp_apartment_build[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals >= 91.69
                                      & is.na(Macro$imp_apartment_build)] <- 23587
Macro$imp_apartment_build[Macro$gdp_annual < 5.3e+04 & Macro$oil_urals < 91.69
                                      & is.na(Macro$imp_apartment_build)] <- 22825
Macro$apartment_build <- NULL


############################
# Impute apartment_fund_sqm  #
###########################

Macro$m_apartment_fund_sqm <- 0
Macro$m_apartment_fund_sqm[is.na(Macro$apartment_fund_sqm)] <- 1

Macro$imp_apartment_fund_sqm <- Macro$apartment_fund_sqm

Macro$imp_apartment_fund_sqm[Macro$imp_lodging_sqm_per_cap >= 19.19 & Macro$oil_urals >= 78.56
                          & is.na(Macro$imp_apartment_fund_sqm)] <- 232840.2
Macro$imp_apartment_fund_sqm[Macro$imp_lodging_sqm_per_cap >= 19.19 & Macro$oil_urals < 78.56
                          & is.na(Macro$imp_apartment_fund_sqm)] <- 234576.9
Macro$imp_apartment_fund_sqm[Macro$imp_lodging_sqm_per_cap < 19.19 & Macro$gdp_annual < 5.866e+04
                          & is.na(Macro$imp_apartment_fund_sqm)] <- 230310
Macro$imp_apartment_fund_sqm[Macro$imp_lodging_sqm_per_cap < 19.19 & Macro$gdp_annual >= 5.866e+04
                          & is.na(Macro$imp_apartment_fund_sqm)] <- 229294.8
Macro$apartment_fund_sqm <- NULL

# check for any missing values in Macro --- if not, all done!
mmv()


##########################################
# Fix missing values in the Test dataset #
##########################################

# product_type
Test$product_type[Test$imp_build_year >= 2014 & is.na(Test$product_type)] <- "OwnerOccupier"
Test$product_type[Test$imp_build_year < 2014 & is.na(Test$product_type)] <- "Investment"

Test$product_type[Test$imp_kitch_sq < 1.3 & is.na(Test$product_type)] <- "OwnerOccupier"
Test$product_type[Test$imp_kitch_sq >= 1.3 & is.na(Test$product_type)] <- "Investment"

##########################################
# Drop variables not needed for test set #
##########################################

Train$neighNeigh0 <- NULL
Train$monthJun <- NULL



####################################################################################################
# Now that all missing values have been addressed, need to combine Train and Test with Macro data  #
####################################################################################################

TrainMacro <- join(Train, Macro[,], by = 'timestamp', type = 'left')
TestMacro <- join(Test, Macro[,], by = 'timestamp', type = 'left')
str(TrainMacro, list.len = ncol(TrainMacro))
str(TestMacro, list.len = ncol(TestMacro))


# Now that I am no longer manipulating data, the id, year, timestamp, Color & build_year variables can be dropped
TrainMacro$id <- NULL
TrainMacro$year <- NULL
TrainMacro$timestamp <- NULL
TrainMacro$Color <- NULL
TrainMacro$build_year <- NULL
TrainMacro$sub_area <- NULL

TestMacro$id <- NULL
TestMacro$year <- NULL
TestMacro$timestamp <- NULL
TestMacro$build_year <- NULL
TestMacro$Color <- NULL
TestMacro$sub_area <- NULL

###############################################################
# Create Dummy Variables for any non-numeric categorical data #
###############################################################

TM.dummies <- dummy.data.frame(TrainMacro, names = c("neigh", "product_type", "culture_objects_top_25", "thermal_power_plant_raion",
                                                     "incineration_raion", "oil_chemistry_raion", "radiation_raion",
                                                     "railroad_terminal_raion", "big_market_raion", "nuclear_reactor_raion",
                                                     "detention_facility_raion", "water_1line", "big_road1_1line", "railroad_1line",
                                                     "ecology", "month"))
str(TM.dummies, list.len = ncol(TM.dummies))

Test.dummies <- dummy.data.frame(TestMacro, names = c("neigh", "product_type", "culture_objects_top_25", "thermal_power_plant_raion",
                                                "incineration_raion", "oil_chemistry_raion", "radiation_raion",
                                                "railroad_terminal_raion", "big_market_raion", "nuclear_reactor_raion",
                                                "detention_facility_raion", "water_1line", "big_road1_1line", "railroad_1line",
                                                "ecology", "month"))
str(Test.dummies, list.len = ncol(Test.dummies))

# Drop columns which have zero variance
TM.dummies <- subset(TM.dummies, select = -c(which(apply(TM.dummies, 2, var) == 0)))
Test.dummies <- subset(Test.dummies, select = -c(which(apply(Test.dummies, 2, var) == 0)))
TM.dummies$m_material <- NULL
TM.dummies$m_kitch_sq <- NULL
TM.dummies$m_floor <- NULL
TM.dummies$m_max_floor <- NULL
TM.dummies$m_num_room <- NULL
TM.dummies$neighNeigh0 <- NULL
TM.dummies$monthJun <- NULL
TM.dummies$m_grp_growth <- NULL
TM.dummies$m_load_of_teachers_preschool_per_teacher <- NULL
TM.dummies$m_hospital_beds_available_per_cap <- NULL
TM.dummies$m_hospital_bed_occupancy_per_year <- NULL
TM.dummies$m_provision_retail_space_sqm <- NULL
TM.dummies$m_provision_retail_space_modern_sqm <- NULL
TM.dummies$m_theaters_viewers_per_1000_cap <- NULL
TM.dummies$m_museum_visitis_per_100_cap <- NULL
TM.dummies$m_population_reg_sports_share <- NULL
TM.dummies$m_students_reg_sports_share <- NULL
TM.dummies$m_grp <- NULL
TM.dummies$imp_grp <- NULL
Test.dummies$m_income_per_cap <- NULL


###########################################################################################
# Deal with outliers for most promising predictor variables and for the response variable #
###########################################################################################


lower <- function(x) {
    quantile(x, .25) - (1.5 * IQR(x))
}

upper <- function(x) {
    quantile(x, .75) + (1.5 * IQR(x))
}

extremel <- function(x) {
    quantile(x, .25) - (3 * IQR(x))
}

extremeu <- function(x) {
    quantile(x, .75) + (3 * IQR(x))
}


multi.fun <- function(x) {
    c(summary = summary(x), IQR = IQR(x), P1 = quantile(x, .01), P99 = quantile(x, .99),
    LOWER = lower(x), upper = upper(x), extremel = extremel(x), extremeu = extremeu(x))
}

prices <- subset(TM.dummies, select = c(imp_build_count_after_1995))

summary.metric.data <- sapply(prices, multi.fun)
summary.metric.data


boxplot(prices, main = 'House price', ylab = 'Price', col = 'gold')
boxplot(TM.dummies$full_sq, data = TM.dummies, main = 'House price', ylab = 'Price', col = 'gold')

summary(TM.dummies$price_doc)
quantile(TM.dummies$price_doc, c(.01, .99))

# Typo 
TM.dummies$price_doc[TM.dummies$price_doc == max(TM.dummies$price_doc)] <- 11110000
TM.dummies$full_sq[TM.dummies$full_sq == max(TM.dummies$full_sq)] <- 53
TM.dummies$full_sq[TM.dummies$full_sq == max(TM.dummies$full_sq)] <- 72
TM.dummies$full_sq[TM.dummies$full_sq == max(TM.dummies$full_sq)] <- 63
TM.dummies$full_sq[TM.dummies$full_sq == max(TM.dummies$full_sq)] <- 63
TM.dummies$full_sq[TM.dummies$full_sq == max(TM.dummies$full_sq)] <- 60
TM.dummies$full_sq[TM.dummies$full_sq == max(TM.dummies$full_sq)] <- 46

# Cap at upper 99%
TM.dummies$price_doc[TM.dummies$price_doc > 54866199.1 & TM.dummies$neighNeigh12 == 1] <- 54866199.1
TM.dummies$price_doc[TM.dummies$price_doc > 24580991 & TM.dummies$neighNeigh10 == 1] <- 24580991
TM.dummies$price_doc[TM.dummies$price_doc > 38380000 & TM.dummies$neighNeigh11 == 1] <- 38380000
TM.dummies$price_doc[TM.dummies$price_doc > 14500000 & TM.dummies$neighNeigh7 == 1] <- 14500000
TM.dummies$price_doc[TM.dummies$price_doc > 36356381.7 & TM.dummies$neighNeigh8 == 1] <- 36356381.7
TM.dummies$price_doc[TM.dummies$price_doc > 11500000 & TM.dummies$neighNeigh6 == 1] <- 11500000
TM.dummies$price_doc[TM.dummies$price_doc > 10000000 & TM.dummies$neighNeigh5 == 1] <- 10000000
TM.dummies$price_doc[TM.dummies$price_doc > 12298000 & TM.dummies$neighNeigh3 == 1] <- 12298000
TM.dummies$price_doc[TM.dummies$price_doc > 7903600 & TM.dummies$neighNeigh2 == 1] <- 7903600
TM.dummies$sport_count_3000[TM.dummies$sport_count_3000 > 88] <- 88
TM.dummies$kremlin_km[TM.dummies$kremlin_km > 38.57270610] <- 38.57270610
TM.dummies$nuclear_reactor_km[TM.dummies$nuclear_reactor_km > 31.517036] <- 31.517036
TM.dummies$X16_29_male[TM.dummies$X16_29_male > 45867] <- 45867
TM.dummies$cafe_count_1000[TM.dummies$cafe_count_1000 > 316] <- 316
TM.dummies$prom_part_3000[TM.dummies$prom_part_3000 > 33] <- 33



# Cap at upper bound
TM.dummies$bulvar_ring_km[TM.dummies$bulvar_ring_km > 35] <- 35
TM.dummies$ttk_km[TM.dummies$ttk_km > 31] <- 31
TM.dummies$sadovoe_km[TM.dummies$sadovoe_km > 34] <- 34
TM.dummies$green_zone_km[TM.dummies$green_zone_km > .887] <- .887

# Cap at extreme upper bound
TM.dummies$price_doc[TM.dummies$price_doc > 18400000 & TM.dummies$neighNeigh9 == 1] <- 18400000
TM.dummies$price_doc[TM.dummies$price_doc > 9392481.8 & TM.dummies$neighNeigh4 == 1] <- 9392481.8
TM.dummies$area_m[TM.dummies$area_m > 50223514] <- 50223514
TM.dummies$zd_vokzaly_avto_km[TM.dummies$zd_vokzaly_avto_km > 65.970642] <- 65.970642
TM.dummies$imp_build_count_after_1995[TM.dummies$imp_build_count_after_1995 > 164] <- 164


# Cap at lower 1%
TM.dummies$price_doc[TM.dummies$price_doc < 1000000] <- 1000000

# Cap above upper extreme bound 
TM.dummies$full_sq[TM.dummies$full_sq > 225] <- 225
TM.dummies$full_sq[TM.dummies$full_sq < 27] <- 27
TM.dummies$kindergarten_km[TM.dummies$kindergarten_km > 10] <- 10

# Now do this to test
Test.dummies$price_doc[Test.dummies$price_doc > 54866199.1 & Test.dummies$neighNeigh12 == 1] <- 54866199.1
Test.dummies$price_doc[Test.dummies$price_doc > 24580991 & Test.dummies$neighNeigh10 == 1] <- 24580991
Test.dummies$price_doc[Test.dummies$price_doc > 38380000 & Test.dummies$neighNeigh11 == 1] <- 38380000
Test.dummies$price_doc[Test.dummies$price_doc > 14500000 & Test.dummies$neighNeigh7 == 1] <- 14500000
Test.dummies$price_doc[Test.dummies$price_doc > 36356381.7 & Test.dummies$neighNeigh8 == 1] <- 36356381.7
Test.dummies$price_doc[Test.dummies$price_doc > 11500000 & Test.dummies$neighNeigh6 == 1] <- 11500000
Test.dummies$price_doc[Test.dummies$price_doc > 10000000 & Test.dummies$neighNeigh5 == 1] <- 10000000
Test.dummies$price_doc[Test.dummies$price_doc > 12298000 & Test.dummies$neighNeigh3 == 1] <- 12298000
Test.dummies$price_doc[Test.dummies$price_doc > 7903600 & Test.dummies$neighNeigh2 == 1] <- 7903600
Test.dummies$sport_count_3000[Test.dummies$sport_count_3000 > 88] <- 88
Test.dummies$kremlin_km[Test.dummies$kremlin_km > 38.57270610] <- 38.57270610
Test.dummies$nuclear_reactor_km[Test.dummies$nuclear_reactor_km > 31.517036] <- 31.517036
Test.dummies$X16_29_male[Test.dummies$X16_29_male > 45867] <- 45867
Test.dummies$bulvar_ring_km[Test.dummies$bulvar_ring_km > 35] <- 35
Test.dummies$ttk_km[Test.dummies$ttk_km > 31] <- 31
Test.dummies$sadovoe_km[Test.dummies$sadovoe_km > 34] <- 34
Test.dummies$green_zone_km[Test.dummies$green_zone_km > .887] <- .887
Test.dummies$cafe_count_1000[Test.dummies$cafe_count_1000 > 316] <- 316
Test.dummies$prom_part_3000[Test.dummies$prom_part_3000 > 33] <- 33



# Cap at extreme upper bound
Test.dummies$price_doc[Test.dummies$price_doc > 18400000 & Test.dummies$neighNeigh9 == 1] <- 18400000
Test.dummies$price_doc[Test.dummies$price_doc > 9392481.8 & Test.dummies$neighNeigh4 == 1] <- 9392481.8
Test.dummies$area_m[Test.dummies$area_m > 50223514] <- 50223514
Test.dummies$zd_vokzaly_avto_km[Test.dummies$zd_vokzaly_avto_km > 65.970642] <- 65.970642


# Cap at lower 1%
Test.dummies$price_doc[Test.dummies$price_doc < 1000000] <- 1000000

# Cap above upper extreme bound 
Test.dummies$full_sq[Test.dummies$full_sq > 225] <- 225
Test.dummies$full_sq[Test.dummies$full_sq < 27] <- 27
Test.dummies$kindergarten_km[Test.dummies$kindergarten_km > 10] <- 10
Test.dummies$imp_build_count_after_1995[Test.dummies$imp_build_count_after_1995 > 164] <- 164

#rattle()

#################################################
# Try PCA rather than actual variables selected #
#################################################

# Need to drop zero variance variables
#TM.dummies[, which(apply(TM.dummies, 2, var) == 0)] <- NULL

TM.dummies <- subset(TM.dummies, select = -c(which(apply(TM.dummies, 2, var) == 0)))
Test.dummies <- subset(Test.dummies, select = -c(which(apply(TM.dummies, 2, var) == 0)))

TM.dummies$price_doc <- NULL

Transformed <- subset(TM.dummies, select = c(full_sq, green_zone_km, cafe_count_3000_price_2500,cafe_count_1000_price_500,
                                             cafe_count_2000, church_count_500,sport_count_3000,imp_usdrub,imp_build_count_after_1995,
                                             neighNeigh0, neighNeigh1,neighNeigh2, neighNeigh3, neighNeigh4, neighNeigh5, neighNeigh6,
                                             neighNeigh7 , neighNeigh8, neighNeigh9, neighNeigh10, neighNeigh11))

prin_comp <- prcomp(TM.dummies, scale. = T)
#prin_comp <- prcomp(Transformed, scale. = T)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev ^ 2

#proportion of variance explained
prop_var <- pr_var / sum(pr_var)

# scree plots to determine percentage of variance explained and number of variables to use
par(mfrow = c(1, 1))
plot(prop_var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")
plot(cumsum(prop_var), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b")


# See what proprtion of variance each set of variables explains
sum(prop_var[1:10])
sum(prop_var[1:35])
sum(prop_var[1:50])
sum(prop_var[1:75])
sum(prop_var[1:100])
sum(prop_var[1:150])
sum(prop_var[1:175])

# Would prefer at leat 80% of variance explained and don't want too many variables so will try using 50 PCs
# add a training set with principal components
train.data <- data.frame(price_doc = TrainMacro$price_doc, prin_comp$x)
train.data <- train.data[, 1:101]
head(train.data)



##############################
# Now start modeling!!!!!!!! #
##############################

# PCA model using the variables in the model below
summary(Model1 <- lm(log(price_doc) ~ ., data = train.data))
summary(VarSelectionMLR <- step(Model1, direction = "both"))

# Create model based on variable selection from decision tree
#    +   
#                   
# 0.5125046
summary(Model1 <- lm(price_doc ~full_sq  
                     +green_zone_km
                     + cafe_count_3000_price_2500
                     + cafe_count_1000_price_500
                     + cafe_count_2000
                     + church_count_500 
                     +  sport_count_3000
                     + imp_usdrub
                     + imp_build_count_after_1995
                     + neighNeigh0 + neighNeigh1
                     + neighNeigh2 + neighNeigh3 + neighNeigh4 + neighNeigh5 + neighNeigh6
                     + neighNeigh7 + neighNeigh8 + neighNeigh9 + neighNeigh10 + neighNeigh11
                     , data = TM.dummies))
summary(VarSelectionMLR <- step(Model1, direction = "both"))

# 0.5127835 - OutputFixed2
summary(Model1 <- lm(price_doc ~ 
                     + full_sq + sport_count_3000 + cafe_count_1000_price_4000 
                     + neighNeigh0 + neighNeigh1
                     + neighNeigh2 + neighNeigh3 + neighNeigh4 + neighNeigh5 + neighNeigh6
                     + neighNeigh7 + neighNeigh8 + neighNeigh9 + neighNeigh10 + neighNeigh11
                     , data = TM.dummies))
summary(VarSelectionMLR <- step(Model1, direction = "both"))


# 0.523004
summary(Model1 <- lm(price_doc ~ cafe_count_3000 + full_sq + sport_count_3000
                     , data = TM.dummies))
summary(VarSelectionMLR <- step(Model1, direction = "both"))

# This first attempt has a terrible adjusted r-squared value of .3026!!!!! 
# Try evaluating the RMSLE to get a sense of where I stand
Actual <- Train$price_doc
Predicted <- exp(fitted(Model1))

Predicted[Predicted < 0] <- min(Predicted[Predicted > 0])

rmsle(Actual, Predicted)
rmse(Actual, Predicted)

# RMSLE actually looks really good. Apply PCA to test data
test.data <- predict(prin_comp, newdata = Test.dummies)

str(TM.dummies, list.len = ncol(TM.dummies))
str(Test.dummies, list.len = ncol(Test.dummies))



##############################
# Build Production output    #
##############################

#test.data <- test.data[, 1:120]
#TestData <- as.data.frame(test.data)
#head(TestData)

#Test.dummies$ID <- Test$id

# 22 predictor variables
Test.dummies$prices <- (4745537.4
                        + Test.dummies$cafe_count_1000_price_4000 * -137925.4
                        + Test.dummies$full_sq * 120089.5
                        + Test.dummies$sport_count_3000 * 51231.5
                        + Test.dummies$neighNeigh1 * - 8883686
                        + Test.dummies$neighNeigh2 * - 6533186.7
                        + Test.dummies$neighNeigh3 * - 5491330.4
                        + Test.dummies$neighNeigh4 * - 5090744.5
                        + Test.dummies$neighNeigh5 * - 5806296
                        + Test.dummies$neighNeigh6 * - 5178700.5
                        + Test.dummies$neighNeigh7 * - 4352874.2
                        + Test.dummies$neighNeigh8 * - 4616358.2
                        + Test.dummies$neighNeigh9 * - 3599987.3
                        + Test.dummies$neighNeigh10 * - 2818262.2
                        + Test.dummies$neighNeigh11 * - 2223651.7)

Test.dummies$prices[Test.dummies$prices < 0] <- min(Test.dummies$prices[Test.dummies$prices > 0])

# Creating the Output file with the id and price_doc
OutputFixed2 <- data.frame(Test.dummies$id, Test.dummies$prices)
names(OutputFixed2) <- c("id", "price_doc")

# Checking the Output Dataset
str(OutputFixed2) # 'data.frame': 259 obs. of 2 variables
head(OutputFixed2)

### Write TESTdata to CSV ###
write.csv(OutputFixed2,
          file = "OutputFixed2.csv",
          row.names = FALSE)


#########################################################################################################################
#  Analysis on the model to see if it meets assumptions and if there is any multi-collinearity                                                                   #
#########################################################################################################################

# Check for collinearity using variance inflation factors - there shouldn't be any since I used PCA
vif(Model1)

model.data$logPrice <- NULL

par(mfrow = c(1, 1))
m <- cor(model.data)
corrplot(m, method="square")

ld.varsF <- attributes(alias(Model1)$Complete)$dimnames[[1]]
ld.varsF

plot(TM.dummies$neighNeigh7, TM.dummies$neighNeigh1)

#Scatterplot of Actual vs predicted
par(mfrow = c(1, 1))
Actual <- TrainMacro$logPrice
Predicted <- fitted(Model1)
plot(Predicted, Actual, main = "Actual vs. Predicted",
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "gray")

abline(lm(Predicted ~ Actual), col = "red") # regression line (y~x) 
lines(lowess(Predicted, Actual), col = "blue") # lowess line (x,y)







###############################################################
# Create Dummy Variables for any non-numeric categorical data #
###############################################################

# TO DO: may go back and collapse sub area factor and possibly ecology and month; This is just to get a PCA rolling
TM.dummies <- dummy.data.frame(TrainMacro, names = c("neigh", "product_type", "culture_objects_top_25", "thermal_power_plant_raion",
                                                     "incineration_raion", "oil_chemistry_raion", "radiation_raion",
                                                     "railroad_terminal_raion", "big_market_raion", "nuclear_reactor_raion",
                                                     "detention_facility_raion", "water_1line", "big_road1_1line", "railroad_1line",
                                                     "ecology", "month"
                                                     ))
str(TM.dummies, list.len = ncol(TM.dummies))

TM.dummies$price_doc <- NULL


# Write out this file to csv
### Write TESTdata to CSV ###
write.csv(TM.dummies,
          file = "TrainMacro.csv",
          row.names = FALSE)

Test.dummies <- dummy.data.frame(TestMacro, names = c("neigh", "product_type", "culture_objects_top_25", "thermal_power_plant_raion",
                                                     "incineration_raion", "oil_chemistry_raion", "radiation_raion",
                                                     "railroad_terminal_raion", "big_market_raion", "nuclear_reactor_raion",
                                                     "detention_facility_raion", "water_1line", "big_road1_1line", "railroad_1line",
                                                     "ecology", "month"
                                                     ))
str(Test.dummies, list.len = ncol(Test.dummies))


# Finally, drop the response variable from dataset

TM.dummies$price_doc <- NULL
TM.dummies$logPrice <- NULL

Test.dummies$price_doc <- NULL
Test.dummies$logPrice <- NULL














