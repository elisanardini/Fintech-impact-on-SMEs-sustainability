library(dplyr)



# DATA CLEANING AND PREPARATION

setwd("~/Desktop/R/fintechsustainabilty")
df <- read.csv("raw data.csv")

df <- df[, -c(1,2)]





################################ Colonna1 - X1 - Da quanto tempo lavora per la sua azienda? ###############################

# Sostituisco "UN ANNO" con 1 
df[, 1] <- gsub("UN ANNO", "1", df[, 1], fixed = TRUE)

# Rimuovo tutte le parti non numeriche dalla prima colonna
df[, 1] <- as.numeric(gsub("[^0-9]", "", df[, 1]))

# Se il valore contiene 4 cifre, sottraggo il valore all'anno corrennte
df[, 1] <- ifelse(nchar(df[, 1]) == 4, 2024 - as.numeric(df[, 1]), df[, 1])


#################################### Colonna2 - X2 - Qual è il suo ruolo in azienda? #####################################

# Converto il testo della seconda colonna in minuscolo
df[, 2] <- tolower(df[, 2])
# print(df[, 2])



############################# Colonna3 - X3 - In che settore opera l'azienda per cui lavora? #############################

df[, 3] <- tolower(df[, 3])
# print(df[, 2])



############################### Colonna4 - X4 - Da quanti anni lavora in questo settore? ################################


# Sostituisci "UN ANNO" con 1 nella quarta colonna
df[, 4] <- gsub("UN ANNO", "1", df[, 4], fixed = TRUE)

# Rimuovo tutte le parti non numeriche dalla prima colonna
df[, 4] <- as.numeric(gsub("[^0-9]", "", df[, 4]))
# print(df[, 4])

# Se il valore contiene 4 cifre, sottraggo il valore all'anno corrennte
df[, 4] <- ifelse(nchar(df[, 4]) == 4, 2024 - as.numeric(df[, 4]), df[, 4])
# print(df[, 4])

# Sostituisci "323" con "23" nella quarta colonna
df[, 4] <- gsub("323", "23", df[, 4], fixed = TRUE)

# Sostituisco i valori mancanti (NA) con i valori corrispondenti dalla prima colonna
df[, 4] <- ifelse(is.na(df[, 4]), df[, 1], df[, 4])


############################### Colonna6 to Colonna15 - X6 -X15 ################################

# Scorro le colonne dalla 7 alla 14
for (colonna in 6:15) {
  # Sostituisco i valori vuoti nella colonna se il valore nella quinta colonna è >= 4
  df[, colonna] <- ifelse(df[, colonna] == "" & df[, 5] >= 4, "Si", ifelse(df[, colonna] == "", "No", df[, colonna]))
}

############################### Colonna16 - X16 ################################

df[, 16] <- ifelse(df[, 16] == "" | df[, 16] == "0", "nessuno", df[, 16])
df[, 16] <- tolower(df[, 16])
df[, 16] <- ifelse(df[, 16] == "nessuno", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == " nessuno ", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "attualmente nessuno", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "non saprei", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "zero", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "nessuno al momento", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "nessuna", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "nessun progetto fintech", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "nd", "0", df[, 16])
df[, 16] <- ifelse(df[, 16] == "uno", "1", df[, 16])
df[, 16] <- ifelse(df[, 16] == "due", "2", df[, 16])
df[, 16] <- ifelse(df[, 16] == "alcuni", "2", df[, 16])



df <- df %>%
  mutate(Colonna17 = ifelse(!grepl("^\\d+$", df[, 16]), df[, 16], NA)) %>%
  select(1:16, Colonna17, 17:ncol(df))


df <- df %>%
  mutate(across(16, ~ifelse(!grepl("^\\d+$", .), "1", .)))


df <- df %>%
  mutate(across(16, ~if_else(is.na(df[, 15]), NA, .)))

############################### Colonna18 - X18 ################################

df[, 18] <- tolower(df[, 18])
df[, 18] <- ifelse(df[, 18] == "", "nessuno", df[, 18])
df[, 18] <- ifelse(df[, 18] == "0", "nessuno", df[, 18])
df[, 18] <- ifelse(df[, 18] == "no", "nessuno", df[, 18])
df[, 18] <- ifelse(df[, 18] == "nessun", "nessuno", df[, 18])
df[, 18] <- ifelse(df[, 18] == "2", "non specificato", df[, 18])


df <- df %>%
  mutate(across(18, ~if_else(is.na(df[, 15]), NA, .)))



# Rinomino tutte le colonne per semplificare l'analisi

colnames(df) <- paste0("X", 1:ncol(df))

# GESTIONE DEI VALORI NULLI

# Sostituisco i valori nulli con tutti NA

replace_empty_with_na <- function(x) {
  ifelse(x == "", NA, x)
}



# Sostituisco i campi vuoti con NA

df <- df %>%
  mutate_all(~replace_empty_with_na(.))

# Elimino le righe che presentano tutti NA

df_ripulito <- df[rowSums(is.na(df)) != ncol(df), ]

write.csv(df_ripulito, file = "Dasatet_cleaned.csv")



# VARIABILI

# X1 - Da quanto tempo lavora per l'azienda
# X2 - Ruolo
# X3 - Settore azienda
# X4 - Da quanto tempo lavora nel settore
# X5 - Familiarità Fintech

#Conoscenza strumenti Fintech (Si/No)

# X6 - Blockchain
# X7 - AI
# X8 - Machine Learning
# X9 - Crowfunding
# X10 - Peer to Peer
# X11 - Mobile payment
# X12 - NFT
# X13 - Chatbot
# X14 - Robotic Process Automation

# Fintech e azienda

# X15 - Utilizzo strumenti Fintech per sostenibilità (Si/No)
# X16 - Quanti progetti in atto
# X17 - Quali strumenti
# X18 - Quali strumenti


# Valutazione affermazioni su benefici Scala 1-7

# X19 - CO2
# X20 - Risparmio Energetico
# X21 - Riduzione rifiuti
# X22 - Riutilizzo materiali di scarto
# X23 - Ottimizzazione risorse
# X24 - Risparmio costi per la sostenibilità
# X25 - Adeguamento normativo
# X26 - Soddisfazione dei clienti
# X27 - Miglioramento della reportistica
# X28 - Accesso al mercato dei capitali
# X29 - Redditività sostenibile
# X30 - Produttività
# X31 - Crescita
# X32 - Redditività delle operazioni

# X33 - Problemi applicativi

