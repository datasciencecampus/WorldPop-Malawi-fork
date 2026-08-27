library(tidyverse)

#Loading data zomba_csv
Chikowi <- read_csv("Chikowi.csv")
glimpse(Chikowi)
chimbalanga <- read_csv("Chimbalanga.csv")
glimpse(chimbalanga)
idana <- read_csv("Idana.csv")
glimpse(idana)
kumtumanji <- read_csv("Kumtumanji.csv")
glimpse(kumtumanji)
Malemia <- read_csv("Malemia.csv")
glimpse(Malemia)
Mbiza <- read_csv("Mbiza.csv")
glimpse(Mbiza)
Mkumbira <- read_csv("Mkumbira.csv")
glimpse(Mkumbira)
Mlumbe <- read_csv("Mlumbe.csv")
glimpse(Mlumbe)
Mwambo <- read_csv("Mwambo.csv")
glimpse(Mwambo)
Mwembere <- read_csv("Mwembere.csv")
glimpse(Mwembere)
Ngwelero <- read_csv("Ngwelero.csv")
glimpse(Ngwelero)
Nkagula <- read_csv("Nkagula.csv")
glimpse(Nkagula)
Nkapita <- read_csv("Nkapita.csv")
glimpse(Nkapita)
Ntholowa <- read_csv("Ntholowa.csv")
glimpse(Ntholowa)

#loading Malemia hh data
Malemia_hh <- read_csv("malemia_hh_without_IDs.csv")

#standardize column labels
standardise_names <- function(df) {
  df %>%
    rename_with(~ "household_number",
                .cols = any_of(c("HOUSEHOLD NUMBER", "household_name")))
}


Chikowi      <- standardise_names(Chikowi)
chimbalanga <- standardise_names(chimbalanga)
idana       <- standardise_names(idana)
kumtumanji  <- standardise_names(kumtumanji)
Malemia     <- standardise_names(Malemia)
Mbiza       <- standardise_names(Mbiza)
Mkumbira    <- standardise_names(Mkumbira)
Mlumbe      <- standardise_names(Mlumbe)
Mwambo      <- standardise_names(Mwambo)
Mwembere    <- standardise_names(Mwembere)
Ngwelero    <- standardise_names(Ngwelero)
Nkagula     <- standardise_names(Nkagula)
Nkapita     <- standardise_names(Nkapita)
Ntholowa    <- standardise_names(Ntholowa)

#validate 
colnames(Chikowi)

#Merge files
zomba_rbind_data <- bind_rows(
  Chikowi,
  chimbalanga,
  idana,
  kumtumanji,
  Malemia,
  Mbiza,
  Mkumbira,
  Mwambo,
  Mlumbe,
  Mwembere,
  Ngwelero,
  Nkagula,
  Nkapita,
  Ntholowa
)

glimpse(zomba_rbind_data)

#remove the fully empty rows 
zomba_rbind_data <- zomba_rbind_data %>%
  filter(!if_all(everything(), is.na))

#to confirm all TAs are included 
count(zomba_rbind_data, traditional_authority_name)

#to write it as a CSV

write.csv(
  zomba_rbind_data,
  "zomba_households_merged.csv",
  row.names = FALSE
)

