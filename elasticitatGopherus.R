# Llibreries utilitzades

library(ggplot2)
library(tidyr)
library(dplyr)
library(popdemo)


# Exercici 2 --------------------------------------------------------------

# Crear matriu de transició ---------------------------------------------

matriu_pob_struc <- matrix(c(
  0,	0,	0,	0,	0, 0.042,	0.069,	0.069,
  0.716,	0.567,	0,	0,	0,	0,	0,	0,
  0,	0.149,	0.567,	0,	0,	0,	0,	0,
  0,	0,	0.149,	0.604,	0, 0, 0, 0,
  0,	0,	0,	0.235,	0.56,	0,	0,	0,
  0,	0,	0,	0,	0.225,	0.678,	0,	0,
  0,	0,	0,	0,	0,	0.249,	0.851,	0,
  0,	0,	0,	0,	0,	0,	0.016,	0.86
), nrow = 8, byrow = TRUE)

# Veure matriu a la consola

matriu_pob_struc

# Calcular lambda i distribució estable de classes ------------------------

# Utilitzem el paquet popdemo

eigs(matriu_pob_struc)

# Veiem en consola el resultat

# Podem accedir als diferents components:
# lambda
# ss: distribució estable de classes

eigs(matriu_pob_struc)$lambda #
eigs(matriu_pob_struc)$ss



# Calcular elasticitat de la matriu ---------------------------------------
# Funció 'elas' del paquet popdemo

elas(matriu_pob_struc) 
#et diu quins valors de la matriu tenen més o menys influencia 
#en el creixement

# Els valors més alts ens indiquen els paràmetres més influents
