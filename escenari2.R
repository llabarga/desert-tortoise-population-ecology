# Llibreries utilitzades

library(ggplot2)
library(tidyr)
library(dplyr)
library(truncnorm)
library(purrr)
library(tidyr)


# Espècie: Gopherus agassizii ----------------------------------------------
# Creem la matriu de transicio 


matriu_pob_goph <- matrix(c(
  0,	0,	0,	0,	0, 0.042,	0.069,	0.069,
  0.716,	0.567,	0,	0,	0,	0,	0,	0,
  0,	0.149,	0.567,	0,	0,	0,	0,	0,
  0,	0,	0.149,	0.604,	0, 0, 0, 0,
  0,	0,	0,	0.235,	0.56,	0,	0,	0,
  0,	0,	0,	0,	0.225,	0.678,	0,	0,
  0,	0,	0,	0,	0,	0.249,	0.851,	0,
  0,	0,	0,	0,	0,	0,	0.016,	0.86
), nrow = 8, byrow = TRUE)

matriu_pob_goph

#creem el vector inicial
vector_inicial <- matrix(c(
  0,
  8276,
  22759,
  28966,
  26897,
  66209,
  167592,
  124142
), nrow = 8, byrow = TRUE)


# Creem un objecte que sigui el vector inicial per crear la taula més endavant
projeccio_poblacio<-vector_inicial

# Nombre de simulacions, anys a simular i any de començament
n_sim <- 100
n_temps <- 50 
y_inici <- 1986

# crear una llista buida per a guardar els resultats

poblist <- vector(length=n_sim,mode='list')

###### Escenari 2

for(sim in 1:n_sim) {
  
  projeccio_poblacio<-vector_inicial
  
  for (time_step in 1:n_temps) {
    
    # Aquí introduïm estocasticitat en 3 paràmetres de la matriu
    # Augmentem la probabilitat de passar a la seguent classe dels juvenils
    # Cada any que passa, aquests valors es mostrejen d'una distribució
    # normal, truncada (per no triar valors erronis)
    
    j1<-rtruncnorm(1, mean = 0.9, sd = 0.1,a=0, b=1)
    j2<-rtruncnorm(1, mean = 0.4, sd = 0.1,a=0, b=1)
    j3<-rtruncnorm(1, mean = 0.4, sd = 0.1,a=0, b=1)
    
    #Aquests valors els hem escollit nosaltres en base a la bibliografia 
    #van canviant de manera aleatoria
    
    matriu_pob_goph <- matrix(c(
      0,	0,	0,	0,	0, 0.042,	0.069,	0.069,
      j1,	0.567,	0,	0,	0,	0,	0,	0,
      0,	j2,	0.567,	0,	0,	0,	0,	0,
      0,	0,	j3,	0.604,	0, 0, 0, 0,
      0,	0,	0,	0.235,	0.56,	0,	0,	0,
      0,	0,	0,	0,	0.225,	0.678,	0,	0,
      0,	0,	0,	0,	0,	0.249,	0.851,	0,
      0,	0,	0,	0,	0,	0,	0.016,	0.86
    ), nrow = 8, byrow = TRUE)
    
    # print(matriu_pob_lotus)
    
    projeccio_poblacio<-cbind(
      projeccio_poblacio,
      round(matriu_pob_goph%*%projeccio_poblacio[,time_step],0)
    )
    
  }
  
  # mostra num simulacio en consola
  print(sim)
  # mostra proj poblacio en consola
  print(projeccio_poblacio)
  # guarda cada proj com a element de la llista
  poblist[[sim]] <- projeccio_poblacio
  # esborra
  rm(projeccio_poblacio)
}


# poblist és una llista que té n_sim elements
# p.ex. per veure q hi ha al primer element:
poblist[[1]]


# Crear data frame amb Nindividus per cada simulacio i any

pob_df <- poblist %>% 
  # la funció map_dfc (paquet purrr) aplica la funció següent per cada element d'una llista i 
  # dóna un data frame
  map_dfc(
    # suma tots els estadis per cada any i simulació
    ~apply(.x,2,sum)) %>% 
  setNames(1:n_sim) %>% 
  cbind(Any=y_inici:(y_inici+n_temps),.) %>% 
  pivot_longer(cols=-Any,names_to='simulation',values_to='Nind') 

# Gràfics de totes les simulacions (fer només si n'hi ha poques, si no, no es veurà res)

ggplot(pob_df, aes(x=Any,y=Nind,col=simulation))+
  geom_line()+
  guides(col="none")
# Farem un promig

# Resum de les simulacions
# mitjana i SD per cada any

pobdf_resum <- pob_df %>% 
  group_by(Any) %>% 
  summarise(
    Ntotal=mean(Nind,na.rm=TRUE),
    SDtotal=sd(Nind,na.rm=TRUE)
  )
pobdf_resum
# Gràfic mostrant N mitjana +/- SD

ggplot(pobdf_resum,aes(x=Any,y= Ntotal))+
  geom_ribbon(aes(ymin=Ntotal-SDtotal,ymax=Ntotal+SDtotal),fill='light green')+
  geom_line(aes(y=Ntotal))+ xlab("Any") + ylab("Nº total d'individus")+ ggtitle("Escenari 2")




