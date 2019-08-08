library(RODBC)
library(dplyr)
library(tidyr)

channel <- odbcConnectAccess(choose.files())

tablesNames <- sqlTables(channel)$TABLE_NAME

##Obtener la Tabla de Conectividad de Frames
connectivity <- sqlQuery(channel, "SELECT * FROM [Connectivity - Frame]") %>%
  as_tibble()

##Obtener la Tabla de Coordenadas de todos los Joints
coordinates <- sqlQuery(channel, "SELECT * FROM [Joint Coordinates]") %>%
  as_tibble()

#CHEQUEO DE CONECTIVIDAD DE FRAMES

##Verificar que no haya dos elementos superpuestos conectando los mismos nodos en la misma 
#dirección (de i hacia j)

connectivity %>%
  mutate(id = paste0(JointI, "-",JointJ)) %>%
  group_by(id) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

##Verificar que no haya ningún elemento que esté conectando un nodo con sí mismo ( i = j )
connectivity %>%
  filter(JointI == JointJ)

##Verificar que no haya dos elementos superpuestos conectando los mismos nodos en direcciones opuestas (uno de i hacia j y el otro de j hacia i)
c1 <- connectivity %>%
  mutate(id = paste0(JointI, "-",JointJ)) %>%
  select(Frame, id)

c2 <- connectivity %>%
  mutate(id = paste0(JointJ, "-",JointI)) %>%
  select(Frame, id)

c1 %>%
  inner_join(c2, by="id")

##Verificar que no haya algún nodo que no esté conectado a ningún elemento
coordinates = coordinates %>%
  select(Joint, XorR, Y, Z)

connectivity %>%
  select(Frame, JointI, JointJ) %>%
  gather(key = "IorJ", "Joint", -"Frame") %>%
  right_join(coordinates, by="Joint") %>%
  filter(is.na(Frame))

#CHEQUEO DE SUPERPOSICION

for(i in 1:dim(coordinates)[1]){
  for(j in 1:dim(coordinates)[1]){
    if(i != j){
      distance = sqrt((coordinates$XorR[j]-coordinates$XorR[i])^2+
                        (coordinates$Y[j]-coordinates$Y[i])^2+
                        (coordinates$Z[j]-coordinates$Z[i])^2)
      if(distance <= 0.1){
        message(paste0("Node I:", coordinates$Joint[i], " Node J:",coordinates$Joint[j]))
      }
    }
  }
}
