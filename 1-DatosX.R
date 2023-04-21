#Programa DatosA.sas para generar base de datos de la poblacion Poll Merino 
#Aqui esta la base de datos sin pedigree de cabañas
#Antes de importar guardar el archivo access como csv

#aqui esta la base de datos de todas las cabañas AACM
excel1 <- read.csv('c:/Drive/Data/Merino/X/Pob/Temp/X.txt', sep=';')

todo1 <- excel1
todo1$DPC1 <- todo1$DE1
todo1$MPC1 <- todo1$ME1
todo1$APC1 <- todo1$AE1
todo1$DPC2 <- todo1$DE2
todo1$MPC2 <- todo1$ME2
todo1$APC2 <- todo1$AE2
todo1$RT2 <- as.numeric(NA) #No hay datos
todo1$SEXO <- todo1$SEX

todo<-todo1[,c('RIM','CAMPO','CRIANZA','MANEJO','SEXO','AN','DD','MD','AD','DE1','ME1','AE1','DPC1','MPC1','APC1','DE2','ME2',
                'AE2','DPC2','MPC2','APC2','PCD','PC1','PVS1','RIN1','PVL1','PDF1','CVF1','FC1','LM1','RT1','PC2','PVS2',
                'RIN2','PVL2','PDF2','CVF2','FC2','LM2','RT2')]

#A patir de 2020 no contemplo mas los de Pilca y RM. Aquí iria el scripr "Pilca_RM.R"
#todo <- rbind(todo1,todo2,todo3)

rm(todo1,excel1)


#esto es para chequear estructura de animales con pdf1
T4 <- subset(todo, todo$AN > 1999 & todo$PDF1 >0 )
table(T4$AN,T4$CAMPO)
rm(T4)
