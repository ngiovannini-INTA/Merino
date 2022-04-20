#Programa DatosA.sas para generar base de datos de la poblacion Merino Astado
#Aqui esta la base de datos sin pedigree de cabañas
#Antes de importar guardar el archivo access como csv

#aqui esta la base de datos de todas las cabañas AACM
excel1 <- read.csv('c:/Drive/Data/Merino/A/Pob/Temp/A.csv', sep=';')

todo1 <- excel1
todo1$DPC1 <- todo1$DE1
todo1$MPC1 <- todo1$ME1
todo1$APC1 <- todo1$AE1
todo1$DPC2 <- todo1$DE2
todo1$MPC2 <- todo1$ME2
todo1$APC2 <- todo1$AE2
todo1$RT2 <- as.numeric(NA)
todo1$DD <- as.integer(NA)
todo1$MD <- as.integer(NA)
todo1$AD <- as.integer(NA)
todo1$PCD <- as.numeric(NA)
todo1$SEXO <- todo1$SEX

todo1<-todo1[,c('RIM','CAMPO','CRIANZA','MANEJO','SEXO','AN','DD','MD','AD','DE1','ME1','AE1','DPC1','MPC1','APC1','DE2','ME2',
                'AE2','DPC2','MPC2','APC2','PCD','PC1','PVS1','RIN1','PVL1','PDF1','CVF1','FC1','LM1','RT1','PC2','PVS2',
                'RIN2','PVL2','PDF2','CVF2','FC2','LM2','RT2')]


#aqui esta la base de datos y pedigree de Rio Mayo
excel2 <- read.csv('c:/Drive/Data/Merino/A/Riomayo/rioma.csv', sep=';')

todo2 <- subset(excel2, excel2$B =='A')
todo2$B <- droplevels(as.factor(todo2$B))
todo2$CAMPO <- 'RioM'

todo2$DPC1 <- todo2$DE1
todo2$MPC1 <- todo2$ME1
todo2$APC1 <- todo2$AE1
todo2$DPC2 <- todo2$DE2
todo2$MPC2 <- todo2$ME2
todo2$APC2 <- todo2$AE2

todo2<-todo2[,c('RIM','CAMPO','CRIANZA','MANEJO','SEXO','AN','DD','MD','AD','DE1','ME1','AE1','DPC1','MPC1','APC1','DE2','ME2',
                'AE2','DPC2','MPC2','APC2','PCD','PC1','PVS1','RIN1','PVL1','PDF1','CVF1','FC1','LM1','RT1','PC2','PVS2',
                'RIN2','PVL2','PDF2','CVF2','FC2','LM2','RT2')]

#aqui esta la base de datos y pedigree de pilca
excel3 <- read.csv('c:/Drive/Data/Merino/A/Pilca/pilca.csv', sep=';')

todo3 <- subset(excel3, excel3$B =='A')
todo3$B <- droplevels(as.factor(todo3$B))
todo3$CAMPO <- 'Pilc'

todo3$MANEJO[todo3$MANEJO=="Alto"] <- "cabaña"
todo3$MANEJO[todo3$MANEJO=="Medio"] <- "cabaña"
todo3$MANEJO[todo3$MANEJO=="Bajo"] <- "campo"

todo3$DPC1 <- todo3$DE1
todo3$MPC1 <- todo3$ME1
todo3$APC1 <- todo3$AE1
todo3$DPC2 <- todo3$DE2
todo3$MPC2 <- todo3$ME2
todo3$APC2 <- todo3$AE2

todo3<-todo3[,c('RIM','CAMPO','CRIANZA','MANEJO','SEXO','AN','DD','MD','AD','DE1','ME1','AE1','DPC1','MPC1','APC1','DE2','ME2',
                'AE2','DPC2','MPC2','APC2','PCD','PC1','PVS1','RIN1','PVL1','PDF1','CVF1','FC1','LM1','RT1','PC2','PVS2',
                'RIN2','PVL2','PDF2','CVF2','FC2','LM2','RT2')]

todo <- rbind(todo1,todo2,todo3)
rm(todo1,todo2,todo3,excel1,excel2,excel3)


#esto es para chequear estructura de animales con pdf1
T4 <- subset(todo, todo$AN > 1999 & todo$PDF1 >0 )
table(T4$AN,T4$CAMPO)
rm(T4)
