#Programa que prepara y datos para correr con BLUPF90

#aqui mergeo la base de datos con la genealogia para agregar sexo, a?o nac, edad madre, etc.
todo1 <- todo[,c(-5,-6)]
#todo.ped2 <- gen[,c(-9,-10)] 
todo2 <- merge(todo1,gen, by="RIM")
rm(todo1)

todo2$FN <- as.Date(strptime(paste(todo2$DN,todo2$MN,todo2$AN,sep='/'),format="%d/%m/%Y"))
todo2$FD <- as.Date(strptime(paste(todo2$DD,todo2$MD,todo2$AD,sep='/'),format="%d/%m/%Y"))
todo2$FE1 <- as.Date(strptime(paste(todo2$DE1,todo2$ME1,todo2$AE1,sep='/'),format="%d/%m/%Y"))                                                                  
todo2$FE2 <- as.Date(strptime(paste(todo2$DE2,todo2$ME2,todo2$AE2,sep='/'),format="%d/%m/%Y"))                                                                  
todo2$FPC1 <- as.Date(strptime(paste(todo2$DPC1,todo2$MPC1,todo2$APC1,sep='/'),format="%d/%m/%Y"))                                                                 
todo2$FPC2 <- as.Date(strptime(paste(todo2$DPC2,todo2$MPC2,todo2$APC2,sep='/'),format="%d/%m/%Y"))

todo2$diaspcd <-todo2$FD- todo2$FN
todo2$diasesq1 <-todo2$FE1-todo2$FN
todo2$diasesq2 <-todo2$FE2-todo2$FN
todo2$diaspc1 <-todo2$FPC1-todo2$FN
todo2$diaspc2 <-todo2$FPC2-todo2$FN

#aqui condiciono datos y codifico con numeros los efectos fijos a usar en las corridas BLUPF90

todo3 <- subset(todo2,todo2$AN >= 1993 & todo2$AN <= ult.camada) #hay pocos animales con datos y padres antes de esa fecha
todo3 <- subset(todo3, todo3$rimpd!=0 | todo3$rimmd!=0)  #estos animales estan desvinculados de la poblacion
todo3$s <- ifelse(todo3$SEXO=='M',1,2)
todo3$m <- todo3$MANEJO
  todo3$m[todo3$m=='campo'] <- 1
  todo3$m[todo3$m=='cabania'] <- 2
  todo3$m[todo3$m=='galpon'] <- 3

todo3$r <- ifelse(todo3$rep=='TE' | todo3$rep=='T',2,1) #esto es porque hay muchos sin registrar rep

todo3$c <-todo3$CAMPO
  todo3$c[todo3$c=='Cabo'] <- 10
  todo3$c[todo3$c=='CoyA'] <- 11
  todo3$c[todo3$c=='DonS'] <- 12
  todo3$c[todo3$c=='LVeg'] <- 13
  todo3$c[todo3$c=='LagT'] <- 14
  todo3$c[todo3$c=='Mait'] <- 15
  #todo3$c[todo3$c=='Pilc'] <- 16
  #todo3$c[todo3$c=='RioM'] <- 17
  todo3$c[todo3$c=='RioP'] <- 18
  todo3$c[todo3$c=='Mana'] <- 19
  todo3$c[todo3$c=='MLun'] <- 20
  todo3$c[todo3$c=='Teck'] <- 21
  todo3$c[todo3$c=='CerC'] <- 22
  
todo3$hys <- paste(todo3$c,todo3$AN,todo3$s,todo3$m,todo3$r,sep='')

todo3$crian <- ifelse(todo3$CRIANZA=='simple',1,2)

todo3$clasem <- todo3$edadm

todo3$clasem <- ifelse(todo3$clasem>2|is.na(todo3$clasem),2,1) #hay unos 3000 sin clasem de pilca y rio mayo; *hay una hembra nacida de borrega DL

#cuento animales con pdf1 por grupo contempor?neo y elimino a los animales en grupos menores a por ej 5;
#todo3x <- subset(todo3, !is.na(todo3$PDF1)) #hys en base a hys con datos de pdf1
hys <- as.data.frame(table(todo3$hys))
hys$hys <- hys$Var1
todo4a <- merge(todo3,hys,by='hys')
todo4 <-subset(todo4a, todo4a$Freq >=5) #ATENCION!!! Modificar a 5, excepcion en 2022(LagT y Teck)

setwd(Provino)
write.table(hys, file = "hys.csv", sep=";", row.names = F, col.names = T, quote=F)
write.table(todo4a, file = "hys.det.csv", sep=";", row.names = F, col.names = T, quote=T)

rm(todo2,todo3,todo4a,hys)

#si hay errores saldran en el output
errores <- todo4
errores$show <- ifelse(is.na(todo4$RIM)|is.na(todo4$c)|is.na(todo4$AN)|is.na(todo4$s)|is.na(todo4$m)|is.na(todo4$crian)|is.na(todo4$clasem)|(is.na(todo4$diaspc1)&todo4$PC1>0)|(is.na(todo4$diaspcd)&todo4$PCD>0),1,0)
errores <- subset(errores, errores$show==1)
print(errores)

setwd(BLUP1)
dat1 <- todo4[,c("RIM","hys","crian","clasem", "diaspc1","diasesq1","PC1","PVS1","PVL1","PDF1")]
dat1 <- subset(dat1,!is.na(dat1$PC1)|!is.na(dat1$PVS1)|!is.na(dat1$PVL1)|!is.na(dat1$PDF1))

#dat1 <- todo4[,c("RIM","hys","crian","clasem", "diaspc1","diasesq1","PC1","PVS1","PVL1","PDF1",'PCD')] #para los que hacen solo PCD+PVS
#dat1 <- subset(dat1,!is.na(dat1$PC1)|!is.na(dat1$PVS1)|!is.na(dat1$PVL1)|!is.na(dat1$PDF1|!is.na(dat1$PCD)))#para los que hacen solo PCD+PVS
#dat1 <-dat1[,c(1:10)] #para los que hacen solo PCD+PVS
write.table(dat1, file = "datX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")
setwd(BLUP2)
dat2 <- todo4[,c("RIM","hys","crian","clasem", "diaspcd","diaspc1","diaspc2","crian","PCD","PC1","PC2")]
dat2 <- subset(dat2,!is.na(dat2$crian)|!is.na(dat2$PCD)|!is.na(dat2$PC1)|!is.na(dat2$PC2))
write.table(dat2, file = "datX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP3)
dat3 <- todo4[,c("RIM","hys","clasem","diasesq1","PDF1","CVF1","RT1")]
dat3 <- subset(dat3,!is.na(dat3$PDF1)|!is.na(dat3$CVF1)|!is.na(dat3$RT1))
write.table(dat3, file = "datX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP4)
dat4 <- todo4[,c("RIM","hys","crian","clasem", "diasesq1","PVS1","PDF1","LM1")]
dat4 <- subset(dat4,!is.na(dat4$PVS1)|!is.na(dat4$PDF1)|!is.na(dat4$LM1))
write.table(dat4, file = "datX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

rm(todo4, dat1, dat2, dat3, dat4)

#Corre BLUP
#copiar el renum.par del a?o anterior

setwd(BLUP1)
shell("renumf90 renum.par")
shell("blupf90 renf90.par")

setwd(BLUP2)
shell("renumf90 renum.par")
shell("blupf90 renf90.par")

setwd(BLUP3)
shell("renumf90 renum.par")
shell("blupf90 renf90.par")

setwd(BLUP4)
shell("renumf90 renum.par")
shell("blupf90 renf90.par")
