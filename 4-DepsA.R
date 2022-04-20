#Programa que procesa el output de BLUPf90

#Lee BLUP1
setwd(BLUP1)
VC1 <-read.delim("solutions", skip=1, header=F, sep='')
names(VC1)=c("trait","effect","RIM.r","solution","se")
VC1 <- subset(VC1,VC1$effect==6)
VC1 <- VC1[,-2]

VC1 <- reshape(VC1, idvar = "RIM.r", timevar = "trait", direction = "wide")
names(VC1)=c("RIM.r","vcPC1","sePC1","vcPVS1","sePVS1","vcPVL1","sePVL1","vcPDF1","sePDF1")

inb1 <-read.delim("renf90.inb", header=F,sep='')
names(inb1)=c("RIM","inb1","code")
inb1 <- inb1[,-3]

ped1.r <-read.delim("renadd06.ped", header=F,sep='')
names(ped1.r)=c("RIM.r","rimpd.r","rimmd.r","v4","V5","V6","V7","n.pad1","n.mad1","RIM")
ped1.r <-ped1.r[,c(1,2,3,8:10)]

VC1.j <- merge(ped1.r,inb1, by='RIM')
VC1.j <- merge(VC1,VC1.j, by='RIM.r')

VC1.j$acPC1 <- (sqrt(1-(VC1.j$sePC1*VC1.j$sePC1)/(4.2875*(1+VC1.j$inb/100))))*100
#VC1.j$acPVS1 <- (sqrt(1-(VC1.j$sePVS1*VC1.j$sePVS1)/(0.0635*(1+VC1.j$inb/100))))*100
VC1.j$acPVL1 <- (sqrt(1-(VC1.j$sePVL1*VC1.j$sePVL1)/(0.0306*(1+VC1.j$inb/100))))*100
VC1.j$acPDF1 <- (sqrt(1-(VC1.j$sePDF1*VC1.j$sePDF1)/(0.9331*(1+VC1.j$inb/100))))*100

#Me quedo con las variables de interes para el informe
VC1.j <-VC1.j[,-c(1,3:5,7,9,11,12)]

rm(VC1,ped1.r,inb1)


#Lee BLUP2
setwd(BLUP2)
VC2 <-read.delim("solutions", skip=1, header=F, sep='')
names(VC2)=c("trait","effect","RIM.r","solution","se")
VC2 <- subset(VC2,VC2$effect==7)
VC2 <- VC2[,-2]

VC2 <- reshape(VC2, idvar = "RIM.r", timevar = "trait", direction = "wide")
names(VC2)=c("RIM.r","vcNCD","seNCD","vcPCD","sePCD","vcPC1","sePC1","vcPC2","sePC2")

inb2 <-read.delim("renf90.inb", header=F,sep='')
names(inb2)=c("RIM","inb2","code")
inb2 <- inb2[,-3]

ped2.r <-read.delim("renadd07.ped", header=F,sep='')
names(ped2.r)=c("RIM.r","rimpd.r","rimmd.r","v4","V5","V6","V7","n.pad2","n.mad2","RIM")
ped2.r <-ped2.r[,c(1:3,8:10)]

VC2.j <- merge(ped2.r,inb2, by='RIM')
VC2.j <- merge(VC2,VC2.j, by='RIM.r')

VC2.j$acNCD <- (sqrt(1-(VC2.j$seNCD*VC2.j$seNCD)/(0.004*(1+VC2.j$inb/100))))*100
VC2.j$acPCD <- (sqrt(1-(VC2.j$sePCD*VC2.j$sePCD)/(2.3805*(1+VC2.j$inb/100))))*100
#VC2.j$acPC1 <- (sqrt(1-(VC2.j$sePC1*VC2.j$sePC1)/(4.2875*(1+VC2.j$inb/100))))*100
VC2.j$acPC2 <- (sqrt(1-(VC2.j$sePC2*VC2.j$sePC2)/(7.0875*(1+VC2.j$inb/100))))*100

#Me quedo con las variables de interes para el informe
VC2.j <-VC2.j[,-c(1,3,5:7,9,11,12)]

rm(VC2,ped2.r,inb2)

#Lee BLUP3
setwd(BLUP3)
VC3 <-read.delim("solutions", skip=1, header=F, sep='')
names(VC3)=c("trait","effect","RIM.r","solution","se")
VC3 <- subset(VC3,VC3$effect==4)
VC3 <- VC3[,-2]

VC3 <- reshape(VC3, idvar = "RIM.r", timevar = "trait", direction = "wide")
names(VC3)=c("RIM.r","vcPDF1","sePDF1","vcCVF1","seCVF1","vcRT1","seRT1")

inb3 <-read.delim("renf90.inb", header=F,sep='')
names(inb3)=c("RIM","inb3","code")
inb3 <- inb3[,-3]

ped3.r <-read.delim("renadd04.ped", header=F,sep='')
names(ped3.r)=c("RIM.r","rimpd.r","rimmd.r","v4","V5","V6","V7","n.pad3","n.mad3","RIM")
ped3.r <-ped3.r[,c(1:3,8:10)]

VC3.j <- merge(ped3.r,inb3, by='RIM')
VC3.j <- merge(VC3,VC3.j, by='RIM.r')

#VC3.j$acPDF1 <- (sqrt(1-(VC3.j$sePDF1*VC3.j$sePDF1)/(0.9331199*(1+VC3.j$inb/100))))*100
VC3.j$acCVF1 <- (sqrt(1-(VC3.j$seCVF1*VC3.j$seCVF1)/(2*(1+VC3.j$inb/100))))*100
VC3.j$acRT1 <- (sqrt(1-(VC3.j$seRT1*VC3.j$seRT1)/(33.075*(1+VC3.j$inb/100))))*100

#Me quedo con las variables de interes para el informe
VC3.j <-VC3.j[,-c(1:3,5,7,9,10)]

rm(VC3,ped3.r,inb3)

#Lee BLUP4
setwd(BLUP4)
VC4 <-read.delim("solutions", skip=1, header=F, sep='')
names(VC4)=c("trait","effect","RIM.r","solution","se")
VC4 <- subset(VC4,VC4$effect==5)
VC4 <- VC4[,-2]

VC4 <- reshape(VC4, idvar = "RIM.r", timevar = "trait", direction = "wide")
names(VC4)=c("RIM.r","vcPVS1","sePVS1","vcPDF1","sePDF1","vcLM1","seLM1")

inb4 <-read.delim("renf90.inb", header=F,sep='')
names(inb4)=c("RIM","inb4","code")
inb4 <- inb4[,-3]

ped4.r <-read.delim("renadd05.ped", header=F,sep='')
names(ped4.r)=c("RIM.r","rimpd.r","rimmd.r","v4","V5","V6","V7","n.pad4","n.mad4","RIM")
ped4.r <-ped4.r[,c(1,2,3,8,9,10)]

VC4.j <- merge(ped4.r,inb4, by='RIM')
VC4.j <- merge(VC4,VC4.j, by='RIM.r')

#VC4.j$acPVS1 <- (sqrt(1-(VC4.j$sePVS1*VC4.j$sePVS1)/(0.06350401*(1+VC4.j$inb/100))))*100
#VC4.j$acPDF1 <- (sqrt(1-(VC4.j$sePDF1*VC4.j$sePDF1)/(0.9331199*(1+VC4.j$inb/100))))*100
VC4.j$acLM1 <- (sqrt(1-(VC4.j$seLM1*VC4.j$seLM1)/(32.4*(1+VC4.j$inb/100))))*100

#Me quedo con las variables de interes para el informe
VC4.j <-VC4.j[,-c(1:5,7,9,10)]

rm(VC4,ped4.r,inb4)

##Ver problema al mergear de columnas duplicadas
todo.j <- merge(VC1.j,VC2.j, by='RIM', all=T)
todo.j <-merge(todo.j,VC3.j, by='RIM', all=T)
todo.j <-merge(todo.j,VC4.j, by='RIM', all=T)

rm(VC1.j,VC2.j,VC3.j,VC4.j)

todo.j$con <- apply(todo.j[ ,c('inb1','inb2','inb3','inb4')], 1, max, na.rm = TRUE)
todo.j$n.pad <- apply(todo.j[ ,c("n.pad1","n.pad2","n.pad3","n.pad4")], 1, max, na.rm = TRUE)
todo.j$n.mad <- apply(todo.j[ ,c("n.mad1","n.mad2","n.mad3","n.mad4")], 1, max, na.rm = TRUE)
todo.j$exa <- apply(todo.j[ ,c("acPC1","acPVL1","acPDF1","acPCD")], 1, mean, na.rm = TRUE)

todo.j <- todo.j[,-c(5:7,14:16,22:24,28:30)]

#Calcula los índices crudos y estandarizados

todo.j$i10crudo <- todo.j$vcNCD*927.4 + todo.j$vcPCD*29.3 + todo.j$vcPVL1*531.1 + todo.j$vcPDF1*(-141.5) + todo.j$vcPC2*2.9;
todo.j$i02crudo <- todo.j$vcNCD*927.4 + todo.j$vcPCD*29.3 + todo.j$vcPVL1*531.1 + todo.j$vcPDF1*(-28.3) + todo.j$vcPC2*2.9;

mean.i10c <- mean(todo.j$i10crudo, na.rm=T)
mean.i02c <- mean(todo.j$i02crudo, na.rm=T)

sd.i10c <- sd(todo.j$i10crudo, na.rm=T)
sd.i02c <- sd(todo.j$i02crudo, na.rm=T)


todo.j$ind02 <- 10*(todo.j$i02crudo-(mean.i02c))/sd.i02c+100     
todo.j$ind10 <- 10*(todo.j$i10crudo-(mean.i10c))/sd.i10c+100

todo.j2 <- merge(todo, todo.j, by='RIM',all.y = T)
ab <- todo.j2[todo.j2$AN==2010,] #Año Base

todo.j2$vcNCD <- todo.j2$vcNCD - mean(ab$vcNCD, na.rm=T)
todo.j2$vcPCD <- todo.j2$vcPCD - mean(ab$vcPCD, na.rm=T)
todo.j2$vcPVL1 <- todo.j2$vcPVL1 - mean(ab$vcPVL1, na.rm=T)
todo.j2$vcPDF1 <- todo.j2$vcPDF1 - mean(ab$vcPDF1, na.rm=T)
todo.j2$vcCVF1 <- todo.j2$vcCVF1 - mean(ab$vcCVF1, na.rm=T)
todo.j2$vcPC1 <- todo.j2$vcPC1 - mean(ab$vcPC1, na.rm=T)
todo.j2$vcPC2 <- todo.j2$vcPC2 - mean(ab$vcPC2, na.rm=T)
todo.j2$vcLM1 <- todo.j2$vcLM1 - mean(ab$vcLM1, na.rm=T)
todo.j2$vcRT1 <- todo.j2$vcRT1 - mean(ab$vcRT1, na.rm=T)
todo.j2$vcPVL1 <- todo.j2$vcPVL1 - mean(ab$vcPVL1, na.rm=T)
todo.j2$ind02 <- todo.j2$ind02 - mean(ab$ind02, na.rm=T) + 100
todo.j2$ind10 <- todo.j2$ind10 - mean(ab$ind10, na.rm=T) + 100

todo.j2$depNCD <- todo.j2$vcNCD/2
todo.j2$depPCD <- todo.j2$vcPCD/2
todo.j2$depPVL1 <- todo.j2$vcPVL1/2
todo.j2$depPDF1 <- todo.j2$vcPDF1/2
todo.j2$depCVF1 <- todo.j2$vcCVF1/2
todo.j2$depPC1 <- todo.j2$vcPC1/2
todo.j2$depPC2 <- todo.j2$vcPC2/2
todo.j2$depLM1 <- todo.j2$vcLM1/2
todo.j2$depRT1 <- todo.j2$vcRT1/2
todo.j2$depPVL1 <- todo.j2$vcPVL1/2

rm(todo.j,ab,mean.i02c,mean.i10c,sd.i02c,sd.i10c)
