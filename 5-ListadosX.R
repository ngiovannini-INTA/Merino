#Programa para presentar DEPs de la poblacion Merino Astado; 

dir.create('c:/Drive/Data/Merino/X/Pob/2023/Resultados')
Result <- ('c:/Drive/Data/Merino/X/Pob/2023/Resultados')

minexa <- 60     #esto es la exactitud minima exigida a un padre para ser publicado
minnt <- 10      #esto es el nro minimo de progenie total de un padre para ser publicado
limgen <-5       #esto es el nro de camadas recientes en que un padre tiene que haber tenido progenie
per.int <-1999   #Per?odo de interes > a...
dec <- ','       #AACM lo quiere con ','
an.tg <- 2005    #limite inferior para calculo de las tendencias genéticas

#Esta parte correrla cada vez que se corra para establecimiento

est <-  'Teck'   #Campo a listar

out <- paste('PadresX_',est,'.csv',sep='')
out2 <- paste('Prog.X.2D_',est,'.csv',sep='')
out3 <- paste('Prog.X.4D_',est,'.csv',sep='')
out4 <- paste('TendenciaX_',est,'.csv',sep='')
out5 <- paste('TablaX1_',est,'.csv',sep='')
out6 <- paste('TablaX2_',est,'.csv',sep='')
out7 <- paste('TablaX3_',est,'.csv',sep='')
out8 <- paste('TablaX4_',est,'.csv',sep='')
out9 <- paste('TablaX5_',est,'.csv',sep='')
out10 <- paste('MadresX_',est,'.csv',sep='')

todo.j3 <- merge(todo.j2,gen, by='RIM', all.x=T)
todo.j3$SEXO <- todo.j3$SEXO.x
todo.j3$AN <- ifelse(is.na(todo.j3$AN.x),todo.j3$AN.y,todo.j3$AN.x)
table(todo.j3$AN)


#esto es porque no se calcula pero lo pide el input de la AACM
todo.j3$depPVL2 <- NA
todo.j3$depPDF2 <- NA
todo.j3$depLM2 <- NA
todo.j3$depRT2 <- NA
todo.j3$pob1 <- NA
todo.j3$pob2	<- NA
todo.j3$egd1	<- NA
todo.j3$egd2	<- NA
todo.j3$deppob1	<- NA
todo.j3$deppob2	<- NA
todo.j3$depegd1	<- NA
todo.j3$depegd2 <- NA

todo.j3$MANEJO <- as.factor(ifelse(todo.j3$MANEJO=='cabania'|todo.j3$MANEJO=='galpon','SI','NO'))

#configuro decimales y renombro variables

todo.j3$con <- todo.j3$con*100

todo.j3$PDF1 <- round(todo.j3$PDF1,digits = 2)
todo.j3$CVF1 <- round(todo.j3$CVF1,digits = 2)
todo.j3$PC1 <- round(todo.j3$PC1,digits = 2)
todo.j3$PVS1 <- round(todo.j3$PVS1,digits = 2)

todo.j3$depPCD <- round(todo.j3$depPCD,digits = 2)
todo.j3$depPVL1 <- round(todo.j3$depPVL1,digits = 2)
todo.j3$depPDF1 <- round(todo.j3$depPDF1,digits = 2)
todo.j3$depPC1 <- round(todo.j3$depPC1,digits = 2)
todo.j3$depPC2 <- round(todo.j3$depPC2,digits = 2)
todo.j3$depCVF1 <- round(todo.j3$depCVF1,digits = 2)
todo.j3$depLM1 <- round(todo.j3$depLM1,digits = 2)
todo.j3$depRT1 <- round(todo.j3$depRT1,digits = 2)
todo.j3$ind10 <- round(todo.j3$ind10,digits = 0)
todo.j3$ind02 <- round(todo.j3$ind02,digits = 0)
todo.j3$exa <- round(todo.j3$exa,digits = 0)
todo.j3$con <- round(todo.j3$con,digits = 0)

dashX <- todo.j3[,c('RIM','CAMPO','NOMBRE','ident','rp_padre','MANEJO','AN.x','SEXO','B','PCD','PC1','PVS1','PDF1','depPCD','depPC1','depPVL1','depPDF1',
                    'ind02','ind10','exa','vcPCD','vcPC1','vcPVL1','vcPDF1')]

dashX <- subset(dashX,AN.x>=2000)
dashX$ANIO <- paste("01","/","01","/",dashX$AN.x,sep="")
dashX <- dashX[,-7]

####################################   INFORME POBLACION #######################################

#aqui determina animales con al menos un dato de finura total y por campo;
cpdf1 <- todo.j3
#aqui quedan los padres con el numero de campos con progenie
tx <- as.data.frame.matrix(table(cpdf1$rimpd,cpdf1$CAMPO))
tx$NT <- tx$Cabo+tx$CoyA+tx$DonS+tx$LagT+tx$LVeg+tx$Mait+tx$Mana+tx$MLun+tx$RioP+tx$Teck+tx$CerC
tx$rimpd <- rownames(tx)
tx1 <- tx[,c(12,13)]

#aqui tabula padres x el numero de hijos con al menos un dato de finura
tx2 <- tx
tx2[tx2!=0] <-1
tx2$NC <- tx2$Cabo+tx2$CoyA+tx2$DonS+tx2$LagT+tx2$LVeg+tx2$Mait+tx2$Mana+tx2$MLun+tx2$RioP+tx2$Teck+tx2$CerC
tx2$rimpd <- rownames(tx2)
tx2 <- tx2[,c(13,14)]

tx3 <- merge(tx1,tx2,by='rimpd')
tx3 <- subset(tx3,tx3$rimpd!=0)
tx3 <- subset(tx3,tx3$NT!=0 & tx3$NC!=0)

rm(tx,tx1,tx2,cpdf1)

colnames(tx3)[colnames(tx3)=="rimpd"] <- "RIM"

#Listados de padres con filtros para para exportar a cat?logo de padres AACM
padres1 <- merge(todo.j3,tx3,by='RIM',all.y=T)
padres1 <- subset(padres1,padres1$exa>=minexa & padres1$NT>=minnt)

padres <- padres1[,c('NOMBRE','AN','rp_padre','depPCD','depPVL1','depPDF1','depPC2','depCVF1','depLM1','depRT1','ind10','ind02','exa','NT','NC','con')]
padres <- padres[order(padres$ind02, decreasing = T),]
setwd(Result)

write.table(padres, file = "PadresX_pob.csv", sep=";", row.names = F, col.names = T, quote=F, na="0", dec=dec)

padresX <- padres1[,c('RIM','NOMBRE','AN','rp_padre','depPCD','depPVL1','depPDF1','depPC2','depCVF1','depLM1','depRT1','ind10','ind02','exa','NT','NC','con')]
padresX$B <- "Poll"
padresX$ANIO <- paste("01","/","01","/",padresX$AN,sep="")
padresX <- padresX[,-3]

#Listado de PROGENIE poblacion para exportar en ese orden a merino.ogr.ar AACM;
progen <- subset(todo.j3,todo.j3$AN==ult.camada  & !is.na(todo.j3$CAMPO) & !is.na(todo.j3$PDF1))
progen <- progen[,c('NOMBRE','SEXO','CRIANZA','MANEJO','PCD','PC1','PVS1','PVL1','PDF1','CVF1','RIN1','FC1','LM1','RT1',
                    'depPCD','depPC1','depPVL1','depPDF1','depLM1','depRT1','ind10','ind02','exa','con')]
progen <- progen[order(progen$NOMBRE),]

write.table(progen, file = "Prog.X.2D_pob.csv", sep=";", row.names = F, col.names = T, quote=F, na="0", dec=dec)

#Tendencia poblacion para exportar a cat?logo de padres AACM
TG.pobX <- aggregate(cbind(vcPCD,vcPC1,vcPC2,vcPVL1,vcPDF1,vcCVF1,vcRT1,vcLM1,ind10,ind02) ~ AN, 
                    FUN=mean, subset = AN >= an.tg, data=todo.j2)

plot(TG.pobX$AN,TG.pobX$ind10)
lines(TG.pobX$AN,TG.pobX$ind10)

write.table(TG.pobX, file = "TendenciaX_pob.csv", sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)


rm(padres1,padres)

#Tabla: Estructura de datos poblacion Merino Astado
pob1 <- subset(todo.j3,todo.j3$AN>per.int & !is.na(todo.j3$PDF1))

#pob1$SEXO <- droplevels(pob1$SEXO)
#pob1$MANEJO <- droplevels(pob1$MANEJO)
#pob1$CRIANZA <- droplevels(pob1$CRIANZA)
#pob1$rep <- droplevels(pob1$rep)

t10 <- table(pob1$AN,pob1$SEXO)
t11 <- table(pob1$AN,pob1$CRIANZA)
t12 <- table(as.factor(pob1$AN),as.factor(pob1$MANEJO):as.factor(pob1$SEXO))
t13 <- table(pob1$AN,pob1$rep)

tabla1 <- as.data.frame(cbind(t10,t13,t11,t12))
tabla1$total <- tabla1$H+tabla1$M
t14 <- (colSums(tabla1))


tabla1 <- rbind(tabla1,t14)
tabla1$AN <- row.names(tabla1)
tabla1$AN <- ifelse(tabla1$AN==nrow(tabla1),'Total',tabla1$AN)

names(tabla1)=c("SEX:h","SEX:m","rep:N","rep:TE","CRIAN:multip","CRIAN:simple","MANEJO:no/SEX:h","MANEJO:no/SEX:m","MANEJO:si/SEX:h",'MANEJO:si/SEX:m','Total','AN')
tabla1 <- tabla1[,c(ncol(tabla1),ncol(tabla1)-1,1:(ncol(tabla1)-2))] #reordeno las columnas
write.table(tabla1, file = "TablaX1_pob.csv", sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(t10,t11,t12,t13,t14,tabla1,tendencia,pob1)


#Tabla: Progenie por padre por a?o en poblacion Merino Astado
pob2 <- subset(todo.j3,todo.j3$AN>(ult.camada-10) & !is.na(todo.j3$PDF1) & todo.j3$CAMPO!='Pilc' & todo.j3$CAMPO!='RioM'& !is.na(todo.j3$CAMPO))
pob2$rp_padre <- droplevels(as.factor(pob2$rp_padre))
tabla2 <- as.data.frame.matrix(table(pob2$rp_padre,pob2$AN))
tabla2$Total <- rowSums(tabla2)

t15 <- (colSums(tabla2))
tabla2 <- rbind(tabla2,t15)
tabla2$rp_padre <- row.names(tabla2)
tabla2[tabla2==0] <-NA
tabla2$rp_padre <- ifelse(tabla2$rp_padre==nrow(tabla2),'Total',tabla2$rp_padre)

tabla2 <- subset(tabla2,tabla2$rp_padre!='')
tabla2 <- tabla2[,c(ncol(tabla2),1:(ncol(tabla2)-1))] #reordeno las columnas
write.table(tabla2, file = "TablaX2_pob.csv", sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(t15,tabla2,pob2)

#Percentiles
progen2 <- subset(todo.j3,todo.j3$AN==ult.camada  & !is.na(todo.j3$CAMPO) & !is.na(todo.j3$PDF1))
progen2 <- progen2[,c('NOMBRE','SEXO','CRIANZA','MANEJO','PCD','PC1','PVS1','PVL1','PDF1','CVF1','RIN1','FC1','LM1','RT1',
                    'depPCD','depPC1','depPC2','depPVL1','depPDF1','depCVF1','depLM1','depRT1','ind10','ind02','exa','con')]

depPCD <- quantile(progen2$depPCD,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
depPC1 <- quantile(progen2$depPC1,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
depPC2 <- quantile(progen2$depPC2,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
depPVL1 <- quantile(progen2$depPVL1,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
depPDF1 <- quantile(progen2$depPDF1,c(1,0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.05,0.01,0),na.rm=T)
depCVF1 <- quantile(progen2$depCVF1,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
depRT1 <- quantile(progen2$depRT1,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
depLM1 <- quantile(progen2$depLM1,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
ind10 <- quantile(progen2$ind10,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)
ind02 <- quantile(progen2$ind02,c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),na.rm=T)

percentiles <- as.data.frame(cbind(depPCD,depPC1,depPC2,depPVL1,depPDF1,depCVF1,depRT1,depLM1,ind10,ind02))
percentiles <- round(percentiles,digits=2)
percentiles$ind10 <- round(percentiles$ind10,digits = 0)
percentiles$ind02 <- round(percentiles$ind02,digits = 0)
percentiles$Percentil <-c('100%','99%','95%','90%','80%','70%','60%','50%','40%','30%','20%','10%','5%','1%','0%')

percentiles <- percentiles[,c(11,1:10)]
percentiles <- percentiles[order(percentiles$ind10, decreasing = T),]

write.table(percentiles, file = "PercentilesX.csv", sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

#Tabla: Promedio de borregos POB manejados a campo por a?o
pob3 <- subset(todo.j3,todo.j3$AN>(per.int) & todo.j3$SEXO=='M' & todo.j3$MANEJO=='NO')

PC1 <- round(tapply(pob3$PC1,pob3$AN, mean, na.rm=T), digits=2)
PVS1 <- round(tapply(pob3$PVS1,pob3$AN, mean, na.rm=T), digits=2)
RIN1 <- round(tapply(pob3$RIN1,pob3$AN, mean, na.rm=T), digits=2)
PVL1 <- round(tapply(pob3$PVL1,pob3$AN, mean, na.rm=T), digits=2)
PDF1 <- round(tapply(pob3$PDF1,pob3$AN, mean, na.rm=T), digits=2)
CVF1 <- round(tapply(pob3$CVF1,pob3$AN, mean, na.rm=T), digits=2)
FC1 <- round(tapply(pob3$FC1,pob3$AN, mean, na.rm=T), digits=2)
LM1 <- round(tapply(pob3$LM1,pob3$AN, mean, na.rm=T), digits=2)
RT1 <- round(tapply(pob3$RT1,pob3$AN, mean, na.rm=T), digits=2)
N <- table(pob3$AN)

tabla3p <- as.data.frame(cbind(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1))
tabla3p$AN <- rownames(tabla3p)
tabla3p <- tabla3p[,c(ncol(tabla3p),1:(ncol(tabla3p)-1))] #reordeno las columnas
write.table(tabla3p, file = 'TablaX3_pob.csv', sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(pob3,N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1,
   depPCD,depPC1,depPC2,depPVL1,depPDF1,depCVF1,depRT1,depLM1,ind10,ind02,percentiles,tabla3p)

#Tabla: Promedio de borregos POB suplementados por a?o
pob4 <- subset(todo.j3,todo.j3$AN>(per.int) & todo.j3$SEXO=='M' & todo.j3$MANEJO=='SI')


PC1 <- round(tapply(pob4$PC1,pob4$AN, mean, na.rm=T), digits=2)
PVS1 <- round(tapply(pob4$PVS1,pob4$AN, mean, na.rm=T), digits=2)
RIN1 <- round(tapply(pob4$RIN1,pob4$AN, mean, na.rm=T), digits=2)
PVL1 <- round(tapply(pob4$PVL1,pob4$AN, mean, na.rm=T), digits=2)
PDF1 <- round(tapply(pob4$PDF1,pob4$AN, mean, na.rm=T), digits=2)
CVF1 <- round(tapply(pob4$CVF1,pob4$AN, mean, na.rm=T), digits=2)
FC1 <- round(tapply(pob4$FC1,pob4$AN, mean, na.rm=T), digits=2)
LM1 <- round(tapply(pob4$LM1,pob4$AN, mean, na.rm=T), digits=2)
RT1 <- round(tapply(pob4$RT1,pob4$AN, mean, na.rm=T), digits=2)
N <- table(pob4$AN)

tabla4p <- as.data.frame(cbind(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1))
tabla4p$AN <- rownames(tabla4p)
tabla4p <- tabla4p[,c(ncol(tabla4p),1:(ncol(tabla4p)-1))] #reordeno las columnas
write.table(tabla4p, file = 'TablaX4_pob.csv', sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,LM1,RT1,tabla4p)

#Tabla: Promedio de borregas por a?o
pob5 <- subset(todo.j3,todo.j3$AN>(per.int) & todo.j3$SEXO=='H')


PC1 <- round(tapply(pob5$PC1,pob5$AN, mean, na.rm=T), digits=2)
PVS1 <- round(tapply(pob5$PVS1,pob5$AN, mean, na.rm=T), digits=2)
RIN1 <- round(tapply(pob5$RIN1,pob5$AN, mean, na.rm=T), digits=2)
PVL1 <- round(tapply(pob5$PVL1,pob5$AN, mean, na.rm=T), digits=2)
PDF1 <- round(tapply(pob5$PDF1,pob5$AN, mean, na.rm=T), digits=2)
CVF1 <- round(tapply(pob5$CVF1,pob5$AN, mean, na.rm=T), digits=2)
FC1 <- round(tapply(pob5$FC1,pob5$AN, mean, na.rm=T), digits=2)
LM1 <- round(tapply(pob5$LM1,pob5$AN, mean, na.rm=T), digits=2)
RT1 <- round(tapply(pob5$RT1,pob5$AN, mean, na.rm=T), digits=2)
N <- table(pob5$AN)

tabla5p <- as.data.frame(cbind(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1))
tabla5p$AN <- rownames(tabla5p)
tabla5p <- tabla5p[,c(ncol(tabla5p),1:(ncol(tabla5p)-1))] #reordeno las columnas
write.table(tabla5p, file = 'TablaX5_pob.csv', sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

#Tabla para cargar datos WEB
web <- todo.j3[,c('RIM','PVS2','RIN2','PVL2','PDF2','CVF2','FC2','PC2','PC1','PVS1','PVL1','PDF1',
                  'CVF1','RIN1','FC1','ind02','ind10','depPC1','depPC2','depPVL1','depPVL2','depPDF1','depPDF2',
                  'exa','PCD','LM1','LM2','RT1','RT2','depPCD','depLM1','depLM2','depRT1','depRT2','con',
                  'pob1','pob2','egd1','egd2','deppob1','deppob2','depegd1','depegd2')]

#Estos son animales NO Pedigree
web <- subset(web, !(web$RIM>4132186 & web$RIM<5000042))

web <- web[order(web$RIM),]

write.table(web, file = 'webX.csv', sep=";", row.names = F, col.names = T, quote=F, na="", dec=dec)

rm(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1,tabla5p)


####################################   INFORME CAMPOS #######################################
setwd(Result)
#Listados de PADRES por campo
padres2 <- subset(todo.j3,todo.j3$CAMPO==est)
padres2 <- as.data.frame(unique(padres2$rimpd))
colnames(padres2) <- 'RIM'
padres2 <- subset(padres2, padres2$RIM!=0)

padres3 <- merge(todo.j3,tx3,by='RIM',all.y=T)
padres4 <- merge(padres3,padres2,by='RIM',all.y=T)

pad.est <- padres4[,c('RIM','NOMBRE','AN','rp_padre','depPCD','depPC1','depPC2','depPVL1','depPDF1','depCVF1','depLM1','depRT1','ind10','ind02','exa','NT','NC','con')]
pad.est <- pad.est[order(pad.est$ind02, decreasing = T),]

write.table(pad.est, file = out, sep=";", row.names = F, col.names = T, quote=F, na="0", dec=dec)

#Listado de progenie 2D de campo
prog2D.est <- subset(todo.j3,todo.j3$AN==ult.camada & todo.j3$CAMPO==est & !is.na(todo.j3$CAMPO)) #Para solo PCD y PVS
#prog2D.est <- subset(todo.j3,todo.j3$AN==ult.camada & todo.j3$CAMPO==est & !is.na(todo.j3$CAMPO) & !is.na(todo.j3$PDF1))#filtro con finura
prog2D.est <- prog2D.est[,c('carav','SEXO','CRIANZA','MANEJO','PCD','PC1','PVS1','PVL1','PDF1','CVF1','RIN1','FC1','LM1','RT1', 
                            'depPCD','depPC1','depPC2','depPVL1','depPDF1','depLM1','depRT1','ind10','ind02','exa','con')]
prog2D.est <- prog2D.est[order(prog2D.est$carav),]
write.table(prog2D.est, file = out2, sep=";", row.names = F, col.names = T, quote=F, na="0", dec=dec)

#Listado de progenie 4D de campo
#prog4D.est <- subset(todo.j3,todo.j3$AN==ult.camada-1 & todo.j3$CAMPO==est & !is.na(todo.j3$CAMPO) & !is.na(todo.j3$PDF1))#sin finura
prog4D.est <- subset(todo.j3,todo.j3$AN==ult.camada-1 & todo.j3$CAMPO==est & !is.na(todo.j3$CAMPO))
prog4D.est <- prog4D.est[,c('carav','SEXO','CRIANZA','MANEJO','PCD','PC1','PVS1','PVL1','PDF1','CVF1','RIN1','FC1','LM1','RT1',
                            'depPCD','depPC1','depPC2','depPVL1','depPDF1','depLM1','depRT1','ind10','ind02','exa','con')]
prog4D.est <- prog4D.est[order(prog4D.est$carav),]
write.table(prog4D.est, file = out3, sep=";", row.names = F, col.names = T, quote=F, na="0", dec=dec)

#Listado de MADRES del campo (opcional)
#madres <- subset(todo.j3,todo.j3$CAMPO==est & todo.j3$AN==ult.camada)
#madres <- as.data.frame(unique(madres$rimmd))
#colnames(madres) <- 'RIM'
#madres <- subset(madres, madres$RIM!=0)

#madres2 <- merge(todo.j3,madres,by='RIM',all.y=T)

#mad.est <- madres2[,c('RIM','NOMBRE','AN','rp_padre','depPCD','depPC1','depPC2','depPVL1','depPDF1','depCVF1','depLM1','depRT1','ind10','ind02','exa','con')]
#mad.est <- mad.est[order(mad.est$NOMBRE, decreasing = F),]

#write.table(mad.est, file = out10, sep=";", row.names = F, col.names = T, quote=F, na="0", dec=dec)

#Listado tendencia de establecimiento
TG.est <- aggregate(cbind(vcPCD,vcPC1,vcPC2,vcPVL1,vcPDF1,vcCVF1,vcRT1,vcLM1,ind10,ind02) ~ AN, 
                    FUN=mean, subset = CAMPO %in% est, data=todo.j2)

TG.est <- subset(TG.est, AN>=an.tg)

plot(TG.est$AN,TG.est$ind10)
lines(TG.est$AN,TG.est$ind10)

write.table(TG.est, file = out4, sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(pad.est,padres2,padres3,padres4,prog2D.est,prog4D.est)


#Tabla: Progenie por padre por a?o en establecimiento
est2 <- subset(todo.j3,todo.j3$AN>(ult.camada-10) & !is.na(todo.j3$PDF1) & todo.j3$CAMPO==est & !is.na(todo.j3$CAMPO))
est2$rp_padre <- droplevels(as.factor(est2$rp_padre))
tabla2.e <- as.data.frame.matrix(table(est2$rp_padre,est2$AN))
tabla2.e$Total <- rowSums(tabla2.e)

t18 <- (colSums(tabla2.e))
tabla2.e <- rbind(tabla2.e,t18)
tabla2.e$rp_padre <- row.names(tabla2.e)
tabla2.e[tabla2.e==0] <-NA
tabla2.e$rp_padre <- ifelse(tabla2.e$rp_padre==nrow(tabla2.e),'Total',tabla2.e$rp_padre)

tabla2.e <- subset(tabla2.e,tabla2.e$rp_padre!='')
tabla2.e <- tabla2.e[,c(ncol(tabla2.e),1:(ncol(tabla2.e)-1))] #reordeno las columnas
write.table(tabla2.e, file = out6, sep=";", row.names = F, col.names = T, quote=F, na=".",dec = dec)

rm(tabla2.e,est2,t18)

#Tabla: Promedio de borregos manejados a campo por a?o
est3 <- subset(todo.j3,todo.j3$AN>(per.int) & todo.j3$CAMPO==est & todo.j3$SEXO=='M' & todo.j3$MANEJO=='NO')


PC1 <- round(tapply(est3$PC1,est3$AN, mean, na.rm=T), digits=2)
PVS1 <- round(tapply(est3$PVS1,est3$AN, mean, na.rm=T), digits=2)
RIN1 <- round(tapply(est3$RIN1,est3$AN, mean, na.rm=T), digits=2)
PVL1 <- round(tapply(est3$PVL1,est3$AN, mean, na.rm=T), digits=2)
PDF1 <- round(tapply(est3$PDF1,est3$AN, mean, na.rm=T), digits=2)
CVF1 <- round(tapply(est3$CVF1,est3$AN, mean, na.rm=T), digits=2)
FC1 <- round(tapply(est3$FC1,est3$AN, mean, na.rm=T), digits=2)
LM1 <- round(tapply(est3$LM1,est3$AN, mean, na.rm=T), digits=2)
RT1 <- round(tapply(est3$RT1,est3$AN, mean, na.rm=T), digits=2)
N <- table(est3$AN)

tabla3.e <- as.data.frame(cbind(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,LM1,RT1))
tabla3.e$AN <- rownames(tabla3.e)
tabla3.e <- tabla3.e[,c(ncol(tabla3.e),1:(ncol(tabla3.e)-1))] #reordeno las columnas
write.table(tabla3.e, file = out7, sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(est3,tabla3.e,N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1)

#Tabla: Promedio de borregos suplementados por a?o
#est3 <- subset(todo.j3,todo.j3$AN>(per.int) & !is.na(todo.j3$PDF1) & todo.j3$CAMPO==est & !is.na(todo.j3$CAMPO) & todo.j3$SEXO=='M' & todo.j3$MANEJO=='NO')
est4 <- subset(todo.j3,todo.j3$AN>(per.int) & todo.j3$CAMPO==est & todo.j3$SEXO=='M' & todo.j3$MANEJO=='SI')


PC1 <- round(tapply(est4$PC1,est4$AN, mean, na.rm=T), digits=2)
PVS1 <- round(tapply(est4$PVS1,est4$AN, mean, na.rm=T), digits=2)
RIN1 <- round(tapply(est4$RIN1,est4$AN, mean, na.rm=T), digits=2)
PVL1 <- round(tapply(est4$PVL1,est4$AN, mean, na.rm=T), digits=2)
PDF1 <- round(tapply(est4$PDF1,est4$AN, mean, na.rm=T), digits=2)
CVF1 <- round(tapply(est4$CVF1,est4$AN, mean, na.rm=T), digits=2)
FC1 <- round(tapply(est4$FC1,est4$AN, mean, na.rm=T), digits=2)
LM1 <- round(tapply(est4$LM1,est4$AN, mean, na.rm=T), digits=2)
RT1 <- round(tapply(est4$RT1,est4$AN, mean, na.rm=T), digits=2)
N <- table(est4$AN)

tabla4.e <- as.data.frame(cbind(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1))
tabla4.e$AN <- rownames(tabla4.e)
tabla4.e <- tabla4.e[,c(ncol(tabla4.e),1:(ncol(tabla4.e)-1))] #reordeno las columnas
write.table(tabla4.e, file = out8, sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1,tabla4.e)

#Tabla: Promedio de borregas por a?o
est5 <- subset(todo.j3,todo.j3$AN>(per.int) & todo.j3$CAMPO==est & todo.j3$SEXO=='H')


PC1 <- round(tapply(est5$PC1,est5$AN, mean, na.rm=T), digits=2)
PVS1 <- round(tapply(est5$PVS1,est5$AN, mean, na.rm=T), digits=2)
RIN1 <- round(tapply(est5$RIN1,est5$AN, mean, na.rm=T), digits=2)
PVL1 <- round(tapply(est5$PVL1,est5$AN, mean, na.rm=T), digits=2)
PDF1 <- round(tapply(est5$PDF1,est5$AN, mean, na.rm=T), digits=2)
CVF1 <- round(tapply(est5$CVF1,est5$AN, mean, na.rm=T), digits=2)
FC1 <- round(tapply(est5$FC1,est5$AN, mean, na.rm=T), digits=2)
LM1 <- round(tapply(est5$LM1,est5$AN, mean, na.rm=T), digits=2)
RT1 <- round(tapply(est5$RT1,est5$AN, mean, na.rm=T), digits=2)
N <- table(est5$AN)

tabla5.e <- as.data.frame(cbind(N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1))
tabla5.e$AN <- rownames(tabla5.e)
tabla5.e <- tabla5.e[,c(ncol(tabla5.e),1:(ncol(tabla5.e)-1))] #reordeno las columnas
write.table(tabla5.e, file = out9, sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(est4,est5,pob4,pob5,tabla5.e,N,PC1,PVS1,RIN1,PVL1,PDF1,CVF1,FC1,LM1,RT1)

#Tabla: Estructura de datos del establecimiento
est1 <- subset(todo.j3,todo.j3$AN>per.int & !is.na(todo.j3$PDF1)&todo.j3$CAMPO==est)

est1$SEXO <- droplevels(as.factor(est1$SEXO))
est1$MANEJO <- droplevels(as.factor(est1$MANEJO))
est1$CRIANZA <- droplevels(as.factor(est1$CRIANZA))
est1$rep <- droplevels(as.factor(est1$rep))

t10 <- table(est1$AN,est1$SEXO)
t11 <- table(est1$AN,est1$CRIANZA)
t12 <- table(est1$AN,est1$MANEJO:est1$SEXO)
t16 <- table(est1$AN,est1$rep)

tabla1.e <- as.data.frame(cbind(t10,t16,t11,t12))
tabla1.e$Total <- tabla1.e$H+tabla1.e$M

t17 <- (colSums(tabla1.e))
tabla1.e <- rbind(tabla1.e,t17)

tabla1.e$AN <- row.names(tabla1.e)
tabla1.e$AN <- ifelse(tabla1.e$AN==nrow(tabla1.e),'Total',tabla1.e$AN)

colnames(tabla1.e)[colnames(tabla1.e)=="H"] <- "SEX:H"
colnames(tabla1.e)[colnames(tabla1.e)=="M"] <- "SEX:M"
colnames(tabla1.e)[colnames(tabla1.e)=="N"] <- "rep:N"
colnames(tabla1.e)[colnames(tabla1.e)=="TE"] <- "rep:TE"
colnames(tabla1.e)[colnames(tabla1.e)=="multip"] <- "CRIAN:multip"
colnames(tabla1.e)[colnames(tabla1.e)=="simple"] <- "CRIAN:simple"
colnames(tabla1.e)[colnames(tabla1.e)=="NO:H"] <- "MANEJO:no/SEX:h"
colnames(tabla1.e)[colnames(tabla1.e)=="NO:M"] <- "MANEJO:no/SEX:m"
colnames(tabla1.e)[colnames(tabla1.e)=="SI:H"] <- "MANEJO:si/SEX:h"
colnames(tabla1.e)[colnames(tabla1.e)=="SI:M"] <- "MANEJO:si/SEX:m"

#tabla1.e <- tabla1.e[,c("AN","Total","SEX:H","SEX:M","rep:N","rep:TE","CRIAN:multip","CRIAN:simple","MANEJO:no/SEX:h","MANEJO:no/SEX:m","MANEJO:si/SEX:h",'MANEJO:si/SEX:m')]
# Cuando no ande correr colnames(tabla1.e) para ordenar de acuerdo a los datos disponibles
tabla1.e <- tabla1.e[,c(ncol(tabla1.e),ncol(tabla1.e)-1,1:(ncol(tabla1.e)-2))] #reordeno las columnas
write.table(tabla1.e, file = out5, sep=";", row.names = F, col.names = T, quote=F, na=".", dec=dec)

rm(t10,t11,t12,t16,t17,tabla1.e,est1)




