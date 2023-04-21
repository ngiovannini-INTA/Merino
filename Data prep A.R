#programa para preparar los datos de Astados para subir a la base de datos de Merino
#para cargar el pedigre: en www.merino.org.ar logearse con usuario=admin y password=codes y 
#generar archivo de backup.txt
#al descarcar el Backup, importarlo al excel como "delimitado de ancho fijo" y definir bien 
#las columnas y formatos de texto correctos y convertirlo a csv

#VARS: CAMPO	CARAV	RIM	AN	SEX	CRIANZA	MANEJO	PVS1	PC1	RIN1	PVL1	PDF1	CVF1	FC1	LM1	RT1	DE1	ME1	AE1	LOTE1	OBS1	PVS2	PC2	RIN2	PVL2	PDF2	CVF2	FC2	LM2	DE2	ME2	AE2	LOTE2	OBS2
#para 


#PARAMETROS
#direccion del archivo
arch <- 'C:/Drive/Data/Merino/A/Leleque/2023/hembras.csv'
wd <- 'C:/Drive/Data/Merino/A/Leleque/2023/'
pcd <- 'C:/Drive/Data/Merino/A/Leleque/2023/pcd.csv' #CARAV	DD	MD	AD	PCD
campo <- 'Lele'
manejo <- 'campo'  #campo, cabania, galpon
ult.cam <- 2021    
lote <- 'A'
cod.campo <- 5   #Cabo:277	Cris:257	Lele:5	Pilca	Rio Mayo	RioP:265	Teck:3	Mana:239 Cerc:522
d.esq1 <- 20 
m.esq1 <- 10
LOTE1 <- '2233/1'
output <- 'H.csv'

library(lubridate)

#PROCESOS
dat <- read.csv(arch,sep=';',header=T,skip=3)
names(dat)=c('CARAV','PVS1','PC1','RIN1','PVL1','PDF1','CVF1','FC1','LMOFDA1','LM1','CVLM1','RT1','CVRT','PU','ME','BA','Padre','Madre','depPC','depPVL','depPDF','Indice','Orden','Obs')
dat$LM1 <- ifelse(is.na(dat$LM1),dat$LMOFDA1,dat$LM1)
dat <- dat[,c(1:8,10,12)]

pcd <- read.csv(pcd,sep=';',header=T)
dat <- merge(dat,pcd,by='CARAV',all.x=T)

#Chequeo duplicados
dups <- dat[duplicated(dat$CARAV),]

#Genero variables necesarias
dat$CAMPO <- campo
dat$MANEJO <- manejo
dat$DE1 <- d.esq1
dat$ME1 <- m.esq1
dat$AE1 <- ult.cam+1
dat$LOTE1 <- LOTE1

#Importo el PED de la Merino y me quedo con lo que necesito
aacm1 <-read.csv('c:/Drive/Data/Merino/PedAX/aacm1.csv', sep=';')
aacm <- aacm1
aacm$RIM <- aacm$cRilAnimal
aacm$rimmd <- aacm$cRilMadre
aacm$rimpd <- aacm$cRilPadre
aacm$ident <- aacm$iAnimal
aacm$CARAV <- aacm$ident
aacm$B <- as.factor(aacm$cLinea)
levels(aacm$B)[levels(aacm$B)==''] <- "A"
levels(aacm$B)[levels(aacm$B)=="0X"] <- "X"
levels(aacm$B)[levels(aacm$B)=="PX"] <- "X"
aacm$rep <- aacm$iEmbriones
aacm$cnac <- aacm$cEstablecimientoNac
aacm$cact <- aacm$cEstablecimientoAct
aacm$SEX <- as.factor(aacm$bGenero)
levels(aacm$SEX)[levels(aacm$SEX)=="Macho"] <- "M"
levels(aacm$SEX)[levels(aacm$SEX)=="Hembra"] <- "H"
aacm$fNacimiento <- strptime(aacm$fNacimiento, format='%d/%m/%Y')
aacm$AN <- year(aacm$fNacimiento)
aacm$MN <- month(aacm$fNacimiento)
aacm$DN <- day(aacm$fNacimiento)

aacm$NOMBRE<-paste(aacm$dAnimal,'_',aacm$ident,aacm$cLinea, sep="")  
aacm<-aacm[,c('RIM',"rimmd","rimpd","CARAV","ident","B","cnac","cact","SEX","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]

ped <- subset(aacm,aacm$AN==ult.cam & aacm$B=='A'& aacm$cnac==cod.campo)


#calculo crianza
uno <- ped[duplicated(ped$rimmd),]
uno$CRIANZA <- 'multip'                         #HABILITAR si hay nacimientos dobles en el lote
uno <- subset(uno,uno$rep=='N')                 #HABILITAR si hay nacimientos dobles en el lote
uno <- uno[,c(1,17)]                            #HABILITAR si hay nacimientos dobles en el lote
dos <- merge(ped, uno, by='RIM', all=T)         #HABILITAR si hay nacimientos dobles en el lote
#dos <- dat                                     #SUPRIMIR si hay nacimientos dobles en el lote 
#dos$CRIANZA <- NA                              #SUPRIMIR si hay nacimientos dobles en el lote 
dos$CRIANZA <- ifelse(is.na(dos$CRIANZA),
                      'simple',dos$CRIANZA)

rim <-dos[,c(1,4,9,10,17)]                      #HABILITAR si hay nacimientos dobles en el lote
#rim <-ped[,c(1,4,9,10)]                        #SUPRIMIR si hay nacimientos dobles en el lote
tres <- merge(dat,rim, by='CARAV', all.x = T)   #HABILITAR si hay nacimientos dobles en el lote
#tres <- merge(dos,rim, by='CARAV', all.x = T)  #SUPRIMIR si hay nacimientos dobles en el lote
errores <-subset(tres,is.na(tres$RIM))

setwd(wd)
write.table(tres, file = output, sep=";", row.names = F, col.names = T, quote=F, na='')

