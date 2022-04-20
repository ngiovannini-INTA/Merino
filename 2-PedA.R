#Programa para armar la base de genealogia de la poblacion Merino, A y X;

#esto es el pedigree AACM (rim que comienzan con 1 son nuevos, 8 son machos SRA, 9 son hembras SRA)
#para cargar el pedigre: en www.merino.org.ar logearse con usuario=admin y password=codes y generar archivo de backup.txt
#abrir un excel y 'Obtener datos externos' como texto y guardar como aacm1.csv;

#Creo y establezco directorios de trabajo


dir.create('c:/Drive/Data/Merino/A/Pob/2022')
dir.create('c:/Drive/Data/Merino/A/Pob/2022/BLUP1')
dir.create('c:/Drive/Data/Merino/A/Pob/2022/BLUP2')
dir.create('c:/Drive/Data/Merino/A/Pob/2022/BLUP3')
dir.create('c:/Drive/Data/Merino/A/Pob/2022/BLUP4')

Provino <- ('c:/Drive/Data/Merino/A/Pob/2022')

BLUP1 <- ('c:/Drive/Data/Merino/A/Pob/2022/BLUP1')
BLUP2 <- ('c:/Drive/Data/Merino/A/Pob/2022/BLUP2')
BLUP3 <- ('c:/Drive/Data/Merino/A/Pob/2022/BLUP3')
BLUP4 <- ('c:/Drive/Data/Merino/A/Pob/2022/BLUP4')

ult.camada=2020

library(lubridate)

aacm1 <-read.csv('c:/Drive/Data/Merino/PedAX/aacm1.csv', sep=';')
aacm <- aacm1
aacm$RIM <- aacm$cRilAnimal
aacm$rimmd <- aacm$cRilMadre
aacm$rimpd <- aacm$cRilPadre
aacm$ident <- aacm$iAnimal
aacm$carav <- aacm$ident
aacm$B <- aacm$cLinea
  aacm$B[aacm$B == ''] <- 'A'
  aacm$B[aacm$B == '0X'] <- 'X'
  aacm$B[aacm$B == 'PX'] <- 'X'
aacm$rep <- aacm$iEmbriones
aacm$TE <- aacm$iEmbriones
  aacm$TE[aacm$TE=='N'] <- '' #Esto solo para generar NOMBRE 
aacm$cnac <- aacm$cEstablecimientoNac
aacm$cact <- aacm$cEstablecimientoAct
aacm$SEXO <- aacm$bGenero
  aacm$SEXO[aacm$SEXO=="Macho"] <- "M"
  aacm$SEXO[aacm$SEXO=="Hembra"] <- "H"
aacm$fNacimiento <- strptime(aacm$fNacimiento, format='%d/%m/%Y')
aacm$AN <- year(aacm$fNacimiento)
aacm$MN <- month(aacm$fNacimiento)
aacm$DN <- day(aacm$fNacimiento)

#aacm$NOMBRE<-paste(aacm$dAnimal,aacm$ident,aacm$cLinea,' ',aacm$TE, sep="")  
aacm$NOMBRE<-paste(aacm$dAnimal,aacm$ident,aacm$cLinea,aacm$TE, sep=" ")
aacm<-aacm[,c('RIM',"rimmd","rimpd","carav","ident","B","cnac","cact","SEXO","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]

#esta es la genealogia de pilca (rim comienzan con 6);
pilca <- read.csv('c:/Drive/Data/Merino/A/Pilca/pilca.csv',sep=';')

pilca$cnac <- 800
pilca$cact <- 800

pilca$rimpd <- ifelse(is.na(pilca$rimpd),0,pilca$rimpd)
pilca$rimmd <- ifelse(is.na(pilca$rimmd),0,pilca$rimmd)

pilca$ident <- pilca$carav

pilca$DN <- ifelse(is.na(pilca$DN),22,pilca$DN)
pilca$MN <- ifelse(is.na(pilca$MN),10,pilca$MN)

pilca <- pilca[,c('RIM',"rimmd","rimpd","carav","ident","B","cnac","cact","SEXO","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]

#esta es la genealogia de Rio Mayo (rim comienzan con 7);
rioma <- read.csv('c:/Drive/Data/Merino/A/Riomayo/rioma.csv',sep=';')

rioma$cnac <- 801
rioma$cact <- 801

rioma$rimpd <- ifelse(is.na(rioma$rimpd),0,rioma$rimpd)
rioma$rimmd <- ifelse(is.na(rioma$rimmd),0,rioma$rimmd)

rioma$ident <- rioma$carav

rioma$DN <- ifelse(is.na(rioma$DN),11,rioma$DN)
rioma$MN <- ifelse(is.na(rioma$MN),10,rioma$MN)

rioma <- rioma[,c('RIM',"rimmd","rimpd","carav","ident","B","cnac","cact","SEXO","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]


#esto es lo que falta en el pedigree de la AACM (rim comienzan con 5);
falta <- read.csv('c:/Drive/Data/Merino/PedAX/falta.csv', sep=';')
falta <- falta[,c('RIM',"rimmd","rimpd","carav","ident","B","cnac","cact","SEXO","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]

#aqui esta toda la genealogía; 
todo.ped <- rbind(aacm,falta,pilca,rioma)
rm(aacm,aacm1,falta,pilca,rioma)

#Calculo edad de madre y agrego a todo
#Aqui genero las madres no duplicadas
uno <- todo.ped[order(todo.ped$rimmd),]
uno <- uno[!duplicated(uno$rimmd),]
uno <- uno[,c(1,2,10)]

#aquí genero otro archivo con las madres como RIM y su correspondiente AN
dos <- todo.ped
dos$rimmd <- dos$RIM
dos$anmad <- dos$AN

dos <- dos[,c('rimmd', "anmad" )]

tres <- merge(dos,uno, by='rimmd')

tres$edadm <- tres$AN-tres$anmad
tres <- subset(tres,tres$edadm>0)

tres <- tres[,c('rimmd', "edadm" )]

gen <- merge(todo.ped,tres,by='rimmd', all=T)
ped <- gen[,c("RIM","rimpd","rimmd")]

setwd(BLUP1)
write.table(ped, file = "pedA.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP2)
write.table(ped, file = "pedA.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP3)
write.table(ped, file = "pedA.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP4)
write.table(ped, file = "pedA.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

rm(uno,dos,tres,todo.ped,ped)

