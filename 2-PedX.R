#Programa para armar la base de genealogia de la poblacion Merino, A y X;

#esto es el pedigree AACM (rim que comienzan con 1 son nuevos, 8 son machos SRA, 9 son hembras SRA)
#para cargar el pedigre: en www.merino.org.ar logearse con usuario=admin y password=codes y generar archivo de backup.txt
#abrir un excel y 'Obtener datos externos' como texto y guardar como aacm1.csv;

#Creo y establezco directorios de trabajo


dir.create('c:/Drive/Data/Merino/X/Pob/2022')
dir.create('c:/Drive/Data/Merino/X/Pob/2022/BLUP1')
dir.create('c:/Drive/Data/Merino/X/Pob/2022/BLUP2')
dir.create('c:/Drive/Data/Merino/X/Pob/2022/BLUP3')
dir.create('c:/Drive/Data/Merino/X/Pob/2022/BLUP4')

Provino <- ('c:/Drive/Data/Merino/X/Pob/2022')

BLUP1 <- ('c:/Drive/Data/Merino/X/Pob/2022/BLUP1')
BLUP2 <- ('c:/Drive/Data/Merino/X/Pob/2022/BLUP2')
BLUP3 <- ('c:/Drive/Data/Merino/X/Pob/2022/BLUP3')
BLUP4 <- ('c:/Drive/Data/Merino/X/Pob/2022/BLUP4')

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
aacm$TE[aacm$TE == 'N'] <- '' #Esto solo para generar NOMBRE 
aacm$cnac <- aacm$cEstablecimientoNac
aacm$cact <- aacm$cEstablecimientoAct
aacm$SEXO <- aacm$bGenero
aacm$SEXO[aacm$SEXO == 'Macho'] <- 'M'
aacm$SEXO[aacm$SEXO == 'Hembra'] <- 'H'
aacm$fNacimiento <- strptime(aacm$fNacimiento, format='%d/%m/%Y')
aacm$AN <- year(aacm$fNacimiento)
aacm$MN <- month(aacm$fNacimiento)
aacm$DN <- day(aacm$fNacimiento)

aacm$NOMBRE<-paste(aacm$dAnimal,aacm$ident,aacm$cLinea,aacm$TE, sep=" ")  
aacm<-aacm[,c('RIM',"rimmd","rimpd","carav","ident","B","cnac","cact","SEXO","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]

#A patir de 2020 no contemplo mas los de Pilca y RM. Aquí iria el scripr "Pilca_RM.R"

#esto es lo que falta en el pedigree de la AACM (rim comienzan con 5);
falta <- read.csv('c:/Drive/Data/Merino/PedAX/falta.csv', sep=';')
falta <- falta[,c('RIM',"rimmd","rimpd","carav","ident","B","cnac","cact","SEXO","AN","MN","DN","NOMBRE","rp_padre","rp_madre","rep")]

#aqui esta toda la genealogía; 
todo.ped <- rbind(aacm,falta)
rm(aacm,aacm1,falta)

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
write.table(ped, file = "pedX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP2)
write.table(ped, file = "pedX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP3)
write.table(ped, file = "pedX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

setwd(BLUP4)
write.table(ped, file = "pedX.txt", sep=" ", row.names = F, col.names = F, quote=F, na="0")

rm(uno,dos,tres,todo.ped,ped)

