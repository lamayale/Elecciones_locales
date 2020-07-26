rm(list=ls())
election_type="DL" 
year_state=2018  
file_name="2018_SEE_DIP_LOC_MR_GRO_CAS.csv"
dist_local=4
var_graph="more21_coal" 
var_title="Not Voting for Morena (AMLO's party)" #Title for Plot

source('/Users/armandoal/Desktop/Elections/Senadurias2018/GUERRERO/Guerrero_Elección Estatal.R')
list_master=list(master)

pass2=c("coalicion","morena2021_c","PRI")
sapply(pass2, function(x) sum(master[master$SECCION %in% list_sections[[dist_local]],x][[1]])/sum(master$TOTAL_VOTOS[master$SECCION %in% list_sections[[dist_local]]]))



#########################################################
rm(list=(ls()[ls()!=c("list_master","list_sections")]))
election_type="DL" 
dist_local=4
year_state=2015  
file_name="DIPUTADOS2015.csv"
var_graph="more21_coal" 
var_title="Not Voting for Morena (AMLO's party)" #Title for Plot

source('/Users/armandoal/Desktop/Elections/Senadurias2018/GUERRERO/Guerrero_Elección Estatal.R')

list_master[[2]]=master

pass2=c("coalicion","morena2021_c","PRI","PAN","PRD","MC","PT","PVEM","MORENA")
sapply(pass2, function(x) sum(master[master$SECCION %in% list_sections[[dist_local]],x][[1]])/sum(master$TOTAL_VOTOS[master$SECCION %in% list_sections[[dist_local]]]))

########################################################
rm(list=(ls()[ls()!=c("list_master","list_sections")]))
election_type="AYUNT" 
dist_local=4
year_state=2018  
file_name="2018_SEE_AYUN_GRO_CAS.csv"
var_graph="more21_coal" 
var_title="Not Voting for Morena (AMLO's party)" #Title for Plot

source('/Users/armandoal/Desktop/Elections/Senadurias2018/GUERRERO/Guerrero_Elección Estatal.R')

list_master[[3]]=master



pass2=c("coalicion","morena2021_c","PRI","PAN","PRD","MC","PT","PVEM","MORENA")
sapply(pass2, function(x) sum(master[master$SECCION %in% list_sections[[dist_local]],x][[1]])/sum(master$TOTAL_VOTOS[master$SECCION %in% list_sections[[dist_local]]]))

########################################################
rm(list=(ls()[ls()!=c("list_master","list_sections")]))
election_type="AYUNT" 
dist_local=4
year_state=2015  
file_name="AYUNTAMIENTO2015.csv"
var_graph="more21_coal" 
var_title="Not Voting for Morena (AMLO's party)" #Title for Plot

source('/Users/armandoal/Desktop/Elections/Senadurias2018/GUERRERO/Guerrero_Elección Estatal.R')

list_master[[4]]=master


pass2=c("coalicion","morena2021_c","PRI","PAN","PRD","MC","PT","PVEM","MORENA")
sapply(pass2, function(x) sum(master[master$SECCION %in% list_sections[[dist_local]],x][[1]])/sum(master$TOTAL_VOTOS[master$SECCION %in% list_sections[[dist_local]]]))

########################################################
rm(list=(ls()[ls()!=c("list_master","list_sections")]))

election_type="GOBERNADOR" 
dist_local=4
year_state=2015  
file_name="GOBERNADOR2015.csv"
var_graph="more21_coal" 
var_title="Not Voting for Morena (AMLO's party)" #Title for Plot

source('/Users/armandoal/Desktop/Elections/Senadurias2018/GUERRERO/Guerrero_Elección Estatal.R')

list_master[[5]]=master
names(list_master)=c("DL18","DL15","Ayunt18","Ayunt15","Gob15")
rm(list=(ls()[ls()!=c("list_master","list_sections")]))
##############################################################################################################################################################################
#Information about coalitions for DL
##############################################################################################################################################################################
dist_local=4





#Check order of seccions is fine to make the comparison 
sec=sapply(1:5, function(x) list_master[[x]]$SECCION)
sapply( 2:5, function(y) sum(sapply(1:length(sec[,1]), function(x) list_master[[1]]$SECCION[x]-sec[x,y] )))

#Create the variables for comparison across years

#To do, there-s a function of graph that depends on master, and it should depend instead of list_master

party=c("PAN","PRI","PRD","MC","MORENA","PT","PVEM")
add_name=function(num_list,suffix,dataset){
#  pp=as.data.frame(sapply(party, function(x) list_master[[num_list]][ list_master[[num_list]]$SECCION %in% list_sections[[dist_local]]   ,x][[1]]))
   pp=as.data.frame(sapply(party, function(x) list_master[[num_list]][ ,x][[1]]))

  colnames(pp)=paste(suffix, colnames(pp),sep="_")
  assign(dataset,pp, envir = .GlobalEnv)
}


add_name(1,suffix="DL18",dataset="DL18")
add_name(2,suffix="DL15",dataset="DL15")
add_name(3,suffix="Ay18",dataset="Ay18")
add_name(4,suffix="Ay15",dataset="Ay15")
add_name(5,suffix="Gob15",dataset="Gob18")

list_master$DL18$PRD_lost=DL15$DL15_PRD-DL18$DL18_PRD

list_master$DL18$dif_votos1815=list_master$DL18$TOTAL_VOTOS-list_master$DL15$TOTAL_VOTOS
list_master$DL18$dif_votos1815[list_master$DL18$dif_votos1815<0]=NA

list_master$DL18$dif_votos1815_pct=list_master$DL18$dif_votos1815/list_master$DL18$TOTAL_VOTOS*100
weight=(1-list_master$DL18$dif_votos1815_pct)

lik=c("PAN","PRI","PRD","MORENA","MC","PT","PVEM")
sapply(lik, function(x) 
  sum(list_master$DL18[list_master$DL18$SECCION %in% list_sections[[4]],x][[1]]*weight[list_master$DL18$SECCION %in% list_sections[[4]]]))

elecc_num=1

#PRD perdio en todas.
V_PRD_perd=-list_master$DL18$PRD[(list_master$DL18$SECCION %in% list_sections[[4]]) ]+list_master$DL15$PRD[(list_master$DL15$SECCION %in% list_sections[[4]]) ]
#PRI perdio en todas menosen 2
-list_master$DL18$PRI[(list_master$DL18$SECCION %in% list_sections[[4]]) ]+list_master$DL15$PRI[(list_master$DL15$SECCION %in% list_sections[[4]]) ]
#PAN pierde en todas menos 3 secciones y en una empata
-list_master$DL18$PAN[(list_master$DL18$SECCION %in% list_sections[[4]]) ]+list_master$DL15$PAN[(list_master$DL15$SECCION %in% list_sections[[4]]) ]
#En la mayor'ia pierde, pero en algunas gana
-list_master$DL18$MC[(list_master$DL18$SECCION %in% list_sections[[4]]) ]+list_master$DL15$MC[(list_master$DL15$SECCION %in% list_sections[[4]]) ]

filter=(list_master$DL18$SECCION %in% list_sections[[4]])

V_Morena2018_NOnuevos=list_master$DL15$MORENA+
  +list_master$DL15$PAN-list_master$DL18$PAN+
  +list_master$DL15$PRD-list_master$DL18$PRD+
  +list_master$DL15$PRI-list_master$DL18$PRI+
  +list_master$DL15$MC-list_master$DL18$MC+
  +list_master$DL15$PVEM-list_master$DL18$PVEM+
  +list_master$DL15$PNA-list_master$DL18$NA.
  +list_master$DL15$PRI_PVEM-list_master$DL18$PRI_PVEM

ganancia_prd=list_master$DL15$PRD-list_master$DL18$PRD
#-list_master$DL18$PAN_PRD_MC
#-list_master$DL18$PAN_PRD-list_master$DL18$PRD_MC-list_master$DL18$PAN_MC
ganancia_otros=list_master$DL15$PAN-list_master$DL18$PAN+
  +list_master$DL15$PRI-list_master$DL18$PRI+
  +list_master$DL15$MC-list_master$DL18$MC+
  +list_master$DL15$PVEM-list_master$DL18$PVEM+
  +list_master$DL15$PNA-list_master$DL18$NA.+
  +list_master$DL15$PRI_PVEM-list_master$DL18$PRI_PVEM


votos_nuevos_todos=list_master$DL18$NUM_VOTOS_VALIDOS-list_master$DL15$TOTAL_VOTOS+list_master$DL15$VOTOS_NULOS

V_Morena2018_nuevos=list_master$DL18$MORENA-ganancia_prd-ganancia_otros-list_master$DL15$MORENA
mor=data.frame(list_sections[[4]],list_master$DL15$MORENA[filter], ganancia_prd[filter], ganancia_otros[filter],
               V_Morena2018_nuevos[filter],
               list_master$DL18$MORENA[filter],votos_nuevos_todos[filter]) 
               #(votos_nuevos_todos[filter]>=V_Morena2018_nuevos[filter]))


colnames(mor)=c("Seccion","Morena 2015","Ganancia PRD","Ganancia Otros","Nuevos Morena","Morena 2018","Votos Nuevos Todos")
               

list_master$DL18$V_Morena2018_nuevos=V_Morena2018_nuevos
list_master$DL18$V_Morena2018_nuevos[list_master$DL18$V_Morena2018_nuevos<0]=NA

list_master$DL18$V_Morena2018_nuevos_pct=list_master$DL18$V_Morena2018_nuevos/list_master$DL18$MORENA*100


#(var_graph,party_color,varname_title,data_name, title_graph)

source('/Users/armandoal/Desktop/Elections/GUERRERO/graph_map.R')
graph_map("dif_votos1815","LISTA_NOMINAL","Votos adicionales",list_master[[elecc_num]],"Distrito 4. Votos Nuevos en 2018")
graph_map("dif_votos1815_pct","LISTA_NOMINAL","Votos Nuevos 2018-2015",list_master[[elecc_num]],"")

graph_map("dif_votos1815","LISTA_NOMINAL","Votos adicionales",list_master[[elecc_num]],"Distrito 4. Votos Nuevos en 2018")
graph_map("V_Morena2018_nuevos","MORENA","Votos Nuevos de Morena",list_master[[elecc_num]]," ")
graph_map("V_Morena2018_nuevos_pct","MORENA","Porcentaje de Votos Nuevos",list_master[[elecc_num]]," ")


graph_map("PRD_lost","PRD","Votos Perdidos",list_master[[elecc_num]],"Distrito 4. Votos Perdidos por el PRD en 2018")

graph_map("more21_pct","MORENA","Coalición Morena 2021 %Votos",list_master[[elecc_num]], "")
graph_map("PRD_pct","PRD","PRD %Votos",list_master[[elecc_num]], "")
graph_map("PAN_pct","PAN","PAN %Votos",list_master[[elecc_num]], "")
graph_map("PRI_pct","PRI","PRI %Votos",list_master[[elecc_num]], "")
graph_map("LISTA_NOMINAL","LISTA_NOMINAL","Lista Nominal",list_master[[elecc_num]], "")
graph_map("participacion","PARTICIPACION","Participación Electoral",list_master[[elecc_num]], "")





#########################################Analysis Voto Duro
res_comp=data.frame(c(sum(PRI15),sum(PRI18)),c(sum(PAN15),sum(PAN18)),c(sum(PRD15),sum(PRD18)),c(sum(MC15),sum(MC18)), 
           c(sum(MOR15),sum(MOR18)), c(sum(PT15),sum(PT18)), c(sum(PVEM15),sum(PVEM18)))
res_comp[3,]=res_comp[1,]-res_comp[2,]
colnames(res_comp)=c("PRI","PAN","PRD","MC","MORENA","PT","PVEM")

data15=read.csv("/Users/armandoal/Desktop/Elections/GUERRERO/Resultados Electorales 2015/Res_DL_2015.csv", sep=",", header=T)
data18=read.csv("/Users/armandoal/Desktop/Elections/GUERRERO/Resultados Electorales 2018/Res_DL_2018.csv", sep=",", header=T)

extract=c("PAN","PRI","PRD","MC","PT","PVEM","MORENA","PNA")
d18=as.numeric(data18[data18$ID_DISTRITO_LOCAL==4,extract])
d15=as.numeric(data15[data15$ID_DISTRITO_LOCAL==4,extract])
d15[-c(5,7)]-d18[-c(5,7)]
sum(d15[-c(5,7)]-d18[-c(5,7)])

party=c("PRI","PAN","PRD","MC","MORENA","PT","PVEM","TOTAL_VOTOS")
#data15[, party] <- sapply(data15[,party], as.numeric)

p15=sapply(party, function(x) sum(data15[,x]))

p18=sapply(party, function(x) sum(data18[,x]))
Diff=p15-p18
res_comp=data.frame(p15,p15/p15["TOTAL_VOTOS"],p18,p18/p18["TOTAL_VOTOS"],diff)
colnames(res_comp)=c("V_15","Pct_15","V_18","Pct_18","Diferencia")
res_comp
v_duro=res_comp["PRD","p15"]-res_comp["PRD","p18"]+res_comp["MORENA","p15"]
v_volatil=res_comp["MORENA","p18"]-v_duro
v_duro
v_volatil

##################################Ayuntamientos
data15=read.csv("/Users/armandoal/Desktop/Elections/GUERRERO/Resultados Electorales 2015/Res_Ay_2015.csv", sep=",", header=T)
data18=read.csv("/Users/armandoal/Desktop/Elections/GUERRERO/Resultados Electorales 2018/Res_Ay_2018.csv", sep=",", header=T)

party=c("PRI","PAN","PRD","MC","MORENA","PT","PVEM","TOTAL_VOTOS")
#data15[, party] <- sapply(data15[,party], as.numeric)

p15=sapply(party, function(x) sum(data15[,x]))

p18=sapply(party, function(x) sum(data18[,x]))
Diff=p15-p18
res_comp=data.frame(p15,p15/p15["TOTAL_VOTOS"],p18,p18/p18["TOTAL_VOTOS"],diff)
colnames(res_comp)=c("V_15","Pct_15","V_18","Pct_18","Diferencia")
res_comp
v_duro=res_comp["PRD","p15"]-res_comp["PRD","p18"]+res_comp["MORENA","p15"]
v_volatil=res_comp["MORENA","p18"]-v_duro
v_duro
v_volatil


MOR_DURO=PRD15-PRD18+list_master$DL15$MORENA
MOR_VOLATIL=list_master$DL18$MORENA-MOR_DURO


############Gober mapa


li=as.data.frame(master[,c("PRI","PRD","MC","PAN")])
li=li[,-length(li)]
rank_1=rowMaxs(as.matrix(li[,c("PRI","PRD","MC","PAN")]))
rank1_party=c(rep(0,length(master[,1])))
rank1_party[rank_1==li[,c("PRI")]]="PRI"
rank1_party[rank_1==li[,c("PRD")]]="PRD"
rank1_party[rank_1==li[,c("MC" )]]="MC"
rank1_party[rank_1==li[,c("PAN")]]="PAN"
master$rankia[master$TOTAL_VOTOS==0]="PRI"

master$rankia=rank1_party
var_graph="rankia"
ggplot() +
  geom_sf(data = master, aes(fill=factor(rankia)), show.legend = T, color="black") +
  scale_fill_manual(values = c("orange", "blue", "gold","palegreen4"), name= "Partido")+ 
  ggtitle("Elección Gobernador 2015 por Sección Electoral")+
  theme_void() 


############################################
dat=read.csv('/Users/armandoal/Desktop/Elections/GUERRERO/computos2012/datos_computos_casillas_diputados.txt', sep='|')

dgro=dat[dat$ID_ESTADO==12,]
dgro4=dgro[dgro$SECCION %in% list_sections[[dist_local]],]
sum(dgro4$LISTA_NOMINAL)


dat=read.csv('/Users/armandoal/Desktop/Elections/GUERRERO/diputaciones18.csv', sep=',')
dgro=dat[dat$ID_ESTADO==12,]
dgro4=dgro[dgro$SECCION %in% list_sections[[dist_local]],]
sum(dgro4$LISTA_NOMINAL_CASILLA)


###################
lin=c("PAN","PRI","PRD","PT","PVEM","MC","PNA","MORENA","PES","PRI_PVEM","PRD_PT","VOTOS_NULOS","TOTAL_VOTOS")
pasti=as.data.frame(sapply(lin, function(x) sum(data[data$ID_DISTRITO_LOCAL==4,x])))



