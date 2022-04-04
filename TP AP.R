rm(list=ls())
library(tidyverse)
library(ggplot2)
library(corrplot)
library(rpart.plot)
library(caret)
library(reshape2)
players=read.csv("players.csv")
colnames(players)
summary(players$SEASON)
teams=read.csv("teams.csv")
colnames(teams)
summary(teams$LEAGUE_ID)
games=read.csv("games.csv")
colnames(games)
str(games)
summary(games$SEASON)
games_det=read.csv("games_details.csv")
colnames(games_det)
raptor=read.csv("raptor.csv")
colnames(raptor)
head(raptor,4)
summary(raptor$season)

games_det=games_det %>% select(GAME_ID,TEAM_ID,PLAYER_ID,PLAYER_NAME,START_POSITION) %>% filter(START_POSITION!="")
pos2 = games_det %>% count(GAME_ID) %>% filter(n==10)#Saco partidos con posiciones de jugadores mal ordenadas
pos=merge(x=games_det,y=pos2,by="GAME_ID")
pos= pos %>% mutate(a=rep(c("F","F","C","G","G"),nrow(pos)/5))
summary(pos$START_POSITION==pos$a)
pos %>% filter(START_POSITION!=rep(c("F","F","C","G","G"),nrow(pos)/5)) %>% head(5) #Ver los datos de posiciones mal ordenadas
view(pos %>% filter(GAME_ID>=11900101))#hasta game 20900001
pos= pos %>% filter(GAME_ID<11900101 | GAME_ID>=20900001)
pos= pos %>% filter(GAME_ID<21900299 | GAME_ID>=40900101)
pos= pos %>% filter(GAME_ID<41900101)
pos=unique(pos %>% select(GAME_ID))
games_det=merge(x=games_det,y=pos,by="GAME_ID")
games_det=games_det %>% mutate(START_POSITION=rep(c("VF1","VF2","VC","VG1","VG2","LF1","LF2","LC","LG1","LG2"),nrow(games_det)/10))
teams=teams %>% select(TEAM_ID,NICKNAME,CITY)
games=games %>% filter(SEASON>=2014) %>% select(GAME_DATE_EST,GAME_ID,HOME_TEAM_ID,VISITOR_TEAM_ID,SEASON,PTS_home,PTS_away)#Los datos de raptor empiezan en la temporada 2014
players=players %>% filter(SEASON>=2014)
raptor= raptor %>% select(player_name,season,raptor_offense,raptor_defense)

games=merge(x=games,y=games_det,by="GAME_ID") #Partido- jugadores
summary(games)
raptor=raptor %>% mutate(cod=paste(player_name,season)) #Estadisticas - jugadores
games=games %>% mutate(cod=paste(PLAYER_NAME,SEASON))
head(games,4)
games=merge(x=games,y=raptor,by="cod")
games=merge(x=games,y=teams,by.x="HOME_TEAM_ID",by.y="TEAM_ID") #Nombres de equipos
games=games %>% mutate(HOME_TEAM_NAME=paste(CITY,NICKNAME))
games$NICKNAME=NULL
games$CITY=NULL
games=merge(x=games,y=teams,by.x="VISITOR_TEAM_ID",by.y="TEAM_ID")
games=games %>% mutate(VISITOR_TEAM_NAME=paste(CITY,NICKNAME))
games$NICKNAME=NULL
games$CITY=NULL
games=merge(x=games,y=teams,by="TEAM_ID")
games=games %>% mutate(TEAM_NAME=paste(CITY,NICKNAME))
games$NICKNAME=NULL
games$CITY=NULL
games=games %>% mutate(RESULT_HOME=PTS_home-PTS_away) #Diferencia de puntos local-visitante
colnames(games)
games10 = games %>% count(GAME_ID) %>% filter(n==10)#Luego de los inner joins, faltan datos de algunos de los jugadores en algunos partidos
games=merge(x=games,y=games10,by="GAME_ID")#Me quedo con los partidos sin faltante de datos
view(games)

#Cambio de formato de datos
games=games %>% select(SEASON,GAME_DATE_EST,HOME_TEAM_NAME,VISITOR_TEAM_NAME,RESULT_HOME,START_POSITION,raptor_offense,raptor_defense)
view(games)
colnames(games)
games=games %>% pivot_wider(names_from = START_POSITION, values_from = c(raptor_offense,raptor_defense))
games= games[,c("SEASON","GAME_DATE_EST","HOME_TEAM_NAME","VISITOR_TEAM_NAME","raptor_offense_LF1","raptor_defense_LF1","raptor_offense_LF2","raptor_defense_LF2","raptor_offense_LC","raptor_defense_LC","raptor_offense_LG1","raptor_defense_LG1","raptor_offense_LG2","raptor_defense_LG2","raptor_offense_VF1","raptor_defense_VF1","raptor_offense_VF2","raptor_defense_VF2","raptor_offense_VC","raptor_defense_VC","raptor_offense_VG1","raptor_defense_VG1","raptor_offense_VG2","raptor_defense_VG2","RESULT_HOME")] #Reordeno columnas
summary(games)

games %>% count(SEASON)


#Histogramas
write.csv(games$RESULT_HOME,"DifP.csv")
hist(games$RESULT_HOME, col = c(rep("red", 6), rep("blue", 7)), breaks = 10,main="Resultados de los partidos", xlab="Diferencia de puntos para el local", ylab="Frecuencia")
games=games %>% mutate(WINNER=ifelse(RESULT_HOME>0,"L","V"))
games$WINNER=as.factor(games$WINNER)
plot(games$WINNER,col=c("blue","red"),main="Victorias",xlab="Condición",ylab="Frecuencia")
write.csv(games$WINNER,"CondWin.csv")

#Correlación
corrs=select_if(games,is.numeric)
corrs$SEASON=NULL
corrs=corrs[,c("RESULT_HOME","raptor_offense_LF1","raptor_offense_LF2","raptor_offense_LC","raptor_offense_LG1","raptor_offense_LG2","raptor_offense_VF1","raptor_offense_VF2","raptor_offense_VC","raptor_offense_VG1","raptor_offense_VG2","raptor_defense_LF1","raptor_defense_LF2","raptor_defense_LC","raptor_defense_LG1","raptor_defense_LG2","raptor_defense_VF1","raptor_defense_VF2","raptor_defense_VC","raptor_defense_VG1","raptor_defense_VG2")]
M <- cor(corrs, method = "spearman")
corrplot(M, method = "ellipse",title="Correlación")
res_corr=M[,"RESULT_HOME"]
write.csv(res_corr,"corr.csv")

#Heatmap Perc Wins
h_games=games %>% group_by(SEASON,HOME_TEAM_NAME) %>% summarise(n_l_g=n()) %>% mutate(id=paste(SEASON,HOME_TEAM_NAME))
v_games=games %>% group_by(SEASON,VISITOR_TEAM_NAME) %>% summarise(n_v_g=n()) %>% mutate(id=paste(SEASON,VISITOR_TEAM_NAME))
t_games=merge(x=h_games,y=v_games,by="id")
t_games=t_games %>% mutate(total_games=n_l_g+n_v_g)
w_h_games=games %>% filter(RESULT_HOME>0) %>% group_by(SEASON,HOME_TEAM_NAME) %>% summarise(n_l_w=n()) %>% mutate(id=paste(SEASON,HOME_TEAM_NAME))
w_v_games=games %>% filter(RESULT_HOME<0)%>% group_by(SEASON,VISITOR_TEAM_NAME) %>% summarise(n_v_w=n()) %>% mutate(id=paste(SEASON,VISITOR_TEAM_NAME))
w_t_games=merge(x=w_h_games,y=w_v_games,by="id")
w_t_games=w_t_games %>% mutate(total_wins=n_l_w+n_v_w)
perc_wins=merge(t_games,w_t_games,by="id")
perc_wins=perc_wins %>% select(Season=SEASON.x.x,Team_Name = HOME_TEAM_NAME.x,Total_Games=total_games,Total_Wins=total_wins)
perc_wins=perc_wins %>% mutate(p_w=Total_Wins/Total_Games)
perc_wins=perc_wins %>% select(Season,Team_Name,p_w)
perc_wins[is.na(perc_wins)] = 0
heatmap(as.matrix(perc_wins), scale="column", col = cm.colors(8))
ggplot(perc_wins, aes(Season, Team_Name)) +
  geom_tile(aes(fill = p_w)) + 
  geom_text(aes(label = round(p_w, 1))) +
  scale_fill_gradient(low = "white", high = "red")
write.csv(perc_wins,"perc_wins.csv")

plot(games$RESULT_HOME,games$raptor_offense_LG2)

#Particion
head(games,5)
games=games %>% arrange(SEASON,GAME_DATE_EST)
games=games %>% mutate(id=rep(1:nrow(games)))
games$GAME_DATE_EST=NULL
games$WINNER=NULL
train=games %>% filter(id<0.75*nrow(games))
test=games %>% filter(id>=0.75*nrow(games))
train$id=NULL
test$id=NULL
write.csv(games,"nba.csv")

#Intento de arbol
arbol=rpart(RESULT_HOME~.,train,method="anova")
rpart.plot(arbol,extra=1,type=5)
pred=predict(arbol,test)#testeo
result=as.data.frame(rbind(test$RESULT_HOME,pred))
result=as.data.frame(t(result))
result=result %>% mutate(coinc=(V1>=pred-5 & V1<=pred+5))
result %>% count(coinc)

#Histogramas de raptor
matrix=games %>% select(raptor_offense_LF1,raptor_defense_LF1,raptor_offense_LF2,raptor_defense_LF2,raptor_offense_LC,raptor_defense_LC,raptor_offense_LG1,raptor_defense_LG1,raptor_offense_LG2,raptor_defense_LG2)
attitudeM <- melt(matrix)
g <- ggplot(attitudeM,aes(x=value))
g <- g + geom_histogram()
g <- g + facet_wrap(~variable)
g
write.csv(matrix,"matrix.csv")