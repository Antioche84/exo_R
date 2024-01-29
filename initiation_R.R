summary(resultats_pres2022$Part_Macron)

summary(resultats_pres2022$Part_abs)

moyenneMacron<- mean(resultats_pres2022$Part_Macron)

moyenneMacron

paste("La moyenne des % de vote Macron est de ", moyenneMacron)

paste("La moyenne des % de vote Macron est de ", round(moyenneMacron, digits = 2), "%")

abstentionNationale <- mean(resultats_pres2022$Part_abs)

paste("La moyenne des parts de vote abstention est de ", round(abstentionNationale, digits = 2), "%")

Requete1 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 10 , ]
View (Requete1)

count (Requete1)

Requete2 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 30 & resultats_pres2022$Part_LePen < 60 , ]
View (Requete2)

voteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 , ]
View (voteJadot2040)

voteRochefort <- resultats_pres2022 [resultats_pres2022$Libelle =='Rochefort' & resultats_pres2022$Code_dep == '17' ,]
View (voteRochefort)
write.csv(voteSaintes, file = "E:/Pro/LPSIG/5_Hubert/4_R ET GITHUB/Exercices/saintes.csv")

voteCandidats1_4 <- resultats_pres2022 [ , c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
View (voteCandidats1_4)

> typeof(voteCandidats1_4)
#[1] "list"

sapply(voteCandidats1_4, class)

voteSaintes_Candidats1_3 <- voteSaintes [ , c("Part_Macron", "Part_LePen", "Part_Melenchon")]
View (voteSaintes_Candidats1_3)

sapply(voteSaintes_Candidats1_3, sd)

voteCandidats1_4$colonneTest <- NA

voteCandidats1_4$colonneTest <- voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse
View(voteCandidats1_4)

voteCandidats1_4$colonneTest <- rowSums(voteCandidats1_4[,c('Part_Macron', 'Part_Pecresse')], na.rm=TRUE)

voteCandidats1_4$colonneTest <- ifelse(voteCandidats1_4$Part_Macron > 25, voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse, NA)

Transpo <- voteCandidats1_4 %>% pivot_longer( cols = c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse),names_to="Candidat", values_to="Parts_vote")
View(Transpo)

voteSaintes_Candidats1_3$colonne <- ifelse(voteSaintes_Candidats1_3$Part_Macron > 30, 'MacronSup30', NA)

test <- na.omit(voteSaintes_Candidats1_3)
sum(test$colonne == 'MacronSup30')

sapply(voteCandidats1_4, mean)

tapply(resultats_pres2022$Part_abs,resultats_pres2022$Libelle_dep, mean )

dataList <- c("Part_Macron", "Part_LePen", "Part_Melenchon")

for(i in dataList){
print(i)
}

for(i in dataList){
print(summary(resultats_pres2022[, i]))
}

Resultat_84 <- resultats_pres2022[resultats_pres2022$Code_dep == 84,]
tapply(Resultat_84$Part_Arthaud,Resultat_84$Libelle, max )

col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")

col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse))

data <- data.frame(group=col1, value=col2)

ggplot(data, aes(x="", y=value , fill=group)) +
+     geom_bar(stat="identity", width=1) +
+     geom_col() +
+     coord_polar("y", start=0) +
+ geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
+     scale_fill_manual(values = c("#0a3895", "#b831f3",
+                                  "#f33157", "#6091f6"))

> ggplot(data= data, aes(x=reorder(group, -value), y=value, fill=group)) +
+     geom_bar(stat="identity")+
+ geom_text(aes(label=round(value, digits=2)), vjust=1.6, color="white", size=3.5)+
+     scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))


Resultat_LR <- resultats_pres2022[resultats_pres2022$Libelle == 'La Rochelle'& resultats_pres2022$Code_dep == 17,c("Part_Hidalgo", "Part_Jadot", "Part_Melenchon")]

col1 <- c("Hidalgo", "Jadot", "Melenchon")
col2 <- c(mean(Resultat_LR$Part_Hidalgo), mean(Resultat_LR$Part_Jadot), mean(Resultat_LR$Part_Melenchon))

data_LR <- data.frame(group=col1, value=col2)

ggplot(data_LR, aes(x="", y=value , fill=group)) +
    geom_bar(stat="identity", width=1) +
    geom_col() +
    coord_polar("y", start=0) +
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#f33157", "#64FF33", "#b831f3"))


Resultat_LR_D <- resultats_pres2022[resultats_pres2022$Libelle == 'La Rochelle'& resultats_pres2022$Code_dep == 17,c("Part_DupontAignan", "Part_LePen", "Part_Pecresse", "Part_Zemmour")]






# col1D <- c("Dupont-Aignan", "LePen", "Pecresse", "Zemmour")
# col2D <- c(mean(Resultat_LR_D$Part_DupontAignan), mean(Resultat_LR_D$Part_LePen), mean(Resultat_LR_D$Part_Pecresse), mean(Resultat_LR_D$Part_Zemmour))
# col3D <- c(min(Resultat_LR_D$Part_DupontAignan), min(Resultat_LR_D$Part_LePen), min(Resultat_LR_D$Part_Pecresse), min(Resultat_LR_D$Part_Zemmour))
# col4D <- c(max(Resultat_LR_D$Part_DupontAignan), max(Resultat_LR_D$Part_LePen), max(Resultat_LR_D$Part_Pecresse), max(Resultat_LR_D$Part_Zemmour))


data <- Resultat_LR_D %>% pivot_longer(
cols = c(Part_DupontAignan, Part_LePen, Part_Pecresse, Part_Zemmour),
names_to="Candidat", values_to="Parts_vote")

# data_LR_D <- data.frame(group=col1D, mean=col2D, min = col3D, max = col4D)

 ggplot(data, aes(x=Candidat, y=Parts_vote)) + 
     geom_boxplot(fill="slateblue", alpha=0.2) + 
     xlab("Candidat")

#Si on veut une coleur par candiddat
     ggplot(data, aes(x=Candidat, y=Parts_vote, fill = Candidat)) + 
+     geom_boxplot(alpha=0.2) + 
+     xlab("Candidat")



voteRochefort_Candidats1_4 <- voteRochefort [ , c("Code_BV", "Part_Macron" , "Part_LePen" , "Part_Melenchon",
"Part_Pecresse")]

reformat <-
voteRochefort_Candidats1_4 %>% pivot_longer(cols=c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"),
names_to='candidats', values_to='parts')


ggplot(data=reformat, aes(x=candidats,y=parts,fill=candidats)) +
geom_bar(stat="identity") +
facet_wrap(~Code_BV) +
ggtitle("Parts de vote dans les BV de Rochefort") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=90)) +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))

voteRoyan <- resultats_pres2022 [resultats_pres2022$Libelle == 'Royan'& resultats_pres2022$Code_dep == 17 , c("Code_BV", "Part_Macron" , "Part_LePen" , "Part_Melenchon",
"Part_Pecresse")]

reformat_Royan <- voteRoyan %>% pivot_longer(cols=c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"), names_to='candidats', values_to='parts')

ggplot(data=reformat_Royan, aes(x=candidats,y=parts,fill=candidats)) +
    geom_bar(stat="identity") +
    facet_wrap(~Code_BV) +coord_polar("y", start=180) +
    ggtitle("Parts de vote dans les BV de Royan") +
    theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
    theme(axis.text.x = element_text(angle=90)) +
    scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))


db <- "postgres"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "****"
conn <- dbConnect(RPostgres::Postgres(),dbname = db,host = db_host,port = db_port,user = db_user,password = db_pass)
conn # doit renvoyer <PqConnection> postgres@localhost:5432
# Requête de sélection
requete <- dbGetQuery(conn, 'SELECT * from php.form;')