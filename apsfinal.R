
library(RColorBrewer)
cores <- brewer.pal(12, "Paired")

aps1<- read_excel("/Users/fred/Library/Mobile Documents/com~apple~CloudDocs/Aps Econometria/aps data certo agora.xlsx")
aps=na.omit(aps1)


#!Univariada
par(mfrow=c(1,1))

tit=c("Média","Mediana","Desvio Padrão","Mínimo","Primeiro Quartil","Terceiro Quartil","Máximo")

annota= c(mean(aps$ratingint), median(aps$ratingint), sd(aps$ratingint), min(aps$ratingint), quantile(aps$ratingint,0.25), quantile(aps$ratingint,0.75), max(aps$ratingint))
tabnota=as.data.frame(rbind(annota))
rownames(tabnota)="Nota Média"
colnames(tabnota)=tit
View(tabnota)

boxplot(aps$ratingint,col=cores[1],main="Nota")

antempo= c(mean(aps$runtimeMinutes),median(aps$runtimeMinutes),sd(aps$runtimeMinutes),min(aps$runtimeMinutes),quantile(aps$runtimeMinutes,0.25),quantile(aps$runtimeMinutes,0.75),max(aps$runtimeMinutes))
tabtempo=as.data.frame(rbind(antempo))
rownames(tabtempo)="Tempo de Duração"
colnames(tabtempo)=tit
View(tabtempo)

boxplot(aps$runtimeMinutes,col=cores[3],main="Tempo de Duração em Minutos")

annum=c(mean(aps$numVotes_mil),median(aps$numVotes_mil),sd(aps$numVotes_mil),min(aps$numVotes_mil),quantile(aps$numVotes_mil,0.25),quantile(aps$numVotes_mil,0.75),max(aps$numVotes_mil))
tabnum=as.data.frame(rbind(annum))
rownames(tabnum)="Número de Votos em milhares"
colnames(tabnum)=tit
View(tabnum)

boxplot(aps$numVotes_mil,col=cores[5],main="Numero de Votos em Milhares",ylim=c(0,2500))

anbugt=c(mean(aps$budget_m),median(aps$budget_m),sd(aps$budget_m),min(aps$budget_m),quantile(aps$budget_m,0.25),quantile(aps$budget_m,0.75),max(aps$budget_m))
tabugt=as.data.frame(rbind(anbugt))
rownames(tabugt)="Orçamento em Milhões"
colnames(tabugt)=tit
View(tabugt)

boxplot(aps$budget_m,col=cores[6], main="Orçamento em Milhões")

angross=c(mean(aps$gross_m),median(aps$gross_m),sd(aps$gross_m),min(aps$gross_m),quantile(aps$gross_m,0.25),quantile(aps$gross_m,0.75),max(aps$gross_m))
tabgross=as.data.frame(rbind(angross))
rownames(tabgross)="Receita Bruta em Milhões"
colnames(tabgross)=tit
View(tabgross)

boxplot(aps$gross_m,col=cores[8],main="Receita Bruta em Milhões")

anidade=c(mean(aps$Idade),median(aps$Idade),sd(aps$Idade),min(aps$Idade),quantile(aps$Idade,0.25),quantile(aps$Idade,0.75),max(aps$Idade))
tabidade=as.data.frame(rbind(anidade))
rownames(tabidade)="Idade do filme em anos"
colnames(tabidade)=tit
View(tabidade)

boxplot(aps$Idade,col=cores[10],main="Idade do filme em Anos")

matrizEstatisticas = cbind(annota, antempo, annum, anbugt, angross, anidade)
dfEstatisticas = as.data.frame(t(matrizEstatisticas))
colnames(dfEstatisticas) = tit
rownames(dfEstatisticas) = c("Nota Média", "Tempo de Duração", "Número de Votos em milhares",
                             "Orçamento em Milhões", "Receita Bruta em Milhões", "Idade do filme em anos")
View(dfEstatisticas)


andrama= c(
  mean(aps$ratingint[aps$DramaDummy==1]),
  median(aps$ratingint[aps$DramaDummy==1]),
  sd(aps$ratingint[aps$DramaDummy==1]),
  min(aps$ratingint[aps$DramaDummy==1]),
  quantile(aps$ratingint[aps$DramaDummy==1], 0.25),
  quantile(aps$ratingint[aps$DramaDummy==1], 0.75),
  max(aps$ratingint[aps$DramaDummy==1])
)

anresto= c(
  mean(aps$ratingint[aps$DramaDummy==0]),
  median(aps$ratingint[aps$DramaDummy==0]),
  sd(aps$ratingint[aps$DramaDummy==0]),
  min(aps$ratingint[aps$DramaDummy==0]),
  quantile(aps$ratingint[aps$DramaDummy==0], 0.25),
  quantile(aps$ratingint[aps$DramaDummy==0], 0.75),
  max(aps$ratingint[aps$DramaDummy==0])
)

tabdummy=as.data.frame(t(rbind(andrama,anresto)))
rownames(tabdummy) = tit
colnames(tabdummy) = c("Filmes de Drama", "Filmes sem ser Drama")
View(tabdummy)

boxplot(aps$ratingint~aps$DramaDummy,col=c(cores[11],cores[12]),main="Nota de filmes de Drama vs filmes sem ser Drama")

#!Bivariada
xlimrt=max(aps$runtimeMinutes)*1.05


plot(aps$budget_m, aps$ratingint, main="Orçamento em Milhões X Nota", xlab="Orçamento em Milhões", ylab="Nota", pch = 19, cex = 1,col=c(cores[1],cores[2]),colorvar=aps$DramaDummy)
modelo0 <- lm(ratingint ~ budget_m, data = aps, subset = (DramaDummy == 0))
abline(modelo0, col = cores[1], lwd = 2)
modelo1 <- lm(ratingint ~ budget_m, data = aps, subset = (DramaDummy == 1))
abline(modelo1, col = cores[2], lwd = 2)
legend("bottomright", legend=c("Drama", "Não Drama"), col=c(cores[1], cores[2]), pch=19)

var.test(aps$ratingint~aps$DramaDummy)
t.test(aps$ratingint~aps$DramaDummy,paired=FALSE,var.equal=FALSE)

library(psych)

cor_matrix <- cor(aps[, c("ratingint", "runtimeMinutes", "budget_m", "numVotes_mil", "Idade", "gross_m")], use="pairwise.complete.obs")

View(cor_matrix)


