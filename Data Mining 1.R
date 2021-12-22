data1 <- read.csv("E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/DataMiningP2.txt", header=TRUE, sep="\t")
data1

#Statistika Deskriptif
summary(data1)

#Tabel Kontingensi
TabulasiSilang <- xtabs(~Kgdp+Klife, data=data1)
TabulasiSilang

#Diagram Lingkaran
Kgdp <- summary(data1$Kgdp)
Kgdp
Label1 <- paste(names(Kgdp),"\n",Kgdp,sep=" ")
pie(Kgdp, labels=Label1, main="Diagram Lingkaran GDP \n (dengan banyak pengamatan)")


#Diagram Lingkaran Dalam %
Persen <- round(Kgdp/sum(Kgdp)*100)
Persen
Label2 <- paste(names(Kgdp),"\n", Persen, "%", sep=" ")
Label2
pie(Persen, labels=Label2, main="Diagram Lingkaran GDP (dengan presentase pengamatan)", col=rainbow(length(Persen)))

#Diagram Batang Simple Vertikal
Frekuensi <- table(data1$Kgdp)
barplot(Frekuensi, main="Banyak GDP", xlab="GDP")

#Diagram Batang Simple Horizontal
barplot(Frekuensi, main="Banyak GDP", xlab="GDP", horiz=TRUE)

#Diagram Batang Stacked
Frekuensi1 <- table(data1$Klife, data1$Kgdp)
Frekuensi1
Warna <- c("darkblue", "red")
barplot(Frekuensi1, main="GDP vs Life", xlab="GDP", col=Warna)
legend("topleft", legend=rownames(Frekuensi1), fill=Warna, title="Life Expected")

#Diagram Batang 100% Stacked
Persen1 <- Frekuensi1
Persen1[,1] <- round(Frekuensi1[,1]/sum(Frekuensi1[,1])*100)
Persen1[,2] <- round(Frekuensi1[,2]/sum(Frekuensi1[,2])*100)
Persen1
barplot(Persen1, main="GDP vs Life", xlab="GDP", col=Warna)
legend("topleft", y=110, legend=rownames(Frekuensi1), fill=Warna, title="Life Expected", bg="White", inset=0.07)

#Diagram batang Group
barplot(Frekuensi1, main="Banyak Pengamatan GDP dan Life", xlab="GDP", col=Warna, beside=TRUE)
legend("topleft", legend=rownames(Frekuensi1), fill=Warna, title="Life Expected", inset=0.07)

#Diagram Garis
BanyakKategori <- length(summary(data1$iso3c))
DataUrut <- data1[order(data1$year),]
X <- DataUrut$year
Y <- DataUrut$gdp_percap
plot(X,Y,type="p", xlab="year", ylab="GDP")
Warna1 <- rainbow(BanyakKategori)
TipeGaris <- c(1:BanyakKategori)

#Menambahkan Garis
for(i in 1:BanyakKategori)
{
  Partisi <- subset(DataUrut, iso3c==names(summary(data1$iso3c))[i])
  lines(Partisi$year, Partisi$gdp_percap, type="l", lwd=1.5, col=Warna1[i])
}

#Memberi Judul
title("Perkembangan GDP Berdasarkan Negara")

#Menambahkan Legend
legend("topleft", legend=names(summary(data1$iso3c)), cex=0.8, col=Warna1, lty=TipeGaris, title="Negara")

#Diagram Pencar
plot(data1$gdp_percap, data1$life_expect, main="GDP terhadap Life Expected", xlab="GDP", ylab="Life Expected")

#Histogram
hist(data1$gdp_percap, main="Histogram GDP", xlab="GDP")

#Boxplot
boxplot(data1$gdp_percap~data1$Klife, data=data1, main="Boxplot GDP Untuk Setiap kategori Life", xlab="Kategori Life", ylab="GDP")