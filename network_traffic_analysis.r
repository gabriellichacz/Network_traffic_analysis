#############################_libraries_##########################

if (!("here" %in% rownames(installed.packages()))) install.packages("here")
if (!("liftLRD" %in% rownames(installed.packages()))) install.packages("liftLRD")
if (!("stringi" %in% rownames(installed.packages()))) install.packages("stringi")
if (!("dplyr" %in% rownames(installed.packages()))) install.packages("dplyr")
if (!("lattice" %in% rownames(installed.packages()))) install.packages("lattice")
if (!("ggplot2" %in% rownames(installed.packages()))) install.packages("ggplot2")
if (!("readr" %in% rownames(installed.packages()))) install.packages("readr")
if (!("moments" %in% rownames(installed.packages()))) install.packages("moments")
if (!("xfun" %in% rownames(installed.packages()))) install.packages("xfun")
if (!("reldist" %in% rownames(installed.packages()))) install.packages("reldist")
if (!("ggbeeswarm" %in% rownames(installed.packages()))) install.packages("ggbeeswarm")
if (!("zip" %in% rownames(installed.packages()))) install.packages("zip")
if (!("ggpubr" %in% rownames(installed.packages()))) install.packages("ggpubr")
if (!("cvequality" %in% rownames(installed.packages()))) install.packages("cvequality")
if (!("MBESS" %in% rownames(installed.packages()))) install.packages("MBESS")
if (!("tidypredict" %in% rownames(installed.packages()))) install.packages("tidypredict")
if (!("jtools" %in% rownames(installed.packages()))) install.packages("jtools")
if (!("corrr" %in% rownames(installed.packages()))) install.packages("corrr")
if (!("lattice" %in% rownames(installed.packages()))) install.packages("lattice")
if (!("viridis" %in% rownames(installed.packages()))) install.packages("viridis")

library(here)
library(liftLRD)
library(stringi)
library(dplyr)
library(lattice)
library(ggplot2)
library(readr)
library(moments)
library(xfun)
library(reldist)
library(ggbeeswarm)
library(zip)
library(ggpubr)
library(cvequality)
library(MBESS)
library(tidypredict)
library(jtools)
library(corrr)
library(lattice)
library(viridis)

setwd(here()) # Directory
getwd()
Sys.setenv(LANG = "en")

#############################_functions_#############################

# Clearing and loading data #
chrum <- function(func_data){
  func_data_file <- paste(func_data,'csv', sep = '.')
  
  func_data <- read.csv(func_data_file, row.names = NULL, header = TRUE, sep = ",", 
                        encoding = "UTF-8", col.names = c("1","Time","Source","Destination","Protocol","Length","Info"))[-1]
  func_data <- data.frame(func_data)
  return(func_data)
}

# Check if max frame length is 1518
max_row_from_column <- function(ramka, kolumna){
  wartosc <- max(kolumna, na.rm = TRUE)
  vector <- ramka[which.max(kolumna),]
}

# Create filtered data frames with selected parameter #
filter_protocol <- function(ramka, filter){
  new_df <- filter(ramka, Protocol == filter)
  return(new_df)
}

# Calculate packets/second
pakiety_s_f <- function(ramka){
  i <- 1
  j <- 1
  k <- ramka$Time[i]
  sum <- 0
  df <- data.frame(ncol = 2)
  
  while(nrow(ramka)>i){
    while(is.na(k) == FALSE && k<j){
      i=i+1
      sum <- sum+1
      k <- ramka$Time[i]
    }
    df[j,2] <- sum
    df[j,1] <- j
    j <- j+1
    sum <- 0
  }
  colnames(df) <- c("Sekunda", "Ilosc pakietow")
  return(df)
}

# # Calculate packets/0.01second
pakiety_001s_f <- function(ramka){
  i <- 1
  j <- 1
  w<- 0.01
  k <- ramka$Time[i]
  sum <- 0
  df <- data.frame(ncol = 2)
  
  while(nrow(ramka)>i){
    while(is.na(k) == FALSE && k<w){
      i=i+1
      sum <- sum+1
      k <- ramka$Time[i]
    }
    df[j,2] <- sum
    df[j,1] <- j
    j <- j+1
    w<- w+0.01
    sum <- 0
  }
  colnames(df) <- c("Setna czesc sekundy", "Ilosc pakietow")
  return(df)
}

# Change given column to numeric vector 
df_to_vector <- function(kolumna){
  num_vector <- as.character(kolumna)
  num_vector <- as.numeric(num_vector)
  return(num_vector)
}

# Kurtosis - packets/second [in 10 second frames]
kurtoza_f <- function(ramka){
  i <- 1
  j <- 1
  kurt <- 0
  df <- data.frame(ncol = 2)
  h <- (-9)+10*j
  hx <- 10*j+1
  
  while(nrow(ramka)>i){
    kurt <- kurtosis(ramka[(h:hx),2], na.rm = FALSE)
    df[j,2] <- kurt
    df[j,1] <- j*10
    j <- j+1
    h <- (-9)+10*j
    hx <- 10*j+1
    i <- i+10
  }
  df <- df[-nrow(df),]
  colnames(df) <- c("Przedzial czasowy", "Kurtoza")
  return(df)
}

# Skewness - packets/second [in 10 second frames]
skewness_f <- function(ramka){
  i <- 1
  j <- 1
  skew <- 0
  df <- data.frame(ncol = 2)
  h <- (-9)+10*j
  hx <- 10*j+1
  
  while(nrow(ramka)>i){
    skew <- skewness(ramka[(h:hx),2], na.rm = FALSE)
    df[j,2] <- skew
    df[j,1] <- j*10
    j <- j+1
    h <- (-9)+10*j
    hx <- 10*j+1
    i <- i+10
  }
  df <- df[-nrow(df),]
  colnames(df) <- c("Przedzial czasowy", "Wspó³czynnik skoœnoœci")
  return(df)
}

# Gini index - packets/second [in 10 second frames]
gini_f <- function(ramka){
  i <- 1
  j <- 1
  gin <- 0
  df <- data.frame(ncol = 2)
  h <- (-9)+10*j
  hx <- 10*j+1
  
  while(nrow(ramka)>i){
    gin <- gini(ramka[(h:hx),2])
    df[j,2] <- gin
    df[j,1] <- j*10
    j <- j+1
    h <- (-9)+10*j
    hx <- 10*j+1
    i <- i+10
  }
  df <- df[-nrow(df),]
  colnames(df) <- c("Przedzial czasowy", "Wspolczynnik Giniego")
  return(df)
}

# Hurst exponent - packets/second [in 2 second frames]
hurst_f <- function(ramka){
  i <- 1
  j <- 1
  hur <- 0
  df <- data.frame(ncol = 3)
  h <- (-199)+200*j
  hx <- 200*j+1
  
  while(nrow(ramka)>i){
    if (j > 12) 
      break
    else
      hur <- liftHurst(ramka[(h:hx),2],tradonly=TRUE)
    df[j,2] <- hur[1,1]
    df[j,3] <- hur[1,2]
    df[j,1] <- j*200
    j <- j+1
    h <- (-199)+200*j
    hx <- 200*j+1
    i <- i+200
  }
  df <- df[-nrow(df),]
  colnames(df) <- c("Przedzial czasowy", "Logarytm", "Wykladnik Hursta")
  return(df)
}

# Coefficient of variation - packets/second [in 10 second frames]
wspol_zmien_f <- function(ramka){
  i <- 1
  j <- 1
  wz <- 0
  df <- data.frame(ncol = 2)
  h <- (-9)+10*j
  hx <- 10*j+1
  
  while(nrow(ramka)>i){
    wz <- cv(C.of.V=NULL, mean(ramka[(h:hx),2]), sd(ramka[(h:hx),2]), unbiased=FALSE)
    df[j,2] <- wz
    df[j,1] <- j*10
    j <- j+1
    h <- (-9)+10*j
    hx <- 10*j+1
    i <- i+10
  }
  df <- df[-nrow(df),]
  colnames(df) <- c("Przedzial czasowy", "Wspolczynnik zmiennosci")
  return(df)
}

# Dominant of frame length of given protocol
dominanta <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Predicting linear data
predict_f <- function(ramka, N){
  x_p <- ramka[,1]
  y_p <- ramka[,2]
  relacja <- lm(y_p~x_p)
  predict_x <- data.frame(x_p = N)
  wynik_predict_x <-  predict(relacja, predict_x)
  
  x_p[N] <- N
  y_p[N] <- wynik_predict_x
  
  return(wynik_predict_x)
}

predict2_f <- function(X){
  pakiety_na_sekunde2 <- pakiety_na_sekunde
  
  for (i in (25:X)){
    pakiety_na_sekunde2[i,1] <- i
    pakiety_na_sekunde2[i,2] <- predict_f(pakiety_na_sekunde, i)
  }
  return(pakiety_na_sekunde2)
}

# Correlation table
correlation_f <- function(){
  c_TCP_ICMP <- correlate(s_TCP_na_sekunde[,2], s_ICMP_na_sekunde[,2])
  c_TCP_TLSv1.2 <- correlate(s_TCP_na_sekunde[,2], s_TLSv1.2_na_sekunde[,2])
  c_TCP_UDP <- correlate(s_TCP_na_sekunde[,2], s_UDP_na_sekunde[,2])
  c_ICMP_TLSv1.2 <- correlate(s_ICMP_na_sekunde[,2], s_TLSv1.2_na_sekunde[,2])
  c_ICMP_UDP <- correlate(s_ICMP_na_sekunde[,2], s_UDP_na_sekunde[,2])
  c_UDP_TLSv1.2 <- correlate(s_UDP_na_sekunde[,2], s_TLSv1.2_na_sekunde[,2])
  
  c_df <- data.frame(ncol = 2)
  c_df[(1:6),1] <- c("TCP_ICMP", "TCP_TLSv1.2", "TCP_UDP", "ICMP_TLSv1.2", "ICMP_UDP", "UDP_TLSv1.2")
  vect <- c(c_TCP_ICMP[1,2], c_TCP_TLSv1.2[1,2], c_TCP_UDP[1,2], c_ICMP_TLSv1.2[1,2], c_ICMP_UDP[1,2], c_UDP_TLSv1.2[1,2])
  for (i in 1:6){
    c_df[i,2] <-  vect[i]
  } 
  colnames(c_df) <- c("Protocol", "Strength of correlation")
  return(c_df)
}

#############################_loading_data_####################

potezne_dane <- chrum('sample.dump_tx')

#############################_data_############################

max_length <- max_row_from_column(potezne_dane,potezne_dane$Length)
max_length_value <- max_length[,5]

all_TCP <- filter_protocol(potezne_dane, 'TCP')
all_HTTP <- filter_protocol(potezne_dane, 'HTTP')
all_TLSv1.2 <- filter_protocol(potezne_dane, 'TLSv1.2')
all_UDP <- filter_protocol(potezne_dane, 'UDP')
all_ARP <- filter_protocol(potezne_dane, 'ARP')
all_DNS <- filter_protocol(potezne_dane, 'DNS')
all_HTTP_JSON <- filter_protocol(potezne_dane, 'HTTP/JSON')
all_ICMP <- filter_protocol(potezne_dane, 'ICMP')
all_NBNS <- filter_protocol(potezne_dane, 'NBNS')
all_OpenVPN <- filter_protocol(potezne_dane, 'OpenVPN')
all_SSH <- filter_protocol(potezne_dane, 'SSH')
all_WOL <- filter_protocol(potezne_dane, 'WOL')

s_TCP_na_sekunde <- pakiety_s_f(all_TCP)
s_HTTP_na_sekunde <- pakiety_s_f(all_HTTP)
s_TLSv1.2_na_sekunde <- pakiety_s_f(all_TLSv1.2)
s_UDP_na_sekunde <- pakiety_s_f(all_UDP)
s_ARP_na_sekunde <- pakiety_s_f(all_ARP)
s_DNS_na_sekunde <- pakiety_s_f(all_DNS)
s_HTTP_JSON_na_sekunde <- pakiety_s_f(all_HTTP_JSON)
s_ICMP_na_sekunde <- pakiety_s_f(all_ICMP)
s_NBNS_na_sekunde <- pakiety_s_f(all_NBNS)
s_OpenVPN_na_sekunde <- pakiety_s_f(all_OpenVPN)
s_SSH_na_sekunde <- pakiety_s_f(all_SSH)
s_WOL_na_sekunde <- pakiety_s_f(all_WOL)

s_TCP_na_001sekunde <- pakiety_001s_f(all_TCP)
s_HTTP_na_001sekunde <- pakiety_001s_f(all_HTTP)
s_TLSv1.2_na_001sekunde <- pakiety_001s_f(all_TLSv1.2)
s_UDP_na_001sekunde <- pakiety_001s_f(all_UDP)
s_ARP_na_001sekunde <- pakiety_001s_f(all_ARP)
s_DNS_na_001sekunde <- pakiety_001s_f(all_DNS)
s_HTTP_JSON_na_001sekunde <- pakiety_001s_f(all_HTTP_JSON)
s_ICMP_na_001sekunde <- pakiety_001s_f(all_ICMP)
s_NBNS_na_001sekunde <- pakiety_001s_f(all_NBNS)
s_OpenVPN_na_001sekunde <- pakiety_001s_f(all_OpenVPN)
s_SSH_na_001sekunde <- pakiety_001s_f(all_SSH)
s_WOL_na_001sekunde <- pakiety_001s_f(all_WOL)

pakiety_na_sekunde <- pakiety_s_f(potezne_dane)
pakiety_na_0.01s <- pakiety_001s_f(potezne_dane)

######################## analysis ##########################
df_kurtoza <- kurtoza_f(pakiety_na_0.01s)
df_skewness <- skewness_f(pakiety_na_0.01s)
df_hurst <- hurst_f(pakiety_na_0.01s)
df_gini <- gini_f(pakiety_na_0.01s)
df_wspol_zmien <- wspol_zmien_f(pakiety_na_0.01s)

# TCP
df_kurtoza_TCP <- kurtoza_f(s_TCP_na_001sekunde)
df_skewness_TCP <- skewness_f(s_TCP_na_001sekunde)
df_hurst_TCP <- hurst_f(s_TCP_na_001sekunde)
df_gini_TCP <- gini_f(s_TCP_na_001sekunde)
df_wspol_zmien_TCP <- wspol_zmien_f(s_TCP_na_001sekunde)

# UDP
df_kurtoza_UDP <- kurtoza_f(s_UDP_na_001sekunde)
df_skewness_UDP <- skewness_f(s_UDP_na_001sekunde)
df_hurst_UDP <- hurst_f(s_UDP_na_001sekunde)
df_gini_UDP <- gini_f(s_UDP_na_001sekunde)
df_wspol_zmien_UDP <- wspol_zmien_f(s_UDP_na_001sekunde)

# TLSv1.2
df_kurtoza_TLSv1.2 <- kurtoza_f(s_TLSv1.2_na_001sekunde)
df_skewness_TLSv1.2 <- skewness_f(s_TLSv1.2_na_001sekunde)
df_hurst_TLSv1.2 <- hurst_f(s_TLSv1.2_na_001sekunde)
df_gini_TLSv1.2 <- gini_f(s_TLSv1.2_na_001sekunde)
df_wspol_zmien_TLSv1.2 <- wspol_zmien_f(s_TLSv1.2_na_001sekunde)

# DNS
df_kurtoza_DNS <- kurtoza_f(s_DNS_na_001sekunde)
df_skewness_DNS <- skewness_f(s_DNS_na_001sekunde)
df_hurst_DNS <- hurst_f(s_DNS_na_001sekunde)
df_gini_DNS <- gini_f(s_DNS_na_001sekunde)
df_wspol_zmien_DNS <- wspol_zmien_f(s_DNS_na_001sekunde)

# ICMP
df_kurtoza_ICMP <- kurtoza_f(s_ICMP_na_001sekunde)
df_skewness_ICMP <- skewness_f(s_ICMP_na_001sekunde)
df_hurst_ICMP <- hurst_f(s_ICMP_na_001sekunde)
df_gini_ICMP <- gini_f(s_ICMP_na_001sekunde)
df_wspol_zmien_ICMP <- wspol_zmien_f(s_ICMP_na_001sekunde)

# korelacje
df_correlation <- correlation_f()

hurst_all <- liftHurst(pakiety_na_0.01s[,2], tradonly=TRUE)[2]
skewness_all <- skewness(pakiety_na_0.01s[,2], na.rm = FALSE) 
kurtoza_all <- kurtosis(pakiety_na_0.01s[,2], na.rm = FALSE)
gini_all <- gini(pakiety_na_0.01s[,2])
srednia_all <- mean(pakiety_na_0.01s[,2])
srednia_s_all <- mean(pakiety_na_sekunde[,2])

# Predicting
pakiety_na_sekunde_predicted <- predict2_f(26)

# For plotting number of occurences
no_occurences <- data.frame()
no_occurences[1,1] <- nrow(all_TCP)
no_occurences[2,1] <- nrow(all_HTTP)
no_occurences[3,1] <- nrow(all_TLSv1.2)
no_occurences[4,1] <- nrow(all_UDP)
no_occurences[5,1] <- nrow(all_ARP)
no_occurences[6,1] <- nrow(all_DNS)
no_occurences[7,1] <- nrow(all_HTTP_JSON)
no_occurences[8,1] <- nrow(all_ICMP)
no_occurences[9,1] <- nrow(all_NBNS)
no_occurences[10,1] <- nrow(all_OpenVPN)
no_occurences[11,1] <- nrow(all_SSH)
no_occurences[12,1] <- nrow(all_WOL)
rownames(no_occurences) <- c("TCP", "HTTP", "TLSv1.2", "UDP", "ARP", "DNS", "HTTP/JSON", "ICMP", "NBNS", "OpenVPN", "SSH", "WOL")
colnames(no_occurences) <- c("Numer of occurences")

#############################_plots_#############################
par(bg = "#faf1dc")

##### for_pakiety_na_00.1_sekundy #####
png(file="plot_predicted.png", width=1366, height = 768)
plot(pakiety_na_sekunde_predicted[,1], pakiety_na_sekunde_predicted[,2], type='l', main='Calkowity rych sieciowy z przewidzianymi wartosciami', 
     xlab="Sekunda czasu badania", ylab="Przep³yw ruchu [liczba pakietów]", col = "#113d23", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_ruch_sieciowy.png", width=1366, height = 768)
plot(pakiety_na_sekunde[,1], pakiety_na_sekunde[,2], type='l', main='Ca³kowity ruch sieciowy', 
     xlab="Sekunda czasu badania", ylab="Przep³yw ruchu [liczba pakietów]", col = "#113d23", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_kurtoza.png", width=1366, height = 768)
plot(df_kurtoza[,1], df_kurtoza[,2], type='l', main='Kurtoza', 
     xlab="Setna czesc sekundy czasu badania", ylab="Kurtoza", col = "#542a2a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_skosnosc.png", width=1366, height = 768)
plot(df_skewness[,1], df_skewness[,2], type='l', main='Skosnosc', 
     xlab="Setna czesc sekundy czasu badania", ylab="Skosnosc", col = "#2e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_hurst.png", width=1366, height = 768)
plot(df_hurst[,1], df_hurst[,3], type='l', main='Wykladnik Hursta', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wykladnik Hursta", col = "#4e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_gini.png", width=1366, height = 768)
plot(df_gini[,1], df_gini[,2], type='p', main='Wspolczynnik Giniego', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik Giniego", col = "#2a5154", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_zmiennosc.png", width=1366, height = 768)
plot(df_wspol_zmien[,1], df_wspol_zmien[,2], type='o', main='Wspolczynnik zmiennosci', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik zmiennosci", col = "#54502a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_gestosc.png", width=1366, height = 768)
densityplot(pakiety_na_0.01s[,2], main="Gêstoœæ rozk³adu prawdopodobieñstwa", xlab="Iloœæ pakietów", ylab="Gêstoœæ", 
            col = "#bda18c", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

##### for_pakiety_TCP #####
png(file="plot_ruch_sieciowy_TCP.png", width=1366, height = 768)
plot(s_TCP_na_sekunde[,1], s_TCP_na_sekunde[,2], type='l', main='Ca³kowity ruch sieciowy pakietów protokolu TCP', 
     xlab="Sekunda czasu badania", ylab="Przep³yw ruchu [liczba pakietów]", col = "#113d23", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_kurtoza_TCP.png", width=1366, height = 768)
plot(df_kurtoza_TCP[,1], df_kurtoza_TCP[,2], type='l', main='Kurtoza pakietów protokolu TCP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Kurtoza", col = "#542a2a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_skosnosc_TCP.png", width=1366, height = 768)
plot(df_skewness_TCP[,1], df_skewness_TCP[,2], type='l', main='Skosnosc pakietów protokolu TCP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Skosnosc", col = "#2e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_hurst_TCP.png", width=1366, height = 768)
plot(df_hurst_TCP[,1], df_hurst_TCP[,3], type='l', main='Wykladnik Hursta pakietów protokolu TCP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wykladnik Hursta", col = "#4e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_gini_TCP.png", width=1366, height = 768)
plot(df_gini_TCP[,1], df_gini_TCP[,2], type='p', main='Wspolczynnik Giniego pakietów protokolu TCP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik Giniego", col = "#2a5154", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_zmiennosc_TCP.png", width=1366, height = 768)
plot(df_wspol_zmien_TCP[,1], df_wspol_zmien_TCP[,2], type='o', main='Wspolczynnik zmiennosci pakietów protokolu TCP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik zmiennosci", col = "#54502a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

##### for_pakiety_UDP #####
png(file="plot_ruch_sieciowy_UDP.png", width=1366, height = 768)
plot(s_UDP_na_sekunde[,1], s_UDP_na_sekunde[,2], type='l', main='Ca³kowity ruch sieciowy pakietów protokolu UDP', 
     xlab="Sekunda czasu badania", ylab="Przep³yw ruchu [liczba pakietów]", col = "#113d23", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_kurtoza_UDP.png", width=1366, height = 768)
plot(df_kurtoza_UDP[,1], df_kurtoza_UDP[,2], type='l', main='Kurtoza pakietów protokolu UDP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Kurtoza", col = "#542a2a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_skosnosc_UDP.png", width=1366, height = 768)
plot(df_skewness_UDP[,1], df_skewness_UDP[,2], type='l', main='Skosnosc pakietów protokolu UDP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Skosnosc", col = "#2e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_hurst_UDP.png", width=1366, height = 768)
plot(df_hurst_UDP[,1], df_hurst_UDP[,3], type='l', main='Wykladnik Hursta pakietów protokolu UDP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wykladnik Hursta", col = "#4e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_gini_UDP.png", width=1366, height = 768)
plot(df_gini_UDP[,1], df_gini_UDP[,2], type='p', main='Wspolczynnik Giniego pakietów protokolu UDP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik Giniego", col = "#2a5154", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_zmiennosc_UDP.png", width=1366, height = 768)
plot(df_wspol_zmien_UDP[,1], df_wspol_zmien_UDP[,2], type='o', main='Wspolczynnik zmiennosci pakietów protokolu UDP', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik zmiennosci", col = "#54502a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

##### for_pakiety_TLSv1.2 #####
png(file="plot_ruch_sieciowy_TLSv1.2.png", width=1366, height = 768)
plot(s_TLSv1.2_na_sekunde[,1], s_TLSv1.2_na_sekunde[,2], type='l', main='Ca³kowity ruch sieciowy pakietów protokolu TLSv1.2', 
     xlab="Sekunda czasu badania", ylab="Przep³yw ruchu [liczba pakietów]", col = "#113d23", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_kurtoza_TLSv1.2.png", width=1366, height = 768)
plot(df_kurtoza_TLSv1.2[,1], df_kurtoza_TLSv1.2[,2], type='l', main='Kurtoza pakietów protokolu TLSv1.2', 
     xlab="Setna czesc sekundy czasu badania", ylab="Kurtoza", col = "#542a2a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_skosnosc_TLSv1.2.png", width=1366, height = 768)
plot(df_skewness_TLSv1.2[,1], df_skewness_TLSv1.2[,2], type='l', main='Skosnosc pakietów protokolu TLSv1.2', 
     xlab="Setna czesc sekundy czasu badania", ylab="Skosnosc", col = "#2e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_hurst_TLSv1.2.png", width=1366, height = 768)
plot(df_hurst_TLSv1.2[,1], df_hurst_TLSv1.2[,3], type='l', main='Wykladnik Hursta pakietów protokolu TLSv1.2', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wykladnik Hursta", col = "#4e2a54", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_gini_TLSv1.2.png", width=1366, height = 768)
plot(df_gini_TLSv1.2[,1], df_gini_TLSv1.2[,2], type='p', main='Wspolczynnik Giniego pakietów protokolu TLSv1.2', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik Giniego", col = "#2a5154", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

png(file="plot_zmiennosc_TLSv1.2.png", width=1366, height = 768)
plot(df_wspol_zmien_TLSv1.2[,1], df_wspol_zmien_TLSv1.2[,2], type='o', main='Wspolczynnik zmiennosci pakietów protokolu TLSv1.2', 
     xlab="Setna czesc sekundy czasu badania", ylab="Wspolczynnik zmiennosci", col = "#54502a", cex.main=2, cex.lab=1.5, cex.axis=1.5)
dev.off()

# Coefficient of variation in 4 protocols
# second
v <- ggplot(s_TCP_na_sekunde, aes(s_TCP_na_sekunde[,1], s_TCP_na_sekunde[,2])) + 
  theme_bw() + geom_point(colour = "#d8b7ed") + geom_quasirandom(alpha = 0.15) +
  labs(title = "TCP/s", x = "Sekunda czasu badania", y = "Pakiety na sekunde") +
  theme(text = element_text(size=20))

vv <- ggplot(s_HTTP_na_sekunde, aes(s_HTTP_na_sekunde[,1], s_HTTP_na_sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#d0f5f4") + geom_quasirandom(alpha = 0.15) +
  labs(title = "HTTP/s", x = "Sekunda czasu badania", y = "Pakiety na sekunde")+
  theme(text = element_text(size=20))

vvv <- ggplot(s_TLSv1.2_na_sekunde, aes(s_TLSv1.2_na_sekunde[,1], s_TLSv1.2_na_sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#f0e2b6") + geom_quasirandom(alpha = 0.15) +
  labs(title = "TLSv1.2/s", x = "Sekunda czasu badania", y = "Pakiety na sekunde")+
  theme(text = element_text(size=20))

vvvv <- ggplot(s_UDP_na_sekunde, aes(s_UDP_na_sekunde[,1], s_UDP_na_sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#cef2b6") + geom_quasirandom(alpha = 0.15) +
  labs(title = "UDP/s", x = "Sekunda czasu badania", y = "Pakiety na sekunde")+
  theme(text = element_text(size=20))

png(file="plot_ps_protokoly_s.png", width=1366, height = 768)
figure <- ggarrange(v, vv, vvv, vvvv,  ncol = 2, nrow = 2)
figure
dev.off()

# 0.01 second
v <- ggplot(s_TCP_na_001sekunde, aes(s_TCP_na_001sekunde[,1], s_TCP_na_001sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#d8b7ed") + geom_quasirandom(alpha = 0.15) +
  labs(title = "TCP/0.01s", x = "Setna czesc sekundy czasu badania", y = "Pakiety na setna czesc sekundy") +
  theme(text = element_text(size=20))

vv <- ggplot(s_HTTP_na_001sekunde, aes(s_HTTP_na_001sekunde[,1], s_HTTP_na_001sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#d0f5f4") + geom_quasirandom(alpha = 0.15) +
  labs(title = "HTTP/0.01s", x = "Setna czesc sekundy czasu badania", y = "Pakiety na setna czesc sekundy") +
  theme(text = element_text(size=20))

vvv <- ggplot(s_TLSv1.2_na_001sekunde, aes(s_TLSv1.2_na_001sekunde[,1], s_TLSv1.2_na_001sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#f0e2b6") + geom_quasirandom(alpha = 0.15) +
  labs(title = "TLSv1.2/0.01s", x = "Setna czesc sekundy czasu badania", y = "Pakiety na setna czesc sekundy") +
  theme(text = element_text(size=20))

vvvv <- ggplot(s_UDP_na_001sekunde, aes(s_UDP_na_001sekunde[,1], s_UDP_na_001sekunde[,2])) + 
  theme_bw()  + geom_point(colour = "#cef2b6") + geom_quasirandom(alpha = 0.15) +
  labs(title = "UDP/0.01s", x = "Setna czesc sekundy czasu badania", y = "Pakiety na setna czesc sekundy") +
  theme(text = element_text(size=20))

png(file="plot_ps_protokoly_001s.png", width=1366, height = 768)
figure <- ggarrange(v, vv, vvv, vvvv,  ncol = 2, nrow = 2)
figure
dev.off()

# correlations #
png(file="plot_correlation.png", width=1366, height = 768)
barplot(df_correlation[,2], col = viridis(length(df_correlation[,2])), main='Correlation between types of packets', border="violet", 
        ylim = c(-0.1, 0.8), names.arg= c("TCP_ICMP", "TCP_TLSv1.2", "TCP_UDP", "ICMP_TLSv1.2", "ICMP_UDP", "UDP_TLSv1.2"),
        cex.main=2, cex.lab=1.5, cex.axis=1.5)  

abline(h=0.1, col = "Red")
text(4.25,0.2, "None or very weak correlation", col = "Red")

abline(h=0.3, col = "Green")
text(4.25,0.4, "Weak correlation", col = "Green")

abline(h=0.5, col = "Blue")
text(4.25,0.6, "Moderate correlation", col = "Blue")

abline(h=0.7, col = "Black")
text(4.25,0.75, "Strong correlation", col = "Black")
dev.off()

# occurences #
png(file="plot_occurences.png", width=1366, height = 768)
barplot(no_occurences[,1], main='Number of occurences of given protocols', 
        names.arg = c("TCP", "HTTP", "TLSv1.2", "UDP", "ARP", "DNS", "HTTP/JSON", "ICMP", "NBNS", "OpenVPN", "SSH", "WOL")
        )
dev.off()
