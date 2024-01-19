library(ggplot2)

diamonds <- read.csv('diamonds.csv',colClasses=c(X='NULL'))
nrow(diamonds)

# chyba bedzie to trzeba bardziej rozwinac 
# Hipoteza 1: wielkość diamentu nie ma wpływu na jego jakość.
# Hipoteza 2: masa diamentu nie zależy od koloru.
# Hipoteza 3  Wraz ze zwiększeniem się zmiennej 'tables' rośnie cena diamentu.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

# wykres podklesla, ze wielkosc raczej nie ma wplywu na jakosc
ggplot(data=diamonds,aes(cut,volume,fill=cut))+
  geom_boxplot()

anova_1 <- aov(volume~cut,data = diamonds)
summary(anova_1)
# 