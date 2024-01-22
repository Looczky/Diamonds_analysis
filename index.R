library(ggplot2)

diamonds <- read.csv('diamonds.csv',colClasses=c(X='NULL'))

# chyba bedzie to trzeba bardziej rozwinac 
# Hipoteza 1: wielkość diamentu nie ma wpływu na jego jakość.
# Hipoteza 2: cena diamentu zależy od koloru.
# Hipoteza 3  Wraz ze zwiększeniem się zmiennej 'tables' rośnie cena diamentu.

#analiza eskploracyjna
View(diamonds)
str(diamonds)
nrow(diamonds)
sum(is.na(diamonds))

diamonds$cut <- factor(diamonds$cut)
diamonds$color <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

diamonds

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(diamonds,aes(x=cut,fill=cut))+
  geom_bar()+
  labs(title='Liczba diamentow danej jakości pocięcia')
  
  
ggplot(diamonds,aes(x=color,fill=clarity))+
  geom_bar()+
  labs(title='Procent diamentow posiadajaca dany kolor podzielona na przejrzystosc')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))

ggplot(diamonds,aes(x=clarity,fill=color))+
  geom_bar()+
  labs(title='Procent diamentow posiadajaca dany poziom przejrzystosci podzielona na kolor')+
  scale_y_continuous(labels = scales::percent_format(scale = 1))


diamonds$table_group <- cut(diamonds$table, breaks=seq(min(diamonds$table),max(diamonds$table),by=4),include.lowest = TRUE)
ggplot(diamonds, aes(x = table_group,y=price,fill=table_group)) +
  geom_boxplot()

ggplot(diamonds,aes(carat,volume,color=color))+
  geom_point()

ggplot(diamonds,aes(price,volume,color=cut))+
  geom_point()

ggplot(diamonds,aes(depth,price,color=cut))+
  geom_point()


ggplot(data=diamonds,aes(cut,price,fill=cut))+
  geom_boxplot()

ggplot(data=diamonds,aes(clarity,price,fill=clarity))+
  geom_boxplot()

ggplot(data=diamonds,aes(color,price,fill=color))+
  geom_boxplot()



#H1

# wykres podklesla, ze wielkosc raczej nie ma wplywu na jakosc
ggplot(data=diamonds,aes(cut,volume,fill=cut))+
  geom_boxplot()

anova_1 <- aov(volume~cut,data = diamonds)
summary(anova_1)
# odrzucamy hipoteze ze wielkosc diamentu ma wplyw na jakosc

