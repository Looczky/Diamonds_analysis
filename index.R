library(ggplot2)

diamonds <- read.csv('diamonds.csv',colClasses=c(X='NULL'))
View(diamonds)
str(diamonds)

diamonds$x[diamonds$x == 0] = NA
diamonds$y[diamonds$y == 0] = NA
diamonds$z[diamonds$z == 0] = NA

diamonds$cut <- factor(diamonds$cut, c('Ideal', 'Premium', 'Very Good', 'Good', 'Fair'))
diamonds$color <- factor(diamonds$color, c('D', 'E', 'F', 'G', 'H', 'I', 'J'))
diamonds$clarity <- factor(diamonds$clarity, c('IF', 'VVS1', 'VVS2', 'VS1', 'VS2', 'SI1', 'SI2', 'I1'))
                           
sum(is.na(diamonds))
diamonds <- na.omit(diamonds)
nrow(diamonds)


# Hipoteza 1: ???
# Hipoteza 2: Istnieje zależność między jakością cięcia a `table`
# Hipoteza 3: Cena diamentu zależy od koloru


# Badania nad zależnością między wymiarami diamentu
cor(diamonds[c(8:10)])

model_0.1 <- lm(y ~ x, diamonds)
summary(model_0.1)
ggplot(diamonds, aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm')

model_0.2 <- lm(z ~ x, diamonds)
summary(model_0.2)
ggplot(diamonds, aes(x, z)) +
  geom_point() +
  geom_smooth(method='lm')

model_0.3 <- lm(z ~ y, diamonds)
summary(model_0.3)
ggplot(diamonds, aes(y, z)) +
  geom_point() +
  geom_smooth(method='lm')

diamonds$volume = diamonds$x * diamonds$y * diamonds$z


# Analiza eskploracyjna
ggplot(diamonds, aes(x=cut, fill=cut)) +
  geom_bar() +
  labs(title='Liczba diamentów danej jakości szlifu') +
  xlab('Jakość szlifu') +
  ylab('Liczba diamentów')
  
ggplot(diamonds, aes(x=color, fill=clarity)) +
  geom_bar() +
  labs(title='Procent diamentów posiadajacy dany kolor', subtitle='Uwzględniony podział na przejrzystość') +
  xlab('Kolor') +
  ylab('Procent') +
  scale_y_continuous(labels=scales::percent_format(scale=1))

ggplot(diamonds,aes(x=clarity, fill=color)) +
  geom_bar() +
  labs(title='Procent diamentów posiadajacy dany poziom przejrzystości', subtitle='Uwzględniony podział na kolor') +
  xlab('Poziom przejrzystości') +
  ylab('Procent') +
  scale_y_continuous(labels=scales::percent_format(scale=1))

diamonds$table_group <- cut(diamonds$table, breaks=seq(min(diamonds$table), max(diamonds$table), by=4), include.lowest=T)
ggplot(diamonds, aes(table_group, price, fill=table_group)) +
  labs(title='Wykres pudełkowy ceny w zależności od `table_group`') +
  xlab('`table_group`') +
  ylab('Cena') +
  geom_boxplot()

ggplot(diamonds, aes(carat, volume, color=color)) +
  labs(title='Objętość diamentów w zależności od liczby karatów', subtitle='Uwzględniony podział na kolor') +
  xlab('Liczba karatów') +
  ylab('Objętość') +
  geom_point()

ggplot(diamonds, aes(volume, price, color=cut)) +
  labs(title='Cena diamentów w zależności od objętości', subtitle='Uwzględniony podział na jakość szlifu') +
  xlab('Objętość') +
  ylab('Cena') +
  geom_point()

ggplot(diamonds, aes(depth, price, color=cut)) +
  labs(title='Cena diamentów w zależności od `depth`', subtitle='Uwzględniony podział na jakość szlifu') +
  xlab('`depth`') +
  ylab('Cena') +
  geom_point()

ggplot(diamonds, aes(cut, price, fill=cut)) +
  labs(title='Wykres pudełkowy ceny w zależności od jakości szlifu') +
  xlab('Jakość szlifu') +
  ylab('Cena') +
  geom_boxplot()

ggplot(diamonds, aes(clarity, price, fill=clarity)) +
  labs(title='Wykres pudełkowy ceny w zależności od przejrzystości') +
  xlab('Przejrzystość') +
  ylab('Cena') +
  geom_boxplot()

ggplot(diamonds, aes(color, price, fill=color)) +
  labs(title='Wykres pudełkowy ceny w zależności od koloru') +
  xlab('Kolor') +
  ylab('Cena') +
  geom_boxplot()

ggplot(diamonds, aes(carat, price, color=clarity)) +
  labs(title='Cena diamentów w zależności od liczby karatów', subtitle='Uwzględniony podział na przejrzystość') +
  xlab('Liczba karatów') +
  ylab('Cena') +
  geom_point()

ggplot(diamonds, aes(cut, volume, fill=cut)) +
  labs(title='Wykres pudełkowy objętości w zależności od jakości szlifu') +
  xlab('Jakość szlifu') +
  ylab('Objętość') +
  geom_boxplot()

# brak zaleznosci: hipoteza do wymiany!
diamonds$table_group <- cut(diamonds$table, breaks=seq(min(diamonds$table), max(diamonds$table), by=4), include.lowest=T)
ggplot(diamonds, aes(cut, table, fill=cut)) +
  labs(title='Wykres pudełkowy ceny w zależności od `table_group`') +
  xlab('`table_group`') +
  ylab('Cena') +
  geom_boxplot()
