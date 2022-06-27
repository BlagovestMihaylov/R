ptu <- c("qqplotr")
IP <- setdiff(ptu, row.names(installed.packages()))
if(length(IP) > 0) {for(el in IP) {install.packages(el)}}
lapply(X = ptu, FUN = library, character.only = TRUE)





#                           О П И С А Т Е Л Н А    С Т А Т И С Т И К А

#       1. Променливи
#       Най-простата дефиниция за променливи е характеристика, която се варира от един човек/обект 
#   до друг. Променливите са категоризирани в два типа - количествени или числови (представят се 
#   с числа)и категорийни или качествени (нечислово изобразяване). 

#       Категорийните променливи се разбиват на два подтипа - номинални (без подредба) и ординални 
#   (с подредба). Примери за стойности на НОМИНАЛНИ променливи са: видове заведения, различни  
#   държави и организции, видове алкохол, раси и т.н. Примери за стойности на ОРДИНАРНИ 
#   променливи са: степени на образование/квалификация, нива на социалните общества и т.н.


#       Количествените се разбиват на два подтипа - непрекъснати (стойностите могат да варират
#   в даден числов интервал) и дискретни (възможните стойности могат да бъдат изброени). 
#   Примери за стойности на НЕПРЕКЪСНАТИ променливи са: възраст, различни съотношения, тегло
#   и т.н. Примери за стойности на ДИСКРЕТНИ променливи са: брой катастрофи на шофьори, брой на 
#   деца и т.н.


#       2. Разпределения
#       Разпределение - това е таблица, графика или формула, съдържаща стойностите на наблюденията 
#   и колко често се случват. Разпределенията се характеризират с форма, модалност (modality), 
#   локация, размах, симетричност/асиметричност и ексцес.

#   На графиката по-долу са представени няколко от формите на плътността на разпределенията
N <- 2*10^4
distributions <- c("Камбановидна", "J-", "Правоъгълна", "Дясно асиметрична", "Двумодална")
par(mfrow = c(2, 3))
for(distr in distributions) {
  distribution_values <- switch(distr, "Камбановидна" = rnorm(N), "J-" = rexp(N), 
                                "Правоъгълна" = round(runif(N), 2), 
                                "Дясно асиметрична" = rgamma(N, shape = 5),
                                "Двумодална" = c(rnorm(N, mean = -2, sd = 2), rnorm(N, mean = 3, sd = 1)))
  distr_density <- density(distribution_values)
  hist(distribution_values, col = "pink", prob = T, main = paste(distr, "форма на разпределението"), 
       ylab = "Плътност", xlab = "Стойности", ylim = range(distr_density$y), 
       xlim = range(distr_density$x))
  lines(distr_density, lwd = 3, col = "blue")
}
par(mfrow = c(1, 1))


#   За видовете разпределения, симулациите, квантили и плътност ще говорим по-нататъка.




country = c('Switzerland', 'Norway', 'Iceland', 'Denmark', 'Finland', 'Netherlands', 'Germany', 
            'Ireland', 'Sweden', 'United Kingdom', 'Austria', 'Belgium', 'France', 'Italy', 
            'Spain', 'Malta', 'Slovenia', 'Estonia', 'Czech Republic', 'Portugal', 'Slovakia', 
            'Poland', 'Croatia', 'Lithuania', 'Greece', 'Latvia', 'Hungary', 'Romania', 
            'Bulgaria', 'Russia', 'Bosnia And Herzegovina', 'Serbia', 'Belarus', 'Albania', 
            'Macedonia', 'Ukraine', 'Moldova')

avg_sal_in_USD = c(4959.48, 3416.1, 3136.95, 2975.8, 2583.43, 2575.2, 2538.92, 2521.51, 2494.39, 
                   2394.78, 2219.23, 2162.23, 2140.64, 1675.32, 1480.64, 1278.04, 1180.66, 
                   1158.76, 1069.03, 925.84, 894.2, 884.81, 852.23, 804.68, 795.34, 771.91, 
                   708.91, 649.68, 611.41, 558.01, 481.17, 412.22, 409.62, 348.56, 346.15, 
                   293.26, 284.94)

salaryDF <- data.frame(country, avg_sal_in_USD)


#   Проучването е правено за 2018-та година за европейските страни
Country <- c("Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia", "Finland",
             "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia",
             "Lithuania", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal",
             "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "UK")
Car <- c("Volkswagen", "Volkswagen", "Skoda", "Skoda", "Peugeot", "Toyota", "Nissan",
         "Renault", "Volkswagen", "Toyota", "Suzuki", "Hyundai", "Fiat", "Volkswagen",
         "Fiat", "Volkswagen", "Volkswagen", "Nissan", "Skoda", "Renault", "Dacia",
         "Skoda", "Renault", "Seat", "Volvo", "Skoda", "Ford")
carsDF <- data.frame(Country, Car)



#                   А Н А Л И З    Н А    Е Д Н О М Е Р Н А    П Р О М Е Н Л И В А 
#                                   (UNIVARIATE ANALYSIS)


#       3. Категорийни променливи
#       Категорийните проенливи като цяло носят по-малко информация отколкото числовите променливи.
#   Често обаче, те носят по-голяма стабилност за прогнозните моделите отколкото числовите. 

#      Освен графично представяне на данните (ще го разгледаме по-късно), най-добре честотата се 
#   вижда посредством таблици. Функцията, която ни трябва е table()


head(carsDF)
tt <- table(carsDF[, "Car"])
tt      #   Взимаме честотното разпределение

round(prop.table(tt)*100, 2)  #   Процентното разпределение
#	С prop.table() взимаме процентите от вече направена таблица

#   Volkswagen държи най-голям пазарен дял по страни, следван от Skoda




#       4. Числови променливи
#           4.1. Оценка на центъра (локацията) на разпределение
#               4.1.1. Средна стойност (Очакване)

meanFunction <- function(x) {
  sum(x)/length(x)
}

meanFunction(salaryDF$avg_sal_in_USD)
mean(salaryDF$avg_sal_in_USD)


#               4.1.2. Медиана
#       Подреждаме данните във вариационен ред. Взимаме средната стойност (при нечетен брой) 
#   или средната стойност на средните два елемента (при четен брой елементи).

medianFunction <- function(x) {
  x_sorted <- sort(x)
  nn <- length(x_sorted)
  
  if(nn %% 2 == 0) {
    return(mean(x_sorted[nn/2 + c(0, 1)]))
  } else {
    return(x_sorted[round(nn/2 + 0.25)])
  }
}

#  %% == mod
#  %/% == div


medianFunction(salaryDF$avg_sal_in_USD)
median(salaryDF$avg_sal_in_USD)


medianFunction(1:10)
median(1:10)



#               4.1.3. Мода - най-често срещаната стойност
modeFunction <- function(x) {
  tt <- table(x)
  return(names(tt)[tt == max(tt)])    
}

#       Модата може да има повече от една стойност

#   table() - честотоното разпределение на променлива (коя стойност колко пъти се с)
#   which.max() - връща индекса на първото число, което приема МАКСИМАЛНА стойност
#   which.min() - връща индекса на първото число, което приема МИНИМАЛНА стойност
#   names() - връща имената на стойностите


modeFunction(round(salaryDF$avg_sal_in_USD/100))



summary(salaryDF$avg_sal_in_USD)    #   Описателна статистика за центъра на разпределението
quantile(salaryDF$avg_sal_in_USD, prob = seq(0.1, 0.9, by = 0.1))
#   квантили





#           Задача 1 - Ежедневни инциденти с мотоциклети
#       Шотландският изпълнителен директор в отдел "Аналитични услуги" на Транспортна 
#   статистика събира данни за произшествията с мотоциклети. В таблицата по-долу са 
#   представени, броят на инцидентите с мотоциклети в Шотландия по пътища с ограничение 
#   до 30 и над 30 мили в час, случили се по дни от седмицата.

Day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
Built_up <- c(88, 100, 76, 98, 103, 85, 69)
Non_built_up <- c(70, 58, 59, 53, 56, 94, 102)

motorcycleAccidentsDF <- data.frame(Day, Built_up, Non_built_up)


#   а. Каква е средната стойност и медианата на броя на произшествията за двата вида пътища
#   б. Каква е формата на разпределението по различните пътища. Интересува ни само модалността


#           Задача 2 - Инвестиции в акциите "LMT" и "FB"
Date <- c('2015-09', '2015-10', '2015-11', '2015-12', '2016-01', '2016-02', '2016-03', '2016-04', 
          '2016-05', '2016-06', '2016-07', '2016-08', '2016-09', '2016-10', '2016-11', '2016-12', 
          '2017-01', '2017-02', '2017-03', '2017-04', '2017-05', '2017-06', '2017-07', '2017-08', 
          '2017-09', '2017-10', '2017-11', '2017-12', '2018-01', '2018-02', '2018-03', '2018-04', 
          '2018-05', '2018-06', '2018-07', '2018-08', '2018-09')
FB <- c(89.9, 101.97, 104.24, 
        104.66, 112.21, 106.92, 114.1, 117.58, 118.81, 114.28, 123.94, 126.12, 128.27, 130.99, 
        118.42, 115.05, 130.32, 135.54, 142.05, 150.25, 151.46, 150.98, 169.25, 171.97, 170.87, 
        180.06, 177.18, 176.46, 186.89, 178.32, 159.79, 172, 191.78, 194.32, 172.58, 175.73, 
        164.46)
LMT <- c(207.31, 219.83, 219.16, 217.15, 211, 215.79, 221.5, 232.38, 236.23, 248.17, 252.73, 
         242.97, 239.72, 246.38, 265.25, 249.94, 251.33, 266.58, 267.6, 269.45, 281.13, 277.61, 
         292.13, 305.39, 310.29, 308.16, 319.12, 321.05, 354.85, 352.44, 337.93, 320.84, 314.54, 
         295.43, 326.1, 320.41, 345.96)
stocksDF <- data.frame(Date, LMT, FB)

#       Анализът трябва да се извърши върху възвръщаемостите на цените, получени по diff(log(x)),
#   където "x" е цената на актива

#   а. Какви са средните стойности и медианите. Според вас как се интерпретират тези числа
#   б. Можете ли да кажете в кой актив бихте инвестирали при наличието само на тази информация



#               Вариация

team1 <- c(72, 73, 76, 76, 78)
team2 <- c(67, 72, 76, 76, 84)
basketball_teams <- data.frame(team1, team2)


#       3. Оценка на вариацията на разпределение


#   Преди да започнем с изследването на разсейването, първо ще видим какво е очакването
colMeans(basketball_teams)  #   Взимаме средните на стойностите по колони
apply(basketball_teams, 2, mean)    #   Еквивалентно на горния ред


#       Като цело се избягват заключенията само на база средните стойности или други оценки за 
#   локацията на разпределенията (ще наблегнем на това при оценките на хипотезите). 
#       И при двета отбора средните стойности са равни - 75. това не означава, че двата отбора 
#   са близки относно разпределението на височината им. В частност, височината при играчите на
#   втория отбор варира много повече от тази на първия. В следващата секция ще разгледаме как
#   можем да оженим вариацията.



#            3.1. Обхват (Range) - максималната стойност - минималната стойност

rangeFunction <- function(x) {
  max(x) - min(x)
}

rangeFunction(basketball_teams$team1)   
rangeFunction(basketball_teams$team2) 
#   както можем да видим от резултатите, при първия отбор имаме разлика от 6 инча, докато при втория - 17


#            3.2. Вариация (дисперсия) и стандартно отклонение


#	За разлика от обхвата, стандартното отклонение взема под внимамнеи всички наблюдения.
#	Стандартното отклонение е оценка на вариацията, която показва колко далече са наблюденията от очакването

#	    Стандартното отклонение е предпочитана оценка за вариацията, когато среднатото се използва за оценка 
#   на локацията (центъра) на разпределението.

#	Вариацията (дисперсията) се изчислява по формулата по-долу:
variationFunction <- function(x) {
  x_mean <- sum(x)/length(x)
  x_minus_xMean <- x - x_mean
  x_minus_xMean_2 <- x_minus_xMean^2
  
  sum(x_minus_xMean_2) / (length(x) - 1)
}


#   Вариацията има функция в базовия пакет на R
variationFunction(basketball_teams$team1)
var(basketball_teams$team1)
var(basketball_teams$team2)

#       При тестването на хипотези и при определянето на доверителните интервали се използва 
#   стандардартното отклонение. Стандартното отклонение е производно на вариацяита и представлява
#   корен квадратен от дисперсията.

sqrt(variationFunction(basketball_teams$team1))
sd(basketball_teams$team1)
sd(basketball_teams$team2)

#       Правилото на Чебишев, което е валидно за всички множества, ни казва, че 89% от наблюденията
#   лежат в интервала (X_mean - 3*X_std; X_mean + 3*X_std), където X_mean - средната стойност и 
#   X_std - стандартното отклонение.
#       При камбановидна форма на разпределението, този процент достига до 99.7


N <- 10^4
set.seed(94171)
dist1 <- rnorm(N)
dist2 <- rgamma(N, 3)


par(mfrow = c(1, 2))
plot(density(dist1), lwd = 2, main = "Плътност", xlab = "Нормално разпределение", 
     ylab = "Плътност", col = "lightblue", xlim = range(dist1))
mu <- mean(dist1);  sigma <- sd(dist1)
abline(v = c(mu - 3*sigma, mu, mu + 3*sigma), lwd = 2, col = c("black", "red", "black"))

plot(density(dist2), lwd = 2, main = "Плътност", xlab = "Гама разпределение", 
     ylab = "Плътност", col = "lightblue", xlim = range(dist2))
mu <- mean(dist2);  sigma <- sd(dist2)
abline(v = mu + c(-3, 0, 3)*sigma, lwd = 2, col = c("black", "red", "black"))
par(mfrow = c(1, 1))

mu <- mean(dist2);  sigma <- sd(dist2)
round(sum(dist2 >= mu - 3*sigma & dist2 <= mu + 3*sigma)*100/N, 2)






par(mfrow = c(1, 2))
plot(density(dist1), lwd = 2, main = "Плътност", xlab = "Нормално разпределение", 
     ylab = "Плътност", col = "lightblue", xlim = range(dist1))
abline(v = fivenum(dist1), lwd = c(1.5, rep(2, 3), 1.5), col = c("black", "red", "red", "red", "black"), 
       lty = c(1, rep(3, 3), 1))

plot(density(dist2), lwd = 2, main = "Плътност", xlab = "Гама разпределение", 
     ylab = "Плътност", col = "lightblue", xlim = range(dist2))
abline(v = fivenum(dist2), lwd = c(1.5, rep(2, 3), 1.5), col = c("black", "red", "red", "red", "black"), 
       lty = c(1, rep(3, 3), 1))

par(mfrow = c(1, 1))


#            3.4. Interquartile Range и MAD
#      Тези два вида оценки на дисперсията е препоръчително да се използват, когато за оценка на центъра
#   на разпределението се използва медианата. И двете оценки се водят "стабилни" към екстремалните стойности


#       Nielsen Company е публикувала информация колко часа седмично американците прекарват пред телевизора.  
#   Това е извадка от 20 човека
tv_viewing_times <- c(25, 41, 27, 32, 43, 66, 35, 31, 15, 5, 34, 26, 32, 38, 16, 30, 38, 30, 20, 21)

#       За да покажем как екстремалните стойности влияят върху част от оценките ще добавим голяма стойност,
#   например 240 часа
tv_viewing_times_new <- c(tv_viewing_times, 240)



#   Интерквартилния обхват се изчислява като разлика между 3-ти квартил и 1-ви квартил
summary(tv_viewing_times)
summary(tv_viewing_times)[c(2, 5)]
diff(summary(tv_viewing_times)[c(2, 5)])

#   Базовата функция в R се казва IQR()
IQR(tv_viewing_times)



IQR(tv_viewing_times)
IQR(tv_viewing_times_new)
#   Както се вижда няма кой знае колко голяма промяна след добаяването на екстремалната стойност

#   Какво обаче би станало, ако използваме стандартното отклонение?
sd(tv_viewing_times)
sd(tv_viewing_times_new)
#   Разликата скача в пъти

#       Ето защо при наличието на екстремуми е по-разумно да използваме медианата за оценка на центъра и 
#   IQR или mad за оценка на дисперсията

#   MAD
#       Оценката MAD представлява медианата на вектора с абсолютните стойности от разлики от стойността и 
#   медианата на самия вектор. Резултатът е умножен по 1.4826
#   Формулата е записана по-долу
X_median <- median(tv_viewing_times_new)
X_median
X_diff <- abs(tv_viewing_times_new - X_median)
X_diff
median(X_diff)*1.4826

mad(tv_viewing_times_new)



#   - Добре, при оценката на вариацията имаме значима промяна. Как ли стоят нещата с оценките за центъра?
#   - Екстремумите оказват влияние и при оценката за центъра. Ето защо, при наличие на такива стойности,
#   предпочитаме да използваме медианата, вместо средната стойност.

mean(tv_viewing_times)
mean(tv_viewing_times_new)

median(tv_viewing_times)
median(tv_viewing_times_new)
#   Разликата е очевидна

#   - Между другото имаме и други опции при наличието на екстремални стойности - bootstrap метод и 
#   trimmed mean. За съжаление, няма да можем да се запознаем в курса с bootstrap, но ви го препоръчвам.

#       Какво прави trim опцията? Тя премахва по част от най-големите и най-малките стойности. 
#   В нашия случай, ние сме посочили, че искаме да махмен 5% от най-големите и най-малките стойности.
#   Тоест ще вземем 5/2 = 2.5% от най-малките стойности и 2.5 от най-големите.
mean(tv_viewing_times, trim = 0.05)
mean(tv_viewing_times_new, trim = 0.05)
#   Както виждаме стойностите са близки








#       4. Графично представяне на разпределение
#           4.1.    Barplot
#   Използваме barplot, когато искаме да представим честотното разпределение на категорийни променливи
set.seed(4012)
fruits <- sample(x = c("Apple", "Banana", "Blackberry", "Peach"), size = 40, replace = T, 
                 prob = c(0.4, 0.1, 0.3, 0.2))

tt <- table(fruits);    tt

barplot(height = tt)
barplot(height = tt, col = "seagreen3", main = "Barplot") 
?barplot
#   height - приема вектор или матрица с числови стойности като вход. Стойностите могат да бъдат и отрицателни
#   main - заглавие на графиката
#   col - цвят на стълбовете
#           Тези параметри са основни и ги има и при другите графики 

prop.table(tt)
barplot(prop.table(tt))



#           4.2.    Хистограма 
#   Използваме хистограма, когато искаме да представим разпределението на непрекъснати променливи
set.seed(7821)
r1 <- rnorm(n = 10^3, mean = 4, sd = 3)
hist(r1, main = "Хистограма (честотно разпределение)", xlab = "Нормално разпределение", ylab = "Честота",
     col = "tomato3", breaks = 20)
hist(r1, main = "Хистограма (вероятностно разпределение)", xlab = "Нормално разпределение", ylab = "Честота",
     col = "springgreen3", prob = T)

colors()    #   различни видове цветове, които се подържат от базовия пакет в R



#           4.3.    Piechart
#   Използваме piechart-, когато боравим с категорийни променливи и искаме да 
#   изобразим процентното им разпределение
cities <- c(rep("London", 14), rep("New York", 49), rep("Singapore", 28), rep("Mumbai", 36))
cities.table <- table(cities)


pie(cities.table)
pie(cities.table, main = "City pie chart", col = rainbow(length(cities.table)))

#       Броя на цветовете е хубаво да бъде равен на броя на категориите. В противен случай два сигмента
#   ще бъдат оцветени в един и същи цвят.

piepercent <- round(100*cities.table/sum(cities.table), 1)
piepercent <- round(100*prop.table(cities.table), 1)

pie(cities.table, labels = piepercent, main = "City pie chart", col = rainbow(n = length(cities.table)))
#   rainbow(n) - връща n на брой цветове, произтичащи от дъгата

legend(x = "topright", legend = names(cities.table), cex = 0.8,
       fill = rainbow(length(cities.table)))
?legend
#   x - разположение на графиката - може да слагате както координати, така и да описвате позицията
#   legend - имената на категориите
#   cex - големината на текста
#   fill - цвета, на който отговаря текста в легендата



#           4.4.    Boxplot
#   При едномерния анализ, boxplot-а се използва, за да откриване на потенциални outlier-и.
tv_viewing_times <- c(25, 41, 27, 32, 43, 66, 35, 31, 15, 5, 34, 26, 32, 38, 16, 30, 38, 30, 20, 21)
tv_viewing_times_new <- c(tv_viewing_times, 240)



boxplot(tv_viewing_times)

par(mfrow = c(1, 2))
boxplot(tv_viewing_times, col = "powderblue", main = "Boxplot", xlab = "TV viewing", ylab = "Hours")
boxplot(tv_viewing_times_new, horizontal = T, col = "palevioletred", main = "Boxplot", 
        xlab = "TV viewing + outlier")
par(mfrow = c(1, 1))


boxplot_obj <- boxplot(tv_viewing_times, col = "powderblue", main = "Boxplot", xlab = "TV viewing", ylab = "Hours")
boxplot_obj
summary(tv_viewing_times)

boxplot_obj <- boxplot(tv_viewing_times_new, col = "powderblue", main = "Boxplot", xlab = "TV viewing", ylab = "Hours")
boxplot_obj




#           4.5.    Q-Q plot
#       Проверяваме дали стойностите на наблюдаваната променлива се доближават до теоретичните 
#   стойности на някое разпределение.

emp <- c(19.14, 6.29, 17.02, 6.13, 1.63, 18.78, 9.43, 11.21, 2.89, 9.52, 9.49, 4.83, 13.26, -0.96, 
         5.12, 1.39, 6.76, 2.1, 4.32, 1.38, 10.7, 9.01, 4.73, 11.59, 7.22, 1.53, 8.36, 10.91, 6.49, 
         3.69, 2.06, 15.92, 16.76, 18.13, 10.22, 19.25, 9.65, 17.75, 2.52, 1.24, 18.51, 11.52, 14.67, 
         12.65, 11.22, 27.78, 1.76, 9.64, 11.42, 12.29)

set.seed(10)
d1 <- rnorm(n = 10^2, mean = mean(emp), sd = sd(emp))
d2 <- rcauchy(n = 10^2, location = mean(emp), scale = sd(emp))


shapiro.test(emp)
#       По-нататък в курса ще вземем тестване на хипотези. Към настоящият момент е достатъчно да
#   знаем, че ако стойността на p-value е по-малко от 0.05, то величината НЕ Е нормално разпределена.


par(mfrow = c(1, 2))
  qqplot_obj <- qqplot(y = emp, x = d1, xlab = "Theoretical distribution", 
                       main = "Check for normal distr", ylab = "Empirical")
  abline(a = 0, b = 1)

  #       Създаденият лист qqplot_obj съдържа два вектора - "x" и "y". И двата вектора, с големина  
  #   дължината на по-малкия вектор. Изобразените стойности на графиката при некратни дължини на  
  #   двата вектора могат да се различават съществено.
  sort_emp <- sort(emp)
  sort_d1 <- sort(d1)
  head(sort_emp)
  head(sort_d1)

  element_number <- 2
  emp_n <- length(emp); d1_n <- length(d1)
  if(emp_n == d1_n) {
    abline(h = sort_emp[element_number])
    abline(v = sort_d1[element_number])
  } else {
    COEF <- (max(emp_n, d1_n) - 1) / min(emp_n, d1_n)
  
    if(emp_n > d1_n) {
      abline(h = sort_emp[round(element_number*COEF - 1)])
      abline(v = sort_d1[element_number])
    } else {
      abline(h = sort_emp[element_number])
      abline(v = sort_d1[round(element_number*COEF - 1)])
    }
  }


  qqplot(y = emp, x = d2, xlab = "Theoretical distribution", main = "Check for cauchy distr", 
         ylab = "Empirical")
  abline(a = 0, b = 1)
par(mfrow = c(1, 1))

#   abline() - чертае права линия
#   a - изместване по Х, b - ъгъл на правата, v - вертикална линия, h - хоризонтална линия



#       В статистиката, много често се правят проверки за нормално разпределение. Ето защо, в R 
#   имаме базова функция, която извършва тази проверка.
set.seed(223)
d3 <- rnorm(200, 3, 15)
shapiro.test(d3)


qqnorm(d3); qqline(d3)


#       По-добра алтернатива е използването на долния код. На графиката се построяват граници,
#   които помагат при определянето дали разпределението е нормално. Ако всички наблюдения
#   попадат в сивата зона, то спокойно можем да твърдим, че разпределението е нормално
ggplot(data = data.frame(emp = d3), mapping = aes(sample = emp)) + stat_qq_band() + 
  stat_qq_line() + stat_qq_point() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")


