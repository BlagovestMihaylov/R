ptu <- c("gplots", "ggplot2", "corrplot")
IP <- setdiff(ptu, row.names(installed.packages()))
if(length(IP) > 0) {for(el in IP) {install.packages(el)}}
lapply(X = ptu, FUN = library, character.only = TRUE)




#           N-мерни променливи и изследване на връзките между тях


#       Какво предсталяват "n"-мерните данни? 
#       Едномерните данни, това са масиви/листа от обекти (числа, стрингове, дати, друг тип обекти).
#   При двумерните данни имаме колекция от едномерни данни. Тоест, представянето е във формата на 
#   матрици, data frame-ове или друга подобна структура, при която най-често по редове са представени
#   примерите/елементите, а по колони техните признаци (променливите).

#       Пример за многомерни данни е
data("mtcars")
head(mtcars)


#       Нека да изследваме обема на двигателя за въпроснтие коли. Първо ще построим хистограма
hist(x = mtcars$disp, col = "red", xlab = "Displacement (u.in.)", main = "Histogram")
summary(mtcars$disp)
sd(mtcars$disp)

#   Довереителен интервал: 
#     - параметрична статистика: средна стойност +/- КОНСТАНТА * стандартно отклонение
#         избор на ниво на съгласие (вероятност за грешка) - alpha
#         КОНСТАНТА - квантил от 1 - alpha/2
#       Пример:
#         ниво на съгласие = 5% => alpha = 0.05
#         КОНСТАНТА = квантил от 1 - 0.05/2 = 0.975
#         КОНСТАНТА = qnorm(0.975) ~ 2
#         ДИ: (средна стойност - 2*стандартно отклонение; средна стойност + 2*стандартно отклонение)
#
#
#     - НЕпараметрична статистика: квантнтили на alpha/2 и 1 - alpha/2
#       Пример:
#       ниво на съгласие = 5% => alpha = 0.05
#       ДИ: (квантил от 0.025; квантил от 0.975)



mean(mtcars$disp) + c(-1, 1)*2*sd(mtcars$disp)
alpha <- 0.05
quantile(mtcars$disp, probs = c(alpha/2, 1 - alpha/2))


abline(v = mean(mtcars$disp), lwd = 2, lty = 4)
abline(v = median(mtcars$disp), lwd = 2, lty = 3, col = "blue")
#       От хистограмата се вижда, че имаме два пика. Тоест, разпределението на променливата е
#   бимодално. Черната вертикалана прекъсната линия показва къде се намира средната стойност,
#   а синята прекъсната - медината. И в двата случая, малко трудно можем да приемем, че тпва е
#   очакването на разпределението.
#       Нека сега да проверим, какво би станало, ако групираме данните по броя на цилиндрите


table(mtcars$cyl)

disp_cyl4 <- mtcars$disp[which(mtcars$cyl == 4)]
disp_cyl6 <- mtcars$disp[which(mtcars$cyl == 6)]
disp_cyl8 <- mtcars$disp[which(mtcars$cyl == 8)]


par(mfrow = c(2, 2))
hist(x = disp_cyl4, col = "red", xlab = "4 cylinders", main = "Histogram of displacement (u.in.)")
hist(x = disp_cyl6, col = "lightblue", xlab = "6 cylinders", main = "Histogram of displacement (u.in.)")
hist(x = disp_cyl8, col = "forestgreen", xlab = "8 cylinders", main = "Histogram of displacement (u.in.)")
par(mfrow = c(1, 1))


summary(disp_cyl4)
sd(disp_cyl4)

summary(disp_cyl6)
sd(disp_cyl6)

summary(disp_cyl8)
sd(disp_cyl8)
mean(disp_cyl8) + c(-1, 1)*2*sd(disp_cyl8)


#     За сравнение
summary(mtcars$disp)
sd(mtcars$disp)


#       За групата на двигетелите, които имат 4 цилиндъра, все още не може да получим добра оценка 
#   за очакването, no за другите две групи - можем, защото имаме по един връх. 
#       Анализирайки зависимостите на една променлива от други променливи, ние успяваме да подобрим 
#   оценките на параметрите, които са ни неогходи. По този начин правим прогнозите си по-точни. 




#               Изследване на двумерни данни

#       1. Категорийни (обясняващи) VS категорийни (зависими)
#   Връзките между тези променливи най-лесно се виждат с помощта на cross таблици и barplot-ове.

#      Пример: Направили сме хипотетично прочуване, което измерва дали студентите, които пушат,
#   учат по-малко.

smokes <- c("Y", "N", "N", "Y", "N", "Y", "Y", "Y", "N", "Y")
amount <- c("0 - 5 hours", "5 - 10 hours", "5 - 10 hours", "more than 10 hours", 
            "more than 10 hours", "0 - 5 hours", "5 - 10 hours", "0 - 5 hours", 
            "more than 10 hours", "5 - 10 hours")
table(amount, smokes)
#       Данните показват, че пушачите учат по-малко от непушачите. Нека да разгледаме резулатите
#   не като честоти, а като проценти. За целта изпозлвае командата

prop.table(x = table(amount, smokes))
#   Показва ни в коя група, колко процента от данните попадат.

prop.table(x = table(amount, smokes), margin = 1)
#   Параметърът "margin" задава как желаем да изчисляваме процентите - по редове или по колони.
#   От данните виждаме, че имаме нарастване в процента на непушещите студентите, спрямо броя на
#   часовете, които отделят за учене.

#   Сега ще разгледаме графичното представяне на данните.
barplot(table(smokes, amount))
#       Малко трудно бихме видяли разликите, освен ако не са фрапиращи. 
#       В долния код ще се опитаме да нормализираме стойностите като използваме процентните 
#   съотношения. При този подход, ясно се вижда превъзходствата на едни признаци в една група,
#   спрямо друга.
barplot(prop.table(x = table(smokes, amount), margin = 2))


#       Друг подход е описаният по-долу
barplot(table(smokes, amount), beside = TRUE, legend.text = T)
#      При този подход съще лесно се забелязват разликите в отделните групи. В сегашния barplot
#   сме задали и легенда

#       Освен че можем да изведем легенда на графиката (legend.text = TRUE), то можем и да я
#   попълним със стойности, които ни трябват. Попълването е показано в примера по-долу.
barplot(table(amount, smokes), main = "table(amount, smokes)", beside = TRUE,
        legend.text = c("less than 5", "5 - 10", "more than 10"))



#       2. Категорийни (обясняващи) VS числови (зависими)
#      Когато имаме такава конфигурация при връзките, то най-удачно е да използваме One-way ANOVA
#   и t-test или техните непараемтрични еквиваленти. Тези анализи ще ги учим по-нататък в курса 
#   по статистика. Ако искаме да ги изследваме графично, удачно решение е boxplot, plotmeans,
#   и violin графиките.


#       В общия случай, най-удачния вариант е да се използва boxplot, тъй като не се изисква
#   да се изследват групите за нормално разпределение
N <- 200
set.seed(122)
amount_group1 <- round(rnorm(n = N, mean = 5, sd = 1.3), 3)
amount_group2 <- round(rnorm(n = N, mean = 3, sd = 1.3), 3)
amount <- c(amount_group1, amount_group2)
category <- as.factor(c(rep(1, N), rep(2, N)))
tt <- boxplot(amount ~ category, horizontal = TRUE, col = "forestgreen")
#      Както се вижда, лесно могат да се сравнят двете категории. Средната дебела линия във всяки
#   един boxplot е медианата, а страните на правоъгълника са 1 и 3-ти квартил. Дължината на опашките
#   са минималната имаксималните стойности, като са изключени потенциалните outlier-и.
#       Тоест интерпретацията на тази графика е, че стойнсотите на първата група като цяло са 
#   по-големи, защото и медианата и третия квартил за първата група са по-големи от 3-тия квартил
#   на втората група.
#   Отделно, медианата и първия квартил на втората група са п-малки от първия квартил на първата група.


ggplot(data.frame(category, amount), aes(x = category, y = amount, fill = category)) + 
  geom_violin(trim = FALSE) + coord_flip() +
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
  labs(x = "Category", y = "Amount") + ggtitle(label = "Violin plot")
#       "Violin plot" показва плътностното разпределение на променливата "Amount", спрямо
#   двете категории.




#       Ако стойностите на всички отделни групи са нормално разпределени, то по-рационално е 
#   да се използва plotmeans.
#       За целта, първо ще проверим дали двете групи са нормално разпределени, използвайки
#   Q-Q графика
par(mfrow = c(1, 2))
  qqnorm(amount_group1);  qqline(amount_group1)
  qqnorm(amount_group2);  qqline(amount_group2)
par(mfrow = c(1, 1))
#   И двете групи са нормално разпределени. Следователно можем да използване plotmeans гфарика.




CI <- 0.95
alpha <- 1 - CI
DF <- data.frame(category, amount)
aggr_DF <- aggregate(formula = amount ~ category, data = DF, FUN = function(x) {c(mean(x), sd(x))})
aggr_DF <- data.frame(category = aggr_DF[, "category"], amount = aggr_DF[, "amount"][, 1],
                      sigma = qnorm(1 - alpha/2)*aggr_DF[, "amount"][, 2])


ggplot(data = aggr_DF, aes(x = category, y = amount, color = category)) + 
  geom_line(size = 3) + geom_point(size = 4) + 
  labs(x = "Category", y = "Amount", title = "Confidence Interval plot") + 
  geom_errorbar(aes(ymin = amount - sigma, ymax = amount + sigma), width = 0.1, size = 1)
rm(list = c("DF", "aggr_DF"))




#       3. Числови (обясняващи) VS категорийни (зависими)
#   Този случай на връзка е сходен с горния. Затова тук също можем да използваме One-way ANOVA или 
#   t-test, непараметричните им екваваленти и boxplot-ове. 




#       4. Числови (обясняващи) VS числови (зависими)
#       Това е може би групата, за която съществуват най-много похвати за анализи. Променливите от 
#   числов тип могат да бъдат превърнати в категорийни и следователно за тях важат горнтие типове
#   анализи. Това, разбира се, би довело до загуба на информация, но в определени случаи е по-подходящо
#   заради по-голямата стабилност на моделите.
#       Похватите, които са характерни за изследването на този тип връзки са най-често корелационен 
#   анализ и регресионен анализ, както и dotplot (графично представяне на връзката).
#       Почти винаги изследването на този тип връзка следва последователността dotplot, корелационен
#   анализ и регресионен анализ.


#       Да разгледаме пример от данните "mtcars". Интересуват ни променливите disp (обем на двигателя)
#   и wt (тегло)
plot(mtcars$disp, mtcars$wt, xlab = "Displacement (cu.in.)", ylab = "Weight (1000 lbs)")

#       От графиката се вижда, че съществува положителна линейан връзка. Тоест с нарастване на обема
#   на двигателя, нараства и теглото на автомобила. Следователно, можем да използваме линеен модел,
#   за да моделираме връзката. Друг е въпросът дали обясняващата променлива трябва да бъде приложена
#   в суров вид или да ? бъде приложена трансформация.


#       Преди да продължим с корелационния и регресионния анализ, нека да разгледаме друг пример. Този
#   път данните ще бъдат симулирани. И връзката няма да бъде линейна, а кубична.

set.seed(4455)
x <- runif(1000, -3, 3)
y <- x^3 - 3 + rnorm(length(x), sd = 2)
plot(x, y)
#       Както се вижда от графиката, този тип връзка не прилича на линейна. Но чрез подходяща трансформация,
#   връзката може да се представи като линейна. Например, ако създадем нова променлива
x3 <- x^3
#   Тогава, новата променлива x3 е в линейна зависимост с променливата y
par(mfrow = c(1, 2))
plot(x, y)
plot(x3, y)
par(mfrow = c(1, 1))





#           Корелационен анализ
#       Корелационният анализ измерва силата на линейна връзка между две променливи. Коефициентът на 
#   корелация (rho) принадлежи на интервала [-1, 1]. Силата на връзката се определя от абсолютната 
#   стойност на rho. Въпреки, че силата на връзката с субективна, все пак можем да определим някакви 
#   нива.


N <- 4000
set.seed(3654)
x1 <- runif(N)


#       abs(rho) = 1 - Детерминистична връзка (y = f(x)). За една стойност на x имаме точно една  
#   единствена стойност на y
y1 <- 3*x1 + 4
#       0.9 <= abs(rho) < 1 - Много силна корелация на между x иy
y2 <- 3*x1+ 4 + rnorm(N, sd = 0.2)
#       0.75 <= abs(rho) < 0.9 - Силна корелация на между x и y
y3 <- -3*x1 + 4 + rnorm(N, sd = 0.5)
#       0.5 <= abs(rho) < 0.75 - Средна корелация на между x и y
y4 <- -3*x1 + 4 + 1*rnorm(N)
#       0 < abs(rho) < 0.5 - Слаба корелация на между x и y
y5 <- 3*x1 + 4 + 3*rnorm(N)
#       0 == abs(rho) - Нулева корелация
y6 <- 0*x1 + 4 + 3*rnorm(N)


rho1 <- round(cor(x1, y1), 3)
rho2 <- round(cor(x1, y2), 3)
rho3 <- round(cor(x1, y3), 3)
rho4 <- round(cor(x1, y4), 3)
rho5 <- round(cor(x1, y5), 3)
rho6 <- round(cor(x1, y6), 3)



par(mfrow = c(2, 3))
  plot(x1, y1, main = paste("rho:", rho1), xlab = "X", ylab = "Y")
  abline(a = 4, b = 3, col = "red", lwd = 2)
  
  plot(x1, y2, main = paste("rho:", rho2), xlab = "X", ylab = "Y")
  abline(a = 4, b = 3, col = "red", lwd = 2)
  
  plot(x1, y3, main = paste("rho:", rho3), xlab = "X", ylab = "Y")
  abline(a = 4, b = -3, col = "red", lwd = 2)
  
  plot(x1, y4, main = paste("rho:", rho4), xlab = "X", ylab = "Y")
  abline(a = 4, b = -3, col = "red", lwd = 2)
  
  plot(x1, y5, main = paste("rho:", rho5), xlab = "X", ylab = "Y")
  abline(a = 4, b = 3, col = "red", lwd = 2)
  
  plot(x1, y6, main = paste("rho:", rho6), xlab = "X", ylab = "Y")
  abline(a = 4, b = 0, col = "red", lwd = 2)
par(mfrow = c(1, 1))

#       От графиките се вижда, че колко по-разпръснати са наблюденията около червената, 
#   права, толкова по-слаба е корелацията.

#       Функцията за корелация е cor. С нея може да се изследват както връзките между
#   две променливи, така и връзките между N-мерни ЧИСЛОВИ данни.

#   Формулата за коралация ще я опишем с примера по-долу
X <- x1;    Y <- y3


X_mean <- mean(X);  Y_mean <- mean(Y)
XY <- (X - X_mean)*(Y - Y_mean)
XX <- (X - X_mean)^2;   YY <- (Y - Y_mean)^2
sum(XY)/sqrt(sum(XX)*sum(YY))   #   Стойността на корелацията


cor(x1, y3)   #       Връща ни само едно число - корелацията между двете променливи


plot(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, y = mtcars$hp)  


#       Понякога, бихме искали да проверим корелацията на множество от променливи
(cor_M <- cor(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]))
corrplot(corr = cor_M)


#       Връща СИМЕТРИЧНА матрица (A[i, j] == A[j, i]) с корелациите между отделните 
#   променливи.
#       Интересно е, че по главния диагонал, всички стойности са единици. Това е
#   следствие от формулата, че вместо стойностите на "y", ползваме стойностите на "x".
#       Благодарение на функцията "corrplot", можем визуално да покажем връзките 
#   между отделнтие променливи


#       Съществуват три основни вида корелации - Pearson, Spearman и Kendall. Първата
#   корелация е параметрична оценка на връзката между две променливи, докато останалите
#   две - непараметрични.
#       Тоест корелацията на Pearson е по-точна, но е неустойчива при наличието на outlier-и
#       Останалите две корелации са по-стабилни и не толкова точни.

#       Най-лесно това ще го демонстрираме с примера по-долу


CEX <- 0.8

set.seed(4413)
x <- sort(rnorm(200, mean = 2, sd = 1))
y <- x + sqrt(1 - 0.8^2)*rnorm(length(x))



par(mfrow = c(2, 2))
  plot(x, y, main = "Графика с първоначалните данни", pch = 20, cex = CEX) #   корелацията е 0.85
  abline(lm(y ~ x), col = "forestgreen", lwd = 2, lty = 4)
  legend("topleft", 
         legend = c(paste0("Pearson's rho:", round(cor(x, y), 2)),
                    paste0("Spearman's rho: ", round(cor(x, y, method = "spearman"), 2)))
  )
  
  
  #   Нека обаче да добавим няколко outlier-а
  x_outlier1 <- c(2.5, 2.1, 2.9, 2.6, 3.1, 3.2)
  y_outlier1 <- c(8.5, 9.5, 10.2, 10, 11, 12)
  x1 <- c(x, x_outlier1)
  y1 <- c(y, y_outlier1)
  plot(x1, y1, main = "Графика с добавени \"outlier\"-и", pch = 20, cex = CEX) 
  points(x_outlier1, y_outlier1, col = "red", pch = 20, cex = CEX)
  abline(lm(y1 ~ x1), col = "red", lwd = 2, lty = 3)
  abline(lm(y ~ x), col = "forestgreen", lwd = 1, lty = 4)
  legend("topleft", 
         legend = c(paste0("Pearson's rho:", round(cor(x1, y1), 2)),
                    paste0("Spearman's rho: ", round(cor(x1, y1, method = "spearman"), 2)))
         )
  
  
  #   Нека обаче да добавим ОЩЕ няколко outlier-а
  x_outlier2 <- c(-5.4, -7, -8, 6, 5, 8)
  y_outlier2 <- c(10, 12, 11.3, -11, -10.5, -11)
  x2 <- c(x1, x_outlier2)
  y2 <- c(y1, y_outlier2)
  plot(x2, y2, main = "Графика с добавени ОЩЕ \"outlier\"-и", pch = 20, cex = CEX) 
  points(x_outlier1, y_outlier1, col = "red", pch = 20, cex = CEX)
  points(x_outlier2, y_outlier2, col = "purple", pch = 20, cex = CEX)
  abline(lm(y ~ x), col = "forestgreen", lwd = 1, lty = 4)
  abline(lm(y1 ~ x1), col = "red", lwd = 1, lty = 3)
  abline(lm(y2 ~ x2), col = "purple", lwd = 2, lty = 2)
  legend("bottomleft", 
         legend = c(paste0("Pearson's rho:", round(cor(x2, y2), 2)),
                    paste0("Spearman's rho: ", round(cor(x2, y2, method = "spearman"), 2)))
  )
par(mfrow = c(1, 1))
#text(x = 0.5, y = 19, labels = paste0("Pearson's rho:", round(cor(x1, y1), 2)))
#text(x = 0.5, y = 17, labels =  paste0("Spearman's rho: ", round(cor(x1, y1, method = "spearman"), 2)))





