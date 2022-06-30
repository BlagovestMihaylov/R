ptu <- c("gplots", "ggplot2", "corrplot")
IP <- setdiff(ptu, row.names(installed.packages()))
if(length(IP) > 0) {for(el in IP) {install.packages(el)}}
lapply(X = ptu, FUN = library, character.only = TRUE)




#           N-����� ���������� � ���������� �� �������� ����� ���


#       ����� ������������ "n"-������� �����? 
#       ����������� �����, ���� �� ������/����� �� ������ (�����, ���������, ����, ���� ��� ������).
#   ��� ���������� ����� ����� �������� �� ��������� �����. �����, ������������� � ��� ������� �� 
#   �������, data frame-��� ��� ����� ������� ���������, ��� ����� ���-����� �� ������ �� �����������
#   ���������/����������, � �� ������ ������� �������� (������������).

#       ������ �� ���������� ����� �
data("mtcars")
head(mtcars)


#       ���� �� ���������� ����� �� ��������� �� ���������� ����. ����� �� �������� ����������
hist(x = mtcars$disp, col = "red", xlab = "Displacement (u.in.)", main = "Histogram")
summary(mtcars$disp)
sd(mtcars$disp)

#   ������������ ��������: 
#     - ������������ ����������: ������ �������� +/- ��������� * ���������� ����������
#         ����� �� ���� �� �������� (���������� �� ������) - alpha
#         ��������� - ������� �� 1 - alpha/2
#       ������:
#         ���� �� �������� = 5% => alpha = 0.05
#         ��������� = ������� �� 1 - 0.05/2 = 0.975
#         ��������� = qnorm(0.975) ~ 2
#         ��: (������ �������� - 2*���������� ����������; ������ �������� + 2*���������� ����������)
#
#
#     - �������������� ����������: ���������� �� alpha/2 � 1 - alpha/2
#       ������:
#       ���� �� �������� = 5% => alpha = 0.05
#       ��: (������� �� 0.025; ������� �� 0.975)



mean(mtcars$disp) + c(-1, 1)*2*sd(mtcars$disp)
alpha <- 0.05
quantile(mtcars$disp, probs = c(alpha/2, 1 - alpha/2))


abline(v = mean(mtcars$disp), lwd = 2, lty = 4)
abline(v = median(mtcars$disp), lwd = 2, lty = 3, col = "blue")
#       �� ������������ �� �����, �� ����� ��� ����. �����, ��������������� �� ������������ �
#   ���������. ������� ����������� ���������� ����� ������� ���� �� ������ �������� ��������,
#   � ������ ���������� - ��������. � � ����� ������, ����� ������ ����� �� �������, �� ���� �
#   ���������� �� ���������������.
#       ���� ���� �� ��������, ����� �� �������, ��� ��������� ������� �� ���� �� ����������


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


#     �� ���������
summary(mtcars$disp)
sd(mtcars$disp)


#       �� ������� �� �����������, ����� ���� 4 ���������, ��� ��� �� ���� �� ������� ����� ������ 
#   �� ����������, no �� ������� ��� ����� - �����, ������ ����� �� ���� ����. 
#       ������������ ������������� �� ���� ���������� �� ����� ����������, ��� �������� �� �������� 
#   �������� �� �����������, ����� �� �� ��������. �� ���� ����� ������ ���������� �� ��-�����. 




#               ���������� �� �������� �����

#       1. ����������� (����������) VS ����������� (��������)
#   �������� ����� ���� ���������� ���-����� �� ������ � ������� �� cross ������� � barplot-���.

#      ������: ��������� ��� ����������� ���������, ����� ������� ���� ����������, ����� �����,
#   ���� ��-�����.

smokes <- c("Y", "N", "N", "Y", "N", "Y", "Y", "Y", "N", "Y")
amount <- c("0 - 5 hours", "5 - 10 hours", "5 - 10 hours", "more than 10 hours", 
            "more than 10 hours", "0 - 5 hours", "5 - 10 hours", "0 - 5 hours", 
            "more than 10 hours", "5 - 10 hours")
table(amount, smokes)
#       ������� ��������, �� �������� ���� ��-����� �� ����������. ���� �� ���������� ����������
#   �� ���� �������, � ���� ��������. �� ����� ��������� ���������

prop.table(x = table(amount, smokes))
#   ������� �� � ��� �����, ����� �������� �� ������� �������.

prop.table(x = table(amount, smokes), margin = 1)
#   ����������� "margin" ������ ��� ������ �� ����������� ���������� - �� ������ ��� �� ������.
#   �� ������� �������, �� ����� ���������� � �������� �� ���������� ����������, ������ ���� ��
#   ��������, ����� ������� �� �����.

#   ���� �� ���������� ���������� ����������� �� �������.
barplot(table(smokes, amount))
#       ����� ������ ����� ������ ���������, ����� ��� �� �� ���������. 
#       � ������ ��� �� �� ������� �� ������������� ����������� ���� ���������� ����������� 
#   �����������. ��� ���� ������, ���� �� ����� ��������������� �� ���� �������� � ���� �����,
#   ������ �����.
barplot(prop.table(x = table(smokes, amount), margin = 2))


#       ���� ������ � ��������� ��-����
barplot(table(smokes, amount), beside = TRUE, legend.text = T)
#      ��� ���� ������ ���� ����� �� ���������� ��������� � ��������� �����. � �������� barplot
#   ��� ������ � �������

#       ����� �� ����� �� ������� ������� �� ��������� (legend.text = TRUE), �� ����� � �� �
#   �������� ��� ���������, ����� �� �������. ����������� � �������� � ������� ��-����.
barplot(table(amount, smokes), main = "table(amount, smokes)", beside = TRUE,
        legend.text = c("less than 5", "5 - 10", "more than 10"))



#       2. ����������� (����������) VS ������� (��������)
#      ������ ����� ������ ������������ ��� ��������, �� ���-������ � �� ���������� One-way ANOVA
#   � t-test ��� ������� �������������� �����������. ���� ������� �� �� ���� ��-������� � ����� 
#   �� ����������. ��� ������ �� �� ���������� ��������, ������ ������� � boxplot, plotmeans,
#   � violin ���������.


#       � ����� ������, ���-������� ������� � �� �� �������� boxplot, ��� ���� �� �� �������
#   �� �� ��������� ������� �� �������� �������������
N <- 200
set.seed(122)
amount_group1 <- round(rnorm(n = N, mean = 5, sd = 1.3), 3)
amount_group2 <- round(rnorm(n = N, mean = 3, sd = 1.3), 3)
amount <- c(amount_group1, amount_group2)
category <- as.factor(c(rep(1, N), rep(2, N)))
tt <- boxplot(amount ~ category, horizontal = TRUE, col = "forestgreen")
#      ����� �� �����, ����� ����� �� �� ������� ����� ���������. �������� ������ ����� ��� �����
#   ���� boxplot � ���������, � �������� �� ������������� �� 1 � 3-�� �������. ��������� �� ��������
#   �� ����������� ������������� ���������, ���� �� ��������� ������������� outlier-�.
#       ����� ��������������� �� ���� ������� �, �� ����������� �� ������� ����� ���� ���� �� 
#   ��-������, ������ � ��������� � ������ ������� �� ������� ����� �� ��-������ �� 3-��� �������
#   �� ������� �����.
#   �������, ��������� � ������ ������� �� ������� ����� �� �-����� �� ������ ������� �� ������� �����.


ggplot(data.frame(category, amount), aes(x = category, y = amount, fill = category)) + 
  geom_violin(trim = FALSE) + coord_flip() +
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
  labs(x = "Category", y = "Amount") + ggtitle(label = "Violin plot")
#       "Violin plot" ������� ������������ ������������� �� ������������ "Amount", ������
#   ����� ���������.




#       ��� ����������� �� ������ ������� ����� �� �������� ������������, �� ��-���������� � 
#   �� �� �������� plotmeans.
#       �� �����, ����� �� �������� ���� ����� ����� �� �������� ������������, �����������
#   Q-Q �������
par(mfrow = c(1, 2))
  qqnorm(amount_group1);  qqline(amount_group1)
  qqnorm(amount_group2);  qqline(amount_group2)
par(mfrow = c(1, 1))
#   � ����� ����� �� �������� ������������. ������������ ����� �� ���������� plotmeans �������.




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




#       3. ������� (����������) VS ����������� (��������)
#   ���� ������ �� ������ � ������ � ������. ������ ��� ���� ����� �� ���������� One-way ANOVA ��� 
#   t-test, ���������������� �� ����������� � boxplot-���. 




#       4. ������� (����������) VS ������� (��������)
#       ���� � ���� �� �������, �� ����� ����������� ���-����� ������� �� �������. ������������ �� 
#   ������ ��� ����� �� ����� ���������� � ����������� � ������������ �� ��� ����� ������� ������
#   �������. ����, ������� ��, �� ������ �� ������ �� ����������, �� � ���������� ������ � ��-���������
#   ������ ��-�������� ���������� �� ��������.
#       ���������, ����� �� ���������� �� ������������ �� ���� ��� ������ �� ���-����� ������������ 
#   ������ � ����������� ������, ����� � dotplot (�������� ����������� �� ��������).
#       ����� ������ ������������ �� ���� ��� ������ ������ ������������������ dotplot, ������������
#   ������ � ����������� ������.


#       �� ���������� ������ �� ������� "mtcars". ����������� �� ������������ disp (���� �� ���������)
#   � wt (�����)
plot(mtcars$disp, mtcars$wt, xlab = "Displacement (cu.in.)", ylab = "Weight (1000 lbs)")

#       �� ��������� �� �����, �� ���������� ����������� ������� ������. ����� � ���������� �� �����
#   �� ���������, �������� � ������� �� ����������. ������������, ����� �� ���������� ������ �����,
#   �� �� ���������� ��������. ���� � �������� ���� ������������ ���������� ������ �� ���� ���������
#   � ����� ��� ��� �� ? ���� ��������� �������������.


#       ����� �� ��������� � ������������� � ������������ ������, ���� �� ���������� ���� ������. ����
#   ��� ������� �� ����� ����������. � �������� ���� �� ���� �������, � �������.

set.seed(4455)
x <- runif(1000, -3, 3)
y <- x^3 - 3 + rnorm(length(x), sd = 2)
plot(x, y)
#       ����� �� ����� �� ���������, ���� ��� ������ �� ������� �� �������. �� ���� ��������� �������������,
#   �������� ���� �� �� ��������� ���� �������. ��������, ��� �������� ���� ����������
x3 <- x^3
#   ������, ������ ���������� x3 � � ������� ���������� � ������������ y
par(mfrow = c(1, 2))
plot(x, y)
plot(x3, y)
par(mfrow = c(1, 1))





#           ������������ ������
#       �������������� ������ ������� ������ �� ������� ������ ����� ��� ����������. ������������ �� 
#   ��������� (rho) ���������� �� ��������� [-1, 1]. ������ �� �������� �� �������� �� ����������� 
#   �������� �� rho. �������, �� ������ �� �������� � ����������, ��� ��� ����� �� ��������� ������� 
#   ����.


N <- 4000
set.seed(3654)
x1 <- runif(N)


#       abs(rho) = 1 - ��������������� ������ (y = f(x)). �� ���� �������� �� x ����� ����� ����  
#   ���������� �������� �� y
y1 <- 3*x1 + 4
#       0.9 <= abs(rho) < 1 - ����� ����� ��������� �� ����� x �y
y2 <- 3*x1+ 4 + rnorm(N, sd = 0.2)
#       0.75 <= abs(rho) < 0.9 - ����� ��������� �� ����� x � y
y3 <- -3*x1 + 4 + rnorm(N, sd = 0.5)
#       0.5 <= abs(rho) < 0.75 - ������ ��������� �� ����� x � y
y4 <- -3*x1 + 4 + 1*rnorm(N)
#       0 < abs(rho) < 0.5 - ����� ��������� �� ����� x � y
y5 <- 3*x1 + 4 + 3*rnorm(N)
#       0 == abs(rho) - ������ ���������
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

#       �� ��������� �� �����, �� ����� ��-����������� �� ������������ ����� ���������, 
#   �����, ������� ��-����� � �����������.

#       ��������� �� ��������� � cor. � ��� ���� �� �� ��������� ����� �������� �����
#   ��� ����������, ���� � �������� ����� N-����� ������� �����.

#   ��������� �� ��������� �� � ������ � ������� ��-����
X <- x1;    Y <- y3


X_mean <- mean(X);  Y_mean <- mean(Y)
XY <- (X - X_mean)*(Y - Y_mean)
XX <- (X - X_mean)^2;   YY <- (Y - Y_mean)^2
sum(XY)/sqrt(sum(XX)*sum(YY))   #   ���������� �� �����������


cor(x1, y3)   #       ����� �� ���� ���� ����� - ����������� ����� ����� ����������


plot(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, y = mtcars$hp)  


#       ��������, ����� ������ �� �������� ����������� �� ��������� �� ����������
(cor_M <- cor(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]))
corrplot(corr = cor_M)


#       ����� ���������� ������� (A[i, j] == A[j, i]) � ����������� ����� ��������� 
#   ����������.
#       ��������� �, �� �� ������� ��������, ������ ��������� �� �������. ���� �
#   ��������� �� ���������, �� ������ ����������� �� "y", �������� ����������� �� "x".
#       ������������ �� ��������� "corrplot", ����� �������� �� ������� �������� 
#   ����� ��������� ����������


#       ����������� ��� ������� ���� ��������� - Pearson, Spearman � Kendall. �������
#   ��������� � ������������ ������ �� �������� ����� ��� ����������, ������ ����������
#   ��� - ��������������.
#       ����� ����������� �� Pearson � ��-�����, �� � ����������� ��� ��������� �� outlier-�
#       ���������� ��� ��������� �� ��-�������� � �� ������� �����.

#       ���-����� ���� �� �� ������������� � ������� ��-����


CEX <- 0.8

set.seed(4413)
x <- sort(rnorm(200, mean = 2, sd = 1))
y <- x + sqrt(1 - 0.8^2)*rnorm(length(x))



par(mfrow = c(2, 2))
  plot(x, y, main = "������� � �������������� �����", pch = 20, cex = CEX) #   ����������� � 0.85
  abline(lm(y ~ x), col = "forestgreen", lwd = 2, lty = 4)
  legend("topleft", 
         legend = c(paste0("Pearson's rho:", round(cor(x, y), 2)),
                    paste0("Spearman's rho: ", round(cor(x, y, method = "spearman"), 2)))
  )
  
  
  #   ���� ����� �� ������� ������� outlier-�
  x_outlier1 <- c(2.5, 2.1, 2.9, 2.6, 3.1, 3.2)
  y_outlier1 <- c(8.5, 9.5, 10.2, 10, 11, 12)
  x1 <- c(x, x_outlier1)
  y1 <- c(y, y_outlier1)
  plot(x1, y1, main = "������� � �������� \"outlier\"-�", pch = 20, cex = CEX) 
  points(x_outlier1, y_outlier1, col = "red", pch = 20, cex = CEX)
  abline(lm(y1 ~ x1), col = "red", lwd = 2, lty = 3)
  abline(lm(y ~ x), col = "forestgreen", lwd = 1, lty = 4)
  legend("topleft", 
         legend = c(paste0("Pearson's rho:", round(cor(x1, y1), 2)),
                    paste0("Spearman's rho: ", round(cor(x1, y1, method = "spearman"), 2)))
         )
  
  
  #   ���� ����� �� ������� ��� ������� outlier-�
  x_outlier2 <- c(-5.4, -7, -8, 6, 5, 8)
  y_outlier2 <- c(10, 12, 11.3, -11, -10.5, -11)
  x2 <- c(x1, x_outlier2)
  y2 <- c(y1, y_outlier2)
  plot(x2, y2, main = "������� � �������� ��� \"outlier\"-�", pch = 20, cex = CEX) 
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





