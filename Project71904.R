#1

watching <- c("Да, постоянно","Рядко","Рядко","Само когато имам време","Рядко","Само когато имам
време","Да, постоянно","Да, постоянно","Рядко","Да, постоянно","Само когато имам
време","Рядко","Само когато имам време","Само когато имам време","Само когато имам
време","Само когато имам време","Да, постоянно","Рядко","Само когато имам време","Само когато
имам време","Само когато имам време","Рядко","Само когато имам време","Само когато имам
време","Да, постоянно","Рядко","Само когато имам време","Рядко","Да, постоянно","Да,
постоянно","Рядко","Само когато имам време","Да, постоянно","Само когато имам време","Само
когато имам време","Само когато имам време","Само когато имам време","Да, постоянно","Само
когато имам време","Само когато имам време","Само когато имам време","Само когато имам
време","Рядко","Да, постоянно","Рядко","Рядко","Само когато имам време","Само когато имам
време","Само когато имам време","Да, постоянно","Само когато имам време","Само когато имам
време","Само когато имам време","Само когато имам време","Само когато имам време","Само
когато имам време","Рядко","Само когато имам време","Само когато имам време","Да,
постоянно","Да, постоянно","Само когато имам време","Да, постоянно","Да, постоянно","Само когато
имам време","Само когато имам време","Рядко","Само когато имам време","Само когато имам
време","Рядко","Да, постоянно","Само когато имам време","Да, постоянно","Рядко","Само когато
имам време","Само когато имам време","Само когато имам време","Рядко","Да, постоянно","Да,
постоянно")

table_watching <- table(watching)

prop_table_watching <- prop.table(table_watching)

barplot(height = prop_table_watching, col = rainbow(5), main = "Гледаш ли филми?")

piepercent_watching <- round(100*table_watching/sum(table_watching), 1)
pie(table_watching, labels = piepercent_watching, main = " Гледаш ли филми?", col =
      rainbow(n = length(table_watching)*2))
legend(x = "bottomleft", legend = c("Да, постоянно","Само когато имам време","Рядко","Не"),
       cex = 0.8, fill = rainbow(length(table_watching) *2
                                 
                                 
                                 # 2
                                 
                                 hours_watching <-
                                   c(3,6,6,5,5,1,5,5,2,5,2,1,2,3,4,2,4,1,3,3,2,3,3,2,5,1,3,1,6,3,3,4,2,4,4,2,3,6,3,4,3,2,2,4,2,2,4,2,2,2,2,2,4,5,2,3,2,3,1,4,3,2,6,
                                     3,2,2,1,8,2,2,2,1,2,1,2,3,3,2,8,5)
                                 
                                 modeFunction <- function(x) {
                                   res_table <- table(x)
                                   return(names(res_table)[res_table == max(res_table)])
                                 }
                                 modeFunction(hours_watching)
                                 
                                 summary(hours_watching)
                                 
                                 var(hours_watching)
                                 
                                 sd(hours_watching)
                                 
                                 hist(hours_watching, main = "По колко часа на ден в интервала 0 - 24 отделяш за гледане на
филми?", xlab = "Часове", ylab = "Честота", col = "gold1", prob = T)
                                 abline(v = mean(hours_watching), lwd = 2, lty = 4, col = "blue")
                                 abline(v = median(hours_watching), lwd = 2, lty = 3, col = "red")
                                 
                                 boxplot(hours_watching, col = "lightcoral", main = "По колко часа на ден в интервала 0 - 24
отделяш за гледане на филми?",xlab = "hours_watching")
                                 
                                 # 3
                                 
                                 movies_or_serials <- c("Сериали","Сериали","Филми","Сериали","Филми","Сериали","Сериали","И
двете","Филми","И двете","И двете","Сериали","Филми","И двете","И двете","Филми","И двете","И
двете","Сериали","И двете","И двете","Сериали","И двете","И двете","И двете","Филми","И
двете","Филми","Филми","И двете","И двете","Филми","Филми","Филми","Филми","И двете","И
двете","Филми","И двете","И двете","Филми","Сериали","И
двете","Филми","Сериали","Филми","Филми","Филми","Филми","Филми","Филми","Филми","И двете","И
двете","И двете","Филми","И двете","И двете","Филми","Филми","Филми","И двете","И
двете","Филми","Сериали","Сериали","И двете","Филми","И двете","Филми","И двете","И двете","И двете","И
двете","Филми","Филми","Филми","Филми","Сериали","Филми")
                                 
                                 table_movies_or_serials <- table(movies_or_serials)
                                 
                                 prop_table_movies_or_serials <- prop.table(table_movies_or_serials)
                                 
                                 barplot(height = table_movies_or_serials, col = rainbow(5), main = " Какво предпочиташ?")
                                 
                                 piepercent_movies_or_serials <- round(100*table_movies_or_serials/sum(table_movies_or_serials),
                                                                       1)
                                 pie(table_movies_or_serials, labels = piepercent_movies_or_serials, main = "Какво предпочиташ?",
                                     col = rainbow(n = length(table_movies_or_serials)*2))
                                 legend(x = "bottomleft", legend = c("И двете","Сериали","Филми"), cex = 0.8, fill =
                                          rainbow(length(table_movies_or_serials)*2))
                                 
                                 # 4
                                 
                                 what_are_movies <- c(rep("Време за релакс",64),rep("Възможност да науча нещо
ново",22),rep("Загуба на време",8))
                                 
                                 table_what_are_movies <- table(what_are_movies)
                                 
                                 prop_table_what_are_movies <- prop.table(table_what_are_movies)
                                 
                                 barplot(height = table_what_are_movies, col = rainbow(5), main = "Какво е за теб гледането на
филми?")
                                 
                                 piepercent_what_are_movies <- round(100*table_what_are_movies/sum(table_what_are_movies), 1)
                                 pie(table_what_are_movies, labels = piepercent_what_are_movies, main = "Какво е за теб гледането на филми?", 
                                     col = rainbow(n = length(piepercent_what_are_movies)*2))
                                 
                                 legend(x = "bottomleft", legend = c("Време за релакс","Възможност да науча нещо ново","Загуба на време"), cex = 0.8,
                                        fill = rainbow(length(table_watching)*2))
                                 
                                 
                                 # 5
                                 
                                 movies_count <-
                                   c(4,9,9,7,6,2,6,5,2,20,2,1,2,3,4,2,6,1,2,6,1,4,4,5,9,1,2,2,11,5,2,5,3,5,5,3,3,10,5,5,7,5,3,8,2,2,10,3,3,2,3,2,7,10,5,7,1,5,2,6,
                                     5,3,12,5,3,10,3,10,2,2,4,1,7,3,4,4,3,4,12,9)
                                 
                                 modeFunction <- function(x) {
                                   res_table <- table(x)
                                   return(names(res_table)[res_table == max(res_table)])
                                 }
                                 modeFunction(movies_count)
                                 
                                 summary(movies_count)
                                 
                                 var(movies_count)
                                 
                                 sd(movies_count)
                                 
                                 hist(movies_count, main = " Обикновено колко филми (в интервала 1- 20) гледаш на
седмица?",xlab = "Брой филми", ylab = "Честота", col = "red2", prob = T)
                                 abline(v = mean(movies_count), lwd = 2, lty = 4, col = "blue")
                                 abline(v = median(movies_count), lwd = 2, lty = 3, col = "black")
                                 
                                 boxplot(movies_count, col = "lightgreen", main = " Обикновено колко филми (в интервала 1- 20)
гледаш на седмица?", xlab = "Брой филми")
                                 
                                 # 6
                                 
                                 genre_movies <-
                                   c(rep("Комедия",50),rep("Екшън",28),rep("Трилър",19),rep("Ужаси",21),rep("Драма",23),rep("Ром
античен",36),rep("Фентъзи",17),rep("Анимация",16),rep("Семеен",18),rep("Приключенски",29))
                                 
                                 table_genre_movies <- table(genre_movies)
                                 
                                 prop_table_genre_movies <- prop.table(table_genre_movies)
                                 
                                 barplot(height = table_genre_movies, col = rainbow(15), main = "Кой е любимият ти жанр филми?")
                                 
                                 piepercent_genre_movies <- round(100*table_genre_movies/sum(table_genre_movies), 1)
                                 pie(table_genre_movies, labels = piepercent_genre_movies, main = "Кой е любимият ти жанр филми?", 
                                     col = rainbow(n = length(table_genre_movies)))
                                 
                                 legend(x = "bottomleft", legend = c("Анимация","Драма","Екшън","Комедия","Приключенски",
                                                                     "Романтичен","Семеен","Трилър","Ужаси","Фентъзи"), cex = 0.8,
                                        fill = rainbow(length(table_genre_movies)))
                                 
                                 # 7
                                 
                                 ideal_movie_length <-
                                   c(20,30,120,100,45,20,20,180,120,180,90,120,150,150,120,120,90,100,90,180,160,120,180,90,135,180,180,130,90,100,160
                                     ,169,120,105,105,160,80,20,80,120,180,60,100,120,90,120,105,120,120,110,80,120,150,180,180,120,82,30,90,150,90,70,1
                                     60,90,90,90,90,90,95,150,120,160,120,110,110,120,30,20,180,40)
                                 
                                 modeFunction <- function(x) {
                                   res_table <- table(x)
                                   return(names(res_table)[res_table == max(res_table)])
                                 }
                                 modeFunction(ideal_movie_length)
                                 
                                 summary(ideal_movie_length)
                                 
                                 var(ideal_movie_length)
                                 
                                 sd(ideal_movie_length)
                                 
                                 hist(ideal_movie_length, main = "Колко дълъг (в интервала 20 - 180 минути) би бил идеалният
филм за теб?", xlab = "Минути", ylab = "Честота", col = "cadetblue2", prob = T)
                                 abline(v = mean(ideal_movie_length), lwd = 2, lty = 4, col = "blue")
                                 abline(v = median(ideal_movie_length), lwd = 2, lty = 3, col = "black")
                                 
                                 boxplot(ideal_movie_length, col = "thistle", main = "Колко дълъг (в интервала 20 - 180 минути) би бил идеалният филм за теб?",
                                         xlab = "Минути")
                                 
                                 # 8
                                 
                                 personal_device <-
                                   c(rep("Телевизор",50),rep("Компютър",37),rep("Таблет",5),rep("Смартфон",22))
                                 
                                 table_personal_device <- table(personal_device)
                                 
                                 prop_table_personal_device <- prop.table(table_personal_device)
                                 
                                 barplot(height = table_personal_device, col = rainbow(5), main = "На кое лично устройство гледаш
филми най-често?")
                                 
                                 
                                 piepercent_personal_device <- round(100*table_watching/sum(table_personal_device), 1)
                                 pie(table_personal_device, labels = piepercent_personal_device, main = "На кое лично устройство гледаш филми най-често?", 
                                     col = rainbow(n = length(table_watching)*2))
                                 
                                 legend(x = "bottomleft", legend = c("Компютър","Смартфон","Таблет","Телевизор"), cex = 0.8,
                                        fill = rainbow(length(table_personal_device)*2))
                                 
                                 # 9
                                 
                                 cinema <-
                                   c("Да","Понякога","Понякога","Понякога","Да","Не","Да","Понякога","Понякога","Не","Да","Понякога","Понякога","П
онякога","Понякога","Понякога","Понякога","Понякога","Понякога","Понякога","Понякога","Понякога","Понякога","П
онякога","Не","Понякога","Да","Понякога","Понякога","Да","Не","Понякога","Понякога","Да","Да","Понякога","Поняк
ога","Понякога","Да","Понякога","Не","Понякога","Понякога","Понякога","Понякога","Понякога","Да","Понякога","По
някога","Понякога","Да","Не","Понякога","Не","Понякога","Да","Понякога","Понякога","Понякога","Понякога","Не","
Понякога","Понякога","Не","Понякога","Понякога","Не","Понякога","Понякога","Понякога","Да","Да","Не","Не","Да","
Да","Да","Да","Понякога","Да")
                                 
                                 
                                 table_cinema <- table(cinema)
                                 
                                 prop_table_cinema <- prop.table(table_cinema)
                                 
                                 barplot(height = prop_table_cinema, col = rainbow(5), main = "Ходиш ли на кино?")
                                 
                                 piepercent_cinema <- round(100*table_cinema/sum(table_cinema), 1)
                                 pie(table_cinema, labels = piepercent_cinema, main = "Ходиш ли на кино?", col = rainbow(n =
                                                                                                                           length(table_cinema)))
                                 legend(x = "bottomleft", legend = c("Да","Не","Понякога"), cex = 0.8,fill =
                                          rainbow(length(table_cinema)))
                                 
                                 # 10
                                 
                                 pay_cinema <-
                                   c(15,10,25,6,20,15,15,10,10,15,10,10,20,10,15,20,6,15,20,15,35,30,10,12,40,15,40,40,20,12,15,20,12,15,10,15,10,40,15,15,
                                     13,10,10,15,13,13,9,25,20,10,20,10,10,22,15,10,10,14,10,20,5,8,20,30)
                                 
                                 modeFunction <- function(x) {
                                   res_table <- table(x)
                                   return(names(res_table)[res_table == max(res_table)])
                                 }
                                 modeFunction(pay_cinema)
                                 
                                 summary(pay_cinema)
                                 
                                 var(pay_cinema)
                                 
                                 sd(pay_cinema)
                                 
                                 hist(pay_cinema, main = " Каква сума обикновено заплащаш в киното?",
                                      xlab = "Сума", ylab = "Честота", col = "brown1", prob = T)
                                 abline(v = mean(pay_cinema), lwd = 2, lty = 4, col = "blue")
                                 abline(v = median(pay_cinema), lwd = 2, lty = 3, col = "black")
                                 
                                 
                                 boxplot(pay_cinema, col = "tan1", main = "Каква сума обикновено заплащаш в киното?",
                                         xlab = "Сума")
                                 
                                 # 11
                                 
                                 books_or_movies <-
                                   c("Не","Не","Да","Не","Да","Да","Да","Да","Да","Не","Не","Да","Да","Не","Не","Да","Не","Да","Не","Да","Да","Не","Да
","Да","Да","Да","Да","Да","Не","Да","Не","Да","Да","Не","Не","Не","Да","Да","Да","Да","Да","Да","Да","Не","Да","Не"
                                     ,"Не","Да","Не","Да","Не","Да","Не","Не","Да","Не","Да","Да","Да","Да","Да","Да","Не","Да","Не","Не","Да","Да","Да",
                                     "Не","Да","Да","Не","Да","Да","Не","Да","Да","Не","Да")
                                 
                                 table_books_or_movies <- table(books_or_movies)
                                 
                                 prop_table_books_or_movies <- prop.table(table_books_or_movies)
                                 
                                 piepercent_books_or_movies<- round(100*table_books_or_movies/sum(table_books_or_movies),
                                                                    1)
                                 pie(table_books_or_movies, labels = piepercent_books_or_movies, main = "Би ли заменил
гледането на филми за четене на книги?", col = rainbow(n = length(table_books_or_movies)*4))
                                 legend(x = "bottomleft", legend = c("Да","Не"), cex = 0.8,fill =
                                          rainbow(length(table_books_or_movies)*4))
                                 
                                 moviesDF <-
                                   data.frame(watching,hours_watching,movies_or_serials,movies_count,ideal_movie_length,cinema,b
                                              ooks_or_movies)
                                 
                                 # Числова VS числова
                                 
                                 plot(moviesDF$movies_count, moviesDF$hours_watching)
                                 
                                 rho <- round(cor(moviesDF$movies_count, moviesDF$hours_watching), 3)
                                 
                                 pairs(moviesDF[, c("hours_watching", "movies_count", "ideal_movie_length")])
                                 
                                 cor(moviesDF[, c("hours_watching", "movies_count", "ideal_movie_length")])
                                 
                                 model <- lm(movies_count ~ hours_watching, data=moviesDF)
                                 model
                                 
                                 summary(model)
                                 
                                 # Категорийна VS числова / Числова VS категорийна 
                                 
                                 hours_watching_vs_books_or_movies <- boxplot(moviesDF$hours_watching ~
                                                                                moviesDF$books_or_movies)
                                 
                                 said_yes <- moviesDF$hours_watching[moviesDF$books_or_movies == 'Да']
                                 said_no <- moviesDF$hours_watching[moviesDF$books_or_movies == 'Не']
                                 
                                 shapiro.test(said_yes) 
                                 shapiro.test(said_no)
                                 
                                 wilcox.test(hours_watching ~ books_or_movies, data = moviesDF, conf.int = TRUE, exact = FALSE)
                                 
                                 # Категорийна VS числова / Числова VS категорийна
                                 
                                 movies_count_vs_watching <- boxplot(moviesDF$movies_count ~ moviesDF$watching)
                                 
                                 aggregate(movies_count ~ watching, data = moviesDF, FUN = function(x) {shapiro.test(x)$p.value})
                                 
                                 bartlett.test(movies_count ~ watching, data = moviesDF)
                                 
                                 kruskal.test(movies_count ~ watching, data = moviesDF)
                                 
                                 pairwise.wilcox.test(moviesDF$movies_count, moviesDF$watching,
                                                      p.adjust.method = "BH", exact = FALSE)