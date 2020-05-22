# Функция возвращает датафрейм с наблюдениями только для указанных генов.

test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"),
                                expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names <- c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
my_names <- function (dataset, names){
  matches <- paste(names, collapse = "|")
  dataset[grepl(matches, dataset[[1]]), ]
}

my_names(test_data, names)

# В этой задаче мы хотели бы вытащить из каждого списка только значение p - value.
# То есть получить новый список или вектор только с одним показателем (p - value) для каждой переменной.

normality_tests_p <- lapply(iris[, 1:4], shapiro.test)
s2 <- sapply(normality_tests, '[')
s2[2,]

# Напишите функцию one_sample_t, которая получает на вход два аргумента:
# 1. Dataframe произвольного размера с произвольным числом переменных различного типа.
# 2. Числовое значение среднего в генеральной совокупности.
# Ваша функция должна применять одновыборочный t - test к каждой числовой переменной в данных, и сравнивать среднее
# значение этой переменной с указанным значением среднего в генеральной совокупности (второй аргумент функции).
# Функция должна возвращать список, где каждый элемент это вектор, состоящий из t - значения, числа степеней свобод
# (df) и значения p - value.

one_sample_t <- function(test_data, general_mean){
  tested <- test_data[sapply(test_data, is.numeric)]
  res <- lapply(tested, function(x) c(t.test(x,mu=general_mean)$statistic,t.test(x,mu=general_mean)$parameter, t.test(x,mu=general_mean)$p.value))
  return(res)
}

one_sample_t(iris[, 1:4], 4)

