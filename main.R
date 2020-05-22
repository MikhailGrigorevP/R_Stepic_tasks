# Функция возвращает датафрейм с наблюдениями только для указанных генов.

test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"),
                                expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names <- c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
my_names <- function (dataset, names){
  matches <- paste(names, collapse = "|")
  dataset[grepl(matches, dataset[[1]]), ]
}

my_names(test_data, names)