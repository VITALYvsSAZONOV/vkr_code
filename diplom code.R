library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
library(car)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(openxlsx)
library(pscl)
library(erer)
library(mfx)
library(broom)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(DEoptim)
library(ROI.plugin.quadprog)
library(ggplot2)
library(corrgram)
library(xts)

#Загрузим и обработаем исходный датасет для кластеризации АМЕРИКАНСКИХ банков (000PCA.xlsx)

PCA_USA2010 <- filter(X000PCA_banks, Year == 2010)
PCA_USA2010 <- PCA_USA2010[,-c(1,2,11:15)]
rows2010 <- filter(X000PCA_banks, Year == 2010)
rownames(PCA_USA2010) <- rows2010$Company


#Корреляционная матрица для кластеризации в 2010 году:
c <- cor(PCA_USA2010)
corrplot(c)


#построим модель в пространстве главных компонент
PCA_USA_mod2010 <- PCA(PCA_USA2010)
fviz_eig(PCA_USA_mod2010, addlabels = TRUE)

m2010 <- PCA_USA_mod2010$var$coord
stargazer(m2010, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2010$var$cos2
rowSums(PCA_USA_mod2010$var$cos2)

corrplot(PCA_USA_mod2010$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2010$var$cos2, is.corr = FALSE)

#Исходя из визуализации матрицы нагрузок, можно дать следующую интерпретацию главным компонентам:
#1-я ГК - отвечает за величину банка: высокая значимая корреляция с капитализацией, кредитным портфелем и показателем EBITDA
#2-я ГК - отвечает за эффективность банка и потенциал роста кэша (и как следствие дивидендов): можем судить по значимой корреляции с FCF yield

#Строим дендрограмму и выделяем кластеры
clusters2010_USA <- HCPC(PCA_USA_mod2010)

usa2010 <- clusters2010_USA$data.clust
clust1 <- filter(usa2010, clust == "1")
clust2 <- filter(usa2010, clust == "2")
clust3 <- filter(usa2010, clust == "3")
clust4 <- filter(usa2010, clust == "4")

fviz_pca_ind(PCA_USA_mod2010, repel = TRUE, fill.ind = usa2010[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2010,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2010 году", label = "all",col.var = "darkblue",habillage=usa2010[,9])

#Мы можем четко выделить три глобальных кластера и мини-кластер из двух банков. Сначала глобальные кластеры.
#Интерпретируем кластеры, основываясь на первой главной компоненте. Кластер 4 - топовые американские банки.
#Кластер 3 - банки-среднего размера и крупные региональные игроки.
#Кластер 2 - мелкие банки, каких абсолютное большинство
#Кластер 1 - мелкие банки с низкой эффективностью (самые слабые): Synovus Financial Corp, United Community Banks Inc.
#По сути, их можно отнести к кластеру 3, так как они отличаются только эффективностью, в то время как эффективность глобальных кластеров равноценна

#_____________Нельзя исходить из предпосылки, что банки остаются в тех же кластерах и в следующие годы.
#Конечно, кластер крупных банков, вероятно не изменится, но ротацию в средних и мелких банках исключать нельзя.
#Для корректного построения регрессии, нужно "отследить" движение банков в пространстве главных компонент во времени.


##################    2011 год     ##################

PCA_USA2011 <- filter(X000PCA_banks, Year == 2011)
PCA_USA2011 <- PCA_USA2011[,-c(1,2,11:15)]
rows2011 <- filter(X000PCA_banks, Year == 2011)
rownames(PCA_USA2011) <- rows2011$Company

#Корреляционная матрица для кластеризации в 2011 году:
c <- cor(PCA_USA2011)
corrplot(c) #Интересна высокая корреляция между доле чистого непроцентного дохода и EBITDA c FCF yield

#построим модель в пространстве главных компонент
PCA_USA_mod2011 <- PCA(PCA_USA2011)
fviz_eig(PCA_USA_mod2011, addlabels = TRUE)

m2011 <- PCA_USA_mod2011$var$coord
stargazer(m2011, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2011$var$cos2
rowSums(PCA_USA_mod2011$var$cos2)

corrplot(PCA_USA_mod2011$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2011$var$cos2, is.corr = FALSE)
#Интерпретация ГК не изменилась ключевым образом в сравнении с 2010 годом

#Строим дендрограмму и выделяем кластеры
clusters2011_USA <- HCPC(PCA_USA_mod2011)

usa2011 <- clusters2011_USA$data.clust
clust1_2011 <- filter(usa2011, clust == "1")
clust2_2011 <- filter(usa2011, clust == "2")
clust3_2011 <- filter(usa2011, clust == "3")
clust4_2011 <- filter(usa2011, clust == "4")
#Теперь кластер самых слабых банков состоит только из United Community Banks Inc

fviz_pca_ind(PCA_USA_mod2011, repel = TRUE, fill.ind = usa2011[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2011,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2011 году", label = "all",col.var = "darkblue",habillage=usa2011[,9])


##################    2012 год     ##################

PCA_USA2012 <- filter(X000PCA_banks, Year == 2012)
PCA_USA2012 <- PCA_USA2012[,-c(1,2,11:15)]
rows2012 <- filter(X000PCA_banks, Year == 2012)
rownames(PCA_USA2012) <- rows2012$Company

#Корреляционная матрица для кластеризации в 2012 году:
c <- cor(PCA_USA2012)
corrplot(c) #отрицательная корреляция ROE и FCF yield

#построим модель в пространстве главных компонент
PCA_USA_mod2012 <- PCA(PCA_USA2012)
fviz_eig(PCA_USA_mod2012, addlabels = TRUE)

m2012 <- PCA_USA_mod2012$var$coord
stargazer(m2012, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2012$var$cos2
rowSums(PCA_USA_mod2012$var$cos2)

corrplot(PCA_USA_mod2012$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2012$var$cos2, is.corr = FALSE)
#Вторая ГК отвечает за эффективность по-прежнему, но ROE поменяла направление

#Строим дендрограмму и выделяем кластеры
clusters2012_USA <- HCPC(PCA_USA_mod2012)

usa2012 <- clusters2012_USA$data.clust
clust1_2012 <- filter(usa2012, clust == "1")
clust2_2012 <- filter(usa2012, clust == "2")
clust3_2012 <- filter(usa2012, clust == "3")
#Теперь 3 кластера


fviz_pca_ind(PCA_USA_mod2012, repel = TRUE, fill.ind = usa2012[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2012,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2012 году", label = "all",col.var = "darkblue",habillage=usa2012[,9])


##################    2013 год     ##################

PCA_USA2013 <- filter(X000PCA_banks, Year == 2013)
PCA_USA2013 <- PCA_USA2013[,-c(1,2,11:15)]
rows2013 <- filter(X000PCA_banks, Year == 2013)
rownames(PCA_USA2013) <- rows2013$Company

#Корреляционная матрица для кластеризации в 2013 году:
c <- cor(PCA_USA2013)
corrplot(c) #как в 2011 году

#построим модель в пространстве главных компонент
PCA_USA_mod2013 <- PCA(PCA_USA2013)
fviz_eig(PCA_USA_mod2013, addlabels = TRUE)

m2013 <- PCA_USA_mod2013$var$coord
stargazer(m2013, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2013$var$cos2
rowSums(PCA_USA_mod2013$var$cos2)

corrplot(PCA_USA_mod2013$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2013$var$cos2, is.corr = FALSE)
#Интерпретация ГК аналогична первоначальной

#Строим дендрограмму и выделяем кластеры
clusters2013_USA <- HCPC(PCA_USA_mod2013)

usa2013 <- clusters2013_USA$data.clust
clust1_2013 <- filter(usa2013, clust == "1")
clust2_2013 <- filter(usa2013, clust == "2")
clust3_2013 <- filter(usa2013, clust == "3")
clust4_2013 <- filter(usa2013, clust == "4")
#Первый кластер состоит из United Community Banks Inc, он выше всех по эффективности, но будет отнесен к мелким банкам


fviz_pca_ind(PCA_USA_mod2013, repel = TRUE, fill.ind = usa2013[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2013,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2013 году", label = "all",col.var = "darkblue",habillage=usa2013[,9])


##################    2014 год     ##################

PCA_USA2014 <- filter(X000PCA_banks, Year == 2014)
PCA_USA2014 <- PCA_USA2014[,-c(1,2,11:15)]
rows2014 <- filter(X000PCA_banks, Year == 2014)
rownames(PCA_USA2014) <- rows2014$Company

#Корреляционная матрица для кластеризации в 2014 году:
c <- cor(PCA_USA2014)
corrplot(c) #как в 2011 году

#построим модель в пространстве главных компонент
PCA_USA_mod2014 <- PCA(PCA_USA2014)
fviz_eig(PCA_USA_mod2014, addlabels = TRUE)

m2014 <- PCA_USA_mod2014$var$coord
stargazer(m2014, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2014$var$cos2
rowSums(PCA_USA_mod2014$var$cos2)

corrplot(PCA_USA_mod2014$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2014$var$cos2, is.corr = FALSE)
#Интерпретация ГК аналогична первоначальной

#Строим дендрограмму и выделяем кластеры
clusters2014_USA <- HCPC(PCA_USA_mod2014)

usa2014 <- clusters2014_USA$data.clust
clust1_2014 <- filter(usa2014, clust == "1")
clust2_2014 <- filter(usa2014, clust == "2")
clust3_2014 <- filter(usa2014, clust == "3")
#3 кластера

fviz_pca_ind(PCA_USA_mod2014, repel = TRUE, fill.ind = usa2014[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2014,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2014 году", label = "all",col.var = "darkblue",habillage=usa2014[,9])


##################    2015 год     ##################

PCA_USA2015 <- filter(X000PCA_banks, Year == 2015)
PCA_USA2015 <- PCA_USA2015[,-c(1,2,11:15)]
rows2015 <- filter(X000PCA_banks, Year == 2015)
rownames(PCA_USA2015) <- rows2015$Company

#Корреляционная матрица для кластеризации в 2015 году:
c <- cor(PCA_USA2015)
corrplot(c) #как в 2011 году

#построим модель в пространстве главных компонент
PCA_USA_mod2015 <- PCA(PCA_USA2015)
fviz_eig(PCA_USA_mod2015, addlabels = TRUE)

m2015 <- PCA_USA_mod2015$var$coord
stargazer(m2015, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2015$var$cos2
rowSums(PCA_USA_mod2015$var$cos2)

corrplot(PCA_USA_mod2015$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2015$var$cos2, is.corr = FALSE)
#Интерпретация ГК аналогична первоначальной

#Строим дендрограмму и выделяем кластеры
clusters2015_USA <- HCPC(PCA_USA_mod2015)

usa2015 <- clusters2015_USA$data.clust
clust1_2015 <- filter(usa2015, clust == "1")
clust2_2015 <- filter(usa2015, clust == "2")
clust3_2015 <- filter(usa2015, clust == "3")
#3 кластера

fviz_pca_ind(PCA_USA_mod2015, repel = TRUE, fill.ind = usa2015[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2015,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2015 году", label = "all",col.var = "darkblue",habillage=usa2015[,9])

#Кластеризация относительно размера сохраняется, интересно, что банки средней руки демонстрируют более низкую эффективность в среднем, что мелкие


##################    2016 год     ##################

PCA_USA2016 <- filter(X000PCA_banks, Year == 2016)
PCA_USA2016 <- PCA_USA2016[,-c(1,2,11:15)]
rows2016 <- filter(X000PCA_banks, Year == 2016)
rownames(PCA_USA2016) <- rows2016$Company

#Корреляционная матрица для кластеризации в 2016 году:
c <- cor(PCA_USA2016)
corrplot(c) 

#построим модель в пространстве главных компонент
PCA_USA_mod2016 <- PCA(PCA_USA2016)
fviz_eig(PCA_USA_mod2016, addlabels = TRUE)

m2016 <- PCA_USA_mod2016$var$coord
stargazer(m2016, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2016$var$cos2
rowSums(PCA_USA_mod2016$var$cos2)

corrplot(PCA_USA_mod2016$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2016$var$cos2, is.corr = FALSE)
#В этом году 2 ГК так же связана с высокой эффективностью, но обратно - с долей непроцентного дохода.
#Можно сказать, что это эффективность традиционного банкинга

#Строим дендрограмму и выделяем кластеры
clusters2016_USA <- HCPC(PCA_USA_mod2016)

usa2016 <- clusters2016_USA$data.clust
clust1_2016 <- filter(usa2016, clust == "1")
clust2_2016 <- filter(usa2016, clust == "2")
clust3_2016 <- filter(usa2016, clust == "3")
#3 кластера

fviz_pca_ind(PCA_USA_mod2016, repel = TRUE, fill.ind = usa2016[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2016,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2016 году", label = "all",col.var = "darkblue",habillage=usa2016[,9])

#Более низкая эффективность средних банков сохраняется


##################    2017 год     ##################

PCA_USA2017 <- filter(X000PCA_banks, Year == 2017)
PCA_USA2017 <- PCA_USA2017[,-c(1,2,11:15)]
rows2017 <- filter(X000PCA_banks, Year == 2017)
rownames(PCA_USA2017) <- rows2017$Company

#Корреляционная матрица для кластеризации в 2017 году:
c <- cor(PCA_USA2017)
corrplot(c) 

#построим модель в пространстве главных компонент
PCA_USA_mod2017 <- PCA(PCA_USA2017)
fviz_eig(PCA_USA_mod2017, addlabels = TRUE)

m2017 <- PCA_USA_mod2017$var$coord
stargazer(m2017, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2017$var$cos2
rowSums(PCA_USA_mod2017$var$cos2)

corrplot(PCA_USA_mod2017$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2017$var$cos2, is.corr = FALSE)
#Аналогично первоначальной интерпретации

#Строим дендрограмму и выделяем кластеры
clusters2017_USA <- HCPC(PCA_USA_mod2017)

usa2017 <- clusters2017_USA$data.clust
clust1_2017 <- filter(usa2017, clust == "1")
clust2_2017 <- filter(usa2017, clust == "2")
clust3_2017 <- filter(usa2017, clust == "3")
#3 кластера

fviz_pca_ind(PCA_USA_mod2017, repel = TRUE, fill.ind = usa2017[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2017,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2017 году", label = "all",col.var = "darkblue",habillage=usa2017[,9])



##################    2018 год     ##################

PCA_USA2018 <- filter(X000PCA_banks, Year == 2018)
PCA_USA2018 <- PCA_USA2018[,-c(1,2,11:15)]
rows2018 <- filter(X000PCA_banks, Year == 2018)
rownames(PCA_USA2018) <- rows2018$Company

#Корреляционная матрица для кластеризации в 2018 году:
c <- cor(PCA_USA2018)
corrplot(c) 

#построим модель в пространстве главных компонент
PCA_USA_mod2018 <- PCA(PCA_USA2018)
fviz_eig(PCA_USA_mod2018, addlabels = TRUE)

m2018 <- PCA_USA_mod2018$var$coord
stargazer(m2018, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2018$var$cos2
rowSums(PCA_USA_mod2018$var$cos2)

corrplot(PCA_USA_mod2018$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2018$var$cos2, is.corr = FALSE)
#Вторая ГК - отрицательно коррелирует с ROE, положительно - с FCF, интерпретация - высокая возможность роста, низкая эффективность - степень зрелости

#Строим дендрограмму и выделяем кластеры
clusters2018_USA <- HCPC(PCA_USA_mod2018)

usa2018 <- clusters2018_USA$data.clust
clust1_2018 <- filter(usa2018, clust == "1")
clust2_2018 <- filter(usa2018, clust == "2")
clust3_2018 <- filter(usa2018, clust == "3")
#3 кластера

fviz_pca_ind(PCA_USA_mod2018, repel = TRUE, fill.ind = usa2018[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2018,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2018 году", label = "all",col.var = "darkblue",habillage=usa2018[,9])


##################    2019 год     ##################

PCA_USA2019 <- filter(X000PCA_banks, Year == 2019)
PCA_USA2019 <- PCA_USA2019[,-c(1,2,11:15)]
rows2019 <- filter(X000PCA_banks, Year == 2019)
rownames(PCA_USA2019) <- rows2019$Company

#Корреляционная матрица для кластеризации в 2019 году:
c <- cor(PCA_USA2019)
corrplot(c) 

#построим модель в пространстве главных компонент
PCA_USA_mod2019 <- PCA(PCA_USA2019)
fviz_eig(PCA_USA_mod2019, addlabels = TRUE)

m2019 <- PCA_USA_mod2019$var$coord
stargazer(m2019, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2019$var$cos2
rowSums(PCA_USA_mod2019$var$cos2)

corrplot(PCA_USA_mod2019$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2019$var$cos2, is.corr = FALSE)
#Вторая ГК отвечает за эффективность
#Строим дендрограмму и выделяем кластеры
clusters2019_USA <- HCPC(PCA_USA_mod2019)

usa2019 <- clusters2019_USA$data.clust
clust1_2019 <- filter(usa2019, clust == "1")
clust2_2019 <- filter(usa2019, clust == "2")
clust3_2019 <- filter(usa2019, clust == "3")
#3 кластера

fviz_pca_ind(PCA_USA_mod2019, repel = TRUE, fill.ind = usa2019[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2019,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2019 году", label = "all",col.var = "darkblue",habillage=usa2019[,9])


##################    2020 год     ##################

PCA_USA2020 <- filter(X000PCA_banks, Year == 2020)
PCA_USA2020 <- PCA_USA2020[,-c(1,2,11:15)]
rows2020 <- filter(X000PCA_banks, Year == 2020)
rownames(PCA_USA2020) <- rows2020$Company

#Корреляционная матрица для кластеризации в 2020 году:
c <- cor(PCA_USA2020)
corrplot(c) 

#построим модель в пространстве главных компонент
PCA_USA_mod2020 <- PCA(PCA_USA2020)
fviz_eig(PCA_USA_mod2020, addlabels = TRUE)

m2020 <- PCA_USA_mod2020$var$coord
stargazer(m2020, type = "text", out = "matrix.html",summary = FALSE)
PCA_USA_mod2020$var$cos2
rowSums(PCA_USA_mod2020$var$cos2)

corrplot(PCA_USA_mod2020$var$coord, is.corr = FALSE)
corrplot(PCA_USA_mod2020$var$cos2, is.corr = FALSE)
#Вторая ГК отвечает за эффективность
#Строим дендрограмму и выделяем кластеры
clusters2020_USA <- HCPC(PCA_USA_mod2020)
#R предлагает 6 кластеров, но они близко друг к другу и не подойдут для качественной интерпретации так, как подошли бы 3 кластера
#Поэтому выделим 3-4 кластера, как раньше

usa2020 <- clusters2020_USA$data.clust
clust1_2020 <- filter(usa2020, clust == "1")
clust2_2020 <- filter(usa2020, clust == "2")
clust3_2020 <- filter(usa2020, clust == "3")
clust4_2020 <- filter(usa2020, clust == "4")
#4 кластера. Опять имеем низкоэффективный банк и числа малых. Интересно, что кластер топ-банков пополнился банком Toronto-Dominion Bank  

fviz_pca_ind(PCA_USA_mod2020, repel = TRUE, fill.ind = usa2020[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_USA_mod2020,repel = TRUE, title = "Американские публичные банки в пространстве \nглавных компонент в 2020 году", label = "all",col.var = "darkblue",habillage=usa2020[,9])

fviz_pca_ind(PCA_USA_mod2020, repel = TRUE, fill.ind = usa2020[,9], pointshape = 21, pointsize = 1, addEllipses = TRUE,title = "Американские публичные банки в пространстве \nглавных компонент в 2020 году")
fviz_dend(clusters2020_USA, cex = 0.7, rect = TRUE, rect_fill = TRUE, labels_track_height = 0.8,title = "Иерархическая кластеризация \nбританских банков в 2020 году")

summary(X001PCA_banks[,-c(1,2)])


#Делаем то же самое для БРИТАНСКИХ банков (001PCA.xlsx)

PCA_UK2010 <- filter(X001PCA_banks, Year == 2010)
PCA_UK2010 <- PCA_UK2010[,-c(1,2,11:15)]
rows2010 <- filter(X001PCA_banks, Year == 2010)
rownames(PCA_UK2010) <- rows2010$Company

#Корреляционная матрица для кластеризации в 2010 году:
c <- cor(PCA_UK2010)
corrplot(c)
#Показатели связаны совсем не так, как в Америке, ROE отрицательно коррелирует с долей непроцентных доходов
#Это говорит о том, что в банковской среде Британии в 2010 году сохранялись традиционные подходы к ведению бизнеса


#построим модель в пространстве главных компонент
PCA_UK_mod2010 <- PCA(PCA_UK2010)
fviz_eig(PCA_UK_mod2010, addlabels = TRUE)

u2010 <- PCA_UK_mod2010$var$coord
stargazer(u2010, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2010$var$cos2
rowSums(PCA_UK_mod2010$var$cos2)

corrplot(PCA_UK_mod2010$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2010$var$cos2, is.corr = FALSE)

#Исходя из визуализации матрицы нагрузок, можно дать следующую интерпретацию главным компонентам:
#1-я ГК - отвечает за величину банка: высокая значимая корреляция с капитализацией, кредитным портфелем и показателем EBITDA (Аналогично в случае США)
#2-я ГК - отвечает за зрелость банка и потенциал роста кэша: Положительно коррелирует с долей непроцентного дохода (то есть с преобладанием комиссий надо процентными доходами)
#отрицательно - с капитализацией и ROE (маленькие и менее эффективные банки - банки на ранней стадии жизненного цикла)
#Фокус на кредитование, а не на привлечение депозитов

#Строим дендрограмму и выделяем кластеры
clusters2010_UK <- HCPC(PCA_UK_mod2010)

UK2010 <- clusters2010_UK$data.clust
clust1 <- filter(UK2010, clust == "1")
clust2 <- filter(UK2010, clust == "2")
clust3 <- filter(UK2010, clust == "3")
clust4 <- filter(UK2010, clust == "4")
clust5 <- filter(UK2010, clust == "5")

fviz_pca_ind(PCA_UK_mod2010, repel = TRUE, fill.ind = UK2010[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2010,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2010 году", label = "all",col.var = "darkblue",habillage=UK2010[,9])

#Оптимальной кластрезацией является разбиение на 5 кластеров, но это не подходит для построения дамми-переменных, отвечающих за то, крупных банк или нет
#Кластер 1 - мелкие банки
#Кластер 2 - Bank or Ireland Group PLC, выделен в отдельный кластер в первую очередь по ГК2, однако по важности и размерам отнесем его к средним
#Кластер 3 - банки средней руки
#В Кластерах 4 и 5 по одному топовому британскому банку, которые отличаются эффективностью и структурой выручки, но исходя из их близости по капитализации, объемам портфелей и EBITDA - формируем из них группу крупных банков


####### 2011 год Британия ############

PCA_UK2011 <- filter(X001PCA_banks, Year == 2011)
PCA_UK2011 <- PCA_UK2011[,-c(1,2,11:15)]
rows2011 <- filter(X001PCA_banks, Year == 2011)
rownames(PCA_UK2011) <- rows2011$Company

#Корреляционная матрица для кластеризации в 2011 году:
c <- cor(PCA_UK2011)
corrplot(c)

#построим модель в пространстве главных компонент
PCA_UK_mod2011 <- PCA(PCA_UK2011)
fviz_eig(PCA_UK_mod2011, addlabels = TRUE)

u2011 <- PCA_UK_mod2011$var$coord
stargazer(u2011, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2011$var$cos2
rowSums(PCA_UK_mod2011$var$cos2)

corrplot(PCA_UK_mod2011$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2011$var$cos2, is.corr = FALSE)

#Интерпретация первой ГК не меняется
#Вторая ГК отвечает за эффективность банка
#К сожалению, нельзя добавить третью ГК, ее интерпретация схожа с интерпретацией ГК-2 в 2010 году

#Строим дендрограмму и выделяем кластеры
clusters2011_UK <- HCPC(PCA_UK_mod2011)

UK2011 <- clusters2011_UK$data.clust
clust1 <- filter(UK2011, clust == "1")
clust2 <- filter(UK2011, clust == "2")
clust3 <- filter(UK2011, clust == "3")
clust4 <- filter(UK2011, clust == "4")
clust5 <- filter(UK2011, clust == "5")

fviz_pca_ind(PCA_UK_mod2011, repel = TRUE, fill.ind = UK2011[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2011,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2011 году", label = "all",col.var = "darkblue",habillage=UK2011[,9])

#Вновь 5 кластеров из-за разницы банков в эффективности
#Кластер 1 - мелкие банки
#Кластер 2,3 и 4 объединяем в один кластер по первой ГК (средние банки)
#Кластер 5 - HSBC - крупнейший банк


####### 2012 год Британия ############

PCA_UK2012 <- filter(X001PCA_banks, Year == 2012)
PCA_UK2012 <- PCA_UK2012[,-c(1,2,11:15)]
rows2012 <- filter(X001PCA_banks, Year == 2012)
rownames(PCA_UK2012) <- rows2012$Company

#Корреляционная матрица для кластеризации в 2012 году:
c <- cor(PCA_UK2012)
corrplot(c)

#построим модель в пространстве главных компонент
PCA_UK_mod2012 <- PCA(PCA_UK2012)
fviz_eig(PCA_UK_mod2012, addlabels = TRUE)

u2012 <- PCA_UK_mod2012$var$coord
stargazer(u2012, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2012$var$cos2
rowSums(PCA_UK_mod2012$var$cos2)

corrplot(PCA_UK_mod2012$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2012$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2012_UK <- HCPC(PCA_UK_mod2012)
#сразу порезал на три кластера

UK2012 <- clusters2012_UK$data.clust
clust1 <- filter(UK2012, clust == "1")
clust2 <- filter(UK2012, clust == "2")
clust3 <- filter(UK2012, clust == "3")


fviz_pca_ind(PCA_UK_mod2012, repel = TRUE, fill.ind = UK2012[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2012,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2012 году", label = "all",col.var = "darkblue",habillage=UK2012[,9])


####### 2013 год Британия ############

PCA_UK2013 <- filter(X001PCA_banks, Year == 2013)
PCA_UK2013 <- PCA_UK2013[,-c(1,2,11:15)]
rows2013 <- filter(X001PCA_banks, Year == 2013)
rownames(PCA_UK2013) <- rows2013$Company

#Корреляционная матрица для кластеризации в 2013 году:
c <- cor(PCA_UK2013)
corrplot(c)

#построим модель в пространстве главных компонент
PCA_UK_mod2013 <- PCA(PCA_UK2013)
fviz_eig(PCA_UK_mod2013, addlabels = TRUE)

u2013 <- PCA_UK_mod2013$var$coord
stargazer(u2013, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2013$var$cos2
rowSums(PCA_UK_mod2013$var$cos2)

corrplot(PCA_UK_mod2013$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2013$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2013_UK <- HCPC(PCA_UK_mod2013)
#теперь R предлагает сразу три кластера, теперь дифференциация по размеру-эффективности не такая расплывчатая, как ранее

UK2013 <- clusters2013_UK$data.clust
clust1 <- filter(UK2013, clust == "1")
clust2 <- filter(UK2013, clust == "2")
clust3 <- filter(UK2013, clust == "3")


fviz_pca_ind(PCA_UK_mod2013, repel = TRUE, fill.ind = UK2013[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2013,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2013 году", label = "all",col.var = "darkblue",habillage=UK2013[,9])


####### 2014 год Британия ############

PCA_UK2014 <- filter(X001PCA_banks, Year == 2014)
PCA_UK2014 <- PCA_UK2014[,-c(1,2,11:15)]
rows2014 <- filter(X001PCA_banks, Year == 2014)
rownames(PCA_UK2014) <- rows2014$Company

#Корреляционная матрица для кластеризации в 2014 году:
c <- cor(PCA_UK2014)
corrplot(c)

#построим модель в пространстве главных компонент
PCA_UK_mod2014 <- PCA(PCA_UK2014)
fviz_eig(PCA_UK_mod2014, addlabels = TRUE)

u2014 <- PCA_UK_mod2014$var$coord
stargazer(u2014, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2014$var$cos2
rowSums(PCA_UK_mod2014$var$cos2)

corrplot(PCA_UK_mod2014$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2014$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2014_UK <- HCPC(PCA_UK_mod2014)
#теперь R предлагает сразу 4 кластера, чтобы не объединять мелкие эффективные и мелкие неэффективные позже, сразу сделаем разбивку на 3 кластера

UK2014 <- clusters2014_UK$data.clust
clust1 <- filter(UK2014, clust == "1")
clust2 <- filter(UK2014, clust == "2")
clust3 <- filter(UK2014, clust == "3")


fviz_pca_ind(PCA_UK_mod2014, repel = TRUE, fill.ind = UK2014[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2014,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2014 году", label = "all",col.var = "darkblue",habillage=UK2014[,9])


####### 2015 год Британия ############

PCA_UK2015 <- filter(X001PCA_banks, Year == 2015)
PCA_UK2015 <- PCA_UK2015[,-c(1,2,11:15)]
rows2015 <- filter(X001PCA_banks, Year == 2015)
rownames(PCA_UK2015) <- rows2015$Company

#Корреляционная матрица для кластеризации в 2015 году:
c <- cor(PCA_UK2015)
corrplot(c)

#построим модель в пространстве главных компонент
PCA_UK_mod2015 <- PCA(PCA_UK2015)
fviz_eig(PCA_UK_mod2015, addlabels = TRUE)

u2015 <- PCA_UK_mod2015$var$coord
stargazer(u2015, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2015$var$cos2
rowSums(PCA_UK_mod2015$var$cos2)

corrplot(PCA_UK_mod2015$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2015$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2015_UK <- HCPC(PCA_UK_mod2015)
#теперь R предлагает сразу 4 кластера, чтобы не объединять мелкие эффективные и мелкие неэффективные позже, сразу сделаем разбивку на 3 кластера

UK2015 <- clusters2015_UK$data.clust
clust1 <- filter(UK2015, clust == "1")
clust2 <- filter(UK2015, clust == "2")
clust3 <- filter(UK2015, clust == "3")

fviz_pca_ind(PCA_UK_mod2015, repel = TRUE, fill.ind = UK2015[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2015,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2015 году", label = "all",col.var = "darkblue",habillage=UK2015[,9])


####### 2016 год Британия ############

PCA_UK2016 <- filter(X001PCA_banks, Year == 2016)
PCA_UK2016 <- PCA_UK2016[,-c(1,2,11:15)]
rows2016 <- filter(X001PCA_banks, Year == 2016)
rownames(PCA_UK2016) <- rows2016$Company

#Корреляционная матрица для кластеризации в 2016 году:
c <- cor(PCA_UK2016)
corrplot(c)

#построим модель в пространстве главных компонент
PCA_UK_mod2016 <- PCA(PCA_UK2016)
fviz_eig(PCA_UK_mod2016, addlabels = TRUE)

u2016 <- PCA_UK_mod2016$var$coord
stargazer(u2016, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2016$var$cos2
rowSums(PCA_UK_mod2016$var$cos2)

corrplot(PCA_UK_mod2016$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2016$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2016_UK <- HCPC(PCA_UK_mod2016)
#Оптимальна разбивка на 4 кластера

UK2016 <- clusters2016_UK$data.clust
clust1 <- filter(UK2016, clust == "1")
clust2 <- filter(UK2016, clust == "2")
clust3 <- filter(UK2016, clust == "3")
clust4 <- filter(UK2016, clust == "4")

fviz_pca_ind(PCA_UK_mod2016, repel = TRUE, fill.ind = UK2016[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2016,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2016 году", label = "all",col.var = "darkblue",habillage=UK2016[,9])

#Кластер 1 - мелкие неэффективные банки
#Кластер 2 - мелкие эффективные банки с потенциалом роста (объединяем с кластером 1)
#Кластер 3 - средние
#Кластер 4 - крупный HSBC 

####### 2017 год Британия ############

PCA_UK2017 <- filter(X001PCA_banks, Year == 2017)
PCA_UK2017 <- PCA_UK2017[,-c(1,2,11:15)]
rows2017 <- filter(X001PCA_banks, Year == 2017)
rownames(PCA_UK2017) <- rows2017$Company

#Корреляционная матрица для кластеризации в 2017 году:
c <- cor(PCA_UK2017)
corrplot(c) #Впервые ROE не коррелирует с долей непроцентного дохода. Похоже на матрицу для американских банков в 2010 году

#построим модель в пространстве главных компонент
PCA_UK_mod2017 <- PCA(PCA_UK2017)
fviz_eig(PCA_UK_mod2017, addlabels = TRUE)

u2017 <- PCA_UK_mod2017$var$coord
stargazer(u2017, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2017$var$cos2
rowSums(PCA_UK_mod2017$var$cos2)

corrplot(PCA_UK_mod2017$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2017$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году, третья ГК отвечала бы за потенциал роста кэша на балансе

#Строим дендрограмму и выделяем кластеры
clusters2017_UK <- HCPC(PCA_UK_mod2017)
#Разбиваем вручную на 3 кластера

UK2017 <- clusters2017_UK$data.clust
clust1 <- filter(UK2017, clust == "1")
clust2 <- filter(UK2017, clust == "2")
clust3 <- filter(UK2017, clust == "3")

fviz_pca_ind(PCA_UK_mod2017, repel = TRUE, fill.ind = UK2017[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2017,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2017 году", label = "all",col.var = "darkblue",habillage=UK2017[,9])


####### 2018 год Британия ############

PCA_UK2018 <- filter(X001PCA_banks, Year == 2018)
PCA_UK2018 <- PCA_UK2018[,-c(1,2,11:15)]
rows2018 <- filter(X001PCA_banks, Year == 2018)
rownames(PCA_UK2018) <- rows2018$Company

#Корреляционная матрица для кластеризации в 2018 году:
c <- cor(PCA_UK2018)
corrplot(c) #Отрицательная корреляция ROE с долей непроцентного дохода еще есть.

#построим модель в пространстве главных компонент
PCA_UK_mod2018 <- PCA(PCA_UK2018)
fviz_eig(PCA_UK_mod2018, addlabels = TRUE)

u2018 <- PCA_UK_mod2018$var$coord
stargazer(u2018, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2018$var$cos2
rowSums(PCA_UK_mod2018$var$cos2)

corrplot(PCA_UK_mod2018$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2018$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2018_UK <- HCPC(PCA_UK_mod2018)
#Разбиваем вручную на 3 кластера

UK2018 <- clusters2018_UK$data.clust
clust1 <- filter(UK2018, clust == "1")
clust2 <- filter(UK2018, clust == "2")
clust3 <- filter(UK2018, clust == "3")

fviz_pca_ind(PCA_UK_mod2018, repel = TRUE, fill.ind = UK2018[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2018,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2018 году", label = "all",col.var = "darkblue",habillage=UK2018[,9])

####### 2019 год Британия ############

PCA_UK2019 <- filter(X001PCA_banks, Year == 2019)
PCA_UK2019 <- PCA_UK2019[,-c(1,2,11:15)]
rows2019 <- filter(X001PCA_banks, Year == 2019)
rownames(PCA_UK2019) <- rows2019$Company

#Корреляционная матрица для кластеризации в 2019 году:
c <- cor(PCA_UK2019)
corrplot(c) #Отрицательная корреляция ROE Отрицательно коррелирует со всеми показателями, но слабо

#построим модель в пространстве главных компонент
PCA_UK_mod2019 <- PCA(PCA_UK2019)
fviz_eig(PCA_UK_mod2019, addlabels = TRUE)

u2019 <- PCA_UK_mod2019$var$coord
stargazer(u2019, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2019$var$cos2
rowSums(PCA_UK_mod2019$var$cos2)

corrplot(PCA_UK_mod2019$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2019$var$cos2, is.corr = FALSE)
#Интерпретация аналогична прошлому году

#Строим дендрограмму и выделяем кластеры
clusters2019_UK <- HCPC(PCA_UK_mod2019)
#Необходимо разбивать на 5 кластеров и смотреть, какие можно объединить

UK2019 <- clusters2019_UK$data.clust
clust1 <- filter(UK2019, clust == "1")
clust2 <- filter(UK2019, clust == "2")
clust3 <- filter(UK2019, clust == "3")
clust4 <- filter(UK2019, clust == "4")
clust5 <- filter(UK2019, clust == "5")

fviz_pca_ind(PCA_UK_mod2019, repel = TRUE, fill.ind = UK2019[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2019,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2019 году", label = "all",col.var = "darkblue",habillage=UK2019[,9])

#Объединяем кластеры 1 и 2 - малые банки
#Кластеры 3 и 4 - средние
#Кластер 5 - единственный гегемон - HSBC


####### 2020 год Британия ############

PCA_UK2020 <- filter(X001PCA_banks, Year == 2020)
PCA_UK2020 <- PCA_UK2020[,-c(1,2,11:15)]
rows2020 <- filter(X001PCA_banks, Year == 2020)
rownames(PCA_UK2020) <- rows2020$Company

#Корреляционная матрица для кластеризации в 2020 году:
c <- cor(PCA_UK2020)
corrplot(c) #Отрицательная корреляция ROE вновь исчезла

#построим модель в пространстве главных компонент
PCA_UK_mod2020 <- PCA(PCA_UK2020)
fviz_eig(PCA_UK_mod2020, addlabels = TRUE)

u2020 <- PCA_UK_mod2020$var$coord
stargazer(u2020, type = "text", out = "matrix.html",summary = FALSE)
PCA_UK_mod2020$var$cos2
rowSums(PCA_UK_mod2020$var$cos2)

corrplot(PCA_UK_mod2020$var$coord, is.corr = FALSE)
corrplot(PCA_UK_mod2020$var$cos2, is.corr = FALSE)
#Интерпретация изменилась. Вторая ГК коррелирует с обхемом кредитного портфеля
#За потенциал роста кэша отвечает 3 ГК, которую нельзя проанализировать

#Строим дендрограмму и выделяем кластеры
clusters2020_UK <- HCPC(PCA_UK_mod2020)
#Необходимо разбивать на 5 кластеров и смотреть, какие можно объединить

UK2020 <- clusters2020_UK$data.clust
clust1 <- filter(UK2020, clust == "1")
clust2 <- filter(UK2020, clust == "2")
clust3 <- filter(UK2020, clust == "3")
clust4 <- filter(UK2020, clust == "4")
clust5 <- filter(UK2020, clust == "5")

fviz_pca_ind(PCA_UK_mod2020, repel = TRUE, fill.ind = UK2020[,9], pointshape = 21,
             pointsize = 4, geom = "point")
fviz_pca_biplot(PCA_UK_mod2020,repel = TRUE, title = "Британские публичные банки в пространстве \nглавных компонент в 2020 году", label = "all",col.var = "darkblue",habillage=UK2020[,9])

fviz_pca_ind(PCA_UK_mod2020, repel = TRUE, fill.ind = UK2020[,9], pointshape = 21, pointsize = 1, addEllipses = TRUE,title = "Британские публичные банки в пространстве \nглавных компонент в 2020 году")
fviz_dend(clusters2020_UK, cex = 0.7, rect = TRUE, rect_fill = TRUE, labels_track_height = 0.8,title = "Иерархическая кластеризация \nбританских банков в 2020 году")

#Объединяем кластеры 1, 2 и 3- малые банки
#Кластер 4 - средние
#Кластер 5 -  HSBC


#____________РЕЗУЛЬТАТЫ КЛАСТЕРИЗАЦИИ________________
#выгрузим резульатты кластеризации, чтобы затем на основе их построить переменные SmallBank, MiddleBank, BigBank

USA_clusters <- rbind(usa2010,usa2011,usa2012,usa2013,usa2014,usa2015,usa2016,usa2017,usa2018,usa2019,usa2020)
UK_clusters <- rbind(UK2010,UK2011,UK2012,UK2013,UK2014,UK2015,UK2016,UK2017,UK2018,UK2019,UK2020)

write.xlsx(USA_clustered, file = "USA_clustered.xlsx")
write.xlsx(UK_clustered, file = "UK_clustered.xlsx")


class_us <- ifelse(USA_clustered$SmallBank == 1, 0, ifelse(USA_clustered$MediumBank == 1, 1, 2))
data <- cbind(USA_clustered,class_us)
data$class_us <- factor(data$class_us, levels=c(0, 1,2),
                             labels=c("Small Bank","Medium Bank", "Large Bank"))

#Предварительный анализ и графики
names(USA_clustered)
library(lattice)
View(data)

colors = c("blue", "orange","red") 
lines = c(1,1,1) 
points = c(16,17,18)
key.trans <- list(title = "Группировка по размеру банка",
                  space="bottom", columns=1,
                  text=list(levels(data$class_us)),
                  points=list(pch=points, col=colors),
                  lines=list(col=colors, lty=lines),
                  cex.title=0.8, cex=.7)
densityplot(~Credit_portf, data=data,
            group=class_us,
            main="Плотность распределения объемов кредитного портфеля \nв зависимости от масштаба банка (США)",
            xlab = "Кредитный портфель (mln USD)",
            ylab = "Плотность",
            pch=points, lty=lines, col=colors,
            lwd=2, jitter=.000018,
            key = key.trans)

densityplot(~EBITDA, data=data,
            group=class_us,
            main="Плотность распределения EBITDA \nв зависимости от масштаба банка (США)",
            xlab = "EBITDA (mln.USD)",
            ylab = "Плотность",
            pch=points, lty=lines, col=colors,
            lwd=2, jitter=.0011,
            key = key.trans)


#Добавим доходности и переменные интереса из датасета по модели Ф-Ф
Yield <- Three_five_factor_FF_USA$Yield
Funding_growth <- Three_five_factor_FF_USA$Funding_growth
Deals_growth <- Three_five_factor_FF_USA$Deals_growth
data <- cbind(data,Yield,Funding_growth,Deals_growth)

densityplot(~Yield, data=data,
            group=class_us,
            main="Плотность распределения доходности акций \nв зависимости от масштаба банка (США)",
            xlab = "Доходность",
            ylab = "Плотность",
            pch=points, lty=lines, col=colors,
            lwd=2, jitter=.8,
            key = key.trans)


#Построим совмещенные диаграммы рассеяния

mypanel <- function(x, y) {
  panel.xyplot(x, y, pch=19) 
  panel.rug(x, y)
  panel.grid(h=-1, v=-1)
  panel.lmline(x, y, col="red", lwd=1, lty=2)
}

xyplot(data$Yield ~ data$Funding_growth|data$class_us,
       layout=c(3, 1), 
       aspect=0.8,
       main = "Диграммы рассеяния для темпов роста \nфинансирования и изменения доходности (США)",
       sub = "Группиковка по масштабу банка",
       xlab = "Funding growth",
       ylab = "Yield",
       panel = mypanel)

xyplot(data$Yield ~ data$Deals_growth|data$class_us,
       layout=c(3, 1), 
       aspect=0.8,
       main = "Диграммы рассеяния для темпов роста \nчисла сделок и изменения доходности (США)",
       sub = "Группиковка по масштабу банка",
       xlab = "Deals growth",
       ylab = "Yield",
       panel = mypanel)


class_uk <- ifelse(UK_clustered$SmallBank == 1, 0, ifelse(UK_clustered$MediumBank == 1, 1, 2))
data2 <- cbind(UK_clustered,class_uk)
data2$class_uk <- factor(data2$class_uk, levels=c(0, 1,2),
                        labels=c("Small Bank","Medium Bank", "Large Bank"))


colors = c("dark grey", "brown","dark green") 
lines = c(1,1,1) 
points = c(16,17,18)
key.trans <- list(title = "Группировка по размеру банка",
                  space="bottom", columns=1,
                  text=list(levels(data2$class_uk)),
                  points=list(pch=points, col=colors),
                  lines=list(col=colors, lty=lines),
                  cex.title=0.8, cex=.7)
densityplot(~Credit_portf, data=data2,
            group=class_uk,
            main="Плотность распределения объемов кредитного портфеля \nв зависимости от масштаба банка (Великобритания)",
            xlab = "Кредитный портфель (mln USD)",
            ylab = "Плотность",
            pch=points, lty=lines, col=colors,
            lwd=2, jitter=.000018,
            key = key.trans)

densityplot(~EBITDA, data=data2,
            group=class_uk,
            main="Плотность распределения EBITDA \nв зависимости от масштаба банка (Великобритания)",
            xlab = "EBITDA (mln.USD)",
            ylab = "Плотность",
            pch=points, lty=lines, col=colors,
            lwd=2, jitter=.0011,
            key = key.trans)

Yield <- Three_factor_FF_UK$Yield
Funding_growth <- Three_factor_FF_UK$Funding_growth
Deals_growth <- Three_factor_FF_UK$Deals_growth
data2 <- cbind(data2,Yield,Funding_growth,Deals_growth)

densityplot(~Yield, data=data2,
            group=class_uk,
            main="Плотность распределения доходности акций \nв зависимости от масштаба банка (Великобритания)",
            xlab = "Доходность",
            ylab = "Плотность",
            pch=points, lty=lines, col=colors,
            lwd=2, jitter=.8,
            key = key.trans)


#Построим совмещенные диаграммы рассеяния

mypanel2 <- function(x, y) {
  panel.xyplot(x, y, col="dark green", pch=19) 
  panel.rug(x, y,col="dark green")
  panel.grid(h=-1, v=-1)
  panel.lmline(x, y, col="brown", lwd=1, lty=2)
}

xyplot(data2$Yield ~ data2$Funding_growth|data2$class_uk,
       layout=c(3, 1), 
       aspect=0.8,
       main = "Диграммы рассеяния для темпов роста \nфинансирования и изменения доходности (Великобритания)",
       sub = "Группиковка по масштабу банка",
       xlab = "Funding growth",
       ylab = "Yield",
       panel = mypanel2)

xyplot(data2$Yield ~ data2$Deals_growth|data2$class_uk,
       layout=c(3, 1), 
       aspect=0.8,
       main = "Диграммы рассеяния для темпов роста \nчисла сделок и изменения доходности (США)",
       sub = "Группиковка по масштабу банка",
       xlab = "Deals growth",
       ylab = "Yield",
       panel = mypanel2)



############# Fama-French model for USA and UK ##########

dataUSA <- pdata.frame(Three_five_factor_FF_USA,
                    index = c("Company","Year"),
                    row.names = TRUE)
names(dataUSA)

dataUSA2011 <- na.omit(dataUSA)

c <- cor(dataUSA2011[,-c(1,2)])
corrplot(c)

#трехфакторная модель Фама-Френча для Американских банков

m.ff3.fe <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth, data = dataUSA2011, model = "within")
m.ff3.fe1 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank, data = dataUSA2011, model = "within")
m.ff3.fe2 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others, data = dataUSA2011, model = "within")
m.ff3.fe3 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE, data = dataUSA2011, model = "within")

m.ff3.pooled <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth-1, data = dataUSA2011, model = "pooling")
m.ff3.pooled1 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank-1, data = dataUSA2011, model = "pooling")
m.ff3.pooled2 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others-1, data = dataUSA2011, model = "pooling")
m.ff3.pooled3 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE-1, data = dataUSA2011, model = "pooling")

bptest(m.ff3.fe)
bptest(m.ff3.fe1)
bptest(m.ff3.fe2)
bptest(m.ff3.fe3)

bptest(m.ff3.pooled)
bptest(m.ff3.pooled1)
bptest(m.ff3.pooled2)
bptest(m.ff3.pooled3)
#нужны робастные ошибки




summary(m.ff3.fe);coeftest(m.ff3.fe, vcovHC)
summary(m.ff3.fe1);coeftest(m.ff3.fe1, vcovHC)
summary(m.ff3.fe2);coeftest(m.ff3.fe2, vcovHC)
summary(m.ff3.fe3);coeftest(m.ff3.fe3, vcovHC)



test1 <- waldtest(m.ff3.fe3, m.ff3.fe2) 
test1 #Контроль на эффективность и долговую нагрузку по факту не нужен
test2 <- waldtest(m.ff3.pooled3, m.ff3.pooled2) 
test2 


summary(m.ff3.pooled);coeftest(m.ff3.pooled, vcovHC)
summary(m.ff3.pooled1);coeftest(m.ff3.pooled1, vcovHC)
summary(m.ff3.pooled2);coeftest(m.ff3.pooled2, vcovHC)
summary(m.ff3.pooled3);coeftest(m.ff3.pooled3, vcovHC)

pdwtest(m.ff3.fe3)
pdwtest(m.ff3.pooled3)
#серийная корреляция отсутствует


pFtest(m.ff3.fe2,m.ff3.pooled2)



#сравним с пяти-факторной
m.ff5.fe <- plm(RIRF ~ RMRF + SMB + HML+RMW+CMA+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE, data = dataUSA2011, model = "within")
m.ff5.pooled <- plm(RIRF ~ RMRF + SMB + HML+RMW+CMA+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE-1, data = dataUSA2011, model = "pooling")

summary(m.ff5.fe);coeftest(m.ff5.fe, vcovHC)
summary(m.ff5.pooled);coeftest(m.ff5.pooled, vcovHC)

bptest(m.ff5.fe)
bptest(m.ff5.pooled)

pFtest(m.ff5.fe,m.ff5.pooled)


stargazer(m.ff3.fe,m.ff3.fe1,m.ff3.fe2,m.ff3.fe3,m.ff5.fe, type = "text", out = "fe_heterosc.html",summary = FALSE)
stargazer(coeftest(m.ff3.fe, vcovHC),coeftest(m.ff3.fe1, vcovHC),coeftest(m.ff3.fe2, vcovHC),coeftest(m.ff3.fe3, vcovHC), coeftest(m.ff5.fe, vcovHC),type = "text", out = "fe_robust.html",summary = FALSE)

stargazer(m.ff3.pooled,m.ff3.pooled1,m.ff3.pooled2,m.ff3.pooled3,m.ff5.pooled, type = "text", out = "pooled_heterosc.html",summary = FALSE)
stargazer(coeftest(m.ff3.pooled, vcovHC),coeftest(m.ff3.pooled1, vcovHC),coeftest(m.ff3.pooled2, vcovHC),coeftest(m.ff3.pooled3, vcovHC), coeftest(m.ff5.pooled, vcovHC),type = "text", out = "pooled_robust.html",summary = FALSE)


#То же самое смотрю для UK
dataUK <- pdata.frame(Three_factor_FF_UK,
                       index = c("Company","Year"),
                       row.names = TRUE)
dataUK2011 <- na.omit(dataUK)

m.ff3.feUK <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth, data = dataUK2011, model = "within")
m.ff3.feUK1 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank, data = dataUK2011, model = "within")
m.ff3.feUK2 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others, data = dataUK2011, model = "within")
m.ff3.feUK3 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE, data = dataUK2011, model = "within")


summary(m.ff3.feUK);coeftest(m.ff3.feUK, vcovHC)
summary(m.ff3.feUK1);coeftest(m.ff3.feUK1, vcovHC)
summary(m.ff3.feUK2);coeftest(m.ff3.feUK2, vcovHC)
summary(m.ff3.feUK3);coeftest(m.ff3.feUK3, vcovHC)

m.ff3.pooledUK <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth-1, data = dataUK2011, model = "pooling")
m.ff3.pooledUK1 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank-1, data = dataUK2011, model = "pooling")
m.ff3.pooledUK2 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others-1, data = dataUK2011, model = "pooling")
m.ff3.pooledUK3 <- plm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE-1, data = dataUK2011, model = "pooling")


summary(m.ff3.pooledUK);coeftest(m.ff3.pooledUK, vcovHC)
summary(m.ff3.pooledUK1);coeftest(m.ff3.pooledUK1, vcovHC)
summary(m.ff3.pooledUK2);coeftest(m.ff3.pooledUK2, vcovHC)
summary(m.ff3.pooledUK3);coeftest(m.ff3.pooledUK3, vcovHC)

pdwtest(m.ff3.feUK3)
pdwtest(m.ff3.pooledUK3)
#серийная корреляция отсутствует

bptest(m.ff3.feUK)
bptest(m.ff3.feUK1)
bptest(m.ff3.feUK2)
bptest(m.ff3.feUK3)

bptest(m.ff3.pooledUK)
bptest(m.ff3.pooledUK1)
bptest(m.ff3.pooledUK2)
bptest(m.ff3.pooledUK3)

test3 <- waldtest(m.ff3.pooledUK3, m.ff3.pooledUK2) 
test3 #Контроль на эффективность и долговую нагрузку по факту не нужен


pFtest(m.ff3.feUK2,m.ff3.pooledUK2)

stargazer(m.ff3.feUK,m.ff3.feUK1,m.ff3.feUK2,m.ff3.feUK3, type = "text", out = "fe_heterosc_UK.html",summary = FALSE)
stargazer(coeftest(m.ff3.feUK, vcovHC),coeftest(m.ff3.feUK1, vcovHC),coeftest(m.ff3.feUK2, vcovHC),coeftest(m.ff3.feUK3, vcovHC),type = "text", out = "fe_robust_UK.html",summary = FALSE)

stargazer(m.ff3.pooledUK,m.ff3.pooledUK1,m.ff3.pooledUK2,m.ff3.pooledUK3, type = "text", out = "pooled_heterosc_UK.html",summary = FALSE)
stargazer(coeftest(m.ff3.pooledUK, vcovHC),coeftest(m.ff3.pooledUK1, vcovHC),coeftest(m.ff3.pooledUK2, vcovHC),coeftest(m.ff3.pooledUK3, vcovHC),type = "text", out = "pooled_robust_UK.html",summary = FALSE)



#Общие выводы на данном этапе: 
# 1. На американских данных существует значимый эффект как для темпов роста объемов финансирования, так и для темпов роста числа сделок.
# 2. Для американских банков справедливо, что ускорение темпов роста финансирования финтехстартапов ведет к увеличению доходности акций банков
# 3. Но если начинает расти частота сделок - доходность снижается. Возможная причина - растущая конкуренция между банками (возможности банков уравниваются)
# 4. В целом, как 3-х, так и 5-ти факторная модель хорошо применима к американским данным и подверждает выводы выше

# 5. Судя по тому, что результаты отличаются для британских банков, их специфика ведения бизнеса по-прежнеиму другая, чем в США
# 6. Модель ФФ не работает, но что интересно, темп роста числа сделок оказывает положительное ЗНАЧИМОЕ влияние на доходность акций.
# 7. Из вывода 6 следует, что банки Великобритании менее цифровизированы, возможно находятся на более ранних стадиях цифровизации, чем в США.


#Проверка спецификации для моделей
#3-FF USA
modff3usa <- lm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE-1, data = Three_five_factor_FF_USA)
summary(modff3usa)
coeftest(modff3usa,vcovHC)
#Контроль на размер не дает значимости, проведем тест на короткую против длинной
modff3usa_res <- lm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth-1, data = Three_five_factor_FF_USA)

test1 <- waldtest(modff3usa, modff3usa_res) 
#Принимаем нулевую гипотезу, короткая модель лучше
summary(modff3usa_res)

plot(modff3usa, which = 2,
     main = "Распределение остатков регрессии. 3-х факторная модель Фама-Френча", 
     xlab = "Theoretical quantilies",
     pch=16, col="blue")

e <- resid(modff3usa)
data_e <- data.frame(e)
ggplot(data = data_e, aes(e)) + geom_density(fill = "blue", alpha = 0.65)+
  xlab("Residuals") + ylab("Density")+theme_light()+ggtitle("Плотность распределения остатков 3-х факторной модели \nФама-Френча для американских банков") + theme(plot.title = element_text(hjust= 0.5))

plot(modff3usa, which = 1,
     main = "Зависимость между остатками и прогнозными значениями модели", 
     pch=16, col="blue")
#Степени не пропущены
crPlots(modff3usa) 
resettest(modff3usa) 
#Тест Рамсея говорит о верной спецификации модели



bptest(modff3usa) 
#Гетероскедастичность есть
plot(modff3usa_res, which = 3,
     main = "Визуальная диагностика наличия гетероскедастичности в модели"
     ,
     pch=16, col="blue") 
#Действительно, для более высоких доходностей характерно увеличение стандартного отклонения, используем робастные ошибки

coeftest(modff3usa, vcovHC)


#5-FF USA
modff5usa <- lm(RIRF ~ RMRF + SMB + HML+RMW+CMA+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE-1, data = Three_five_factor_FF_USA)
summary(modff5usa)
#Контроль на размер так же не дает значимости, проведем тест на короткую против длинной
modff5usa_res <- lm(RIRF ~ RMRF + SMB + HML+RMW+CMA+Funding_growth+Deals_growth-1, data = Three_five_factor_FF_USA)

test2 <- waldtest(modff5usa, modff5usa_res) 
#Принимаем нулевую гипотезу, короткая модель лучше
summary(modff5usa_res)

plot(modff5usa, which = 2,
     main = "Распределение остатков регрессии. \n5-ти факторная модель Фама-Френча", 
     xlab = "Theoretical quantilies",
     pch=16, col="blue")

e <- resid(modff5usa)
data_e <- data.frame(e)
ggplot(data = data_e, aes(e)) + geom_density(fill = "blue", alpha = 0.65)+
  xlab("Residuals") + ylab("Density")+theme_light()+ggtitle("Плотность распределения остатков 5-ти факторной модели \nФама-Френча для американских банков") + theme(plot.title = element_text(hjust= 0.5))

plot(modff5usa, which = 1,
     main = "Зависимость между остатками и прогнозными значениями модели. \n5-факторная модель Фама-Френча", 
     pch=16, col="blue")
#Степени не пропущены
crPlots(modff5usa) 
resettest(modff5usa) 
#Тест Рамсея говорит о верной спецификации модели

bptest(modff5usa) 
#Гетероскедастичность есть
plot(modff5usa_res, which = 3,
     main = "Визуальная диагностика наличия гетероскедастичности в модели"
     ,
     pch=16, col="blue") 
#Для более высоких доходностей опять характерно увеличение стандартного отклонения, используем робастные ошибки

coeftest(modff5usa, vcovHC)


#3-FF UK
modff3uk <- lm(RIRF ~ RMRF + SMB + HML+Funding_growth+Deals_growth+BigBank+MA_others+Debt_to_Capital+ROE-1, data = Three_factor_FF_UK)
summary(modff3uk)


plot(modff3uk, which = 2,
     main = "Распределение остатков регрессии. Банки Великобритании", 
     xlab = "Theoretical quantilies",
     pch=16, col="darkgreen")

e <- resid(modff3uk)
data_e <- data.frame(e)
ggplot(data = data_e, aes(e)) + geom_density(fill = "darkgreen", alpha = 0.65)+
  xlab("Residuals") + ylab("Density")+theme_light()+ggtitle("Плотность распределения остатков 3-х факторной модели \nФама-Френча для британских банков") + theme(plot.title = element_text(hjust= 0.5))
#В левом хвосте распределения больше плотности, чем в предыдущих моделях, больше похоже на стьюдента.


plot(modff3uk, which = 1,
     main = "Зависимость между остатками и \nпрогнозными значениями модели. Великобритания", 
     pch=16, col="darkgreen")
#Пропущен куб?
crPlots(modff3uk) 
resettest(modff3uk) 
#Тест Рамсея говорит, что спецификация почти плохая, но гипотезу принять можно


e <- resid(modff3uk)
g <- abs(e/sd(e))
barplot(g, main = "Выявление наблюдений-выбросов", cex.axis = 1) 
abline(a = 1.96, b =0,
       col= "red")

N <- which(g > 1.96)
N 
plot(modff3uk,which=4,
     main = "Выявление влиятельных наблюдений",
     col="blue")
abline(a = 4/117, b=0, 
       col= "red")

d <- cooks.distance(modff3uk)
D <- which(d > 4/117)
D

New_data <- Three_factor_FF_UK[-D,]
mod55 <- update(modff3uk,data=New_data)
summary(mod55);coeftest(mod55,vcovHC)

#особого смысла удалять влиятельные наблюдения не было


bptest(modff3uk) 
#Гетероскедастичности нет (формально)
plot(modff3uk, which = 3,
     main = "Визуальная диагностика наличия гетероскедастичности в модели"
     ,
     pch=16, col="darkgreen") 
#Из графика видно, что есть

coeftest(modff3uk, vcovHC)


################ Оценка влияния на операционные показатели ##############

names(USA_clustered)

data <- na.omit(USA_clustered)
data <- data[,-c(1,2)]
c <- cor(data)
corrplot(c)

model1 <- lm(Credit_portf ~ Funding, data = USA_clustered)
summary(model1)
vif(model1)
#все не очень хорошо, надо проверить спецификацию, так как есть переменные на грани значимости
plot(model1, which = 2)
plot(model1, which =1)
#Нужно логарифмировать, посмотрим, где могут быть ошибки в спецификации

crPlots(model1)
#Нужен логарифм кредитного портфеля и депозитов
#Существуют нулевые значения переменных, нужно отфильтровать выборку

data <- filter(USA_clustered, Credit_portf > 0 & Deposit_portf > 0)
model2 <- lm(EBITDA ~ log(Credit_portf) + log(Deposit_portf) + Debt_to_Capital+I(BigBank*Funding)+Deals+BigBank+MediumBank, data = data)
summary(model2)
plot(model2, which =1)


################ Event-study ###########################

#Bank of New York Mellon Corp
#Покупка PNC Global Investment
BNY <- getSymbols("BK", from = "2009-02-12", to = "2010-02-12", 
                   auto.assign = FALSE)
SP500 <- getSymbols("^GSPC", from = "2009-02-12", to = "2010-02-12", 
                  auto.assign = FALSE)
quotes <- data.frame(BNY$BK.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[244:251,]
returns <- as.xts(returns)
#Дата события - 2 февраля (244 день выборки)
data_train <- returns[1:233,]
data_test <- returns[234:251,]
plot(data_test$BK.Close,type="b", main = "Bank of New York Mellon Corp\n2 февраля 2010. Покупка PNC Global Investment")
#Визуально ничего особенного
mod <- lm(BK.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$BK.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$BK.Close.1,type="b",main = "Bank of New York Mellon Corp\n2 февраля 2010. Покупка PNC Global Investment")
#Видим сильную отрицательную abnormal return в 11 день
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$BK.Close.2,type="b",main = "Bank of New York Mellon Corp\n2 февраля 2010. Покупка PNC Global Investment")
data_test_3$BK.Close.2[11]
qt(0.975, 233)
#Отклонились на 1,2 сигмы влево (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$BK.Close.1[1:18])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[18]-CAR$CAR[8])/(sqrt(10)*SD)
qt(0.975, 235)

#Покупка Cutwater Asset Management
BNY <- getSymbols("BK", from = "2014-01-15", to = "2015-01-15", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC", from = "2014-01-15", to = "2015-01-15", 
                    auto.assign = FALSE)
quotes <- data.frame(BNY$BK.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[244:251,]
returns <- as.xts(returns)
#Дата события - 5 января 2015 (244 день выборки)
data_train <- returns[1:233,]
data_test <- returns[234:251,]
plot(data_test$BK.Close,type="b", main = "Bank of New York Mellon Corp\n5 янвраля 2015. Покупка Cutwater Asset Management")
#Визуально ничего особенного
mod <- lm(BK.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$BK.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$BK.Close.1,type="b","Bank of New York Mellon Corp\n5 янвраля 2015. Покупка Cutwater Asset Management")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$BK.Close.2,type="b","Bank of New York Mellon Corp\n5 янвраля 2015. Покупка Cutwater Asset Management")
data_test_3$BK.Close.2[11]
qt(0.975, 243)
#На сигму влево (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$BK.Close.1[1:18])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[18]-CAR$CAR[8])
(CAR$CAR[18]-CAR$CAR[8])/(sqrt(10)*SD)
qt(0.975, 235)


#Покупка DBV-X
BNY <- getSymbols("BK", from = "2015-05-06", to = "2016-05-06", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC", from = "2015-05-06", to = "2016-05-06", 
                    auto.assign = FALSE)
quotes <- data.frame(BNY$BK.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[245:252,]
returns <- as.xts(returns)
#Дата события - 26 апреля (246 день выборки)
data_train <- returns[1:235,]
data_test <- returns[236:252,]
plot(data_test$BK.Close,type="b",main = "Bank of New York Mellon Corp\n26 апреля 2016. Покупка DBV-X")
#Визуально ничего особенного
mod <- lm(BK.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$BK.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$BK.Close.1,type="b",main = "Bank of New York Mellon Corp\n26 апреля 2016. Покупка DBV-X")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$BK.Close.2,type="b",main = "Bank of New York Mellon Corp\n26 апреля 2016. Покупка DBV-X")
data_test_3$BK.Close.2[11]
qt(0.975, 235)
#Не отклонились(гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$BK.Close.1[1:17])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[17]-CAR$CAR[8])
(CAR$CAR[17]-CAR$CAR[8])/(sqrt(9)*SD)
qt(0.975, 235)

#Capital One
#Покупка ING Direct
CO <- getSymbols("COF", from = "2011-02-27", to = "2012-02-27", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC", from = "2011-02-27", to = "2012-02-27", 
                    auto.assign = FALSE)
quotes <- data.frame(CO$COF.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[246:250,]
returns <- as.xts(returns)
#Дата события - 17 февраля (246 день выборки)
data_train <- returns[1:235,]
data_test <- returns[236:250,]
plot(data_test$COF.Close,type="b",main = "Capital One\n17 февраля 2012. Покупка ING Direct")
#Визуально ничего особенного
mod <- lm(COF.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$COF.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$COF.Close.1,type="b",main = "Capital One\n17 февраля 2012. Покупка ING Direct")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$COF.Close.2,type="b",main = "Capital One\n17 февраля 2012. Покупка ING Direct")
data_test_3$COF.Close.2[11]
qt(0.975, 235)
#Почти на сигму влево (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$COF.Close.1[1:15])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[15]-CAR$CAR[8])
(CAR$CAR[15]-CAR$CAR[8])/(sqrt(7)*SD)
qt(0.975, 235)


#Покупка Level Money
CO <- getSymbols("COF", from = "2014-01-22", to = "2015-01-22", 
                 auto.assign = FALSE)
SP500 <- getSymbols("^GSPC", from = "2014-01-22", to = "2015-01-22", 
                    auto.assign = FALSE)
quotes <- data.frame(CO$COF.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[245:251,]
returns <- as.xts(returns)
#Дата события -12 января (245 день выборки)
data_train <- returns[1:234,]
data_test <- returns[235:251,]
plot(data_test$COF.Close,type="b", main = "Capital One\n12 января 2015. Покупка Level Money")
#Визуально в 11 день ничего особенного
mod <- lm(COF.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$COF.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$COF.Close.1,type="b",main = "Capital One\n12 января 2015. Покупка Level Money")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$COF.Close.2,type="b",main = "Capital One\n12 января 2015. Покупка Level Money")
data_test_3$COF.Close.2[11]
qt(0.975, 234)
#На 0,66 сигмы вправо (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$COF.Close.1[1:17])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[17]-CAR$CAR[8])
(CAR$CAR[17]-CAR$CAR[8])/(sqrt(9)*SD)
qt(0.975, 235)

#Покупка Notch
CO <- getSymbols("COF", from = "2017-01-15", to = "2018-01-15", 
                 auto.assign = FALSE)
SP500 <- getSymbols("^GSPC", from = "2017-01-15", to = "2018-01-15", 
                    auto.assign = FALSE)
quotes <- data.frame(CO$COF.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[245:250,]
returns <- as.xts(returns)
#Дата события - 5 января (245 день выборки)
data_train <- returns[1:234,]
data_test <- returns[235:250,]
plot(data_test$COF.Close,type="b",main = "Capital One\n5 января 2018. Покупка Notch")
#Визуально в 11 день ничего особенного. Падение после отскока
mod <- lm(COF.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$COF.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$COF.Close.1,type="b",main = "Capital One\n5 января 2015. Покупка Notch")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$COF.Close.2,type="b",main = "Capital One\n5 января 2015. Покупка Notch")
data_test_3$COF.Close.2[11]
qt(0.975, 234)
#отклонились на 0,74 сигмы влево (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$COF.Close.1[1:16])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[16]-CAR$CAR[8])
(CAR$CAR[16]-CAR$CAR[8])/(sqrt(8)*SD)
qt(0.975, 235)

#First Republic Bank
#Покупка Gradifi, Inc
FRC <- getSymbols("FRC", from = "2015-12-22", to = "2016-12-22", 
                 auto.assign = FALSE)
SP500 <- getSymbols("^GSPC",  from = "2015-12-22", to = "2016-12-22",  
                    auto.assign = FALSE)
quotes <- data.frame(FRC$FRC.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[245:252,]
returns <- as.xts(returns)
#Дата события - 12 декабря (245 день выборки)
data_train <- returns[1:234,]
data_test <- returns[235:252,]
plot(data_test$FRC.Close,type="b",main = "First Republic Bank\n5 января 2015. Покупка Gradifi, Inc")
#Визуально в 11 день ничего особенного
mod <- lm(FRC.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$FRC.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$FRC.Close.1,type="b",main = "First Republic Bank\n5 января 2015. Покупка Gradifi, Inc")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$FRC.Close.2,type="b",main = "First Republic Bank\n5 января 2015. Покупка Gradifi, Inc")
data_test_3$FRC.Close.2[11]
qt(0.975, 234)
#Отклонились на 0,71 влево (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$FRC.Close.1[1:18])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[18]-CAR$CAR[8])
(CAR$CAR[18]-CAR$CAR[8])/(sqrt(10)*SD)
qt(0.975, 235)

#JP Morgan Chase 
#Покупка MCX 
JPM <- getSymbols("JPM", from = "2016-03-20", to = "2017-03-20", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC",  from = "2016-03-20", to = "2017-03-20", 
                    auto.assign = FALSE)
quotes <- data.frame(JPM$JPM.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[245:250,]
returns <- as.xts(returns)
#Дата события - 10 марта (245 день выборки)
data_train <- returns[1:234,]
data_test <- returns[235:250,]
plot(data_test$JPM.Close,type="b",main = "JP Morgan Chase\n10 марта 2017. Покупка MCX ")
#Визуально в 11 день ничего особенного
mod <- lm(JPM.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$JPM.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$JPM.Close.1,type="b",main = "JP Morgan Chase\n10 марта 2017. Покупка MCX ")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$JPM.Close.2,type="b",main = "JP Morgan Chase\n10 марта 2017. Покупка MCX")
data_test_3$JPM.Close.2[11]
qt(0.975, 234)
#Отклонились на сигму влево (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$JPM.Close.1[1:16])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[16]-CAR$CAR[8])
(CAR$CAR[16]-CAR$CAR[8])/(sqrt(8)*SD)
qt(0.975, 235)

#Покупка WePay
JPM <- getSymbols("JPM", from = "2016-10-27", to = "2017-10-27", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC",  from =  "2016-10-27", to = "2017-10-27",
                    auto.assign = FALSE)
quotes <- data.frame(JPM$JPM.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[244:251,]
returns <- as.xts(returns)
#Дата события - 17 октября (244 день выборки)
data_train <- returns[1:233,]
data_test <- returns[234:251,]
plot(data_test$JPM.Close,type="b",main = "JP Morgan Chase\n17 октября 2017. Покупка WePay")
#Визуально в 11 день ничего особенного
mod <- lm(JPM.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$JPM.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$JPM.Close.1,type="b",main = "JP Morgan Chase\n17 октября 2017. Покупка WePay")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$JPM.Close.2,type="b")
data_test_3$JPM.Close.2[11]
qt(0.975, 234)
#Почти не отклонились (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$JPM.Close.1[1:18])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[18]-CAR$CAR[8])
(CAR$CAR[18]-CAR$CAR[8])/(sqrt(10)*SD)
qt(0.975, 235)

#Покупка InstaMed
JPM <- getSymbols("JPM", from = "2018-05-27", to = "2019-05-27", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC",  from = "2018-05-27", to = "2019-05-27", 
                    auto.assign = FALSE)
quotes <- data.frame(JPM$JPM.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[244:249,]
returns <- as.xts(returns)
#Дата события - 17 октября (244 день выборки)
data_train <- returns[1:233,]
data_test <- returns[234:249,]
plot(data_test$JPM.Close,type="b",main = "JP Morgan Chase\n17 октября 2019. Покупка InstaMed")
#Визуально в 11 день ничего особенного
mod <- lm(JPM.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$JPM.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$JPM.Close.1,type="b",main = "JP Morgan Chase\n17 октября 2019. Покупка InstaMed")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$JPM.Close.2,type="b",main = "JP Morgan Chase\n17 октября 2019. Покупка InstaMed")
data_test_3$JPM.Close.2[11]
qt(0.975, 234)
#Почти не отклонились (гипотеза о равентсве средних принимается)
CAR <- cumsum(data_test_3$JPM.Close.1[1:16])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[16]-CAR$CAR[8])
(CAR$CAR[16]-CAR$CAR[8])/(sqrt(10)*SD)
qt(0.975, 235)


#Toronto Dominion Bank
#Покупка Layer 6 AI
TD <- getSymbols("TD", from = "2017-01-19", to = "2018-01-19", 
                  auto.assign = FALSE)
SP500 <- getSymbols("^GSPC",  from = "2017-01-19", to = "2018-01-19", 
                    auto.assign = FALSE)
quotes <- data.frame(TD$TD.Close,SP500$GSPC.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[245:251,]
returns <- as.xts(returns)
#Дата события - 9 января (245 день выборки)
data_train <- returns[1:234,]
data_test <- returns[235:251,]
plot(data_test$TD.Close,type="b",main = "Toronto Dominion Bank\n9 января 2018. Покупка Layer 6 AI")
#Падение доходности
mod <- lm(TD.Close ~ GSPC.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$GSPC.Close
Abnormal_returns <- data_test$TD.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$TD.Close.1,type="b",main = "Toronto Dominion Bank\n9 января 2018. Покупка Layer 6 AI")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$TD.Close.2,type="b",main = "Toronto Dominion Bank\n9 января 2018. Покупка Layer 6 AI")
data_test_3$TD.Close.2[11]
qt(0.975, 234)
#Отклонились почти на 1,8 сигм влево - событие негативное, но гипотеза о равентсве средних принимается
CAR <- cumsum(data_test_3$TD.Close.1[1:17])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[17]-CAR$CAR[8])
(CAR$CAR[17]-CAR$CAR[8])/(sqrt(9)*SD)
qt(0.975, 235)


#Barclays
#Покупка Analog analytics
BARC <- getSymbols("BARC.L", from = "2011-06-14", to = "2012-06-14", 
                 auto.assign = FALSE)
FTSE <- getSymbols("^FTSE",  from = "2011-06-14", to = "2012-06-14",
                    auto.assign = FALSE)
quotes <- data.frame(BARC$BARC.L.Close,FTSE$FTSE.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[246:251,]
returns <- as.xts(returns)
#Дата события - 4 июня - неторговый день - ближайшая дата 6 июня (246 день выборки)
data_train <- returns[1:235,]
data_test <- returns[236:251,]
plot(data_test$BARC.L.Close,type="b",main = "Barclays\n4 июня 2012. Покупка Analog analytics")
#Рост на 8%!!!!
mod <- lm(BARC.L.Close ~ FTSE.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$FTSE.Close
Abnormal_returns <- data_test$BARC.L.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$BARC.L.Close.1,type="b",main = "Barclays\n4 июня 2012. Покупка Analog analytics")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$BARC.L.Close.2,type="b")
data_test_3$BARC.L.Close.2[11]
qt(0.975, 235)
#Отклонились почти на 1,1 сигм вправо - несмотря на то, что доходность высокая, избыточная доходность входит в доверительный интервал. Гипотеза о равентсве средних принимается
CAR <- cumsum(data_test_3$BARC.L.Close.1[1:16])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[16]-CAR$CAR[8])
(CAR$CAR[16]-CAR$CAR[8])/(sqrt(8)*SD)
qt(0.975, 235)

#Natwest
#Покупка FreeAgent
NWG <- getSymbols("NWG.L", from = "2017-04-07", to = "2018-04-07", 
                   auto.assign = FALSE)
FTSE <- getSymbols("^FTSE",  from = "2017-04-07", to = "2018-04-07", 
                   auto.assign = FALSE)
quotes <- data.frame(NWG$NWG.L.Close,FTSE$FTSE.Close)
returns <- Return.calculate(quotes, method = "log")
returns <- na.omit(returns)
dim(returns)
returns[244:250,]
returns <- as.xts(returns)
#Дата события - 27 марта (244 день выборки)
data_train <- returns[1:233,]
data_test <- returns[234:250,]
plot(data_test$NWG.L.Close,type="b",main = "Natwest\n27 марта 2018. Покупка FreeAgent")
#Ничего особенного
mod <- lm(NWG.L.Close ~ FTSE.Close,data = data_train)
summary(mod)
k1 <- coef(mod)[1]
k2 <- coef(mod)[2]
e <- resid(mod)
SD <- sd(e)
Pr <- k1 + k2*data_test$FTSE.Close
Abnormal_returns <- data_test$NWG.L.Close - Pr
data_test_2 <- data.frame(data_test, Abnormal_returns)
data_test_2 <- as.xts(data_test_2)
plot(data_test_2$NWG.L.Close.1,type="b",main = "Natwest\n27 марта 2018. Покупка FreeAgent")
data_test_3 <- data.frame(data_test_2, Abnormal_returns_sd = Abnormal_returns/SD)
data_test_3 <- as.xts(data_test_3)
plot(data_test_3$NWG.L.Close.2,type="b",main = "Natwest\n27 марта 2018. Покупка FreeAgent")
data_test_3$NWG.L.Close.2[11]
qt(0.975, 235)
#Отклонились почти на 1,1 сигм влево - гипотеза о равентсве средних принимается
CAR <- cumsum(data_test_3$NWG.L.Close.1[1:17])
plot(CAR)
CAR <- data.frame(CAR)
colnames(CAR) <- c("CAR")
(CAR$CAR[17]-CAR$CAR[8])
(CAR$CAR[17]-CAR$CAR[8])/(sqrt(9)*SD)
qt(0.975, 235)

#Ни один кейс не показал, что сам факт покупки стартапа банком оказывает значимое влияние на доходность акций банка
#Вероятно, это связано с тем, что к соответствующей дате участники торгов заложили в цену акций покупку стартапа
#Следовательно, рыночные ожидания уже отражены в доходности акций банка. Возможно, более ранние даты (появление инсайтов о вероятной покупке или предварительные переговоры являлись бустом для цены акций)
#Однако этот рост не имеет общего с увеличением внутренней стоимости и бизнеса и вызван исключительно новостным фоном.


#_____________________Портфели_____________________
#Скачиваем котировки всех банков за последние 2021 год - более широкий тайм-фрейм не обеспечит нас постоянной дисперсией доходности.
#Банки, которые заключали сделки M&A со стартапами
BNY <- getSymbols("BK", from = "2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE)
CO <- getSymbols("COF", from = "2021-01-01", to = "2021-12-31", 
                 auto.assign = FALSE)
FRC <- getSymbols("FRC", from = "2021-01-01", to = "2021-12-31",  
                  auto.assign = FALSE)
JPM <- getSymbols("JPM", from = "2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE)
TD <- getSymbols("TD", from = "2021-01-01", to = "2021-12-31", 
                 auto.assign = FALSE)
BARC <- getSymbols("BARC.L", from = "2021-01-01", to = "2021-12-31",  
                   auto.assign = FALSE)
NWG <- getSymbols("NWG.L", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE)
#Банки, которые не заключали M&A с финтехами, но другими банками или финансовыми компаниями, а так же e-commerce и т.д.
BOKF <- getSymbols("BOKF", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #BOK Financial Corp
CFG <- getSymbols("CFG", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Citizens Financial Group Inc
CMA <- getSymbols("CMA", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Comerica Inc
EWBC <- getSymbols("EWBC", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #East West Bancorp Inc
FITB <- getSymbols("FITB", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Fifth Third Bancorp
#IBERIABANK Ord Shs - не включается, эмитент произвел делистинг к 2021 году
IBTX <- getSymbols("IBTX", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Independent Bank Group Inc
MTB <- getSymbols("MTB", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #M&T Bank Corp
PRK <- getSymbols("PRK", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Park National Corp
#People's United Financial Inc - не включается, делистинг к 2022
PNC <- getSymbols("PNC", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #PNC Financial Services Group Inc
PB <- getSymbols("PB", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Prosperity Bancshares Inc
SNV <- getSymbols("SNV", from ="2021-01-01", to = "2021-12-31", 
                 auto.assign = FALSE) #Synovus Financial Corp. 
TFC <- getSymbols("TFC", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Truist Financial Corp
TRMK <- getSymbols("TRMK", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Trustmark Corp
UMBF <- getSymbols("UMBF", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #UMB Financial Corp
UMPQ <- getSymbols("UMPQ", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Umpqua Holdings Corp
UCBI <- getSymbols("UCBI", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #United Community Banks Inc
VLY <- getSymbols("VLY", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Valley National Bancorp
WFC <- getSymbols("WFC", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Wells Fargo & Co
WSBC <- getSymbols("WSBC", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #WesBanco Inc
CBG.L <- getSymbols("CBG.L", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Close Brothers Group PLC
LLOY.L <- getSymbols("LLOY.L", from ="2021-01-01", to = "2021-12-31", 
                    auto.assign = FALSE) #Lloyds Banking Group PLC 
#PCF Group PLC - не включается
STAN.L <- getSymbols("STAN.L", from ="2021-01-01", to = "2021-12-31", 
                     auto.assign = FALSE) #Standard Chartered PLC
#Банки, которые не совершали M&A ни с какими компаниями
AUB <- getSymbols("AUB", from ="2021-01-01", to = "2021-12-31", 
                     auto.assign = FALSE) #Atlantic Union Bankshares Corp
BAC <- getSymbols("BAC", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Bank of America Corp
C <- getSymbols("C", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Citigroup Inc
FFIN <- getSymbols("FFIN", from ="2021-01-01", to = "2021-12-31", 
                auto.assign = FALSE) #First Financial Bankshares Inc
HSBC <- getSymbols("HSBC", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #HSBC Holdings PLC
NTRS <- getSymbols("NTRS", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Northern Trust Corp
NWBI <- getSymbols("NWBI", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Northwest Bancshares Inc
PNBI <- getSymbols("PNBI", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Pioneer Bankshares Inc
WAFD <- getSymbols("WAFD", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Washington Federal Inc
WBS <- getSymbols("WBS", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Webster Financial Corp
ZION <- getSymbols("ZION", from ="2021-01-01", to = "2021-12-31", 
                  auto.assign = FALSE) #Zions Bancorporation NA
ARBB.L <- getSymbols("ARBB.L", from ="2021-01-01", to = "2021-12-31", 
                   auto.assign = FALSE) #Arbuthnot Banking Group PLC
BIRG.L <- getSymbols("BIRG.L", from ="2021-01-01", to = "2021-12-31", 
                     auto.assign = FALSE) #Bank of Ireland Group PLC
PAG.L <- getSymbols("PAG.L", from ="2021-01-01", to = "2021-12-31", 
                     auto.assign = FALSE) #Paragon Banking Group PLC
PFG.L <- getSymbols("PFG.L", from ="2021-01-01", to = "2021-12-31", 
                    auto.assign = FALSE) #Provident Financial PLC
STB.L <- getSymbols("STB.L", from ="2021-01-01", to = "2021-12-31", 
                    auto.assign = FALSE) #Secure Trust Bank PLC
VMUK.L <- getSymbols("VMUK.L", from ="2021-01-01", to = "2021-12-31", 
                    auto.assign = FALSE) #Virgin Money UK PLC
close_prices <- merge(BNY$BK.Close,CO$COF.Close,FRC$FRC.Close,JPM$JPM.Close,TD$TD.Close,BARC$BARC.L.Close,NWG$NWG.L.Close,
                      BOKF$BOKF.Close,CFG$CFG.Close,CMA$CMA.Close,EWBC$EWBC.Close,FITB$FITB.Close,IBTX$IBTX.Close,MTB$MTB.Close,PRK$PRK.Close,PNC$PNC.Close,PB$PB.Close,SNV$SNV.Close,TFC$TFC.Close,TRMK$TRMK.Close,UMBF$UMBF.Close,UMPQ$UMPQ.Close,UCBI$UCBI.Close,VLY$VLY.Close,WFC$WFC.Close,WSBC$WSBC.Close,CBG.L$CBG.L.Close,LLOY.L$LLOY.L.Close,STAN.L$STAN.L.Close,
                      AUB$AUB.Close,BAC$BAC.Close,C$C.Close,FFIN$FFIN.Close,HSBC$HSBC.Close,NTRS$NTRS.Close,NWBI$NWBI.Close,PNBI$PNBI.Close,WAFD$WAFD.Close,WBS$WBS.Close,ZION$ZION.Close,ARBB.L$ARBB.L.Close,BIRG.L$BIRG.L.Close,PAG.L$PAG.L.Close,PFG.L$PFG.L.Close,STB.L$STB.L.Close,VMUK.L$VMUK.L.Close)
close_prices <- na.omit(close_prices)
close_prices <- as.xts(close_prices)
colnames(close_prices) <- c("BNY","COF","FRC","JPM","TD","BARC","NWG",
                            "BOKF","CFG","CMA","EWBC","FITB","IBTX","MTB","PRK","PNC","PB","SNV","TFC","TRMK","UMBF","UMPQ","UCBI","VLY","WFC","WSBC","CBG","LLOY","STAN",
                            "AUB","BAC","C","FFIN","HSBC","NTRS","NWBI","PNBI","WAFD","WBS","ZION","ARBB","BIRG","PAG","PFG","STB","VMUK")
r <- Return.calculate(close_prices, method = "log")
r <- na.omit(r)
#Убедимся, что скользящее стандарное отклонение умеренно волатильно
chart.RollingPerformance(r, width = 50, FUN = "sd", main = "50-дневное скользящее стандартное отклонение \nвсех выбранных банков")
#В целом, удовлетворительно, можно строить портфели
c <- cor(r)
corrgram(c,order = TRUE, main = "Корреляционная матрица выбранных активов")

#Допустимое множество портфелей
port <- portfolio.spec(colnames(r))
port <- add.constraint(port, type = "leverage", min_sum = 0.99, max_sum = 1.01)
port <- add.constraint(port, type = "box", min = -1, max = 1)
port_random <- random_portfolios(port, permutations = 250000, rp_method = "sample")
return_mean <- apply(port_random, MARGIN = 1, function(x) {mean(r %*% x)})
return_sd <- apply(port_random, MARGIN = 1, function(x) {StdDev(r, weights = x)})
plot(return_sd, return_mean, main = "Допустимое множество портфелей из выбранных активов",
     xlab = "Standart Deviation", ylab = "Expected Return", col = "black")

#Разобьем банки на группы
Fintech_MnA <- r[,1:7]
Other_MnA <- r[,8:29]
No_MnA <- r[,30:46]

port1 <- portfolio.spec(colnames(Fintech_MnA))
port1 <- add.constraint(port1, type = "leverage", min_sum = 0.99, max_sum = 1.01)
port1 <- add.constraint(port1, type = "box", min = -1, max = 1)
port_random <- random_portfolios(port, permutations = 2000, rp_method = "sample")
return_mean <- apply(port_random, MARGIN = 1, function(x) {mean(r %*% x)})
return_sd <- apply(port_random, MARGIN = 1, function(x) {StdDev(r, weights = x)})
plot(return_sd, return_mean, main = "Допустимое множество портфелей из выбранных активов",
     xlab = "Standart Deviation", ylab = "Expected Return", col = "darkred")


#Создадим портфели с заданной дневной доходностью для 3-х категорий (коротких продаж нет)
#M&A с финтехами
portfolio2 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio2 <- add.constraint(portfolio2, type = "full_invsetment")
portfolio2 <- add.constraint(portfolio2, type = "box", min = 0, max = 1)
portfolio2 <- add.constraint(portfolio2, type = "return", name = "mean", return_target = 0.001)
portfolio2 <- add.objective(portfolio2, type = "risk", name = "StdDev")
portfolio2 <- add.objective(portfolio2, type = "return", name = "mean", multiplier = 0)
Result2 <- optimize.portfolio(portfolio = portfolio2, R = Fintech_MnA, optimize_method = "ROI")
round(Result2$weights,4)
Result2$opt_values

portfolio3 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio3 <- add.constraint(portfolio3, type = "full_invsetment")
portfolio3 <- add.constraint(portfolio3, type = "box", min = 0, max = 1)
portfolio3 <- add.constraint(portfolio3, type = "return", name = "mean", return_target = 0.0013)
portfolio3 <- add.objective(portfolio3, type = "risk", name = "StdDev")
portfolio3 <- add.objective(portfolio3, type = "return", name = "mean", multiplier = 0)
Result3 <- optimize.portfolio(portfolio = portfolio3, R = Fintech_MnA, optimize_method = "ROI")
round(Result3$weights,4)
Result3$opt_values

portfolio4 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio4 <- add.constraint(portfolio4, type = "full_invsetment")
portfolio4 <- add.constraint(portfolio4, type = "box", min = 0, max = 1)
portfolio4 <- add.constraint(portfolio4, type = "return", name = "mean", return_target = 0.0016)
portfolio4 <- add.objective(portfolio4, type = "risk", name = "StdDev")
portfolio4 <- add.objective(portfolio4, type = "return", name = "mean", multiplier = 0)
Result4 <- optimize.portfolio(portfolio = portfolio4, R = Fintech_MnA, optimize_method = "ROI")
round(Result4$weights,4)
Result4$opt_values

#M&A с компаниями других сфер
portfolio5 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio5 <- add.constraint(portfolio5, type = "full_invsetment")
portfolio5 <- add.constraint(portfolio5, type = "box", min = 0, max = 1)
portfolio5 <- add.constraint(portfolio5, type = "return", name = "mean", return_target = 0.001)
portfolio5 <- add.objective(portfolio5, type = "risk", name = "StdDev")
portfolio5 <- add.objective(portfolio5, type = "return", name = "mean", multiplier = 0)
Result5 <- optimize.portfolio(portfolio = portfolio5, R = Other_MnA, optimize_method = "ROI")
round(Result5$weights,4)
Result5$opt_values

portfolio6 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio6 <- add.constraint(portfolio6, type = "full_invsetment")
portfolio6 <- add.constraint(portfolio6, type = "box", min = 0, max = 1)
portfolio6 <- add.constraint(portfolio6, type = "return", name = "mean", return_target = 0.0013)
portfolio6 <- add.objective(portfolio6, type = "risk", name = "StdDev")
portfolio6 <- add.objective(portfolio6, type = "return", name = "mean", multiplier = 0)
Result6 <- optimize.portfolio(portfolio = portfolio6, R = Other_MnA, optimize_method = "ROI")
round(Result6$weights,4)
Result6$opt_values

portfolio7 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio7 <- add.constraint(portfolio7, type = "full_invsetment")
portfolio7 <- add.constraint(portfolio7, type = "box", min = 0, max = 1)
portfolio7 <- add.constraint(portfolio7, type = "return", name = "mean", return_target = 0.0016)
portfolio7 <- add.objective(portfolio7, type = "risk", name = "StdDev")
portfolio7 <- add.objective(portfolio7, type = "return", name = "mean", multiplier = 0)
Result7 <- optimize.portfolio(portfolio = portfolio7, R = Other_MnA, optimize_method = "ROI")
round(Result7$weights,4)
Result7$opt_values

#Нет M&A 
portfolio8 <- portfolio.spec(assets = colnames(No_MnA))
portfolio8 <- add.constraint(portfolio8, type = "full_invsetment")
portfolio8 <- add.constraint(portfolio8, type = "box", min = 0, max = 1)
portfolio8 <- add.constraint(portfolio8, type = "return", name = "mean", return_target = 0.001)
portfolio8 <- add.objective(portfolio8, type = "risk", name = "StdDev")
portfolio8 <- add.objective(portfolio8, type = "return", name = "mean", multiplier = 0)
Result8 <- optimize.portfolio(portfolio = portfolio8, R = No_MnA, optimize_method = "ROI")
round(Result8$weights,4)
Result8$opt_values

portfolio10 <- portfolio.spec(assets = colnames(No_MnA))
portfolio10 <- add.constraint(portfolio10, type = "full_invsetment")
portfolio10 <- add.constraint(portfolio10, type = "box", min = 0, max = 1)
portfolio10 <- add.constraint(portfolio10, type = "return", name = "mean", return_target = 0.0013)
portfolio10 <- add.objective(portfolio10, type = "risk", name = "StdDev")
portfolio10 <- add.objective(portfolio10, type = "return", name = "mean", multiplier = 0)
Result10 <- optimize.portfolio(portfolio = portfolio10, R = No_MnA, optimize_method = "ROI")
round(Result10$weights,4)
Result10$opt_values

portfolio11 <- portfolio.spec(assets = colnames(No_MnA))
portfolio11 <- add.constraint(portfolio11, type = "full_invsetment")
portfolio11 <- add.constraint(portfolio11, type = "box", min = 0, max = 1)
portfolio11 <- add.constraint(portfolio11, type = "return", name = "mean", return_target = 0.0016)
portfolio11 <- add.objective(portfolio11, type = "risk", name = "StdDev")
portfolio11 <- add.objective(portfolio11, type = "return", name = "mean", multiplier = 0)
Result11 <- optimize.portfolio(portfolio = portfolio11, R = No_MnA, optimize_method = "ROI")
round(Result11$weights,4)
Result11$opt_values

#Для наглядности объединим
std1 <- c(Result2$opt_values$StdDev,Result3$opt_values$StdDev,Result4$opt_values$StdDev)
std1 <- round(std1,4)
mean <- c(Result2$opt_values$mean,Result3$opt_values$mean,Result4$opt_values$mean)
mean <- round(mean,4)

std2 <- c(Result5$opt_values$StdDev,Result6$opt_values$StdDev,Result7$opt_values$StdDev)
std2 <- round(std2,4)

std3 <- c(Result8$opt_values$StdDev,Result10$opt_values$StdDev,Result11$opt_values$StdDev)
std3 <- round(std3,4)

#Разрешим шорты
#M&A с финтехами
portfolio12 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio12 <- add.constraint(portfolio12, type = "full_invsetment")
portfolio12 <- add.constraint(portfolio12, type = "box", min = -1, max = 1)
portfolio12 <- add.constraint(portfolio12, type = "return", name = "mean", return_target = 0.001)
portfolio12 <- add.objective(portfolio12, type = "risk", name = "StdDev")
portfolio12 <- add.objective(portfolio12, type = "return", name = "mean", multiplier = 0)
Result12 <- optimize.portfolio(portfolio = portfolio12, R = Fintech_MnA, optimize_method = "ROI")
round(Result12$weights,4)
Result12$opt_values

portfolio13 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio13 <- add.constraint(portfolio13, type = "full_invsetment")
portfolio13 <- add.constraint(portfolio13, type = "box", min = -1, max = 1)
portfolio13 <- add.constraint(portfolio13, type = "return", name = "mean", return_target = 0.00113)
portfolio13 <- add.objective(portfolio13, type = "risk", name = "StdDev")
portfolio13 <- add.objective(portfolio13, type = "return", name = "mean", multiplier = 0)
Result13 <- optimize.portfolio(portfolio = portfolio13, R = Fintech_MnA, optimize_method = "ROI")
round(Result13$weights,4)
Result13$opt_values

portfolio14 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio14 <- add.constraint(portfolio14, type = "full_invsetment")
portfolio14 <- add.constraint(portfolio14, type = "box", min = -1, max = 1)
portfolio14 <- add.constraint(portfolio14, type = "return", name = "mean", return_target = 0.0016)
portfolio14 <- add.objective(portfolio14, type = "risk", name = "StdDev")
portfolio14 <- add.objective(portfolio14, type = "return", name = "mean", multiplier = 0)
Result14 <- optimize.portfolio(portfolio = portfolio14, R = Fintech_MnA, optimize_method = "ROI")
round(Result14$weights,4)
Result14$opt_values

#M&A с компаниями других сфер
portfolio15 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio15 <- add.constraint(portfolio15, type = "full_invsetment")
portfolio15 <- add.constraint(portfolio15, type = "box", min = -1, max = 1)
portfolio15 <- add.constraint(portfolio15, type = "return", name = "mean", return_target = 0.001)
portfolio15 <- add.objective(portfolio15, type = "risk", name = "StdDev")
portfolio15 <- add.objective(portfolio15, type = "return", name = "mean", multiplier = 0)
Result15 <- optimize.portfolio(portfolio = portfolio15, R = Other_MnA, optimize_method = "ROI")
round(Result15$weights,4)
Result15$opt_values

portfolio16 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio16 <- add.constraint(portfolio16, type = "full_invsetment")
portfolio16 <- add.constraint(portfolio16, type = "box", min = -1, max = 1)
portfolio16 <- add.constraint(portfolio16, type = "return", name = "mean", return_target = 0.0013)
portfolio16 <- add.objective(portfolio16, type = "risk", name = "StdDev")
portfolio16 <- add.objective(portfolio16, type = "return", name = "mean", multiplier = 0)
Result16 <- optimize.portfolio(portfolio = portfolio16, R = Other_MnA, optimize_method = "ROI")
round(Result16$weights,4)
Result16$opt_values

portfolio17 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio17 <- add.constraint(portfolio17, type = "full_invsetment")
portfolio17 <- add.constraint(portfolio17, type = "box", min = -1, max = 1)
portfolio17 <- add.constraint(portfolio17, type = "return", name = "mean", return_target = 0.0016)
portfolio17 <- add.objective(portfolio17, type = "risk", name = "StdDev")
portfolio17 <- add.objective(portfolio17, type = "return", name = "mean", multiplier = 0)
Result17 <- optimize.portfolio(portfolio = portfolio17, R = Other_MnA, optimize_method = "ROI")
round(Result17$weights,4)
Result17$opt_values

#Нет M&A 
portfolio18 <- portfolio.spec(assets = colnames(No_MnA))
portfolio18 <- add.constraint(portfolio18, type = "full_invsetment")
portfolio18 <- add.constraint(portfolio18, type = "box", min = -1, max = 1)
portfolio18 <- add.constraint(portfolio18, type = "return", name = "mean", return_target = 0.001)
portfolio18 <- add.objective(portfolio18, type = "risk", name = "StdDev")
portfolio18 <- add.objective(portfolio18, type = "return", name = "mean", multiplier = 0)
Result18 <- optimize.portfolio(portfolio = portfolio18, R = No_MnA, optimize_method = "ROI")
round(Result18$weights,2)
Result18$opt_values

portfolio20 <- portfolio.spec(assets = colnames(No_MnA))
portfolio20 <- add.constraint(portfolio20, type = "full_invsetment")
portfolio20 <- add.constraint(portfolio20, type = "box", min = -1, max = 1)
portfolio20 <- add.constraint(portfolio20, type = "return", name = "mean", return_target = 0.0013)
portfolio20 <- add.objective(portfolio20, type = "risk", name = "StdDev")
portfolio20 <- add.objective(portfolio20, type = "return", name = "mean", multiplier = 0)
Result20 <- optimize.portfolio(portfolio = portfolio20, R = No_MnA, optimize_method = "ROI")
round(Result20$weights,2)
Result20$opt_values

portfolio21 <- portfolio.spec(assets = colnames(No_MnA))
portfolio21 <- add.constraint(portfolio21, type = "full_invsetment")
portfolio21 <- add.constraint(portfolio21, type = "box", min = -1, max = 1)
portfolio21 <- add.constraint(portfolio21, type = "return", name = "mean", return_target = 0.0016)
portfolio21 <- add.objective(portfolio21, type = "risk", name = "StdDev")
portfolio21 <- add.objective(portfolio21, type = "return", name = "mean", multiplier = 0)
Result21 <- optimize.portfolio(portfolio = portfolio21, R = No_MnA, optimize_method = "ROI")
round(Result21$weights,2)
Result21$opt_values

#Для наглядности объединим
std4 <- c(Result12$opt_values$StdDev,Result13$opt_values$StdDev,Result14$opt_values$StdDev)
std4 <- round(std4,4)
mean <- c(Result2$opt_values$mean,Result3$opt_values$mean,Result4$opt_values$mean)
mean <- round(mean,4)

std5 <- c(Result15$opt_values$StdDev,Result16$opt_values$StdDev,Result17$opt_values$StdDev)
std5 <- round(std5,4)

std6 <- c(Result18$opt_values$StdDev,Result20$opt_values$StdDev,Result21$opt_values$StdDev)
std6 <- round(std6,4)

#Портфели с минимальной дисперсией (без коротких продаж)
portfolio22 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio22 <- add.constraint(portfolio22, type = "full_invsetment")
portfolio22 <- add.constraint(portfolio22, type = "box", min = 0, max = 1)
portfolio22 <- add.objective(portfolio22, type = "risk", name = "StdDev")
Result22 <- optimize.portfolio(portfolio = portfolio22, R = Fintech_MnA, optimize_method = "ROI")
round(Result22$weights,2)
Result22$objective_measures

portfolio23 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio23 <- add.constraint(portfolio23, type = "full_invsetment")
portfolio23 <- add.constraint(portfolio23, type = "box", min = 0, max = 1)
portfolio23 <- add.objective(portfolio23, type = "risk", name = "StdDev")
Result23 <- optimize.portfolio(portfolio = portfolio23, R = Other_MnA, optimize_method = "ROI")
round(Result23$weights,2)
Result23$objective_measures

portfolio24 <- portfolio.spec(assets = colnames(No_MnA))
portfolio24 <- add.constraint(portfolio24, type = "full_invsetment")
portfolio24 <- add.constraint(portfolio24, type = "box", min = 0, max = 1)
portfolio24 <- add.objective(portfolio24, type = "risk", name = "StdDev")
Result24 <- optimize.portfolio(portfolio = portfolio24, R = No_MnA, optimize_method = "ROI")
round(Result24$weights,2)
Result24$objective_measures

#Соберу в таблицу
mean2 <- c(Result22$opt_values$mean,Result23$opt_values$mean,Result24$opt_values$mean)
mean2 <- round(mean2,4)

std4 <- c(Result22$opt_values$StdDev,Result23$opt_values$StdDev,Result24$opt_values$StdDev)
std4 <- round(std4,4)

#Портфели с минимальной дисперсией (Короткие продажи разрешены)
portfolio25 <- portfolio.spec(assets = colnames(Fintech_MnA))
portfolio25 <- add.constraint(portfolio25, type = "full_invsetment")
portfolio25 <- add.constraint(portfolio25, type = "box", min = -1, max = 1)
portfolio25 <- add.objective(portfolio25, type = "risk", name = "StdDev")
Result25 <- optimize.portfolio(portfolio = portfolio25, R = Fintech_MnA, optimize_method = "ROI")
round(Result25$weights,2)
Result25$objective_measures

portfolio26 <- portfolio.spec(assets = colnames(Other_MnA))
portfolio26 <- add.constraint(portfolio26, type = "full_invsetment")
portfolio26 <- add.constraint(portfolio26, type = "box", min = -1, max = 1)
portfolio26 <- add.objective(portfolio26, type = "risk", name = "StdDev")
Result26 <- optimize.portfolio(portfolio = portfolio26, R = Other_MnA, optimize_method = "ROI")
round(Result26$weights,2)
Result26$objective_measures

portfolio27 <- portfolio.spec(assets = colnames(No_MnA))
portfolio27 <- add.constraint(portfolio27, type = "full_invsetment")
portfolio27 <- add.constraint(portfolio27, type = "box", min = -1, max = 1)
portfolio27 <- add.objective(portfolio27, type = "risk", name = "StdDev")
Result27 <- optimize.portfolio(portfolio = portfolio27, R = No_MnA, optimize_method = "ROI")
round(Result27$weights,2)
Result27$objective_measures

#Соберу в таблицу
mean3 <- c(Result25$opt_values$mean,Result26$opt_values$mean,Result27$opt_values$mean)
mean3 <- round(mean3,4)

std5 <- c(Result25$opt_values$StdDev,Result26$opt_values$StdDev,Result27$opt_values$StdDev)
std5 <- round(std5,4)

#Портфели с самым разным восприятием риска
#Зададим функцию полезности
CRRA <- function(R, weights, lambda, sigma, m3, m4){
  weights <- matrix(weights, ncol=1)
  M2.w <- t(weights) %*% sigma %*% weights
  M3.w <- t(weights) %*% m3 %*% (weights %x% weights)
  M4.w <- t(weights) %*% m4 %*% (weights %x% weights %x% weights)
  term1<-(1/2)*lambda*M2.w
  term2<-(1/6)*lambda*(lambda+1)*M3.w
  term3<-(1/24)*lambda*(lambda+1)*(lambda+2)*M4.w
  out <- -term1 + term2 - term3
  out
}
crra.moments <- function(R){
  out <- list()
  out$mu <- colMeans(R)
  out$sigma <- cov(R)
  out$m3 <- PerformanceAnalytics:::M3.MM(R)
  out$m4 <- PerformanceAnalytics:::M4.MM(R)
  out
}

#M&A с Финтехами
crra.portfolio1 <- portfolio.spec(assets=colnames(Fintech_MnA))
crra.portfolio1 <- add.constraint(portfolio=crra.portfolio1, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio1 <- add.constraint(portfolio=crra.portfolio1, type="box", min=0, max=1)
crra.portfolio1 <- add.objective(portfolio=crra.portfolio1, type="return",  name="CRRA", arguments=list(lambda=0.1))
crra.portfolio1 <- add.objective(crra.portfolio1, type="return", name="mean", multiplier=0)
crra.portfolio1 <- add.objective(crra.portfolio1, type="risk", name="StdDev", multiplier=0)
crra.portfolio1 <- add.objective(crra.portfolio1, type="risk_budget", name="ES", multiplier=0)
Result_crra1 <- optimize.portfolio(Fintech_MnA, crra.portfolio1, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra1$objective_measures

crra.portfolio2 <- portfolio.spec(assets=colnames(Fintech_MnA))
crra.portfolio2 <- add.constraint(portfolio=crra.portfolio2, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio2 <- add.constraint(portfolio=crra.portfolio2, type="box", min=0, max=1)
crra.portfolio2 <- add.objective(portfolio=crra.portfolio2, type="return",  name="CRRA", arguments=list(lambda=1))
crra.portfolio2 <- add.objective(crra.portfolio2, type="return", name="mean", multiplier=0)
crra.portfolio2 <- add.objective(crra.portfolio2, type="risk", name="StdDev", multiplier=0)
crra.portfolio2 <- add.objective(crra.portfolio2, type="risk_budget", name="ES", multiplier=0)
Result_crra2 <- optimize.portfolio(Fintech_MnA, crra.portfolio2, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra2$objective_measures

crra.portfolio3 <- portfolio.spec(assets=colnames(Fintech_MnA))
crra.portfolio3 <- add.constraint(portfolio=crra.portfolio3, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio3 <- add.constraint(portfolio=crra.portfolio3, type="box", min=0, max=1)
crra.portfolio3 <- add.objective(portfolio=crra.portfolio3, type="return",  name="CRRA", arguments=list(lambda=10))
crra.portfolio3 <- add.objective(crra.portfolio3, type="return", name="mean", multiplier=0)
crra.portfolio3 <- add.objective(crra.portfolio3, type="risk", name="StdDev", multiplier=0)
crra.portfolio3 <- add.objective(crra.portfolio3, type="risk_budget", name="ES", multiplier=0)
Result_crra3 <- optimize.portfolio(Fintech_MnA, crra.portfolio3, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra3$objective_measures

mean11 <- c(Result_crra1$objective_measures$mean,Result_crra2$objective_measures$mean,Result_crra3$objective_measures$mean)
std11 <- c(Result_crra1$objective_measures$StdDev,Result_crra2$objective_measures$StdDev,Result_crra3$objective_measures$StdDev)
es11 <- c(Result_crra1$objective_measures$ES$MES,Result_crra2$objective_measures$ES$MES,Result_crra3$objective_measures$ES$MES)


#M&A с другими компаниями (нефинтехами)
crra.portfolio4 <- portfolio.spec(assets=colnames(Other_MnA))
crra.portfolio4 <- add.constraint(portfolio=crra.portfolio4, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio4 <- add.constraint(portfolio=crra.portfolio4, type="box", min=0, max=1)
crra.portfolio4 <- add.objective(portfolio=crra.portfolio4, type="return",  name="CRRA", arguments=list(lambda=0.1))
crra.portfolio4 <- add.objective(crra.portfolio4, type="return", name="mean", multiplier=0)
crra.portfolio4 <- add.objective(crra.portfolio4, type="risk", name="StdDev", multiplier=0)
crra.portfolio4 <- add.objective(crra.portfolio4, type="risk_budget", name="ES", multiplier=0)
Result_crra4 <- optimize.portfolio(Other_MnA, crra.portfolio4, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra4$objective_measures

crra.portfolio5 <- portfolio.spec(assets=colnames(Other_MnA))
crra.portfolio5 <- add.constraint(portfolio=crra.portfolio5, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio5 <- add.constraint(portfolio=crra.portfolio5, type="box", min=0, max=1)
crra.portfolio5 <- add.objective(portfolio=crra.portfolio5, type="return",  name="CRRA", arguments=list(lambda=1))
crra.portfolio5 <- add.objective(crra.portfolio5, type="return", name="mean", multiplier=0)
crra.portfolio5 <- add.objective(crra.portfolio5, type="risk", name="StdDev", multiplier=0)
crra.portfolio5 <- add.objective(crra.portfolio5, type="risk_budget", name="ES", multiplier=0)
Result_crra5 <- optimize.portfolio(Other_MnA, crra.portfolio5, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra5$objective_measures

crra.portfolio6 <- portfolio.spec(assets=colnames(Other_MnA))
crra.portfolio6 <- add.constraint(portfolio=crra.portfolio6, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio6 <- add.constraint(portfolio=crra.portfolio6, type="box", min=0, max=1)
crra.portfolio6 <- add.objective(portfolio=crra.portfolio6, type="return",  name="CRRA", arguments=list(lambda=10))
crra.portfolio6 <- add.objective(crra.portfolio6, type="return", name="mean", multiplier=0)
crra.portfolio6 <- add.objective(crra.portfolio6, type="risk", name="StdDev", multiplier=0)
crra.portfolio6 <- add.objective(crra.portfolio6, type="risk_budget", name="ES", multiplier=0)
Result_crra6 <- optimize.portfolio(Other_MnA, crra.portfolio6, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra6$objective_measures

mean22 <- c(Result_crra4$objective_measures$mean,Result_crra5$objective_measures$mean,Result_crra6$objective_measures$mean)
std22 <- c(Result_crra4$objective_measures$StdDev,Result_crra5$objective_measures$StdDev,Result_crra6$objective_measures$StdDev)
es22 <- c(Result_crra4$objective_measures$ES$MES,Result_crra5$objective_measures$ES$MES,Result_crra6$objective_measures$ES$MES)


#Нет сделок M&A
crra.portfolio7 <- portfolio.spec(assets=colnames(No_MnA))
crra.portfolio7 <- add.constraint(portfolio=crra.portfolio7, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio7 <- add.constraint(portfolio=crra.portfolio7, type="box", min=0, max=1)
crra.portfolio7 <- add.objective(portfolio=crra.portfolio7, type="return",  name="CRRA", arguments=list(lambda=0.1))
crra.portfolio7 <- add.objective(crra.portfolio7, type="return", name="mean", multiplier=0)
crra.portfolio7 <- add.objective(crra.portfolio7, type="risk", name="StdDev", multiplier=0)
crra.portfolio7 <- add.objective(crra.portfolio7, type="risk_budget", name="ES", multiplier=0)
Result_crra7 <- optimize.portfolio(No_MnA, crra.portfolio7, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra7$objective_measures

crra.portfolio8 <- portfolio.spec(assets=colnames(No_MnA))
crra.portfolio8 <- add.constraint(portfolio=crra.portfolio8, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio8 <- add.constraint(portfolio=crra.portfolio8, type="box", min=0, max=1)
crra.portfolio8 <- add.objective(portfolio=crra.portfolio8, type="return",  name="CRRA", arguments=list(lambda=1))
crra.portfolio8 <- add.objective(crra.portfolio8, type="return", name="mean", multiplier=0)
crra.portfolio8 <- add.objective(crra.portfolio8, type="risk", name="StdDev", multiplier=0)
crra.portfolio8 <- add.objective(crra.portfolio8, type="risk_budget", name="ES", multiplier=0)
Result_crra8 <- optimize.portfolio(No_MnA, crra.portfolio8, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra8$objective_measures

crra.portfolio9 <- portfolio.spec(assets=colnames(No_MnA))
crra.portfolio9 <- add.constraint(portfolio=crra.portfolio9, type="weight_sum", min_sum=0.99, max_sum=1.01)
crra.portfolio9 <- add.constraint(portfolio=crra.portfolio9, type="box", min=0, max=1)
crra.portfolio9 <- add.objective(portfolio=crra.portfolio9, type="return",  name="CRRA", arguments=list(lambda=10))
crra.portfolio9 <- add.objective(crra.portfolio9, type="return", name="mean", multiplier=0)
crra.portfolio9 <- add.objective(crra.portfolio9, type="risk", name="StdDev", multiplier=0)
crra.portfolio9 <- add.objective(crra.portfolio9, type="risk_budget", name="ES", multiplier=0)
Result_crra9 <- optimize.portfolio(No_MnA, crra.portfolio9, optimize_method="DEoptim",  search_size=20000, trace=TRUE,
                                   traceDE=0, momentFUN="crra.moments")
Result_crra9$objective_measures

mean33 <- c(Result_crra7$objective_measures$mean,Result_crra8$objective_measures$mean,Result_crra9$objective_measures$mean)
std33 <- c(Result_crra7$objective_measures$StdDev,Result_crra8$objective_measures$StdDev,Result_crra9$objective_measures$StdDev)
es33 <- c(Result_crra7$objective_measures$ES$MES,Result_crra8$objective_measures$ES$MES,Result_crra9$objective_measures$ES$MES)




