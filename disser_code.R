## made by Polina Shchukina 
## Lomonosov Moscow State University
## Telegram channel: https://t.me/rstudioprogr

library(readxl)
library(car)
library(stargazer)
library(dplyr)
library(ggplot2)
library(lmtest)
library(plm)
library(GGally)
library(scales)
library(webr)
library(MASS)
library(xgboost)
library(rpart)
library(rpart.plot)
library(gtsummary)
library(qgraph)
library(flexclust)
library(scales)
library(gifski)
library(gghighlight)
library(tidyverse)
library(plotROC) #для PlotROC
library(gganimate)
library(viridis)
library(sandwich)
library(ggstatsplot)
library(ggExtra)
library(ggridges)
library(circlize)
library(caret)
library(gridExtra)
library(class)
library(tidyr)
library(FactoMineR) #PCA
library(factoextra)
library(psych)
library(cluster)
library(corrplot)
library(quantregForest)
library(randomForest)
library(randomForestExplainer)
library(openxlsx)
library(ranger)
library(kernelshap)
library(DALEX)
library(shapviz)
library(treeshap)
library(quantreg)
library(devtools)
library(ggradar)
library(gganimate)
#devtools::install_github("laresbernardo/lares")
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE) 
library(lares)

data_CG <- read_excel("~/data CG.xlsx", sheet = "v0402") %>% as.data.frame()

##### --------------ОПРЕДЕЛЕНИЕ КЛЮЧЕВЫХ ДАТАФРЕЙМОВ------------------ #####

#1 - Датасет с дивидендом на одну акцию: data_DPS (326 наблюдений)
#2 - Датасет с дивидендной доходностью: data_DY (316 наблюдений)
#3 - Датасет, где есть и DPS и DY: data_DPS_DY
names(data_CG)
data_DPS <- data_CG %>% transmute(Ticker, Number_Year, DPS, ROA, ROE, Stage_Indicator, CapexToSales = CAPEX/Sales, log_Assets = log(Total_Assets), DebtToAssets, Percentile, Board_size, Gender_diversity, 
                                  Board_meetings, Average_age, Average_WE, share_ID = Independent_Directors/Board_size, Share_Foreign_Directors, Gov_Part, Turnover_ratio, Turnover_lag_ratio, Industry) %>% na.omit()
data_DPS1 <- data_DPS %>% mutate(log_DPS = log(DPS+1))
data_DPS1$Crisis_period <- ifelse(data_DPS1$Number_Year %in% c(2014, 2020), 1, 0)
data_DPS2 <- data_DPS1 %>% dplyr::select(-Ticker, -Number_Year, -DPS, -Industry, -Crisis_period) #для кластеризации нужны только числовые переменные


data_DY <- data_CG %>% transmute(Ticker, Number_Year, DY, ROA, ROE, Stage_Indicator, CapexToSales = CAPEX/Sales, log_Assets = log(Total_Assets), DebtToAssets, Percentile, Board_size, Gender_diversity, 
                                 Board_meetings, Average_age, Average_WE, share_ID = Independent_Directors/Board_size, Share_Foreign_Directors, Gov_Part, Turnover_ratio, Turnover_lag_ratio, Industry) %>% na.omit()

data_DPS_DY <- merge(data_DPS, data_DY)
dim(data_DPS_DY)
write.xlsx(data_DPS_DY, file = "data_DPS_DY.xlsx")

max_IMOEX <- read_excel("~/data CG.xlsx", sheet = "svod1") %>% as.data.frame()

##### --------------ОЧИСТКА ОТ ВЫБРОСОВ------------------ #####
outlier_data <- read_excel("~/data CG.xlsx", sheet = "v0402_na_outlier") %>% as.data.frame()
outlier_data1 <- outlier_data %>% dplyr::filter(Outlier == 0)
outlier_data1$log_DPS <- log(outlier_data1$DPS + 1)
outlier_data1$log_Board_meeting <- log(outlier_data1$Board_meetings)
chord_data <- as.data.frame(table(outlier_data1$Industry, outlier_data1$Number_Year)) #для построения хордовой диаграммы (пакет: circlize)
dim(outlier_data1)

#описательные статистики
stargazer(outlier_data1, out = "DS.html", type = "html")

#индекс Герфиндаля-Хиршмана
outlier_data_HHI <- read_excel("~/data CG.xlsx", sheet = "v0402_HHI0") %>% as.data.frame()
outlier_data_HHI$log_DPS <- log(outlier_data_HHI$DPS + 1)
outlier_data_HHI1 <- outlier_data_HHI %>% dplyr::filter(Outlier == 0) %>% na.omit()
outlier_data_HHI1$log_Board_meeting <- log(outlier_data_HHI1$Board_meetings)
cor(outlier_data_HHI1$log_DPS, outlier_data_HHI1$HHI_level0)
cor(outlier_data_HHI1$log_DPS, outlier_data_HHI1$HHI_level0_adj)
dim(outlier_data_HHI1)

#Два директора - дата
outlier_data_DD <- merge(outlier_data1, data_CG %>% transmute(Ticker, Number_Year, DPS, ROA, ROE, Stage_Indicator, CapexToSales = CAPEX/Sales, log_Assets = log(Total_Assets), DebtToAssets, Percentile, Board_size, Gender_diversity, 
                                                              Board_meetings, Average_age, Average_WE, share_ID = Independent_Directors/Board_size, Share_Foreign_Directors, Gov_Part, Turnover_ratio, Turnover_lag_ratio, Industry, Double_Directors = Double/Board_size) %>% na.omit())
dim(outlier_data_DD)

dim(merge(outlier_data_DD, outlier_data_HHI1))
#описательные статистики
stargazer(outlier_data_HHI1, out = "DS1.html", type = "html")
stargazer(outlier_data_DD, out = "DS2.html", type = "html")

##### --------------ОБРАБОТКА ДАННЫХ ИЗ ОТЧЕТА ККУ------------------ #####
AR_CG_CBR_data <- read_excel("ККУ_дата.xlsx", sheet = "в R")

data_level0 <- AR_CG_CBR_data %>% group_by(Company, Ticker, Number_Year) %>% summarise(Complied = sum(Complied), 
                                                                                Partial_complied = sum(Partial_complied), 
                                                                                Not_complied = sum(Not_complied), 
                                                                                All = Complied + Partial_complied + Not_complied, 
                                                                                HHI_level0 = (Complied/All)^2 + (Partial_complied/All)^2 + (Not_complied/All)^2) %>% as.data.frame()
#запишем в эксель
write.xlsx(file = "level0.xlsx", data_level0)

data_level1 <- AR_CG_CBR_data %>% group_by(Company, Ticker, Number_Year, Level_1) %>% summarise(Complied = sum(Complied), 
                                                                                Partial_complied = sum(Partial_complied), 
                                                                                Not_complied = sum(Not_complied), 
                                                                                All = Complied + Partial_complied + Not_complied, 
                                                                                HHI_level1 = (Complied/All)^2 + (Partial_complied/All)^2 + (Not_complied/All)^2) %>% as.data.frame()  %>% group_by(Company, Number_Year, Ticker) %>% summarise(HHI_level1 = mean(HHI_level1))
write.xlsx(file = "level1.xlsx", data_level1)
data_level2 <- AR_CG_CBR_data %>% group_by(Company, Ticker, Number_Year, Level_2) %>% summarise(Complied = sum(Complied), 
                                                                                         Partial_complied = sum(Partial_complied), 
                                                                                         Not_complied = sum(Not_complied), 
                                                                                         All = Complied + Partial_complied + Not_complied, 
                                                                                         HHI_level2 = (Complied/All)^2 + (Partial_complied/All)^2 + (Not_complied/All)^2) %>% as.data.frame() %>% dplyr::filter(Level_2 == "Дивиденды") %>% dplyr::select(-Complied, -Partial_complied, -Not_complied, -All)
write.xlsx(file = "level2.xlsx", data_level2)
data_KKY <- merge(merge(merge(data_DPS_DY, data_level0), data_level1), data_level2)
names(data_KKY)
corrplot(cor(data_KKY[,c(22,23,28,29,31)]), method = "number")



##### --------------МОДЕЛИ------------------ #####

summary(outlier_data1)
names(outlier_data1)

model1 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio)
summary(model1)
vif(model1)
head(outlier_data1)
names(outlier_data1)
bptest(model1)
summary(outlier_data1)

model2 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio)
summary(model2)
bptest(model2)
vif(model2)

model3 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Industry)
summary(model3)
waldtest(model1, model3)
vif(model3)

model4 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Industry)
summary(model4)
vif(model4)

model5 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(Number_Year))
summary(model5)
waldtest(model1, model5)
vif(model5)

model6 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(Number_Year))
summary(model6)

model57 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(Number_Year) + Industry)
model58 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(Number_Year) + Industry)

summary(model57)
waldtest(model1, model57)
waldtest(model3, model57)
waldtest(model5, model57)
vif(model57)

waldtest(model2, model4)
waldtest(model2, model6)
waldtest(model2, model58)
waldtest(model4, model58)
waldtest(model6, model58)

#сохранение моделей 
#DPS
stargazer(model1, model3, model5, model57, model7, type = "html", df = FALSE, se = list(cse(model1), cse(model3), cse(model5), cse(model57), cse(model7)), out = "Summary_models3.html")
waldtest(model5, model57)
waldtest(model1, model3) #model3
waldtest(model3, model5)
#DY
stargazer(model2, model4, model6, model58, model8, type = "html", df = FALSE, se = list(cse(model2), cse(model4), cse(model6), cse(model58), cse(model8)), out = "Summary_models4.html")
waldtest(model6, model58)
bptest(model2)
waldtest(model4, model58)

#выделение особых периодов: 2014, 2020 и 2022
outlier_data1$Year_2014_2020_2022 <- ifelse(outlier_data1$Number_Year %in% c("2014", "2020", "2022"), 1, 0)

model7 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Year_2014_2020_2022)
summary(model7)

model8 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Year_2014_2020_2022)
summary(model8)


bptest(model1)
bptest(model2)

cse <- function(model) {
  A <- sqrt(diag(vcovHC(model,type = "HC0")))
  return(A)
}

stargazer(model1, model2, model3, model4, model5, model6, type = "html", df = FALSE, se = list(cse(model1), cse(model2), cse(model3), cse(model4), cse(model5), cse(model6)), out = "Summary_models.html")
waldtest(model1, model3)
waldtest(model1, model5)

waldtest(model2, model4)
waldtest(model2, model6)

#с индексом HHI 
names(outlier_data_HHI1)
model9 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj)
model10 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj)
model11 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj + Industry)
model12 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj + Industry)
model13 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj + as.character(Number_Year))
model14 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj + as.character(Number_Year))
model59 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj + as.character(Number_Year) + Industry)
model60 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj + as.character(Number_Year) + Industry)

bptest(model9)
bptest(model10)

waldtest(model9, model11)
waldtest(model9, model13)
waldtest(model9, model59)
waldtest(model11, model59)
waldtest(model13, model59)

waldtest(model10, model12)
waldtest(model10, model14)
waldtest(model10, model60)
waldtest(model12, model60)
waldtest(model14, model60)

#FE, RE, pooled

model61 <- plm(data = outlier_data_HHI1,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj, model = "pooling",index = c("Number_Year", "Ticker"))
model62 <- plm(data = outlier_data_HHI1,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model63 <- plm(data = outlier_data_HHI1,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets  + Gov_Part + HHI_level0_adj, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
stargazer(model61, model62, model63, type = "html", df = FALSE, se = list(cse(model61), cse(model62), cse(model63)), out = "Summary_models10.html")

summary(model61)

#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(model63, model61) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(model62, model63) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(model61, type = "bp") #лучше случайные
#случайные > пула 

#фиксированные > случайные > пул
#model62


summary(model9)
stargazer(model9, model10, model11, model12, model13, model14, type = "html", df = FALSE, se = list(cse(model9), cse(model10), cse(model11), cse(model12), cse(model13), cse(model14)), out = "Summary_models2.html")
stargazer(model9, model11, model13, model59, type = "html", df = FALSE, se = list(cse(model9), cse(model11), cse(model13), cse(model59)), out = "Summary_models4.html")
stargazer(model10, model12, model14, model60, type = "html", df = FALSE, se = list(cse(model10), cse(model12), cse(model14), cse(model60)), out = "Summary_models5.html")


#тест короткой против длинной 
waldtest(model9, model11)
waldtest(model9, model13)

waldtest(model10, model12)
waldtest(model10, model14)
waldtest(model10, model60)
waldtest(model12, model60)
waldtest(model14, model60)

#изменится ли знак перед D/A если удалить наблюдения с D/A > 1
data_test <- outlier_data1 %>% dplyr::filter(DebtToAssets < 1)
model15 <- lm(data = data_test, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio)
summary(model1)
vif(model1)
head(outlier_data1)

model16 <- lm(data = data_test, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio)
summary(model16)
vif(model2)

#нет не изменился 

###### Удаление 2021, 2022 гг. - проверка ######

model92 <- lm(data = outlier_data1 %>% dplyr::filter(!(Number_Year %in% c("2021", "2022"))), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model92)
model93 <- lm(data = outlier_data1 %>% dplyr::filter(!(Number_Year %in% c("2022"))), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model93)
model94 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model94)

stargazer(model94, model93, model92, type = "html", df = FALSE, se = list(cse(model94), cse(model93), cse(model92)), out = "Summary_2021_2022.html")

model95 <- lm(data = outlier_data1 %>% dplyr::filter(!(Number_Year %in% c("2021", "2022"))), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model96 <- lm(data = outlier_data1 %>% dplyr::filter(!(Number_Year %in% c("2022"))), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model97 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)

stargazer(model97, model96, model95, type = "html", df = FALSE, se = list(cse(model97), cse(model96), cse(model95)), out = "SummaryDY_2021_2022.html")
names(outlier_data_HHI1)
model98 <- lm(data = outlier_data_HHI1 %>% dplyr::filter(!(Number_Year %in% c("2021", "2022"))), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + HHI_level0_adj)
model99 <- lm(data = outlier_data_HHI1 %>% dplyr::filter(!(Number_Year %in% c("2022"))), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + HHI_level0_adj)
model100 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + HHI_level0_adj)

stargazer(model100, model99, model98, type = "html", df = FALSE, se = list(cse(model100), cse(model99), cse(model98)), out = "SummaryHHI_2021_2022.html")

model101 <- lm(data = outlier_data_DD %>% dplyr::filter(!(Number_Year %in% c("2021", "2022"))), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)
model102 <- lm(data = outlier_data_DD %>% dplyr::filter(!(Number_Year %in% c("2022"))), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)
model103 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)

stargazer(model103, model102, model101, type = "html", df = FALSE, se = list(cse(model103), cse(model102), cse(model101)), out = "SummaryDD_2021_2022.html")

model104 <- lm(data = data_CEO %>% dplyr::filter(!(Number_Year %in% c("2021", "2022"))), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
model105 <- lm(data = data_CEO %>% dplyr::filter(!(Number_Year %in% c("2022"))), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
model106 <- lm(data = data_CEO, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
vif(model106)
stargazer(model106, model105, model104, type = "html", df = FALSE, se = list(cse(model106), cse(model105), cse(model104)), out = "SummaryCEO_2021_2022.html")

model107 <- lm(data = data_CEO %>% dplyr::filter(!(Number_Year %in% c("2021", "2022"))), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
model108 <- lm(data = data_CEO %>% dplyr::filter(!(Number_Year %in% c("2022"))), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
model109 <- lm(data = data_CEO, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)

stargazer(model109, model108, model107, type = "html", df = FALSE, se = list(cse(model109), cse(model108), cse(model107)), out = "SummaryCEO_2021_2022_DY.html")

###### Удаление каких-то значений Board_size ######
quantile(outlier_data1$Board_size, 0.95)
quantile(outlier_data1$Board_size, 0.05)

model110 <- lm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model110)

model111 <- lm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry)
summary(model111)

model112 <- lm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry + as.character(Number_Year))
summary(model112)

model113 <- lm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model113)

model114 <- lm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry)
summary(model114)

model115 <- lm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry + as.character(Number_Year))
summary(model115)

stargazer(model110, model111, model112, model113, model114, model115, type = "html", df = FALSE, 
          se = list(cse(model110), cse(model111), cse(model112), cse(model113), cse(model114), cse(model115)), out = "SummaryBoard.html")

model116 <- plm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "pooling",index = c("Number_Year", "Ticker"))
summary(model116)

model117 <- plm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "within",index = c("Number_Year", "Ticker"))
summary(model117)

model118 <- plm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "pooling",index = c("Number_Year", "Ticker"))
summary(model118)

model119 <- plm(data = outlier_data1 %>% dplyr::filter(Board_size < 16, Board_size > 6),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "within",index = c("Number_Year", "Ticker"))
summary(model119)

stargazer(model116, model117, model118, model119, type = "html", df = FALSE, 
          se = list(cse(model116), cse(model117), cse(model118), cse(model119)), out = "SummaryBoard_plm.html")

###### Удаление каких-то значений log_Board_meetings ######
quantile(outlier_data1$log_Board_meeting, 0.95)
quantile(outlier_data1$log_Board_meeting, 0.05)

model120 <- lm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model120)

model121 <- lm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry)
summary(model121)

model122 <- lm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry + as.character(Number_Year))
summary(model122)

model123 <- lm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
summary(model123)

model124 <- lm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry)
summary(model124)

model125 <- lm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)), DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Industry + as.character(Number_Year))
summary(model125)

stargazer(model120, model121, model122, model123, model124, model125, type = "html", df = FALSE, 
          se = list(cse(model120), cse(model121), cse(model122), cse(model123), cse(model124), cse(model125)), out = "SummaryMeetings.html")

model126 <- plm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "pooling",index = c("Number_Year", "Ticker"))
model127 <- plm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "within",index = c("Number_Year", "Ticker"))
model128 <- plm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "pooling",index = c("Number_Year", "Ticker"))
model129 <- plm(data = outlier_data1 %>% dplyr::filter(log_Board_meeting <= quantile(outlier_data1$log_Board_meeting, 0.95), log_Board_meeting >= quantile(outlier_data1$log_Board_meeting, 0.05)),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, model = "within",index = c("Number_Year", "Ticker"))

stargazer(model126, model127, model128, model129, type = "html", df = FALSE, 
          se = list(cse(model126), cse(model127), cse(model128), cse(model129)), out = "SummaryMeetings_plm.html")

###### Тест драйв на устойчивость ######
#Для DPS
model17 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model18 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model19 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model20 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model21 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model22 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part+ Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)

stargazer(model17, model18, model19, model20, model21, model22, type = "html", df = FALSE, se = list(cse(model17), cse(model18), cse(model19), cse(model20), cse(model21), cse(model22)), out = "Summary_models3.html")

#для DY
model23 <- lm(data = outlier_data_HHI1, DY ~ ROA + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model24 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model25 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model26 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model27 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
model28 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part+ Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)

stargazer(model23, model24, model25, model26, model27, model28, type = "html", df = FALSE, se = list(cse(model23), cse(model24), cse(model25), cse(model26), cse(model27), cse(model28)), out = "Summary_models4.html")

#тестирование модели с переменной интереса HHI
#Для DPS
model29 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + HHI_level0_adj)
model30 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + HHI_level0_adj)
model31 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + HHI_level0_adj)
model32 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + HHI_level0_adj)
model33 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + HHI_level0_adj)
model34 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + HHI_level0_adj)

stargazer(model29, model30, model31, model32, model33, model34, type = "html", df = FALSE, se = list(cse(model29), cse(model30), cse(model31), cse(model32), cse(model33), cse(model34)), out = "Summary_models4.html")

#для DY
model35 <- lm(data = outlier_data_HHI1, DY ~ ROA + HHI_level0_adj)
model36 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + HHI_level0_adj)
model37 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + HHI_level0_adj)
model38 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + HHI_level0_adj)
model39 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + HHI_level0_adj)
model40 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + HHI_level0_adj)

stargazer(model35, model36, model37, model38, model39, model40, type = "html", df = FALSE, se = list(cse(model35), cse(model36), cse(model37), cse(model38), cse(model39), cse(model40)), out = "Summary_models5.html")


#lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio)

model41 <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Industry + as.character(Number_Year))
model42 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Industry + as.character(Number_Year))

stargazer(model41, model42, type = "html", df = FALSE, se = list(cse(model41), cse(model42)), out = "Summary_models6.html")

###### Модели с Double Directors ######
dim(outlier_data_DD)
names(outlier_data_DD)
model43 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors)
model44 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors + Industry)
model45 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors + as.character(Number_Year))
model46 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors + as.character(Number_Year) + Industry)

model47 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors)
model48 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors + Industry)
model49 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors + as.character(Number_Year))
model50 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors + as.character(Number_Year) + Industry)

stepAIC(model47)
stepAIC(model49)
stepAIC(model50)

#тесты короткой против длинной
waldtest(model43, model44)
waldtest(model43, model45)
waldtest(model43, model46)
waldtest(model44, model46)
waldtest(model45, model46)

waldtest(model47, model48)
waldtest(model47, model49)
waldtest(model47, model50)
waldtest(model48, model50)
waldtest(model49, model50)

stargazer(model43, model44, model45, model46, type = "html", df = FALSE, se = list(cse(model43), cse(model44), cse(model45), cse(model46)), out = "Summary_models7.html")
stargazer(model47, model48, model49, model50, type = "html", df = FALSE, se = list(cse(model47), cse(model48), cse(model49), cse(model50)), out = "Summary_models8.html")

###### --------------Разные проверки на устойчивость------------------ ######

model1_1 <- plm(data = outlier_data1 %>% dplyr::filter(log_DPS < 5), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Ticker", "Number_Year"))
summary(model1_1)
model1_3 <- update(model1_1, .~.-log_Board_meeting-Board_size-share_ID-CapexToSales-Average_WE)
summary(model1_3)
coeftest(model1_3, vcov = vcovHC(model1_3,type = "HC0"))
model1_2 <- plm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "individual", model = "random", index = c("Ticker", "Number_Year"))
summary(model1_2)
model1_4 <- plm(data = outlier_data1, log_DPS ~ Gender_diversity + Share_Foreign_Directors + Turnover_ratio + I(Gender_diversity^2), effect = "twoways", model = "within", index = c("Ticker", "Number_Year"))
summary(model1_4)

#с датой основной 

model51 <- plm(data = outlier_data1,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, model = "pooling",index = c("Number_Year", "Ticker"))
model52 <- plm(data = outlier_data1,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model53 <- plm(data = outlier_data1,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

stargazer(model51, model52, model53, type = "html", df = FALSE, se = list(cse(model51), cse(model52), cse(model53)), out = "Summary_models8.html")

#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(model53, model51) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(model52, model53) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(model52, type = "bp") #лучше пул
#случайные < пула 

#Double_Directors - FE, RE, pooled
#тут log_DPS
model51_1 <- plm(data = outlier_data_DD,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors, model = "pooling",index = c("Number_Year", "Ticker"))
model52_1 <- plm(data = outlier_data_DD,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model53_1 <- plm(data = outlier_data_DD,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

stargazer(model51_1, model52_1, model53_1, type = "html", df = FALSE, se = list(cse(model51_1), cse(model52_1), cse(model53_1)), out = "Summary_models8.html")

#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(model53_1, model51_1) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(model52_1, model53_1) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(model52_1, type = "bp") #лучше пул
#случайные < пула 

#Double_Directors - значимая переменная в фиксированных эффектах и фиксированные эффекты - лучшая модель по трем тестам!!!

#тут DY
model51_2 <- plm(data = outlier_data_DD,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors, model = "pooling",index = c("Number_Year", "Ticker"))
model52_2 <- plm(data = outlier_data_DD,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model53_2 <- plm(data = outlier_data_DD,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + Double_Directors, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

stargazer(model51_2, model52_2, model53_2, type = "html", df = FALSE, se = list(cse(model51_2), cse(model52_2), cse(model53_2)), out = "Summary_models8_1.html")

#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(model53_2, model51_2) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(model52_2, model53_2) #лучше случайные эффекты
#фиксированные < случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(model52_2, type = "bp") #лучше случайные эффекты
#случайные > пула 

#Double_Directors - значимая переменная в случайных эффектах и случайные эффекты - лучшая модель по трем тестам!!!

#фиксированные > пула > случайные 

#очищение
summary(model53)
summary(outlier_data1)

#ограничение - Stage_Indicator > 0
model53_1 <- plm(data = outlier_data1 %>% dplyr::filter(Stage_Indicator > 0),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
#ограничение - Gov_Part < 0.6
model53_2 <- plm(data = outlier_data1 %>% dplyr::filter(Gov_Part < 0.6),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
#ограничение - log_DPS < 5
model53_3 <- plm(data = outlier_data1 %>% dplyr::filter(log_DPS < 5),log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

stargazer(model53, model53_1, model53_2, model53_3, type = "html", df = FALSE, se = list(cse(model53), cse(model53_1), cse(model53_2), cse(model53_3)), out = "FE_sustain.html")

summary(model53)
model53_4 <- plm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gender_diversity  + Average_WE  + Share_Foreign_Directors + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
summary(model53_4)
#ограничение - Stage_Indicator > 0
model53_5 <- plm(data = outlier_data1 %>% dplyr::filter(Stage_Indicator > 0), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gender_diversity  + Average_WE  + Share_Foreign_Directors + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
summary(model53_5)
#ограничение - log_DPS < 5
model53_6 <- plm(data = outlier_data1 %>% dplyr::filter(log_DPS < 5), log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gender_diversity  + Average_WE  + Share_Foreign_Directors + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
summary(model53_6)

stargazer(model53, model53_1, model53_3, model53_4, model53_5, model53_6, type = "html", df = FALSE, se = list(cse(model53), cse(model53_1), cse(model53_3), cse(model53_4), cse(model53_5), cse(model53_6)), out = "FE_sustain.html")

#c DY

model54 <- plm(data = outlier_data1,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, model = "pooling",index = c("Number_Year", "Ticker"))
model55 <- plm(data = outlier_data1,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model56 <- plm(data = outlier_data1,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))


stargazer(model54, model55, model56, type = "html", df = FALSE, se = list(cse(model54), cse(model55), cse(model56)), out = "Summary_models9.html")


#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(model56, model54) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(model55, model56) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(model55, type = "bp") #лучше случайные
#случайные > пула 

#фиксированные > случайные > пул
#model56

#ограничение - Stage_Indicator > 0
model56_1 <- plm(data = outlier_data1 %>% dplyr::filter(Stage_Indicator > 0),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
#ограничение - Gov_Part < 0.6
model56_2 <- plm(data = outlier_data1 %>% dplyr::filter(Gov_Part < 0.6),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
#ограничение - log_DPS < 5
model56_3 <- plm(data = outlier_data1 %>% dplyr::filter(log_DPS < 5),DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

summary(model56)
model56_4 <- plm(data = outlier_data1, DY ~ ROA + CapexToSales + log_Assets + Board_size + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
summary(model56_4)
#ограничение - Stage_Indicator > 0
model56_5 <- plm(data = outlier_data1 %>% dplyr::filter(Stage_Indicator > 0), DY ~ ROA + CapexToSales + log_Assets + Board_size + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
summary(model56_5)
#ограничение - log_DPS < 5
model56_6 <- plm(data = outlier_data1 %>% dplyr::filter(log_DPS < 5), DY ~ ROA + CapexToSales + log_Assets + Board_size + Share_Foreign_Directors + Gov_Part + Turnover_ratio, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))
summary(model56_6)

stargazer(model56, model56_1, model56_3, model56_4, model56_6, type = "html", df = FALSE, se = list(cse(model56), cse(model56_1), cse(model56_3), cse(model56_4), cse(model56_6)), out = "FE_sustain1.html")

###### --------------МОДЕЛИ С DOUBLE_DIRECTORS------------------ ######

model70 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)
model71 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors + Industry)
model72 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors + as.character(Number_Year))
model73 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors + as.character(Number_Year) + Industry)

model74 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)
model75 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors + Industry)
model76 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors + as.character(Number_Year))
model77 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors + as.character(Number_Year) + Industry)

stargazer(model70, model71, model72, model73, type = "html", df = FALSE, 
          se = list(cse(model70), cse(model71), cse(model72), cse(model73)), out = "Summary_DD_1.html")
stargazer(model74, model75, model76, model77, type = "html", df = FALSE, 
          se = list(cse(model74), cse(model75), cse(model76), cse(model77)), out = "Summary_DD_2.html")

##### --------------КВАНТИЛЬНАЯ РЕГРЕССИЯ------------------ #####
#для каждого квантиля оценивается отдельное уравнение. Тау – кровни квантилей

#асимметричные зависимые переменные 
g9 <- outlier_data1 %>% dplyr::select(log_DPS, DY) %>% pivot_longer(cols = "log_DPS":"DY", values_to = "Values", names_to = "Div")
ggplot(data = g9, aes(x = Values, fill = Div)) + geom_density() + facet_wrap(~Div, scale = "free")


q1 <- rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
     DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
     Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, 
   data = outlier_data1, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q1, se = "nid")
plot(q1, nrow = 2, ncol = 1)
plot(q1)
anova(q1)

round(q1$coefficients,2)
write.xlsx(as.data.frame(round(q1$coefficients,3)), file = "file1.xlsx")
taus<-seq(from = .05, to = .95, by = 0.05) #Taus ranging from 0.05 to 0.95 with a step value of 0.05
quant_all  <- rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                   DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                   Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio,
                 tau = taus, 
                 data = outlier_data1)

aic_df <- data.frame(AIC = AIC(quant_all), model = paste("tau =",taus))
ggplot(aic_df, aes(x = model, y = AIC)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "AIC values for different tau values",
       x = "Quantile regression models with different Tau values",
       y = "Akaike Information Criterion") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

q2 <- rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
           Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, 
         data = outlier_data1, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q2, se = "nid")
write.xlsx(as.data.frame(round(q2$coefficients,3)), file = "file2.xlsx")

names(outlier_data_HHI1)
q3 <- rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + HHI_level0_adj, 
         data = outlier_data_HHI1, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q3, se = "nid")
write.xlsx(as.data.frame(round(q3$coefficients,3)), file = "file3.xlsx")

q4 <- rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + HHI_level0_adj, 
         data = outlier_data_HHI1, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q4, se = "nid")
write.xlsx(as.data.frame(round(q4$coefficients,3)), file = "file3.xlsx")

q5 <- rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
           Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board, 
         data = data_CEO, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q5, se = "nid")

write.xlsx(as.data.frame(round(q5$coefficients,3)), file = "file5.xlsx")

q6 <- rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
           Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board, 
         data = data_CEO, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q6, se = "nid")

write.xlsx(as.data.frame(round(q6$coefficients,3)), file = "file6.xlsx")

q7 <- rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
           Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors, 
         data = outlier_data_DD, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q7, se = "nid")

write.xlsx(as.data.frame(round(q7$coefficients,3)), file = "file7.xlsx")

q8 <- rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
           DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
           Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors, 
         data = outlier_data_DD, tau = c(0.10, 0.25, 0.5, 0.75, 0.9))
summary(q8, se = "nid")

write.xlsx(as.data.frame(round(q8$coefficients,3)), file = "file8.xlsx")

g10 <- q4$fitted.values %>% as.data.frame() %>% mutate(DY = outlier_data_HHI1$DY, HHI_level0_adj = outlier_data_HHI1$HHI_level0_adj)
colnames(g10) <- c("q10", "q25", "q50", "q75", "q90", "DY", "HHI_adjusted")
g11 <- g10 %>% group_by(DY, HHI_adjusted) %>% pivot_longer(cols = "q10":"q90", names_to = "quantiles", values_to = "values")
g12 <- ggplot(data = outlier_data_HHI1, aes(x = HHI_level0_adj, y = DY)) + geom_point(color = "grey35") + 
  geom_quantile(aes(colour = "10-ый перцентиль"), quantiles = 0.1, size = 1.5) + 
  geom_quantile(aes(colour = "25-ый перцентиль"), quantiles = 0.25, size = 1.5) + 
  geom_quantile(aes(colour = "50-ый перцентиль"), quantiles = 0.5, size = 1.5) + 
  geom_quantile(aes(colour = "75-ый перцентиль"), quantiles = 0.75, size = 1.5) + 
  geom_quantile(aes(colour = "90-ый перцентиль"), quantiles = 0.9, size = 1.5) + theme_classic() + 
  geom_rug() + scale_color_manual(values = c("lightblue", "slateblue", "darkblue", "darkred", "deepskyblue4")) + 
  xlab("HHI скорректированный") + ylab("Дивидендная доходность") + guides(color=guide_legend(title="Перцентили")) 

g13 <- ggplot(data = outlier_data_HHI1, aes(x = HHI_level0_adj, y = log_DPS)) + geom_point(color = "grey35") + 
  geom_quantile(aes(colour = "10-ый перцентиль"), quantiles = 0.1, size = 1.5) + 
  geom_quantile(aes(colour = "25-ый перцентиль"), quantiles = 0.25, size = 1.5) + 
  geom_quantile(aes(colour = "50-ый перцентиль"), quantiles = 0.5, size = 1.5) + 
  geom_quantile(aes(colour = "75-ый перцентиль"), quantiles = 0.75, size = 1.5) + 
  geom_quantile(aes(colour = "90-ый перцентиль"), quantiles = 0.9, size = 1.5) + theme_classic() + 
  geom_rug() + scale_color_manual(values = c("lightblue", "slateblue", "darkblue", "darkred", "deepskyblue4")) + 
  xlab("HHI скорректированный") + ylab("Логарифмированный дивиденд на одну акцию") + guides(color=guide_legend(title="Перцентили")) 

grid.arrange(g12, g13, ncol = 2)
#графики для квантильной регрессии 
quants <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_res <- map(quants, ~rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, 
                tau = .x, 
                data = outlier_data1))
qr_res
qr_tidy <- map(qr_res, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
qr_tidy
?tidy
ols_res <- lm(data = outlier_data1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
ols_tidy <- broom::tidy(ols_res) %>% dplyr::filter(term %in% c("Board_size", "Gender_diversity", "log_Board_meeting", "Average_age", "Average_WE", "share_ID", "Share_Foreign_Directors", "Turnover_ratio")) 

unique(qr_tidy$term)
dim(qr_tidy)
qr_tidy %>% dplyr::filter(term %in% c("Board_size", "Gender_diversity", "log_Board_meeting", "Average_age", "Average_WE", "share_ID", "Share_Foreign_Directors", "Turnover_ratio")) %>% ggplot(aes(x = tau,
             y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
  position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  facet_wrap(~term, scales="free", ncol=4, labeller = as_labeller(c(Average_age='Average_age',
                                                                    Average_WE='Average_tenure',
                                                                    Board_size='Board_size',
                                                                    Gender_diversity='Gender_diversity', 
                                                                    log_Board_meeting = 'log_Board_meeting', 
                                                                    share_ID = 'Share_Independent', 
                                                                    Turnover_ratio = 'Turnover_ratio', 
                                                                    Share_Foreign_Directors = 'Share_Foreign'))) + 
  theme_classic() + ylab("") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для характеристик корпоративного управления и 
логарифмированного дивиденда на одну акцию")

#графики для квантильной регрессии 
quants <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_res1 <- map(quants, ~rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                            DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                            Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio, 
                          tau = .x, 
                          data = outlier_data1))
qr_tidy1 <- map(qr_res1, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res1 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio)
ols_tidy1 <- broom::tidy(ols_res1) %>% dplyr::filter(term %in% c("Board_size", "Gender_diversity", "log_Board_meeting", "Average_age", "Average_WE", "share_ID", "Share_Foreign_Directors", "Turnover_ratio")) 

qr_tidy1 %>% dplyr::filter(term %in% c("Board_size", "Gender_diversity", "log_Board_meeting", "Average_age", "Average_WE", "share_ID", "Share_Foreign_Directors", "Turnover_ratio")) %>% ggplot(aes(x = tau,
                                                                                                                                                                                                   y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy1, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  facet_wrap(~term, scales="free", ncol=4, labeller = as_labeller(c(Average_age='Average_age',
                                                                    Average_WE='Average_tenure',
                                                                    Board_size='Board_size',
                                                                    Gender_diversity='Gender_diversity', 
                                                                    log_Board_meeting = 'log_Board_meeting', 
                                                                    share_ID = 'Share_Independent', 
                                                                    Turnover_ratio = 'Turnover_ratio', 
                                                                    Share_Foreign_Directors = 'Share_Foreign'))) + 
  theme_classic() + ylab("") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для характеристик корпоративного управления и 
дивидендной доходности")

names(outlier_data_HHI1)

qr_res2 <- map(quants, ~rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                             DebtToAssets + Gov_Part + HHI_level0_adj, 
                           tau = .x, 
                           data = outlier_data_HHI1))
qr_tidy2 <- map(qr_res2, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res2 <- lm(data = outlier_data_HHI1, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                 DebtToAssets + Gov_Part + HHI_level0_adj)
ols_tidy2 <- broom::tidy(ols_res2) %>% dplyr::filter(term %in% c("HHI_level0_adj")) 

mm2 <- qr_tidy2 %>% dplyr::filter(term %in% c("HHI_level0_adj"))  %>% ggplot(aes(x = tau, y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy2, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  facet_wrap(~term, scales="free", ncol=4, labeller = as_labeller(c(HHI_level0_adj='HHI_adjusted'))) + 
  theme_classic() + ylab("") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для индекса Герфиндаля-Хиршмана и 
логарифмированного дивиденда на одну акцию")

qr_res3 <- map(quants, ~rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                             DebtToAssets + Gov_Part + HHI_level0_adj, 
                           tau = .x, 
                           data = outlier_data_HHI1))
qr_tidy3 <- map(qr_res3, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res3 <- lm(data = outlier_data_HHI1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                 DebtToAssets + Gov_Part + HHI_level0_adj)
ols_tidy3 <- broom::tidy(ols_res3) %>% dplyr::filter(term %in% c("HHI_level0_adj")) 

mm3 <- qr_tidy3 %>% dplyr::filter(term %in% c("HHI_level0_adj"))  %>% ggplot(aes(x = tau, y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy3, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  facet_wrap(~term, scales="free", ncol=4, labeller = as_labeller(c(HHI_level0_adj='HHI_adjusted'))) + 
  theme_classic() + ylab("") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для индекса Герфиндаля-Хиршмана и 
дивидендной доходности")

grid.arrange(mm2, mm3, ncol = 2)

qr_res4 <- map(quants, ~rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                             DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                             Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board, 
                           tau = .x, 
                           data = data_CEO))
qr_tidy4 <- map(qr_res4, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res4 <- lm(data = data_CEO, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                 DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                 Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
ols_tidy4 <- broom::tidy(ols_res4) %>% dplyr::filter(term %in% c("CEO_in_BoardYes")) 

qrt4 <- qr_tidy4 %>% dplyr::filter(term %in% c("CEO_in_BoardYes")) %>% ggplot(aes(x = tau, y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy4, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  theme_classic() + ylab("Присутствие CEO в СД") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для присутствия СЕО в СД и 
логарифма дивиденда на одну акцию")

qr_res5 <- map(quants, ~rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                             DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                             Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board, 
                           tau = .x, 
                           data = data_CEO))
qr_tidy5 <- map(qr_res5, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res5 <- lm(data = data_CEO, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                 DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                 Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
ols_tidy5 <- broom::tidy(ols_res5) %>% dplyr::filter(term %in% c("CEO_in_BoardYes")) 

qrt5 <- qr_tidy5 %>% dplyr::filter(term %in% c("CEO_in_BoardYes")) %>% ggplot(aes(x = tau, y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy5, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  theme_classic() + ylab("Присутствие CEO в СД") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для присутствия СЕО в СД и 
дивидендной доходности")

grid.arrange(qrt4, qrt5, ncol = 2)

qr_res6 <- map(quants, ~rq(log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                             DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                             Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors, 
                           tau = .x, 
                           data = outlier_data_DD))
qr_tidy6 <- map(qr_res6, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res6 <- lm(data = outlier_data_DD, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                 DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                 Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)
ols_tidy6 <- broom::tidy(ols_res6) %>% dplyr::filter(term %in% c("Double_Directors")) 

qrt6 <- qr_tidy6 %>% dplyr::filter(term %in% c("Double_Directors")) %>% ggplot(aes(x = tau, y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy6, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  theme_classic() + ylab("Совмещение функций независимого директора в нескольких компаниях") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для совмещения функций независимого директора в нескольких компаниях и 
логарифмированного дивиденда на одну акцию")

qr_res7 <- map(quants, ~rq(DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                             DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                             Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors, 
                           tau = .x, 
                           data = outlier_data_DD))
qr_tidy7 <- map(qr_res7, ~broom::tidy(.x, conf.int = TRUE)) %>%
  bind_rows()
ols_res7 <- lm(data = outlier_data_DD, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + 
                 DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + 
                 Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + Double_Directors)
ols_tidy7 <- broom::tidy(ols_res7) %>% dplyr::filter(term %in% c("Double_Directors")) 

qrt7 <- qr_tidy7 %>% dplyr::filter(term %in% c("Double_Directors")) %>% ggplot(aes(x = tau, y = estimate)) + geom_point(color = "#27408b", size = 3) + 
  geom_line(color="#27408b", size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, size  = 0.3,
                position = position_dodge(.9)) + 
  geom_hline(data = ols_tidy7, aes(yintercept = estimate, color = "Коэффициент в регрессии")) +
  geom_hline(aes(yintercept = 0, color = "Для тестирования на значимость")) + 
  theme_classic() + ylab("Совмещение функций независимого директора в нескольких компаниях") + xlab("Квантили") + theme(strip.background =element_rect(fill="lightblue")) + theme(legend.position = "top") + guides(color=guide_legend(title="")) + 
  scale_color_manual(values = c("green", "red")) + ggtitle("Квантильная регрессия для совмещения функций независимого директора в нескольких компаниях и 
дивидендной доходности")

grid.arrange(qrt6, qrt7, ncol = 2)

##### --------------СЛУЧАЙНЫЙ ЛЕС------------------ #####
forest_data1 <- outlier_data1 %>% dplyr::select(-ROE, -Ticker, -Number_Year, -Percentile, -Board_meetings, -Industry, -DPS, -DY, -Outlier, -Double_Directors)
forest_data2 <- outlier_data1 %>% dplyr::select(-ROE, -Ticker, -Number_Year, -Percentile, -Board_meetings, -Industry, -DPS, -log_DPS, -Outlier, -Double_Directors)
forest_data3 <- outlier_data_HHI1 %>% dplyr::select(ROA, Stage_Indicator, CapexToSales, log_Assets, DebtToAssets, Gov_Part, HHI_level0_adj, DY)
names(outlier_data_DD)
forest_data4 <- outlier_data_DD %>% dplyr::select(-ROE, -Ticker, -Number_Year, -Percentile, -Board_meetings, -Industry, -DPS, -DY, -Outlier)
forest_data5 <- outlier_data_DD %>% dplyr::select(-ROE, -Ticker, -Number_Year, -Percentile, -Board_meetings, -Industry, -DPS, -log_DPS, -Outlier)
names(data_CEO)
forest_data6 <- data_CEO %>% dplyr::select(-ROE, -Ticker, -Number_Year, -Percentile, -Board_meetings, -Industry, -DPS, -DY, -Outlier, -Double_Directors, -Year_2014_2020_2022, -Company, -Name_Chaiman, -Name_CEO)
forest_data7 <- data_CEO %>% dplyr::select(-ROE, -Ticker, -Number_Year, -Percentile, -Board_meetings, -Industry, -DPS, -log_DPS, -Outlier, -Double_Directors, -Year_2014_2020_2022, -Company, -Name_Chaiman, -Name_CEO)


#ЛЕС для log_DPS и первого сета переменных
forest1 <- randomForest(log_DPS ~ ., data = forest_data1, localImp = TRUE)
Pr_model_forest1 <- predict(forest1)
data_forest1 <- data.frame(y_true = forest_data1$log_DPS, y_predict = Pr_model_forest1)
min_depth_frame1 <- min_depth_distribution(forest1)
head(min_depth_frame1, n = 10)
labels_y2 <- c("ROA", "Показатель жизненного цикла", "Debt to Assets", "Размер Совета директоров", 
               "Средний стаж работы в СД", "Доля независимых директоров", "Средний возраст директоров", 
               "Логарифм активов", "Доля иностранных директоров", "Количество заседаний СД")
labels_y2

min_depth_frame1 %>% group_by(variable, minimal_depth) %>% summarise(count = n()) 
min_depth_frame1 %>% group_by(variable, minimal_depth) %>% summarise(count = n()) %>% group_by(variable) %>% summarise(depth = sum(minimal_depth*count)/500)

#график распределения минимальной глубины для десяти лучших переменных
plot_min_depth_distribution(min_depth_frame1) + scale_x_discrete(label = labels_y2) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", 
                               "#e45149", "#b6d2dd", "#259086", "#0a463c", "darkblue")) + ggtitle("Распределение средней глубины дерева
для первого набора переменных и логарифмированного дивиденда на одну акцию")

###### SHAP VALUES ######
names(forest_data1)
fit <- ranger(log_DPS ~ .-Turnover_lag_ratio, data = forest_data1,)
model_unified <- ranger.unify(fit, forest_data1)
treeshap_res <- treeshap(model_unified, forest_data1)
s <- kernelshap(fit, forest_data1[,-15], bg_X = forest_data1)
sv_waterfall(shapviz(s)) + scale_fill_viridis_d()
sv_force(shapviz(s))
labels_y3 <- c("Гендерное разнообразие", "Постоянство совета директоров", "CAPEX к Выручке", 
               "Заседания СД", "Средний стаж работы в СД", "Средний возраст членов СД", 
               "Государственное участие", "Логарифм активов", "Доля иностранных директоров", "Размер СД", 
               "Доля независимых директоров", "Показатель жизненного цикла", "Долг к активам", "ROA")
names(shapviz(s))

sv_importance(shapviz(s)[,-14], kind = "bee",  show_numbers = TRUE) + theme_classic() + scale_color_gradient2(low="darkred", mid="yellow",
                                                                                  high="darkblue", midpoint=0.5) + scale_y_discrete(label = labels_y3)

plot_contribution(treeshap_res)
plot_feature_importance(treeshap_res, max_vars = 8)


fit1 <- ranger(DY ~ .-Turnover_lag_ratio, data = forest_data2,)
model_unified1 <- ranger.unify(fit1, forest_data2)
treeshap_res1 <- treeshap(model_unified1, forest_data2)
s1 <- kernelshap(fit1, forest_data2[,-15], bg_X = forest_data2)
sv_waterfall(shapviz(s1)) + scale_fill_viridis_d()
sv_force(shapviz(s1))
labels_y3 <- c("Гендерное разнообразие", "Постоянство совета директоров", "CAPEX к Выручке", 
               "Заседания СД", "Средний стаж работы в СД", "Средний возраст членов СД", 
               "Государственное участие", "Логарифм активов", "Доля иностранных директоров", "Размер СД", 
               "Доля независимых директоров", "Показатель жизненного цикла", "Долг к активам", "ROA")
names(shapviz(s1))

labels_y4 <- c("Доля иностранных директоров", "Средний возраст членов СД", "Доля независимых директоров", "Постоянство совета директоров", 
               "Государственное участие", "Заседания СД", "Средний стаж работы в СД", "Долг к активам", 
               "CAPEX к Выручке", "Логарифм активов", "Гендерное разнообразие", "Показатель жизненного цикла", "Размер совета директоров", "ROA")

sv_importance(shapviz(s1)[,-14], kind = "bee",  show_numbers = TRUE) + theme_classic() + scale_color_gradient2(low="darkred", mid="yellow",
                                                                                                              high="darkblue", midpoint=0.5) + scale_y_discrete(label = labels_y4)
fit2 <- ranger(DY ~ ., data = forest_data3,)
model_unified2 <- ranger.unify(fit2, forest_data3)
treeshap_res2 <- treeshap(model_unified2, forest_data3)
s2 <- kernelshap(fit2, forest_data3[,-8], bg_X = forest_data3)

labels_y5 <- c("Государственное участие", "Показатель жизненного цикла", "CAPEX к Выручке", "Логарифм активов", 
               "Долг к активам", "ROA", "HHI скорректированный")
sv_importance(shapviz(s2)[,-8], kind = "bee",  show_numbers = TRUE) + theme_classic() + scale_color_gradient2(low="darkred", mid="yellow",
                                                                                                               high="darkblue", midpoint=0.5) + scale_y_discrete(label = labels_y5)


#ЛЕС для DY и первого сета переменных
forest2 <- randomForest(DY ~ ., data = forest_data2, localImp = TRUE)
min_depth_frame2 <- min_depth_distribution(forest2)
head(min_depth_frame2, n = 10)
labels_y3 <- c("ROA", "Показатель жизненного цикла", "Размер Совета директоров", "Логарифм активов", "CAPEX к Выручке", 
               "Средний стаж работы в СД", "Количество заседаний СД", "Долг к активам", "Постоянство совета директоров", 
               "Средний возраст директоров")
#график распределения минимальной глубины для десяти лучших переменных
plot_min_depth_distribution(min_depth_frame2) + scale_x_discrete(label = labels_y3) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", 
                               "#e45149", "#b6d2dd", "#259086", "#0a463c", "blue2", "lightblue3")) + ggtitle("Распределение средней глубины дерева
для первого набора переменных и дивидендной доходности")

View(min_depth_frame2 %>% group_by(variable, minimal_depth) %>% summarise(count = n())) 
min_depth_frame2 %>% group_by(variable, minimal_depth) %>% summarise(count = n()) %>% group_by(variable) %>% summarise(depth = sum(minimal_depth*count)/500)


#ЛЕС для DY и второго сета переменных
forest3 <- randomForest(DY ~ ., data = forest_data3, localImp = TRUE)
min_depth_frame3 <- min_depth_distribution(forest3)
head(min_depth_frame3, n = 10)
labels_y4 <- c("HHI", "ROA", "Логарифм активов", "Показатель жизненного цикла", "CAPEX к Выручке", "Долг к активам", 
               "Государственное участие")
#график распределения минимальной глубины для десяти лучших переменных
plot_min_depth_distribution(min_depth_frame3) + scale_x_discrete(label = labels_y4) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", 
                               "#e45149", "#b6d2dd", "#259086", "#0a463c")) + ggtitle("Распределение средней глубины дерева
для второго набора переменных и дивидендной доходности")

View(min_depth_frame3 %>% group_by(variable, minimal_depth) %>% summarise(count = n())) 
min_depth_frame3 %>% group_by(variable, minimal_depth) %>% summarise(count = n()) %>% group_by(variable) %>% summarise(depth = sum(minimal_depth*count)/500)

#var explained
forest1
forest2
forest3

#ЛЕС для log_DPS и Double_Directors
forest4 <- randomForest(log_DPS ~ ., data = forest_data4, localImp = TRUE)
min_depth_frame4 <- min_depth_distribution(forest4)
head(min_depth_frame4, n = 10)
plot_min_depth_distribution(min_depth_frame4)

#ЛЕС для DY и Double_Directors
forest5 <- randomForest(DY ~ ., data = forest_data5, localImp = TRUE)
min_depth_frame5 <- min_depth_distribution(forest5)
head(min_depth_frame5, n = 10)
plot_min_depth_distribution(min_depth_frame5)

#ЛЕС для log_DPS и CEO_in_Board
forest6 <- randomForest(log_DPS ~ ., data = forest_data6, localImp = TRUE)
min_depth_frame6 <- min_depth_distribution(forest6)
plot_min_depth_distribution(min_depth_frame6)

#ЛЕС для DY и CEO_in_Board
forest7 <- randomForest(DY ~ ., data = forest_data7, localImp = TRUE)
min_depth_frame7 <- min_depth_distribution(forest7)
plot_min_depth_distribution(min_depth_frame7)

###### Графики частичной зависимости ######
names(forest_data1)

forest_data1_mean1 <- transmute(forest_data1, ROA = mean(ROA), Stage_Indicator = mean(Stage_Indicator), 
                                CapexToSales = mean(CapexToSales), log_Assets = mean(log_Assets), DebtToAssets = mean(DebtToAssets), 
                                Board_size = mean(Board_size), Gender_diversity = mean(Gender_diversity), Average_age = mean(Average_age), 
                                Average_WE = mean(Average_WE), share_ID = mean(share_ID), Share_Foreign_Directors = mean(Share_Foreign_Directors), 
                                Gov_Part = mean(Gov_Part), Turnover_ratio = mean(Turnover_ratio), Turnover_lag_ratio = mean(Turnover_lag_ratio), 
                                log_Board_meeting = log_Board_meeting)
Predict_values <- predict(forest1, newdata = forest_data1_mean1)
forest_data1_mean2 <- mutate(forest_data1_mean1, Predict_values)

ggplot(forest_data1_mean2, aes(x = log_Board_meeting, y = Predict_values)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Логарифмированное количество заседаний СД") + ylab("Прогнозные значения логарифмированного дивиденда на одну акцию") + ggtitle("Отражение зависимости между параметром корпоративного управления и 
дивидендом на одну акцию для среднего наблюдения по выборке")

forest_data1_mean3 <- transmute(forest_data2, ROA = mean(ROA), Stage_Indicator = mean(Stage_Indicator), 
                                CapexToSales = mean(CapexToSales), log_Assets = mean(log_Assets), DebtToAssets = mean(DebtToAssets), 
                                Board_size = mean(Board_size), Gender_diversity = mean(Gender_diversity), Average_age = mean(Average_age), 
                                Average_WE = mean(Average_WE), share_ID = mean(share_ID), Share_Foreign_Directors = mean(Share_Foreign_Directors), 
                                Gov_Part = mean(Gov_Part), Turnover_ratio = mean(Turnover_ratio), Turnover_lag_ratio = mean(Turnover_lag_ratio), 
                                log_Board_meeting = log_Board_meeting)
Predict_values <- predict(forest2, newdata = forest_data1_mean3)
forest_data1_mean4 <- mutate(forest_data1_mean3, Predict_values)

ggplot(forest_data1_mean4, aes(x = log_Board_meeting, y = Predict_values)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Логарифмированное количество заседаний СД") + ylab("Дивидендная доходность") + ggtitle("Отражение зависимости между параметром корпоративного управления и 
дивидендной доходностью для среднего наблюдения по выборке")


forest_data1_mean5 <- transmute(forest_data3, ROA = mean(ROA), Stage_Indicator = mean(Stage_Indicator), 
                                CapexToSales = mean(CapexToSales), log_Assets = mean(log_Assets), DebtToAssets = mean(DebtToAssets), Gov_Part = mean(Gov_Part), 
                                HHI_level0_adj = HHI_level0_adj)
Predict_values <- predict(forest3, newdata = forest_data1_mean5)
forest_data1_mean6 <- mutate(forest_data1_mean5, Predict_values)

ggplot(forest_data1_mean5, aes(x = HHI_level0_adj, y = Predict_values)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Индекс Герфиндаля-Хиршмана") + ylab("Дивидендная доходность") + ggtitle("Отражение зависимости между параметром корпоративного управления и 
дивидендной доходностью для среднего наблюдения по выборке")

ggplot(  ) + 
  geom_point( aes(x = forest_data3$HHI_level0_adj, y = forest_data3$DY, color = 'red2') ) + 
  geom_point( aes(x = forest_data3$HHI_level0_adj , y = Predict_values, color = 'blue3')) + 
  labs(x = "Индекс Герфиндаля-Хиршмана", y = "Дивидендная доходность", color = "") +
  scale_color_manual(labels = c( "Предсказанное", "Фактическое"), values = c("blue3", "red2")) + theme_classic() + theme(legend.position = "top")

forest1_1 <- randomForest(log_DPS ~ ., data = forest_data1)
var_importance <- data_frame(variable=setdiff(colnames(forest_data1 %>% as.data.frame()),"log_DPS"),
                             importance=as.vector(forest1_1$importance))
var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Demographic Attribute") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + scale_fill_discrete(name="Variable Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))

##### --------------RPART------------------ #####
#Maxdepth – максимальное количество слоев
#Minbucket – минимальное число наблюдений в потомке
#Minsplit – минимальное число наблюдений в родителе
names(forest_data1)
d1 <- forest_data1
colnames(d1) <- c("ROA", "Stage_Indicator", "CapexToSales", "log_Assets", "DebtToAssets", 
                  "Board_size", "Gender_diversity", "Average_age", "Average_tenure", 
                  "Share_Independent", "Share_Foreign", "Gov_Part", "Turnover_ratio", "Turnover_lag_ratio", "log_DPS", "log_Board_meeting")
mod_tree1 <- rpart(log_DPS ~ ., data = d1, control = rpart.control(minsplit = 10, minbucket = 6, maxdepth = 5))
summary(mod_tree1)
mod_tree1$cptable[which.min(mod_tree1$cptable[,"xerror"]),"CP"]

mod_tree1_1 <- rpart(log_DPS ~ ., data = d1, control = rpart.control(minsplit = 10, minbucket = 6, maxdepth = 5, cp = 0.01))


names(forest_data2)
d2 <- forest_data2
colnames(d2) <- c("ROA", "Stage_Indicator", "CapexToSales", "log_Assets", "DebtToAssets", 
                  "Board_size", "Gender_diversity", "Average_age", "Average_tenure", 
                  "Share_Independent", "Share_Foreign", "Gov_Part", "Turnover_ratio", "Turnover_lag_ratio", "DY", "log_Board_meeting")
mod_tree2 <- rpart(DY ~ ., data = d2, control = rpart.control(minsplit = 10, minbucket = 6, maxdepth = 5))
summary(mod_tree2)
mod_tree2$cptable[which.min(mod_tree2$cptable[,"xerror"]),"CP"]

mod_tree2_1 <- rpart(DY ~ ., data = d2, control = rpart.control(minsplit = 10, minbucket = 6, maxdepth = 5, cp = 0.06776927))

names(forest_data3)
d3 <- forest_data3
colnames(d3) <- c("ROA", "Stage_Indicator", "CapexToSales", "log_Assets", "DebtToAssets", "Gov_Part", 
                  "HHI_adjusted", "DY")
mod_tree3 <- rpart(DY ~ ., data = d3, control = rpart.control(minsplit = 10, minbucket = 6, maxdepth = 5))
mod_tree3$cptable[which.min(mod_tree3$cptable[,"xerror"]),"CP"]

mod_tree3_1 <- rpart(DY ~ ., data = d3, control = rpart.control(minsplit = 10, minbucket = 6, maxdepth = 5, cp = 0.01))


###### --------------Построение деревьев------------------ ######
rpart.plot(mod_tree1, digits = 2, box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.6)
rpart.plot(mod_tree2, digits = 2, box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.6)
rpart.plot(mod_tree3, digits = 2, box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.6)

par(mfrow = c(1,1))
par(mar=c(5.1 ,4.1 ,4.1 ,2.1)) 

rpart.plot(mod_tree1_1, digits = 2, box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.5)
rpart.plot(mod_tree2_1, digits = 2, box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.6)
rpart.plot(mod_tree3_1, digits = 2, box.palette = viridis::viridis(10, option = "D", begin = 0, end = 0.85), shadow.col = NULL, col = "grey99", type = 4, extra = 1, tweak = 1.5)


##### --------------КЛАСТЕРИЗАЦИЯ------------------ #####
model9 <- PCA(outlier_data1 %>% dplyr::select(-Ticker, -Number_Year, -Turnover_lag_ratio, -ROE, -Percentile, -Industry, -DPS, -Outlier, -Double_Directors, -DY, -Board_meetings))
#длина вектора отвечает за долю переданной информации конкретной переменной в PCA
fviz_pca_var(model9, col.var = "contrib", gradient.cols = c("lightblue", "blue", "darkblue"), repel = TRUE)

#как переменные задаются через главные компоненты
model9$var$coord

#график каменистой осыпи (сколько каждая главная компонента в себе несет)
fviz_eig(model9)
#имеет смысл рассматривать 3 компоненты

#интерпретация знака и компонент - матрица нагрузок
corrplot(model9$var$coord, is.corr = FALSE)

#значимые переменные 
corrplot(model9$var$cos2, is.corr = FALSE)

#каждую компоненту нужно задать информативно

H <- scale(outlier_data1 %>% dplyr::select(-Ticker, -Number_Year, -Turnover_lag_ratio, -ROE, -Percentile, -Industry, -DPS, -Outlier, -Double_Directors, -DY, -Board_meetings)) #стандартизация (приведение к mean = 0, sd = 1)
head(H)
A <- dist(H)
H1 <- hclust(A)

plot(H1, hang = -1) #подписи на одном уровне
plot(H1, hang = -1, ann = FALSE) 
rect.hclust(H1, k = 2, border = "darkblue") #разделение на два кластера
barchart(H1, data = outlier_data1 %>% dplyr::select(-Ticker, -Number_Year, -Turnover_lag_ratio, -ROE, -Percentile, -Industry, -DPS, -Outlier, -Double_Directors, -DY, -Board_meetings), k = 2)
barchart(H1, data = H, k = 2)

fviz_dist(A) 
flexclust::barchart(H1, data = H, k = 4)

#ирерахическая кластеризация в пространстве главных комппонент
mod1_clust <- HCPC(model9)

model9_1 <- PCA(outlier_data1 %>% dplyr::select(-Ticker, -Number_Year, -Turnover_lag_ratio, -ROE, -Percentile, -Industry, -DPS, -Outlier, -Double_Directors, -log_DPS, -Board_meetings))
#длина вектора отвечает за долю переданной информации конкретной переменной в PCA
fviz_pca_var(model9_1, col.var = "contrib", gradient.cols = c("lightblue", "blue", "darkblue"), repel = TRUE)

#как переменные задаются через главные компоненты
model9_1$var$coord

#график каменистой осыпи (сколько каждая главная компонента в себе несет)
fviz_eig(model9_1)
#имеет смысл рассматривать 3 компоненты

#интерпретация знака и компонент - матрица нагрузок
corrplot(model9_1$var$coord, is.corr = FALSE)

#значимые переменные 
corrplot(model9_1$var$cos2, is.corr = FALSE)

#каждую компоненту нужно задать информативно

#ирерахическая кластеризация в пространстве главных комппонент
mod2_clust <- HCPC(model9_1)

fviz_cluster(mod2_clust, show_labels = FALSE, rect = TRUE)


res.hc <- eclust(data_DPS2, "kmeans", nboot = 5, k = 4)
fviz_silhouette(res.hc) # silhouette plot

##### --------------Регрессии с кластерами------------------ ######
mod1_clust$data.clust

mod80 <- lm(data = mod1_clust$data.clust,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(clust))
summary(mod80)

ggplot(data = mod1_clust$data.clust, aes(y = log_DPS, x = as.character(clust), fill = as.character(clust))) + geom_boxplot() + scale_fill_viridis_d()

ggplot(data = mod1_clust$data.clust, aes(y = log_DPS, x = ROA, color = as.character(clust), size = log_Assets)) + geom_point() + scale_color_viridis_d(alpha = 0.5)

dim1 <- model9$ind$cos2[,1]
dim2 <- model9$ind$cos2[,2]

dim3 <- model9_1$ind$cos2[,1]
dim4 <- model9_1$ind$cos2[,2]

mod81 <- lm(data = mod2_clust$data.clust,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(clust))
summary(mod81)

mod81_1 <- lm(data = mod2_clust$data.clust,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + as.character(clust))


ggplot(data = mod2_clust$data.clust, aes(y = DY, x = as.character(clust), fill = as.character(clust))) + geom_boxplot() + scale_fill_viridis_d()

mod82 <- lm(data = mod1_clust$data.clust,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1 + dim2)
summary(mod82)

mod83 <- lm(data = mod2_clust$data.clust,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim4)
summary(mod83)

bptest(mod82)
bptest(mod83)

##### --------------КЛАСТЕРИЗАЦИЯ компоненты для индекса ------------------ #####
names(outlier_data1)
data_qgraph <- outlier_data1 %>% dplyr::select(Board_size, Gender_diversity, log_Board_meeting, 
                                               Average_age, Average_WE, share_ID, Share_Foreign_Directors, Turnover_ratio)
colnames(data_qgraph) <- c("Board_size", "Gender_diversity", "log_Board_meeting", "Average_age", 
                           "Average_tenure", "Share_Independent", "Share_Foreign", "Turnover_ratio")
model9_2 <- PCA(data_qgraph)
data_qgraph <- outlier_data1 %>% dplyr::select(Board_size, Gender_diversity, log_Board_meeting, 
                                               Average_age, Average_WE, share_ID, Share_Foreign_Directors, Turnover_ratio)
qgraph(cor(outlier_data1 %>% dplyr::select(Board_size, Gender_diversity, log_Board_meeting, 
                                           Average_age, Average_WE, share_ID, Share_Foreign_Directors, Turnover_ratio)), 
       labels = colnames(data_qgraph))

#матрица нагрузок
corrplot(model9_2$var$coord, is.corr = FALSE)
fviz_pca_var(model9_2, col.var = "contrib", gradient.cols = c("lightblue", "blue", "darkblue"), repel = TRUE) + ggtitle("Доля переданной информации в PCA каждой переменной")
fviz_eig(model9_2, ylab = "Процент объяснения полученных данных", xlab = "Компонента", barfill = "darkblue", main = "График каменистой осыпи", ggtheme = theme_classic(), addlabels = TRUE) #имеет смысл рассматривать 2 компоненты 

write.xlsx(as.data.frame(model9_2$var$coord), file = "PCA.xlsx") 

#интерпретация знака и компонент - матрица нагрузок
corrplot(model9_2$var$coord, is.corr = FALSE, col=colorRampPalette(c("darkred","lightblue","darkblue"))(100), tl.col="black")

#значимые переменные 
corrplot(model9_2$var$cos2, is.corr = FALSE, col=colorRampPalette(c("darkred","lightblue","darkblue"))(100), tl.col="black")


dim1_1 <- model9_2$ind$cos2[,1]
dim2_1 <- model9_2$ind$cos2[,2]

dim_data <- data.frame(dim1 = dim1_1, 
                       dim2 = dim2_1) %>% pivot_longer(cols = "dim1":"dim2", names_to = "Dim", values_to = "Values")
ggplot(data = dim_data, aes(x = Values, fill = Dim)) + geom_density(alpha = 0.5)

#ирерахическая кластеризация в пространстве главных комппонент
mod3_clust <- HCPC(model9_2)

fviz_cluster(mod3_clust, show_labels = FALSE, rect = TRUE) + theme_classic() + ggtitle("Разбиение по кластерам") + scale_color_manual(values = c("slateblue", "lightblue"))

mod3_clust$data.clust

table(mod3_clust$data.clust$clust)

ggplot(data = mod3_clust$data.clust, aes(x = Average_age, y = Share_Independent, color = as.character(clust))) + geom_point()
ggplot(data = mod3_clust$data.clust, aes(y = Share_Independent, x = as.character(clust), fill = as.character(clust))) + geom_boxplot()
names(mod3_clust$data.clust)

ggpairs(mod3_clust$data.clust, columns = c(1:8), aes(color = clust, alpha = 0.5),
        lower = list(continuous = "smooth")) + theme_classic() + scale_fill_manual(values = c("slateblue", "lightblue")) + scale_color_manual(values = c("slateblue", "lightblue"))

mod84 <- lm(data = outlier_data1,
            log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1)
summary(mod84)

mod85 <- lm(data = outlier_data1,
            DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1)
summary(mod85)
bptest(mod85)
bptest(mod84)

vif(mod85)
vif(mod84)
stargazer(mod84, mod85, type = "html", df = FALSE, out = "mod85.html")

mod86 <- lm(data = outlier_data1, DY ~ ROA + dim1_1 + dim2_1)
mod87 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + dim1_1 + dim2_1)
mod88 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + dim1_1 + dim2_1)
mod89 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + dim1_1 + dim2_1)
mod90 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + dim1_1 + dim2_1)
mod91 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1)
mod92 <- lm(data = outlier_data1, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part)

stargazer(mod86, mod87, mod88, mod89, mod90, mod91, type = "html", df = FALSE, out = "models.html")

summary(mod91) #R squared - 0.1375
summary(mod92) #R squared - 0.1205
0.1375-0.1205

mod93 <- lm(data = outlier_data1,
            log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1 + as.character(mod3_clust$data.clust$clust))
summary(mod93)

mod94 <- lm(data = outlier_data1,
            DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1 + as.character(mod3_clust$data.clust$clust))
summary(mod94)

###### Кластеризация корректная ######
data_qgraph1 <- outlier_data1 %>% group_by(Ticker) %>% summarise(
  Board_size = median(Board_size), Gender_diversity = median(Gender_diversity), 
  log_Board_meeting = median(log_Board_meeting), Average_age = median(Average_age), 
  Average_tenure = median(Average_WE), Share_Independent = median(share_ID), 
  Share_Foreign = median(Share_Foreign_Directors), Turnover_ratio = median(Turnover_ratio)
)
data_qgraph1 <- as.data.frame(data_qgraph1)
rownames(data_qgraph1) <- data_qgraph1$Ticker
modPCA <- PCA(data_qgraph1[,-1])
View(data_qgraph1)
fviz_eig(modPCA, ylab = "Процент объяснения полученных данных", xlab = "Компонента", barfill = "darkblue", main = "График каменистой осыпи", ggtheme = theme_classic(), addlabels = TRUE) #имеет смысл рассматривать 2 компоненты 
fviz_pca_ind(modPCA, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_classic() +
  scale_color_gradient2(low="lightblue", mid="slateblue",
                        high="darkblue", midpoint=0.5)

fviz_pca_biplot(modPCA, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_classic() +
  scale_color_gradient2(low="lightblue", mid="slateblue",
                        high="darkblue", midpoint=0.5)

modPCA_clust  <- HCPC(modPCA)
#строим дендрограмму
fviz_dend(modPCA_clust, rect = TRUE, rect_fill = TRUE, cex = 0.8, palette = c("#664845", "#003986"))

fviz_cluster(modPCA_clust, repel = TRUE) + theme_classic() + scale_color_manual(values = c("#8e064e", "#172b69")) + scale_fill_manual(values = c("#8e064e", "#172b69"))

clust_data <- modPCA_clust$data.clust[,c(8,9)]
clust_data$Ticker <- rownames(clust_data)
clust_data <- clust_data[,-1]
clust_data1 <- merge(outlier_data1, clust_data)

mod95 <- lm(data = clust_data1,
            log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1 + as.character(clust))
summary(mod95)
bptest(mod95)

mod96 <- lm(data = clust_data1,
            DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + dim1_1 + dim2_1 + as.character(clust))
summary(mod96)
bptest(mod96)

stargazer(mod95, mod96, type = "html", se = list(cse(mod95), cse(mod96)), df = FALSE, out = "models.html")

mod97 <- lm(data = clust_data1,
            log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + as.character(clust))
summary(mod97)
bptest(mod95)

mod98 <- lm(data = clust_data1,
            DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + as.character(clust))
summary(mod98)
bptest(mod96)

stargazer(mod97, mod98, type = "html", se = list(cse(mod97), cse(mod98)), df = FALSE, out = "models_90.html")


names(clust_data1)
colnames(clust_data1)[14] <- "Average_tenure"
colnames(clust_data1)[15] <- "Share_Independent"
colnames(clust_data1)[16] <- "Share_Foreign"
ggpairs(clust_data1, columns = c(10,11,26,13,14,15,16,18), aes(color = clust, alpha = 0.5),
        lower = list(continuous = "smooth")) + theme_classic() + scale_fill_manual(values = c("slateblue", "lightblue")) + scale_color_manual(values = c("slateblue", "lightblue"))

#интерпретация знака и компонент - матрица нагрузок
corrplot(modPCA$var$coord, is.corr = FALSE)

#значимые переменные 
corrplot(modPCA$var$cos2, is.corr = FALSE)

#gt table
names(clust_data1)
clust_data1 %>% dplyr::select(Board_size, Gender_diversity, log_Board_meeting, 
                              Average_age, Average_tenure, Share_Independent, 
                              Share_Foreign, Turnover_ratio, clust) %>%
  tbl_summary(by = clust) %>%
  add_p()

###### Кластеризация по дивидендной доходности ######
data_qgraph2 <- outlier_data1 %>% group_by(Ticker) %>% summarise(
  Board_size = median(Board_size), Gender_diversity = median(Gender_diversity), 
  log_Board_meeting = median(log_Board_meeting), Average_age = median(Average_age), 
  Average_tenure = median(Average_WE), Share_Independent = median(share_ID), 
  Share_Foreign = median(Share_Foreign_Directors), Turnover_ratio = median(Turnover_ratio), DY = median(DY)
)
data_qgraph2 <- as.data.frame(data_qgraph2)
rownames(data_qgraph2) <- data_qgraph2$Ticker
modPCA2 <- PCA(data_qgraph2[,-1])
fviz_eig(modPCA2, ylab = "Процент объяснения полученных данных", xlab = "Компонента", barfill = "darkblue", main = "График каменистой осыпи", ggtheme = theme_classic(), addlabels = TRUE) #имеет смысл рассматривать 2 компоненты 
fviz_pca_ind(modPCA2, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_classic() +
  scale_color_gradient2(low="lightblue", mid="slateblue",
                        high="darkblue", midpoint=0.5)

fviz_pca_biplot(modPCA2, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_classic() +
  scale_color_gradient2(low="lightblue", mid="slateblue",
                        high="darkblue", midpoint=0.5)

modPCA_clust2  <- HCPC(modPCA2)
#строим дендрограмму
fviz_dend(modPCA_clust2, rect = TRUE, rect_fill = TRUE, cex = 0.8, palette = c("#664845", "#003986"))

fviz_cluster(modPCA_clust2, repel = TRUE) + theme_classic() + scale_color_manual(values = c("#8e064e", "#172b69", "lightblue")) + scale_fill_manual(values = c("#8e064e", "#172b69", "lightblue"))

clust_data2 <- modPCA_clust2$data.clust[,c(9,10)]
clust_data2$Ticker <- rownames(clust_data2)
clust_data2 <- clust_data2[,-1]
clust_data3 <- merge(outlier_data1, clust_data2)
head(clust_data3)

cl1 <- clust_data3 %>% group_by(clust) %>% pivot_longer(cols = c("Board_size", "Gender_diversity", "Average_age", "Average_WE", "share_ID", 
                                                                 "Share_Foreign_Directors", "Turnover_ratio", "log_Board_meeting", "DY"), 
                                                        names_to = "Variables", values_to = "Values")
ggplot(data = cl1, aes(x = clust, y = Values, fill = clust)) + geom_boxplot() + facet_wrap(~Variables, scales = "free_y") + theme_classic() + scale_fill_manual(values = c("#8e064e", "#172b69", "lightblue"))

#по каким кластерам есть отличия?
ggbetweenstats(data = clust_data3, x = clust, y = DY, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = Average_age, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = Average_WE, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = Board_size, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = Gender_diversity, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = log_Board_meeting, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = Share_Foreign_Directors, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = share_ID, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data3, x = clust, y = Turnover_ratio, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 

###### Кластеризация по логарифму дивиденда на одну акцию ######
data_qgraph3 <- outlier_data1 %>% group_by(Ticker) %>% summarise(
  Board_size = median(Board_size), Gender_diversity = median(Gender_diversity), 
  log_Board_meeting = median(log_Board_meeting), Average_age = median(Average_age), 
  Average_tenure = median(Average_WE), Share_Independent = median(share_ID), 
  Share_Foreign = median(Share_Foreign_Directors), Turnover_ratio = median(Turnover_ratio), log_DPS = median(log_DPS)
)
data_qgraph3 <- as.data.frame(data_qgraph3)
rownames(data_qgraph3) <- data_qgraph3$Ticker
modPCA3 <- PCA(data_qgraph3[,-1])
fviz_eig(modPCA3, ylab = "Процент объяснения полученных данных", xlab = "Компонента", barfill = "darkblue", main = "График каменистой осыпи", ggtheme = theme_classic(), addlabels = TRUE) #имеет смысл рассматривать 2 компоненты 
fviz_pca_ind(modPCA3, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_classic() +
  scale_color_gradient2(low="lightblue", mid="slateblue",
                        high="darkblue", midpoint=0.5)

fviz_pca_biplot(modPCA3, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_classic() +
  scale_color_gradient2(low="lightblue", mid="slateblue",
                        high="darkblue", midpoint=0.5)

modPCA_clust3  <- HCPC(modPCA3)
#строим дендрограмму
fviz_dend(modPCA_clust3, rect = TRUE, rect_fill = TRUE, cex = 0.8, palette = c("#664845", "#003986"))

fviz_cluster(modPCA_clust3, repel = TRUE) + theme_classic() + scale_color_manual(values = c("#8e064e", "#172b69", "lightblue")) + scale_fill_manual(values = c("#8e064e", "#172b69", "lightblue"))

clust_data4 <- modPCA_clust3$data.clust[,c(9,10)]
clust_data4$Ticker <- rownames(clust_data4)
clust_data4 <- clust_data4[,-1]
clust_data5 <- merge(outlier_data1, clust_data4)
head(clust_data5)

cl2 <- clust_data5 %>% group_by(clust) %>% pivot_longer(cols = c("Board_size", "Gender_diversity", "Average_age", "Average_WE", "share_ID", 
                                                                 "Share_Foreign_Directors", "Turnover_ratio", "log_Board_meeting", "log_DPS"), 
                                                        names_to = "Variables", values_to = "Values")
ggplot(data = cl2, aes(x = clust, y = Values, fill = clust)) + geom_boxplot() + facet_wrap(~Variables, scales = "free_y") + theme_classic() + scale_fill_manual(values = c("#8e064e", "#172b69", "lightblue"))

#по каким кластерам есть отличия?
ggbetweenstats(data = clust_data5, x = clust, y = log_DPS, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = Average_age, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = Average_WE, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = Board_size, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = Gender_diversity, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = log_Board_meeting, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = Share_Foreign_Directors, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = share_ID, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 
ggbetweenstats(data = clust_data5, x = clust, y = Turnover_ratio, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 

###### Просто иерархическая кластеризация с log_DPS ######
ic1 <- data_qgraph3
h1 <- hclust(dist(ic1))
#определяем число кластеров
fviz_nbclust(ic1, FUN = hcut, method = "wss") #2 кластера
flexclust::barchart(h1, ic1[,-1], k = 3)

cl_data1 <- ic1
cl_data1$Cluster <- cutree(h1, k = 3) 

head(outlier_data1)
cl2 <- outlier_data1
names(cl_data1)
cl3 <- merge(cl2, cl_data1[,c(1,11)])
ggbetweenstats(data = cl3, x = Cluster, y = log_DPS, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 

###### Просто иерархическая кластеризация с DY ######
ic2 <- data_qgraph2
h2 <- hclust(dist(ic2))
#определяем число кластеров
fviz_nbclust(ic2, FUN = hcut, method = "wss") #2 кластера
flexclust::barchart(h2, ic2[,-1], k = 3)

cl_data2 <- ic2
cl_data2$Cluster <- cutree(h2, k = 3) 

cl4 <- merge(cl2, cl_data2[,c(1,11)])
ggbetweenstats(data = cl4, x = Cluster, y = DY, p.adjust.method = "none", bf.message = FALSE,   title = "Результаты теста Геймса-Ховелла") 

##### -------------- CEO в Board ------------------ #####
CEO_Chairman_connections <- read_excel("CEO_Chairman_connections.xlsx")
colnames(CEO_Chairman_connections) <- c("Number_Year","Ticker",  "Company", "Name_Chaiman", "Name_CEO", "CEO_in_Board")
dim(CEO_Chairman_connections)
data_CEO <- merge(outlier_data1, CEO_Chairman_connections)
dim(data_CEO)

model78 <- lm(data = data_CEO, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
model79 <- lm(data = data_CEO, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board + Industry)
model80 <- lm(data = data_CEO, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board + as.character(Number_Year))
model81 <- lm(data = data_CEO, log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board + as.character(Number_Year) + Industry)

model82 <- lm(data = data_CEO, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board)
model83 <- lm(data = data_CEO, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board + Industry)
model84 <- lm(data = data_CEO, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board + as.character(Number_Year))
model85 <- lm(data = data_CEO, DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Gov_Part + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Turnover_ratio + CEO_in_Board + as.character(Number_Year) + Industry)

#результаты короткой против длинной
waldtest(model78, model79)
waldtest(model78, model80)
waldtest(model78, model81)
waldtest(model79, model81)
waldtest(model80, model81)

waldtest(model82, model83)
waldtest(model82, model84)
waldtest(model82, model85)
waldtest(model83, model85)
waldtest(model84, model85)

stargazer(model78, model79, model80, model81, type = "html", df = FALSE, 
          se = list(cse(model78), cse(model79), cse(model80), cse(model81)), out = "Summary_CEO_1.html")

stargazer(model82, model83, model84, model85, type = "html", df = FALSE, 
          se = list(cse(model82), cse(model83), cse(model84), cse(model85)), out = "Summary_CEO_2.html")

table(data_CEO$CEO_in_Board)

#c log_DPS

model86 <- plm(data = data_CEO,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + CEO_in_Board, model = "pooling",index = c("Number_Year", "Ticker"))
model87 <- plm(data = data_CEO,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + CEO_in_Board, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model88 <- plm(data = data_CEO,log_DPS ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + CEO_in_Board, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

stargazer(model86, model87, model88, type = "html", df = FALSE, se = list(cse(model86), cse(model87), cse(model88)), out = "Summary_plm_CEO_1.html")

model89 <- plm(data = data_CEO,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + CEO_in_Board, model = "pooling",index = c("Number_Year", "Ticker"))
model90 <- plm(data = data_CEO,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + CEO_in_Board, effect = "individual", model = "random", random.method = "walhus", index = c("Number_Year", "Ticker"))
model91 <- plm(data = data_CEO,DY ~ ROA + Stage_Indicator + CapexToSales + log_Assets + DebtToAssets + Board_size + Gender_diversity + log_Board_meeting + Average_age + Average_WE + share_ID + Share_Foreign_Directors + Gov_Part + Turnover_ratio + CEO_in_Board, effect = "twoways", model = "within", index = c("Number_Year", "Ticker"))

stargazer(model89, model90, model91, type = "html", df = FALSE, se = list(cse(model89), cse(model90), cse(model91)), out = "Summary_plm_CEO_2.html")


#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(model91, model89) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(model90, model91) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(model89, type = "bp") #лучше случайные
#случайные > пула 

#фиксированные > случайные > пул
#model91

###### Соотношение всех выборок ######
dim(outlier_data1)
dim(outlier_data_HHI1)
dim(outlier_data_DD)
dim(data_CEO)

#CEO + HH1
dim(merge(data_CEO, outlier_data_HHI1))

#CEO + DD
dim(merge(data_CEO, outlier_data_DD))

#DD + HH1
dim(merge(outlier_data_DD, outlier_data_HHI1))

#DD + HH1 + CEO
dim(merge(merge(outlier_data_DD, outlier_data_HHI1), data_CEO))

test <- data_CEO
test$CEO_in_Board <- ifelse(test$CEO_in_Board == "Yes", 1, 0)
summary(test)

stargazer(test, out = "test.html", type = "html")

##### --------------ГРАФИКИ------------------ #####
###### 1 - Как связаны дивидендная доходность и дивиденд на одну акцию? ######
g1 <- ggplot(data = data_DPS_DY, aes(x = log(DPS+1), y = DY)) + geom_point(color = "slateblue") + geom_smooth(color = "darkblue") + theme_classic() + ggtitle("Диаграмма рассеивания 
дивидендной доходности и дивиденда на одну акцию") + ylab("Дивидендная доходность") + xlab("Логарифмированный дивиденд на одну акцию")
cor(data_DPS_DY$DY, log(data_DPS_DY$DPS+1))
ggMarginal(g1, type="histogram", fill = "slateblue")

ggscatterstats(
  data  = outlier_data1,
  x     = log_DPS,
  y     = DY,
  ylab  = "Дивидендная доходность",
  xlab  = "Логарифмированный дивиденд на одну акцию",
  title = "Диаграмма рассеивания 
дивидендной доходности и дивиденда на одну акцию", 
  xfill = "#018abd", 
  yfill = "#97cbdc", 
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x), 
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE),
  xsidehistogram.args = list(fill = "slateblue", color = "black", na.rm = TRUE),
  ysidehistogram.args = list(fill = "lightblue2", color = "black", na.rm = TRUE)
)

outlier_data1 %>% group_by(Number_Year) %>% summarise(mean_DY = mean(DY))

###### 2 - Есть ли отличия дивидендной доходности и дивиденда на одну акцию по годам? ######
#Дивидендная доходность
str(data_DY)
ggbetweenstats(
  data = outlier_data1, 
  x     = Number_Year,
  y     = DY,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Год", 
  ylab = "Дивидендная доходность", 
  p.adjust.method = "none",
  bf.message = FALSE) 

#Дивиденд на одну акцию 
ggbetweenstats(
  data = outlier_data1, 
  x     = Number_Year,
  y     = log_DPS,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Год", 
  ylab = "Логарифмированный дивиденд", 
  p.adjust.method = "none",
  bf.message = FALSE) 

grd <- outlier_data1 %>% group_by(Number_Year) %>% summarise(median_DY = median(DY)) %>% as.data.frame()
grd1 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(mean_DY = mean(DY)) %>% as.data.frame()
grd2 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(mean_DPS = mean(log_DPS)) %>% as.data.frame()

m4 <- ggplot(data = outlier_data1, aes(x = as.character(Number_Year), y = DY, group = as.character(Number_Year))) + geom_boxplot(fill = "darkblue", color = "grey") + theme_classic() + ylab("Дивидендная доходность") + xlab("Год") + geom_point(data = grd1, mapping = aes(x = as.character(Number_Year), y = mean_DY), color="green") +
  geom_line(data = grd1, mapping = aes(x = as.character(Number_Year), y = mean_DY, group=1), color = "lightblue")

m5 <- ggplot(data = outlier_data1, aes(x = as.character(Number_Year), y = log_DPS, group = as.character(Number_Year))) + geom_boxplot(fill = "darkblue", color = "grey") + theme_classic() + ylab("Логарифмированный дивиденд на одну акцию") + xlab("Год") + geom_point(data = grd2, mapping = aes(x = as.character(Number_Year), y = mean_DPS), color="green") +
  geom_line(data = grd2, mapping = aes(x = as.character(Number_Year), y = mean_DPS, group=1), color = "lightblue")

grid.arrange(m4, m5, ncol = 2)
names(outlier_data1)
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2013) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2014) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2015) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2016) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2017) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2018) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2019) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2020) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2021) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2010) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2022) %>% dplyr::select(DY))

t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2013) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2014) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2015) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2016) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2017) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2018) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2019) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2020) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2021) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2011) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2022) %>% dplyr::select(DY))

t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2013) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2014) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2015) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2016) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2017) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2018) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2019) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2020) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2021) %>% dplyr::select(DY))
t.test(outlier_data1 %>% dplyr::filter(Number_Year == 2012) %>% dplyr::select(DY), 
       outlier_data1 %>% dplyr::filter(Number_Year == 2022) %>% dplyr::select(DY))

###### 3 - Корреляция ######
str(data_DPS)
names(data_DPS)
ggcorrmat(data = data_DPS[,-c(1,2, 5, 21)], 
          colors = c("darkblue", "white", "darkred"))

names(data_DY)
ggcorrmat(data = data_DY[,-c(1,2, 5, 21)], 
          colors = c("darkblue", "white", "darkred"))

###### 4 - Распределение по отраслям ######
table(outlier_data1$Industry, outlier_data1$Number_Year)
?chordDiagram
grid.col = c("2010" = "blue1", "2011" = "blue2", "2012" = "blue3","2013" = "blue4", "2014"="lightblue4", "2015" = "lightblue3", "2016" ="lightblue2", "2017" = "lightblue1", "2018" = "slateblue", "2019"="slateblue1","2020"= "slateblue2", "2021"= "slateblue3", "2022"= "purple",
             "Telecoms" = "lightblue", "Oil & Gas" = "#163B88", "Retail" = "#7990A3", "Metals & Mining" = "#748CDB", "Industrials" = "#C1DFF9", "Energy" = "#192D45")
chordDiagram(chord_data, grid.col = grid.col)

ggplot(outlier_data1, aes(x = log_DPS, y = Industry, group = Industry, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Квартили", alpha = 0.7, option = "E") + theme_classic() + xlab("Логарифмированный дивиденд на одну акцию") + ylab("Отрасль")

ggplot(data_DY, aes(x = DY, y = Industry, group = Industry, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Квартили", alpha = 0.7, option = "G") + theme_classic() + xlab("Дивидендная доходность") + ylab("Отрасль")

###### 5 - Как меняется следование нормам ККУ? ######
names(outlier_data_HHI1)
g3 <- ggplot(data = outlier_data_HHI1, aes(x = as.character(Number_Year), y = HHI_level0)) + geom_boxplot(fill = "#E8EDE7") + theme_classic() + geom_hline(yintercept=median(outlier_data_HHI1$HHI_level0), linetype="dashed", color = "#036280", size=1) + 
                                                                                                                                          geom_hline(yintercept=mean(outlier_data_HHI1$HHI_level0), linetype="dashed", color = "#81BECE", size=1) + 
                                                                                                                                          annotate("label", fill = "#81BECE", x="2022", y=mean(outlier_data_HHI1$HHI_level0), label= "mean") + 
                                                                                                                                          annotate("label", fill = "#036280", x="2010", y=median(outlier_data_HHI1$HHI_level0), label= "median") + xlab("Год") + ylab("Индекс Герфиндаля Хиршмана")
g4 <- ggplot(data = outlier_data_HHI1, aes(x = as.character(Number_Year), y = HHI_level0_adj)) + geom_boxplot(fill = "#E8EDE7") + theme_classic() + geom_hline(yintercept=median(outlier_data_HHI1$HHI_level0_adj), linetype="dashed", color = "#036280", size=1) + 
  geom_hline(yintercept=mean(outlier_data_HHI1$HHI_level0_adj), linetype="dashed", color = "#81BECE", size=1) + 
  annotate("label", fill = "#81BECE", x="2022", y=mean(outlier_data_HHI1$HHI_level0_adj), label= "mean") + 
  annotate("label", fill = "#036280", x="2010", y=median(outlier_data_HHI1$HHI_level0_adj), label= "median") + xlab("Год") + ylab("Индекс Герфиндаля Хиршмана скорректированный")
dim(outlier_data_HHI1)
grid.arrange(g3, g4, ncol = 2)

g5 <- ggplot(data = outlier_data_HHI1, aes(x = Industry, y = HHI_level0)) + geom_boxplot(fill = "#E8EDE7") + theme_classic() + geom_hline(yintercept=median(outlier_data_HHI1$HHI_level0), linetype="dashed", color = "#036280", size=1) + 
  geom_hline(yintercept=mean(outlier_data_HHI1$HHI_level0), linetype="dashed", color = "#81BECE", size=1) + 
  annotate("label", fill = "#81BECE", x="Telecoms", y=mean(outlier_data_HHI1$HHI_level0), label= "mean") + 
  annotate("label", fill = "#036280", x="Energy", y=median(outlier_data_HHI1$HHI_level0), label= "median") + xlab("Отрасль") + ylab("Индекс Герфиндаля Хиршмана")
g6 <- ggplot(data = outlier_data_HHI1, aes(x = Industry, y = HHI_level0_adj)) + geom_boxplot(fill = "#E8EDE7") + theme_classic() + geom_hline(yintercept=median(outlier_data_HHI1$HHI_level0_adj), linetype="dashed", color = "#036280", size=1) + 
  geom_hline(yintercept=mean(outlier_data_HHI1$HHI_level0_adj), linetype="dashed", color = "#81BECE", size=1) + 
  annotate("label", fill = "#81BECE", x="Telecoms", y=mean(outlier_data_HHI1$HHI_level0_adj), label= "mean") + 
  annotate("label", fill = "#036280", x="Energy", y=median(outlier_data_HHI1$HHI_level0_adj), label= "median") + xlab("Отрасль") + ylab("Индекс Герфиндаля Хиршмана скорректированный")

grid.arrange(g5, g6, ncol = 2)

###### 6 - Доля в индексе МосБиржи ######

graph_data1 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(Percentile = sum(Percentile))
graph_data2 <- max_IMOEX %>% group_by(Svod, Year) %>% summarise(Value = sum(Value)) %>% dplyr::filter(Svod == 1)
colnames(graph_data2) <- c("Svod", "Number_Year", "Max_Percentile")
graph_data3 <- merge(graph_data1, graph_data2)
ggplot(data = graph_data3, aes(x = as.character(Number_Year), y = Percentile)) + geom_col(fill = "darkblue") + theme_classic() + ylab("% от индекса MOEX") + xlab("Год") + geom_errorbar(aes(x=factor(Number_Year),
                                                                                                                                                                                             y=Max_Percentile,ymin=Max_Percentile,
                                                                                                                                                                                             ymax=Max_Percentile,color="orange", size = 1)) + 
  geom_hline(yintercept=median(graph_data1$Percentile), linetype="dashed", color = "purple2", size=1) + 
  annotate("text", color = "purple2", x="2022", y=0.66, label= "медиана") + 
  geom_hline(yintercept=mean(graph_data1$Percentile), linetype="dashed", color = "blue", size=1) + 
  annotate("text", color = "blue", x="2022", y=0.59, label= "среднее") + 
  geom_label(aes(label = round(Percentile,2)))

graph_data4 <- outlier_data_HHI1 %>% group_by(Number_Year) %>% summarise(Percentile = sum(Percentile))
graph_data5 <- merge(graph_data4, graph_data2)
ggplot(data = graph_data5, aes(x = as.character(Number_Year), y = Percentile)) + geom_col(fill = "darkblue") + theme_classic() + ylab("% от индекса MOEX") + xlab("Год") + geom_errorbar(aes(x=factor(Number_Year),
                                                                                                                                                                                             y=Max_Percentile,ymin=Max_Percentile,
                                                                                                                                                                                             ymax=Max_Percentile,color="orange", size = 1)) + 
  geom_hline(yintercept=median(graph_data5$Percentile), linetype="dashed", color = "purple2", size=1) + 
  annotate("text", color = "purple2", x="2022", y=0.44, label= "медиана") + 
  geom_hline(yintercept=mean(graph_data5$Percentile), linetype="dashed", color = "blue", size=1) + 
  annotate("text", color = "blue", x="2022", y=0.37, label= "среднее") + 
  geom_label(aes(label = round(Percentile,2)))


graph_data6 <- outlier_data_DD %>% group_by(Number_Year) %>% summarise(Percentile = sum(Percentile))
graph_data7 <- merge(graph_data6, graph_data2)
ggplot(data = graph_data7, aes(x = as.character(Number_Year), y = Percentile)) + geom_col(fill = "darkblue") + theme_classic() + ylab("% от индекса MOEX") + xlab("Год") + geom_errorbar(aes(x=factor(Number_Year),
                                                                                                                                                                                             y=Max_Percentile,ymin=Max_Percentile,
                                                                                                                                                                                             ymax=Max_Percentile,color="orange", size = 1)) + 
  geom_hline(yintercept=median(graph_data7$Percentile), linetype="dashed", color = "purple2", size=1) + 
  annotate("text", color = "purple2", x="2022", y=0.62, label= "медиана") + 
  geom_hline(yintercept=mean(graph_data7$Percentile), linetype="dashed", color = "blue", size=1) + 
  annotate("text", color = "blue", x="2022", y=0.59, label= "среднее") + 
  geom_label(aes(label = round(Percentile,2)))


###### 7 - ККУ по индустриям и годам ######
names(outlier_data_HHI1)
graph_data2 <- outlier_data_HHI1 %>% group_by(Number_Year) %>% summarise(Not_complied = sum(Not_complied), 
                                                                         Partial_complied = sum(Partial_complied),
                                                                         Complied = sum(Complied)) %>% pivot_longer(cols = c("Not_complied", "Partial_complied", "Complied"), names_to = "Type", values_to = "Amount") %>% as.data.frame()
graph_data2$Type <- factor(graph_data2$Type, levels = c("Not_complied", "Partial_complied" , "Complied"),
                                              labels = c("Не соблюдается", "Частично соблюдается", "Соблюдается"))
g7 <- ggplot(data = graph_data2, aes(x = as.character(Number_Year), y = Amount, fill = Type)) + geom_col(position = "fill") + xlab("Год") + ylab("Пропорция категорий выполнения ККУ") + theme_classic() + scale_fill_viridis_d(option = "E") + theme(legend.position = "bottom") + guides(fill=guide_legend(title="Тип соблюдения")) 

graph_data3 <- outlier_data_HHI1 %>% group_by(Industry) %>% summarise(Not_complied = sum(Not_complied), 
                                                                         Partial_complied = sum(Partial_complied),
                                                                         Complied = sum(Complied)) %>% pivot_longer(cols = c("Not_complied", "Partial_complied", "Complied"), names_to = "Type", values_to = "Amount") %>% as.data.frame()
graph_data3$Type <- factor(graph_data3$Type, levels = c("Not_complied", "Partial_complied" , "Complied"),
                           labels = c("Не соблюдается", "Частично соблюдается", "Соблюдается"))

g8 <- ggplot(data = graph_data3, aes(x = Industry, y = Amount, fill = Type)) + geom_col(position = "fill") + xlab("Отрасль") + ylab("Пропорция категорий выполнения ККУ") + theme_classic() + scale_fill_viridis_d(option = "E") + theme(legend.position = "bottom") + guides(fill=guide_legend(title="Тип соблюдения")) 

grid.arrange(g7, g8, ncol = 2)

names(outlier_data_HHI1)
ggbetweenstats(
  data = outlier_data_HHI1, 
  x     = Industry,
  y     = HHI_level0_adj,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Отрасль", 
  ylab = "Скорректированный индекс Герфиндаля-Хиршмана", 
  p.adjust.method = "none",
  bf.message = FALSE) + scale_color_viridis_d()

###### 8 - Средняя динамика ККУ по годам ######
names(outlier_data_HHI1)
cor(outlier_data_HHI1$HHI_level0, outlier_data_HHI1$HHI_level0_adj)
graph_data4 <- outlier_data_HHI1 %>% group_by(Number_Year) %>% summarise(HHI = mean(HHI_level0), 
                                                                         HHI_adjusted = mean(HHI_level0_adj)) %>% pivot_longer(cols = c("HHI","HHI_adjusted"), names_to = "Type_HHI", values_to = "Value")
ggplot(data = graph_data4, aes(x = as.character(Number_Year), y = Value, group = Type_HHI, color = Type_HHI)) + geom_line(size = 1.5) + theme_classic() + ylab("Значение HHI") + xlab("Год") + theme(legend.position = "top") + geom_label(aes(label = round(Value,2))) + scale_color_manual(values = c("darkblue", "darkred"))
?theme

###### 9 - Анимированный график для Turnover > 0.52, < 0.52 ######
forest_data2
graph_data8 <- outlier_data1
graph_data8$Turnover_criteria <- ifelse(graph_data8$Turnover_ratio > 0.5, "High", "Low")

g1 <- ggplot(graph_data8, aes(x = as.character(Number_Year), y = DY, color = Turnover_criteria, group = Turnover_criteria)) + geom_line(size = 1.5) + theme_minimal() + scale_color_manual(values = c("darkblue", "slateblue", "lightblue")) + theme(axis.title.x = element_text(size = 20), 
                                                                                                                                                                                         axis.title.y = element_text(size = 20), 
                                                                                                                                                                                         axis.text.x = element_text(size = 15), 
                                                                                                                                                                                         axis.text.y = element_text(size = 15), 
                                                                                                                                                                                         legend.text = element_text(size = 15), 
                                                                                                                                                                                         legend.title = element_text(size = 15))
Plot1 <- g1 + transition_reveal(Number_Year) 
m <- gganimate::animate(Plot1, duration = 5, fps = 20, width = 1000, height = 1000, renderer = gifski_renderer())
anim_save("Turnover_ratio.gif",animation = m)

table(graph_data8$Turnover_criteria)

###### 10 - Доля директоров в двух и более СД ######
graph_data9 <- outlier_data_DD %>% group_by(Number_Year) %>% summarise(Double_Directors = mean(Double_Directors), 
                                                                       Other_Directors = 1-Double_Directors) %>% as.data.frame() %>% pivot_longer(cols = c("Double_Directors":"Other_Directors"), names_to = "Directors", values_to = "Values")
ggplot(data = graph_data9, aes(x = as.character(Number_Year), y = Values, fill = Directors)) + geom_col() + xlab("Год") + scale_y_continuous(labels = scales::percent) + ylab("") + scale_fill_manual(values = c("#C1DFF9", "#748CDB")) + theme_classic() + theme(legend.position = "bottom")
summary(outlier_data_DD$Double_Directors)
summary(outlier_data_DD)

###### 11 - Разница между дивидендами и логарифмированным дивидендами ######
names(outlier_data1)
m1 <- ggplot(data = outlier_data1, aes(x = DPS)) + geom_density(fill = "slateblue") + theme_classic() + ylab("Плотность распределения") + xlab("Дивиденд на одну акцию")
m2 <- ggplot(data = outlier_data1, aes(x = log_DPS)) + geom_density(fill = "slateblue") + theme_classic() + ylab("Плотность распределения") + xlab("Логарифмированный дивиденд на одну акцию")
m3 <- ggplot(data = outlier_data1, aes(x = DY)) + geom_density(fill = "slateblue") + theme_classic() + ylab("Плотность распределения") + xlab("Дивидендная доходность")

grid.arrange(m1, m2, m3, ncol = 3)

###### 12 - Есть ли отличия дивидендной доходности и дивиденда на одну акцию по годам? ######
#Дивидендная доходность
str(data_DY)
ggbetweenstats(
  data = outlier_data1, 
  x     = Industry,
  y     = DY,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Отрасль", 
  ylab = "Дивидендная доходность", 
  p.adjust.method = "none",
  bf.message = FALSE) + scale_color_viridis_d()

#Дивиденд на одну акцию 
ggbetweenstats(
  data = outlier_data1, 
  x     = Industry,
  y     = log_DPS,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Отрасль", 
  ylab = "Логарифмированный дивиденд", 
  p.adjust.method = "none",
  bf.message = FALSE) 

###### 13 - Плательщики/неплательщики дивидендов ######
graph13 <- outlier_data1
graph13$criteria <- ifelse(graph13$log_DPS > 0, "Плательщики", "Неплательщики")

m6 <- ggplot(data = graph13, aes(x = as.character(Number_Year), fill = criteria)) + geom_bar() + theme_classic() + scale_fill_manual(values = c("slateblue", "lightblue")) + ylab("Количество") + xlab("Год") + geom_label(aes(label = ..count..), stat='count', position = position_stack(vjust = 0.5)) + 
  guides(fill=guide_legend(title="Факт выплаты дивидендов")) + theme(legend.position = "bottom")
m6

graph13_1 <- graph13 %>% group_by(Number_Year, criteria) %>% summarise(count = n())
graph13_2 <- graph13 %>% group_by(Number_Year) %>% summarise(count1 = n())
graph13_3 <- merge(graph13_1, graph13_2)
graph13_3$count_perc <- graph13_3$count/graph13_3$count1
m7 <- ggplot(data = graph13_3, aes(x = as.character(Number_Year), y = count_perc, fill = criteria)) + geom_col() + theme_classic() + scale_fill_manual(values = c("slateblue", "lightblue")) + ylab("Количество") + xlab("Год") +  geom_label(aes(label = round(count_perc,2)), position = position_stack(vjust = 0.5)) + 
  guides(fill=guide_legend(title="Факт выплаты дивидендов")) + theme(legend.position = "bottom") 
m7
grid.arrange(m6, m7, ncol = 2)

table(graph13$Number_Year, graph13$criteria)

###### 14 - Размер совета директоров и их плотность ######
ggplot(data = outlier_data1, aes(x = Board_size)) + geom_density(fill = "darkblue") + facet_grid(~as.character(Number_Year)) + ylab("Плотность распределения") + xlab("Размер совета директоров") + theme_classic() +
  theme(strip.background = element_rect(fill = "royalblue4"), strip.text = element_text(colour = 'white'))
ggplot(data = outlier_data1, aes(x = as.character(Number_Year), y = Board_size)) + geom_boxplot()

ggplot(data = outlier_data1, aes(x = Board_size)) + geom_density(aes(fill = Industry)) + facet_wrap(~ Industry) + gghighlight::gghighlight() + theme_minimal() + scale_fill_viridis_d(option = "F") + theme(legend.position = "none") + xlab("Размер совета директоров") + ylab("Плотность распределения")

###### 15 - Динамика среднего дивидендного параметра по годам и отрасли ######
gr15 <- outlier_data1 %>% group_by(Number_Year, Industry) %>% summarise(log_DPS = mean(log_DPS), DY = mean(DY))
ggplot(data = gr15, aes(x = as.character(Number_Year), y = DY, fill = Industry)) + geom_col() + facet_grid(~Industry) + scale_fill_viridis_d()

###### 16 - Доля женщин в СД ######
names(outlier_data1)
gr16 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(Gender_diversity = mean(Gender_diversity)) %>% as.data.frame()
gr16$Other_members <- 1-gr16$Gender_diversity
colnames(gr16) <- c("Number_Year", "Woman", "Man")
gr16_1 <- gr16 %>% group_by(Number_Year) %>% pivot_longer(cols = "Woman":"Man", names_to = "Criteria", values_to = "Values") %>% as.data.frame()

m9 <- ggplot(data = gr16_1, aes(x = Number_Year, y = Values, fill = Criteria)) + geom_col() + xlab("Год") + ylab("Доля") + guides(fill=guide_legend(title="Структура совета")) + 
  theme_classic() + scale_fill_viridis_d(labels=c('Мужчины', 'Женщины'), option = "H") + geom_text(aes(label = round(Values,2)*100), color = "white") + scale_y_continuous(labels = scales::percent) +  theme(legend.position = "none")

gr16_2 <- outlier_data1 %>% group_by(Industry) %>% summarise(Gender_diversity = mean(Gender_diversity)) %>% as.data.frame()
gr16_2$Other_members <- 1-gr16_2$Gender_diversity
colnames(gr16_2) <- c("Industry", "Woman", "Man")
gr16_3 <- gr16_2 %>% group_by(Industry) %>% pivot_longer(cols = "Woman":"Man", names_to = "Criteria", values_to = "Values") %>% as.data.frame()

m10 <- ggplot(gr16_3, aes(x = "", y = Values, fill = Criteria)) + geom_col(position = "fill") + coord_polar("y", start = 80) + 
  facet_wrap(~Industry) + scale_fill_manual(values = c("#192D45", "#163B88", "#748CDB"))+ 
  xlab("") + ylab("") + geom_text(aes(label = round(Values,2)*100), 
                                  position = position_fill(vjust = 0.5), color = "white") + 
  theme_classic() + theme(axis.text.x = element_blank(), strip.text = element_text(size = 8)) + scale_fill_viridis_d(labels=c('Мужчины', 'Женщины'), option = "H") + guides(fill=guide_legend(title="Структура совета"))

grid.arrange(m9, m10, ncol = 2)

###### 17 - Независимые и иностранные директора ######
names(outlier_data1)
gr17 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(Foreign = mean(Share_Foreign_Directors), 
                                                              Independent = mean(share_ID)) %>% as.data.frame()
gr17_1 <- gr17 %>% group_by(Number_Year) %>% pivot_longer(cols = "Foreign":"Independent", names_to = "Directors", values_to = "Values") %>% as.data.frame()

ggplot(data = gr17_1, aes(x = as.character(Number_Year), y = Values, group = Directors, color = Directors)) + geom_line()+ scale_y_continuous(labels = scales::percent) + geom_text(aes(label = round(Values,2)*100), hjust=-.5) + xlab("Год") + scale_color_manual(labels = c("Иностранные", "Независимые"), values = c("#604d9e", "#a27eb8")) + 
  ylab("Доля") +  theme_classic() + theme(legend.position = "top")

cor(outlier_data1$share_ID, outlier_data1$Share_Foreign_Directors)

###### 18 - Постоянство совета директоров ######   
gr18 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(Turnover_ratio = mean(Turnover_ratio), 
                                                              Turnover_lag_ratio = mean(Turnover_lag_ratio)) %>% as.data.frame()
gr18_1 <- gr18 %>% group_by(Number_Year) %>% pivot_longer(cols = "Turnover_ratio":"Turnover_lag_ratio", names_to = "Turnover", values_to = "Values") %>% as.data.frame()

ggplot(data = gr18_1, aes(x = as.character(Number_Year), y = Values, group = Turnover, color = Turnover)) + geom_line()+ scale_y_continuous(labels = scales::percent) + geom_text(aes(label = round(Values,2)*100), hjust=-.5) + xlab("Год") + scale_color_manual(labels = c("Постоянство относительно года t-1", "Постоянство относительно года t"), values = c("#0BA5BE", "#013F4E")) + 
  ylab("Доля") +  theme_classic() + theme(legend.position = "top")

paletter_vector <-
  paletteer::paletteer_d(
    palette = "palettetown::venusaur",
    n = nlevels(as.factor(gr18_1$Number_Year)),
    type = "discrete"
  )

ggdotplotstats(
  data       = gr18_1 %>% dplyr::filter(Turnover == "Turnover_ratio"),
  x          = Values,
  y          = Number_Year,
  title      = "Среднее значение постоянства совета директоров и распределение по годам",
  xlab       = "Среднее значение постоянства совета директоров", 
  point.args = list(shape = 16, size = 5, color = paletter_vector)) + theme_classic()

###### 19 - Два директора ######
max(outlier_data_DD$Double_Directors)
gr19 <- outlier_data_DD %>% group_by(Number_Year) %>% summarise(Double = mean(Double_Directors), 
                                                                Other = 1-Double)
gr19_1 <- gr19 %>% pivot_longer(cols = "Double":"Other", names_to = "Directors", values_to = "Values")
gr19_1$Directors <- factor(gr19_1$Directors, levels = c("Other", "Double"))
m11 <- ggplot(data = gr19_1, aes(x = as.character(Number_Year), y = Values, fill = Directors)) + geom_col() + scale_fill_viridis_d(labels = c("Не совмещают", "Совмещают")) + theme_classic() + xlab("Год") + ylab("Доля") + scale_y_continuous(labels = scales::percent) + guides(fill=guide_legend(title="Директора")) + theme(legend.position = "none") + 
  geom_text(aes(label = round(Values,2)*100, color = Directors), position = position_fill(vjust = 0.5)) + scale_color_manual(values = c("white", "black"))

gr19_2 <- outlier_data1 %>% group_by(Industry) %>% summarise(Double = mean(Double_Directors)) %>% as.data.frame()
gr19_2$Other_members <- 1-gr19_2$Double
colnames(gr19_2) <- c("Industry", "Double", "Others")
gr19_3 <- gr19_2 %>% group_by(Industry) %>% pivot_longer(cols = "Double":"Others", names_to = "Criteria", values_to = "Values") %>% as.data.frame()
gr19_3$Criteria <- factor(gr19_3$Criteria, levels = c("Others", "Double"))

m12 <- ggplot(gr19_3, aes(x = "", y = Values, fill = Criteria)) + geom_col(position = "fill") + coord_polar("y", start = 80) + 
  facet_wrap(~Industry) + scale_fill_manual(values = c("#192D45", "#163B88", "#748CDB"))+ 
  xlab("") + ylab("") + geom_text(aes(label = round(Values,2)*100, color = Criteria), 
                                  position = position_fill(vjust = 0.5)) + 
  theme_classic() + theme(axis.text.x = element_blank(), strip.text = element_text(size = 8)) + scale_fill_viridis_d(labels=c('Не совмещают', 'Совмещают')) + guides(fill=guide_legend(title="Директора")) + scale_color_manual(values = c("white", "black"))

grid.arrange(m11, m12, ncol = 2)

###### 20 - Анализ гражданства ######
data_Country <- read_excel("~/data CG.xlsx", sheet = "Data_Board") %>% as.data.frame()
ggplot(data = data_Country, aes(x = as.character(Number_Year), fill = Nation)) + geom_bar() 
data_Country$Criteria <- ifelse(data_Country$Nation == "Россия", "Российские", ifelse(data_Country$Nation == "не РФ", "Не РФ", "Иностранные"))
ggplot(data = data_Country, aes(x = as.character(Number_Year), fill = Nation)) + geom_bar() + facet_wrap(~Criteria, scale = "free_y") + scale_fill_viridis_d() + theme_classic() + guides(fill=guide_legend(title="Гражданство")) + ylab("Количество") + xlab("Год") + theme(axis.text.x = element_text(size = 7))

as.data.frame(table(data_Country$Nation)) %>% arrange(desc(Freq))
as.data.frame(table(data_Country$Nation, data_Country$Number_Year)) %>% arrange(desc(Freq)) %>% dplyr::filter(!(Var1 %in% c("Россия", "не РФ")))

###### 21 - Средний опыт и возраст ######
names(outlier_data1)
gr21 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(Average_age = mean(Average_age)) %>% as.data.frame()
gr21_1 <- outlier_data1 %>% group_by(Number_Year, Industry) %>% summarise(Average_age = mean(Average_age)) %>% as.data.frame()

m13 <- ggplot() + theme_classic() + ylab("Средний возраст") + xlab("Год") + 
  geom_point(data = gr21, mapping = aes(x = as.character(Number_Year), y = Average_age), color="red") +
  geom_line(data = gr21, mapping = aes(x = as.character(Number_Year), y = Average_age, group=1), color = "red", linetype = "dashed", size = 1) + 
  geom_point(data = gr21_1, mapping = aes(x = as.character(Number_Year), y = Average_age, color = Industry)) +
  geom_line(data = gr21_1, mapping = aes(x = as.character(Number_Year), y = Average_age, color = Industry, group=Industry)) + scale_color_viridis_d(option = "H") + theme(legend.position = "bottom")
m13
gr21_2 <- outlier_data1 %>% group_by(Number_Year) %>% summarise(Average_WE = mean(Average_WE)) %>% as.data.frame()
gr21_3 <- outlier_data1 %>% group_by(Number_Year, Industry) %>% summarise(Average_WE = mean(Average_WE)) %>% as.data.frame()

m14 <- ggplot() + theme_classic() + ylab("Средний стаж работы в Совете директоров") + xlab("Год") + 
  geom_point(data = gr21_2, mapping = aes(x = as.character(Number_Year), y = Average_WE), color="red") +
  geom_line(data = gr21_2, mapping = aes(x = as.character(Number_Year), y = Average_WE, group=1), color = "red", linetype = "dashed", size = 1) + 
  geom_point(data = gr21_3, mapping = aes(x = as.character(Number_Year), y = Average_WE, color = Industry)) +
  geom_line(data = gr21_3, mapping = aes(x = as.character(Number_Year), y = Average_WE, color = Industry, group=Industry)) + scale_color_viridis_d(option = "H") + theme(legend.position = "bottom")

grid.arrange(m13, m14, ncol = 2)

min(outlier_data1$Average_age)
max(outlier_data1$Average_age)
min(outlier_data1$Average_WE)

###### 22 - Корреляционные связи по переменным интереса ######
names(outlier_data1)
cor(outlier_data1[,c(10,11,12,13,14,15,16,18)])

labels_y <- c("Гендерное разнообразие", "Количество заседаний СД", "Средний возраст членов СД","Средний стаж пребывания в СД",  
            "Независимые директора", "Иностранные директора", "Постоянство СД")
labels_x <- c("Размер СД", "Гендерное разнообразие", "Количество заседаний СД", "Средний возраст членов СД","Средний стаж пребывания в СД",  
              "Независимые директора", "Иностранные директора")
labels_x1 <- c("Размер СД", "Гендерное разнообразие", "Количество заседаний СД", "Средний возраст членов СД","Средний стаж пребывания в СД",  
              "Независимые директора", "Иностранные директора", "Постоянство СД")
ggcorrmat(data = outlier_data1[,c(10,11,12,13,14,15,16,18)], 
          colors = c("#FF6933", "white", "#2D4628")) +  scale_y_discrete(label = labels_y) + scale_x_discrete(label = labels_x)

names(outlier_data1[,c(10,11,12,13,14,15,16,18)])
d1 <- outlier_data1[,c(10,11,12,13,14,15,16,18)]
colnames(d1) <- c("Board_size", "Gender_diversity", "Board_meetings", "Average_age", "Average_tenure", 
                  "Share_Independent", "Share_Foreign", "Turnover_ratio")

corr_cross(d1, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
) + theme_classic() + ggtitle("Попарные корреляции", subtitle = "Выделены 10 наиболее существенных корреляций") 

names(outlier_data1)
d2 <- outlier_data1[,c(3,5,6,7,8,17,10,11,12,13,14,15,16,18,22,25)]
names(d2)
colnames(d2) <- c("ROA", "Stage_Indicator", "CapexToSales", "log_Assets", "DebtToAssets", "Gov_Part", "Board_size", 
                  "Gender_diversity", "Board_meetings", "Average_age", "Average_tenure", "Share_Independent", "Share_Foreign", 
                  "Turnover_ratio", "DY", "log_DPS")

corr_cross(d2, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
) + theme_classic() + ggtitle("Попарные корреляции", subtitle = "Выделены 10 наиболее существенных корреляций") 

#p-value для корреляции
ggscatterstats(
  data  = d2,
  x     = Share_Foreign,
  y     = Share_Independent,
  ylab  = "Дивидендная доходность",
  xlab  = "Логарифмированный дивиденд на одну акцию",
  title = "Диаграмма рассеивания 
дивидендной доходности и дивиденда на одну акцию", 
  xfill = "#018abd", 
  yfill = "#97cbdc", 
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x), 
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE),
  xsidehistogram.args = list(fill = "slateblue", color = "black", na.rm = TRUE),
  ysidehistogram.args = list(fill = "lightblue2", color = "black", na.rm = TRUE)
)

ggscatterstats(data  = d2, x = Average_age, y = Average_tenure)
ggscatterstats(data  = d2, x = Board_size, y = Board_meetings)
ggscatterstats(data  = d2, x = Turnover_ratio, y = Average_tenure)
ggscatterstats(data  = d2, x = Board_size, y = Stage_Indicator)
ggscatterstats(data  = d2, x = DebtToAssets, y = Average_tenure)

#контрольные переменные и переменные интереса
names(outlier_data1)
corr_data <- cor(outlier_data1[,c(10,11,12,13,14,15,16,18, 3, 5, 6, 7, 8, 17)]) %>% as.data.frame()
names(corr_data)
corr_data1 <- corr_data[c(1,2,3,4,5,6,7,8),c(9,10,11,12,13,14)]
corr_data1$CG <- rownames(corr_data1)
corr_data2 <- corr_data1 %>% group_by(CG) %>% pivot_longer(cols = "ROA":"Gov_Part", names_to = "Control", values_to = "Correlation")
corr_data2$Criteria <- ifelse(corr_data2$Correlation > 0, "Positive", "Negative")

labels_x1 <- c("Гендерное разнообразие", "Иностранные директора", "Независимые директора",  "Постоянство СД", 
               "Размер СД", "Средний стаж пребывания в СД",   "Средний возраст членов СД","Количество заседаний СД")

names <- c("CAPEX к Выручке", "Долг к активам", "Доля гос. участия", "Логарифм активов", "Рентабельность активов", "Стадия жизненного цикла")
corr_data2$Control_RUS <- rep(c("Рентабельность активов", "Стадия жизненного цикла", "CAPEX к Выручке", "Логарифм активов", "Долг к активам", "Доля гос. участия"), 8)
ggplot(data = corr_data2, aes(x = reorder(CG, Correlation), y = Correlation, fill = Criteria)) + geom_col(alpha = 0.5) + facet_wrap(~Control_RUS) + theme_minimal() + geom_label(aes(label = round(Correlation, 2)*100), color = "white") + scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("darkred", "darkgreen")) + theme(axis.text.x = element_text(size = 8, angle = 45,  hjust=1), legend.position = "none") + scale_x_discrete(labels = labels_x1) + ylab("Корреляция") + xlab("") 

corr_data3 <- outlier_data1[,c(25,22,3,5,6,7,8,17,10,11,26,13,14,15,16,18)]
names(corr_data3)
colnames(corr_data3) <- c("log_DPS", "DY", "ROA", "Stage_Indicator", "CapexToSales", "log_Assets", 
                          "DebtToAssets", "Gov_Part", "Board_size", "Gender_diversity", "log_Board_meetings", "Average_age", 
                          "Average_tenure", "Share_Independent", "Share_Foreign", "Turnover_ratio")
ggcorrmat(data = corr_data3, 
          colors = c("#FF6933", "white", "#2D4628")) 

##### --------------Сборная солянка ------------------ #####

names(outlier_data_HHI1)
t.test(outlier_data_HHI1 %>% dplyr::filter(Number_Year == "2014") %>% dplyr::select(HHI_level0_adj), 
       outlier_data_HHI1 %>% dplyr::filter(Number_Year == "2016") %>% dplyr::select(HHI_level0_adj))

t.test(outlier_data_HHI1 %>% dplyr::filter(Industry == "Energy") %>% dplyr::select(HHI_level0_adj), 
       outlier_data_HHI1 %>% dplyr::filter(Industry == "Metals & Mining") %>% dplyr::select(HHI_level0_adj))

t.test(outlier_data_HHI1 %>% dplyr::filter(Industry == "Energy") %>% dplyr::select(HHI_level0_adj), 
       outlier_data_HHI1 %>% dplyr::filter(Industry == "Telecoms") %>% dplyr::select(HHI_level0_adj))

dim(outlier_data_HHI1)
write.xlsx(outlier_data_HHI1, "HHI_data.xlsx")
write.xlsx(outlier_data_DD, "Double_Director_data.xlsx")
dim(outlier_data_DD)

###### 23 - HHI по отраслям и по годам ######
dim(outlier_data_HHI1)

graph_1 <- outlier_data_HHI1
graph_1$Number_Year <- as.integer(graph_1$Number_Year)
# Make a ggplot, but add frame=year: one image per year
names(outlier_data_HHI1)
ggplot(graph_1, aes(DY, HHI_level0_adj, size = log_Assets, color = Industry)) +
  geom_point() +
  theme_classic() + 
  facet_wrap(~Industry) + 
  labs(title = 'Year: {frame_time}', x = 'Дивидендная доходность', y = 'Индекс Герфиндаля-Хиршмана 
(степень следования рекомендациям ККУ от Банка России)') +   
  transition_time(Number_Year) + 
  ease_aes('linear') + scale_color_viridis_d() + theme(axis.title.x = element_text(size = 13), 
                                                       axis.title.y = element_text(size = 13)) + geom_label(aes(label = Ticker))

###### 24 - Кластеры и отрасли ######
head(mod2_clust$data.clust)
dim(mod2_clust$data.clust)
data_m <- merge(outlier_data1, mod2_clust$data.clust)
dim(data_m)
library(vcd)
mosaic(~Industry + clust, shade = TRUE, data = data_m) 

###### 25 - CEO в СД ######

df1 <- table(data_CEO$CEO_in_Board, data_CEO$Industry) %>% as.data.frame()
df2 <- table(data_CEO$CEO_in_Board, data_CEO$Industry) %>% as.data.frame() %>% group_by(Var2) %>% summarise(n = sum(Freq))
df3 <- merge(df1, df2)
df3$Count <- df3$Freq/df3$n
df4 <- df3 %>% dplyr::select(-Freq,-n) %>% pivot_wider(names_from = "Var2", values_from = "Count")

g8 <- ggradar(df4, axis.label.size = 4) + scale_color_manual(values = c("darkred", "darkblue"))
g8
?ggradar
df5 <- df3 %>% dplyr::select(Var1, Var2, Count)

g9 <- ggplot(data=df5,aes(Var2,Count,fill=Var1))+
  geom_bar(stat="identity", position=position_stack())+
  coord_polar(theta = "x",start=0) + scale_fill_manual(values = c("lightblue", "darkblue")) + theme_classic() + ylab("") + xlab("") + theme(legend.position = "none")

grid.arrange(g8, g9, ncol = 2)
#запомнить график g8

###### 26 - CEO в СД по годам  ######
library(ggpubr)

df6 <- table(data_CEO$CEO_in_Board, data_CEO$Number_Year) %>% as.data.frame()
df7 <- table(data_CEO$CEO_in_Board, data_CEO$Number_Year) %>% as.data.frame() %>% group_by(Var2) %>% summarise(n = sum(Freq))
df8 <- merge(df6, df7)
df8$Count <- df8$Freq/df8$n
df9 <- df8 %>% dplyr::select(-Freq,-n) %>% pivot_wider(names_from = "Var2", values_from = "Count")
df10 <- df8 %>% dplyr::select(Var1, Var2, Count)
colnames(df10) <- c("CEO_in_Board", "Number_Year", "Count")

View(df10)
View(df4)

g10 <- ggdotchart(
  df10, x = "Number_Year", y = "Count", 
  group = "CEO_in_Board", color = "CEO_in_Board",
  add = "segment", position = position_dodge(0.3), sorting = "none") + geom_label(aes(label = round(Count,2)*100, color = CEO_in_Board)) + xlab("Год") + 
  scale_y_continuous(labels = scales::percent) + scale_color_manual(values = c("darkred", "darkblue")) + ylab("")

grid.arrange(g10, g8, ncol= 2)

###### 27 - CEO в СД по отраслям - stacked donut  ###### 
library(webr)
d1 <- as.data.frame(table(data_CEO$Industry, data_CEO$CEO_in_Board))
colnames(d1) <- c("Industry", "CEO_in_Board", "Freq")

library(ggrepel)
colors= c("#2DA86D", "#A72D98", "#423FA9", "#4BC88B", "#5FCE98", "#73D4A5", "#87DAB2", "#9BE0BF", "#AFE6CB", "#C3ECD8", "#D7F2E5",
          "#C74BB7", "#CD5FBF", "#D373C7", "#D987CF", "#E09BD7", "#E6AFDF", "#ECC3E7", "#F2D7EF", "#5F5DC8", "#716FCE", "#8381D4", "#9493DA", "#A6A5E0", "#B8B7E6", "#C9C9EC", "#DBDBF2")
PieDonut(d1, aes(Industry, CEO_in_Board, count=Freq), r0=0.2,r1=0.9,r2=1.3,explode=1,start=pi/2,labelposition=0, color = "grey", use.label = TRUE)