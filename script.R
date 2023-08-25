# Import the data 
library(readr) 
ward_profiles_2 <- read_csv("ward_profiles_2.csv") 
View(ward_profiles_2)
head(ward_profiles_2)


#PTAL map
library(readr) 
PTAL_2011<- read_csv("PTAL.csv") 
View(PTAL_2011)
missing_values <- is.na(PTAL_2011)
PTAL_2011 <- PTAL_2011[, c(2, 3, 52)]
missing_value <- is.na(PTAL_2011)

library(sf)
ward_shp<- st_read("Ward-shp-file/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp")
# 查看对象的结构
str(ward_shp)
# 查看前几行属性数据
head(ward_shp)

#更改列名称-为了限免的合并
colnames(PTAL_2011)[colnames(PTAL_2011) == "New ward code"] <- "GSS_CODE"
colnames(PTAL_2011)[colnames(PTAL_2011) == "Public Transport Accessibility - 2011"] <- "PTA_2011"
View(PTAL_2011)

#合并csv和shp
library(dplyr)
PTA_map_2011 <- left_join(ward_shp, PTAL_2011, by = "GSS_CODE")

#制作伦敦wardlevel的pta的图形
install.packages("RColorBrewer")
library(RColorBrewer)
library(tidyverse)

PTA_theme_map <- ggplot() +
  geom_sf(data = PTA_map_2011,color = "White",
          aes(fill = PTA_2011),size = 0.3) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray") +
  labs(title = "Public Transport Accessibility(PTA)", fill = "Public Transport Accessibility Value") +
  guides(fill = guide_legend(title = "PTA index Value",title.position = "bottom"),
         position = "bottom", 
         ncol = 1)  # 控制图例列

print(PTA_theme_map)


# 查看数据结构和类型
str(PTAL_2011)

# 假设你的数据集是一个数据框，其中的数值型列名为 "NumericColumn"
# 如果你的数据集是一个向量，直接使用 hist() 即可，无需指定列名

# 绘制直方图
hist(PTAL_2011$PTA_2011, 
     xlab = "PTA index value",
     ylab = "Frequency",
     col = "lightblue",
     border = "white")

# employment rate map
employment_rate <- ward_profiles_2[, c(1,3, 27)]
missing_value_2 <- is.na(employment_rate)
colnames(employment_rate)[colnames(employment_rate) == "New code"] <- "GSS_CODE"
colnames(employment_rate)[colnames(employment_rate) == "Employment rate (16-64) - 2011"] <- "Employement_rate"
View(employment_rate)

#merge the data
library(sf)
ward_shp<- st_read("Ward-shp-file/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp")
str(ward_shp)
head(ward_shp)

library(dplyr)
employment_rate_map <- left_join(ward_shp, employment_rate, by = "GSS_CODE")
view(employment_rate_map)

#map_it
library(RColorBrewer)
library(tidyverse)
custom_colors <- c("blue", "#d7b0d3", "red")

em_theme_map <- ggplot() +
  geom_sf(data = employment_rate_map,color = "White",
          aes(fill = Employement_rate),size = 0.3) + 
  scale_fill_gradient(low = "#ffffff", high = "#c67373", na.value = "gray") +
  labs(title = "Employment rate (16-64) - 2011", fill = "Employment rate(%)") +
  guides(fill = guide_legend(title = "Employment rate(%)",title.position = "bottom"),
         position = "bottom", 
         ncol = 1)

print(em_theme_map)

hist(employment_rate_map$Employement_rate, 
     xlab = "Employement_rate",
     ylab = "Frequency",
     col = "#c67373",
     border = "white")

#选择数据
View(ward_profiles_2)
Part_1 <- ward_profiles_2[, c(1,3,27,56,57,64,66)]
colnames(Part_1)[colnames(Part_1) == "New code"] <- "GSS_CODE"

#PTAI数据
library(readr) 
Part_2<- read_csv("PTAL.csv") 
View(Part_2)
Part_2 <- Part_2[, c(2, 3, 52)]
View(Part_2)
colnames(Part_2)[colnames(Part_2) == "New ward code"] <- "GSS_CODE"

#population density 数据
library(readr)
X1 <- read_csv("1.csv")
Part_3 <- X1[, c(3,5,12)]
View(Part_3)
colnames(Part_3)[colnames(Part_3) == "geography code"] <- "GSS_CODE"
colnames(Part_3)[colnames(Part_3) == "Variable: All usual residents; measures: Value"] <- "Population"
colnames(Part_3)[colnames(Part_3) == "Variable: Density (number of persons per hectare); measures: Value"] <- "Population_density"
View(Part_3)

library(dplyr)
Part_23 <- left_join(Part_2, Part_3, by = "GSS_CODE")
View(Part_23)

#种族数据
library(readr)
ethic_ward <- read_csv("ethic-ward.csv")
Part_4 <- ethic_ward[, c(3,7,18,25,30)]
View(Part_4)
colnames(Part_4)[colnames(Part_4) == "geography code"] <- "GSS_CODE"
colnames(Part_4)[colnames(Part_4) == "portion of White"] <- "Proportion_of_W"
colnames(Part_4)[colnames(Part_4) == "portion of Asian/Asian British"] <- "Proportion_of_A"
colnames(Part_4)[colnames(Part_4) == "portion_of_Black/African/Caribbean/Black British"] <- "Proportion_of_B"
colnames(Part_4)[colnames(Part_4) == "portion_of_Other ethnic group"] <- "Proportion_of_O"

library(dplyr)
Part_4 <- Part_4 %>%
  mutate(Proportion_of_A = round(Proportion_of_A, 2))
Part_4 <- Part_4 %>%
  mutate(Proportion_of_W = round(Proportion_of_W, 2))
Part_4 <- Part_4 %>%
  mutate(Proportion_of_B = round(Proportion_of_B, 2))
Part_4 <- Part_4 %>%
  mutate(Proportion_of_O = round(Proportion_of_O, 2))

Part_14 <- left_join(Part_1, Part_4, by = "GSS_CODE")
View(Part_14)

Final <- left_join(Part_14, Part_23, by = "GSS_CODE")
View(Final)

num_na <- sum(is.na(Final))  # 返回缺失值的数量
Final <- na.omit(Final)  # 删除包含缺失值的行
num_na_1 <- sum(is.na(Final))

class(Final)
column_data_types <- sapply(Final, class)
print(column_data_types)
View(Final)

Final_1 <- Final[, c(3:11, 13:15)]
View(Final_1)
colnames(Final_1)[colnames(Final_1) == "Employment rate (16-64) - 2011"] <- "Employment_rate"
colnames(Final_1)[colnames(Final_1) == "Public Transport Accessibility - 2011"] <- "PTAI"
colnames(Final_1)[colnames(Final_1) == "% travel by bicycle to work - 2011"] <- "Bicycle_to _work"
colnames(Final_1)[colnames(Final_1) == "Cars per household - 2011"] <- "Cars_per_hs"
colnames(Final_1)[colnames(Final_1) == "% with no qualifications - 2011"] <- "no_qualifications"
colnames(Final_1)[colnames(Final_1) == "% with Level 4 qualifications and above - 2011"] <- "Level_4_qualifications"

colnames(Final_1)[colnames(Final_1) == "Bicycle_to _work"] <- "Bicycle_to_work"

class(Final_1)
column_data_types <- sapply(Final_1, class)
print(column_data_types)
Final_1 <- Final_1[, -6]  # 删除第6列

cor(Final_1)
cor_matrix <- cor(Final_1)

# 使用基础绘图函数 heatmap()
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Heatmap"
        )

install.packages("ggplot2")
library(ggplot2)

# 将相关系数矩阵转换为长格式的数据框以供 ggplot2 使用
install.packages("reshape2")
library(reshape2)
cor_matrix_long <- melt(cor_matrix)

# 使用 ggplot2 绘制热力图
ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "pink", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 假设 Y 是因变量，X1, X2, ..., Xn 是自变量
model <- lm(Employment_rate ~ PTAI + no_qualifications + Level_4_qualifications + Cars_per_hs + Bicycle_to_work + Proportion_of_A + Proportion_of_B + Proportion_of_O + Population + Population_density, data = Final_1)
summary(model)

library(car)
qqPlot(model,id.method='identify',simulate = TRUE,labels=row.names(Final_1),main='Q-Q plot')

durbinWatsonTest(model)
crPlots(model)
ncvTest(model)

install.packages("psych")
library(psych)
describe <- describe(Final_1)
describe(Final_1)

#log 一下
log_column <- log(Final_1$Employment_rate)
Final_1$log_Employment_rate <- log(Final_1$Employment_rate)

model_log<- lm(log_Employment_rate ~ PTAI + no_qualifications + Level_4_qualifications + Cars_per_hs + Bicycle_to_work + Proportion_of_W + Proportion_of_A + Proportion_of_B + Proportion_of_O + Population + Population_density, data = Final_1)
summary(model_log)

library(car)
qqPlot(model,id.method='identify',simulate = TRUE,labels=row.names(Final_1),main='Q-Q plot')

durbinWatsonTest(model_log)

ncvTest(model_log)


vif_results <- vif(model_log)
cond_indices <- vif_results$cond

# 安装和加载nlme包
install.packages("nlme")
library(nlme)

# 创建示例数据集
data <- Final_1  # 假设这是你的数据集
weights <- data$weights  # 假设weights是一个包含权重的列

# 进行加权最小二乘法回归
model_WLS <- gls(Employment_rate ~ PTAI + no_qualifications + Level_4_qualifications + Cars_per_hs + Bicycle_to_work + Proportion_of_A + Proportion_of_B + Proportion_of_O + Population + Population_density, data = data, weights = weights)

# 查看回归分析结果
summary(model_WLS)

# 创建示例数据集
data <- Final_1  # 假设这是你的数据集
weights <- data$weights  # 假设weights是一个包含权重的列

# 进行加权最小二乘法回归
model_WLS_log<- gls(log_Employment_rate ~ PTAI + no_qualifications + Cars_per_hs + Bicycle_to_work + Proportion_of_A + Proportion_of_B + Proportion_of_O + Population + Population_density, data = data, weights = weights)

# 查看回归分析结果
summary(model_WLS_log)

#检验
library(car)
vif_values <- vif(model_WLS_log)
print(vif_values)


# 确保 residuals 是向量并且是数值型的
residuals <- as.vector(residuals(model_WLS_log))

durbinWatsonTestResult <- durbinWatsonTest(residuals)
print(durbinWatsonTestResult)


install.packages("AER")
library(AER)
# 使用ivreg函数进行IV回归分析
# formula: 回归方程，用 ~ 表示，包括因变量和解释变量
# data: 数据集
# subset: 可选参数，用于选择子集
# instruments: 仪器变量
iv_model <- ivreg(formula = log_Employment_rate ~ PTAI + no_qualifications + Level_4_qualifications + Cars_per_hs + Bicycle_to_work + Proportion_of_A + Proportion_of_B + Proportion_of_O + Population + Population_density | instruments,
                  data = data)

# 查看IV回归结果摘要
summary(iv_model)

library(psych)
describe <- describe(Final_1)
describe(Final_1)