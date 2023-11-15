library("dplyr")
library("ggplot2")
library("tidyr")

data2 <- read.csv("/Users/zhaoxiuping/R_zxp/data-assignment/LTPP/iri.csv")
attach(data2)

subset_data <- subset(data2, STATE_CODE == 6 & grepl("050", SHRP_ID))

subset_data2 <- data2 |>
  group_by(STATE_CODE,SHRP_ID) |>
  summarise(
    observation = n(),
    # 填补缺失值
    iri_max = max(IRI,na.rm = TRUE),
    iri_min = min(IRI,na.rm = TRUE),
    iri_median = median(IRI,na.rm = TRUE),
    iri_mean = mean(IRI,na.rm = TRUE)
  ) |>
ungroup()

subset_data3 <- arrange(subset_data2,desc(iri_mean))


# STATE_CODE=6 SHRP_ID 以050 开始的section 作图，包含图表
subset_data4 <- subset_data |>
  group_by(VISIT_DATE,STATE_CODE,SHRP_ID) |>
  summarise(
    iri_mean = mean(IRI,na.rm = TRUE)
  ) |>
  ungroup()
subset_data5 <- subset_data4 |>
  group_by(VISIT_DATE,STATE_CODE,SHRP_ID) |>
  separate(VISIT_DATE, c("VISIT_DATE", NULL), sep=",") |> mutate(
    VISIT_DATE = as.Date(VISIT_DATE, "%m/%d/%y"), 
    iri_mean = iri_mean
  )

data <- data.frame(
  VISIT_DATE = subset_data5$VISIT_DATE,
  IRI_MEAN = subset_data5$iri_mean,
  STATE_CODE = subset_data5$STATE_CODE,
  SHRP_ID = subset_data5$SHRP_ID
)
library("ggplot2")
scatter_plot <- ggplot(data, aes(x = VISIT_DATE, y = IRI_MEAN)) +
  geom_point(aes(color = paste(STATE_CODE, SHRP_ID)), size = 2) +
  labs(x = "Date", y = "Average IRI", title = "Scatter Plot of meanIRIvs.date") +
  labs(color = "section",size = 2)+
  theme() 
print(scatter_plot)


# #全部的section作图，无图标，图标太多无法显示完全图
# subset_data6 <- data2 |>
#   group_by(VISIT_DATE,STATE_CODE,SHRP_ID) |>
#   summarise(
#     iri_mean = mean(IRI,na.rm = TRUE)
#   ) |>
#   ungroup()
# subset_data7 <- subset_data6 |>
#   group_by(VISIT_DATE,STATE_CODE,SHRP_ID) |>
#   separate(VISIT_DATE, c("VISIT_DATE", NULL), sep=",") |> mutate(
#     VISIT_DATE = as.Date(VISIT_DATE, "%m/%d/%y"), 
#     iri_mean = iri_mean
#   )
# 
# data <- data.frame(
#   VISIT_DATE = subset_data7$VISIT_DATE,
#   IRI_MEAN = subset_data7$iri_mean,
#   STATE_CODE = subset_data7$STATE_CODE,
#   SHRP_ID = subset_data7$SHRP_ID
# )
# 
# scatter_plot <- ggplot(data, aes(x = VISIT_DATE, y = IRI_MEAN)) +
#   geom_point() +
#   labs(x = "Date", y = "Average IRI", title = "Scatter Plot of meanIRIvs.date") +
#   labs(color = "section",size = 2)+
#   theme() 
# print(scatter_plot)

accident <- data.frame(read.csv("/Users/zhaoxiuping/R_zxp/data-assignment/CRSS/ACCIDENT.csv"))
person <- data.frame(read.csv("/Users/zhaoxiuping/R_zxp/data-assignment/CRSS/PERSON.csv"))


#取交集
inter <- intersect(colnames(accident),colnames(person))

inter_ap <- inner_join(
  x=accident,
  y=person,
  by = inter
)

observations_injury_severity  <- inter_ap |>
  group_by(INJ_SEV) |>
  summarise(
    count = n()
  )

vehicle <- read.csv("/Users/zhaoxiuping/R_zxp/data-assignment/CRSS/VEHICLE.CSV")
inter_2 <- intersect(colnames(accident),colnames(vehicle))
left_av <- left_join(
  x=accident,
  y=vehicle,
  # by = c("CASENUM","PSU")
  by = inter_2
) |>
  distinct()
#右侧数据集的缺失值
missing_VEH_NO <- sum(is.na(left_av$VEH_NO))
print(missing_VEH_NO)
#合并后的数据集维度
dim(left_av)