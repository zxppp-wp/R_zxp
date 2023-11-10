library("dplyr")
library("ggplot2")
library("tidyr")

data2 <- read.csv("/Users/zhaoxiuping/My R/data-assignment/LTPP/iri.csv")
attach(data2)

subset_data <- subset(data2, STATE_CODE == 6 & grepl("050", SHRP_ID))
# filter(stringr::str_detect(SHRP_ID, "010")) |>

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


subset_data4 <- subset_data |>
  separate(VISIT_DATE, c("VISIT_DATE", NULL), sep=",") |>
  mutate(VISIT_DATE = parse_date(VISIT_DATE, "%m/%d/%y")) |> 
  filter(VISIT_DATE > as.Date("12/1/3", "%m/%d/%y") ) %>%
  arrange(VISIT_DATE)


p <- ggplot(data = subset_data4, aes(x = VISIT_DATE, y = IRI, color = paste(STATE_CODE, SHRP_ID, sep = "."))) +
  geom_point() +
  labs(title = "IRI vs. VISIT_DATE",
       x = "VISIT_DATE",
       y = "IRI") +
  theme_minimal()

# 显示散点图
print(p)

accident <- data.frame(read.csv("/Users/zhaoxiuping/My R/data-assignment/CRSS/ACCIDENT.csv"))
person <- data.frame(read.csv("/Users/zhaoxiuping/My R/data-assignment/CRSS/PERSON.csv"))

# accident_2017 <- accident |> 
#   filter(YEAR==2017)
# person_2017 <- person |>
#   filter(MOD_YEAR==2017 )

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

vehicle <- read.csv("/Users/zhaoxiuping/My R/data-assignment/CRSS/VEHICLE.csv")
inter_2 <- intersect(colnames(accident),colnames(vehicle))
left_av <- left_join(
  x=accident,
  y=vehicle,
  # by = c("CASENUM","PSU")
  by = inter_2
) |>
  distinct()
missing_ALCOHOL <- sum(is.na(left_av$MAX_SEV.y))
print(missing_ALCOHOL)

dim(left_av)