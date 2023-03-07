library(readr)
library(dplyr)
NTU_Drug <- read_csv("NTU_Drug.csv")

#篩選美妝產品
filtered_drug <- NTU_Drug %>% filter(grepl('粧前乳|水凝露|蜜粉|唇釉|唇蜜|唇膏|隔離霜|防曬乳|粉餅|眼影盤|眼影筆|腮紅|修容筆|眉粉|化妝水|乳液|粉撲|抗老|凝露|凝凍|凝膠|打亮|BB霜|乳霜|面膜|遮瑕|抗痘|粉底|精華|口紅|護唇|保養|保濕|睫毛|眼妝|底妝|眼睫|眉筆|鼻影', name))

#篩選康是美
cosmed <- filtered_drug %>% filter(grepl("康是美",channel))

#忠實顧客
cosmed["Freq"] <- 1
loyal_customer <- cosmed %>% group_by(deviceid, invo_idx) %>%
  summarise() %>%
  group_by(deviceid) %>%
  summarise(count = n()) %>%
  filter(count > 1)

cosmed_F <- cosmed[cosmed$deviceid %in% loyal_customer$deviceid, ]

# Add frequency to cosmed
Freq <- replicate(nrow(cosmed), 1)
for (i in 1:nrow(loyal_customer)){
  Freq[which(cosmed[, "deviceid"] == loyal_customer$deviceid[i])] <- loyal_customer[i, "count"]
}

for (i in 1:nrow(cosmed)){
  cosmed[i, "Freq"] <- Freq[[i]]
}

# Add "whether F" to cosmed
Whether_F <- replicate(nrow(cosmed), 0)
for (i in 1:nrow(loyal_customer)){
  Whether_F[which(cosmed[, "deviceid"] == loyal_customer$deviceid[i])] <- 1
}

for (i in 1:nrow(cosmed)){
  cosmed[i, "Whether_F"] <- Whether_F[[i]]
}

#週幾
day <- as.data.frame(format(cosmed$datetime_UTC_8,format = "%w"))
names(day)[1] <- "day"
cosmed["day"] <- day

#早中晚
library(stringr)
subtime<-as.data.frame(cosmed$datetime_UTC_8)
pos1<-str_locate(subtime[[1]],"2021-")
time_table<-data.frame(time=str_sub(subtime[[1]],pos1[,2]+7,pos1[,2]+8))
time_table$time <- gsub("06|07|08|09|10|11", "morning", time_table$time) 
time_table$time <- gsub("12|13|14|15|16|17", "afternoon", time_table$time)
time_table$time <- gsub("18|19|20|21|22|23|00|01|02|03|04|05", "night", time_table$time)

cosmed["time"] <- time_table$time

# 品項分類
category <- cosmed$name
category[grepl("水凝露|化妝水|乳液|抗老|凝露|凝凍|凝膠|乳霜|面膜|抗痘|精華|保養|保濕",cosmed$name)]<-"maintain"
category[grepl("唇膏|唇釉|唇蜜|口紅|護唇|眉筆|睫毛|眼睫|眼影盤|眼影筆|眼妝|眉粉|修容筆|腮紅|打亮|腮紅|打亮|鼻影",cosmed$name)]<-"makeup"
category[grepl("粉餅|蜜粉|防曬乳|粧前乳|隔離霜|BB霜|底妝|遮瑕|粉底|粉撲",cosmed$name)]<-"base"
category<-as.data.frame(category)

cosmed["category"] <- category$category

# 折扣分類
discount <- replicate(nrow(cosmed), "一般商品")
discount[grepl("一般折扣",cosmed$name)]<-"一般折扣"
discount[grepl("買1送1",cosmed$name)]<-"買1送1"
discount[grepl("加碼-指定面膜", cosmed$name)]<-"加價購"
discount[grepl("開架臉部保養滿888現折100元",cosmed$name)]<-"滿888現折100元"
discount[grepl("開架身體保養滿388現折50元",cosmed$name)]<-"滿388現折50元"
discount[grepl("0224-0226面膜.醫美.彩妝.統藥82折", cosmed$name)]<-"期限內品項82折"
discount<-as.data.frame(discount)

cosmed["discount_type"] <- discount$discount


# 是否為折扣
for (i in 1:nrow(cosmed)){
  if (cosmed[i, "discount_type"] == "一般商品"){
    cosmed[i, "Whether_Discount"] = 0
  }
  else{
    cosmed[i, "Whether_Discount"] = 1
  }
}

# 計算cosmetic方面的invo_price
cos_invo_price <- cosmed %>% group_by(invo_idx) %>%
  summarise(cos_invo_price = sum(totprice))

for (i in 1:nrow(cos_invo_price)){
  cosmed[which(cosmed[, "invo_idx"] == cos_invo_price$invo_idx[i]), "cos_invo_price"] <- cos_invo_price[i, "cos_invo_price"]
}

#城市
sub<-as.data.frame(cosmed$county_district)
pos1<-str_locate(sub[[1]],"市|縣")
city <- data.frame(city=str_sub(sub[[1]],pos1[,1]-2,pos1[,1]))
cosmed["city"] <- city$city
cosmed <- cosmed %>% filter(grepl("臺北市|新北市|桃園市|臺中市|臺南市|高雄市",city))

#是否有折扣
discount_invo_idx <- subset(cosmed, Whether_Discount == 1)
for (i in 1:nrow(discount_invo_idx)){
  cosmed[cosmed$invo_idx == discount_invo_idx$invo_idx[i], "If_Discount"] <- 1
}
cosmed[is.na(cosmed$If_Discount), "If_Discount"] = 0

#單次購買量
tot_quant <- cosmed %>% group_by(invo_idx) %>%
  summarise(tot_quant = sum(quant))

for (i in 1:nrow(tot_quant)){
  cosmed[cosmed$invo_idx == tot_quant$invo_idx[i], "invo_quant"] <- tot_quant$tot_quant[i]
}

#刪outlier
Q <- quantile(cosmed$cos_invo_price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(cosmed$cos_invo_price)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

cosmed_F <- na.omit(subset(cosmed, cosmed$cos_invo_price > (Q[1] - 1.5*iqr) & cosmed$cos_invo_price < (Q[2]+1.5*iqr)))

#Linear Model
lm1<-lm(cos_invo_price~ category*uniprice + discount_type*totprice + city*invo_quant + quant*category + If_Discount*Freq + If_Discount*invo_quant ,data=cosmed_F)

lm2<-lm(cos_invo_price~ Freq+Whether_F+invo_quant*day + category*uniprice + discount_type*totprice + city*invo_quant + quant*category + If_Discount*Freq + If_Discount*invo_quant + Whether_F*If_Discount ,data=cosmed_F)

lm3<-lm(cos_invo_price~ Freq+Whether_F+invo_quant*day + category*uniprice + discount_type*totprice + city*invo_quant + quant*category + If_Discount*Freq + If_Discount*invo_quant ,data=cosmed_F)

# ANOVA
anova(lm3,lm1)

# residual plot
plot(fitted(lm3), residuals(lm3))

hist(x = subset(residuals(lm3), 500 > residuals(lm3) & residuals(lm3) > -500),xlab="residual(lm3)")

# QQplot
qqnorm(residuals(lm3))
qqline(residuals(lm3))

# EDA
boxplot(cosmed$cos_invo_price ~ factor(cosmed$Whether_F)+factor(cosmed$day),xlab = "高頻率與否&消費日",ylab="客單價",ylim=c(-1000,15000))

boxplot(cosmed$cos_invo_price ~ factor(cosmed$Whether_F)+factor(cosmed$category),xlab = "高頻率與否&消費品項",ylab="客單價",ylim=c(-1000,15000))

boxplot(cosmed$cos_invo_price ~ factor(cosmed$Whether_F)+factor(cosmed$city),xlab = "高頻率與否&消費地點",ylab="客單價",ylim=c(-1000,15000))

boxplot(cosmed$cos_invo_price ~ factor(cosmed$Whether_F)+factor(cosmed$time),xlab = "高頻率與否&消費時間",ylab="客單價",ylim=c(-1000,15000))

# apple電腦顯示中文
install.packages("showtext")
library(showtext)

