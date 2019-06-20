library(jsonlite)
library(dplyr)
library(readr)

x103each_edu <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/103each_edusalary_Industry.csv")
X103eachtype <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/103eachtype_juniorsalary.csv")
x104each_edu <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/104each_edusalary_Industry.csv")
X104eachtype <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/104eachtype_juniorsalary.csv")
x105each_edu <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/105each_edusalary_Industry.csv")
X105eachtype <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/105eachtype_juniorsalary.csv")
x106each_edu <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/106each_edusalary_Industry.csv")
X106eachtype <- read_csv("C:/Users/71414/Downloads/junior_employ_avgsalary/106eachtype_juniorsalary.csv")

#-------------------------------------------------------------------
#1

#將106年度薪資表與103年度薪資表依照大職業別連結
hw1_DF <- merge(x103each_edu,x106each_edu,by = "大職業別")
#新增欄位rate(106大學畢業薪資/103大學畢業薪資)
hw1_DF$rate <- hw1_DF$`大學-薪資.y`/hw1_DF$`大學-薪資.x`
#按照rate從大到小做排序
hw1_DF <- hw1_DF[order(hw1_DF$rate,decreasing = T),]
#取前十筆資料
View(head(hw1_DF,n=10))
# 其他服務業的成長明顯，在成長比例前十名中佔了3個;再來是住宿及餐飲業，
# 佔了2個。可推斷106年台灣服務產業較三年前有不小的提升。

#篩選rate>1.05的資料
hw1_DF1 <- filter(hw1_DF,rate > 1.05)
#提高超過5%的職業
View(table(hw1_DF1$大職業別))
View(head(hw1_DF,n=10))


#取出大職業別"-"前面字串
industry_DF <- strsplit (hw1_DF1$大職業別,"-")

industryFirst_DF <- data.frame(index= c(1:length(industry_DF)))

for (n in 1:length(industry_DF)) {
  industryFirst_DF[n,1]<-industry_DF[[n]][1]
}

#提高超過5%的主要職業種別
table(industryFirst_DF)

#-------------------------------------------------------------------
#2

#將103到106年大學畢業男女薪資比例由小到大排序並取前十筆
#將103到106年大學畢業男女薪資比例由大到小排序並取前十筆

View(head(arrange(x103each_edu,`大學-女/男`),10))
View(head(arrange(x103each_edu,desc(`大學-女/男`)),10))

View(head(arrange(x104each_edu,`大學-女/男`),10))
View(head(arrange(x104each_edu,desc(`大學-女/男`)),10))

View(head(arrange(x105each_edu,`大學-女/男`),10))
View(head(arrange(x105each_edu,desc(`大學-女/男`)),10))

View(head(arrange(x106each_edu,`大學-女/男`),10))
View(head(arrange(x106each_edu,desc(`大學-女/男`)),10))
# 觀察103到106年男女大學畢業薪資比例，發現男性在礦業及土石採取業、
# 電力及燃氣供應業等兩大產業普遍高於女性。但在105年度中，
# 大學畢業男性在不動產業的薪資表現明顯優於女性。
# 另外可以觀察到女性在薪資比例上與男性的差距逐年縮小。
# 103年時女性僅在三種大職業別上高於男性，
# 而到106年時女性已有九種大職業別薪資高於男性。
#-------------------------------------------------------------------
#3

#新增欄位顯示研究所與大學薪資比例
x106each_edu$salary_rate <- x106each_edu$`研究所及以上-薪資`/x106each_edu$`大學-薪資`
#將研究所與大學薪資比例從大到小排序並取出前十筆觀察
View(head(arrange(x106each_edu,desc(salary_rate)),10))
View(arrange(x106each_edu,desc(salary_rate)))
# 從106的資料來看，研究所學歷薪資在科學及技術服務業明顯較大學學歷薪資高，
# 表示在相關科技技術領域研究所學歷具有一定的價值，念研究所也較划算。
# 從事礦業及土石採取業-事務支援人員念研究所最划算。而從事服務業相關產業
# 的薪資比例則相對較低，念研究所相對不划算。

#-------------------------------------------------------------------
#4

#列出專業_科學及技術服務業-專業人員與資訊及通訊傳播業-專業人員的資料
View(x106each_edu[grepl("專業_科學及技術服務業-專業人員|資訊及通訊傳播業-專業人員",x106each_edu$大職業別),])
myfavor_DF <- x106each_edu[grepl("專業_科學及技術服務業-專業人員|資訊及通訊傳播業-專業人員",x106each_edu$大職業別),]
#兩個職業別的不同學歷薪資差異
myfavor_DF$salary_minus <- myfavor_DF$`研究所及以上-薪資`- myfavor_DF$`大學-薪資`

# 根據觀察，兩個職業別的研究所薪資與大學薪資差距分別為4700與5000，
# 與我所想的狀況差不多。但此資料並未能顯示研究所與大學薪資之成長性，
# 如欲考量其成長性需更進一步追蹤畢業後薪資成長之變化資料。分析完此資料
# 後不會因此改變心意，決定念研究所。
