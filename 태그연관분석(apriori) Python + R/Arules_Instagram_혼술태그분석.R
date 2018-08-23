# install.packages("arules")
# install.packages("arulesViz")
library(arules)
library(arulesViz)
# library(KoNLP)
# install.packages("rJava")
# library(rJava)


# 19번 줄의 함수를 사용할 때 매개변수로 파일을 넣어줘야하므로, 데이터프레임을 export했다가 다시 불러들이는 코드 

dataset = read.transactions('/Users/JiheeJeon/AnacondaProjects/Python Data Analysis Practice/finalplease.csv', sep = ',', rm.duplicates = TRUE, encoding="UTF-8")

dataset2 = read.transactions('/Users/JiheeJeon/AnacondaProjects/Python Data Analysis Practice/finalplease2.csv', sep = ',', rm.duplicates = TRUE, encoding="UTF-8")

dataset3 = read.transactions('/Users/JiheeJeon/AnacondaProjects/Python Data Analysis Practice/finalplease3.csv', sep = ',', rm.duplicates = TRUE, encoding="UTF-8")

# apriori 함수에 사용할 수 있는 transaction 형태의 데이터 포맷으로 변환 
transaction2 = as(dataset, "transactions")
transaction3 = as(dataset2, "transactions")
transaction4 = as(dataset3, "transactions")


# 가장 많이 나오는 아이템 추출
par(family="AppleGothic")
itemFrequencyPlot(transaction2, topN = 50)
itemFrequencyPlot(transaction3, topN = 50)
itemFrequencyPlot(transaction4, topN = 50)



# 실험하기: support, confidence, minlen을 바꿔가면서.
# Visualization 2
## 원이 연관 관계를 나타내며, 원의 크기가 Support, 색상진하기가 Lift
dev.off()
par(family="AppleGothic")

rules = apriori(data = transaction2, parameter = list(support = 0.01, confidence = 0.2, maxlen=5))
inspect(rules)
plot(rules, method="graph", control=list(type="items"))

# 혼술 태그 포함 안된 것
r = apriori(data = transaction2, parameter = list(support = 0.001, confidence = 0.5, maxlen=5))
inspect(r)
plot(r, method="graph", control=list(type="items"),labelfontsize = 0.2, arowsize= 0.4, edge.arrow.size=0.1, vertex.size=0.3)


r1 = apriori(data = transaction2, parameter = list(support = 0.0001, confidence = 0.1, maxlen=5), appearance=list(lhs="맥주", default="rhs"))
inspect(r1)
plot(r1, method="graph", control=list(type="items"))

r11 = apriori(data = transaction2, parameter = list(support = 0.001, confidence = 0.1, maxlen=5), appearance=list(lhs="안주", default="rhs"))
inspect(r1)
plot(r1, method="graph", control=list(type="items"))

r2 = apriori(data = transaction2, parameter = list(support = 0.001, confidence = 0.5, maxlen=5), appearance=list(none=c("맥주")))
inspect(r2)
plot(r2, method="graph", control=list(type="items"))
plot(r2, method="grouped", control=list(type="items"))


r3 = apriori(data = transaction2, parameter = list(support = 0.001, confidence = 0.3, maxlen=5), appearance=list(rhs="야식", default="lhs"))
inspect(r3)
plot(r3, method="graph", control=list(type="items"))

#===============#===============#===============#

# 혼술 태그 포함된거 
w = apriori(data = transaction4, parameter = list(support = 0.001, confidence = 0.5, maxlen=5))
inspect(w)
plot(w, method="graph", control=list(type="items"))
plot(w, method="graph", control=list(type="itemsets"), labelfontsize = 0.2, arowsize= 0.4)

w2 = apriori(data = transaction4, parameter = list(support = 0.001, confidence = 0.5, maxlen=5), appearance=list(rhs="맥주", default="lhs"))
inspect(w2)
plot(w2, method="graph", control=list(type="items"))

r = apriori(data = transaction2, parameter = list(support = 0.001, confidence = 0.5, maxlen=5))
inspect(r)
plot(r, method="graph", control=list(type="itemsets"),labelfontsize = 0.2, arowsize= 0.4)
