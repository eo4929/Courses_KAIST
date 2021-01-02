
#1

O = c(20,39,29,36,41,189,174,12)
tc = c("Student")
tr = c("Physics","Mathematical Sciences","Chemistry","Biological Sciences","Mechanical Engineering", "Electrical Engineering","School of Computing","Industrial Design")
res = matrix(O, dimnames=list(tr,tc))
as.table(res)
res
chisq.test(res) # ans: Because p value is less than 0.05, I observe all students don't be equally divided 


#2
#2-1)
dt1 <- array(c(14,4,7, 10,15,9, 3,11,5), dim=c(3,3),
             dimnames=list("Ratio Format Preference"=c("Music","News-talk","Sports"),
                           "Age"=c("Young Adult","Middle Age","Older Adult")))
dt1 <- as.table(dt1)
dt1
chisq.test(dt1) # ans: Because p value is less than 0.05, we reject the null hypothesis of independence

#2-2)
prop.table(dt1,1) # row
prop.table(dt1,2) # col

#2-3)
library(grid); library(vcd)
mosaic(dt1,shade=TRUE,legend=TRUE)


#3
data("Punishment", package="vcd")
str(Punishment)
head(Punishment)

#3-1)
Punishment <- Punishment[,-2]
Punishment_3wt <- xtabs(Freq~memory+education+age,data=Punishment)

ft3 <- ftable(Punishment_3wt)
ft3

#3-2)
prop.table(ft3,1)
chisq.test(ft3) # ans: Because p value is less than 0.05, we reject the null hypothesis of independence for punishment frequency

#3-3)
data("Punishment", package="vcd")
Punishment <- Punishment[,-3]
Punishment <- Punishment[,-3]
Punishment_2wt <- xtabs(Freq~attitude+age,data=Punishment) # 아 x에 속하는 변수들의 순서가 달라지면 테이블 모양이 바뀌어서 p-value도 다르게 나오는구나..

ft2 <- ftable(Punishment_2wt)
ft2

#3-4)
prop.table(ft2,1)
chisq.test(ft2) # ans: Because p value is less than 0.05, we reject the null hypothesis of independence for punishment frequency

#3-5)

doubledecker(ft3)

doubledecker(ft2)

