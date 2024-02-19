#### DB 경제공모전 ####
## 팀 : 계량해보자
## 팀원 : 김겨레 심우석
## 주제 : 기업 규모에 따른 차등정책과 중소기업의 성장성

##### Hypothesis 1 #####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(coefplot)
library(readxl)
library(haven)
library(dplyr)
library(writexl)
library(plm)
library(stargazer)
library(ggplot2)

data=read_excel('./KFS_merge_data/KFS_panel_with_threeyearsales.xlsx')
data2 = data%>%
  as.data.frame() %>%
  filter(자산t<500000L & 자본총계t<100000L
          & "3년평균매출액기준tp1"<80000L & 상시근로자수t<300L
          & 산업분류_대t == '제조업(10~33)'
          & 기준연도<2019)

data2=data2%>%mutate(d=log(매출액tp1)-log(매출액t), 마진율t = 법인세차감전순이익t/매출액t, 당좌비율t=당좌자산t/자산t, 부채비율t=부채t/자산t,매출액증가율t=log(매출액t)-log(매출액tm1))

data2 = rename(data2, "평균매출액기준tp1" = "3년평균매출액기준tp1")

data2=data2%>%filter(d!=Inf & d!=-Inf & d!="NA" & 평균매출액기준tp1!='NA' &
                     GR_EMPtp1!='NA' & GR_EMPtp1!=Inf & GR_EMPtp1!=-Inf &
                     당좌비율t !=Inf & 부채비율t !=Inf & 매출액증가율t != Inf &
                     매출액증가율t != -Inf & 매출액증가율t != "NA")
post=if_else(data2$기준연도>=2014L,1,0)
treat=if_else(data2$상시근로자수t>=270L & data2$상시근로자수t<300L,1,0)

data2=data.frame(data2,post= post, treat = treat)
pdata=pdata.frame(data2, index=c('패널키','기준연도'))

d08=as.numeric(if_else(pdata$기준연도==2008L,1,0))
d09=as.numeric(if_else(pdata$기준연도==2009L,1,0))
d10=as.numeric(if_else(pdata$기준연도==2010L,1,0))
d11=as.numeric(if_else(pdata$기준연도==2011L,1,0))
d12=as.numeric(if_else(pdata$기준연도==2012L,1,0))
d13=as.numeric(if_else(pdata$기준연도==2013L,1,0))
d15=as.numeric(if_else(pdata$기준연도==2015L,1,0))
d16=as.numeric(if_else(pdata$기준연도==2016L,1,0))
d17=as.numeric(if_else(pdata$기준연도==2017L,1,0))
d18=as.numeric(if_else(pdata$기준연도==2018L,1,0))

year=cbind(d08,d09,d10,d11,d12,d13,d15,d16,d17,d18)

a= pdata%>%filter(treat==1)%>%group_by(기준연도)%>%summarise(고용성장률평균=mean(GR_EMPtp1))
b= pdata%>%filter(treat==0)%>%group_by(기준연도)%>%summarise(고용성장률평균=mean(GR_EMPtp1))
c= pdata%>%group_by(기준연도)%>%summarise(평균=mean(GR_EMPtp1))

pdata$characteristic <- factor(pdata$기준연도)
pdata$treat_factor <- factor(pdata$treat)

plot(ts(b[,2]), type='l', ylim=c(-0.2,0.2))
lines(ts(a[,2]), col='red', type='o')
lines(ts(c[,2]), col='blue', type='o')

abline(v=7)

##### 더미 = 1 비율 구하기
a= pdata%>%filter(treat==0)%>%group_by(기준연도)%>%count()
b= pdata%>%filter(treat==1)%>%group_by(기준연도)%>%count()
c= b[,2]/(a[,2]+b[,2])
c*100

##### Unbalanced Panel Data Regression
mod1=plm(GR_EMPtp1~log(상시근로자수t)+post*treat+d+산업분류_중t+마진율t+당좌비율t+매출액증가율t+부채비율t
         , data=pdata, index=c('패널키','기준연도'))

mod2=plm(GR_EMPtp1~log(상시근로자수t)+post*treat+d+산업분류_중t+마진율t+당좌비율t+매출액증가율t+부채비율t
         , data=pdata, index=c('패널키','기준연도') , model= 'within', effect = 'time')

mod3=plm(GR_EMPtp1~log(상시근로자수t)+post*treat+d+산업분류_중t+마진율t+당좌비율t+매출액증가율t+부채비율t
         , data=pdata, index=c('패널키','기준연도'), model= 'within', effect = 'individual')

mod4=plm(GR_EMPtp1~log(상시근로자수t)+post*treat+d+산업분류_중t+마진율t+당좌비율t+매출액증가율t+부채비율t
         , data=pdata, index=c('패널키','기준연도'), model= 'within', effect = 'twoways')

stargazer(mod1,mod2,mod3,mod4, omit=c('year','산업분류_중t','d','마진율t','매출액증가율t','당좌비율t','부채비율t'), type = 'text', out = 'Did비교2.doc',
          column.labels = c('Pooled','Time FE','Individual FE', 'Twoways'),add.lines=list(c("Industry", "YES",
                                                                                            "YES","YES","YES")))

#####Coefplot each year
mod3=plm(GR_EMPtp1~log(상시근로자수t)+year*treat+산업분류_중t
         , data=pdata, index=c('패널키','기준연도'), model= 'pooling')

mod4=plm(GR_EMPtp1~log(상시근로자수t)+산업분류_중t+ year*treat
         , data=pdata, index=c('패널키','기준연도') , model= 'within', effect = 'time')

mod5=plm(GR_EMPtp1~log(상시근로자수t)+산업분류_대tm1+year*treat 
         , data=pdata, index=c('패널키','기준연도'), model= 'within', effect = 'individual')

mod6=plm(GR_EMPtp1~log(상시근로자수t)+year*treat 
         , data=pdata, index=c('패널키','기준연도'), model= 'within', effect = 'twoways')

coefplot(mod6,coef= c('yeard08:treat','yeard09:treat','yeard10:treat','yeard11:treat',
                      'yeard12:treat','yeard13:treat','yeard15:treat'
                      ,'yeard16:treat','yeard17:treat','yeard18:treat'),horizontal = TRUE)

####Hausman Test
fixed= mod6
random = plm(GR_EMPtp1~log(상시근로자수t)+year*treat 
             , data=pdata, index=c('패널키','기준연도'), model= 'random')

phtest(random, fixed)

##### Hypothesis 2 #####

kfs_did_data_2010=read_excel('./KFS_merge_data/KFS_2010_2020.xlsx',)
kfs_panel_data_2010=read_excel('./KFS_merge_data/KFS_Panel_2010_2020.xlsx',)

# Filter data
did_test_key <- kfs_did_data_2010 %>%
  filter(산업분류_대2014 == "제조업(10~33)" &
           !(산업분류_중2014 %in% c("음료제조업", "의료용물질및의약품제조업", "비금속광물제품제조업", "의료,정밀,광학기기및시계제조업", "기타제품제조업")) &
           (상시근로자수2014 < 300 | 자본금2014 < 8000) & 자산2014 < 500000 & 자본총계2014 < 100000 & `3년평균매출액2015기준` < 100000 & 상시근로자수2014 < 1000)

did_test_key_panel_anal <- did_test_key %>%
  filter((상시근로자수2013 < 300 | 자본금2013 < 8000) & 자산2013 < 500000 & 자본총계2013 < 100000 & `3년평균매출액2014기준` < 100000 & 상시근로자수2013 < 1000)

did_test_key_panel_anal <- did_test_key_panel_anal %>%
  filter((상시근로자수2012 < 300 | 자본금2012 < 8000) & 자산2012 < 500000 & 자본총계2012 < 100000 & `3년평균매출액2013기준` < 100000 & 상시근로자수2012 < 1000)

did_test_key_panel_anal <- did_test_key_panel_anal %>%
  filter((상시근로자수2011 < 300 | 자본금2011 < 8000) & 자산2011 < 500000 & 자본총계2011 < 100000 & `3년평균매출액2012기준` < 100000 & 상시근로자수2011 < 1000)

did_test_key_panel_anal$treat <- ifelse(did_test_key_panel_anal$산업분류_중2014 %in% c("식료품제조업", "섬유제품제조업;의복제외", "목재및나무제품제조업;가구제외", "코크스,연탄및석유정제품제조업", "화학물질및화학제품제조업;의약품제외", "고무제품및플라스틱제품제조업", "금속가공제품제조업;기계및가구제외", "전자부품,컴퓨터,영상,음향및통신장비제조업", "기타기계및장비제조업", "자동차및트레일러제조업", "기타운송장비제조업", "담배제조업"), TRUE, FALSE)

kfs_panel_anal_did_2010 <- kfs_panel_data_2010 %>%
  filter(패널키 %in% did_test_key_panel_anal$패널키)

kfs_panel_anal_did_2010_data <- merge(kfs_panel_anal_did_2010, did_test_key_panel_anal[, c("패널키", "treat")], by = "패널키")

kfs_panel_anal_did_2010_data <- kfs_panel_anal_did_2010_data %>%
  filter(기준연도 >= 2012 & 기준연도 < 2020)

kfs_panel_anal_did_2010_data$post = kfs_panel_anal_did_2010_data['기준연도'] >= 2015

kfs_panel_anal_did_2010_data$ln상시근로자수 <- log(kfs_panel_anal_did_2010_data$상시근로자수)
kfs_panel_anal_did_2010_data$ln매출액 <- log(kfs_panel_anal_did_2010_data$매출액)
kfs_panel_anal_did_2010_data$ln자산 <- log(kfs_panel_anal_did_2010_data$자산)

years <- unique(kfs_panel_anal_did_2010_data$기준연도)
for (i in 1:length(years)) {
  kfs_panel_anal_did_2010_data[[paste0("yeardummy", i)]] <- ifelse(kfs_panel_anal_did_2010_data$기준연도 == years[i], TRUE, FALSE)
}

kfs_panel_anal_did_2010_data_p <- kfs_panel_anal_did_2010_data

# Pooled OLS regression
model_1 <- lm(매출액증가율 ~ treat*post + ln매출액 + 마진율 + 부채비율, data = kfs_panel_anal_did_2010_data_p)
summary(model_1)

# Panel OLS regression with unit effects
model_2 <- plm(매출액증가율 ~ treat*post + ln매출액 + 마진율 + 부채비율, data = kfs_panel_anal_did_2010_data_p,
               index=c('패널키','기준연도'), model = "within", effect = "individual")
summary(model_2)

# Panel OLS regression with time effects
model_3 <- plm(매출액증가율 ~ treat*Post + ln매출액 + 마진율 + 부채비율, data = kfs_panel_anal_did_2010_data_p,
               index=c('패널키','기준연도'), model = "within", effect = "time")
summary(model_3)

# Panel OLS regression with both unit and time effects
model_4 <- plm(매출액증가율 ~ treat*Post + ln매출액 + 마진율 + 부채비율, data = kfs_panel_anal_did_2010_data_p,
               index=c('패널키','기준연도'), model = "within", effect = "twoways")
summary(model_4)

####Hausman Test
fixed=model_4
random =plm(매출액증가율 ~ treat*Post + ln매출액 + 마진율 + 부채비율, data = kfs_panel_anal_did_2010_data_p, model = "within")

phtest(random, fixed)

library(lmtest)
model_year <- plm(매출액증가율 ~ treat*yeardummy1 + treat*yeardummy2 + treat*yeardummy4 + treat*yeardummy5 + treat*yeardummy6 + treat*yeardummy7 + treat*yeardummy8, data = kfs_panel_anal_did_2010_data_p, model = "within", effect = "individual")
coeftest(model_year, vcov = vcovHC(model_year, cluster="group"))

# Print results
summary(ddd_year)

# Plot coefficients
coefficients <- coef(ddd_year)
variables <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

plot(variables, coefficients, type = "b", xlab = "Year", ylab = "Coefficients", main = "Difference in Difference Interaction Term Coefficient")
abline(v = 2.5, col = "red", lty = 2)
grid()
