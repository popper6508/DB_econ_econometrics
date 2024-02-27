#### DB 경제공모전 ####
## 팀 : 계량해보자
## 팀원 : 김겨레 심우석
## 주제 : 기업 규모에 따른 차등정책과 중소기업의 성장성

using DataFrames
using FixedEffects
using FixedEffectModels
using Statistics
using StatsModels
using XLSX
using Plots

#### Hypothesis 1
kfs_total_2009_2019 = DataFrame(XLSX.readtable("./KFS_merge_data/KFS_panel_with_threeyearsales.xlsx", "Sheet1"))

did_cross_data_1_2 = filter(row -> row[:산업분류_대t] == "제조업(10~33)", kfs_total_2009_2019)
did_cross_data_1_2[!,:매출액증가율tp1] = log.(did_cross_data_1_2[!, :매출액tp1]) - log.(did_cross_data_1_2[!, :매출액t])
did_cross_data_1_2[!,:매출액증가율t] = log.(did_cross_data_1_2[!, :매출액t]) - log.(did_cross_data_1_2[!, :매출액tm1])
did_cross_data_1_2[!,:상시근로자증가율] = log.(did_cross_data_1_2[!, :상시근로자수tp1]) - log.(did_cross_data_1_2[!, :상시근로자수t])
did_cross_data_1_2[!,:상시근로자기준dummy] = (did_cross_data_1_2[!, :상시근로자수t] .>= 270) .& (did_cross_data_1_2[!, :상시근로자수t] .< 300)
did_cross_data_1_2[!,:Post] = (did_cross_data_1_2[!, :기준연도] .>= 2014)
did_cross_data_1_2[!,:ln상시근로자수t] = log.(did_cross_data_1_2[!, :상시근로자수t])

did_cross_data_1_2 = filter(row -> isfinite(row[:매출액증가율tp1]) && isfinite(row[:상시근로자증가율]) && (row[:기준연도] <= 2018), did_cross_data_1_2)

grouped_mean = combine(groupby(did_cross_data_1_2, [:기준연도, :상시근로자기준dummy]), :상시근로자증가율 => mean => :상시근로자증가율_mean)

plot(grouped_mean[grouped_mean[!,:상시근로자기준dummy] .== 0, :기준연도], grouped_mean[grouped_mean[!,:상시근로자기준dummy] .== 0, :상시근로자증가율_mean], label="Other Group")
plot!(grouped_mean[grouped_mean[!,:상시근로자기준dummy] .== 1, :기준연도], grouped_mean[grouped_mean[!,:상시근로자기준dummy] .== 1, :상시근로자증가율_mean], label="270-300 Group")
xlabel!("Year")
ylabel!("Growth Rate Mean")
vline!([2014.5], color=:red, linestyle=:dash)
title!("Average Employment Growth Rate")

industries = unique(did_cross_data_1_2[:, :산업분류_중t])
for (i, industry) in enumerate(industries)
       did_cross_data_1_2[!, "industries$i"] = did_cross_data_1_2[:, :산업분류_중t] .== industry
end

indudummy = ["industries$i" for i in 1:25]
indudummy_str = join(indudummy, " + ")

using Statistics

# Pooled regression
ols_cross_3_1 = reg(did_cross_data_1_2_panel_anal_p, @formula(상시근로자증가율 ~ 1 + 상시근로자기준dummy*Post + ln상시근로자수t + 매출액증가율tp1 + 매출액증가율t + 산업분류_중t))
println(ols_cross_3_1)

# Regression with unit effect
did_cross_3_2 = reg(did_cross_data_1_2, @formula(상시근로자증가율 ~ 상시근로자기준dummy*Post + ln상시근로자수t + 매출액증가율tp1 + 매출액증가율t + fe(패널키) + 산업분류_중t))
println(did_cross_3_2)

# Regression with time effect
did_cross_3_3 = reg(did_cross_data_1_2, @formula(상시근로자증가율 ~ 상시근로자기준dummy*Post + ln상시근로자수t + 매출액증가율tp1 + 매출액증가율t + fe(기준연도) + 산업분류_중t))
println(did_cross_3_3)

# Regression with two way effect
did_cross_3_4 = reg(did_cross_data_1_2, @formula(상시근로자증가율 ~ 상시근로자기준dummy*Post + ln상시근로자수t + 매출액증가율tp1 + 매출액증가율t + fe(패널키) + fe(기준연도)  + 산업분류_중t))
println(did_cross_3_4)

#### Hypothesis 2
kfs_did_data_2010 = DataFrame(XLSX.readtable("KFS_merge_data/KFS_2010_2020.xlsx", "Sheet1"))
kfs_panel_data_2010 = DataFrame(XLSX.readtable("KFS_merge_data/KFS_Panel_2010_2020.xlsx", "Sheet1"))

kfs_did_data_2010_anal = filter(row -> (row[:산업분류_대2014] == "제조업(10~33)") &&
                                       (row[:산업분류_중2014] != "음료제조업") &&
                                       (row[:산업분류_중2014] != "의료용물질및의약품제조업") &&
                                       (row[:산업분류_중2014] != "비금속광물제품제조업") &&
                                       (row[:산업분류_중2014] != "의료,정밀,광학기기및시계제조업") &&
                                       (row[:산업분류_중2014] != "기타제품제조업") &&
                                       (row[:자산2014] < 500000) && 
                                       (row[:종사자수_상용_합2014] < 1000) && 
                                       (row[:"3년평균매출액2015기준"] < 100000) &&
                                       (row[:자본총계2014] < 100000) &&
                                       ((row[:자본금2014] < 8000) || (row[:종사자수_상용_합2014] < 300)), 
                                       kfs_did_data_2010)

kfs_did_data_2010_anal = filter(row ->  (row[:자산2013] < 500000) && 
                                        (row[:종사자수_상용_합2013] < 1000) &&
                                        (row[:"3년평균매출액2014기준"] < 100000) &&
                                        (row[:자본총계2013] < 100000) &&
                                        ((row[:자본금2013] < 8000) || (row[:종사자수_상용_합2013] < 300)), 
                                        kfs_did_data_2010_anal)

kfs_did_data_2010_anal = filter(row ->  (row[:자산2012] < 500000) && 
                                        (row[:종사자수_상용_합2012] < 1000) && 
                                        (row[:"3년평균매출액2013기준"] < 100000) &&
                                        (row[:자본총계2013] < 100000) &&
                                        ((row[:자본금2012] < 8000) || (row[:종사자수_상용_합2012] < 300)), 
                                        kfs_did_data_2010_anal)

kfs_did_data_2010_anal = filter(row ->  (row[:자산2011] < 500000) && 
                                        (row[:종사자수_상용_합2011] < 1000) && 
                                        (row[:"3년평균매출액2012기준"] < 100000) &&
                                        (row[:자본총계2012] < 100000) &&
                                        ((row[:자본금2011] < 8000) || (row[:종사자수_상용_합2011] < 300)), 
                                        kfs_did_data_2010_anal)

kfs_did_data_2010_anal[!, :treatment] .= 0
mask = (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "식료품제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "섬유제품제조업;의복제외") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "목재및나무제품제조업;가구제외") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "코크스,연탄및석유정제품제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "화학물질및화학제품제조업;의약품제외") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "고무제품및플라스틱제품제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "금속가공제품제조업;기계및가구제외") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "전자부품,컴퓨터,영상,음향및통신장비제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "기타기계및장비제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "자동차및트레일러제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "기타운송장비제조업") .|
       (kfs_did_data_2010_anal[!,:산업분류_중2014] .== "담배제조업")
kfs_did_data_2010_anal.treatment[mask] .= 1

kfs_panel_data_2010_anal = filter(row -> (row.기준연도 >= 2012) && 
                                           (row.패널키 in kfs_did_data_2010_anal[:, :"패널키"]), 
                                    kfs_panel_data_2010)

kfs_panel_data_2010_anal_did = innerjoin(kfs_panel_data_2010_anal, kfs_did_data_2010_anal[:, [:패널키, :treatment]], on=:패널키)

years = unique(kfs_panel_data_2010_anal_did[:, :기준연도])
for (i, year) in enumerate(years)
       kfs_panel_data_2010_anal_did[!, "yeardummy$i"] = kfs_panel_data_2010_anal_did[:, :기준연도] .== year
end

kfs_panel_data_2010_anal_did[!, :ln자산] = log.(kfs_panel_data_2010_anal_did[:, :자산])
kfs_panel_data_2010_anal_did[!, :ln매출액] = log.(kfs_panel_data_2010_anal_did[:, :매출액])

kfs_panel_data_2010_anal_did = filter(row -> isfinite(row[:매출액증가율]) && isfinite(row[:ln매출액]), kfs_panel_data_2010_anal_did)

kfs_panel_data_2010_anal_did.매출액증가율 = convert(Vector{Float64}, kfs_panel_data_2010_anal_did.매출액증가율)
kfs_panel_data_2010_anal_did.ln매출액 = convert(Vector{Float64}, kfs_panel_data_2010_anal_did.ln매출액)
kfs_panel_data_2010_anal_did.treatment = Int.(kfs_panel_data_2010_anal_did.treatment)
kfs_panel_data_2010_anal_did.Post = Int.(kfs_panel_data_2010_anal_did.Post)

# Pooled OLS regression
model_1 = reg(kfs_panel_data_2010_anal_did, @formula(매출액증가율 ~ treatment*Post + ln매출액 + 마진율 + 부채비율))
println(model_1)

# Panel OLS regression with unit effects
model_2 = reg(kfs_panel_data_2010_anal_did, @formula(매출액증가율 ~ treatment*Post + ln매출액 + 마진율 + 부채비율 + fe(패널키)))
println(model_2)

# Panel OLS regression with time effects
model_3 = reg(kfs_panel_data_2010_anal_did, @formula(매출액증가율 ~ treatment*Post + ln매출액 + 마진율 + 부채비율 + fe(기준연도)))
println(model_3)

# Panel OLS regression with both entity and time effects
model_4 = reg(kfs_panel_data_2010_anal_did, @formula(매출액증가율 ~ treatment*Post + ln매출액 + 마진율 + 부채비율 + fe(패널키) + fe(기준연도)))
println(model_4)

# Panel OLS regression with clustered standard errors by entity
model_year = reg(kfs_panel_data_2010_anal_did, 매출액증가율 ~ treatment*yeardummy1 + treat*yeardummy2 + treat*yeardummy4 + treat*yeardummy5 + treat*yeardummy6 + treat*yeardummy7 + treat*yeardummy8)
println(model_year)

# Plot Coefficients
coefficients = model_year.β[9:end]
conf_int = model_year.β_stderror[9:end]
variables = [2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019]

plot(variables, coefficients, marker = :o, label = "Coefficients", xlabel = "Year", ylabel = "Coefficients", title = "Difference in Difference Interaction Term Coefficient", legend = :bottomright)
vline!([2.5], color = :red, linestyle = :dash)
plot!(xticks = (1:8, variables), grid = true)

# Plot Average Sales Growth Rate
grouped_mean = by(kfs_panel_anal_did_2010_data_p, [:기준연도, :treat], df -> mean(df.매출액증가율))
grouped_mean = combine(grouped_mean, :매출액증가율 => mean => :매출액증가율_mean)
dummy_0 = grouped_mean[grouped_mean.treat .== 0, :]
dummy_1 = grouped_mean[grouped_mean.treat .== 1, :]

plot(dummy_0.기준연도, dummy_0.매출액증가율_mean, label = "Control Group", xlabel = "Year", ylabel = "Growth Rate Mean", title = "Average Sales Growth Rate")
plot!(dummy_1.기준연도, dummy_1.매출액증가율_mean, label = "Treat Group")
vline!([2014.5], color = :red, linestyle = :dash)



using MultivariateStats

# Example dataset (replace this with your own data)
data = rand(10, 2)

# Perform PCA
pca_model = fit(PCA, data; maxoutdim = 2)  # Set maxoutdim to the number of principal components you want

# Project the data onto the principal components
projected_data = transform(pca_model, data)

# Plot the original and projected data
using Plots
scatter(data[:, 1], data[:, 2], label = "Original Data", xlabel = "X1", ylabel = "X2", zlabel = "X3")
scatter!(projected_data[:, 1], label = "Projected Data")
