library(RMySQL)
conn = dbConnect(MySQL(), user='***', password='***', dbname='ma_charity_full', host='localhost')
#sqlQuery(db, "USE ma_charity_full")
query = "SELECT contact_id, 
DATEDIFF(20131031, MIN(Act_Date)) / 365 AS 'T.cal',
DATEDIFF(MAX(Act_Date), MIN(Act_Date)) / 365 AS 't.x',
COUNT(Amount) - 1 AS 'x'
FROM acts
WHERE Act_Type_id LIKE 'DO'
GROUP BY 1;"
data = dbGetQuery(conn, query)
head(data)
# Buy-til-you-die library
library(BTYD)
# Clean data
data = data[!(data$T.cal < 0),]
# Estimate parameters for the Pareto-NBD process
params = bgnbd.EstimateParameters(data)
print(params)
# Plot goodness of fit (7 purchases)
bgnbd.PlotFrequencyInCalibration(params, data, 7)
# Get individual estimates
for (i in 1:185754) {
  data$ltv_5[i] = 
    bgnbd.ConditionalExpectedTransactions(params,
                                         T.star = 5,
                                         data$x[i], data$t.x[i], data$T.cal[i])
  #data$palive[i] = bgnbd.PAlive(params,
                               #data$x[i], data$t.x[i], data$T.cal[i])
}
print(data[1:10, ])
summary(data$ltv_5)
data$ltv_5 =data$ltv_5 - data$ltv_4 - data$ltv_3 - data$ltv_2 - data$ltv
data_2 = subset(data, select = -c(palive,T.cal,t.x,x))
write.csv(data_2,file = "frequency.csv")


query1 = "select s.segment
, ceiling(sum(a.Amount)/count(DISTINCT(s.contact_id))) as average_donation
from
segments s,
periods p,
acts a
Where
s.contact_id = a.contact_id and
s.period_id = 0 and
p.period_id = 0 and
a.act_date >= p.first_day and
a.act_date <= p.last_day
group by 1;"
Avg_donation_TB = dbGetQuery(conn, query1)

query2 = "select old.segment as old_segment, new.segment as new_segment,
count(new.segment) as count
from segments old,
segments new
where old.contact_id = new.contact_id and
old.period_id = 1 and
new.period_id = 0 
group by 1,2;"

Trans_data = dbGetQuery(conn, query2)

library(reshape)
Transition_data = cast(Trans_data, old_segment ~ new_segment)
Transition_data[is.na(Transition_data)] = 0

Transition_matrix = Transition_data

Transition_matrix[1,c(2:7)] = round(Transition_matrix[1,c(2:7)]/sum(Transition_matrix[1,c(2:7)]),2)
Transition_matrix[2,c(2:7)] = round(Transition_matrix[2,c(2:7)]/sum(Transition_matrix[2,c(2:7)]),2)
Transition_matrix[3,c(2:7)] = round(Transition_matrix[3,c(2:7)]/sum(Transition_matrix[3,c(2:7)]),2)
Transition_matrix[4,c(2:7)] = round(Transition_matrix[4,c(2:7)]/sum(Transition_matrix[4,c(2:7)]),2)
Transition_matrix[5,c(2:7)] = round(Transition_matrix[5,c(2:7)]/sum(Transition_matrix[5,c(2:7)]),2)
Transition_matrix[6,c(2:7)] = round(Transition_matrix[6,c(2:7)]/sum(Transition_matrix[6,c(2:7)]),2)

transition = data.matrix(Transition_matrix[2:7])

num_periods = 5
discount_rate = 0.1

segments = matrix(nrow = 6, ncol = num_periods)
segments[1:6,1] = c(41618, 16871, 120865, 14403, 13723, 21036)

for (i in 2:num_periods) {
  segments[,i] = segments[, i-1]
}
plot(segments[5, ])
lines(segments[1, ])
lines(segments[2, ])
lines(segments[3, ])
lines(segments[4, ])
lines(segments[5, ])
print(round(segments))



yearly_revenue = c(Avg_donation_TB[1,2],0, 0, Avg_donation_TB[2,2], Avg_donation_TB[3,2], 0)

revenue_per_segment = yearly_revenue*segments
print("Revenue per segment:")
print(revenue_per_segment)

cumulated_revenue = cumsum(yearly_revenue)
print("cumulative_revenue:")
print(cumulated_revenue)
plot(cumulated_revenue)
lines(cumulated_revenue)

discount = 1/((1+discount_rate)^((1:num_periods)-1))
print(discount)

disc_yearly_revenue = yearly_revenue * discount
print("Discounted yearly revenue:")
print(disc_yearly_revenue)
plot(yearly_revenue)
lines(yearly_revenue)
lines(disc_yearly_revenue)

disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print("discounted cumulated revenue:")
print(disc_cumulated_revenue)
plot(cumulated_revenue)
lines(cumulated_revenue)
lines(disc_cumulated_revenue)

query3 = "select *  from segments
where period_id = 0;"

seg_data = dbGetQuery(conn, query3)

results = merge(x=data_2,y=seg_data,by="contact_id",all.x=TRUE)

results$rev_multi_factor = NA 
results$rev_multi_factor[results$segment == 'BOTTOM'] = 40
results$rev_multi_factor[results$segment == 'COLD'] =  0
results$rev_multi_factor[results$segment == 'LOST'] = 0
results$rev_multi_factor[results$segment == 'NEW'] =  44
results$rev_multi_factor[results$segment == 'TOP'] = 144
results$rev_multi_factor[results$segment == 'WARM'] =  0

results$CLV = (results$ltv + results$ltv_2 + results$ltv_3 + results$ltv_4 + results$ltv_5)*results$rev_multi_factor

results2 = results[,c(1,10,12)]

write.csv(results2,file = "F:\\DebdeepEssec\\MarketingAnalytics\\CLV2.csv")