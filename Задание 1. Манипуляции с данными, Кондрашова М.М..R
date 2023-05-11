#для региона 55 рассчитайте урожайность пшеницы в 2002 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с 24 ближайших метеостанций

library(dplyr)
library(tidyverse)
library(rnoaa) 

station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")
station_data = read_csv("station_data.csv")

omsk = data.frame(id = "Omsk", latitude = 54.9924,  longitude = 73.3686)
omsk_around = meteo_nearby_stations(lat_lon_df = omsk, station_data = station_data,limit = 24, var = c("PRCP", "TAVG"), year_min = 1993, year_max = 2001)

omsk_id = omsk_around$Omsk$id
all_omsk_data = meteo_tidy_ghcnd(stationid = omsk_id)

all_omsk_data$date
all_omsk_data = all_omsk_data %>% mutate (
  year = year(date),
  month = month(date),
  day = yday(date)
)

all_omsk_data = all_omsk_data %>% select (id, year, month, day, tavg, tmin, tmax)

all_omsk_data=all_omsk_data %>% mutate(
  tavg=tavg/10,
  tmin=tmin/10,
  tmax=tmax/10
)

all_omsk_data = all_omsk_data %>% mutate(
  tavg = case_when(
    is.na(tavg) ~ 0,
    tavg < 5 ~ 0,
    TRUE ~ tavg
  ))
all_omsk_data = all_omsk_data %>% filter (
  year >= 1993,
  year <= 2001
)


alldays = group_by(all_omsk_data, id, year, month)

sumT_alldays_omsk = summarise(alldays,  tsum =  sum(tavg))
summary(sumT_alldays_omsk)

groups_omsk_months = group_by(sumT_alldays_omsk, month) 
sumT_months = summarise(groups_omsk_months, St = mean(tsum))
sumT_months

#Произведём расчёт урожайности

afi =c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi =c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di =c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 # - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры;  

sumT_months = mutate(sumT_months,  Fi = afi + bfi * y * St)

sumT_months = mutate(sumT_months,  Yi =  ((Fi * di) * Kf)/(Qj * Lj * (100 - Ej)))

Yield = sum(sumT_months$Yi)
Yield
#урожайность - 15,6 ц/га