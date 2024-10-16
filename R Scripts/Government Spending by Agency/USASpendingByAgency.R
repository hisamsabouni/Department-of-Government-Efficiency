#install.packages('httr')
#install.packages('jsonlite')
library(httr)
library(jsonlite)
setwd('~/Documents/GitHub/Department-of-Government-Efficiency/R Scripts/Government Spending by Agency')

url <- 'https://api.usaspending.gov/api/v2/spending/'
years <- 2017:2024
periods <- seq(3,12,by=3)

periods_to_gather <- rep(periods,length(years))
years_to_gather <- sort(rep(years,4))

years_to_gather <- years_to_gather[-1]#Data starts at Q2 2017
periods_to_gather <- periods_to_gather[-1] 
#Collection Date is October 2024 Drop the last 12
years_to_gather <- years_to_gather[-length(periods_to_gather)]#Data starts at Q2 2017
periods_to_gather <- periods_to_gather[-length(periods_to_gather)] #Data starts at Q2 2017
length(years_to_gather) == length(periods_to_gather) #TRUE

store_spending_by_year_and_agency <- list()
for(i in 1:length(years_to_gather)){
	#Generate request body to query usaspending
		#Each iteration is a year quarter combination
	body <- list(type='agency',filters=list(fy=years_to_gather[i],period=periods_to_gather[i]))
	#Generate post request with the body
	res <- POST(url,body=body,encode = 'json')
	#Read data in JSON format after extracting body in text format
	data <- fromJSON(content(res,'text'))
	#Extract results and store
	store_spending_by_year_and_agency[[i]] <- data$results
	#Pause for an average of 0.5 secs
	Sys.sleep(runif(1,0,1))
	#Track progress
	print(paste0('Percent Complete: ',100*round(i/length(years_to_gather),2),'%'))
}

#Iterate across all time frame gathered
unique_agency_names <- unique(unlist(lapply(store_spending_by_year_and_agency,function(x){x$name})))
#Create matrix to track agency data over time (time x agency)
spending_over_time_by_agency <- matrix(NA,length(years_to_gather),length(unique_agency_names))
colnames(spending_over_time_by_agency) <- unique_agency_names
#Fill the matrix
for(i in 1:length(years_to_gather)){
	matched_cols <- match(store_spending_by_year_and_agency[[i]]$name,unique_agency_names)
	spending_over_time_by_agency[i,matched_cols] <- store_spending_by_year_and_agency[[i]]$amount
}
#convert to data.frame
spending_over_time_by_agency_df <- data.frame(spending_over_time_by_agency)

#The data reported is cumulative as of the period date..
	#Will segement by year than iterate across years and difference to track quarterly allocation
data_by_year <- split(spending_over_time_by_agency_df,years_to_gather)
#Loop across the list of years and take differences
data_by_year_cleaned <- lapply(data_by_year,function(y){apply(y,2,function(x){c(x[1],diff(x))})})
#Re-stack data as one large data.frame
data_by_year_cleaned_df <- do.call('rbind.data.frame',data_by_year_cleaned)

#Add a column for the year
data_by_year_cleaned_df$Year <- years_to_gather
#Add a column for the quarter
data_by_year_cleaned_df$Quarter <- periods_to_gather/3
#Move year and quarter columnes to front of data frame
total_columns <- ncol(data_by_year_cleaned_df)
data_by_year_cleaned_df <- data_by_year_cleaned_df[,c(total_columns-1,total_columns,1:(total_columns-2))]
#Write data
write.csv(data_by_year_cleaned_df,'Spending By Agency Overtime.csv',row.names = F,na='')
