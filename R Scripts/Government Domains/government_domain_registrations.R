#Leverage Rwhois  to get domain registration data
###NOTES###
#The goal of this file is to get a sense of when government agencies have been created and 
# where they are created. Clearly, many government agencies were made before the advent of 
# the internet, however, this should track all newly created agencies in the last 20 or so years.
##########

##Necessary Libraries
#install.packages('Rwhois') - Used to pull domain registration data
#install.packages("ggplot2") - Used for Plots
#install.packages("gganimate") - Used for Plots
#install.packages("gifski") - Used for Plots
library(Rwhois)
library(ggplot2)
library(gganimate)
library(gifski)
library(transformr)
library(dplyr)
setwd('~/Documents/GitHub/Department-of-Government-Efficiency/R Scripts/Government Domains')
## Collect List of Government Agency Websites
## Source: Cybersecurity & Infrastructure Security Agency
government_websites <- read.csv('https://raw.githubusercontent.com/cisagov/dotgov-data/refs/heads/main/current-federal.csv')
#Write the baseline data with no augmentation
write.csv(government_websites,'Government Domains from Cybersecurity & Infrastructure Security Agency.csv',row.names = F)


#Number of unique sites
num_sites <- nrow(government_websites)

#create list to store whois domain registration data
who_is_registration_data <- list()
#Iterate through .gov domains whois registrations
for(i in 1:num_sites){
	#Query whois
	whois_res <- whois_query(government_websites$Domain.name[i])
	#Store the data
	who_is_registration_data[[i]] <- whois_res
	#Pause for an average of 0.5 secs
	Sys.sleep(runif(1,0,1))
	#Track progress
	print(paste0('Percent Complete: ',100*round(i/num_sites,2),'%'))
}
#Loop across stored data and collect unique set of whois registration keys
unique_whois_keys <- unique(unlist(lapply(who_is_registration_data,function(x){x$key})))

#Create a matrix to store whois data for each domain for a tabular format
who_is_registration_data_mat <- matrix(NA,num_sites,length(unique_whois_keys))
colnames(who_is_registration_data_mat) <- unique_whois_keys
#Iterate through list generated and fill matrix for tabular format
for(i in 1:length(who_is_registration_data)){
	matched_cols <- match(who_is_registration_data[[i]]$key,unique_whois_keys)
	who_is_registration_data_mat[i,matched_cols] <- who_is_registration_data[[i]]$val
}
#Convert to data.frame
who_is_registration_data_df <- data.frame(who_is_registration_data_mat)

#Convert date from string to datetime
who_is_registration_data_df$Creation.Date <- as.POSIXct(gsub('T',' ',who_is_registration_data_df$Creation.Date), tz="Etc/UTC")

#Write the file out
combined_data <- cbind(government_websites,who_is_registration_data_df)

#Sort data from oldest .gov domains to newest
combined_data <- combined_data[order(combined_data$Creation.Date,decreasing = F),]
combined_data$simple_date <- as.Date(combined_data$Creation.Date)

combined_data$president <- case_when(
	combined_data$simple_date >= as.Date("1997-01-20") & combined_data$simple_date < as.Date("2001-01-20") ~ "Bill Clinton",
	combined_data$simple_date >= as.Date("2001-01-20") & combined_data$simple_date < as.Date("2009-01-20") ~ "George W. Bush",
	combined_data$simple_date >= as.Date("2009-01-20") & combined_data$simple_date < as.Date("2017-01-20") ~ "Barack Obama",
	combined_data$simple_date >= as.Date("2017-01-20") & combined_data$simple_date < as.Date("2021-01-20") ~ "Donald J. Trump",
	combined_data$simple_date >= as.Date("2021-01-20") ~ "Joe Biden"
)
#Save the data 
write.csv(combined_data,'Government Domains with WHOIS Data from Cybersecurity & Infrastructure Security Agency.csv',row.names = F)

#Generate Plots
date_range <- seq.Date(as.Date(combined_data$Creation.Date[1]),
		 as.Date(combined_data$Creation.Date[nrow(combined_data)]),by='year')
registered_domains_over_time <- sapply(date_range,function(x){
	sum(as.Date(combined_data$Creation.Date) <= x,na.rm = T)
})
presidents_over_time <- sapply(date_range,function(x){
	y <- combined_data$president[which(combined_data$simple_date <= x)]
	y[length(y)]
})


data_for_plot <- data.frame(date = date_range,
							value = registered_domains_over_time,
							president = presidents_over_time)


# Create a dataframe for the presidents and their inauguration dates
presidents <- data.frame(
	president = c("Bill Clinton", "George W. Bush", "Barack Obama", "Donald Trump", "Joe Biden"),
	start_date = as.Date(c("1997-01-20", "2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20")),
	color = c('blue','red','blue','red','blue'),
	term = c(8,8,8,4,4)
)

# Create the plot
p <- ggplot(data_for_plot, aes(x = date, y = value)) +
	geom_line(color = "forestgreen", size = 3) +
	geom_point(size = 4) +
	labs(x = "Date", y = "Number of .Gov Websites") +
	theme_minimal() +
	theme(plot.margin = unit(c(3, 0.1, 0.1, 0.5), "cm"))

# Add vertical lines and annotate the president names
p <- p + 
	geom_vline(data = presidents, aes(xintercept = as.numeric(start_date)), 
			   color = presidents$color, linetype = "dashed", size = 1) +  # Vertical line for each president
	geom_text(data = presidents, aes(x = start_date, y = Inf, label = president), 
			  angle = 90, vjust = -0.5, hjust = 1.2, size = 3, color =  presidents$color)  # President name annotation


# Create the animation with transition_reveal and frame_along
anim <- p + 
	transition_reveal(date) +  # This makes the line form over time
	ease_aes('linear') +
	labs(title = '.Gov Websites Over Time {frame_along}\nSources:\n -Cybersecurity & Infrastructure Security Agency\n -WHOIS\n -Hisam Sabouni, PhD') +  # Update title dynamically
	transition_reveal(along = date)

# Save the GIF
anim_save("gov_domains_registered_over_time.gif", animation = anim)

others <- length(which(combined_data$simple_date <presidents$start_date[2]))
george_w_bush <- length(which(combined_data$simple_date >= presidents$start_date[2] & combined_data$simple_date < presidents$start_date[3]))
barack_obama <- length(which(combined_data$simple_date >= presidents$start_date[3] & combined_data$simple_date < presidents$start_date[4]))
donald_trump <- length(which(combined_data$simple_date >= presidents$start_date[4] & combined_data$simple_date < presidents$start_date[5]))
joe_biden <-  length(which(combined_data$simple_date >= presidents$start_date[5]))
presidents$Number.Domains <- c(NA,george_w_bush,barack_obama,donald_trump,joe_biden)
presidents$Number.Domains.Per.Year <- round(presidents$Number.Domains/presidents$term)

#Check Counts
(george_w_bush+barack_obama+donald_trump+joe_biden+others) == nrow(combined_data)
