
#Loading Libraries

library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggmap)
library(ggthemes)
library(tidyr)
library(ggcorrplot)
library(corrplot)


# reading the file
Loan <-  read.csv("loan.csv", stringsAsFactors = F, na.strings = c("", "NA"))

names(Loan)
View(Loan)
######################################################
############Data Understanding & Preparation#########
######################################################

#It seems some columns have same/Blank/NA value in entire row. Which will not help 
#to identify trends. 
#Removing those colums 
Loan <- Filter(function(x) length(unique(x))>1,Loan)

#list of fields choosed for analysis
names(Loan)

#No Duplicate found. Takin member id as primayKey
anyDuplicated(Loan$member_id)
anyDuplicated(Loan$id)

###########################################################
##################Data Cleaning & Manipulation#############
###########################################################

#Filling values for blank fields 

Loan$emp_title[Loan$emp_title==""]="NotGiven"

#Formatting Date values
Loan$issue_d <- dmy(paste("01",Loan$issue_d,sep = ""))

Loan$issue_d_mth <- month(Loan$issue_d)
Loan$issue_d_yr <- year(Loan$issue_d)

Loan$earliest_cr_line <- dmy(paste("01",Loan$earliest_cr_line,sep = ""))

Loan$earliest_cr_line_mth <- months(Loan$earliest_cr_line)
Loan$earliest_cr_line_yr <- year(Loan$earliest_cr_line)



#Formatting Percentage values 
Loan$int_rate <- as.numeric(gsub("%","",Loan$int_rate))/100
Loan$revol_util <- as.numeric(gsub("%","",Loan$revol_util))/100



#######################################################################
#########################Data Analysis##############################
#######################################################################


#Loan Amount

loan_amnt_plot <- ggplot(Loan,aes(Loan$loan_amnt,fill=Loan$loan_status)) + geom_histogram(breaks=(seq(0,max(Loan$loan_amnt),by=5000)),position = "dodge") +
  labs(title="Histogram of Loan Amount") +
  labs(x ="Loan Amount",y="Count")+
  labs(fill="Loan Status")+
  theme_minimal()

loan_amnt_plot
ggsave("Loan_amount_hist.jpg",loan_amnt_plot)

#Term 

term_graph <- ggplot(Loan, aes(Loan$term,fill=Loan$loan_status)) + geom_histogram(stat = "count", position = "fill") + 
  labs(title="Histogram of Term with Loan Status") +
  labs(x ="Term Type",y="Count")+
  labs(fill="Loan Status")+
  theme_minimal()

term_graph

ggsave("term_fill_hist.jpg",term_graph)


#Interest Rate Calculation 
intrate_loanamnt_graph <- ggplot(Loan,aes(x=Loan$int_rate,y=Loan$loan_amnt,color=Loan$loan_status)) + 
  geom_point() + 
  labs(title="Interest Rate for Loan Amount") + 
  labs(x ="Interest Rate",y="Loan Amount") +
  labs(fill="Loan Status")+
  theme_minimal()

intrate_loanamnt_graph

ggsave("interest_rate_loanamnt.jpg",intrate_loanamnt_graph)




#Grade field analysis


#####################
## Loan Ratings Plot
#####################
sum(is.na(Loan$sub_grade)) #no NA
#Histogram plot for Grade data

grade_hist_graph <-ggplot(data=Loan, aes(Loan$grade)) + 
  geom_histogram(stat="count",col="red", fill="pink") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1, size=2.5) +
  labs(title="LC Grade of Borrowers") +
  labs(x="Grade of Borrower according to LC", y="Count") +
  theme_minimal()
grade_hist_graph

ggsave("grade_histogram.jpg",grade_hist_graph)


#Histogram Plot for grades filled with the 'Sub-Grades'
subgrade_hist_graph<-ggplot(data=Loan, aes(Loan$grade,fill=factor(Loan$sub_grade))) + 
  geom_histogram(stat="count",colour="white",lwd=0.2) + 
  geom_text(stat='count',aes(label=Loan$sub_grade),position = position_stack(vjust = 0.5), size=2) +
  labs(title="LC Grade of Borrowers") +
  labs(x="Grade of Borrower according to LC", y="Count") + 
  theme_minimal()

subgrade_hist_graph  

ggsave("subgrade_histogram.jp",subgrade_hist_graph)


#Histogram plot for sub grades
subgrade_borrowers <-ggplot(data=Loan, aes(Loan$sub_grade)) + 
  geom_histogram(stat="count",fill="pink",color="red") + 
  geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5), size=2) +
  labs(title="LC Sub-Grade of Borrowers") +
  labs(x="Grade of Borrower according to LC", y="Count") + 
  theme_minimal()

subgrade_borrowers
ggsave("subgrade_borrowers.jpg",subgrade_borrowers)

#Histogram Plot Sub grades. Each bar splited and filled with Loan Status 
subgrade_borrowers_fill<-ggplot(data=Loan, aes(Loan$sub_grade,fill=factor(Loan$loan_status))) + 
  geom_histogram(stat="count",colour="white",lwd=0.2) + 
  geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5), size=2.5) +
  labs(title="LC Sub-Grade of Borrowers") +
  labs(x="Grade of Borrower according to LC", y="Count") + 
  labs(fill="Loan Status") + 
  theme_minimal()
  

subgrade_borrowers_fill

ggsave("subgrade_borrowers_fill.jpg",subgrade_borrowers_fill)

#Histogram plot for subgrade wise date segmented with Loan status

subgrade_hist_fill <- ggplot(Loan,aes(Loan$sub_grade,fill=Loan$loan_status)) + 
  geom_histogram(stat = "count",position = "fill",colour="white",lwd=0.2) + 
  labs(title="Grade histogram") +
  labs(x ="Loan Grade",y="Filled Histogram")+
  labs(fill="Loan Status") + 
  theme_minimal()
subgrade_hist_fill
ggsave("subgradefill_hist.jpg",subgrade_hist_fill)
#With the Filled histogram Grades F & G have more posibilty for Charged Off when comparing to others
#For sub grades F5 , G3 G5 more attention is needed.


##############################
##Annual Income Representaion
##############################

#remove outliers from annual inc
#Histogram plot for Annual income 
sum(is.na(Loan$annual_inc)) #no NA

annualinc_hist<-ggplot(data=Loan, aes(Loan$annual_inc),na.rm=TRUE) + 
  geom_histogram(breaks=seq(0, tail(quantile(Loan$annual_inc, probs = seq(0.95, 0.99, by= 0.01)),1), by = 5000), 
                 col="red", 
                 fill="pink", 
                 alpha = .1) + 
  labs(title="Histogram for Annual Income") +
  labs(x="Income", y="Count")  +
  xlim(c(0,234999.4)) + 
  theme_minimal()

annualinc_hist

ggsave("annualinc_hist.jpg",annualinc_hist)

#######################
## Length of Employment
#######################
Loan$emp_length <- sub(".*<.*1.*", "0", Loan$emp_length)
Loan$emp_length <- sub(".*?([0-9]{1,2}).*", "\\1", Loan$emp_length)

#Histogram plot for 'emp_length' field
#X axis emp_lenght categorical data
#Y axis Count of emp_length

emp_length_histplot<-ggplot(data=Loan, aes(Loan$emp_length),na.rm=TRUE) + 
  geom_histogram(stat="count", 
                 col="red", 
                 fill="pink", 
                 alpha = .1) + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=2) +
  #stat_bin(geom="text",hjust=-1.65,angle=90, size=2,aes(label=..count..)) + 
  labs(title="Years of Employment of Borrowers Granted Loan") +
  labs(x="Years", y="Count")
emp_length_histplot
ggsave("emp_length_histplot.jpg",emp_length_histplot)


#Emp_length histogram segmented with Loan Status 
#X axis emp_length
#Y axis count
#Fill with Loan_status

emplen_histfill<-ggplot(data=Loan, aes(Loan$emp_length,fill=factor(Loan$loan_status)),na.rm=TRUE) + 
  geom_histogram(stat="count", 
                 col="blue", 
                 #                fill="yellow", 
                 alpha = .5) + 
  geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5), size=2) +
  #stat_bin(geom="text",hjust=-1.65,angle=90, size=2,aes(label=..count..)) + 
  labs(title="Years of Employment of Borrowers Granted Loan") +
  labs(x="Years", y="Count") + 
  labs(fill="Loan Status")+
  theme_minimal()

emplen_histfill

ggsave("emplen_histfill.jpg",emplen_histfill)

#load us map data
#Circle value for world Map
circle_scale_amt=0.00019

loan_data<-Loan %>% 
  select(annual_inc,addr_state) %>%
  group_by(addr_state) %>%
  summarise(Avg=median(annual_inc)) 

states <- data.frame(state.name, state.abb, state.center)
loan_data<-merge(loan_data, states, by.x = "addr_state",by.y="state.abb")

#Loading US map Data
map_data<-get_map(location='united states', zoom=4, maptype = "roadmap",
                  source='google',color='color')
map=ggmap(map_data)

#point plot in Geo location for annual income
annual_geo_plot <-map+geom_point(aes(x=x, y=y), 
               data=loan_data,alpha=0.4,col="orange",size=loan_data$Avg*circle_scale_amt) + 
  labs(title="Annual income Range")+
  labs(x="",y="") +
  scale_size_continuous(range=range(loan_data$Avg))

annual_geo_plot

ggsave("annualInc.jpg",annual_geo_plot)


#Getting locations related to loan status 
status_data <- Loan %>%
  select(loan_status,addr_state) %>%
  group_by(addr_state,loan_status) %>%
  summarise(n=n())

status_data_map <- merge(status_data,states,by.x = "addr_state",by.y = "state.abb")
status_data_map_chargedoff <- filter(status_data_map,status_data_map$loan_status=="Charged Off")
status_data_map_fullypaid <- filter(status_data_map,status_data_map$loan_status=="Fully Paid")

#Geo point plot for loan_status category 
loanstatus_geo_chargedoff <- map + 
  geom_jitter(aes(x=x,y=y, color=loan_status,size=sqrt(n)),data=status_data_map_chargedoff) + 
  labs(title="Charged Off count by Size")

loanstatus_geo_chargedoff
loanstatus_geo_fullypaid <- map + 
  geom_jitter(aes(x=x,y=y, color=loan_status,size=sqrt(n)),data=status_data_map_fullypaid,color="blue") +
  labs(title="Fully Paid by Size")

loanstatus_geo_fullypaid
ggsave("loanstatus_geo.jpg",loanstatus_geo)

#Histogram Graph for Address

address_hist <- ggplot(Loan,aes(Loan$addr_state,fill=Loan$loan_status)) + geom_histogram(stat = "count") +
  labs(title="Address histogram") + 
  labs(x="State Code",y="Count ") + 
  labs(fill="Loan Status")+
  theme_minimal()


address_hist

ggsave("address_hist.jpg",address_hist)

######################
# Debt to Income Ratio
######################
sum(is.na(Loan$dti)) #no NA

#Histogram plot for debt to income value 
dti_hist<-ggplot(aes(x=dti), data=Loan) + 
  geom_histogram(fill='pink', color='black',alpha=0.1) +
  labs(title=expression(paste("Debt to Income Ratio")), x='Debt / Income')
dti_hist
ggsave("dti_hist.jpg",dti_hist)

#Income Group that is most Likely to Pay Off the Loan
#Spliting the income range
ir<-as.character(round(quantile(Loan$annual_inc, probs = seq(0, 1, by= 0.1)),0))
income_range<-c( paste("$",ir[1],"- $",ir[2]),
                 paste("$",ir[2],"- $",ir[3]),
                 paste("$",ir[3],"- $",ir[4]),
                 paste("$",ir[4],"- $",ir[5]),
                 paste("$",ir[5],"- $",ir[6]),
                 paste("$",ir[6],"- $",ir[7]),
                 paste("$",ir[7],"- $",ir[8]),
                 paste("$",ir[8],"- $",ir[9]),
                 paste("$",ir[9],"- $",ir[10]),
                 paste("$",ir[10],"- $",ir[11])
)


Loan$annual_inc_range<- cut(Loan$annual_inc,
                            breaks=quantile(Loan$annual_inc, probs = seq(0, 1, by= 0.1)),
                            include.lowest = TRUE, right = TRUE,
                            labels=income_range)

# sum(is.na(Loan$dti_joint)) shows NAs needs to be cleaned

#dti density for each annual income range
#X axis Debt to Income Ratio 
#Y axis Density
#Facet Income range
dti_density_facet<-ggplot(aes(x=dti), data=Loan) +
  geom_density(aes(color = annual_inc_range, 
                   fill  = annual_inc_range),alpha = 0.3) +
  facet_wrap(~annual_inc_range, ncol=2, scales = "free") +
  labs(title=expression(paste("Income Group that is most Likely to Pay Off the Loan")),
       x='Debt To Income Ratio',y="Density")
dti_density_facet
ggsave("dti_density_facet.jpg",dti_density_facet)
######################
# Purpose of Loan
######################
sum(is.na(Loan$purpose))
#Histogram graph for purpose data
#X axis Purpose (Loan purpose category)
#Y axis Count 

purpose_hist<-ggplot(data=Loan,aes(x=purpose)) + 
  #geom_bar(stat="identity")+
  geom_histogram(stat="count",fill='Orange', color='black') +
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=2)+
  labs(title=expression(paste("Purpose of Loan")), x='Purpose',y="Count")+
  theme_minimal()

purpose_hist
ggsave("purpose_hist.jpg",purpose_hist)

#Histogram graph for purpose data
#X axis Purpose (Loan purpose category)
#Y axis Count 
#Fill Loan status
purpose_hist_statusfill<-ggplot(data=Loan,aes(x=Loan$purpose,fill=factor(Loan$loan_status))) + 
  geom_histogram(stat="count",colour="white",lwd=0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title=expression(paste("Purpose of Loan Which is most Likely to Paid Off")), x='Purpose',y="Count")

purpose_hist_statusfill
ggsave("purpose_hist_statusfill.jpg",purpose_hist_statusfill)


#Date data anaysis

#Histogram graph for issue date
#X axis Issue_date month
#Y axis count
issue_date_mth_graph <- ggplot(Loan,aes(Loan$issue_d_mth,fill=Loan$loan_status)) + 
  geom_histogram(stat = "count")+
  labs(title="Issue Date (Month) Histogram") + 
  labs(x ="Month",y="Count")+
  theme_minimal()

issue_date_mth_graph
ggsave("issue_date_mth_graph.jpg",issue_date_mth_graph)

#Histogram graph for issue date
#X axis Issue_date month
#Y axis count

issue_date_yr_graph <- ggplot(Loan,aes(Loan$issue_d_yr,fill=Loan$loan_status)) + 
  geom_histogram(stat = "count")+
  labs(title="Issue Date (Year) Histogram") + 
  labs(x ="Year",y="Count")+
  theme_minimal()

issue_date_yr_graph
ggsave("issue_date_yr_graph.jpg",issue_date_yr_graph)


#Earliest Credit line 

#Funtion to change 2 digt year to 4 digit with 19th centure 
#i.e 2050 to 1950
refactor <- function(x){
  if(x>2017){
    x <- x-100
  }
  return(x)
}
#Handing year to differentiate 1950 vs 2050 for the 50 value 
Loan$earliest_cr_line_yr <- sapply(Loan$earliest_cr_line_yr, refactor)

#Histogram plot for earliest credit 
#X axis earliest creidt year
#Y axis count 
#Fill Loan Status

earliest_credit_hist <- ggplot(Loan,aes(Loan$earliest_cr_line_yr,fill=Loan$loan_status)) + geom_histogram(stat = "count")  +
  labs(title="Earliest Crd. year Hist")+
  labs(x="Year",y="Count")+
  labs(fill="Loan Status")+
  theme_minimal()

earliest_credit_hist
ggsave("earliest_credit_hist.jpg",earliest_credit_hist)


################################
#To find who is going to default 
#################################

lp<-Loan %>% 
  select(loan_amnt,term,int_rate,grade,home_ownership,
         annual_inc,verification_status,loan_status,dti,
         pub_rec)

lp$term <- as.numeric(sub(".*?([0-9]{1,2}).*", "\\1", lp$term))
lp$int_rate <- as.numeric(sub(".*?([0-9]{1,2}\\.[0-9]{1,2}).*", "\\1", lp$int_rate))

lp$home_ownership <-as.numeric(factor(lp$home_ownership,levels=unique(lp$home_ownership)))
lp$grade <-as.numeric(factor(lp$grade,levels=unique(lp$grade)))
lp$verification_status <-as.numeric(factor(lp$verification_status,levels=unique(lp$verification_status)))
lp$loan_status <-as.numeric(factor(lp$loan_status,levels=unique(lp$loan_status)))

correl_pmat<-cor(lp,use="pairwise.complete.obs")

#correaltion graph

corr_plot <- ggcorrplot(correl_pmat, hc.order = TRUE,
           type = "lower",lab = TRUE,
           ggtheme = ggplot2::theme_classic,
           colors = c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corr_plot
ggsave("corr_plot.jpg",corr_plot)


