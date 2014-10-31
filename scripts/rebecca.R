#Rebecca Project 10/30/14

#setwd
setwd("C:/Users/Jonathan/Desktop/Stoke Informatics/Clients/Personal/data")

#Utility Scale Resource Analysis
# #read in DG data
# dg <- read.csv("eia/2012_dg.csv", skip=1, stringsAsFactors=FALSE)
#filter data for states: AZ, CA, CO, ID, MT, NM, NV, OR, UT, WA, WY
states <- c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY")
# dg <- dg[which(dg$state %in% states), ]

# #convert necessary data fields to numeric
# names(dg)
# head(dg)
# dg$total_mw <- as.numeric(dg$total_mw)
# dg$wind_mw <- as.numeric(dg$wind_mw)
# 
# #sum wind capacity across all states. TEPPC sum is 31781.43MW
# sum(dg$wind_mw, na.rm=TRUE) # EIA only 2.9MW
# dg$wind_mw
# #EIA wind data is incomplete...
# #what about solar across all states? TEPPC is 6358.9MW
# class(dg$pv_mw)
# dg$pv_mw <- as.numeric(dg$pv_mw)
# sum(dg$pv_mw, na.rm=TRUE) # EIA only 101.7MW

#read in TEPPC data
teppc <- read.csv("TEPPC/TEPPCdata.csv", header=TRUE, stringsAsFactors=FALSE)
#filter for states
names(teppc)
teppc <- teppc[which(teppc$State %in% states), ]


#filter for 2012 resources
#Don't filter Certainty.Flag for 2012 resources because of conflict between Class.Code
# teppc <- teppc[teppc$Certainty.Flag == "Certain", ]
unique(teppc$WECC.Class.Code)
resources2012 <- "0 (Existing)"
teppc2012 <- teppc[teppc$WECC.Class.Code %in% resources2012, ]

#create utility wind dataframe
unique(teppc2012$Primary.Fuel)
wind2012 <- teppc2012[teppc2012$Primary.Fuel == "Wind", ]
#create wind summary for 2012 and 2024
library(plyr)
windsummary2012 <- ddply(wind2012, .(State), summarize,
                         wind_mw_2012=sum(Nameplate..MW., na.rm=TRUE))

#create utility solar dataframe
solar2012 <- teppc2012[teppc2012$Primary.Fuel == "Sun", ]
#create utility solar summary for 2012 and 2024
solarsummary2012 <- ddply(solar2012, .(State), summarize,
                          solar_mw_2012=sum(Nameplate..MW., na.rm=TRUE))

#2024 data and creating 2012 MWh figures. 

#now filter based on Class Codes
resources2024 <- c("0 (Existing)", "1 (Under Construction)", "2 (Pre-Const Reg Approval-Review)", "3 (Future-Planned)")
teppc2024 <- teppc[teppc$WECC.Class.Code %in% resources2024, ]



#Separating out resource types
#wind
wind2024 <- teppc2024[teppc2024$Primary.Fuel == "Wind", ]
#wind 2024 summary table
windsummary2024 <- ddply(wind2024, .(State), summarize,
                     wind_mw_2024=sum(Nameplate..MW., na.rm=TRUE),
                     wind_mwh_2024=sum(X2024_140725.Energy..MWh., na.rm=TRUE))

#solar
solar2024 <- teppc2024[teppc2024$Primary.Fuel == "Sun", ]
#solar 2024 summary table
solarsummary2024 <- ddply(solar2024, .(State), summarize,
                          solar_mw_2024=sum(Nameplate..MW., na.rm=TRUE),
                          solar_mwh_2024=sum(X2024_140725.Energy..MWh., na.rm=TRUE)) 

#Capacity Factor Calcs
#only resources greater than 2MW
wind2024 <- wind2024[wind2024$Nameplate..MW. >= 2, ]
solar2024 <- solar2024[solar2024$Nameplate..MW. >= 2, ]
#then only resources with more than 17,520 MWh generation
wind2024 <- wind2024[wind2024$X2024_140725.Energy..MWh. >= 3000, ]
solar2024 <- solar2024[solar2024$X2024_140725.Energy..MWh. >= 3000, ]

#wind aggregate capacity factor
windMW <- sum(wind2024$Nameplate..MW., na.rm=T)
windMWh <- sum(wind2024$X2024_140725.Energy..MWh., na.rm=T)
windCF <- (windMWh/(8760*windMW)) 
  
#solar aggregate capacity factor
solarMW <- sum(solar2024$Nameplate..MW., na.rm=T)
solarMWh <- sum(solar2024$X2024_140725.Energy..MWh., na.rm=T)
solarCF <- (solarMWh/(8760*solarMW))-.1

#back to 2012 summaries...using capacity factors to add in 2012 MWh estimates
#wind
windsummary2012$wind_mwh_2012 <- windsummary2012$wind_mw_2012*8760*windCF
#solar
solarsummary2012$solar_mwh_2012 <- solarsummary2012$solar_mw_2012*8760*solarCF

#merging 2012 and 2024 data for wind and solar and writing to .xlsx file
library(xlsx)
if(!file.exists("finaldata")){
  dir.create("finaldata")
  }
#wind
windsummary <- merge(windsummary2012, windsummary2024, by="State")
#writing to xlsx file in "finaldata" folder
write.xlsx(windsummary, "./finaldata/utilitywind2012_2024.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)

#solar
solarsummary <- merge(solarsummary2012, solarsummary2024, by="State", all.y=TRUE)
#writing to xlsx file in "finaldata" folder
write.xlsx(solarsummary, "./finaldata/utilitysolar2012_2024.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)


#Distributed Generation data
#read in EIA net metering data
nm <- read.csv("eia/2012netmetering.csv", skip=2, stringsAsFactors=FALSE)
nm <- nm[nm$State %in% states, ]

nm$total_pv_mw <- as.numeric(nm$total_pv_mw)

#Summary with total PV MW and MWh, 
DGsummary2012 <- ddply(nm, .(State), summarize, 
                 pv_MW=sum(total_pv_mw, na.rm=TRUE))

#creating 2012MWh with capacity factor from solarCF
DGsummary2012$pv_MWh <- DGsummary2012$pv_MW*8760*solarCF







# #read in openPV data from NRELs open PV Project to compare to EIA data
# openpv <- read.csv("openPVdata.csv", header=T, stringsAsFactors=FALSE)
# #filter desired states
# openpv <- openpv[openpv$State %in% states, ]
# #convert Date.Installed to POSIXct
# openpv$Date.Installed <- as.POSIXct(openpv$Date.Installed, format ='%m/%d/%Y')
# #exclude 2013 and 2014 data
# deleteyears <- c("2013", "2014")
# openpv <- openpv[!(strftime(openpv$Date.Installed, "%Y") %in% deleteyears), ]
# #create capacity summary table
# openpvsummary <- ddply(openpv, .(State), summarize,
#                        openpv_MW=sum(Size..kW.DC., na.rm=TRUE)/1000)


#GHG Emissions
#need total GHG emissions for each state. 
emissions <- read.csv("emissions/WECC2012emissions.csv", header=T, stringsAsFactors=FALSE)



###fuel mix charts... use teppc2012 and teppc2024 dataframes###
#group similar fuel types
#coal, biomass, natural gas, put petrolium into other category, storage, hydro, DR, 
#purchased steam in other, nuclear, gas-propane and gas-other in natural gas, ignore Electricity!

#one graphic with stacked bar chart by state showing fuel mix MW for 2012 and 2024
#2012 Primary.Fuel MW by State#
fuelmix2012 <- ddply(teppc2012, .(State, Primary.Fuel), summarize,
                  MW_2012=sum(Nameplate..MW., na.rm=T))

#now put into broader categories
# unique(fuelmix2012$Primary.Fuel)
fuelmix2012$MWh_2012=NA
fuelmix2012$Fuel_Cat <- NA
#coal
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Coal-Bit","Coal-Sub","Coal-Lig","Coal-Other")] <- "Coal"
#biomass
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Biomass-LandfillGas","Biomass-Wood-Solid","Biomass-Agricultural","Biomass-Muni Solid","Biomass-Other","Biomass-Sludge Waste","Biomass-Wood-Liquid","Biomass-Black Liquor")] <- "Biomass"
#natural gas
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Gas-Natural Gas","Gas-Other","Gas-Propane","Natural Gas")] <- "Natural Gas"
#Other
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Oil-Distillate Fuel","Other","Petroleum Coke","Purchased Steam","N/A","Various")] <- "Other"
#Nuclear
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Nuclear")] <- "Nuclear"
#Geothermal
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Geothermal")] <- "Geothermal"
#Hydro
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Water","Water-Electricity")] <- "Hydro"
#Solar
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Sun")] <- "Solar"
#Wind
fuelmix2012$Fuel_Cat[fuelmix2012$Primary.Fuel %in% c("Wind")] <- "Wind"

#delete any "Electricity" Primary.Fuel rows
if(length(which(fuelmix2012$Primary.Fuel=="Electricity")) > 0){
  elecdelete <- which(fuelmix2012$Primary.Fuel=="Electricity")
  fuelmix2012 <- fuelmix2012[-elecdelete, ]
}

#which ones are still NA? Ask Rebecca how she wants them coded.
unique(fuelmix2012[which(is.na(fuelmix2012$Fuel_Cat)), 2])

#2024 Fuel Mix (MW and MWh) by State
fuelmix2024 <- ddply(teppc2024, .(State, Primary.Fuel), summarize,
                  MW_2024=sum(Nameplate..MW., na.rm=T),
                  MWh_2024=sum(X2024_140725.Energy..MWh., na.rm=T))

#now put into broader catergories
fuelmix2024$Fuel_Cat <- NA
# unique(fuelmix2024$Primary.Fuel)
#coal
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Coal-Bit","Coal-Sub","Coal-Lig","Coal-Other")] <- "Coal"
#biomass
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Biomass-LandfillGas","Biomass-Wood-Solid","Biomass-Agricultural","Biomass-Muni Solid","Biomass-Other","Biomass-Sludge Waste","Biomass-Wood-Liquid","Biomass-Black Liquor")] <- "Biomass"
#natural gas
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Gas-Natural Gas","Gas-Other","Gas-Propane","Natural Gas")] <- "Natural Gas"
#Other
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Oil-Distillate Fuel","Other","Petroleum Coke","Purchased Steam","N/A","Various")] <- "Other"
#Nuclear
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Nuclear")] <- "Nuclear"
#Geothermal
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Geothermal")] <- "Geothermal"
#Hydro
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Water","Water-Electricity")] <- "Hydro"
#Solar
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Sun")] <- "Solar"
#Wind
fuelmix2024$Fuel_Cat[fuelmix2024$Primary.Fuel %in% c("Wind")] <- "Wind"

#delete any "Electricity" Primary.Fuel rows
if(length(which(fuelmix2024$Primary.Fuel=="Electricity")) > 0){
  elecdelete <- which(fuelmix2024$Primary.Fuel=="Electricity")
  fuelmix2024 <- fuelmix2024[-elecdelete, ]
}

#which ones are still NA? Ask Rebecca how she wants them coded.
unique(fuelmix2024[which(is.na(fuelmix2024$Fuel_Cat)), 2])

#total fuel mix datasets
totalfuelmix2012 <- ddply(fuelmix2012, .(Fuel_Cat), summarize,
                          MW_2012=sum(MW_2012, na.rm=T),
                          MWh_2012=sum(MWh_2012, na.rm=F))

totalfuelmix2024 <- ddply(fuelmix2024, .(Fuel_Cat), summarize,
                          MW_2024=sum(MW_2024, na.rm=T),
                          MWh_2024=sum(MWh_2024, na.rm=F))

#add in capacity factor values to totalfuelmix2024
totalfuelmix2024$CF_2024 <- totalfuelmix2024$MWh_2024/(8760*totalfuelmix2024$MW_2024)




#prepare datasets and give to Rebecca as .xlsx



