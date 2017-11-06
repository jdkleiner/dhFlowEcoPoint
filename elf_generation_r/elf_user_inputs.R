rm(list = ls())  #clear variables
#library(httr);   #Load httr package
options(timeout=120); # set timeout to twice default level to avoid abort due to high traffic
 
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------

#----FOR RUNNING LOCALLY:
#fxn_locations <- "C:\\Users\\nrf46657\\Desktop\\point_analysis\\"          #Specify location of supporting function .R files
#save_directory <- "C:\\Users\\nrf46657\\Desktop\\point_analysis\\plots"  #Specify location for storing plot images locally

#----FOR RUNNING FROM SERVER:
fxn_locations <- "/var/www/html/files/dh/flow_ecology/d.dh/"
save_directory <- "/var/www/html/images/dh"


#retrieve rest token
source(paste(fxn_locations,"dh_rest_token.R", sep = ""));     #loads function used to generate rest session token
dh_rest_token (site, token)
token <- dh_rest_token(site, token)
#print(token)


#RETRIEVE ANALYSIS POINT PROPERTIES
apt_props <- paste(site,"point-analysis-props-export",sep="/")
apt_props <- read.csv(apt_props, header = TRUE, sep = ",")

y_metric <- as.character(apt_props[which(apt_props$propname == "analysis_pt_y"),"propcode"])
x_metric <- as.character(apt_props[which(apt_props$propname == "analysis_pt_x"),"propcode"])
pct_chg <- as.numeric(as.character(apt_props[which(apt_props$propname == "analysis_pt_pct_reduction"),"propcode"]))
ws_ftype <- as.character(apt_props[which(apt_props$propname == "analysis_pt_region"),"propcode"])
sampres <- as.character(apt_props[which(apt_props$propname == "analysis_pt_sampres"),"propcode"])
method <- as.character(apt_props[which(apt_props$propname == "analysis_pt_method"),"propcode"])

#RETRIEVE ANALYSIS POINT REGION HYDROCODE
apt_region <- paste(site,"elf-point-to-region/279034",ws_ftype,sep="/")
apt_region <- read.csv(apt_region, header = TRUE, sep = ",")
target_hydrocode <- apt_region$hydrocode

#a case arose where 2 containing huc8s were returned?? nhd_huc8_02080203 nhd_huc8_03010102 for 37.311254079827 -79.035694152117 why would that happen?
# forced to use the first huc8 
if (length(target_hydrocode) > 1) {target_hydrocode <- as.character(target_hydrocode[1])}

#note: add a 0 for the HUC6s and HUC10s or else the url doesn't work
if (ws_ftype == 'nhd_huc6') {
  if (length(target_hydrocode) <= 5) {
    target_hydrocode <- paste('0', target_hydrocode, sep='');
  }   
}
if (ws_ftype == 'nhd_huc10') {
  if (length(target_hydrocode) <= 9) {
    target_hydrocode <- paste('0', target_hydrocode, sep='');
  }   
}


if (method == "elf_quantreg") {
  quantreg <- "YES"
  pw_it <- "NO"
} else if (method == "elf_quantreg_pwit") {
  quantreg <- "NO"
  pw_it <- "YES"
}


#------------------------------------------------------------------------------------------------
#User inputs 
inputs <- list(
  site = site,
  offset_x_metric = 1,                      #Leave at 1 to start from begining of x_metric for-loop
  offset_y_metric = 1,                      #Leave at 1 to start from begining of y_metric for-loop
  offset_ws_ftype = 1,                      #Leave at 1 to start from begining of ws_ftype for-loop
  offset_hydrocode = 1,                     #Leave at 1 to start from begining of Watershed_Hydrocode for-loop
  pct_chg = pct_chg,                        #Percent decrease in flow for barplots (keep at 10 for now)
  save_directory = save_directory, 
  x_metric = x_metric,
  y_metric = y_metric,
  ws_ftype = ws_ftype,		  
  target_hydrocode = target_hydrocode,
  quantile = .9,                  #Specify the quantile to use for quantile regresion plots 
  xaxis_thresh = 15000,            #Leave at 15000 so all plots have idential axis limits 
  #analysis_timespan = '1990-2000',#used to subset data on date range 
  analysis_timespan = 'full',      #used to plot for entire timespan 
  send_to_rest = "NO",             #"YES" to set ELF stats as drupal submittal properties, "NO" otherwise
  station_agg = "max",             #Specify aggregation to only use the "max" NT value for each station or "all" NT values
  sampres = sampres,  
  quantreg = quantreg,
  pw_it = pw_it,
  glo = 1,   # PWIT Breakpoint lower guess (sqmi/cfs)
  ghi = 530, # PWIT Breakpoint upper guess (sqmi/cfs) - also used as DA breakpoint for elf_quantreg method
             # ghi values determined from ymax analyses,  q25 = 72 
             #                                            q50 = 205 
             #                                            q75 = 530
  token = token
) 

#------------------------------------------------------------------------------------------------
#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro

elf_retrieve_data (inputs) 

##############################################################################
