# Script to remove rows from pad data that duplicate data
# TODO: Check with Vincent etc whether these are correct decisions

# Any studies in the multiple.timepoint.studies table below
# that have more than 2 arms or 2 timepoints are suspect

# For patency there are 50 studies
#Trial.ID n.timepoints n.arms time.per.arm
#1         5            2      2     1.000000
#2        21            6      2     3.000000
#3        35            2      2     1.000000
#4        67            2      2     1.000000
#5        99            4      2     2.000000 # Correct
#6       114            4      2     2.000000 # Correct. Uses unclear data at 6m and time-to-event at 12m
#7       208            4      2     2.000000 # KM data
#8       218            2      2     1.000000
#9       262            6      2     3.000000 # KM data # Correct, only uses time-to-event at 2 months
#10      281            4      2     2.000000
#11      385            6      2     3.000000
#12      493            4      2     2.000000 # KM data
#13      519            4      2     2.000000 # Checked # KM data
#14      539            2      2     1.000000  
#15      553            2      2     1.000000 
#16      634            4      2     2.000000 
#17      646            6      2     3.000000 
#18      721            8      2     4.000000 # Checked # KM data
#19      724            5      2     2.500000 # Checked. DES/PCS has 3 timepoints, PTA only 2.
#20      751            6      2     3.000000
#21      770            4      2     2.000000
#22      774            6      2     3.000000 
#23      884            2      2     1.000000 
#24     1075            8      2     4.000000  # Correct. Uses KM at 6m so ITT discarded
#25     1093            8      2     4.000000  # Checked
#26     1170            6      2          3.0
#27     1175            4      4          1.0 # Correct. Four arms are distinct as they are +drug version
#28     1214            2      2          1.0 
#29     1311            6      2          3.0 # KM data
#30     1320            6      2          3.0
#31     1346            4      2          2.0
#32     1466            2      2          1.0 
#33     1519            2      2          1.0
#34     1546            6      2          3.0
#35     1577            3      3          1.0 # Two arms of AT with 2.7mm and 4.0mm
#36     1680            9      3          3.0 # Three timepoints (3, 6, 12m) and three arms (PTA and two LA)
#37     1700            2      2          1.0 
#38     3058            3      3          1.0
#39     4197            4      2          2.0
#40     4448            4      2          2.0 # KM data
#41     4657            6      3          2.0 # Three arms in total as two MS arms slightly different ####
#42     6434            6      2          3.0 
#43     5715            4      2          2.0
#44     6431            4      2          2.0
#45     6447           10      2          5.0 # Checked, really are 5 timepoints
#46     6590            4      2          2.0 
#47     6674            4      2          2.0 
#48     6695            2      2          1.0 
#49     6701            4      2          2.0 
#50     6736            8      2          4.0 # Checked, really are 4 timepoints

# For restenosis there are 44 studies
#     Trial.ID n.timepoints n.arms time.per.arm
#1        21            4      2     2.000000
#2        44            3      3     1.000000
#3        58            2      2     1.000000
#4       114            2      2     1.000000
#5       208            2      2     1.000000
#6       493            4      2     2.000000
#7       496            2      2     1.000000
#8       514            2      2     1.000000
#9       519            4      2     2.000000
#10      553            2      2     1.000000
#11      575            4      4     1.000000
#12      646            4      2     2.000000 # Checked two arms PTA and MS and 'n' rather than 'rate' is used
#13      653            2      2     1.000000
#14      751            6      2     3.000000 # Correct, two arms three timepoints and the <=36 months data not used 
#15      855            2      2     1.000000
#16      859            6      2     3.000000 
#17      884            2      2     1.000000
#18      892            2      2     1.000000 # Three arms DCB, DCM, PTA but DCM only at 6 months. Choose to use only 1 year data. See justification below for Tepe 2008
#19      947            2      2     1.000000
#20      984            2      2     1.000000
#21     1016            8      4     2.000000  # KM data  # Checked and ignore Restenosis (Angio) at 6 month as DUS for other timepoints 
#22     1055            8      2     4.000000 # Checked
#23     1075            2      2     1.000000
#24     1077            4      2     2.000000 
#25     1081            4      4     1.000000
#26     1093            2      2     1.000000
#27     1114           10      2     5.000000 # Checked, really reports 5 timepoints
#28     1160            6      2     3.000000 # Checked
#29     1172            2      2     1.000000
#30     1175            2      2     1.000000
#31     1202            2      2     1.000000
#32     1346            2      2     1.000000
#33     1379            2      2     1.000000
#34     1683            3      3     1.000000
#35     4197            2      2     1.000000
#36     4448            2      2     1.000000
#37     4657            6      6     1.000000
#38     6434            2      2     1.000000
#39     5712            2      2     1.000000
#40     5719            3      3     1.000000
#41     6437            6      6     1.000000
#42     6443            4      4     1.000000
#43     6455            2      2     1.000000
#44     6674            4      2     2.000000 # Checked

# Patency corrections
# ID 281 Rosenfield 2015. Duplicate 1 yr data. Arbitrarily remove those with Source "5715 - p37"
# as I think T2 (the other studies) refers to table 2 of main article. 5715 likely typo as alt ID is 5714
pad.data <- pad.data[pad.data[, "Trial.ID"] != "281" | pad.data[, "Source"] != "5715 - p37", ]
# ID 67 Krishnan 2017. Reports 410d data per protocol as well as 365d KM time-to-event data. The latter are preferred.
pad.data <- pad.data[pad.data[, "Trial.ID"] != "67" | pad.data[, "Analysis"] != "Per Protocol", ]
# ID 99 de Boer 2017 reports 1 year time-to-event data both ITT and per protocol. We choose ITT when available. 
pad.data <- pad.data[pad.data[, "Trial.ID"] != "99" | pad.data[, "Comment.1..Stat.."] != "p=0.273 by log-rank\r\nPP population\r\nKM (fig2B)", ]
# ID724 Dake 2011. Use 549 Fig 4 as "Acute PTA failure counted as a loss of patency". Don't use 724 - Fig 3
pad.data <- pad.data[pad.data[, "Trial.ID"] != "724" | pad.data[, "Source"] != "724 - Fig 3", ]
# ID1170 Gracher 2004. Reports Prip, Clinical PriP, and Hemodynamic PriP. Use PriP. 
pad.data <- pad.data[pad.data[, "Trial.ID"] != "1170" | pad.data[, "Category"] != "Patency" | pad.data[, "Outcome"] == "PriP", ]
# ID 1346 Minar 2000. Use PriP rather than clinical patency for hte 1yr outcome
pad.data <- pad.data[pad.data[, "Trial.ID"] != "1346" | pad.data[, "Category"] != "Patency" | pad.data[, "Outcome"] == "PriP", ]
# ID 1680 Lammer 1992. Use PriP (Angio) from Fig 3rather than PriP (based on ABI) or PriP (clinical) from Figs 1/2
#pad.data <- pad.data[pad.data[, "Trial.ID"] != "1680" | pad.data[, "Category"] != "Patency" | pad.data[, "Outcome"] == "PriP (Angio)", ]
# ID 385 Scheinhert 2014 assume all patients at year 1 available at year 2 and use percentage at year 2 to calculate events
# Remove 6m data. Decided against changing the model as only one point doesn't justify a shared parameter model. Instead assume the N measured at year 1 (i.e. 45 on DCB and 42 on PTA) are available at year 2 but use proportion at year 2 (i.e. 24/32 on DCB and 17/43 on PTA) to calculate number of events (i.e. 45*24/32 on DCB and 42*17/43 on PTA). 
pad.data[pad.data[, "Trial.ID"] == "385" & pad.data[, "Category"] == "Patency" & pad.data[, "Time.point"] == "2yrs" & pad.data[, "Class"] == "DCB", "Measured.N"] <- "45"
pad.data[pad.data[, "Trial.ID"] == "385" & pad.data[, "Category"] == "Patency" & pad.data[, "Time.point"] == "2yrs" & pad.data[, "Class"] == "PTA", "Measured.N"] <- "42"
pad.data[pad.data[, "Trial.ID"] == "385" & pad.data[, "Category"] == "Patency" & pad.data[, "Time.point"] == "2yrs" & pad.data[, "Class"] == "DCB", "Observed.Events.or.rate..N.or..."] <- 45 * (24/32)
pad.data[pad.data[, "Trial.ID"] == "385" & pad.data[, "Category"] == "Patency" & pad.data[, "Time.point"] == "2yrs" & pad.data[, "Class"] == "PTA", "Observed.Events.or.rate..N.or..."] <- 42 * (17/43)
pad.data <- pad.data[pad.data[, "Trial.ID"] != "385" | pad.data[, "Category"] != "Patency" | pad.data[, "Time.point"] != "6m", ]


# Restenonsis corrections
# ID 646 Brancaccio 2012. Use n units restenosis rather than rate units for all timepoints
pad.data <- pad.data[pad.data[, "Trial.ID"] != "646" | pad.data[, "Category"] != "Restenosis" | pad.data[, "Unit..measurement."] == "n", ]
# ID 1016 Schillinger 2006, use Restenosis (DUS) for all timepoints as Restenosis (Angio) is only reported at one timepoint
pad.data <- pad.data[pad.data[, "Trial.ID"] != "1016" | pad.data[, "Category"] != "Restenosis" | pad.data[, "Outcome"] == "Restenosis (DUS)", ]
# ID 892 Tepe 2008. For restenosis (no patency data). Only use the 1 year data
# All results are in the 5 year paper. The 1-year figures are fine. The 5-year figures on their own are fine. However, for the piecewise model, we cannot use the 5-year figures as we cannot obtain the number of events / non-events that occurred between the 1-year and 5-year follow-up periods.
pad.data <- pad.data[pad.data[, "Trial.ID"] != "892" | pad.data[, "Category"] != "Restenosis" | pad.data[, "Time.point"] == "1yr", ]
# ID 1160 Krueger 2004 restenosis need to change PTA arm denominator from 15 to 13 at 12m and 24m as 2 patients were retreated. 
# The BT arm is fine. 0/15 at 6mths, 0/15 between 6-12 mths, and 2/15 between 12-24 mths. The Control arm is odd though. 7/15 at 6 months, but only 5/15 at 12m. Reading the paper it's explained on p.550 3rd column, 2nd para that 2 of control patients were re-treated (and hence the drop from 7 to 5). So I think the data are 7/15 on 0-6 months. 0/13 between 6mths - 12mths, and 0/13 between 12mths - 24 mths
# One of the timepoints was NA so use !is.na to remove it
pad.data[pad.data[, "Trial.ID"] == "1160" & !is.na(pad.data[, "Time.point"]) & pad.data[, "Category"] == "Restenosis" & (pad.data[, "Time.point"] == "1yr" | pad.data[, "Time.point"] == "2yrs") & pad.data[, "Class"] == "PTA", "Measured.N"] <- c("13", "13")



