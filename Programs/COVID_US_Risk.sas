*USA COVID-19 Risk Assessment
*
*Assesses risk of locations in US by county. Risk is based on availability of hospital beds for COVID-19 patients at given date.
*Ratio of hospital beds are used for US risk due to availability of more detailed information per location (evolution of analysis method as more data becomes available).
*Risk is output via a categorization system with a risk factor, discount value and multiplier similar to OUS risk assessment.
*CSV output of risk factor by US county will be cross analyized with study locations;

%let data_date = 2020_05_04;

PROC IMPORT OUT= WORK.beds 
            DATAFILE= "/lrlhps/users/c269016/Reference_hospitalization_all_locs.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
	 GUESSINGROWS=9955;
RUN;

data beds;
   set beds;
   format date_reported date9.;
   *location = location_name;
   *drop location_name;
   date_reported = date;
*put(date, MMDDYY10.);
run;

proc sort data = beds;
   by location date_reported;
run;

data beds2;
   set beds;
   if bedover_upper > 0 then beds_available = allbed_upper - bedover_upper; 
   if icuover_upper > 0 then icubeds_available = ICUbed_upper - icuover_upper; 
run;

proc sort data = beds2 out=beds_a(keep=location beds_available) nodupkey;
   by location;
   where beds_available ne .;
run;

proc sort data = beds2 out=icubeds_a(keep=location icubeds_available) nodupkey;
   by location;
   where icubeds_available ne .;
run;

data beds3;
   merge beds2(keep = location date_reported allbed_mean ICUbed_mean) beds_a(in=in2) icubeds_a(in=in3);
   by location;
   *if not in2 then bedrisk = 0;
   if not in3 then icubedrisk = 0;

      if in2 then beds_ratio = allbed_mean/beds_available;
	  else beds_ratio = .;
	  if beds_ratio < 0.8 and date_reported > '30APR2020'D then bedrisk = 0;
	  if beds_ratio < 0.8 and date_reported <= '30APR2020'D then bedrisk = 1;
	  if beds_ratio < 0.9 and beds_ratio >= 0.8 then bedrisk = 2;
	  if beds_ratio < 1.0 and beds_ratio >= 0.9 then bedrisk = 3;
	  if beds_ratio >= 1.0 then bedrisk = 4;
      
   if in3 then 
   do;
      icubeds_ratio = icubed_mean/icubeds_available;
	  if icubeds_ratio < 0.9 then icubedrisk = 0;
	  else if icubeds_ratio < 1.0 then icubedrisk = 1;
           else if icubeds_ratio >= 1.0 then icubedrisk = 2;
   end;
   risk = bedrisk + icubedrisk;
   if risk > 2 then risk = 2;
run;

data beds4;
   set beds3;
   retain bedrisk_flag2 bedrisk_stopdate2 bedrisk_flag3 bedrisk_stopdate3 bedrisk_flag4 bedrisk_stopdate4;
   if first.location then 
   do;
	  bedrisk_stopdate2 = .;
      bedrisk_stopdate3 = .;
	  bedrisk_stopdate4 = .;
   end;

   bedrisk2 = bedrisk;

   if lag(bedrisk)=2 and bedrisk<2 then
   do;
      bedrisk_flag2 = 1;
	  bedrisk_stopdate2 = date_reported;
   end;
   bedrisk_lag2 = date_reported-bedrisk_stopdate2;
   if bedrisk_lag2 >=0 and bedrisk_lag2 <= 21 then bedrisk2=2;

   if lag(bedrisk)=3 and bedrisk<3 then
   do;
      bedrisk_flag3 = 1;
	  bedrisk_stopdate3 = date_reported;
   end;
   bedrisk_lag3 = date_reported-bedrisk_stopdate3;
   if bedrisk_lag3 >=0 and bedrisk_lag3 <= 21 then bedrisk2=3;

   if lag(bedrisk)=4 and bedrisk<4 then
   do;
      bedrisk_flag4 = 1;
	  bedrisk_stopdate4 = date_reported;
   end;
   bedrisk_lag4 = date_reported-bedrisk_stopdate4;
   if bedrisk_lag4 >=0 and bedrisk_lag4 <= 21 then bedrisk2=4;

   by location date_reported;
run;

data USARISK;
   set beds4;
   if bedrisk2 = 0 then discount = 0;
   if bedrisk2 = 1 then discount = 0.2;
   if bedrisk2 = 2 then discount = 0.5;
   if bedrisk2 = 3 then discount = 0.8;
   if bedrisk2 = 4 then discount = 1;
   rename bedrisk2 = risk;
   multiplier = 1 - discount;
   keep location date_reported allbed_mean beds_available beds_ratio bedrisk2 discount multiplier;
   where location ne 'United States of America';
run;

/*
proc gplot data = beds4;
   plot bedrisk2*date_reported = location;
run;

proc sort data = beds5 out = a nodupeky;
   by location;
run;
*/

PROC EXPORT DATA= WORK.Usarisk 
            OUTFILE= "/lrlhps/users/c269016/USARISK_July6.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

