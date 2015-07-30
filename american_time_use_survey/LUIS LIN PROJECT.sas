*/*************************************************************************
* Class: STAT 440                                                         *
* Author: Luis Lin                                                        *
* Date: 12/12/11                                                          *
* Title: ATUS Survey                                                      *
*                                                                         *
* Details:                                                                *
* The file is divided in secionts for the format,input data,creating data *
* sets for analysis, and the analysis. 									  *
**************************************************************************/

* Create permanent library;

libname st 'C:\STAT 440';

* Create permanent formats;
options fmtsearch=(st);

* Note: formats are different from the ones given in the codebook;

proc format library=st;

  value AGEf	  low-19='15 to 19 years'
  				  20-24='20 to 24 years'
				  25-34='25 to 34 years'
				  35-44='35 to 44 years'
				  45-54='45 to 54 years'
				  55-64='55 to 64 years'
				  65-74='65 to 74 years'
				  75-high='75 years and over';

  value SEXf      1='Male'
                  2='Female';
				
  value RACEf     1='White only'
                  2='Black only' 
				  3='Native only'
                  4='Asian only' 
				  other='Mixed or Other';

  value CHILDf    1='Yes' 
                  2='No';

  value STATUSf   1,2='Employed' 
                  3,4='Unemployed' 
                  5='Not in labor force';

  value DEGREEf   31-38='No High School Diploma'
                  39-42='High School Diploma'
                  43='Bachelors degree'
                  44-46='Masters, Professional or Doctoral degree';

  value WEEKDAYf  0='No' 
                  1='Yes';
run;

***********************************************************************;

* Create the starting dataset by reading the raw data file;

data st.start;

	*Note: length of the records in raw file is greater than 
    * 256 bytes (default). In this case, record length is 4590, 
	*so use LRECL=4590 to tell SAS to read a line up to 4590 characters;
	infile "C:\STAT 440\26149-0001-data.txt" LRECL=4590;
	input

		/*Raw data file contains 572 variables; only selected variables
	      listed below were read. Also variable names different from
	      the ones listed in the codebook*/

		/*Identifier and Weight*/
		ID 15-28  /*TUCASEID-ATUS Case ID*/
		WEIGHT 799-814 .6  /*TUFINLWGT-ATUS final weight*/

		/*Day indicator*/
		DIARYDAY 751-758 /*TUDIARYDAY - Day of Week of diary, 1=sunday*/
		HOLIDAY 341-348 /*TRHOLIDAY - Indicator if Day of diary is a holiday, 1=yes*/

		/*Demographics*/
		AGE 1375-1382 /*TEAGE - Age*/
		SEX 1383-1390 /*TESEX - Sex*/
		RACE 1399-1406 /*PTDTRACE - Race*/
		CHILD 333-340 /*TRHHCHILD - Presence of household children < 18*/
		STATUS 181-188 /*TELFS - Labor force status*/
		DEGREE 1391-1398  /*PEEDUCA - Education Attainment*/
		
		
		/*Activity: Sleeping*/
		T010101 1423-1430		T010102 1431-1438

		/*Activity: Personal Care*/

		T010201 1439-1446       T010299 1447-1454		T010301 1455-1462       
		T010399 1463-1470       T010401 1471-1478		T010499 1479-1486       
		T010501 1487-1494       T019999 1495-1502

		/*Activity: Household*/

		T020101 1503-1510       T020102 1511-1518       T020103 1519-1526
        T020104 1527-1534       T020199 1535-1542       T020201 1543-1550
        T020202 1551-1558       T020203 1559-1566       T020301 1567-1574
        T020302 1575-1582       T020303 1583-1590       T020399 1591-1598
        T020401 1599-1606       T020402 1607-1614       T020499 1615-1622
        T020501 1623-1630       T020502 1631-1638       T020601 1639-1646
        T020602 1647-1654       T020699 1655-1662       T020701 1663-1670
        T020799 1671-1678       T020801 1679-1686       T020899 1687-1694
        T020901 1695-1702       T020902 1703-1710       T020903 1711-1718
        T020904 1719-1726       T020905 1727-1734       T020999 1735-1742
        T029999 1743-1750

		/*Activity: Work and Work Related*/

		T050101 2295-2302       T050102 2303-2310       T050103 2311-2318
        T050104 2319-2326       T050199 2327-2334       T050201 2335-2342
        T050202 2343-2350       T050204 2351-2358       T050205 2359-2366
        T050299 2367-2374       T050301 2375-2382       T050302 2383-2390
        T050303 2391-2398       T050304 2399-2406       T050399 2407-2414
        T050401 2415-2422       T050403 2423-2430       T050404 2431-2438

		/*Activity: Education*/
        
		T060101 2439-2446       T060102 2447-2454       T060103 2455-2462
        T060104 2463-2470       T060199 2471-2478       T060201 2479-2486
        T060202 2487-2494       T060299 2495-2502       T060301 2503-2510
        T060302 2511-2518       T060399 2519-2526       T060401 2527-2534
        T060402 2535-2542       T060499 2543-2550       T069999 2551-2558
 
        /*Activity: Consumer Purchases*/
		T070101 2559-2566       T070102 2567-2574       T070103 2575-2582
        T070104 2583-2590       T070105 2591-2598       T070199 2599-2606
        T070201 2607-2614       T070299 2615-2622

       	/*Activity: Eating and Drinking*/ 
		T110101 2927-2934       T110201 2935-2942

		/*Activity: Socializing and Communicating*/
		T120101 2943-2950

		/*Activity: Watching TV*/
 		T120303 2991-2998       T120304 2999-3006
        
    	/*Activity: Participating in Sports, Exercise or Recreation*/
               
        T130101 3175-3182
        T130102 3183-3190       T130103 3191-3198       T130104 3199-3206
        T130105 3207-3214       T130106 3215-3222       T130107 3223-3230
        T130108 3231-3238       T130109 3239-3246       T130110 3247-3254
        T130112 3255-3262       T130113 3263-3270       T130114 3271-3278
        T130116 3279-3286       T130117 3287-3294       T130118 3295-3302
        T130119 3303-3310       T130120 3311-3318       T130121 3319-3326
        T130122 3327-3334       T130123 3335-3342       T130124 3343-3350
        T130125 3351-3358       T130126 3359-3366       T130127 3367-3374
        T130128 3375-3382       T130129 3383-3390       T130130 3391-3398
        T130131 3399-3406       T130132 3407-3414       T130133 3415-3422
        T130134 3423-3430       T130135 3431-3438       T130136 3439-3446
        T130199 3447-3454 ;

		
	
run;

***********************************************************************;

* Create the main dataset by combining the activity variables,
* applying permanent formats and labels, and dealing with missing values;

data st.main;
	set st.start;

		/*Day of week indicator*/
		* If it is a weekend of holiday, weekday is 0;
		if DIARYDAY in (1,7) |  HOLIDAY = 1 then  WEEKDAY=0;
		else WEEKDAY=1;

		/*Combine Activity variables*/
		SLEEP = T010101 + T010102;
		PERSONAL = sum(of T010201--T019999);
		HOUSEHOLD = sum(of T02:);
		WORK = sum(of T05:);
		EDU = sum(of T06:);
		SHOP = sum(of T07:);
		EAT = sum(of T11:);
		SOCIAL= T120101;
		TV = sum(of T120303--T120304);
		REC = sum(of T13:);
		OTHER = 24*60 - sum(of SLEEP--REC);

		/*Convert to hours for better interpretability*/
		array time{11} SLEEP--OTHER;
		do i=1 to 11;
			time{i}=time{i}/60;
		end;

	* To simplify calcuations and subsetting, set NA values 
    * (if any) of the demographics variables to missing.
	* Value of -3,-2,-1 denote Refused, Don't know and blank
	* respectively;

	array demog{6} AGE--DEGREE;
		do i=1 to 6;
			if demog{i} in (-3,-2,-1) then demog{i}=.;
		end;

	/*Drop unnecessary variables*/
	drop i T010101--T130199 DIARYDAY HOLIDAY;


	*Apply labels;
	label 
		ID        = 'ATUS Case ID'
		WEIGHT    = 'ATUS final weight'
		AGE       = 'Age'
		SEX       = 'Sex'
		RACE      = 'Race'
		CHILD     = 'Presence of household children < 18'
		STATUS    = 'Labor force status'
		DEGREE    = 'Education Attainment'
		WEEKDAY   = 'Weekday (non-holiday)'
		SLEEP     = 'Sleeping'
		PERSONAL  = 'Personal Care'
		HOUSEHOLD = 'Household activities'
		WORK      = 'Working and work-related activities'
		EDU       = 'Educational activities'
		SHOP      = 'Purchasing goods and services'
		EAT       = 'Eating and drinking'
		SOCIAL    = 'Socializing and communicating'
		TV        = 'Watching television'
		REC       = 'Participating in sports, exercise, and recreation'
		OTHER     = 'Other activities';

	*Apply formats;
	format 
		AGE AGEf.
		SEX SEXf. 
		RACE RACEf.
		CHILD CHILDf. 
		STATUS STATUSf.
		DEGREE DEGREEf.
		WEEKDAY WEEKDAYf.
		SLEEP--OTHER 4.2;
run;

ods rtf file='C:\STAT 440\Check.rtf';
*Examine the descriptor portion of the SAS data set;
proc contents data=st.main varnum;
run;

*Examine the data portion of the SAS data set;
proc print data=st.main (firstobs=1 obs=10) noobs label;
	var ID--SLEEP;
run;

* Frequency report of age by sex (unweighted);

proc freq data=st.main;
	tables AGE*SEX/ norow nocol;
run;


*Frequency report of age by sex (weighted);

proc freq data=st.main;
	weight weight;
	tables AGE*SEX/ norow nocol;
run;

ods rtf close;


***********************************************************************
***** SET1: 
***** Daily participation rates (% of population engaged in activity);

ods rtf file='C:\STAT 440\Output.rtf';

* Create dataset for analysis; 
data st.set1;
	set st.main;
	array time{11} SLEEP--OTHER;

		/*1 if respondent is engaged in activity i during the
		reference day and 0 otherwise*/
		do i=1 to 11;
			if time{i} eq 0 then time{i}=0;
			else time{i}=1;
		end;
	drop i;

run;



* Compute weighted statistics;
proc means data=st.set1 maxdec=2 mean nonobs;
	weight weight;
	var SLEEP--OTHER;
	output out=out1;
run;

*Rotate previous dataset;
data st.result1(DROP=_TYPE_ _FREQ_ _STAT_ SLEEP--OTHER i);
	set out1;
		if _STAT_ eq 'MEAN';
		array times{11} SLEEP--OTHER;
		do i=1 to 11;
			ACTIVITY = vname(times{i});
			TIME=times{i};
			output;
		end;
		format TIME percent.;
run;


* Generate the vertical bar chart;
goptions reset=all;
title1  'Participation Rate';
title2 'by Activity';
axis1 label=('Participation Rate');
proc gchart data=st.result1;
	vbar3d ACTIVITY / sumvar=TIME descending 
					  raxis=axis1 shape=C cframe='LIGR' minor=0 autoref;
run;

***********************************************************************;
***** SET2:
***** Average hours per day by weekday and weekend/holiday;

* Compute weighted statistics;
proc means data=st.main maxdec=2 mean nonobs;
	weight weight;
	var SLEEP--OTHER ;
	class WEEKDAY;
	output out=out2;
run;

*Rotate previous dataset;
*Convert time to seconds so HH:MM format can be used;
data st.result2(DROP=_TYPE_ _FREQ_ _STAT_ SLEEP--OTHER i);
	set out2;
		if _STAT_ eq 'MEAN' & _TYPE_=1;
		array times{11} SLEEP--OTHER;
		do i=1 to 11;
			ACTIVITY = vname(times{i});
			TIME=times{i};
			PERCENT=TIME/24;
			TIME=TIME*3600;
			output;
		end;

		format TIME HHMM5. PERCENT percent.;
run;

* Generate pie chart;
goptions reset=all;
title1 'Average hours per day of awake time'; 
title2 'by type of day';
proc gchart data=st.result2;
	* Subset: don't include sleep, only plot activities during awake time;
	* Order slices;
	pie ACTIVITY / sumvar=TIME group=WEEKDAY
				   midpoints=  'PERSONAL' 'EAT' 'SOCIAL' 'WORK' 'REC'  
                               'HOUSEHOLD' 'EDU' 'TV'  'SHOP'  'OTHER'
				   other=0 across=2 percent=outside  noheading;
run;



***********************************************************************;
***** SET3:
***** Average hours per day by sex;



* Compute weighted statistics;
proc means data=st.main maxdec=2 mean nonobs;
	weight weight;
	var SLEEP--OTHER ;
	class SEX;
	output out=out3;
run;

*Rotate previous dataset;
*Convert time to seconds so HH:MM format can be used;
data st.result3(DROP=_TYPE_ _FREQ_ _STAT_ SLEEP--OTHER i);
	set out3;
		if _STAT_ eq 'MEAN' & _TYPE_=1;
		array times{11} SLEEP--OTHER;
		do i=1 to 11;
			ACTIVITY = vname(times{i});
			TIME=times{i};
			PERCENT=TIME/24;
			TIME=TIME*3600;
			output;
		end;

		format TIME HHMM5. PERCENT percent.;
run;

* Generate the vertical bar chart;
* Subset: select activities; 
goptions reset=all;
title1 'Average hours per day';
title2 'by sex';
axis1 label=(angle= 90 'Average hours per day') order=(0 to 10800 by 1800);
axis2 order=('TV' 'HOUSEHOLD' 'PERSONAL'  'REC');
pattern1 color=bigb; pattern2 color=lipk;

proc gchart data=st.result3;

	where ACTIVITY in ('TV','PERSONAL','HOUSEHOLD','REC');
	vbar SEX / discrete sumvar=TIME group=ACTIVITY autoref
					  raxis=axis1 gaxis=axis2 cframe='LIGR'
					  minor=0 inside=sum patternID=midpoint
                      space=0 gspace=3;
run;




***********************************************************************;
***** SET4:
***** Average work hours per day of participants by child and age;



* Compute weighted statistics;
* Subset: participants engaging in work; 
proc means data=st.main maxdec=2 mean nonobs;
	where WORK ne 0;
	weight weight;
	var WORK ;
	class CHILD AGE;
	output out=out4;
run;

*Rotate previous dataset;
*Convert time to seconds so HH:MM format can be used;
data st.result4(DROP=_TYPE_ _FREQ_ _STAT_ WORK);
	set out4;
		if _STAT_ eq 'MEAN' & ( AGE ne . & CHILD ne .) ;
		ACTIVITY = vname(WORK);
		TIME=WORK;
		PERCENT=TIME/24;
		TIME=TIME*3600;
		output;

		format TIME HHMM5. PERCENT percent.;
run;

* Generate the vertical bar chart;
goptions reset=all;
title1 'Average work hours per day of participants';
title2 'by age and presence of child';
axis1 value=none label=none; 
axis2 label=(angle=90'Average work hours per day of participants') 
              order=(0 to 28800 by 3600);

legend1 frame;
proc gchart data=st.result4;

	vbar AGE/ discrete sumvar=TIME subgroup= AGE group=CHILD    
			autoref space=0 gspace=4 cframe='LIGR' minor=0 
			inside=sum maxis=axis1 raxis=axis2 ;
run;



***********************************************************************;
***** SET5:
***** Average educuation hours per day of participants 
***** by degree and labor force status ;



* Compute weighted statistics;
* Subset: participants engaging in edu; 
proc means data=st.main maxdec=2 mean nonobs;
	where EDU ne 0;
	weight weight;
	var EDU ;
	class STATUS DEGREE;
	output out=out5;
run;


*Rotate previous dataset;
*Convert time to seconds so HH:MM format can be used;
data st.result5(DROP=_TYPE_ _FREQ_ _STAT_ EDU);
	set out5;
		if _STAT_ eq 'MEAN' & ( STATUS ne . & DEGREE ne .) ;
		ACTIVITY = vname(EDU);
		TIME=EDU;
		PERCENT=TIME/24;
		TIME=TIME*3600;
		output;

		format TIME HHMM5. PERCENT percent.;
run;

* Generate the vertical bar chart;
goptions reset=all;
title1 'Average educational hours per day of participants';
title2 'by educational attainment and labor force status';
axis1 value=none label=none; 
axis2 label=(angle=90'Average educational hours per day of participants') 
             order=(0 to 28800 by 3600);

legend1 frame;
proc gchart data=st.result5;

	vbar DEGREE/ discrete sumvar=TIME subgroup= DEGREE group=STATUS    
			autoref space=0 gspace=3 cframe='LIGR' minor=0 
			outside=sum maxis=axis1 raxis=axis2;
run;



***********************************************************************;
***** SET6:
***** Average hours per day by race;



* Compute weighted statistics;
proc means data=st.main maxdec=2 mean nonobs;
	weight weight;
	var SLEEP--OTHER ;
	class RACE;
	output out=out6;
run;

*Rotate previous dataset;
*Convert time to seconds so HH:MM format can be used;
data st.result6(DROP=_TYPE_ _FREQ_ _STAT_ SLEEP--OTHER i);
	set out6;
		if _STAT_ eq 'MEAN' & _TYPE_=1;
		array times{11} SLEEP--OTHER;
		do i=1 to 11;
			ACTIVITY = vname(times{i});
			TIME=times{i};
			PERCENT=TIME/24;
			TIME=TIME*3600;
			output;
		end;

		format TIME HHMM5. PERCENT percent.;
run;

* Generate the vertical bar chart;
* Subset: select activities; 
goptions reset=all;
title1 'Average hours per day';
title2 'by race';
axis1 label=('Average hours per day') order=(0 to 14400 by 1800);
pattern1 color=bigb; pattern2 color=lipk; 
pattern3 color=lilg; pattern4 color=vliv; pattern5 color=lioy;

proc gchart data=st.result6;

	where ACTIVITY in ('TV','SOCIAL','REC');
	Hbar RACE / discrete sumvar=TIME type=mean group=ACTIVITY autoref
					  raxis=axis1 cframe='LIGR'
					  space=0 gspace=2 minor=0 patternID=midpoint;
run;

ods rtf close;

quit;



