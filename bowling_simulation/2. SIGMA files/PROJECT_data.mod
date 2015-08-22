

         MODEL DEFAULTS
         --------------

Model Name:         PROJECT_data.mod
Model Description:  BOWLING ALLEY
Output File:        PROJECT_data.OUT
Output Plot Style:  NONE
Run Mode:           HI_SPEED
Trace Vars:         SI,B,WAIT,CTIME,N,WDAY,L[0],L[1],L[2],L[12],L[13],L[14],L[15],L[16],L[17],L[18],L[19],L[20],L[21],L[22],L[23]
Random Number Seed: 12345
Initial Values:     0,16,0,0,0,0,0,0,0,0
Ending Condition:   STOP_ON_EVENT
Event:              END   (1 iterations)
Trace Events:       END
Hide Edges:         



         STATE VARIABLES
         ---------------

     State Variable #1
Name:          S
Description:   NUMBER OF AVAILABLE LANES
Type:          INTEGER
Size:          1

     State Variable #2
Name:          Q
Description:   NUMBER OF GROUPS IN LINE
Type:          INTEGER
Size:          1

     State Variable #3
Name:          B
Description:   NUMBER OF GROUPS THAT BALK
Type:          INTEGER
Size:          1

     State Variable #4
Name:          AI
Description:   ARRIVAL INDEX (NOT INCLUDING GROUPS THAT BALK)
Type:          INTEGER
Size:          1

     State Variable #5
Name:          SI
Description:   SERVICE INDEX (GROUP ID THAT START SERVICE)
Type:          INTEGER
Size:          1

     State Variable #6
Name:          DAY
Description:   RUNNIG DAY OF SIMULATION
Type:          INTEGER
Size:          1

     State Variable #7
Name:          WDAY
Description:   WORKING DAY (FROM OPENING TO EMPTY AFTER CLOSED)
Type:          INTEGER
Size:          1

     State Variable #8
Name:          ARRIVE
Description:   ARRAY CONTAINING ARRIVAL TIME OF GROUP (NO BALK)
Type:          REAL
Size:          15627

     State Variable #9
Name:          WAIT
Description:   ARRAY WITH TOTAL WAITING TIME OF SERVED GROUPS
Type:          REAL
Size:          1

     State Variable #10
Name:          K
Description:   GROUP SIZE
Type:          INTEGER
Size:          15627

     State Variable #11
Name:          OPEN
Description:   STATUS OF BOWLING
Type:          INTEGER
Size:          1

     State Variable #12
Name:          CTIME
Description:   ARRAY WITH CLOSING HOUR OF WORKING DAY
Type:          REAL
Size:          1

     State Variable #13
Name:          L
Description:   RRAY W/ NUMBER OF LANES IN USE AT CERTAIN HOUR
Type:          INTEGER
Size:          15627

     State Variable #14
Name:          N
Description:   TOTAL NUMBER OF PEOPLE SERVED
Type:          INTEGER
Size:          1

     State Variable #15
Name:          A
Description:   holder for current index of group that arrived 
Type:          INTEGER
Size:          1



          VERTICES
          --------

     Vertex #1
Name:             RUN
Description:      THE SIMULATION RUN IS STARTED
State Changes:    
Parameter(s):     Q,S,B,AI,SI,DAY,WDAY,WAIT,N,CTIME
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X: -0.96;  Y:  2.26;  Z:  0.00;
Local Trace:      
Trace Location:   Left

     Vertex #2
Name:             OPEN
Description:      THE ALLEY IS OPENED AT 12:00 PM
State Changes:    OPEN=1, WDAY=WDAY+1
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  0.18;  Y:  2.27;  Z:  0.00;
Local Trace:      OPEN,WDAY
Trace Location:   Top

     Vertex #3
Name:             START
Description:      SERVICE STARTS
State Changes:    S=S-1, Q=Q-1, ARRIVE[0]=RND, SI=SI+1, WAIT=WAIT+(CLK-ARRIVE[SI]), K[SI]=1*(ARRIVE[0]<0.3)+2*(ARRIVE[0]>=0.3)*(ARRIVE[0]<0.5)+3*(ARRIVE[0]>=0.5)*(ARRIVE[0]<0.6)+4*(ARRIVE[0]>=0.6), N=N+K[SI]
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  1.50;  Y:  2.27;  Z:  0.00;
Local Trace:      S,Q,ARRIVE[0],SI,WAIT,K[SI],N
Trace Location:   Top

     Vertex #4
Name:             FINISH
Description:      GROUPS FINISH SERVICE - LEAVE ALLEY IMMEDIATELY
State Changes:    S=S+1, 
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  2.39;  Y:  2.27;  Z:  0.00;
Local Trace:      S
Trace Location:   Bottom

     Vertex #5
Name:             STOP
Description:      ALLEY CLOSED AT 23:00 (11:00PM) - NO MORE GROUPS ARE ADDED TO THE QUEUE
State Changes:    OPEN=0, ARRIVE[15000]=CLK
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  0.50;  Y:  2.74;  Z: -1.00;
Local Trace:      OPEN,ARRIVE[15000]
Trace Location:   Bottom

     Vertex #6
Name:             NEW DAY
Description:      NEW DAYS STARTS AT 0:00 AM HOURS
State Changes:    DAY=DAY+1
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X: -0.36;  Y:  2.26;  Z: -1.00;
Local Trace:      DAY
Trace Location:   Bottom

     Vertex #7
Name:             ARRIVE
Description:      GROUPS ARRIVE TO THE BOWLING ALLEY - EXCLUDE GROUPS THAT BALK FROM THE ARRIVAL INDEX AND THE QUEUE
State Changes:    ARRIVE[0]=RND, AI=AI+1*(S>0)+1*(S==0)*(Q<=5)+1*(S==0)*(Q>=6)*(Q<=15)*(ARRIVE[0]>=(Q/10-0.5)), A=AI*(S>0)+AI*(S==0)*(Q<=5)+AI*(S==0)*(Q>=6)*(Q<=15)*(ARRIVE[0]>=(Q/10-0.5)), B=B+1*(S==0)*(Q>=6)*(Q<=15)*(ARRIVE[0]<(Q/10-0.5))+1*(S==0)*(Q>15), Q=Q+1*(S>0)+1*(S==0)*(Q<=5)+1*(S==0)*(Q>=6)*(Q<=15)*(ARRIVE[0]>=(Q/10-0.5)), ARRIVE[A]=CLK, 
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  0.84;  Y:  2.27;  Z: -1.00;
Local Trace:      Q,S,AI,A,ARRIVE[A],B,Q
Trace Location:   Bottom

     Vertex #8
Name:             LANES
Description:      CHECK NUMBER OF LANES IN USE FOR A SPECIFIC HOUR OF THE DAY
State Changes:    L[CLK/60-24*(DAY-1)]=L[CLK/60-24*(DAY-1)]+(16-S)
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  0.18;  Y:  1.85;  Z: -1.00;
Local Trace:      WDAY,S,L[0]
Trace Location:   Bottom

     Vertex #9
Name:             CLOSE
Description:      CHECK IF THE SYSTEM IS EMPTY TO DETERMINE THE CLOSING TIME FOR THE CURRENT WORKING DAY
State Changes:    CTIME=CTIME+(CLK-ARRIVE[15000])
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  2.39;  Y:  1.83;  Z: -1.00;
Local Trace:      ARRIVE[15000],CTIME
Trace Location:   Bottom

     Vertex #10
Name:             END
Description:      
State Changes:    
Parameter(s):     
Bitmap(Inactive): 
Bitmap(Active):   
Use Flowchart Shapes:   0
Use Opaque Bitmaps:   0
Location:         X:  3.00;  Y:  1.82;  Z: -1.00;
Local Trace:      
Trace Location:   Bottom



          EDGES
          -----


     Graphics Edge #1

  Sub-Edge #1
Description:   SCHEDULE CLOSE IN 11 HOURS (AT 23:00)
Type:          Scheduling
Origin:        OPEN
Destination:   STOP
Condition:     1==1
Delay:         60*11
Priority:      5
Attributes:    

     Graphics Edge #2

  Sub-Edge #2
Description:   DAY STARTS AT 0:00 
Type:          Scheduling
Origin:        RUN
Destination:   NEW DAY
Condition:     1==1
Delay:         0
Priority:      1
Attributes:    

     Graphics Edge #3

  Sub-Edge #3
Description:   SCHEDULING OPENING IN 12 HOURS ( AT 12:00 PM)
Type:          Scheduling
Origin:        NEW DAY
Destination:   OPEN
Condition:     1==1
Delay:         60*12
Priority:      1
Attributes:    

     Graphics Edge #4

  Sub-Edge #4
Description:   START A NEW DAY EVERY 24 HOURS 
Type:          Scheduling
Origin:        NEW DAY
Destination:   NEW DAY
Condition:     1==1
Delay:         60*24
Priority:      1
Attributes:    

     Graphics Edge #5

  Sub-Edge #5
Description:   THERE IS ALWAYS ONE GROUP WAITING BEFORE OPENING - SCHEDULE THAT ARRIVAL WITH NO TIME DELAY
Type:          Scheduling
Origin:        OPEN
Destination:   ARRIVE
Condition:     1==1
Delay:         0
Priority:      4
Attributes:    

     Graphics Edge #6

  Sub-Edge #6
Description:   STOP ARRIVALS IMMEDIATELY
Type:          Cancelling
Origin:        STOP
Destination:   ARRIVE
Condition:     1==1
Delay:         0
Priority:      4
Attributes:    

     Graphics Edge #7

  Sub-Edge #7
Description:   SCHEUDLE SERVICE IMMEDIATELY IF THERE ARE IDLE SERVERS (FREE LANES)
Type:          Scheduling
Origin:        ARRIVE
Destination:   START
Condition:     S>0
Delay:         0
Priority:      2
Attributes:    

     Graphics Edge #8

  Sub-Edge #8
Description:   INTERARRIVAL TIME IS EXPONENTIAL WITH MEAN 4 MINUTES
Type:          Scheduling
Origin:        ARRIVE
Destination:   ARRIVE
Condition:     1==1
Delay:         4*ERL{1}
Priority:      4
Attributes:    

     Graphics Edge #9

  Sub-Edge #9
Description:   INITIALIZE CHECK OF LANES 30 MINUTES AFTER OPENING
Type:          Scheduling
Origin:        OPEN
Destination:   LANES
Condition:     DAY==1
Delay:         60
Priority:      6
Attributes:    

     Graphics Edge #10

  Sub-Edge #10
Description:   CHECK NUMBER OF LANES EVERY 1 HOUR
Type:          Scheduling
Origin:        LANES
Destination:   LANES
Condition:     1==1
Delay:         60
Priority:      6
Attributes:    

     Graphics Edge #11

  Sub-Edge #11
Description:   SCHEDULE END OF SERVICE (TIME GROUP LEAVES ALLEY) ACCORDING TO NORMAL(30*K,4K)
Type:          Scheduling
Origin:        START
Destination:   FINISH
Condition:     1==1
Delay:         NOR{30*K[SI];4*K[SI]}
Priority:      3
Attributes:    

  Sub-Edge #12
Description:   IF THERE ARE GROUPS WAITING, SCHEDULE NEW SERVICE WITH NO TIME DELAY
Type:          Scheduling
Origin:        FINISH
Destination:   START
Condition:     Q>0
Delay:         0
Priority:      2
Attributes:    

     Graphics Edge #12

  Sub-Edge #13
Description:   CHECK IF THE SYSTEM IS EMPTY AFTER CLOSING TIME (11PM)
Type:          Scheduling
Origin:        FINISH
Destination:   CLOSE
Condition:     (Q==0)&(S==16)&(OPEN==0)
Delay:         0
Priority:      6
Attributes:    

     Graphics Edge #13

  Sub-Edge #14
Description:   
Type:          Scheduling
Origin:        CLOSE
Destination:   END
Condition:     WDAY==7
Delay:         24*7*60+11*60-CLK
Priority:      5
Attributes:    

