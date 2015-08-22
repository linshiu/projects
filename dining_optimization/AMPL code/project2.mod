# CEE 512 - Logistics Systems Analysis 
# ---------------------------------------------------------------------------------
# Project: Joint Replenishment Policy for Special Vegan Items
# Client: University of Illinios at Urbana-Champaign Housing Dining Services
# Authors: Luis Steven Lin, Akkaravuth Kopsombut, Maomao Yang, Namdong Yoon
# Date: 4/9/12
# Description: Classical Dynamic Joint Replenishment Problem (DJRP) formulation
# In multi-item inventory replenishment contexts, cost savings can be achieved
# by coordinating the replenishment of some item. The DJRP consists of determining 
# the order quantities xit for each item type i and for each period t to minimize 
# the sum of replenishment and inventory holding costs over the whole planning
# horizon.(Boctor 2004)
#
# Software:
# Computer Platform: 
# ---------------------------------------------------------------------------------

#### Assumptions

# Time-varying deterministic demand
# Replenishments are made at the beginning of each period
# Items consumed during period t generate no holding cost during this period
# No quantity discounts, 
# No backlogging is allowed
# No limit is imposed on order sizes and on inventory levels

# ---------------------------------------------------------------------------------

#### Inputs
   param numItems;                  #maximum number of items
   param numPeriods;                #maximum number of periods
   param M;                         #sufficiently large number 
                                    #(max of total demand of an item)

   set ITEM   := 1..numItems;       # types of item i
   set PERIOD := 1..numPeriods;     # demand periods t

# Costs
   param S;         #common ordering cost (units: $/order)
   param s {ITEM};  #individual ordering cost for item type i (units: $/order)
   param c {ITEM};  #unit cost for item type i (units: $/item) 
   param p;         #percentage of unit cost for item i for holding cost (units: %)
   param h {i in ITEM} := p*c[i];  #unit inventory holding cost 
                                   #for item type i (units: $/item)

# Demand
   param d {PERIOD,ITEM};  # demand for item type i for period t (units: item)

# Initial Inventory
   param Io {ITEM};        # initial inventory level of item type i (units: item)

# ---------------------------------------------------------------------------------

#### Variables

# Inventory level of item type i at the end of period t (for t>0)
   var I {ITEM,PERIOD} >=0; # (units: item)

# Replenishment quantity of item type i at the beginning of period t
   var x {ITEM,PERIOD} >=0; # (units: item)

# Binary variables = 1 if and only if item type i is replenished at the beginning
# of period t, i.e. yit = 1 if xit>0
   var y {ITEM,PERIOD} binary; # (units: item)

# Binary variables taking the value 1 if an order is placed for period t,
   var z {PERIOD} binary; # (units: item)
# ---------------------------------------------------------------------------------

#### Objective: 

# sum of orders and holding costs.
minimize Cost: 
   sum{t in PERIOD} (S*z[t] + sum{i in ITEM} (0.047*c[i]*x[i,t] + h[i]*I[i,t]) );

# ---------------------------------------------------------------------------------

#### Constraints

# Initial demand satisfaction
   subject to DemandInitial {i in ITEM}: x[i,1] + Io[i] - I[i,1] = d[1,i];

# Demand satisfaction
   subject to Demand{i in ITEM, t in PERIOD: t>1}:
	             x[i,t] + I[i,t-1] - I[i,t] = d[t,i];
# Replenishment quantity of an item type can be positive only if that item type is
# replenished
   subject to ReplenishQuantity {i in ITEM, t in PERIOD}: x[i,t] <= M*y[i,t];  

# Individual item types can only be included in a joint replenishment if
# that replenishment is made
   subject to ReplenishItem{t in PERIOD}: sum{i in ITEM} y[i,t] <= numItems*z[t];   

