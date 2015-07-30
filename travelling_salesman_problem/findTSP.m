%% Function: findTSP

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: findTSP.m
% Date: 3/27/12
% Author: Luis S Lin
% Description: find a TSP given the distance matrix of cities by appying the 
%              farthest insertion heuristic
%
% Files used: insertToTour.m
%
% Inputs: 1) matrix containing distances between cities 
%
% Outputs: 1) TSP tour (path with city numbers ordered)
%          2) length of tour in kM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Function: findTSP
function[tourTSP,lengthTSP] = findTSP(c)

n = length(c(:,1));         % Number of cities
maxcost=max(max(c))*3;      % Upper bound for insertion cost
lengthTSP=max(max(c))*n;     % Upper bound for TSP length
% repeat algorithm for all possible starting cities
for startCity=1:n

tstart=tic; % Start computation time

%% Step 1: Create a staring tour of two cities
T=[];   % Create array containing list of cities in tour
V= 1:n; % Create array containing loist of cities not in tour
TOUR = zeros(n,n); % Create cost matrix of cities' edges in tour 
                   % (Adjacency matrix with entries equal to distance
                   %  between cities in tour)         
% First city
first=startCity;  % Select first city
T(1)=first ;      % Add to array containing cities in tour
V(V==first) = []; % Remove selected city from array not in tour

% Second city
minvalue=min(c(first,c(first,:)~=0));  % Find min distance from first city
mincity=find(c(first,:)==minvalue);    % Find correponding city (index)
mincity=mincity(1);                    % If tie, pick first listed city
T(2)=mincity;                          % Add city to tour
V(V==mincity) = [];                    % Remove city from non-tour

% Add edge first-second to TOUR
TOUR(first,mincity)=c(first,mincity);
TOUR(mincity,first)=TOUR(first,mincity);

% Add cities to tour path
tourTSPtemp=[first,mincity,first];

%% Step 2: Add cities to tour

% Repeat until all cities included in tour
while length(V)~=0

% Step 2.1: Find farthest city from V (non-tour) to T (tour)

maxvalue=0; % set current maxvalue (distance) to minimum 

% Repeart for all cties not in tour
for i=V
    maxvaluetemp=max(c(i,T)); % Find farthest city i not in tour to cities in tour
    if(maxvaluetemp>maxvalue) % If distance exceeds previous maximum distance
        maxvalue=maxvaluetemp;% Set current distance as new max distance
        k = i;                % Set current city i as current farthest city to 
    end                       % be inserted
end

% Step 2.2: Find existing edge (i,j) in tour to be removed
increase=maxcost; % Set sarting increase cost to max
% Check for all possible combinations i and j of cities in tour
for counter1=1:(length(T)-1)
    i=T(counter1);
    for counter2=(counter1+1):length(T)
        j=T(counter2);
        if(TOUR(i,j)~=0) %check existing edge
            % compute insertion cost of adding k between i and j
            increasetemp=c(i,k)+c(j,k)-c(i,j);                              
            if(increasetemp<increase) % if insertion cost is less than previous one
                increase=increasetemp;% set as current one as new insertion cost
                i_cut=i; % set current edge (i,j) to be cut
                j_cut=j;
            end
        end
    end
end

% Step 2.3: Replace exiting edge by new edges

if(length(T)==2) % if only 2 cities in tour
    
% add edges (i,k) and (j,k)
TOUR(i_cut,k)=c(i_cut,k);
TOUR(k,i_cut)=TOUR(i_cut,k);
TOUR(j_cut,k)=c(j_cut,k);
TOUR(k,j_cut)=TOUR(j_cut,k);

else  % for more than 2 cities in tour
% insert city k into tour by replacing edge (i,j) with (i,k) and (j,k)
% remove edge (i,j)
TOUR(i_cut,j_cut)=0;
TOUR(j_cut,i_cut)=0;
% add edges (i,k) and (j,k)
TOUR(i_cut,k)=c(i_cut,k);
TOUR(k,i_cut)=TOUR(i_cut,k);
TOUR(j_cut,k)=c(j_cut,k);
TOUR(k,j_cut)=TOUR(j_cut,k);
end

% update city k to T and V
T(length(T)+1)=k; % Add city to tour
V(V==k) = []; % Remove city from non-tour

% update tour by adding k in corresponding location
tourTSPtemp=insertToTour(i_cut,j_cut,k,tourTSPtemp);
end

tElapsedtemp=toc(tstart);  % End computation time and store elapsed time

%% Determine best tour so far
lengthTSPtemp=sum(sum(TOUR))/2; % sum all edges in tour matrix and divide by 2
                                % (sum of edges in tour = tour distance)

if(lengthTSPtemp<lengthTSP)  % if current TSP length is less than previous
    lengthTSP=lengthTSPtemp; % set current TSP length as new one
    tourTSP=tourTSPtemp;     % set current TSP tour as new one
    tElapsed=tElapsedtemp;   % set current elapsed time as new one
    
end


end

%% Display Results (for 150 cities)
sprintf('%s\n',...
    '--Best Tour found by Farthest Insertion Heuristic--',...
    sprintf('Computation TIme for heuristic starting at city %d: %f seconds',...
             tourTSP(1),tElapsed),...
    sprintf('Tour Length: %g km',lengthTSP),...
    'Tour Path: ',...
    sprintf('%g-',tourTSP(1:n/6*1)),...
    sprintf('%g-',tourTSP(n/6*1+1:n/6*2)),...
    sprintf('%g-',tourTSP(n/6*2+1:n/6*3)),...
    sprintf('%g-',tourTSP(n/6*3+1:n/6*4)),...
    sprintf('%g-',tourTSP(n/6*4+1:n/6*5)),...
    strcat(sprintf('%g-',tourTSP(n/6*5+1:n/6*6)),sprintf('%g',tourTSP(n+1))))  
