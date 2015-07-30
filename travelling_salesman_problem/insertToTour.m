%% Function: insertToTour

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: insertToTour.m
% Date: 3/27/12
% Author: Luis S Lin
% Description: given an array, the function inserts a k value bewteen
%              values i and j in the array. Values i and j have to be
%              located consecutively in the array. Note that the function
%              works also when the first and last value of the array are
%              the same.
%
% Inputs: 1) i=value 1
%         2) j=value 2
%         3) k=value to be inserted
%         4) array 
%
% Outputs: 1) cost matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Function: insertToTour

function[arrayNew]=insertToTour(i,j,k,array)

%% Determine position of k to be inserted

% find the position of i and j
position_i=find(array==i);
position_j=find(array==j);

% if both i and j are not located at the end points
if (length(position_i)==1)&(length(position_j)==1)
    if(position_i==position_j+1)
        position_k=position_i;
    elseif(position_i==position_j-1)
        position_k=position_j;
    else
    disp('Error1')
    end
    
% if j is located in the endpoints
elseif (length(position_i)==1)&(length(position_j)==2)
    if(position_i==position_j(1)+1)
        position_k=position_i;
    elseif(position_i==position_j(2)-1)
        position_k=position_j(2);
    else
    disp('Error2')
    end
    
% if i is located in the endpoints
elseif(length(position_i)==2)&(length(position_j)==1)
     if(position_j==position_i(1)+1)
        position_k=position_j;
    elseif(position_j==position_i(2)-1)
        position_k=position_i(2);
    else
    disp('Error3')
    end
else

disp('Error: either i or j have more than 2 entries in array, or both have 2 or more entries')    

end

%% Insert k

array; % initial array;
k; %  value to insert
position_k;% index of current array where the value k is to be inserted.

% Create a new array
arrayNew = zeros(1,length(array)+length(k));
% Insert k
arrayNew(position_k +(0:length(position_k)-1)) = k;
% Fill with the rest of original entries
arrayNew(~arrayNew) = array;
