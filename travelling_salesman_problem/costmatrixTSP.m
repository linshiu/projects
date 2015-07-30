%% Function: costmatrixTSP

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: costmatrixTSP.m
% Date: 3/27/12
% Author: Luis S Lin
% Description: read matrix (latitude and longitude coordinates)  
%              and create a distance (cost) matrix by compuing the 
%              distances between each pair of cities
%
% Files used: findDistance.m
%
% Inputs: 1) matrix containing latitude and 
%         longitude (absolute value, so N and W assumed) of cities 
%
% Outputs: 1) cost matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%% Create distance matrix
function[c]=costmatrixTSP(coords)
n = length(coords(:,1));
c = zeros(n,n); % cost matrix
% iterate through coords
for i=1:(n)
    for j=(i+1):n
        % Get coordinates of two cities
        lat1=coords(i,1);
        lat2=coords(j,1);
        lon1=coords(i,2);
        lon2=coords(j,2);
        % Compute disance and save in matrix (symmetric)
        c(i,j)=findDistance(lat1,lat2,lon1,lon2);
        c(j,i)=c(i,j);
    end
end
