%% Function : findDistance

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: findDistance.m
% Date: 3/27/12
% Author: Luis S Lin
% Description: Compute the distance between two points (in km) given the 
%              latitude and longitude coordinates in degrees
%              Calculations based on great circle distance. 
%
% Inputs: 1) latitude 1
%         2) latitude 2
%         3) longitude 1
%         4) longitude 2
%
% Ouput: 1) distance in km bewteen points
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Function : findDistance

function[d] = findDistance(lat1,lat2,lon1,lon2)
    % Get coordinates and convert to radians
    lat1=degtorad(lat1);
    lat2=degtorad(lat2);
    lon1=degtorad(lon1);
    lon2=degtorad(lon2);
    % Compute coordinate differences
    dlat=lat2-lat1;
    dlon=lon2-lon1;
    % Square of half the chord length between the cities
    a=sin(dlat/2)^2+cos(lat1)*cos(lat2)*sin(dlon/2)^2;
    % angular distance (in radians)
    c=2*atan2(sqrt(a),sqrt(1-a));
    % Earth’s radius (mean radius = 6,371km)
    R=6371;
    % Compute distance
    d=R*c;