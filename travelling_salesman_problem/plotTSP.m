%%  Function: plotTSP

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: plotTSP.m
% Date: 3/27/12
% Author: Luis S Lin
% Description: Plot the TSP tour for cities in the US
%
% Inputs: 1) latitude (N) and longitudes (W) of US cities 
%            (absolute values in matrix form, order by ascending order
%             of city number)
%         2) order of tour by city number (array as column vector)
%         3) tour length (scalar value in km units)
%
% Output: US map with TSP tour and tour length displayed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Function

function[tourMAP] = plotTSP(coords,tourTSP,lengthTSP)

%%  Reorder coordinates by tour
n=length(tourTSP);
coordsTSP=zeros(n,2);
for i=1:n
    coordsTSP(i,1)= coords(tourTSP(i),1);
    coordsTSP(i,2)= coords(tourTSP(i),2);
end

% Add - sign for W coordinate 
coordsTSP(:,2)=-coordsTSP(:,2);

%% Build a point geostruct

% The first field by convention is Geometry (dimensionality).
% As Geometry is the same for all elements, assign it with deal:
[Cities(1:n).Geometry] = deal('Point');

% Add the latitudes and longitudes to the geostruct:
for i=1:n
    Cities(i).Lat = coordsTSP(i,1);
    Cities(i).Lon = coordsTSP(i,2);
    Cities(i).Name = i;
end

%% Plot US Map
figure; ax = usamap('conus');
states = shaperead('usastatelo', 'UseGeoCoords', true,...
  'Selector',...
  {@(name) ~any(strcmp(name,{'Alaska','Hawaii'})), 'Name'});
geoshow(ax, states, 'FaceColor', [1 1 0.6])
framem off; gridm off; mlabel off; plabel off

%% Plot points
% Map the City locations with filled circular markers
geoshow(Cities,'Marker','o',...
    'MarkerFaceColor','b','MarkerEdgeColor','b','MarkerSize',5);

%% Plot connections
% Line Type: Great Circle
[lttrk,lntrk] = track('gc',coordsTSP,'degrees'); 
geoshow(lttrk,lntrk,'DisplayType','line','color','r','LineWidth',2)
% Display tour length
str = sprintf('Tour Length: %g km',lengthTSP);
title(str,'fonts',12)

