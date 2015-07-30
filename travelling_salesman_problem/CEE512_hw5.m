%% CEE 512 - Homework 5

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File Name: CEE512_hw5.m
% Date: 3/27/12
% Author: Luis S Lin
% Description: immplement the farthest insertion heuristic to solve for 
%              a TSP tour given the latitude and longitdue of the 150 
%              largest cities in the US
%
% Files used:     1) costmatrixTSP.m
%                    1.1) findDistance.m
%                 2) findTSP.m
%                    2.1) insertToTour.m
%                 3) plotTSP.m
%
% Inputs: data150cities.xls : excel data file containing latitude and 
%         longitude (absolute value, so N and W assumed) of the largest 
%         150 cities in the US       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp('File Name: CEE512_hw5.m')
disp('Date: 3/27/12')
disp('Author: Luis S Lin')
disp('Description: TSP solution for 150 cities using farthest insertion heuristic')
disp('Inputs: data150cities.xls ')

%% Set folder path wehre all data files located
tic
folderPath='E:\2011-2012 ~ UIUC\SPRING 2012\CEE 512 - Logistics Systems Analysis\Homework\Hw5';
path(path,folderPath);

%% Read data file
filePath ='E:\2011-2012 ~ UIUC\SPRING 2012\CEE 512 - Logistics Systems Analysis\Homework\Hw5\data150cities.xls';
coords = xlsread(filePath,'B4:C153');

%% Create a distance/cost matrix 
c=costmatrixTSP(coords);

%% Find TSP
[tourTSP,lengthTSP]=findTSP(c);

%% Plot TSP
plotTSP(coords,tourTSP,lengthTSP)
disp('Computer Platform: HP Z210, Intel Core i5-2400 3.1Ghz Processors, 8GB of RAM')
disp('Software: 7.12.0.635 (R2011a) , 64-bit (win64)')
disp('Total computation time for program CEE512_hw5.m')
toc
