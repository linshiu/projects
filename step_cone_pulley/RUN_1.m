%% IE 513 - Final Project

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: RUN1.m
% Date: 12/01/12
% Author: Luis S Lin
% Description: Step-cone pulley weight minimization
%
% Files used: FUN.m
%             NONLCON.m
%
% Inputs: 1) Algorithm: 'sqp', 'active-set','interior-point'
%
% Outputs: 1) Optimal Design Variables
%          2) Optimal Function Value
%          3) Iterations Table
%          4) Functiona Value and Evaluations vs Iterations Plot
%          5) Computation Time
%          6) Exit Flag
%          7) Multipliers
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Information
clear all %clear out old variables
clc % clear command window

fprintf('\n-------------------------------------\n');
fprintf('\nAuthor: Luis S Lin\n');
fprintf('\nCourse: IE 513 - Optimal System Design\n');
fprintf('\nFinal Project: Step-cone pulley optimization \n');
fprintf('\n-------------------------------------\n');


%%
clear all %clear out old variables

% Options and Common Parameters for fmincon
% algorithm & output
% large scale optimization (usually should be set to OFF)
options = optimset('algorithm','sqp','Display','iter',... 
    'LargeScale','off','MaxFunEvals',3000,'MaxIter',1000,...
    'Diagnostics','on','PlotFcns',{@optimplotx,@optimplotfval,...
    @optimplotfirstorderopt,@optimplotfunccount,...
    @optimplotconstrviolation,@optimplotstepsize}); 

% matrix/vectors for defining linear constraints (not used)
A=[]; b=[]; Aeq=[]; beq=[]; 

% lower bounds on the problem
lb = [ 40 40 40 40 16]; 

% upper bounds on the problem (not used)
ub = [500 500 500 500 100]; 

% initial starting point
% x1 = d1 (diameter of step 1 in mm)
% x2 = d2 (diameter of step 2 in mm)
% x3 = d3 (diameter of step 3 in mm)
% x4 = d4 (diameter of step 4 in mm)
% x5 = w  (width of the belts and steps in mm)

%x0 = [70 96.25 128.333 154 50]; 
x0 = [50 50 50 50 80]; 


% parameters
P.N  = 350;   % Input speed of the shaft (RPM)
P.N1 = 750;   % Output speed of the step 1 (RPM) 
P.N2 = 450;   % Output speed of the step 2 (RPM)
P.N3 = 250;   % Output speed of the step 3 (RPM)
P.N4 = 150;   % Output speed of the step 4 (RPM)
P.a  = 3;     % Center distance between shafts (m)
P.p  = 7200;  % Density of the material of the pulleys (kg/m^3)
P.mu = 0.35;  % Coefficient of friction between belt and pulley
P.s  = 1.75;  % Maximum allowable stress in the belt (MPa)
P.t  = 8;     % Thickness of the belt (mm)
P.P0 = 0.75;  % Minimum required power transmitted by the step pulley (hp)
P.R0 = 2;     % Minimum required ratio of the tension on the tight side of
              % the belt to that on the slack side

% convert parameters to appropriate units
P.a  = P.a*(10^3);  % (mm)
P.p  = P.p*(10^-9); % (kg/mm^3)
P.s  = P.s*(10^6)*(10^-3); % (kg/mm^2)
P.P0 = P.P0*745.6998*(10^6); % (kg*mm^2/s^3)

% solve
tic % Start clock

[xopt,fval,exitflag,ouput,lambda] = fmincon(@(x)FUN(x,P),...
    x0,A,b,Aeq,beq,lb,ub,@(x)NONLCON(x,P),options)

toc  % End Clock
