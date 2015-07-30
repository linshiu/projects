%% IE 513 - Final Project

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: FUN.m
% Date: 12/01/12
% Author: Luis S Lin
% Description: Objective for step-cone pulley weight minimization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
function [f]=FUN(x,P)
x1 = x(1);
x2 = x(2);
x3 = x(3);
x4 = x(4);
x5 = x(5);

% Objective = sum of weights (weight = density * volume of cylinder)

f = P.p*x5*pi/4*(x1^2*(1+(P.N1/P.N)^2)+...
                 x2^2*(1+(P.N2/P.N)^2)+...
                 x3^2*(1+(P.N3/P.N)^2)+...
                 x4^2*(1+(P.N4/P.N)^2));