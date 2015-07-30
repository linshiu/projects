%% IE 513 - Final Project

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Name: NONLCON.m
% Date: 12/01/12
% Author: Luis S Lin
% Description: Nonlinear constraints for step-cone pulley weight minimization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%

function [g,h]=NONLCON(x,P)
x1 = x(1);
x2 = x(2);
x3 = x(3);
x4 = x(4);
x5 = x(5);

% Output speed of the step i
N = [P.N1 P.N2 P.N3 P.N4];

% Diameter of step i
d = [x1 x2 x3 x4];

% C(i): length of the belt needed to obtain output speed Ni 
C = zeros(1,4);
for i = 1:4
    C(i) = pi*d(i)/2*(1+N(i)/P.N)+(N(i)/P.N-1)^2/(4*P.a)+2*P.a;
end

% RT(i): ratio of the tension on the tight side of the belt to that
% on the slack side of the ith step
RT = zeros(1,4);
for i = 1:4
    RT(i) = exp(P.mu*(pi-2*asin((N(i)/P.N-1)*d(i)/(2*P.a))));
end

% PW(i): power transmitted at the ith step
PW = zeros(1,4);
for i = 1:4
    PW(i) = P.s*P.t*x5*(1-exp(-P.mu*(pi-2*asin((N(i)/P.N-1)*d(i)/(2*P.a)))))*pi*d(i)*N(i)/60;
end

% inequality constraints
g1 = P.R0 - RT(1);
g2 = P.R0 - RT(2);
g3 = P.R0 - RT(3);
g4 = P.R0 - RT(4);
g5 = P.P0 - PW(1);
g6 = P.P0 - PW(2);
g7 = P.P0 - PW(3);
g8 = P.P0 - PW(4);

g=[g1;g2;g3;g4;g5;g6;g7;g8];

% equality constraints
h1= C(1) - C(2);
h2= C(1) - C(3);
h3= C(1) - C(4);

h=[h1;h2;h3];


