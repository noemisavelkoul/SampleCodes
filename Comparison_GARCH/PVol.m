function [vol] = PVol(parameter_vector, model, d, lastreturn, last_h)
%FORECAST Summary of this function goes here
%   Detailed explanation goes here

%  extract params
[mu,omega,alpha1,alpha2,beta,phi1,phi2,nu] = ExtractParams(parameter_vector, model);

alpha = (alpha1+alpha2)/2;
phi   = (phi1+phi2)/2;
K     = 3 + 6/(nu - 4);
h     = (omega + phi + alpha1*(phi1*K - phi)/2 + alpha2*(phi2*K - phi)/2 ) / (1-alpha-beta);

if lastreturn < mu
    gNext = omega + (alpha1) * (lastreturn - mu)^2 + beta * last_h;
else
    gNext = omega + (alpha2) * (lastreturn - mu)^2 + beta * last_h;
end
        

one = d*(mu^2+h+phi*(K-1));
two = ((1-(alpha+beta)^d) / (1-alpha-beta)) * (gNext+phi-h);
vol = sqrt(one + two);


end

