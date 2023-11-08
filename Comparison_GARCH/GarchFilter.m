function [ h ] = GarchFilter(parameter_vector, returns, model)
% Extract the sample size (make sure returns are a column vector)
n     = size(returns,1);
% Extract the stuff we need from the input arguments

[mu,omega,alpha1,alpha2,beta,phi1,phi2,nu] = ExtractParams(parameter_vector, model);

alpha = (alpha1+alpha2)/2;
phi   = (phi1+phi2)/2;
 
K   = 3 + 6/(nu - 4);
h0  = (omega + phi + alpha1*(phi1*K - phi)/2 + alpha2*(phi2*K - phi)/2 ) / (1-alpha-beta);
g0  = h0 - phi;

%% Run the GARCH filter
g(1,1) = g0; % change this perhaps fishy
h(1,1) = (g(1,1) + sqrt(g(1,1)^2 + 4*F(returns(1,1),mu,phi1,phi2)*(returns(1,1)-mu)^2))/2;

for t = 2:n
    g(t,1) = omega + F(returns(t-1,1),mu,alpha1,alpha2) * (returns(t-1,1) - mu)^2 + beta * h(t-1,1);
    h(t,1) = (g(t,1) + sqrt(g(t,1)^2 + 4*F(returns(t,1),mu,phi1,phi2)*(returns(t,1)-mu)^2))/2;
end
% Close the function

end
