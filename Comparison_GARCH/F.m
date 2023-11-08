function [num] = F(rt, mu, phi1, phi2)
%F defined section 3.3
%   Detailed explanation goes here

num = (rt <= mu) * phi1 + (rt > mu) * phi2;

end

