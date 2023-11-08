function [vol] = RVol(returns, p_type)
%   RVOL is a function to calculate realised volatility
%   p_type is the type of target variable, look at page 11 of
%   case description, 1 is "Classic" and 2 is "Oxford"

if p_type == "Classic"
    vol = sqrt(sum(returns.^2));
elseif p_type == "Oxford"
    vol = sqrt(1.5*sum(returns.^2));
else 
    disp("incorrect type!");
end

end

