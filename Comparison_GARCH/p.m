function [pdf] = p(e, nu)
%P is the p function on page 4, equation 2.2
% 

if nu <= 4
    disp('something wrong with nu');
end

pdf = (gamma((nu+1)/2) / (gamma(nu/2) * sqrt(pi*(nu-2)))) * (1 + e.^2 / (nu-2)).^(-(nu+1)/2);

end