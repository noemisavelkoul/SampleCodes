function [negativeLL] = NegativeLogLikelihood(parameter_vector, returns, model)
n = size(returns, 1);
%extract params
[mu,~,~,~,~,phi1,phi2,nu] = ExtractParams(parameter_vector, model);

%% Run the GARCH filter
[h] = GarchFilter(parameter_vector, returns, model);

% Collect a row vector of log likelihood per observation (this is the log
% of the pdf of a normal distribution)

LL = log(sqrt(h)./(h + F(returns, mu, phi1, phi2).*(((returns - mu).^2)./h)))...
    + log(p((returns - mu)./sqrt(h), nu));
% Put a negative sign in front and sum over all obserations
negativeLL = - sum(LL(1:end));   

% Close the function
end