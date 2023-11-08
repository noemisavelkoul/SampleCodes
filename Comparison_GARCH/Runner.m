function [ML_parameters, ML_NegativeLogL, implied_epsilon, last_h] = Runner(model, x, returns)
%RUNNER Summary of this function goes here
%   Detailed explanation goes here

%% Specify starting values for the paramsters mu, omega, alpha, beta
% startingvalues = [ mu, omega, alpha1, alpha2 , beta, phi1, phi2, nu]
format short

if model == "SGARCH"
    % startingvalues = [ mu, omega, alpha, beta, nu]
    startingvalues = [mean(returns); var(returns)/20; 0.10; 0.85; 6];
    lowerbound = [-inf,  0,  0,  0,  5];
    upperbound = [ inf,inf,inf,inf,inf];
elseif model == "AGARCH"
    % startingvalues = [ mu, omega, alpha1, alpha2, beta, nu]
    startingvalues = [mean(returns); var(returns)/20; 0.10; 0.10; 0.85; 6];
    lowerbound = [-inf,  0,  0,  0,  0,  5];
    upperbound = [ inf,inf,inf,inf,inf,inf];
elseif model == "SRT-SGARCH"
    % startingvalues = [ mu, omega, alpha, beta, phi, nu]
    startingvalues = [mean(returns); var(returns)/20; 0.10; 0.85; 0.10; 6];
    lowerbound = [-inf,  0,  0,  0,  0,  5];
    upperbound = [ inf,inf,inf,inf,inf,inf];
elseif model == "SRT-AGARCH"
    % startingvalues = [ mu, omega, alpha1, alpha2 , beta, phi, nu]
    startingvalues = [mean(returns); var(returns)/20; 0.10; 0.10; 0.85; 0.10; 6];
    lowerbound = [-inf,  0,  0,  0,  0,  0,  5];
    upperbound = [ inf,inf,inf,inf,inf,inf,inf];
elseif model == "ART-SGARCH"
    % startingvalues = [ mu, omega, alpha, beta, phi1, phi2, nu]
    startingvalues = [mean(returns); var(returns)/20; 0.10; 0.85; 0.10; 0.10; 6];
    lowerbound = [-inf,  0,  0,  0,  0,  0,  5];
    upperbound = [ inf,inf,inf,inf,inf,inf,inf];
elseif model == "ART-AGARCH"
    % startingvalues = [ mu, omega, alpha1, alpha2 , beta, phi1, phi2, nu]
    startingvalues = [mean(returns); var(returns)/20; 0.10; 0.10; 0.85; 0.10; 0.10; 6];
    lowerbound = [-inf,  0,  0,  0,  0,  0,  0,  5];
    upperbound = [ inf,inf,inf,inf,inf,inf,inf,inf];
end

%% GARCH: Check the negative log likelihood at the starting values
format bank
ans1 = NegativeLogLikelihood(startingvalues, returns, model);

%% Do maximum likelihood optimsiation 
% Matlab likes to minimize, so we minimise the *negative* log likelihood

% Clear any pre-existing options
clearvars options

% Load some options
options  =  optimset('fmincon');
options  =  optimset(options , 'TolFun'      , 1e-6);
options  =  optimset(options , 'TolX'        , 1e-6);
options  =  optimset(options , 'Display'     , 'off');
options  =  optimset(options , 'Diagnostics' , 'off');
options  =  optimset(options , 'LargeScale'  , 'off');
options  =  optimset(options , 'MaxFunEvals' , 10^6) ;
options  =  optimset(options , 'MaxIter'     , 10^6) ;

% Parameter lower bound and upper bound (note that sigma^2,alpha1,alpha2,beta,phi1,phi2,nu must be
% positive)
% lowerbound = [-inf,0,0,0,0,0,0,5];
% upperbound = [inf,inf,inf,inf,inf];

% Perform ML maximisation (we actually minimize the negative likelihood)
format short
[ML_parameters,ML_NegativeLogL] = fmincon('NegativeLogLikelihood', startingvalues,[],[],[],[],lowerbound,upperbound,[],options,returns,model);
mu_ML = ML_parameters(1);
[sigmasquared] = GarchFilter(ML_parameters,returns,model);
last_h = sigmasquared(end, 1);

%% Check that the implied shocks have approximately the desired characteristics (mean zero, variance one)
mu_ML = ML_parameters(1);
implied_epsilon=(returns-mu_ML)./sqrt(sigmasquared);
[mean(implied_epsilon);var(implied_epsilon)]; % should be roughly [0;1] if the model is correct
end

