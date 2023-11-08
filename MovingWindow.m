function [realized, prediction1, prediction2] = MovingWindow(x, model1, model2, p_type, d)
%MOVING_WINDOW Summary of this function goes here
%   Detailed explanation goes here


%% Load data
load('data');
n = size(returns, 1);
% window
w = floor(n*x);

%% For loop:
for t = 1:n-w-d-1
    % change window
    returnsX = returns(t  :t+w-1,1);
    returnsF = returns(t+w:t+w+d,1);
    % get parameters
    [ML_parameters1, ~, ~, last_h1] = Runner(model1, x, returnsX);
    [ML_parameters2, ~, ~, last_h2] = Runner(model2, x, returnsX);
    
    realized(t,1)   = RVol(returnsF, p_type);
    prediction1(t,1) = PVol(ML_parameters1, model1, d, returnsX(end,1), last_h1);
    prediction2(t,1) = PVol(ML_parameters2, model2, d, returnsX(end,1), last_h2);
    disp(t);
end

end

