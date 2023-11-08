%% Clear all data 
clear
close all

%% Load the directory (change this to where you have saved this file)
pad = '/Users/cemsirin/Desktop/qf-ect-case/matlab demo/allGarchs';
cd(pad);

load('data');
n       = size(returns, 1);
x       = 0.995;

%% Specify your model
% choose model: SGARCH, AGARCH, SRT-SGARCH, SRT-AGARCH, ART-SGARCH, ART-AGARCH
models = ["SGARCH"; "AGARCH"; "SRT-SGARCH"; "SRT-AGARCH"; "ART-SGARCH"; "ART-AGARCH"];
model = models(6,1);

%fav garch model
model1 = models(2,1);
model2 = models(6,1);

% choose x between 0 and 1
%[ML_parameters, ML_NegativeLogL, implied_epsilon, last_h] = Runner(model, 1, returns);
%size(ML_parameters);
%ML_parameters

% choose day 5 or 21
d = 21;

% choose p_type "Classic" or "Oxford"
p_types = ["Classic"; "Oxford"];
p_type  = p_types(1,1);


[realized, prediction1, prediction2] = MovingWindow(x, model1, model2, p_type, d);
