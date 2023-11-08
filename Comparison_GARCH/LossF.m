function [val] = LossF(real, predict, loss_type)
%LOSS Summary of this function goes here
%   Detailed explanation goes here

n = size(real, 1);

if n == size(predict, 1)
    disp("real and predict size match! yay!");
end


if loss_type == "MSE1"
    val = (1/n) * sum((sqrt(real) - sqrt(predict)).^2);
elseif loss_type == "MSE2"
    val = (1/n) * sum((real - predict).^2);
elseif loss_type == "QLIKE"
    val = (1/n) * sum(log(predict) + real./predict);
elseif loss_type == "RLOG"
    val = (1/n) * sum((log(real./predict)).^2);
elseif loss_type == "MAE1"
    val = (1/n) * sum(abs(sqrt(real) - sqrt(predict)));
elseif loss_type == "MAE2"
    val = (1/n) * sum(abs(real - predict));
end

end

