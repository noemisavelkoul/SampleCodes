function [mu,omega,alpha1,alpha2,beta,phi1,phi2,nu] = ExtractParams(parameter_vector, model)
%PARAMS extracting parameters
%   Detailed explanation goes here
    
if model == "SGARCH"
    mu     = parameter_vector(1,1);
    omega  = parameter_vector(2,1);
    alpha  = parameter_vector(3,1);
    beta   = parameter_vector(4,1);
    nu     = parameter_vector(5,1);
    
    alpha1 = alpha;
    alpha2 = alpha;
    phi1   = 0;
    phi2   = 0;
elseif model == "AGARCH"
    mu     = parameter_vector(1,1);
    omega  = parameter_vector(2,1);
    alpha1 = parameter_vector(3,1);
    alpha2 = parameter_vector(4,1);
    beta   = parameter_vector(5,1);
    nu     = parameter_vector(6,1);
    
    phi1   = 0;
    phi2   = 0;
elseif model == "SRT-SGARCH"
    mu     = parameter_vector(1,1);
    omega  = parameter_vector(2,1);
    alpha  = parameter_vector(3,1);
    beta   = parameter_vector(4,1);
    phi    = parameter_vector(5,1);
    nu     = parameter_vector(6,1);
    
    alpha1 = alpha;
    alpha2 = alpha;
    phi1   = phi;
    phi2   = phi;
elseif model == "SRT-AGARCH"
    mu     = parameter_vector(1,1);
    omega  = parameter_vector(2,1);
    alpha1 = parameter_vector(3,1);
    alpha2 = parameter_vector(4,1);
    beta   = parameter_vector(5,1);
    phi    = parameter_vector(6,1);
    nu     = parameter_vector(7,1);
    
    phi1   = phi;
    phi2   = phi;
elseif model == "ART-SGARCH"
    mu     = parameter_vector(1,1);
    omega  = parameter_vector(2,1);
    alpha  = parameter_vector(3,1);
    beta   = parameter_vector(4,1);
    phi1   = parameter_vector(5,1);
    phi2   = parameter_vector(6,1);
    nu     = parameter_vector(7,1);
    
    alpha1 = alpha;
    alpha2 = alpha;
elseif model == "ART-AGARCH"
    mu     = parameter_vector(1,1);
    omega  = parameter_vector(2,1);
    alpha1 = parameter_vector(3,1);
    alpha2 = parameter_vector(4,1);
    beta   = parameter_vector(5,1);
    phi1   = parameter_vector(6,1);
    phi2   = parameter_vector(7,1);
    nu     = parameter_vector(8,1);
end

end

