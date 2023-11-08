%% Model log likelihoods
logL = -[7222.49462337472; 7133.44076987160; 7220.72938166737; 7130.29543636256; 7158.82353277729; 7101.64453307381];
numParam = [5; 6; 6; 7; 7; 8];
numObs = 5355;

%AIC ARCH and ART-AGARCH are the best
aic = aicbic(logL,numParam);

%LR Test
[h,pValue,stat] = lratiotest(logL(6,1),logL(2,1),2)

% mu 0.04509736579	omega 0.01736931459	alpha1 0.2161424619	alpha2 2.87E-09 beta 0.8801792541 nu 6.319393518

sigma = 0.01736931459 / ( 1 - 0.5* 0.2161424619 - 0.5 * 2.87E-09 - 0.8801792541)


% 
parameter_vector = [0.04414766138; 6.17E-08; 0.1511151828; 4.45E-08; 0.8872054733; 0.05356848182; 0.00297733487;16.7753008];
[mu,omega,alpha1,alpha2,beta,phi1,phi2,nu] = extractParams(parameter_vector, model);

alpha = (alpha1+alpha2)/2;
phi   = (phi1+phi2)/2;
 
K   = 3 + 6/(nu - 4);

h = (omega + phi + alpha1*(phi1*K - phi)/2 + alpha2*(phi2*K - phi)/2 ) / (1-alpha-beta)