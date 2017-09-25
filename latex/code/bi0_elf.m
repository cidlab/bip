% MATLAB/Octave code for Example 6

% Define the three models
e =  5;
s = 15;

h0 = @(x) (x<e & x>-e) / (2*e);
hn = @(x) normpdf(x,-e,s)*2.*(x<-e);
hp = @(x) normpdf(x,e,s)*2.*(x> e);

% Define the data and likelihood
d   =  -2;
n   = 100;
sem = sqrt((s^2 + s^2) / (2*n));

likelihood = @(x) normpdf(d,x,sem);

% Define the integrands and integrate
fn = @(x)likelihood(x).*hn(x);
mn = quadgk(fn,-inf,-e,'waypoints',[-e,e]);

f0 = @(x)likelihood(x).*h0(x);
m0 = quadgk(f0,-e,e,'waypoints',[-e,e]);

fp = @(x)likelihood(x).*hp(x);
mp = quadgk(fp,e,inf,'waypoints',[-e,e]);

ev = [mn,m0,mp];

% Apply Bayes' rule
eq19 = @(p,m) p.*m ./ sum(p.*m);

marchbanks = [.25,.50,.25];
granger    = [.15,.70,.15];
runcorn    = [.45,.10,.45];

eq19(marchbanks,ev)
% ans =    0.0061    0.9939    0.0000
eq19(granger,ev)
% ans =    0.0026    0.9974    0.0000
eq19(runcorn,ev)
% ans =    0.0122    0.9878    0.0000

