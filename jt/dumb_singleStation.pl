:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.

% There is nothing to do caching on (required becase cache/1 is static)
cache(_) :- fail.

max_car_number(5). %number of station

stn.
car(M) :- max_car_number(Z), between(1, Z, M).

% Fluents
rel_fluent(car_waiting(M)):-car(M).       % Car needs washing
rel_fluent(station_free).   %whick station is used
rel_fluent(soap). %soap level
%fun_fluent(soap(N)):- stn(N). %soap level

% Initial state
initially(station_free, true).
initially(soap, true).
initially(car_waiting(M), true) :-  between(1, 5, M).

% Action
prim_action(start_wash(_)).
prim_action(finish_wash(_)).
prim_action(reload_soap).

% Preconditions
%poss(start_wash(M, N), and(neg(station_used(N)), car_waiting(M))).
poss(start_wash(M), and(and(car_waiting(M),station_free),soap)).
%poss(start_wash(M), and(and((soap ,station_free)), car_waiting(M))).
poss(finish_wash(_), neg(station_free)).
poss(reload_soap, neg(soap)).

% Causal Laws
causes_false(start_wash(_), soap, true).  % Dopo il lavaggio, il sapone finisce
%causes_val(startwash(_,N), soap(N), V,  V is soap(N) -1).
causes_false(start_wash(_),station_free,true).
causes_false(start_wash(M), car_waiting(M), true).
causes_true(finish_wash(_), station_free, true).  % Clear station.
causes_true(reload_soap, soap, true).  % Dopo il lavaggio, il sapone finisce


% Exogenous action




% Exogenous effects
%causes_true(arrive(N), car_waiting(N), true).

% First fix the serve_car procedure to ensure proper sequencing
proc(serve_car(M), 
    [
        start_wash(M),
        finish_wash(M),
        reload_soap
    ]
).
proc(some_cars, some(m, car_waiting(m))).
proc(serve, pi(n, [?(car_waiting(n)),  serve_car(n)])).

%proc(serve, pi(n, [?(car_waiting(n)), serve_car(n)])).

% Easy, serve all cars with predefined procedure
proc(control(car_wash_dumb),
    while(some_cars, serve)
).




        
actionNum(X, X).
