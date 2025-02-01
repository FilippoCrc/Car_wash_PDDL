:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3,
    initially/2.
cache(_) :- fail.

% Fluents
rel_fluent(car_waiting(N)).       % Car needs washing

rel_fluent(station_used(N)).     %whick station is used
station_number(3). %number of station

stn(N) :- station_number(M), between(1, M, N). 

fun_fluent(soap(N)). %soap level
% Action
prim_action(start_wash(_,_)).
prim_action(finish_wash(_,_)).
allowed_soap(M) :- soap(M) > 2.
% Causal Laws
causes_val(start_wash(N,M), soap(M), newValue, newValue is soap(M)-1).
causes_true(start_wash(_,N),station_used(N),true).
causes_false(start_wash(N,_), car_waiting(N), true).
causes_false(finish_wash(_,N), station_used(N), true).  % Clear station.
% Exogenous action
exog_action(arrive(_)).      % Car arrives for washing

% Preconditions
poss(start_wash(N, M), 
     and(neg(station_used(M)), car_waiting(N), allowed_soap(M))).

poss(finish_wash(_,M), station_used(M)).



% Exogenous effects
causes_true(arrive(N), car_waiting(N), true).

% First fix the serve_car procedure to ensure proper sequencing
proc(serve_car(N,M), 
    [
        start_wash(N,M),
        
        finish_wash(N,M)
    ]
).

% Then fix the first controller
proc(control(car_wash),
    while(some(n, car_waiting(n)),
        pi(n, [
            ?(car_waiting(n)),        % Check if car n is waiting
            pi(m, [
                ?(stn(m)),            % Check if m is a valid station
                ?(neg(station_used(m))), % Check if station m is free
                serve_car(n,m)         % Serve the car
            ])
        ])
    )
).

% Fix the second controller's syntax
proc(control(car_wash_exog),
    prioritized_interrupts([
        % Medium priority: serve waiting cars
        interrupt(some(n, car_waiting(n)), 
            pi(n, [
                ?(car_waiting(n)),
                pi(m, [
                    ?(stn(m)),
                    ?(neg(station_used(m))),
                    serve_car(n,m)
                ])
            ])
        ),
        % Default: wait for new cars
        interrupt(true, ?(wait_exog_action))
    ])
).
% Initial state

initially(station_used(M), false) :- station_number(N), between(1, N, M).
initially(soap(M), 10) :- station_number(N), between(1, N, M).
initially(car_waiting(1), true).
initially(car_waiting(2), true).
initially(car_waiting(3), true).

% Action mapping
actionNum(X, X).