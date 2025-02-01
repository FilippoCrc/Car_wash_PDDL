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
rel_fluent(car_waiting(_)).       % Car needs washing
rel_fluent(current_car(_)).       % Which car is being washed

% Actions
prim_action(start_wash(_)).
prim_action(finish_wash).

% Causal Laws
causes_true(start_wash(N), current_car(N), true).
causes_false(start_wash(N), car_waiting(N), true).
causes_false(finish_wash, current_car(_), true).  % Clear any car

% Exogenous action
exog_action(arrive(_)).      % Car arrives for washing

% Preconditions
poss(start_wash(N), and(neg(current_car(_)), car_waiting(N))).
poss(finish_wash, current_car(_)).



% Exogenous effects
causes_true(arrive(N), car_waiting(N), true).

% Procedures
proc(serve_car(N), [
    start_wash(N),
    finish_wash
]).

proc(control(car_wash),
    while(some(n, car_waiting(n)),
        pi(n, [
            ?(car_waiting(n)),
            serve_car(n)
        ])
    )
).
proc(control(car_wash_exog),
    prioritized_interrupts([
       
        % Medium priority: serve waiting cars (like floor buttons)
        interrupt(some(n, car_waiting(n)), 
            pi(n, [
                ?(car_waiting(n)),
                serve_car(n)
            ])
        ),
        
        % Default: wait for new cars (like elevator's wait_exog_action)
        interrupt(true, ?(wait_exog_action))
    ])
).

% Initial state
initially(current_car(_), false).
initially(car_waiting(1), true).
initially(car_waiting(3), true).

% Action mapping
actionNum(X, X).