

% Interface to the outside world via read and write
execute(A, SR) :- ask_execute(A, SR).
exog_occurs(_) :- fail.

% Locations
station(entrance).
station(exit).
station(wash_station).

% Actions
prim_action(move(Vehicle, From, To)) :- station(From), station(To), From \= To.
prim_action(refill(Resource)).
prim_action(start_wash(Vehicle, Program)).

% Fluents
prim_fluent(at(Vehicle, Location)) :- station(Location).
prim_fluent(available(Program)).
prim_fluent(resource_available(Resource)).
prim_fluent(total_cost).

% Causal laws
causes_val(move(Vehicle, From, To), at(Vehicle, To), true).
causes_val(refill(Resource), resource_available(Resource), true).
causes_val(start_wash(Vehicle, Program), available(Program), false).
causes_val(move(Vehicle, From, To), total_cost, C, C is total_cost + 1).

% Preconditions of prim actions
poss(move(Vehicle, From, To), at(Vehicle, From)).
poss(refill(Resource), true).
poss(start_wash(Vehicle, Program), [at(Vehicle, wash_station), available(Program)]).

% Initial state
initially(at(car1, entrance)).
initially(available(fast1)).
initially(at(wash_station, wash_station)).
initially(at(exit, exit)).
initially(resource_available(water1)).
initially(total_cost, 0).

% Definitions of complex actions

% Move vehicle to a specified location with cost minimization
proc(go_to(Vehicle, Location), while(neg(at(Vehicle, Location)), move(Vehicle, _, Location))).

% Perform the car wash process at the station
proc(wash_car(Vehicle, Program), [go_to(Vehicle, wash_station), start_wash(Vehicle, Program)]).

% Complete the entire car wash cycle and exit
proc(car_wash_cycle(Vehicle, Program), [wash_car(Vehicle, Program), go_to(Vehicle, exit)]).

% Minimize cost by choosing the best action sequence
proc(minimize_cost(Max), 
    ndet(handle_reqs(Max), pi(m, [m is Max + 1, minimize_cost(m)]))).

proc(handle_reqs(Max),
    ndet( [?(and(neg(at(car1, wash_station)), Max >= total_cost)), go_to(car1, exit)],
          pi(m, [?(m is Max - 1), wash_car(car1, fast1), handle_reqs(m)]))).


% Main control procedure optimized for cost
proc(control, minimize_cost(0)).
