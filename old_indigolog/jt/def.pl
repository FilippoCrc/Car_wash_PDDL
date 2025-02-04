:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.

% There is nothing to do caching on (required becase cache/1 is static)
cache(_) :- fail.

max_station_number(3). %number of station
car_number(5). %number of station

stn(N) :- max_station_number(Y), between(1, Y, N). 

% Fluents
rel_fluent(car_waiting(_)).       % Car needs washing
rel_fluent(station_free(N)):- stn(N).    %whick station is used
rel_fluent(soap(N)):- stn(N). %soap level

% Initial state
initially(station_free(M), true) :- max_station_number(N), between(1, N, M).
initially(soap(N), true) :- stn(N).
initially(car_waiting(M), true) :- car_number(N), between(1, N, M).

% Actions
prim_action(start_wash(_,_)).
prim_action(finish_wash(_,_)).
prim_action(reload_soap(_)).

% Preconditions
poss(start_wash(M, N), and(and((soap(N)),(station_free(N))), car_waiting(M))).
poss(finish_wash(_,N), neg(station_free(N))).
poss(reload_soap(N), neg(soap(N))).

% Causal Laws
causes_false(start_wash(_, N), soap(N), true).  % Dopo il lavaggio, il sapone finisce
causes_false(start_wash(_,N),station_free(N),true).
causes_false(start_wash(M,_), car_waiting(M), true).
causes_true(finish_wash(_,N), station_free(N), true).  % Clear station.
causes_true(reload_soap(N), soap(N), true).  % Dopo il lavaggio, il sapone finisce

% Exogenous action
exog_action(arrive(_)).      % New Car arrives for washing
causes_true(arrive(M), car_waiting(M), true).


prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).




%############################################ 
% CONTROLLERS:
% ###########################################

% Shortcuts(checks):
proc(some_cars, some(m, car_waiting(m))). %checks for car waiting
proc(some_stn, some(n, station_free(n))). %checks for station free
proc(free_station_with_soap, some(n, and(station_free(n), soap(n)))). % checks for station free with soap


% Controller 1. Simply apply the same plan for all the cars (same station and reload every time)

% Fixed sequence
proc(serve_car(M,N), 
    [
        start_wash(M,N),
        finish_wash(M,N),
        reload_soap(N)

    ]
).
proc(serve_dumb_fixed, pi(n, [?(car_waiting(n)),  serve_car(n,2)])). %find a car waiting to serve at station 2 (fixed)

proc(control(car_wash_dumb_fixed),
    while(some_cars, serve_dumb_fixed)
).

% Controller 2. Serve cars with the predefined plan, but choose the first station avaible --> so always use the first station
proc(serve_dumb, pi(n, [?(car_waiting(n)),   pi(m, [?(station_free(m)),serve_car(n,m)])])).

proc(control(car_wash_dumb),
    while(some_cars, serve_dumb)
).


% Controller 3. boh ora non funziona piu 
proc(serve_car_dynamic2(M, N), 
    [
        if(neg(soap(N)), reload_soap(N)),  % Se non c'è sapone, ricarica prima
        ?(station_free(N)), % Assicura che la stazione sia libera
        start_wash(M, N), 
        finish_wash(M, N)
    ]
).


proc(serve_c, pi(n, [?(car_waiting(n)),   pi(m, [?(station_free(m)),serve_car_dynamic2(n,m)])])).

proc(control(car_wash_smart_boh),
    while(some_cars, serve_c)
).

% Controller 4. Serve cars but choosing the smartest action, i.e. id station with already soap use them, otherwise choose the first avaible station and reload soap recharge one and use it 
% (so when all stations without soap it will use always the first station recharging it every time)


proc(serve_car_dynamic(M),
    pi(n, [
        ?(soap(n)),  % Cerca una stazione libera con sapone
        start_wash(M, n),
        finish_wash(M, n)
    ])
).

proc(serve_car_smart(M), 
    if(free_station_with_soap,
        serve_car_dynamic(M),   % Se esiste una stazione libera con sapone, la usa
        pi(n, [                % Altrimenti trova la prima disponibile e ricarica
            ?(station_free(n)), 
            reload_soap(n),
            start_wash(M, n),
            finish_wash(M, n)
        ])
    )
).

proc(control(car_wash_smart),
    while(some_cars,
        pi(n, [
            ?(car_waiting(n)),  % Trova una macchina in attesa
            serve_car_smart(n)  % Usa la versione intelligente per scegliere la stazione
        ])
    )
).



% Controller 5. Serve cars as for controller 4, but with exogenous actions (arrive) that insert a new car in the pipeline

proc(control(car_wash_exog),
    prioritized_interrupts([
        interrupt(some_cars,
            pi(n, [
                ?(car_waiting(n)),  % Trova una macchina in attesa
                serve_car_smart(n)  % Usa la versione intelligente per scegliere la stazione
            ])
    ),
    interrupt(true, ?(wait_exog_action))
])
).



debug_stations :- 
    findall(N, stn(N), Stazioni),
    writeln(Stazioni).



debug_waiting_cars :- 
    findall(M, car_waiting(M), Cars),
    writeln('Macchine in attesa: '), writeln(Cars).
      
actionNum(X, X).



