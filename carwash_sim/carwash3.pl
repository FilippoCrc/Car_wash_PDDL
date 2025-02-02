    % --------------------------------------------------
    % Basic declarations and directives
    % --------------------------------------------------
    :- dynamic controller/1.
    :- dynamic water_state/2.
    :- discontiguous
        fun_fluent/1,
        rel_fluent/1,
        proc/2,
        causes_true/3,
        causes_false/3,
        causes_val/4,
        initially/2.
    cache(_) :- fail.

    % --------------------------------------------------
    % Fluents
    % --------------------------------------------------
    rel_fluent(car_waiting(_)).       % A car is waiting to be washed.
    rel_fluent(station_used(_)).      % Which station is in use.

    station_number(3).  % Number of stations.
    stn(N) :- station_number(M), between(1, M, N).

    fun_fluent(water(N)) :- stn(N).

    % --------------------------------------------------
    % Actions
    % --------------------------------------------------
    prim_action(start_wash(_,_)).
    prim_action(finish_wash(_,_)).

    % --------------------------------------------------
    % Causal Laws
    % --------------------------------------------------
    causes_true(start_wash(_,N), station_used(N), true).
    causes_false(start_wash(N,_), car_waiting(N), true).
    causes_false(finish_wash(_,N), station_used(N), true).

    % Revised causal law that updates the water level:
    causes_val(start_wash(_,M), water(M), NewVal,
        ( water_state(M, Current),
        NewVal is Current - 1,
        retract(water_state(M, Current)),
        assertz(water_state(M, NewVal))
        )
    ).
    % --------------------------------------------------
    % Exogenous actions and their possibilities
    % --------------------------------------------------
    exog_action(arrive(_)).
    poss(start_wash(N,M), and(and(neg(station_used(M)), car_waiting(N)),water(M)>8)).
    poss(finish_wash(_,M), station_used(M)).

    causes_true(arrive(N), car_waiting(N), true).


    % --------------------------------------------------
    % Procedures (for ordering actions)
    % --------------------------------------------------
    proc(serve_car(N,M),
        [ start_wash(N,M),
        finish_wash(N,M)
        ]
    ).

    proc(control(car_wash),
        while(some(n, car_waiting(n)),
            pi(n, [
                ?(car_waiting(n)),
                pi(m, [
                    ?(stn(m)),
                    ?(neg(station_used(m))),
                    
                    serve_car(n,m)
                ])
            ])
        )
    ).

    proc(control(car_wash_exog),
        prioritized_interrupts([
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
            interrupt(true, ?(wait_exog_action))
        ])
    ).

    % --------------------------------------------------
    % Initial State
    % --------------------------------------------------
    initially(station_used(M), false) :-
        station_number(N),
        between(1, N, M).

    initially(water(M), 10) :-
        station_number(N),
        between(1, N, M).

    initially(car_waiting(1), true).
    initially(car_waiting(2), true).
    initially(car_waiting(3), true).

    actionNum(X, X).

    % --------------------------------------------------
    % Definition of current_val/2 using water_state/2
    % --------------------------------------------------
    % current_val(+Fluent, -Value)
    % For our case, Fluent will be of the form water(M)
    current_val(water(M), Value) :-
        water_state(M, Value).

    % --------------------------------------------------
    % Initialization: Set up water_state from initial conditions.
    % --------------------------------------------------
    initialize_water :-
        forall(stn(M),
            ( initially(water(M), Val)
            -> assertz(water_state(M, Val))
            ;  true
            )
        ).

    :- initialization(initialize_water).

    % --------------------------------------------------
    % (Optional) A helper to update water_state explicitly if needed.
    % You may or may not require this depending on how your engine
    % uses causes_val/4.
    % --------------------------------------------------
    update_water_after_wash(M) :-
        water_state(M, OldVal),
        NewVal is OldVal - 1,
        retract(water_state(M, OldVal)),
        assertz(water_state(M, NewVal)).
