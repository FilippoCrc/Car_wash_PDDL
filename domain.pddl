(define (domain recharging-robots)
(:requirements :strips :typing :adl :action-costs)
(:types
    
    vehicle -object
    small_car -vehicle
    big_car -vehicle
    moto -vehicle

    resource -object
    water -resource
    soap - resource
    wax -resource

    water_level -object
    soap_level -object
    wax_level -object

    location -object
    station -location
    entrance -location
    exit -location

    program -object
    fast - program
    basic -program
    premium -program
    moto_prog -program
)

(:predicates
    ;; v is the kind of vehicle, l is the kind of location, TRUE if the vehicle is at that location
    (VEHICLE-AT ?v - vehicle ?l - location)

    ;; TRUE if a vehicle v is free
    ;;(FREE-VEHICLE ?v - vehicle)

    ;;TRUE if a location l is free 
    (FREE-LOCATION ?l -object)

    ;;TRUE if a cleaning program is compatible with a vehicle type
    (STATION-COMPATIBILITY ?p - program ?v - vehicle) 

    ;;TRUE if a vehicle is being cleaned at a station
    (CLEANING-STARTED ?v - vehicle ?s - station)

    ;;TRUE if a station needs maintenance
    (RESOURCE-NEED-REFILL ?s – station ?r - resource)

     ;; Track resource levels at stations
    (HAS-RESOURCE-LEVEL ?s - station ?r - resource ?level - number)
    
    ;; Track if a station has the resource installed
    (STATION-HAS-RESOURCE ?s - station ?r - resource)

    ;; Track if a station has the resource installed
    (CONNECTED ?l1 - location ?l2 - location)


)


(:functions
    (move-cost) - number
    (refill-cost) - number
    (time-cost ?p - program) - number       ; Now takes a program parameter
    (total-cost) - number                ; Total cost of the plan
    (total-time-cost)                       ; Total time cost of the plan
)

;; Move the vehicle ?v from the location ?from to the location ?to 
(:action move
    :parameters (?v - vehicle ?from - location ?to - location)
    :precondition
        (and
            (VEHICLE-AT ?v ?from)
            (FREE-LOCATION ?to)
            (or (CONNECTED ?from ?to) (CONNECTED ?to ?from))
        )
    :effect
        (and
            (not (VEHICLE-AT ?v ?from))
            (VEHICLE-AT ?v ?to)
            (not ((FREE-LOCATION ?to)))
            (FREE-LOCATION ?from)
            (increase (total-cost) (move-cost))
        )
)

(:action start-premium-cleaning
    :parameters 
        (?v - vehicle ?s - station ?p - program)
    :precondition
        (and 
            (STATION-COMPATIBILITY premium ?v)
            (VEHICLE-AT ?v ?s)
            (STATION-HAS-RESOURCE ?s water)
            (STATION-HAS-RESOURCE ?s soap)
            (STATION-HAS-RESOURCE ?s wax)
            
            ;; Resource level checks - can vary by vehicle type
            (or 
                ;; Small car requirements
                (and 
                    (small_car ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 3)
                    (>= (HAS-RESOURCE-LEVEL ?s soap) 2)
                    (>= (HAS-RESOURCE-LEVEL ?s wax) 1))
                ;; Big car requirements
                (and 
                    (big_car ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 4)
                    (>= (HAS-RESOURCE-LEVEL ?s soap) 3)
                    (>= (HAS-RESOURCE-LEVEL ?s wax) 2))
                ;; Moto requirements
                (and 
                    (moto ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 2)
                    (>= (HAS-RESOURCE-LEVEL ?s soap) 1)
            )
        )
    :effect 
        (and 
            (CLEANING-STARTED ?v ?s)
            (OCCUPIED-STATION ?s)
            ;; Resource consumption based on vehicle type
            (when (small_car ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 3)
                    (decrease (HAS-RESOURCE-LEVEL ?s soap) 2)
                    (decrease (HAS-RESOURCE-LEVEL ?s wax) 1)))
            (when (big_car ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 4)
                    (decrease (HAS-RESOURCE-LEVEL ?s soap) 3)
                    (decrease (HAS-RESOURCE-LEVEL ?s wax) 2)))
            (when (moto ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 2)
                    (decrease (HAS-RESOURCE-LEVEL ?s soap) 1)
            (increase (total-time-cost) (time-cost ?p))
        )
)

(:action start-basic-cleaning
    :parameters 
        (?v - vehicle ?s - station ?p - program)
    :precondition
        (and 
            (STATION-COMPATIBILITY basic ?v)
            (VEHICLE-AT ?v ?s)
            (STATION-HAS-RESOURCE ?s water)
            (STATION-HAS-RESOURCE ?s soap)
            
            ;; Resource level checks - can vary by vehicle type
            (or 
                ;; Small car requirements
                (and 
                    (small_car ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 2)
                    (>= (HAS-RESOURCE-LEVEL ?s soap) 1)
                ;; Big car requirements
                (and 
                    (big_car ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 3)
                    (>= (HAS-RESOURCE-LEVEL ?s soap) 2)
                ;; Moto requirements
                (and 
                    (moto ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 1)
                    (>= (HAS-RESOURCE-LEVEL ?s soap) 1)
            )
        )
    :effect 
        (and 
            (CLEANING-STARTED ?v ?s)
            ;; Resource consumption based on vehicle type
            (when (small_car ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 3)
                    (decrease (HAS-RESOURCE-LEVEL ?s soap) 2)
            (when (big_car ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 4)
                    (decrease (HAS-RESOURCE-LEVEL ?s soap) 3)
            (when (moto ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 1)
                    (decrease (HAS-RESOURCE-LEVEL ?s soap) 1)
            (increase (total-time-cost) (time-cost ?p))
        )
)

(:action start-fast-cleaning
    :parameters 
        (?v - vehicle ?s - station ?p - program)
    :precondition
        (and 
            (STATION-COMPATIBILITY basic ?v)
            (VEHICLE-AT ?v ?s)
            (STATION-HAS-RESOURCE ?s water)
            
            ;; Resource level checks - can vary by vehicle type
            (or 
                ;; Small car requirements
                (and 
                    (small_car ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 1)
                ;; Big car requirements
                (and 
                    (big_car ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 2)
                ;; Moto requirements
                (and 
                    (moto ?v)
                    (>= (HAS-RESOURCE-LEVEL ?s water) 1)
            )
        )
    :effect 
        (and 
            (CLEANING-STARTED ?v ?s)
            ;; Resource consumption based on vehicle type
            (when (small_car ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 1)
            (when (big_car ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 2)
            (when (moto ?v)
                (and 
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 1)
            (increase (total-time-cost) (time-cost ?p))
        )
)

(:action refill
    :parameters 
        (?s - station ?r - resource)
    :precondition
        (and 
            (RESOURCE-NEED-REFILL ?s ?r)
            (STATION-HAS-RESOURCE ?s ?r)
        )
    :effect 
        (and 
            (increase (HAS-RESOURCE-LEVEL ?s ?r) 2)
            (increase (total-cost) (refill-cost))
        )
)
)

