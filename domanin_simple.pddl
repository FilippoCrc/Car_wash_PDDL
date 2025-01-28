(define (domain car_wash)   
    (:requirements :strips :typing :adl :action-costs)
    
    (:types
        vehicle - object
        small_car - vehicle
        ;big_car moto - vehicle
        
        resource - object
        water - resource
        ;soap wax - resource
        water_level - object
        ;soap_level wax_level - object
        
        location - object
        station entrance exit - location
        
        program - object
        fast - program
        ;basic premium moto_prog - program
    )

    ;; Predicate definitions for representing the state of the world
    (:predicates
        ;; Tracks vehicle location
        (VEHICLE-AT ?v - vehicle ?l - location)
        
        ;; Tracks if a location is available
        (FREE-LOCATION ?l - location)
        
        ;; Tracks which programs can be run on the station
        (STATION-COMPATIBILITY ?p - program ?s - station)
        
        ;; Tracks if a vehicle has started cleaning at a station
        (CLEANING-STARTED ?v - vehicle ?s - station)
        
        ;; Tracks if a resource needs refilling at a station
        (RESOURCE-NEED-REFILL ?s - station ?r - resource)
        
        ;; Tracks resource levels at stations
        
        ;; Tracks which resources are installed at which stations
        (STATION-HAS-RESOURCE ?s - station ?r - resource)
        
        ;; Tracks station connections
        (CONNECTED ?l1 - location ?l2 - location)
    )

    ;; Function definitions for cost tracking
    (:functions
        (move-cost) - number
        (refill-cost) - number
        (time-cost ?p - program) - number
        (total-cost) - number
        (HAS-RESOURCE-LEVEL ?s - station ?r - resource) - number 
    )

    ;; Action for moving vehicles between locations
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
                (not (FREE-LOCATION ?to))
                (FREE-LOCATION ?from)
                (increase (total-cost) (move-cost))
            )
    )

    ;; Action for fast cleaning program
    (:action start-fast-cleaning
        :parameters (?v - vehicle ?s - station ?p - programs)  ; Removed water level parameters
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?s)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s water1)  ; Changed to directly check for water resource
                (>= (HAS-RESOURCE-LEVEL ?s water1) 3)  ; Directly check the water level
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                (decrease (HAS-RESOURCE-LEVEL ?s water1) 3)
                (increase (total-cost) (time-cost ?p))
            )
)

    ;; Action for refilling resources
    (:action refill
        :parameters (?s - station ?r - resource)
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