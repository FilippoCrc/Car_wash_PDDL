(define (domain car_wash)   
    (:requirements :strips :typing :adl :action-costs)
    
    (:types
        vehicle - object
        small_car - vehicle
        
        resource - object
        water - resource
        
        location - object
        station entrance exit - location
        
        program - object
        fast - program
    )

    ;; Predicate definitions for representing the state of the world
    (:predicates
        ;; Tracks vehicle location
        (VEHICLE-AT ?v - vehicle ?l - location)
        
        ;; Tracks if a location is available
        (FREE-LOCATION ?l - location)
        
        ;; Tracks if a vehicle has started cleaning at a station
        (CLEANING-STARTED ?v - vehicle ?s - station)
        
        ;; Tracks if a resource needs refilling at a station
        (RESOURCE-NEED-REFILL ?s - station ?r - resource)

        ;; Tracks which resources are installed at which stations
        (STATION-HAS-RESOURCE ?s - station ?r - resource)
        
        ;; Tracks station connections
        (CONNECTED ?l1 - location ?l2 - location)
        
        (SMALL-CAR ?v - small_car)
        
        (BIG-CAR ?v)
    )

    ;; Function definitions for cost tracking
    (:functions
        (move-cost) - number
        (refill-cost) - number
        (time-cost ?p - program) - number
        (total-cost) - number
        (HAS-RESOURCE-LEVEL ?s - station ?w -water) - number 
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
        :parameters (?v - small_car ?s - station ?p - fast ?r - water)
        :precondition
            (and 
                (VEHICLE-AT ?v ?s)
                (SMALL-CAR ?v)
                (STATION-HAS-RESOURCE ?s ?r)
                (>= (HAS-RESOURCE-LEVEL ?s ?r) 3)
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                ;(decrease (HAS-RESOURCE-LEVEL ?s ?r) 3)
                (increase (total-cost) (time-cost ?p))
            )
)

    ;; Action for refilling resources
    (:action refill
        :parameters (?s - station ?r - water)
        :precondition
            (STATION-HAS-RESOURCE ?s ?r)
        :effect 
            (and 
                (increase (HAS-RESOURCE-LEVEL ?s ?r) 2)
                (increase (total-cost) (refill-cost))
            )
    )
)