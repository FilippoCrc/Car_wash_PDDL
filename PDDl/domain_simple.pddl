(define (domain car_wash_simple)   
    (:requirements :strips :typing :adl :action-costs :numeric-fluents)
    
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
        (CLEANING-DONE ?v - vehicle ?s - station)

        ;; Tracks which resources are installed at which stations
        (STATION-HAS-WATER ?s - station ?w - water)
        
        ;; Tracks station connections
        (CONNECTED ?l1 - location ?l2 - location)
    )

    ;; Function definitions for cost tracking
    (:functions
        (move-cost) - number
        (refill-cost) - number
        (time-cost ?p - program) - number
        (total-cost) - number
        (HAS-RESOURCE-LEVEL-WATER ?s - station ?w - water) - number 
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
        :parameters (?v - vehicle ?s - station ?p - fast ?w - water)
        :precondition
            (and 
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-WATER ?s ?w)
                (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
            )
        :effect 
            (and 
                (CLEANING-DONE ?v ?p)
                (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
                (increase (total-cost) (time-cost ?p))
            )
)

    ;; Action for refilling resources
    (:action refill-water
        :parameters (?s - station ?w - water)
        :precondition
                (STATION-HAS-WATER ?s ?w)
        :effect 
            (and 
                (increase (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
                (increase (total-cost) (refill-cost))
            )
    )
)