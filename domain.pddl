(define (domain car_wash)   
    (:requirements :strips :typing :adl :action-costs)
    
    (:types
        vehicle - object
        small_car big_car moto - vehicle
        
        resource - object
        water soap wax - resource
        water_level soap_level wax_level - object
        
        location - object
        station entrance exit - location
        
        program - object
        fast basic premium moto_prog - program
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
        (HAS-RESOURCE-LEVEL ?s - station ?r - resource)
        
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

    ;; Action for premium cleaning program
    (:action start-premium-cleaning
        :parameters (?v - vehicle ?s - station ?p - program 
                    ?wpre - water_level ?wpost - water_level 
                    ?spre - soap_level ?spost - soap_level 
                    ?waxpre - wax_level ?waxpost - wax_level)
        :precondition
            (and 
                (STATION-COMPATIBILITY premium ?s)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s station ?wpre water)
                (STATION-HAS-RESOURCE ?s station ?spre soap)
                (STATION-HAS-RESOURCE ?s station ?waxpre wax)
                (or 
                    ;; Small car requirements
                    (and 
                        (small_car ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?wpre water) 3)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?spre soap) 2)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?waxpre wax) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (big_car ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?wpre water) 4)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?spre soap) 3)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?waxpre wax) 2)
                    )
                    ;; Moto requirements
                    (and 
                        (moto ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?wpre water) 2)
                        (>= (HAS-RESOURCE-LEVEL ?s station ?spre soap) 1)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                ;; Resource consumption based on vehicle type
                (when (small_car ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?wpre water) 3)
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?spre soap) 2)
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?waxpre wax) 1)
                    )
                )
                (when (big_car ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?wpre water) 4)
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?spre soap) 3)
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?waxpre wax) 2)
                    )
                )
                (when (moto ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?wpre water) 2)
                        (decrease (HAS-RESOURCE-LEVEL ?s station ?spre soap) 1)
                    )
                )
                (increase (total-cost) (time-cost ?p))
            )
    )

    ;; Action for basic cleaning program
    (:action start-basic-cleaning
        :parameters (?v - vehicle ?s - station ?p - program)
        :precondition
            (and 
                (STATION-COMPATIBILITY basic ?v)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s water)
                (STATION-HAS-RESOURCE ?s soap)
                (or 
                    ;; Small car requirements
                    (and 
                        (small_car ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 2)
                        (>= (HAS-RESOURCE-LEVEL ?s soap) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (big_car ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 3)
                        (>= (HAS-RESOURCE-LEVEL ?s soap) 2)
                    )
                    ;; Moto requirements
                    (and 
                        (moto ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 1)
                        (>= (HAS-RESOURCE-LEVEL ?s soap) 1)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                (when (small_car ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s water) 2)
                        (decrease (HAS-RESOURCE-LEVEL ?s soap) 1)
                    )
                )
                (when (big_car ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s water) 3)
                        (decrease (HAS-RESOURCE-LEVEL ?s soap) 2)
                    )
                )
                (when (moto ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s water) 1)
                        (decrease (HAS-RESOURCE-LEVEL ?s soap) 1)
                    )
                )
                (increase (total-cost) (time-cost ?p))
            )
    )

    ;; Action for fast cleaning program
    (:action start-fast-cleaning
        :parameters (?v - vehicle ?s - station ?p - program)
        :precondition
            (and 
                (STATION-COMPATIBILITY fast ?v)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s water)
                (or 
                    ;; Small car requirements
                    (and 
                        (small_car ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (big_car ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 2)
                    )
                    ;; Moto requirements
                    (and 
                        (moto ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 1)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                (when (small_car ?v)
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 1)
                )
                (when (big_car ?v)
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 2)
                )
                (when (moto ?v)
                    (decrease (HAS-RESOURCE-LEVEL ?s water) 1)
                )
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