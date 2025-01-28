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
        
        ;; Tracks which resources are installed at which stations
        (STATION-HAS-RESOURCE ?s - station ?r - resource)
        
        ;; Tracks station connections
        (CONNECTED ?l1 - location ?l2 - location)

        ;; TRUE if the vehicle is a small car
        (SMALL-CAR ?v - vehicle)

        ;; TRUE if the vehicle is a big car
        (BIG-CAR ?v - vehicle)

        ;; TRUE if the vehicle is a moto
        (MOTO ?v - vehicle)

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

;; Action for premium cleaning program
    (:action start-premium-cleaning
        :parameters (?v - vehicle ?s - station ?p - premium)
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?s)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s ?w water)
                (STATION-HAS-RESOURCE ?s ?s soap)
                (STATION-HAS-RESOURCE ?s ?wax wax)
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
        :parameters (?v - vehicle ?s - station ?p - basic)
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?v)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s water1)
                (STATION-HAS-RESOURCE ?s soap1)
                (or 
                    ;; Small car requirements
                    (and 
                        (SMALL-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water1) 2)
                        (>= (HAS-RESOURCE-LEVEL ?s soap1) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (BIG-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water1) 3)
                        (>= (HAS-RESOURCE-LEVEL ?s soap1) 2)
                    )
                    ;; Moto requirements
                    (and 
                        (MOTO ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water1) 1)
                        (>= (HAS-RESOURCE-LEVEL ?s soap1) 1)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                (when (SMALL-CAR ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s water1) 2)
                        (decrease (HAS-RESOURCE-LEVEL ?s soap1) 1)
                    )
                )
                (when (BIG-CAR ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s water1) 3)
                        (decrease (HAS-RESOURCE-LEVEL ?s soap1) 2)
                    )
                )
                (when (MOTO ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL ?s water1) 1)
                        (decrease (HAS-RESOURCE-LEVEL ?s soap1) 1)
                    )
                )
                (increase (total-cost) (time-cost ?p))
            )
    )

    ;; Action for fast cleaning program
    (:action start-fast-cleaning
        :parameters (?v - vehicle ?s - station ?p - fast)
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?v)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-RESOURCE ?s water1)
                (or 
                    ;; Small car requirements
                    (and 
                        (SMALL-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water1) 2)
                    )
                    ;; Big car requirements
                    (and 
                        (BIG-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water1) 2)
                    )
                    ;; Moto requirements
                    (and 
                        (MOTO ?v)
                        (>= (HAS-RESOURCE-LEVEL ?s water) 1)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-STARTED ?v ?s)
                (when (SMALL-CAR ?v)
                    (decrease (HAS-RESOURCE-LEVEL ?s water1) 1)
                )
                (when (BIG-CAR ?v)
                    (decrease (HAS-RESOURCE-LEVEL ?s water1) 2)
                )
                (when (MOTO ?v)
                    (decrease (HAS-RESOURCE-LEVEL ?s water1) 1)
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