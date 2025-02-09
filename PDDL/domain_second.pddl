(define (domain car_wash_second)   
    (:requirements :strips :typing :adl :action-costs :numeric-fluents)
    
    (:types
        vehicle - object
        small_car - vehicle
        big_car - vehicle
        
        resource - object
        water soap wax - resource
        
        location - object
        station entrance exit - location
        
        program - object
        fast basic premium - program
    )

    ;; Predicate definitions for representing the state of the world
    (:predicates
        ;; TRUE if the vehicle is at a location
        (VEHICLE-AT ?v - vehicle ?l - location)
        
        ;; TRUE if a location is available
        (FREE-LOCATION ?l - location)
        
        ;; TRUE if locations are connected
        (CONNECTED ?l1 - location ?l2 - location)
        
        ;; Tracks which programs can be run on the station
        (STATION-COMPATIBILITY ?p - program ?s - station)
        
        ;; TRUE if a vehicle has been cleaned with premium program
        (CLEANING-DONE ?v - vehicle ?p - program)

        ;; TRUE if a station has water
        (STATION-HAS-WATER ?s - station ?w - water)

        ;; TRUE if a station has soap
        (STATION-HAS-SOAP ?s - station ?f - soap)

        ;; TRUE if a station has wax
        (STATION-HAS-WAX ?s - station ?c - wax)

        ;; TRUE if the vehicle is a small car
        (SMALL-CAR ?v - small_car)

        ;; TRUE if the vehicle is a big car
        (BIG-CAR ?v - big_car)

    )

    ;; Function definitions for cost tracking
    (:functions
        (move-cost) - number
        (refill-cost) - number
        (time-cost ?p - program) - number
        (total-cost) - number
        (HAS-RESOURCE-LEVEL-WATER ?s - station ?w - water) - number
        (HAS-RESOURCE-LEVEL-SOAP ?s - station ?f - soap) - number 
        (HAS-RESOURCE-LEVEL-WAX ?s - station ?c - wax) - number  
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
        :parameters (?v - vehicle ?s - station ?p - premium ?w - water ?f - soap ?c - wax)
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?s)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-WATER ?s ?w)
                (STATION-HAS-SOAP ?s ?f)
                (STATION-HAS-WAX ?s ?c)
                (or 
                    ;; Small car requirements
                    (and 
                        (SMALL-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
                        (>= (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 2)
                        (>= (HAS-RESOURCE-LEVEL-WAX ?s ?c) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (BIG-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 4)
                        (>= (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 3)
                        (>= (HAS-RESOURCE-LEVEL-WAX ?s ?c) 2)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-DONE ?v ?p)
                ;; Resource consumption based on vehicle type
                (when (SMALL-CAR ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
                        (decrease (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 2)
                        (decrease (HAS-RESOURCE-LEVEL-WAX ?s ?c) 1)
                    )
                )
                (when (BIG-CAR ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 4)
                        (decrease (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 3)
                        (decrease (HAS-RESOURCE-LEVEL-WAX ?s ?c) 2)
                    )
                )
                (increase (total-cost) (time-cost ?p))
            )
    )

    ;; Action for basic cleaning program
    (:action start-basic-cleaning
        :parameters (?v - vehicle ?s - station ?p - basic ?w - water ?f - soap)
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?s)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-WATER ?s ?w)
                (STATION-HAS-SOAP ?s ?f)
                (or 
                    ;; Small car requirements
                    (and 
                        (SMALL-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 2)
                        (>= (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (BIG-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
                        (>= (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 2)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-DONE ?v ?p)
                (when (SMALL-CAR ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 2)
                        (decrease (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 1)
                    )
                )
                (when (BIG-CAR ?v)
                    (and 
                        (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 3)
                        (decrease (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 2)
                    )
                )
                (increase (total-cost) (time-cost ?p))
            )
    )

    ;; Action for fast cleaning program
    (:action start-fast-cleaning
        :parameters (?v - vehicle ?s - station ?p - fast ?w - water)
        :precondition
            (and 
                (STATION-COMPATIBILITY ?p ?s)
                (VEHICLE-AT ?v ?s)
                (STATION-HAS-WATER ?s ?w)
                (or 
                    ;; Small car requirements
                    (and 
                        (SMALL-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 1)
                    )
                    ;; Big car requirements
                    (and 
                        (BIG-CAR ?v)
                        (>= (HAS-RESOURCE-LEVEL-WATER ?s ?w) 2)
                    )
                )
            )
        :effect 
            (and 
                (CLEANING-DONE ?v ?p)
                (when (SMALL-CAR ?v)
                    (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 1)
                )
                (when (BIG-CAR ?v)
                    (decrease (HAS-RESOURCE-LEVEL-WATER ?s ?w) 2)
                )
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
    
    ;; Action for refilling resources
    (:action refill-soap
        :parameters (?s - station ?f - soap)
        :precondition
                (STATION-HAS-SOAP ?s ?f)
        :effect 
            (and 
                (increase (HAS-RESOURCE-LEVEL-SOAP ?s ?f) 2)
                (increase (total-cost) (refill-cost))
            )
    )
    
    ;; Action for refilling resources
    (:action refill-wax
        :parameters (?s - station ?c - wax)
        :precondition
                (STATION-HAS-WAX ?s ?c)
        :effect 
            (and 
                (increase (HAS-RESOURCE-LEVEL-WAX ?s ?c) 1)
                (increase (total-cost) (refill-cost))
            )
    )
)