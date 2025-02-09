(define (problem car-wash-simple)
    (:domain car_wash_simple)
    
    (:objects
        ; Vehicles
        car1 - small_car
        
        ; Locations
        entrance1 - entrance
        exit1  - exit
        station1 - station
        
        ; Resources
        water1 - water
        
        ; Programs
        fast1 - fast
    )
    
    (:init
        ; Base move and refill costs
        (= (move-cost) 2)
        (= (refill-cost) 1)
        (= (total-cost) 0)
        
        ; Fast program
        (= (time-cost fast1) 3)
        
        ; Initial vehicle locations
        (VEHICLE-AT car1 entrance1)
        
        ; Initial free locations
        (FREE-LOCATION exit1)
        (FREE-LOCATION station1)
        
        ; Station resource installations
        (STATION-HAS-WATER station1 water1)
        
        ; Initial resource levels
        (= (HAS-RESOURCE-LEVEL-WATER station1 water1) 1)
        
        ; Location connections
        (CONNECTED entrance1 station1)
        (CONNECTED station1 exit1)

    )
    
    (:goal
        (and
            ; Goal: All vehicles should be cleaned and at exits
            (CLEANING-DONE car1 fast1)
            (VEHICLE-AT car1 exit1)
        )
    )
    
    (:metric minimize (total-cost))
)