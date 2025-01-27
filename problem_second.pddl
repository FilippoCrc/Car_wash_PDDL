(define (problem car-wash-problem1)
    (:domain recharging-robots)
    
    (:objects
        ; Vehicles
        car1 car2 - small_car
        truck1 - big_car
        bike1 - moto
        
        ; Locations
        entrance1 - entrance
        exit1  - exit
        station1 station2 - station
        
        ; Resources
        water1 - water
        soap1 - soap
        wax1 - wax
        
        ; Programs
        fast1 - fast
        basic1 - basic
        premium1 - premium
        moto1 - moto_prog
    )
    
    (:init
        ; Base move and refill costs
        (= (move-cost) 2)
        (= (refill-cost) 1)
        (= (total-cost) 0)
        
        ; Program-specific costs
        ; Premium program
        (= (time-cost premium1) 8)
        
        ; Basic program
        (= (time-cost basic1) 5)
        
        ; Fast program
        (= (time-cost fast1) 3)
        
        ; Moto program
        (= (time-cost moto1) 2)
        
        ; Initial vehicle locations
        (VEHICLE-AT car1 entrance1)
        (VEHICLE-AT car2 entrance1)
        (VEHICLE-AT truck1 entrance1)
        (VEHICLE-AT bike1 entrance1)
        
        ; Initial free locations
        (FREE-LOCATION exit1)
        (FREE-LOCATION exit1)
        (FREE-LOCATION station1)
        (FREE-LOCATION station2)
        (FREE-LOCATION station3)
        
        ; Station resource installations
        (STATION-HAS-RESOURCE station1 water1)
        (STATION-HAS-RESOURCE station1 soap1)
        (STATION-HAS-RESOURCE station1 wax1)
        (STATION-HAS-RESOURCE station2 water1)
        (STATION-HAS-RESOURCE station2 soap1)
        (STATION-HAS-RESOURCE station3 water1)
        
        ; Initial resource levels
        (= (HAS-RESOURCE-LEVEL station1 water1) 7)
        (= (HAS-RESOURCE-LEVEL station1 soap1) 5)
        (= (HAS-RESOURCE-LEVEL station1 wax1) 3)
        (= (HAS-RESOURCE-LEVEL station2 water1) 5)
        (= (HAS-RESOURCE-LEVEL station2 soap1) 3)
        (= (HAS-RESOURCE-LEVEL station3 water1) 5)
        
        ; Location connections
        (CONNECTED entrance1 station1)
        (CONNECTED entrance1 station2)
        (CONNECTED entrance1 station3)
        (CONNECTED station1 exit1)
        (CONNECTED station2 exit1)
        (CONNECTED station3 exit1)
        
        ; Program compatibility
        (STATION-COMPATIBILITY premium1 car1)
        (STATION-COMPATIBILITY premium1 car2)
        (STATION-COMPATIBILITY premium1 truck1)
        (STATION-COMPATIBILITY basic1 car1)
        (STATION-COMPATIBILITY basic1 car2)
        (STATION-COMPATIBILITY basic1 truck1)
        (STATION-COMPATIBILITY fast1 car1)
        (STATION-COMPATIBILITY fast1 car2)
        (STATION-COMPATIBILITY fast1 truck1)
        (STATION-COMPATIBILITY moto1 bike1)
    )
    
    (:goal
        (and
            ; Goal: All vehicles should be cleaned and at exits
            (CLEANING-STARTED car1 station1)
            (VEHICLE-AT car1 exit1)
            (CLEANING-STARTED car2 station2)
            (VEHICLE-AT car2 exit1)
            (CLEANING-STARTED truck1 station1)
            (VEHICLE-AT truck1 exit1)
            (CLEANING-STARTED bike1 station3)
            (VEHICLE-AT bike1 exit1)
        )
    )
    
    (:metric minimize (total-cost) (time-cost))
)