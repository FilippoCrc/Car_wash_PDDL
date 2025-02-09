(define (problem car-wash-second)
    (:domain car_wash_second)
    
    (:objects
        ; Vehicles
        car1 - small_car
        big_car1 - big_car
        car2 - small_car
        
        ; Locations
        entrance1 - entrance
        exit1 - exit
        exit2 - exit
        exit3 - exit
        station1 - station
        station2 - station

        
        ; Resources
        water1 - water
        soap1 - soap
        wax1 - wax

        
        ; Programs
        premium1 - premium
        basic1 - basic
        fast1 - fast
    )
    
    (:init
        ; Base move and refill costs
        (= (move-cost) 2)
        (= (refill-cost) 1)
        (= (total-cost) 0)
        
        (SMALL-CAR car1)
        (SMALL-CAR car2)
        (BIG-CAR big_car1)
        
        ;(PROGRAM-TYPE premium1)
        ; Fast program
        (= (time-cost premium1) 9)
        (= (time-cost basic1) 6)
        (= (time-cost fast1) 3)
        
        ; Initial vehicle locations
        (VEHICLE-AT car1 entrance1)
        (VEHICLE-AT car2 entrance1)
        (VEHICLE-AT big_car1 entrance1)
        
        ; Initial free locations
        (FREE-LOCATION exit1)
        (FREE-LOCATION exit2)
        (FREE-LOCATION exit3)
        (FREE-LOCATION station1)
        (FREE-LOCATION station2)
        
        ; Station resource installations
        (STATION-HAS-WATER station1 water1)
        (STATION-HAS-SOAP station1 soap1)
        (STATION-HAS-WAX station1 wax1)

        (STATION-HAS-WATER station2 water1)
        (STATION-HAS-SOAP station2 soap1)
        
        ; Initial resource levels
        (= (HAS-RESOURCE-LEVEL-WATER station1 water1) 0)
        (= (HAS-RESOURCE-LEVEL-SOAP station1 soap1) 0)
        (= (HAS-RESOURCE-LEVEL-WAX station1 wax1) 0)
        
        (= (HAS-RESOURCE-LEVEL-WATER station2 water1) 0)
        (= (HAS-RESOURCE-LEVEL-SOAP station2 soap1) 0)
        
        ; Location connections
        (CONNECTED entrance1 station1)
        (CONNECTED entrance1 station2)
        (CONNECTED station1 exit1)
        (CONNECTED station1 exit2)
        (CONNECTED station1 exit3)
        
        (CONNECTED station2 exit1)
        (CONNECTED station2 exit2)
        (CONNECTED station2 exit3)
        
        ; Program compatibility

        (STATION-COMPATIBILITY premium1 station1)
        (STATION-COMPATIBILITY basic1 station1)
        (STATION-COMPATIBILITY fast1 station1)

        (STATION-COMPATIBILITY basic1 station2)
        (STATION-COMPATIBILITY fast1 station2)


    )
    
    (:goal
        (and
            ; Goal: All vehicles should be cleaned and at exits
            (CLEANING-DONE car1 premium1)
            (CLEANING-DONE big_car1 fast1)
            (CLEANING-DONE car2 basic1)
            (VEHICLE-AT car1 exit1)
            (VEHICLE-AT big_car1 exit2)
            (VEHICLE-AT car2 exit3)
        )
    )
    
    (:metric minimize (total-cost))
)