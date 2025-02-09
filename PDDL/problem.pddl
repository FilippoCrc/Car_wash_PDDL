(define (problem car-wash-second)
    (:domain car_wash_second)
    
    (:objects
        ; Vehicles
        car1 - small_car
        car2 - small_car
        car3 - small_car
        big_car1 - big_car
        big_car2 - big_car
        moto1 - moto
        moto2 - moto
        
        ; Locations
        entrance1 - entrance
        exit1  - exit
        station1 - station
        station2 - station
        station3 - station
        station4 - station
        interior1 - interior

        
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
    
        ;(INTERIOR interior1)
        
        ; Base move and refill costs
        (= (move-cost) 2)
        (= (refill-cost) 1)
        (= (total-cost) 0)
        (= (interior-cost)5)
        
        (SMALL-CAR car1)
        (SMALL-CAR car2)
        (SMALL-CAR car3)
        (BIG-CAR big_car1)
        (BIG-CAR big_car2)
        (MOTO moto1)
        (MOTO moto2)
        
        ;(PROGRAM-TYPE premium1)
        ; Fast program
        (= (time-cost premium1) 9)
        (= (time-cost basic1) 6)
        (= (time-cost fast1) 3)
        
        ; Initial vehicle locations
        (VEHICLE-AT car1 entrance1)
        (VEHICLE-AT car2 entrance1)
        (VEHICLE-AT car3 entrance1)
        (VEHICLE-AT big_car1 entrance1)
        (VEHICLE-AT big_car2 entrance1)
        (VEHICLE-AT moto1 entrance1)
        (VEHICLE-AT moto2 entrance1)
        
        ; Initial free locations
        (FREE-LOCATION exit1)
        (FREE-LOCATION station1)
        (FREE-LOCATION station2)
        (FREE-LOCATION station3)
        (FREE-LOCATION station4)
        (FREE-LOCATION interior1)
    
        ; Station resource installations
        ;satation 1 up to premium
        (STATION-HAS-WATER station1 water1)
        (STATION-HAS-SOAP station1 soap1)
        (STATION-HAS-WAX station1 wax1)
        
        ;station 2 up to basic
        (STATION-HAS-WATER station2 water1)
        (STATION-HAS-SOAP station2 soap1)
        
        ; station 3 up to fast
        (STATION-HAS-WATER station3 water1)
        
        ;station 4 up to basic
        (STATION-HAS-WATER station4 water1)
        (STATION-HAS-SOAP station4 soap1)
        
        ; Initial resource levels
        (= (HAS-RESOURCE-LEVEL-WATER station1 water1) 5)
        (= (HAS-RESOURCE-LEVEL-SOAP station1 soap1) 2)
        (= (HAS-RESOURCE-LEVEL-WAX station1 wax1) 1)
        
        (= (HAS-RESOURCE-LEVEL-WATER station2 water1) 5)
        (= (HAS-RESOURCE-LEVEL-SOAP station2 soap1) 2)

        (= (HAS-RESOURCE-LEVEL-WATER station3 water1) 5)

        (= (HAS-RESOURCE-LEVEL-WATER station4 water1) 5)
        (= (HAS-RESOURCE-LEVEL-SOAP station4 soap1) 2)
        
        (CAN-BE-REFILLED station1)
        (CAN-BE-REFILLED station2)
        (CAN-BE-REFILLED station3)
        (CAN-BE-REFILLED station4)
        
        ; Location connections
        (CONNECTED entrance1 station1)
        (CONNECTED entrance1 station2)
        (CONNECTED entrance1 station3)
        (CONNECTED entrance1 station4)
        (CONNECTED station1 exit1)
        (CONNECTED station2 exit1)
        (CONNECTED station3 exit1)
        (CONNECTED station4 exit1)
        
        (CONNECTED station1 interior1)
        (CONNECTED station2 interior1)
        (CONNECTED station3 interior1)
        (CONNECTED station4 interior1)
        
        (CONNECTED interior1 exit1)
      
        ; Reverse connections for stations to interior
        ;(CONNECTED interior1 station1)
        ;(CONNECTED interior1 station2)
        ;(CONNECTED interior1 station3)
        ;(CONNECTED interior1 station4)
        ; Reverse connection for interior to exit
        ;(CONNECTED exit1 interior1)
        
        ; Program compatibility

        (STATION-COMPATIBILITY premium1 station1)
        (STATION-COMPATIBILITY basic1 station1)
        (STATION-COMPATIBILITY fast1 station1)

        (STATION-COMPATIBILITY basic1 station2)
        (STATION-COMPATIBILITY fast1 station2)

        (STATION-COMPATIBILITY fast1 station3)

        (STATION-COMPATIBILITY basic1 station4)
        (STATION-COMPATIBILITY fast1 station4)
        
        ;(INTERIOR-CLEAN car1)

    )
    
    (:goal
        (and
            ; Goal: All vehicles ready
            (VEHICLE-READY-INT car1 premium1)
            (VEHICLE-READY car2 basic1)
            (VEHICLE-READY car3 fast1)
            ;(VEHICLE-READY-INT big_car1 premium1)
            (VEHICLE-READY big_car1 premium1)
            (VEHICLE-READY big_car2 fast1)
            (VEHICLE-READY moto1 basic1)
            (VEHICLE-READY moto2 fast1)
            
        )
    )
    
    (:metric minimize (total-cost))
)