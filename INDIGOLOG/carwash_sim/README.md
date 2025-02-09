Car-washing Indiogolog.

For execution: 
1) put "carwash_sim" in Indigolog-Main/examples
2) Launch "swipl config.pl examples/carwash_sim/main.pl"
3) Play with the 4 controllers

For Legality/projection tasks after launched swipl as in step 2 use one of the 3 command of test:

% Test 1. Checks if is it possible to 2 consecutive washes in the same station
indigolog(test_execution1). %command to run the test 
EXPECTED BEHAVIOUR -----> test will fail since the station haven't been reloaded

% Test 2. Checks if resources are empty after a 2 consecutive washes in the same station with only 1 reload
%indigolog([test_execution2,?(resources(2))]). %command to run the test
% EXPECTED BEHAVIOUR -----> test will fail since resources are empty after the second wash

% Test 3. Checks if after a cycle of wash, the car is still waiting
%indigolog([test_execution3,?(neg(car_waiting(1)))]). %command to run the test
% EXPECTED BEHAVIOUR -----> test will get success since the car has been washed and is no longer waiting
