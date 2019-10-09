(reset)

(deffunction clear_screen()
	(for (bind ?i 0) (< ?i 50) (++ ?i)
    	(printout t crlf)    
    )    
)
(deftemplate house_data 
    (slot type)
    (slot room)
	(slot price)
    (slot location)
    (slot garage)
)
(deftemplate house_search_data
	(slot type)
    (slot room)
	(slot price)
    (slot location)
    (slot garage)        
    (slot rate)
)
(deftemplate user_data
    (slot name)
    (slot gender)
    (slot house_interest)
    (slot income)
    (slot pref_loc)
	(slot pref_type)
    (slot pref_size)      
)
(defquery view_houseNG
	?data <- (house_data (garage 0))    
)
(defquery view_houseWG

	?data <- (house_data {garage > 0})    
)
(defquery user_info
	(user_data
        (name ?userName)
    	(gender ?userGender)
        (house_interest ?houseInterest)
        (income ?userIncome)
        (pref_loc ?userPrefLoc)
        (pref_type ?userPrefType)
        (pref_size ?userPrefSize)    
    )      
)
(defquery get_house_data_retract
    ?house_data_retract <- (house_search_data)
)

(deffunction retract_search_house()
    (bind ?house (run-query* get_house_data_retract))
    (while (?house next)
     	(retract (?house getObject house_data_retract))   
    )
)

(defquery get_user_data_retract
    ?user_data_retract <- (user_data)
)

(deffunction retract_user_data()
   	(bind ?result (run-query* get_user_data_retract))
    (while (?result next)
        (retract (?result getObject user_data_retract))
    )
)

(defquery match_results
   (house_search_data
        (type ?type)
        (room ?room)
        (garage ?garage)
        (location ?location)
        (price ?price)
        (rate ?rate)
    )  
)
(deffunction view_house (?type)
    ;(printout t ?type)
    (if (eq ?type 1)then
    	(bind ?result (run-query* view_houseWG))
    elif(eq ?type 2)then
        (bind ?result (run-query* view_houseNG))
	)
    (bind ?idx 0)
    (while (?result next)
        
	    (if (eq ?idx 0)then
	        (if (eq ?type 1)then 
	    		(printout t "List of houses with garage(s): " crlf)
             	(printout t "=======================================================================" crlf)
                (printout t "| No.| Type            | Room  | Price     | Location        | Garage |" crlf)
                (printout t "=======================================================================" crlf)
	        
	        elif (eq ?type 2)then
	            (printout t "List of houses without garage: " crlf)
                (printout t "==============================================================" crlf)
                (printout t "| No.| Type            | Room  | Price     | Location        |" crlf)
                (printout t "==============================================================" crlf)
	        )
	    )
	    (bind ?h (?result getObject data))
       
	    (if (eq ?type 1)then
            
            (printout t (format nil "|%-1d.  | %-15s | %-5d | %-9d | %-15s | %-6d |"
            	(++ ?idx)
            	?h.type
                ?h.room
                ?h.price
                ?h.location
                ?h.garage        	
            ))
             
            (printout t crlf)
        	
       
         elif (eq ?type 2)then
            (printout t (format nil "|%-1d.  | %-15s | %-5d | %-9d | %-15s |"
            	(++ ?idx)
            	?h.type
                ?h.room
                ?h.price
                ?h.location
                ;?h.garage
            ))
            (printout t crlf)
        )
        
	) 
    (if (eq ?type 1) then
    	(printout t "=======================================================================" crlf)    
    
  	elif (eq ?type 2) then
        (printout t "==============================================================" crlf)	  
    )  
)
(deffunction add_house()
    (bind ?ah_input -1)
    (bind ?temp_type "")
    (bind ?temp_room -1)
    (bind ?temp_price -1)
    (bind ?temp_location "")
    (bind ?temp_garage -1)
    
    (clear_screen)
	(printout t "Type of house(s) to be added" crlf)
    (printout t "==========================" crlf)
    (printout t "1. House with garage" crlf)
    (printout t "2. House without garage" crlf)
    
  	(while (or (< ?ah_input 0) (> ?ah_input 2))
        (printout t ">> Choose [1..2 | 0 back to main menu]: ")
        (bind ?ah_input (read))
        (if (> ?ah_input 0) then
        	(while (and (neq ?temp_type "Cottage")(neq ?temp_type "Light House")(neq ?temp_type "Skyscraper"))
				(printout t "Input house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
				(bind ?temp_type (readline))
		 	)
		    
            
		    (while (or (not (numberp ?temp_room)) (< ?temp_room 1) (> ?temp_room 5))
		    	(printout t "Input room number [1-5]: ")
		        (bind ?temp_room (read))
		    )
		    
		    (while (or (not (numberp ?temp_price))  (< ?temp_price 1000) (> ?temp_price 500000))
		    	(printout t "Input house price [1000 - 500000] (dollars) : ")
		        (bind ?temp_price (read))        
		    )
		    
		    (while (and (neq ?temp_location "West Jakarta")(neq ?temp_location "North Jakarta")(neq ?temp_location "South Jakarta"))
				(printout t "Input house type [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
		    	(bind ?temp_location (readline))
		 	)
		        
		    (if (eq ?ah_input 1) then
		        (while (or (not (numberp ?temp_garage))  (< ?temp_garage 1) (> ?temp_garage 5))
		    		(printout t "Input garage number [1-5]: ")
		        	(bind ?temp_garage (read)) 
		   		)
		        (assert(house_data(type ?temp_type)(room ?temp_room)(price ?temp_price)(location ?temp_location)(garage ?temp_garage)))
		    
		   	elif (eq ?ah_input 2) then
		   		;assert data to house without garage
		        (assert(house_data(type ?temp_type)(room ?temp_room)(price ?temp_price)(location ?temp_location)(garage 0)))
		    )
		    
		    (printout t "House successfully added!")
		    (readline)       
        )
  	 )
    
)
(deffunction update_house()
    (bind ?uh_input -1)
 	(bind ?temp_type "")
    (bind ?temp_room -1)
    (bind ?temp_price -1)
    (bind ?temp_location "")
    (bind ?temp_garage -1)
    (bind ?mod 0)
    
    (clear_screen)
	(printout t "Type of houses to be updated" crlf)
    (printout t "============================" crlf)
    (printout t "1. House with garage" crlf)
    (printout t "2. House without garage" crlf)
    
  	(while (or (< ?uh_input 0) (> ?uh_input 2))
        (printout t ">> Choose [1..2 | 0 back to main menu]: ")
        (bind ?uh_input (read))
        (if (> ?uh_input 0) then
    	    (view_house ?uh_input)
    		(printout t ">> Modify: ")
    		(bind ?mod (read))
    
    		(while (and (neq ?temp_type "Cottage")(neq ?temp_type "Light House")(neq ?temp_type "Skyscraper"))
				(printout t "Input house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
    			(bind ?temp_type (readline))
 			)
    
		    (while (or (not (numberp ?temp_room))  (< ?temp_room 1) (> ?temp_room 5))
		    	(printout t "Input room number [1-5]: ")
		        (bind ?temp_room (read))
		    )
    
		    (while (or (not (numberp ?temp_price))  (< ?temp_price 1000) (> ?temp_price 500000) )
		    	(printout t "Input house price [1000 - 500000] (dollars) : ")
		        (bind ?temp_price (read))
		    )
		    
		    (while (and (neq ?temp_location "West Jakarta")(neq ?temp_location "North Jakarta")(neq ?temp_location "South Jakarta"))
				(printout t "Input house type [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
		    	(bind ?temp_location (readline))
		 	)
        	;(printout t ?uh_input)
		    (if (eq ?uh_input 1) then
		        (while (or (not (numberp ?temp_garage)) (< ?temp_garage 1) (> ?temp_garage 5))
		    		(printout t "Input garage number [1-5]: ")
		        	(bind ?temp_garage (read))
		   		)
                
	 			(bind ?result (run-query* view_houseWG))
	        		(for (bind ?i 0)(< ?i ?mod)(++ ?i)
    					(?result next)
	       		 	)
                
       		(modify (?result getObject data)(type ?temp_type)(room ?temp_room)(price ?temp_price)(location ?temp_location)(garage ?temp_garage))
    
    		elif (eq ?uh_input 2) then
   				(bind ?result (run-query* view_houseNG))
        		(for (bind ?i 0)(< ?i ?mod)(++ ?i)
        			(?result next)
        		)
       		(modify (?result getObject data)(type ?temp_type)(room ?temp_room)(price ?temp_price)(location ?temp_location)(garage 0))
    		) 
        )
  	 ) 
)
(deffunction delete_house()
    (bind ?dh_input -1)
    (bind ?delWG 0)
    (bind ?delNG 0)
    
    (clear_screen)
	(printout t "List of house to be deleted" crlf)
    (printout t "==========================" crlf)
    (printout t "1. House with a Garage" crlf)
    (printout t "2. House without a Garage" crlf)
    
    (while (or (not (numberp ?dh_input))  (< ?dh_input 0) (> ?dh_input 2))
        (printout t ">> Choose [1..2 | 0 back to main menu]: ")
        (bind ?dh_input (read))
    )
    
    (if (eq ?dh_input 1) then
        (view_house 1)
        (bind ?result (run-query* view_houseWG))      
        (printout t "Which house to be deleted [1..5 | 0 back to main menu]: ")
        (bind ?delWG (read))
        (for (bind ?i 0)(< ?i ?delWG)(++ ?i)
        	(?result next)
        )
        (retract (?result getObject data))
        
    elif (eq ?dh_input 2) then
        (view_house 2)
        (bind ?result (run-query* view_houseWG))      
        (printout t "Which house to be deleted [1..5 | 0 back to main menu]: ")
        (bind ?delWG (read))
        (for (bind ?i 0)(< ?i ?delWG)(++ ?i)
        	(?result next)
        )
        (retract (?result getObject data))
    )
    
)



(defrule search_house
    ?user <- (user_data (pref_size ?pref_size))
    ?h <- (house_data (garage ?garage&:			(or		(and(eq ?pref_size 0)(eq ?garage 0))	 (and(> ?pref_size 0)(> ?garage 0))	)		               ))               
 
    =>
    
    (bind ?rate 100)
    (bind ?flag 0)
 	(if(< ?user.income ?h.price) then
   		(bind ?rate (- ?rate 10))
        (++ ?flag)     
    )
    (if (neq ?user.house_interest ?h.type)then
        (bind ?rate (- ?rate 5))
        (++ ?flag)
  	)
    (if (neq ?user.pref_loc ?h.location)then
        (bind ?rate (- ?rate 10))
        (++ ?flag)
  	)
    (if (> ?user.pref_size 0)then
        (if (> ?user.pref_size ?h.garage)then
        	(bind ?rate (- ?rate 10))
            (++ ?flag)
  		)
    )
    ;ASSERT 
    (if(> ?user.pref_size 0)then
        (if(< ?flag 4)then
    		(assert (house_search_data(type ?h.type) (room ?h.room) (price ?h.price) (location ?h.location) (garage ?h.garage)(rate ?rate)))
        )
    elif(eq ?user.pref_size 0)then
        (if(< ?flag 3)then
    		(assert (house_search_data(type ?h.type) (room ?h.room) (price ?h.price) (location ?h.location) (garage ?h.garage)(rate ?rate)))
        )     
    )

        
)
(deffunction search_house_function()
	(bind ?name "")
    (bind ?gender "")
    (bind ?house_pref "")
    (bind ?income -1)
    (bind ?work_location "")
    (bind ?house_type "")
    (bind ?car_number -1)
    (bind ?garage_pref "")
    
    (while (or (< (str-length ?name) 3) (> (str-length ?name) 20))
        (printout t "Input your name [3 - 20 characters in length]: ")
   	 	(bind ?name (readline))
	)
    
    (while (and (neq ?gender "Male") (neq ?gender "Female")) 
        (printout t "Input your gender [Male | Female] (CASE-SENSITIVE): ")
        (bind ?gender (readline))
    )
    
    (while (and (neq ?house_pref "With Garage") (neq ?house_pref "Without Garage"))
    	(printout t "Input your house preference [With Garage | Without Garage] (CASE-SENSITIVE): ")    
    	(bind ?house_pref (readline))
    )
    
    (while (or (not (numberp ?income))  (< ?income 10000) (> ?income 500000))
    	(printout t "Input your income [10000 - 500000] (dollars): ")
        (bind ?income (read))    
    )
    
    (while (and (neq ?work_location "North Jakarta") (neq ?work_location "West Jakarta") (neq ?work_location "South Jakarta"))
    	(printout t "Input  your work location [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ") 
        (bind ?work_location (readline))   
    )
    
    (while (and (neq ?house_type "Cottage") (neq ?house_type "Light House") (neq ?house_type "Skyscraper"))
    	(printout t "Input your preferred house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?house_type (readline))    
    )
    
    (if (eq ?house_pref "With Garage") then
		(while (or (not (numberp ?car_number)) (< ?car_number 1) (> ?car_number 5))
            (printout t "Input the number of car(s) you own [1 - 5] (cars): ")
       	 	(bind ?car_number (read))
        )
    elif (eq ?house_pref "Without Garage") then
        (bind ?car_number 0)
	)
    (assert(user_data(name ?name)(gender ?gender)(house_interest ?house_pref)(income ?income)(pref_loc ?work_location)(pref_type ?house_type)(pref_size ?car_number)))
	(run)
)

(deffacts house_default
;HOUSES WITH GARAGE(s)
	(house_data(type "Cottage") (room 3) (price 30000) (location "West Jakarta") (garage 2))
	(house_data(type "Light House") (room 2) (price 7500) (location "South Jakarta") (garage 2))
	(house_data(type "Cottage") (room 3) (price 4500) (location "North Jakarta") (garage 1))
	(house_data(type "Skyscraper") (room 5) (price 175000) (location "South Jakarta") (garage 3))
	(house_data(type "Light House") (room 4) (price 7500) (location "West Jakarta") (garage 1))
	;HOUSES WITHOUT GARAGE
	(house_data(type "Light House")(room 3)(price 10000)(location "West Jakarta")(garage 0))
	(house_data(type "Cottage")(room 2)(price 5000)(location "North Jakarta")(garage 0))
	(house_data(type "Skyscraper")(room 4)(price 100000)(location "West Jakarta")(garage 0))
	(house_data(type "Light House")(room 3)(price 25000)(location "South Jakarta")(garage 0))
	(house_data(type "Cottage")(room 3)(price 7500)(location "South Jakarta")(garage 0))
)
(reset)

(bind ?command 0)
(bind ?command_view_house -1)
 	
(while (neq ?command 6)   

    (bind ?command 0)
    (bind ?command_view_house -1)
       
    (clear_screen)
    ;(facts)
    (printout t "Beli Rumah" crlf)
    (printout t "1. View House" crlf)
    (printout t "2. Add a New House" crlf)
    (printout t "3. Update House Details" crlf)
    (printout t "4. Delete a House" crlf)
    (printout t "5. Search Match" crlf)
    (printout t "6. Exit" crlf)
    
    (while (or (< ?command 1) (> ?command 6))  
	    (printout t ">> Input [1-6]: ")
	    (bind ?command (read))
    )
    
    (if (eq ?command 1) then
        (clear_screen)
    	(printout t "List of house to be viewed" crlf)
        (printout t "==========================" crlf)
        (printout t "1. House with a Garage" crlf)
        (printout t "2. House without a Garage" crlf)
       	
        (while (or (< ?command_view_house 0) (> ?command_view_house 2))
	        (printout t ">> Choose [1..2 | 0 back to main menu]: ")
	        (bind ?command_view_house (read))
        )
	        ;COMMAND 1 - VIEW HOUSES
	        (if (eq ?command_view_house 1) then
	            (clear_screen)
	            (view_house ?command_view_house)
            	(readline)	            
	        elif (eq ?command_view_house 2) then
	         	(clear_screen)
	            (view_house ?command_view_house)
            	(readline)	 
	        )
        ;COMMAND 2 - ADD HOUSE
	    elif (eq ?command 2) then
	    (clear_screen)
        (add_house)
        
        ;COMMAND 3 - UPDATE HOUSE
        elif (eq ?command 3) then
        (clear_screen)
        (update_house)
        
        ;COMMAND 4 - DELETE HOUSE
        elif (eq ?command 4) then
        (clear_screen)
        (delete_house)
        
        elif (eq ?command 5) then
        (clear_screen)
        (search_house_function)
        (new Template)
        (retract_user_data)
        (retract_search_house)
    )
)






