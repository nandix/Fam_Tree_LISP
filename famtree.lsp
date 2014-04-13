#|
    File:       famtree.lsp
    
    Authors:    Daniel Nix and Caitlin Taggart
    
    Professor:  Dr. Weiss

    Course:     Programming Languages (CSC461)
    
    Brief:      This program analyzes a family tree and
                then waits for family requests from the 
                user. i.e. (children 'sam) would ouput a
                list of sam's children in the database.

    Date Due:   Apr 16, 2014

    Desc:       

    Usage:      clisp -repl famtree.lsp <database_file_name>

    Bugs:       There are tons of no bugs 

    Todos:      Everythigg
|#

#| 
	This is the structures for each person. It includes the person's name,
	sex, children and parents.
|#
; Define the database "person" structure
(defstruct person name sex children parents)

; split_record takes a string from input and converts it to 
; individual fields to be used in person structs
(defun split_record( str_rec )
    
)

; The Read Database function reads in the database file line by line
; and returns the result as a list of structs for the global var
; "*database*"
(defun read_database (db_file)
    ;Try and open the file
    (setf fin (open db_file :if-does-not-exist nil)) 

    ;If we couldn't open file, return from the function and notify user
    (when (null fin) 
        ;Return an error message
        (return-from read_database
            (format nil "Could not open ~a" db_file)
        )
    )
    
    ; initialize an empty list to hold the database
    (setf database_records '() )

    ;If we opened the file, we want to read each line into its own 
    ; person struct
    (do ((record (read fin nil) (read fin nil)))          ; read the first line into record
        ((null record) (close fin))            ; exit when file is read

        ; Delcare the person structure to store in database list
        (setf record (make-person :name (first record) :sex (second record)
                      :children (third record) :parents (fourth record)  ))  ; Create a person struct

        ; Append the newly created person record to the list
        (setf database_records (append database_records (cons record nil)))  ; print what we read
        
    )
    ; Return the database list
    (return-from read_database database_records) 
)

; Look up a database structure based on name
(defun lookup ( name next )
    (cond
        ((null next) ()) 
        ;Check if the nth person's name equals the name of 
        ; the person we are trying to find
        ((equalp (person-name (car next)) name) (car next) )
        (t (lookup name (cdr next)))
    )
)





#|
	Description: This functions takes a list of people, and filters out
		everything but the males. 

	Param: L - The list of people to filter. 
	Returns: A list with only male people in it
|#
(defun male_filter (L)
	(apply #'append (mapcar #'(lambda(x) (if (equalp (person-sex (lookup x *database*)) 'male) (list x) nil)) L))	
)




#|
	Description: This functions takes a list of people, and filters out
		everything but the female. 

	Param: L - The list of people to filter. 
	Returns: A list with only female people in it
|#
(defun female_filter (L)
	(apply #'append (mapcar #'(lambda(x) (if (equalp (person-sex (lookup x *database*)) 'female) (list x) nil)) L))	
)


; Return a list of a person's children
(defun children (name)
    (person-children (lookup name *database*))
)



#|
	Description: This fuctions finds a person in the *database* global and
	finds their parents by looking at the parents part of the list. 

	Param: p - The person to find the parents of 
	Returns: A list of the parents of p
|#
(defun parents (p)
	(person-parents (lookup p *database*))
)



#|
	Description: This function finds the parents of a person and then filters 
	to get only males, thus finding the fathers of the person. 

	Param: p - the person to find the fathers of
	Returns: A list with the fathers in it
|#
(defun fathers (p)
	(male_filter (parents p))
)



#|
	Description: This function finds the parents of a person and then filters 
	to get only female, thus finding the mothers of the person. 

	Param: p - the person to find the mothers of
	Returns: a list of the mothers
|#
(defun mothers (p)
	(female_filter (parents p))
)

(defun daughters (p)
    (female_filter (children p))
)

(defun sons (p)
    (male_filter (children p))
)



#|
	Description: This function finds the grandparents of a person by finding
		the parents of the parents. 

	Param: p - the person to find the grandparents
	Returns: A list of grandparents
|#
(defun grandparents (p)
	(setf L (parents p))	; Find the parents of p	
    (apply #'append (mapcar #'parents L) )	;Find the parents of each parent
)



#|
	Description: This function finds the grandmothers of a person by first 
		finding the grandparents of them, and then picking only the females. 

	Param: p - the person to find the grandmothers of 
	Returns: A list with the grandmothers in it
|#
(defun grandmothers (p)
	(female_filter (grandparents p))
)




#|
	Description: This function finds the grandfathers of a person by finding 
		the grandparents of that person, and then taking only the males. 

	Param: p - the person to find the grandfathers of
	Returns: A list with the grandfaters in it
|#
(defun grandfathers (p)
	(male_filter (grandparents p))
)


(defun grandchildren (p)
    (setf L (children p))
    (apply #'append (mapcar #'children L) )
)

(defun grandsons (p)
    (male_filter (grandchildren p))
)

(defun granddaughters (p)
    (female_filter (grandchildren p))
)



#|
	Description: This function finds the siblings of a person, by finding the 
		parents first, and then finding the children of the parents

	Param: p - the person to find the siblings of 
	Returns: A list with the siblings in it 
|#
(defun siblings (p)
	(setf L (parents p))	;Find the parents of p
	(setf L (apply #'append (mapcar #'children L)))	;find the children of parents
	(setf L (remove-duplicates L))		;remove duplicate people
	(remove	p L)					;remove yourself from the list
)



#|
	Description: This function find the sisters of a peron, by first finding 
		this siblings, and then filtering out the females

	Param: p - the person to find the sisters of 
	Returns: A list with the sisters in it
|#
(defun sisters (p)
	(female_filter (siblings p))
)




#|
	Description: This function finds the brothers of a person by first finding 
		the siblings of that person, and then filtering out the males. 

	Param: p - the person to find the brothers 
	Returns: A list with the brothers in it
|#
(defun brothers (p)
	(male_filter (siblings p))
)




#|
	Description: This function finds both the neices and nephews by finding 
		the person's siblings, and then finding their children. 

	Param: p - the person to find the nieces and nephews of 
	Returns: A list with nieces and nephews
|#
(defun nieces_nephews (p)
	(setf L (siblings p)) ;find the siblings of the person
	(setf L (apply #'append (mapcar #'children L)))	;find the chilren of siblings
	(setf L (remove-duplicates L))	;remove any duplicates 
)





#|
	Description: This function finds the nieces of the person by finding both
		the neices and nephews, and then filtering out the females

	Param: p - the person to find the nieces of 
	Returns: A list with the nieces in it
|#
(defun nieces (p)
	(female_filter (nieces_nephews p))
)




#|
	Description: This function finds the nephews of a person by first finding
		the nieces and nephews, and then filtering out the males

	Param: p - the person to find the nephews of 
	Returns: A list with the nephews in it
|#
(defun nephews (p)
	(male_filter (nieces_nephews p))
)



#|
	Description: This function finds the cousins of a person by first finding 
		the parents and then finding the siblings of the parents, and then
		finding the children of those people (some aunts and uncles)

	Param: p - the person to find the cousins of 
	Returns: A list with the cousins
|#
(defun cousins (p)
	(setf L (parents p))	;find the parents of the person 
	(setf L (apply #'append (mapcar #'siblings L)))	;find their siblings
	(setf L (apply #'append (mapcar #'children L)))	;find their children
	(remove-duplicates L)	;remove any duplicates
)



#|
	Description: This function finds the female cousins of a person by finding 
		the cousins of a person, and then filtering out the females 

	Param: p - the person to find the female cousins of 
	Returns: A list with the females cousins in it
|#
(defun female-cousins (p)
	(female_filter (cousins p))
)



#|
	Description: This function finds the male cousins of a person by finding 
		the cousins of a person, and then filtering out the males 

	Param: p - the person to find the male cousins of 
	Returns: A list with the males cousins in it
|#
(defun male-cousins (p)
	(male_filter (cousins p))
)




#|
	Description: This function finds the aunts and uncles of a person by first
		finding the siblings of their parents, and then by finding the husbands
		or wives of those people. This is done by taking the children of the
		first found, and then looking at their parents.  

	Param: p - the person to find the aunts and uncles of 
	Returns: A list with the aunts and uncles in it 
|#
(defun aunts_uncles (p)
	(setf L (parents p))	;find the parents of the person
	(setf L (apply #'append (mapcar #'siblings L)))	;find initial aunts and uncles
	(setf L2 (apply #'append (mapcar #'children L))) ;find cousins
	(setf L2 (apply #'append (mapcar #'parents L2))) ;find their parents 
	(setf L (append L L2))	;combine the two lists
	(remove-duplicates L)	;remove and duplicates found 
)


#|
	Description: This function finds the aunts by first finding both the aunts
		and uncles and then filtering out the females

	Param: p - the person to find the aunts of 
	Returns: A list with the aunts of in it 
|#
(defun aunts (p)
	(female_filter (aunts_uncles p))
)


#|
	Description: This function finds the uncles of a person by first finding 
		the aunts and uncles and then filtering out the male

	Param: p - the person to find the unlces of 
	Returns: A list with the uncles in it
|#
(defun uncles (p)
	(male_filter (aunts_uncles p))
)



(defun ancestors_and_me (p)
    (setf L (parents p))     ;Find the parents of p
    (if (null L) (return-from ancestors_and_me (cons p nil))) ;If no ancestors, return "myself"    
    (setf L2 (apply #'append (mapcar #'ancestors_and_me L)));Recursively call the ancestors on the parents
    (setf L (append L L2 (cons p nil)))
    (remove-duplicates L)
    
)

(defun descendants_and_me (p)
    (setf L (children p))
    (if (null L) (return-from descendants_and_me (cons p nil)))    
    (setf L2 (apply #'append (mapcar #'descendants_and_me L)))
    (setf L (append L L2 (cons p nil)))
    (remove-duplicates L)
)

(defun descendants (me)
    (setf d_and_me (descendants_and_me me))
    (remove me d_and_me)
)

(defun ancestors (me)
    (setf a_and_me (ancestors_and_me me))
    (remove me a_and_me)
)

; Read the database into *database* using the read_database function
(setf *database* (read_database (car (last *ARGS*))))

(defun male-ancestors (p)
    (male_filter (ancestors p))
)

(defun female-ancestors (p)
    (female_filter (ancestors p))
)

(defun male-descendants (p)
    (male_filter (descendants p))
)

(defun female-descendants (p)
    (female_filter (descendants p))
)
