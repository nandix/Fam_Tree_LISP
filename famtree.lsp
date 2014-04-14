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

    Bugs:       There are no known bugs 

    Todos:      Double check function names
|#

#| 
	This is the structures for each person. It includes the person's name,
	sex, children and parents.
|#
; Define the database "person" structure
(defstruct person name sex children parents)

; split_record takes a string from input and converts it to 
; individual fields to be used in person structs
;(defun split_record( str_rec )
    
;)

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

#|
    Name: children
	Description: This fuctions finds the children of a person

	Param: p - The person to find the children of 
	Returns: A list of the parents of p
|#

(defun children (name)
    (person-children (lookup name *database*))
)



#|
    Name: parents
	Description: This fuctions finds a person in the *database* global and
	finds their parents by looking at the parents part of the list. 

	Param: p - The person to find the parents of 
	Returns: A list of the parents of p
|#
(defun parents (p)
	(person-parents (lookup p *database*))
)



#|
    Name: fathers
	Description: This function finds the parents of a person and then filters 
	to get only males, thus finding the fathers of the person. 

	Param: p - the person to find the fathers of
	Returns: A list with the fathers in it
|#
(defun fathers (p)
	(male_filter (parents p))
)



#|
    Name: mothers
	Description: This function finds the parents of a person and then filters 
	to get only female, thus finding the mothers of the person. 

	Param: p - the person to find the mothers of
	Returns: a list of the mothers
|#
(defun mothers (p)
	(female_filter (parents p))
)


#|
    Name: daughters
	Description: This function finds the children of a person and then filters 
	to get only female, thus finding the daughters. 

	Param: p - the person to find the daughters of
	Returns: a list of the daughters
|#
(defun daughters (p)
    (female_filter (children p))
)

#|
    Name: sons
	Description: This function finds the children of a person and then filters 
	to get only male, thus finding the sons of the person. 

	Param: p - the person to find the sons of
	Returns: a list of the sons
|#
(defun sons (p)
    (male_filter (children p))
)



#|
    Name: grandparents
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
    Name: grandmothers
	Description: This function finds the grandmothers of a person by first 
		finding the grandparents of them, and then picking only the females. 

	Param: p - the person to find the grandmothers of 
	Returns: A list with the grandmothers in it
|#
(defun grandmothers (p)
	(female_filter (grandparents p))
)




#|
    Name: grandfathers
	Description: This function finds the grandfathers of a person by finding 
		the grandparents of that person, and then taking only the males. 

	Param: p - the person to find the grandfathers of
	Returns: A list with the grandfaters in it
|#
(defun grandfathers (p)
	(male_filter (grandparents p))
)

#|
    Name: grandchildren
    Description: This function finds the grandchildren of a person by finding
         the children of p's children. 
 
    Param: p - the person to find the grandchildren
    Returns: A list of grandchildren
|#

(defun grandchildren (p)
    (setf L (children p))
    (apply #'append (mapcar #'children L) )
)

#|
    Name: grandsons
    Description: This function finds the grandsons of a person by finding
          the children of p's children and running the male filter. 
 
    Param: p - the person to find the grandsons of
    Returns: A list of grandsons
|#
(defun grandsons (p)
    (male_filter (grandchildren p))
)

#|
    Name: granddaughters
    Description: This function finds the granddaughters of a person by finding
                the children of p's children and running the female filter. 
 
    Param: p - the person to find the granddaughters of
    Returns: A list of granddaughters
|#
(defun granddaughters (p)
    (female_filter (grandchildren p))
)



#|
    Name: siblings
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
    Name: sisters
	Description: This function find the sisters of a peron, by first finding 
		this siblings, and then filtering out the females

	Param: p - the person to find the sisters of 
	Returns: A list with the sisters in it
|#
(defun sisters (p)
	(female_filter (siblings p))
)




#|
    Name: brothers
	Description: This function finds the brothers of a person by first finding 
		the siblings of that person, and then filtering out the males. 

	Param: p - the person to find the brothers 
	Returns: A list with the brothers in it
|#
(defun brothers (p)
	(male_filter (siblings p))
)




#|
    Name: nieces_nephews
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
    Name: nieces
	Description: This function finds the nieces of the person by finding both
		the neices and nephews, and then filtering out the females

	Param: p - the person to find the nieces of 
	Returns: A list with the nieces in it
|#
(defun nieces (p)
	(female_filter (nieces_nephews p))
)




#|
    Name: nephews
	Description: This function finds the nephews of a person by first finding
		the nieces and nephews, and then filtering out the males

	Param: p - the person to find the nephews of 
	Returns: A list with the nephews in it
|#
(defun nephews (p)
	(male_filter (nieces_nephews p))
)



#|
    Name: cousins
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
    Name: female-cousins
	Description: This function finds the female cousins of a person by finding 
		the cousins of a person, and then filtering out the females 

	Param: p - the person to find the female cousins of 
	Returns: A list with the females cousins in it
|#
(defun female-cousins (p)
	(female_filter (cousins p))
)



#|
    Name: male-cousins
	Description: This function finds the male cousins of a person by finding 
		the cousins of a person, and then filtering out the males 

	Param: p - the person to find the male cousins of 
	Returns: A list with the males cousins in it
|#
(defun male-cousins (p)
	(male_filter (cousins p))
)




#|
    Name: aunts_uncles
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
    Name: aunts
	Description: This function finds the aunts by first finding both the aunts
		and uncles and then filtering out the females

	Param: p - the person to find the aunts of 
	Returns: A list with the aunts of in it 
|#
(defun aunts (p)
	(female_filter (aunts_uncles p))
)


#|
    Name: uncles
	Description: This function finds the uncles of a person by first finding 
		the aunts and uncles and then filtering out the male

	Param: p - the person to find the unlces of 
	Returns: A list with the uncles in it
|#
(defun uncles (p)
	(male_filter (aunts_uncles p))
)


#|
    Name:   ancestors_and_me
    Description: This function recursively finds the ancestors of person p
        and returns them as a list. The only downfall of this function is 
        that the recursive call includes person p as an ancestor of 
        him/herself.  
                
    Param: p - the person to find the ancestors (and self) of
    Returns: A list of ancestors, as well as person p
|#
(defun ancestors_and_me (p)
    ;Find the parents of p
    (setf L (parents p))   
    ;Base Case: If no ancestors, return "myself"
    (if (null L) (return-from ancestors_and_me (cons p nil)))
    ;Recursively call ancestors on the parents of p    
    (setf L2 (apply #'append (mapcar #'ancestors_and_me L)))
    ;append the lists, plus person p
    (setf L (append L L2 (cons p nil)))
    ;remove the duplicates in the list of ancestors
    (remove-duplicates L)
    
)

#|
    Name: descendants_and_me
    Description: This function recursively finds the descendants of person p
        by calling the finding each descendant's children and returns them as a list. 
        The only downfall of this function is that the recursive call includes person 
        p as an ancestor of him/herself.  
                
    Param: p - the person to find the descendants (and self) of
    Returns: A list of descendants, as well as person p
|#
(defun descendants_and_me (p)
    ;Fiund the children of p
    (setf L (children p))
    ;Base Case: If no children, append "myself" to the list
    (if (null L) (return-from descendants_and_me (cons p nil)))
    ;Recursively call descendants_and_me on the children of p    
    (setf L2 (apply #'append (mapcar #'descendants_and_me L)))
    ;Append the lists and person p
    (setf L (append L L2 (cons p nil)))
    ;Remove duplicates from the list of descendants
    (remove-duplicates L)
)

#|
    Name: descendants
    Description: This function calls descendants_and_me and removes
        the the person who we are finding descendants of from the list  
                
    Param: p - the person to find the descendants of
    Returns: A list of descendants of person p
|#
(defun descendants (me)
    (setf d_and_me (descendants_and_me me))
    (remove me d_and_me)
)

#|
    Name: ancestors
    Description: This function calls ancestors_and_me and removes
        the the person who we are finding ancestors of from the list  
                
    Param: p - the person to find the descendants of
    Returns: A list of ancestors of person p
|#
(defun ancestors (me)
    (setf a_and_me (ancestors_and_me me))
    (remove me a_and_me)
)


#|
    Name: male-ancestors
    Description: This function calls ancestors and removes
        the males who we are finding ancestors of from the list  
                
    Param: p - the person to find the male ancestors of
    Returns: A list of male-ancestors of person p
|#
(defun male-ancestors (p)
    (male_filter (ancestors p))
)

#|
    Name: female-ancestors
    Description: This function calls ancestors and removes
        the females who we are finding ancestors of from the list  
                
    Param: p - the person to find the male ancestors of
    Returns: A list of male-ancestors of person p
|#
(defun female-ancestors (p)
    (female_filter (ancestors p))
)

#|
    Name: male-descendants
    Description: This function calls descendants and removes
        the males who we are finding ancestors of from the list  
                
    Param: p - the person to find the male descendants of
    Returns: A list of male-descendants of person p
|#
(defun male-descendants (p)
    (male_filter (descendants p))
)

#|
    Name: female-descendants
    Description: This function calls descendants and removes
        the females who we are finding ancestors of from the list  
                
    Param: p - the person to find the female descendants of
    Returns: A list of female-descendants of person p
|#
(defun female-descendants (p)
    (female_filter (descendants p))
)

; Read the database into *database* using the read_database function
(setf *database* (read_database (car (last *ARGS*))))

