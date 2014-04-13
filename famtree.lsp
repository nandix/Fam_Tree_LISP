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


(defun parents (p)
	(person-parents (lookup p *database*))
)

(defun fathers (p)
	(male_filter (parents p))
)

(defun mothers (p)
	(female_filter (parents p))
)

(defun daughters (p)
    (female_filter (children p))
)

(defun sons (p)
    (male_filter (children p))
)

(defun grandparents (p)
	(setf L (parents p))
	(apply #'append (mapcar #'(lambda(x) (parents x)) L)) 
)

(defun grandmothers (p)
	(female_filter (grandparents p))
)

(defun grandfathers (p)
	(male_filter (grandparents p))
)

(defun grandchildren (p)
    (setf L (children p))
    (apply #'append
        (mapcar #'(lambda(x) (children x)) L) )
)

(defun grandsons (p)
    (male_filter (grandchildren p))
)

(defun granddaughters (p)
    (female_filter (grandchildren p))
)

(defun siblings (p)
	(setf L (parents p))
	(setf L (apply #'append (mapcar #'children L)))
	(setf L (remove-duplicates L))
	(remove	p L)
)

(defun sisters (p)
	(female_filter (siblings p))
)

(defun brothers (p)
	(male_filter (siblings p))
)

(defun nieces_nephews (p)
	(setf L (siblings p))
	(setf L (apply #'append (mapcar #'children L)))
	(setf L (remove-duplicates L))
)

(defun nieces (p)
	(female_filter (nieces_nephews p))
)

(defun nephews (p)
	(male_filter (nieces_nephews p))
)

(defun cousins (p)
	(setf L (parents p))
	(setf L (apply #'append (mapcar #'siblings L)))
	(setf L (apply #'append (mapcar #'children L)))
	(remove-duplicates L)
)

(defun female-cousins (p)
	(female_filter (cousins p))
)

(defun male-cousins (p)
	(male_filter (cousins p))
)

(defun aunts_uncles (p)
	(setf L (parents p))
	(setf L (apply #'append (mapcar #'siblings L)))
	(setf L2 (apply #'append (mapcar #'children L)))
	(setf L2 (apply #'append (mapcar #'parents L2)))
	(setf L (append L L2))
	(remove-duplicates L)
)

(defun aunts (p)
	(female_filter (aunts_uncles p))
)

(defun uncles (p)
	(male_filter (aunts_uncles p))
)
; Read the database into *database* using the read_database function
(setf *database* (read_database (car (last *ARGS*))))


