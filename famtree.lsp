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
;(print "Read records")
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

; Read the database into *database* using the read_database function
(setf *database* (read_database (car (last *ARGS*))))

