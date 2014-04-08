#| 
	This file contains the data for testing many many cases using database.txt
	as the database. Commented after each test is the solution that should be 
	returned from our functions.

	Notes: 
	
	When a function would return a large list of numbers in order, it is 
	abbriviated (21 - 26) to stand for (21 22 23 24 25 26) to save space. 

	It is possible that the functions could return these in a different order
	than given. It is still considered correct as long as it gets the same
	numbers no more and no less.
|#

(parents 5) 		;(1 2 3 4)
(parents 1)			; nil
(parents 11)		;(9 10)
(parents 12)		;(11) 

(mothers 5)			;(2 4)
(mothers 11)		;(10)
(mothers 15)		;(13 14)
(mothers 18)		; nil
(mothers 13)		; nil

(fathers 5)			;(1 3)
(fathers 11)		;(9)
(fathers 18)		;(16 17)
(fathers 15)		; nil
(fathers 17)		; nil


(children 1)		;(5 6 7 8)
(children 9)		;(11)
(children 15)		; nil

(sons 1) 			;(5 7)
(sons 11)			;(12)
(sons 13)			; nil
(sons 15)			; nil
(sons 17)			;(18 19)

(daughters 1)		;(6 8)
(daughters 13)		; (15 20)
(daughters 16)		; nil
(daughters 15)		; nil
(daughters 10)		; (11)


(siblings 5)		; (6 7 8)
(siblings 19)		; (18)
(siblings 11)		; nil
(siblings 9)		; nil

(brothers 9)		; nil
(brothers 6)		; (5 7)
(brothers 5)		; (7)
(brothers 15)		; nil
(brothers 12)		; nil
(brothers 18)		; (19)

(sisters 5)			; (6 8)
(sisters 6)			; (8)
(sisters 20)		; (15)
(sisters 9)			; nil
(sisters 12)		; nil
(sisters 18)		; nil


(grandparents 30)	; (21 - 26)
(grandparents 21)	; nil
(grandparents 37)	; nil
(grandparents 29)	; (31 32 33)

(grandmothers 27)	; nil
(grandmothers 28)	; (34 35)
(grandmothers 29)	; (32)
(grandmothers 30)	; (22 24 26)
(grandmothers 38)	; nil
(grandmothers 39)	; nil

(grandfathers 27)	; (36 37)
(grandfathers 28)	; nil
(grandfathers 29)	; (31 33)
(grandfathers 30)	; (21 23 25)
(grandfathers 38)	; nil
(grandfathers 39) 	; nil


(ancestors 30)		; (21 - 37)
(ancestors 28)		; (23 24 34 35)
(ancestors 34)		; nil 

(female-ancestors 30)	; (22 24 26 28 32 34 35)
(female-ancestors 21)	; nil
(female-ancestors 34)	; nil

(male-ancestors 30)	; (21 23 25 27 29 31 33 36 37)
(male-ancestors 23)	; nil
(male-ancestors 34)	; nil


(grandchildren 40)	; (44 - 52)
(grandchildren 41)	; (53 - 58)
(grandchildren 43)	; (59 - 64)
(grandchildren 42)	; nil
(grandchildren 47)	; nil

(grandsons 40)		; (45 47 48 51 52)
(grandsons 41)		; (53 - 58)
(grandsons 43)		; nil
(grandsons 42)		; nil
(grandsons 47)		; nil

(granddaughters 40)	; (44 46 49 50)
(granddaughters 41)	; nil
(granddaughters 43)	; (59 - 64)
(granddaughters 42)	; nil
(granddaughters 47)	; nil


(descendants 40)	; (41 - 64)
(descendants 43)	; (50 51 52 59 60 61 62 63 64)
(descendants 64)	; nil

(female-descendants 40)	; (41 43 44 46 49 50 59-64)
(female-descendants 43)	; (50 59-64)
(female-descendants 41)	; (44 46)
(female-descendants 45)	; nil
(female-descendants 55) ; nil

(male-descendants 40)	; (42 45 47 48 51-58)
(male-descendants 41)	; (45 53-58)
(male-descendants 43)	; (51 52)
(male-descendants 50)	; nil
(male-descendants 59)	; nil


(aunts&uncles 71)	; (74 75 77)
(aunts&uncles 76)	; (81 82 87 88)
(aunts&uncles 66)	; nil
(aunts&uncles 65)	; nil
(aunts&uncles 81)	; nil
(aunts&uncles 84)	; (78 79 81 87 89)

(aunts 71)			; (75)
(aunts 76)			; (81 88)
(aunts 84)			; (79 81 89)
(aunts 66)			; nil
(aunts 65)			; nil

(uncles 72)			; nil
(uncles 71)			; (74 77)
(uncles 76)			; (82 87)
(uncles 84)			; (78 87)
(uncles 65)			; nil
(uncles 66)			; nil


(nieces&nephews 76)	; (69 70 72 73)
(nieces&nephews 66)	; nil
(nieces&nephews 65)	; nil
(nieces&nephews 82)	; (75 76 85 86)
(nieces&nephews 77)	; (71)

(nieces 76)			; (70 73)
(nieces 77)			; nil
(nieces 66)			; nil
(nieces 82)			; (75 76 85)

(nephews 76)		; (69 72)
(nephews 77)		; (71)
(nephews 66)		; nil
(nephews 82)		; (86)


(cousins 71)		; (69 70 72 73)
(cousins 79)		; nil
(cousins 75)		; (83 84 85 86)

(female-cousins 71)	; (70 73)
(female-cousins 79)	; nil
(female-cousins 75)	; (84 85)

(male-cousins 71)	; (69 72)
(male-cousins 79)	; nil
(male-cousins 75)	; (83 86)


