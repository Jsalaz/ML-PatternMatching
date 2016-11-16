	(* Jorge Salazar *)
	(* Professor R. Wyatt *)
	(* CSC 345 Paradigms *)
	(* ML Homework *)
	
	(*-------------------------------------------------------------------*)
	(* flip: Takes a list and reverses every pair of elements *)
	(* 1: If the list is empty return an empty list *)
	(* 2: If the list has at least 3 elements, nil inclusive, such as 
		[1,2] = 1::2::[]. Then flips the first and second, and recursively 
		calls flip on the rest of the list *)
	(* 3: If the list has at least 2 elements, nil inclusive, 
		such as [1] = 1::[], then return the entire list *)

	fun flip nil = nil                      (* 1: *)
	  | flip (x::xa::xs) = xa::x::flip xs   (* 2: *)
	  | flip (x::xs) = x::xs;               (* 3: *)

	(*-------------------------------------------------------------------*)
	(* deleteIth: Takes a list of elements and deletes the Ith element. The 
              function uses the i value to count down when the Ith element 
              is the first of the rest of the list *)
	(* 1: deleteIth takes an empty list with any number, then returns empty list *)
	(* 2: deleteIth takes a list and index 1, then return the rest of the list*)
	(* 3: deleteIth takes a list and an index larger than 1 *)
	(* 4: If the length of the list is less than i is true, then return the list *)
	(* 5: else the first is concatenated to the recursive call of deleteIth on the
		rest of the list and the index-1*)

	fun deleteIth (nil, _) = nil           (* 1: *)
	  | deleteIth (x::xs, 1) = xs          (* 2: *)
	  | deleteIth (x::xs, i) =             (* 3: *)
	    if length(x::xs) < i then x::xs    (* 4: *) 
	    else x::deleteIth(xs, i-1);        (* 5: *)

	(*-------------------------------------------------------------------*)
	(* vowelCheck: Takes a Char list and returns true if the first 
				Char is a vowel *)
	(* 1: If the list is empty, return false *)
	(* 2: If the list is not empty, then convert the first Char into
		a lowercase Char, and compare its value to cases of vowels *)

	fun vowelCheck nil = false             (* 1: *)
	  | vowelCheck (x::_) =                (* 2: *)
	    case (Char.toLower x) of
		   #"a" => true
	      | #"e" => true
	      | #"i" => true
	      | #"o" => true
	      | #"u" => true
	      | _ => false;

	(*-------------------------------------------------------------------*)
	(* beginsWithVowel: Checks if the first letter is a vowel by converting 
	                    a string to a char list and passing the list to 
	                    vowelCheck *)
	(* 1: If the string is empty then return false *)
	(* 2: If the string is not empty then explode the string, and 
	      send the Char list to vowelCheck *)

	fun beginsWithVowel "" = false                    (* 1: *)
	  | beginsWithVowel (L) = vowelCheck (explode L); (* 2: *)
  
	
	
	
	
	(*-------------------------------------------------------------------*)
	(* moveToVowel: Moves the first of a Char list until a vowel is the first 
	                of the list *)
	(* 1: If the list is empty, return the empty list *)
	(* 2: If the first of the list is a vowel, then implode the Char list and 
	      return it as a string *)
	(* 3: else call moveToVowel on the rest of the list and append it to the 
	      first of the list *)

	fun moveToVowel nil = implode nil       (* 1: *)
	  | moveToVowel (x::xs) =               
	    if vowelCheck (x::xs)               (* 2: *)
	    then implode (x::xs)
	    else moveToVowel(xs@[x]);           (* 3: *)

	(*-------------------------------------------------------------------*)
	(* piglatinize: converts a string into it's piglatin version *)
	(* 1: If the input is an empty string, return an empty *)
	(* 2: If the first letter is a vowel then return the string concatenated 
	      to with "yay" at the end *)
	(* 3: Else move the elements of the list until the first element is a 
	      vowel and concatenate with "ay" *)

	fun piglatinize "" = ""                  (* 1: *)
	  | piglatinize L =                      (* 2: *)
	    if beginsWithVowel L then L^"yay"   
	    else (moveToVowel (explode L))^"ay"; (* 3: *)

	(*-------------------------------------------------------------------*)
	(* Tests *)
	val _ = print "\n====== Running Tests ====== \n";

	(* flip with even number of elements *)
	val _ = print "\nflip with even number of elements \n";
	val _ = print "flip [1,2,3,4,5,6]; \n";
	flip [1,2,3,4,5,6];

	(* flip with odd number of elements *)
	val _ = print "\nflip with odd number of elements \n";
	val _ = print "flip [1.1,2.2,3.3,4.4,5.5]; \n";
	flip [1.1,2.2,3.3,4.4,5.5];

	(* deleteIth with the ith symbol < the list's length *)
	val _ = print "\ndeleteIth with the ith symbol < the list's length \n";
	val _ = print "deleteIth ([#\"a\", #\"b\", #\"c\", #\"d\"],3); \n";
	deleteIth ([#"a", #"b", #"c", #"d"],3);

	(* deleteIth with the ith symbol > the list's length *)
	val _ = print "\ndeleteIth with the ith symbol > the list's length \n";
	val _ = print "deleteIth ([1,2,3,4,5,6], 7); \n";
	deleteIth ([1,2,3,4,5,6], 7);

	(* deleteIth with the ith symbol is a negative number *)
	val _ = print "\ndeleteIth with the ith symbol is a negative number \n";
	val _ = print "deleteIth ([\"one\",\"two\",\"three\"], ~2); \n";
	deleteIth (["one","two","three"], ~2);

	(* beginsWithVowel with lowercase vowel as first letter *)
	val _ = print "\nbeginsWithVowel with lowercase vowel as first letter \n";
	val _ = print "beginsWithVowel \"apple\"; \n";
	beginsWithVowel "apple";

	(* beginsWithVowel with uppercase vowel as first letter *)
	val _ = print "\nbeginsWithVowel with uppercase vowel as first letter \n";
	val _ = print "beginsWithVowel \"Orange\"; \n";
	beginsWithVowel "Orange";

	(* beginsWithVowel with consonant as first letter *)
	val _ = print "\nbeginsWithVowel with consonant as first letter \n";
	val _ = print "beginsWithVowel \"hello\"; \n";
	beginsWithVowel "hello";

	(* piglatinize with vowel as first letter *)
	val _ = print "\npiglatinize with vowel as first letter \n";
	val _ = print "piglatinize \"able\"; \n";
	piglatinize "able";

	(* piglatinize with consonant as first letter *)
	val _ = print "\npiglatinize with consonant as first letter \n";
	val _ = print "piglatinize \"stripe\"; \n";
	piglatinize "stripe";
	
	(* end *)
