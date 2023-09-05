
"""
    run(worry, monkey)

 worry number 
 monkey is 0 .. 3

 0 : 79 , 98
 1 : 54 , 65 , 75 , 74
 2 : 79 , 60 , 97
 3 : 74

 0 : new = old * 19
      mod 23 ?  2 :   3

 1 : new = old + 6
      mod 19 ?  2 :   0

 2 : new = old * old
      mod 13 ?  1 :   3

 3 : new = old + 3
      mod 17 ?  0 :   1

 track worry 79
 79 - 0 

"""
function run(worry, monkey)
 let w = BigInt(worry) , s = monkey , i = 1
   println(s , " , ", w)
   for tick = 1 : 20
	 if s == 0	   
	        w = w * 19
		"""
		reduce worry
		"""
		w = div(w,3)  
		if mod(w,23) == 0
		   s = 2
		else
		   s = 3
		end
	 end
	 
	 if s == 1
	        w = w + 6
		w = div(w,3)
		if mod(w,19) == 0
		   s = 2
		else
		   s = 0
		end
	 end
	 
         if s == 2
	        w = w * w
		w = div(w,3)
		if mod(w,13) == 0
		   s = 1
		else
		   s = 3
		end 
         end

         if s == 3
                w = w + 3
		w = div(w,3)
		if mod(w,17) == 0
		   s = 0
		else
		   s = 1
		end 
	 end

      println(s , " , ", w)
	

"""
commented out
   i = i + 1
   if i > 11
    i = 1
    println("")
   end 
"""
   

   end
 end
end




function demo()
 run(79 , 0)
end



