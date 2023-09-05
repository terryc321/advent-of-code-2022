
"""
    run(worry, monkey)

 worry number 71 
 monkey is 0 

 run(71 , 0)
 
 71-0 is initial monkey problem

 do something 10,000 times
 print location each time

track number items monkey
"""
function run(worry, monkey)
 let w = BigInt(worry) , s = monkey , i = 1
   println(s , " , " , w)
   for tick = 1:10000
	 if s == 0
	        w = w * 11
		if mod(w,13) == 0
		   s = 1
		else
		   s = 7
		end
	 end

         if s == 1
	        w = w + 1
		if mod(w,7) == 0
		   s = 3
		else
		   s = 6
		end
	 end
	 
         if s == 2
	        w = w * w
		if mod(w,3) == 0
		   s = 5
		else
		   s = 4
		end 
         end

         if s == 3
	        w = w + 2
		if mod(w,19) == 0
		   s = 2
		else
		   s = 6
		end 
         end
	 
         if s == 4
	        w = w + 6
		if mod(w,5) == 0
		   s = 0
		else
		   s = 5
		end 
	 end
	 
         if s == 5
	        w = w + 7
		if mod(w,2) == 0
		   s = 7
		else
		   s = 0
		end 
	 end
	 
         if s == 6
	        w = w * 7
		if mod(w,11) == 0
		   s = 2
		else
		   s = 4
		end 
	 end
	 
         if s == 7
	        w = w + 8
		if mod(w,17) == 0
		   s = 1
		else
		   s = 3
		end 		
	end
	
	println(s , " , " , w)

"""
  commented it out
  
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
 run(71 , 0)
end




