fun digits x =
  let
      fun help x res = if x < 10 then x :: res
		       else help (x div 10) ((x mod 10) :: res)
  in
      rev (help x nil)
  end
      
fun captcha xs =
  let
      val input = digits xs
      val c = input @ [hd input]
  in
      #2 (foldl (fn (x, (lx, res)) => if x = lx
				      then (x, res + x)
				      else (x, res))
		(0, 0) c)
  end
