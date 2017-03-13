def max(xs: List[Int]): Int = {
  def vs(x: Int, remainder: List[Int]): Int = {
    if(remainder.isEmpty) x
    else {
      val y = remainder.head
      if(x > y) vs(x, remainder.tail)
      else vs(y, remainder.tail)
    }
  }

  vs(0, xs)
}

max(List(1,3,5,6, 55))