
def isSorted[T](array: List[T], isSorted: (T, T) => Boolean) : Boolean =
    (array, array.tail).zipped map isSorted forall identity

def intSorted(x: Int, y : Int) : Boolean = x <= y

def intSorted2(x: Int, y : Int) : Boolean = x >= y

val x : Any

x match {
  case y:String => println(y)
  case y:Int => println(y)
}