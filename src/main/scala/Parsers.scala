package parsing
trait Parsers {
  //type of input to the parser
  type Input
  abstract class ParseResult[+T] {
    def next: Input
    def map[U](f:T=>U):ParseResult[U]
    def flatMapWithNext[U](f:T=>Parser[U]):ParseResult[U]
    def append[U>:T](alt: => ParseResult[U]):ParseResult[U]

  }

  case class Success[+T](result: T, next: Input) extends ParseResult[T]{
    def map[U](f:T=>U):ParseResult[U] = Success(f(result),next)
    def flatMapWithNext[U](f:T=> Parser[U]) =f(result)(next)
     def append[U>:T](alt: => ParseResult[U]):ParseResult[U] = this
  }

  case class Failure(msg: String, next: Input) extends ParseResult[Nothing]{
    def map[U](f:Nothing=>U):ParseResult[U]=this
    def flatMapWithNext[U](f:Nothing=> Parser[U])= this
    def append[U](alt: => ParseResult[U]):ParseResult[U] = alt
  }

  abstract class Parser[+T] extends (Input => ParseResult[T]) {
    def apply(in: Input)    : ParseResult[T]
    def map[U](f:T=> U):Parser[U] = 
      new Parser[U]{
      def apply(in:Input) = Parser.this(in).map(f)
    }
    def flatMap[U](f:T => Parser[U]):Parser[U] = 
      new Parser[U]{
      def apply(in:Input):ParseResult[U] =  Parser.this(in).flatMapWithNext(f)
    }
    //alternate parser p1|p2 giving priority to p1
    def |[U >: T](p: => Parser[U]): Parser[U] =
      new Parser[U] {
        def apply(in: Input) = Parser.this(in).append(p(in))
      }
     //sequencing parser
    def ~[U](p: => Parser[U]): Parser[(T, U)] =
      for(a <- this ; b<-p) yield (a,b)
      //this.flatMap(x=> p.map(y=> (x,y)))
              
    //for logging parser
    def log[T](p: ⇒ Parser[T])(name: String) = new Parser[T] {
      def apply(in: Input): ParseResult[T] = {
        println("trying " + name + " at " + in + " ")
        val r = p(in)
        println(name + " --> " + r)
        r
      }
    }

    

  }
}
