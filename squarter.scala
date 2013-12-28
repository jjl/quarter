package object forth {
  import scala.util.{Try, Success, Failure}

  case class StackEmptyException(message: String) extends Exception(message)
  case class WordNotDefinedException(word: String) extends Exception(word)
  case class InvalidArgumentException(word: String) extends Exception(word)
  case class UnexpectedThingException(thing: String) extends Exception(thing)

  type WordF = Stacks => Try[Stacks]
  type WordM = Try[Stacks] => Try[Stacks]

  case class Word(name: String, value: WordF)

/* this one means we don't have to use pure if we don't want to
  implicit class WordFWrapper(val f:(Stacks => Try[Stacks])) {
    def >>(f2:WordF) = (s:Stacks) => f2(f(s).get)
  }
*/
  implicit class WordMWrapper(val f:Try[Stacks]) {
    def >>(f2:WordF) = f2(f.get)
  }
  object Machine {
    def default_words:List[Word] = List(
      Word("+", words.plus),
      Word("-", words.minus),
      Word("*", words.multiply),
      Word("/", words.divide),
      Word("inc", words.inc),
      Word("dec", words.dec),
      Word("dup", words.dup),
      Word("drop", words.drop),
      Word("swap", words.swap),
      Word("over", words.over),
      Word("rot", words.rot),
      Word("rrot", words.rrot),
      Word("puts", words.puts)
    )
    def test_run(ts: List[Any]) = {
      val inp = Stacks(Nil,ts,default_words)
      this.run(inp)
    }
    def run(s: Stacks):Unit = {
      s.popT match {
        case Success((t,s2)) => {
          val s3 = t match {
            case l: Long => s2.pushD(l)
            case i: Int => s2.pushD(i.toLong)
            case b: Boolean => s2.pushD(b)
            case f: Double => s2.pushD(f)
            case Word(n,v) => s2.pure >> v
            case _ => {
              println("Unexpected, t")
              Failure(UnexpectedThingException("t"))
            }
          }
          s3 match {
            case Success(s4) => this.run(s4)
            case Failure(e) => println("Failed",t,e)
          }
        }
        case _ => ()
      }
    }
  }

  case class Stacks(data: List[Any], tokens: List[Any], words: List[Word]) {
    def minD(n: Long) = this.data.length >= n
    def minT(n: Long) = this.tokens.length >= n
    def pushD(d: Any):Try[Stacks] = Success(Stacks(d::this.data, this.tokens, this.words))
    def pushDN(d: List[Any]):Try[Stacks] = Success(Stacks(d:::this.data, this.tokens, this.words))
    def popD:Try[(Any,Stacks)] = this.data match {
      case d::ds => Success(d, Stacks(ds, this.tokens, this.words))
      case Nil => Failure(StackEmptyException("Stack empty"))
    }
    def popD2:Try[(Any, Any, Stacks)] = this.data match {
      case d1::d2::ds => Success(d1,d2, Stacks(ds, this.tokens, this.words))
      case _ => Failure(StackEmptyException("Stack empty"))
    }
    def popD3:Try[(Any, Any, Any, Stacks)] = this.data match {
      case d1::d2::d3::ds => Success(d1,d2,d3,Stacks(ds, this.tokens, this.words))
      case _ => Failure(StackEmptyException("Stack empty"))
    }
    def popDN(n: Int):Try[(List[Any],Stacks)] = {
      if (this.minD(n)) {
        val (p1, p2) = this.data.splitAt(n)
        Success((p1, Stacks(p2,this.tokens,this.words)))
      } else Failure(StackEmptyException("Stack empty: popDN"))
    }
    def pushT(t: Any):Try[Stacks] = Success(Stacks(this.data, t::this.tokens, this.words))
    def popT():Try[(Any,Stacks)] = this.tokens match {
      case t::ts => Success((t, Stacks(this.data, ts, this.words)))
      case _ => Failure(StackEmptyException("Stack empty"))
    }
    def pushW(w: Word):Try[Stacks] = Success(Stacks(this.data, this.tokens, w::this.words))
    def popW():Try[(Word, Stacks)] = this.words match {
      case w::ws => Success((w, Stacks(this.data, this.tokens, ws)))
      case _ => Failure(StackEmptyException("Stack empty"))
    }
    def undefW(wName: String):Try[Stacks] = {
      val i = this.words.indexWhere(w => w.name == wName)
      if (i > -1) {
        val (p1,p2) = this.words.splitAt(i)
        Success(Stacks(this.data,this.tokens,p1 ::: (p2.tail)))
      } else Failure(WordNotDefinedException(wName))
    }
    def pure = Try(this)
  }

  object lifts {
    def part1(f:(Any => Try[List[Any]])):WordF = (s: Stacks) => for {
      (a, s2) <- s.popD
      n <- f(a)
      ret <- s2.pushDN(n.reverse)
    } yield ret
    def part2(f:((Any, Any) => Try[List[Any]])):WordF = (s: Stacks) => for {
      (a1, a2, s2) <- s.popD2
      n <- f(a1,a2)
      ret <- s2.pushDN(n.reverse)
    } yield ret
    def part3(f:((Any, Any, Any) => Try[List[Any]])):WordF = (s: Stacks) => for {
      (a1, a2, a3, s2) <- s.popD3
      n <- f(a1,a2,a3)
      ret <- s2.pushDN(n.reverse)
    } yield ret
  }

  def pushD(a:Any) = {s:Stacks => s.pushD(a)}
  object words {
    def plus:WordF = {s:Stacks => s.pure >> lifts.part2(raw_words.plus)}
    def minus:WordF = lifts.part2(raw_words.minus)
    def multiply:WordF = lifts.part2(raw_words.multiply)
    def divide:WordF = lifts.part2(raw_words.divide)
    def inc:WordF = {s: Stacks => s.pure >> pushD(1) >> words.plus}
    def dec:WordF = {s: Stacks => s.pure >> pushD(1) >> words.swap >> words.minus}
    def dup:WordF = lifts.part1(raw_words.dup)
    def drop:WordF = lifts.part1(raw_words.drop)
    def swap:WordF = lifts.part2(raw_words.swap)
    def over:WordF = lifts.part2(raw_words.over)
    def rot:WordF = lifts.part3(raw_words.rot)
    def rrot:WordF = lifts.part3(raw_words.rrot)
    def puts:WordF = lifts.part1(raw_words.puts)
  }

  object raw_words {
    def plus(a:Any, b:Any):Try[List[Any]] = {
      a match {
        case l: Long => b match {
          case l2: Long => Success(List(l + l2))
          case f: Float => Success(List(l + f))
          case _ => Failure(InvalidArgumentException("+"))
        }
        case f: Float => b match {
          case l: Long => Success(List(f + l))
          case f2: Float => Success(List(f + f2))
          case _ => Failure(InvalidArgumentException("+"))
        }
        case _ => Failure(InvalidArgumentException("+"))
      }
    }
    def minus(a:Any, b:Any):Try[List[Any]] = {
      a match {
        case l: Long => b match {
          case l2: Long => Success(List(l - l2))
          case f: Float => Success(List(l - f))
          case _ => Failure(InvalidArgumentException("-"))
        }
        case f: Float => b match {
          case l: Long => Success(List(f-l))
          case f2: Float => Success(List(f-f2))
          case _ => Failure(InvalidArgumentException("-"))
        }
        case _ => Failure(InvalidArgumentException("-"))
      }
    }
    def multiply(a:Any, b:Any):Try[List[Any]] = {
      a match {
        case l: Long => b match {
          case l2: Long => Success(List(l * l2))
          case f: Float => Success(List(l * f))
          case _ => Failure(InvalidArgumentException("*"))
        }
        case f:Float => b match {
          case l: Long => Success(List(f * l))
          case f2: Float => Success(List(f * f2))
          case _ => Failure(InvalidArgumentException("*"))
        }
        case _ => Failure(InvalidArgumentException("*"))
      }
    }
    def divide(a:Any, b:Any):Try[List[Any]] = {
      a match {
        case l: Long => b match {
          case l2: Long => Success(List(l / l2))
          case f: Float => Success(List(l / f))
          case _ => Failure(InvalidArgumentException("/"))
        }
        case f: Float => b match {
          case l: Long => Success(List(f / l))
          case f2: Float => Success(List(f / f2))
          case _ => Failure(InvalidArgumentException("/"))
        }
        case _ => Failure(InvalidArgumentException("/"))
      }
    }
    def dup(a:Any):Try[List[Any]] = Success(List(a,a))
    def drop(a:Any):Try[List[Any]] = Success(Nil)
    def swap(a:Any, b:Any):Try[List[Any]] = Success(List(b,a))
    def over(a:Any, b:Any):Try[List[Any]] = Success(List(a,b,b))
    def rot(a:Any, b:Any, c:Any):Try[List[Any]] = Success(List(c,a,b))
    def rrot(a:Any, b:Any, c:Any):Try[List[Any]] = Success(List(b,c,a))
    def puts(a:Any):Try[List[Any]] = {
      println(a)
      Success(Nil)
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    forth.Machine.test_run(List(4,4,forth.Word("+",forth.words.plus),forth.Word("puts",forth.words.puts)))
  }
}
