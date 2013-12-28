package object forth {
  import scala.util.{Try, Success, Failure}

  case class StackEmptyException(message: String) extends Exception(message)
  case class WordNotDefinedException(word: String) extends Exception(word)
  case class InvalidArgumentException(word: String) extends Exception(word)
  case class UnexpectedThingException(thing: String) extends Exception(thing)

  type Word = Stacks => Try[Stacks]
  type WordM = Try[Stacks] => Try[Stacks]

  case class NamedWord(name: String, value: Word)

/* this one means we don't have to use pure if we don't want to
  implicit class WordWrapper(val f:(Stacks => Try[Stacks])) {
    def >>(f2:Word) = (s:Stacks) => f2(f(s).get)
  }
*/
  implicit class WordMWrapper(val f:Try[Stacks]) {
    def >>(f2:Word) = f2(f.get)
  }
  object Machine {
    def default_words:List[NamedWord] = List(
      NamedWord("+", words.plus),
      NamedWord("-", words.minus),
      NamedWord("*", words.multiply),
      NamedWord("/", words.divide),
      NamedWord("inc", words.inc),
      NamedWord("dec", words.dec),
      NamedWord("dup", words.dup),
      NamedWord("drop", words.drop),
      NamedWord("swap", words.swap),
      NamedWord("over", words.over),
      NamedWord("rot", words.rot),
      NamedWord("rrot", words.rrot),
      NamedWord("puts", words.puts)
    )
    def test_run(ts: List[Any]) = {
      val inp = Stacks(Nil,ts,default_words)
      this.run(inp)
    }
    def runOne(t:Any,s:Stacks):Try[Stacks] = t match {
      case l: Long => s.pushD(l)
      case i: Int => s.pushD(i.toLong)
      case b: Boolean => s.pushD(b)
      case f: Double => s.pushD(f)
      case NamedWord(n,v) => s.pure >> v
      case _ => {
        println("Unexpected, t")
        Failure(UnexpectedThingException("t"))
      }
    }
    def run(s: Stacks):Unit = s.popT match {
      case Success((t,s2)) => this.runOne(t,s2) match {
        case Success(s3) => this.run(s3)
        case Failure(e) => println("Failed",t,e)
      }
      case _ => () // Run out of program
    }
  }

  case class Stacks(data: List[Any], tokens: List[Any], words: List[NamedWord]) {
    def minD(n: Long) = this.data.length >= n
    def minT(n: Long) = this.tokens.length >= n
    def pushD(d: Any):Try[Stacks] = Success(Stacks(d::this.data, this.tokens, this.words))
    def pushDN(d: List[Any]):Try[Stacks] = Success(Stacks(d:::this.data, this.tokens, this.words))
    def popD:Try[(Any,Stacks)] = this.data match {
      case d::ds => Success(d, Stacks(ds, this.tokens, this.words))
      case Nil => Failure(StackEmptyException("Stack empty"))
    }
    def popD2:Try[(Any, Any, Stacks)] = for {
      (d1,t2) <- this.popD
      (d2,t3) <- t2.popD
    } yield (d1,d2,t3)
    def popD3:Try[(Any, Any, Any, Stacks)] = for {
      (d1,t2) <- this.popD
      (d2,t3) <- t2.popD
      (d3,t4) <- t3.popD
    } yield (d1,d2,d3,t4)
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
    def pushW(w: NamedWord):Try[Stacks] = Success(Stacks(this.data, this.tokens, w::this.words))
    def popW():Try[(NamedWord, Stacks)] = this.words match {
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
    def part1(f:(Any => Try[List[Any]])):Word = (s: Stacks) => for {
      (a, s2) <- s.popD
      n <- f(a)
      ret <- s2.pushDN(n.reverse)
    } yield ret
    def part2(f:((Any, Any) => Try[List[Any]])):Word = (s: Stacks) => for {
      (a1, a2, s2) <- s.popD2
      n <- f(a1,a2)
      ret <- s2.pushDN(n.reverse)
    } yield ret
    def part3(f:((Any, Any, Any) => Try[List[Any]])):Word = (s: Stacks) => for {
      (a1, a2, a3, s2) <- s.popD3
      n <- f(a1,a2,a3)
      ret <- s2.pushDN(n.reverse)
    } yield ret
  }

  def pushD(a:Any) = {s:Stacks => s.pushD(a)}
  object words {
    def plus:Word = {s:Stacks => s.pure >> lifts.part2(raw_words.plus)}
    def minus:Word = lifts.part2(raw_words.minus)
    def multiply:Word = lifts.part2(raw_words.multiply)
    def divide:Word = lifts.part2(raw_words.divide)
    def inc:Word = {s: Stacks => s.pure >> pushD(1) >> words.plus}
    def dec:Word = {s: Stacks => s.pure >> pushD(1) >> words.swap >> words.minus}
    def dup:Word = lifts.part1(raw_words.dup)
    def drop:Word = lifts.part1(raw_words.drop)
    def swap:Word = lifts.part2(raw_words.swap)
    def over:Word = lifts.part2(raw_words.over)
    def rot:Word = lifts.part3(raw_words.rot)
    def rrot:Word = lifts.part3(raw_words.rrot)
    def puts:Word = lifts.part1(raw_words.puts)
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
    forth.Machine.test_run(List(4,4,forth.NamedWord("+",forth.words.plus),forth.NamedWord("puts",forth.words.puts)))
  }
}
