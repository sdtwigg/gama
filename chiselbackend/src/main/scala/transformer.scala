package gama.backend

trait Transformers {
  type Input

  sealed trait TransformResult[+T]
  case class             Success[+T](result: T)     extends TransformResult[T]
  sealed abstract class  NoSuccess(val msg: String) extends TransformResult[Nothing]

  object NoSuccess {
    def unapply[T](x: TransformResult[T]) = x match {
      case Failure(msg) => Some(msg)
      case Error(msg)   => Some(msg)
      case _ => None
    }
  }

  case class Failure(override val msg: String) extends NoSuccess(msg)
  case class Error(override val msg: String)   extends NoSuccess(msg)

  abstract class Transformer[+T] extends (Input => TransformResult[T])
  def Transformer[T](f: PartialFunction[Input,T]) = new Transformer[T] {
    def apply(in: Input): TransformResult[T] = 
      try {Success(f(in))} catch {case _: MatchError => (Failure(s"Failed to transform ${in.toString}"))}
  }
}
