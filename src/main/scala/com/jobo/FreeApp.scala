package com.jobo

object TestApp extends App {
  println("Free test")

  import CombinateFree._

//  println(result)

  import DataSource._, Interacts._

  val evaled: Unit = program.foldMap(interpreter)

}

//sealed trait KVStoreA[A]
//case class Put[A](key: String, value: A) extends KVStoreA[Unit]
//case class Get[A](key: String): A extends KVStoreA[Option[A]]
//case class Delete(key: String) extends KVStoreA[Unit]

object Freeing {
  sealed trait KVStoreA[A]
  case class Put[A](key: String, value: A) extends KVStoreA[Unit]
  case class Get[A](key: String) extends KVStoreA[Option[A]]
  case class Delete(key: String) extends KVStoreA[Unit]

  // Freeing
  import cats.free.Free
  import cats.free.Free.liftF

  type KVStore[A] = Free[KVStoreA, A]

  // Put Returns Nothing.
  def put[A](key: String, value: A): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put[A](key, value))

  // Get Returns A value.
  def get[A](key: String): KVStore[Option[A]] =
    liftF[KVStoreA, Option[A]](Get[A](key))

  // Delete returns Nothing.
  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key))

  // Update composed get and set, returns Nothing.
  def update[A](key: String, f: A => A): KVStore[Unit] =
    for {
      mK <- get[A](key)
      _  <- mK.map(v => put[A](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // Imports for compiler
  import cats.arrow.FunctionK
  import cats.{Id, ~>}
  import scala.collection.mutable

  def impureCompiler: KVStoreA ~> Id =
    new (KVStoreA ~> Id) {
      // Simple K-V store.
      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: KVStoreA[A]): Id[A] =
        fa match {
          case Put(k, v) =>
            println(s"put($k, $v)")
            kvs(k) = v
            ()
          case Get(k) =>
            println(s"get($k)")
            kvs.get(k).map(_.asInstanceOf[A])
          case Delete(k) =>
            println(s"delete($k)")
            kvs.remove(k)
            ()
        }
    }

  val result: Option[Int] = program.foldMap(impureCompiler)

  import cats.data.State
  
  type KVStoreState[A] = State[Map[String, Any], A]
  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
      fa match {
        case Put(k,v) =>
          State.modify(_.updated(k,v))
        case Get(k) =>
          State.inspect(_.get(k).map(_.asInstanceOf[A]))
        case Delete(k) =>
          State.modify(_ - k)
      }
  }

  val pureResult: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
}

object CombinateFree {
  import cats.data.EitherK
  import cats.free.Free
  import cats.{Id, ~>, InjectK}
  import scala.collection.mutable.ListBuffer

  /* Handles user interaction */
  sealed trait Interact[A]
  case class Tell[A](msg: String) extends Interact[Unit]
  case class Ask[A](prompt: String) extends Interact[String]

  /* Represents persistance operations */
  sealed trait DataOp[A]
  case class AddCat[A](a: String) extends DataOp[Unit]
  case class GetAllCats() extends DataOp[List[String]]

  type CatsApp[A] = EitherK[DataOp, Interact, A]

  /* In order to take advantage of monadic composition we use smart constructors to lift our Algebra to the Free context. */

  class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
    def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
    def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
  }

  object Interacts {
    implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
  }

  class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
    def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
    def getAllCats(): Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
  }

  object DataSource {
    implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
  }

  /* Program */
  def program(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {

    import I._, D._

    for {
      cat  <- ask("What is the kitty name?")
      _    <- addCat(cat)
      cats <- getAllCats()
      _    <- tell(cats.toString)
    } yield ()

  }

  object ConsoleCatsInterpreter extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  object InMemoryDataSourceInterpreter extends (DataOp ~> Id) {
    private[this] val memDataSet = new ListBuffer[String]

    def apply[A](fa: DataOp[A]) = fa match {
      case AddCat(s) =>
        memDataSet.append(s)
        ()
      case GetAllCats() =>
        memDataSet.toList
    }
  }

  val interpreter: CatsApp ~> Id = InMemoryDataSourceInterpreter or ConsoleCatsInterpreter


}


object FreeTApp {
  import cats.free._
  import cats._
  import cats.data._

  /* A base ADT for user interaction without State semantics */
  sealed abstract class Teletype[A] extends Product with Serializable
  final case class WriteLine(line: String) extends Teletype[Unit]
  final case class ReadLine(prompt: String) extends Teletype[String]

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]

  type Log = List[String]

  type TeletypeState[A] = State[List[String], A]

  /* Smart constructors */
  object TeletypeOps {
    def writeLine(line: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
    def readLine(prompt: String): TeletypeT[TeletypeState, String] =
      FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
    def log(s : String) : TeletypeT[TeletypeState, Unit] =
      FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
  }

  def program: TeletypeT[TeletypeState, Unit] = {
    for {
      userSaid <- TeletypeOps.readLine("what's up?")
      _        <- TeletypeOps.log(s"User said: $userSaid")
      _        <- TeletypeOps.writeLine("thanks, see you soon!")
    } yield ()
  }

  def interpreter: Teletype ~> TeletypeState = new (Teletype ~> TeletypeState) {
    def apply[A](fa: Teletype[A]): TeletypeState[A] = fa match {
      case ReadLine(prompt) =>
        println(prompt)
        val userInput = "hanging in here" //scala.io.StdIn.readLine()
	StateT.pure[Eval, List[String], A](userInput)
      case WriteLine(line) =>
        StateT.pure[Eval,List[String],A](println(line))
    }
  }

  import TeletypeOps._

  val state = program.foldMap(interpreter)

  val initialState = Nil

  val (stored, _) = state.run(initialState).value

  println(stored)

}
