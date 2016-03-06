package net.chwthewke.valuellama

import monocle._, Monocle._
import scala.language.higherKinds
import scalaz.{ Lens => _, _ }, Scalaz._

trait MonadStateSyntax {
  def stateM[F[_], S, A]( st : S => ( S, A ) )( implicit F : MonadState[F, S] ) : F[A] =
    for {
      s <- F.get
      ( s1, a ) = st( s )
      _ <- F.put( s1 )
    } yield a
}

object MonadStateSyntax extends MonadStateSyntax
