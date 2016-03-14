package net.chwthewke.valuellama

import monocle._, Monocle._
import monocle.macros._
// TODO move to build
import scala.language.higherKinds
import scalaz.{ Lens => _, _ }, Scalaz._

case class GenState( seed : Seed, failures : Int )

object GenState {
  val seed : Lens[GenState, Seed] = GenLens[GenState]( _.seed )
  val failures : Lens[GenState, Int] = GenLens[GenState]( _.failures )

  val incFail = failures.modify( _ + 1 )
  val failOnce = failures.set( 1 )
}

case class GenParams( size : Int, maxFailures : Int )

object GenParams {
  val size : Lens[GenParams, Int] = GenLens[GenParams]( _.size )
  val maxFailures : Lens[GenParams, Int] = GenLens[GenParams]( _.maxFailures )
}

sealed trait GenFailure
case object UnspecifiedFailure extends GenFailure
case object FilterFailed extends GenFailure
case class RetryFailed( f : GenFailure ) extends GenFailure

