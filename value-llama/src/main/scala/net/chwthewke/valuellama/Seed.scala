package net.chwthewke.valuellama

import scala.annotation.tailrec
import scalaz._, Scalaz._

/**
 * Simple RNG by Bob Jenkins:
 *
 * http://burtleburtle.net/bob/rand/smallprng.html
 */
sealed abstract class Seed {
  protected val a : Long
  protected val b : Long
  protected val c : Long
  protected val d : Long

  /** Generate the next seed in the RNG's sequence. */
  def next : Seed = {
    import java.lang.Long.rotateLeft
    val e = a - rotateLeft( b, 7 )
    val a1 = b ^ rotateLeft( c, 13 )
    val b1 = c + rotateLeft( d, 37 )
    val c1 = d + e
    val d1 = e + a
    Seed( a1, b1, c1, d1 )
  }

  /** Reseed the RNG using the given Long value. */
  def reseed( n : Long ) : Seed = {
    val n0 = ( n >>> 32 ) & 0xffffffff
    val n1 = n & 0xffffffff
    var i = 0
    var seed : Seed = Seed( a ^ n0, b ^ n1, c, d )
    while ( i < 16 ) { seed = seed.next; i += 1 }
    seed
  }

  /**
   * Generates a Long value.
   *
   * The values will be uniformly distributed.
   */
  def long : ( Seed, Long ) = ( next, d )

  /**
   * Generates a long value in [origin; bound[
   *
   * @param origin the (inclusive) lower bound
   * @param bound the (exclusive) upper bound
   */
  def long( origin : Long, bound : Long ) : ( Seed, Long ) = {
    if ( origin < bound ) {
      val range = bound - origin
      val m = range - 1
      if ( ( range & m ) == 0L ) longPowerOfTwo( origin, m )
      else if ( range > 0L ) longRange( origin, range )
      else longUnrepresentable( origin, bound )
    } else long
  }

  private def longPowerOfTwo( origin : Long, mask : Long ) : ( Seed, Long ) =
    long.map( r => r & mask + origin )

  @tailrec
  private def longRange( origin : Long, range : Long ) : ( Seed, Long ) = {
    val ( nextSeed, r ) = long
    val u = r >>> 1
    val l = u % range
    if ( u + ( range - 1 ) - l < 0L )
      nextSeed.longRange( origin, range )
    else
      ( nextSeed, origin + l )
  }

  @tailrec
  private def longUnrepresentable( origin : Long, bound : Long ) : ( Seed, Long ) = {
    val ( nextSeed, r ) = long
    if ( r >= origin && r < bound ) ( nextSeed, r ) else nextSeed.longUnrepresentable( origin, bound )
  }

  /**
   * Generates a Double value.
   *
   * The values will be uniformly distributed, and will be contained
   * in the interval [0.0, 1.0).
   */
  def double : ( Seed, Double ) = ( next, ( d >>> 11 ) * 1.1102230246251565e-16 )
}

object Seed {

  private[valuellama] case class apply( a : Long, b : Long, c : Long, d : Long ) extends Seed

  /** Generate a deterministic seed. */
  def apply( s : Long ) : Seed = {
    var i = 0
    var seed : Seed = Seed( 0xf1ea5eed, s, s, s )
    while ( i < 20 ) { seed = seed.next; i += 1 }
    seed
  }

  /** Generate a random seed. */
  def random() : Seed = apply( scala.util.Random.nextLong )

}
