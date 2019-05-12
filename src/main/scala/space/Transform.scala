package space

import Implicits._

trait Transform {
  def transform(t: Trajectory): Concept

  def inverse(c: Concept): Trajectory
}

object Direction {

  sealed trait Direction

  trait Forward extends Direction

  trait Inverse extends Direction

  object Forward extends Forward

  object Inverse extends Inverse

}

trait FastFourierTransform extends Transform {

  def transform(trajectory: Trajectory): Concept = Concept(
    fft(trajectory.tensor.pad, Direction.Forward))

  def inverse(concept: Concept): Trajectory = Trajectory(
    fft(concept.tensor, Direction.Inverse) match {
      case c: Complex => Vector(Concept(c))
      case t: Tensor => t.ts.map(c => Concept(c))
    })

  def fft(t: TensorLike, dir: Direction.Direction): TensorLike = {

    def _fft(t: TensorLike): TensorLike = t match {
      case base if t.length == 1 => base
      case t: Tensor =>
        val n = t.length
        (_fft(t.evens), _fft(t.odds)) match {
          case (evens: Tensor, odds: Tensor) =>
            val c1 = (0 until n / 2) map (m => evens(m) + odds(m) * omega(n, m))
            val c2 = (0 until n / 2) map (m => evens(m) - odds(m) * omega(n, m))
            Tensor((c1 ++ c2).toVector)
        }
    }

    def omega(n: Int, m: Int): Complex = (dir match {
      case _: Direction.Forward => (-2 * math.Pi * Complex.i * m / n).exp
      case _: Direction.Inverse => (2 * math.Pi * Complex.i * m / n).exp
    }).asInstanceOf[Complex]

    val done = _fft(t)
    dir match {
      case _: Direction.Forward => done
      case _: Direction.Inverse => (1.0 / t.length) * done
    }
  }
}
