package calculator

import java.lang.Math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    // e.g. Δ = b² - 4ac
    Signal {
      pow(b(), 2) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // e.g. (-b ± √Δ) / 2a
    Signal {
      val d = delta()
      if (d < 0) Set()
      else {
        val aVal = a()
        val bVal = b()
        Set(
          (-bVal + sqrt(d) / 2 * aVal),
          (-bVal - sqrt(d) / 2 * aVal)
        )
      }
    }
  }
}
