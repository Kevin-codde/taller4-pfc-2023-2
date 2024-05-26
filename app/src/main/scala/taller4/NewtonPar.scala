/**
 * Taller 4 - Programación Funcional
 * Autores: Kevin Andres Bejarano-2067678,Juan david Gutierrez-2060104,Johan Sebastian Acosta-2380393
 * Profesor: Carlos A Delgado
 */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

import common._

object NewtonPar{

  def mostrarFormula(expr: Expr): String = expr match {
    case Numero(d) => s"$d"
    case Atomo(x) => s"$x"
    case Suma(e1, e2) => 
      val (resul1, resul2) = parallel(mostrarFormula(e1), mostrarFormula(e2))
      s"(${resul1} + ${resul2})"
    case Prod(e1, e2) => 
      val (resul1, resul2) = parallel(mostrarFormula(e1), mostrarFormula(e2))
      s"(${resul1} * ${resul2})"
    case Resta(e1, e2) => 
      val (resul1, resul2) = parallel(mostrarFormula(e1), mostrarFormula(e2))
      s"(${resul1} - ${resul2})"
    case Div(e1, e2) => 
      val (resul1, resul2) = parallel(mostrarFormula(e1), mostrarFormula(e2))
      s"(${resul1} / ${resul2})"
    case Expo(e1, e2) => 
      val (resul1, resul2) = parallel(mostrarFormula(e1), mostrarFormula(e2))
      s"(${resul1} ^ ${resul2})"
    case Logaritmo(e1) => s"log(${mostrarFormula(e1)})"
  }

  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(d) => Numero(0)
    case Atomo(ato) => if (ato == a.x) Numero(1) else Numero(0)
    case Suma(e1, e2) => 
      val (resul1, resul2) = parallel(derivar(e1, a), derivar(e2, a))
      Suma(resul1, resul2)
    case Prod(e1, e2) => 
      val (resul1, resul2) = parallel(derivar(e1, a), derivar(e2, a))
      Suma(Prod(resul1, e2), Prod(e1, resul2))
    case Resta(e1, e2) => 
      val (resul1, resul2) = parallel(derivar(e1, a), derivar(e2, a))
      Resta(resul1, resul2)
    case Div(e1, e2) => 
      val (resul1, resul2) = parallel(derivar(e1, a), derivar(e2, a))
      Div(Resta(Prod(resul1, e2), Prod(e1, resul2)), Expo(e2, Numero(2)))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
    case Expo(e1, e2) => 
      val (resul1, resul2) = parallel(derivar(e1, a), derivar(e2, a))
      Prod(e1, Suma(Div(Prod(resul1, e2), e1), Prod(resul2, Logaritmo(e1))))
  }

  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) => v
    case Suma(e1, e2) => 
      val (resul1, resul2) = parallel(evaluar(e1, a, v), evaluar(e2, a, v))
      resul1 + resul2
    case Prod(e1, e2) => 
      val (resul1, resul2) = parallel(evaluar(e1, a, v), evaluar(e2, a, v))
      resul1 * resul2
    case Resta(e1, e2) => 
      val (resul1, resul2) = parallel(evaluar(e1, a, v), evaluar(e2, a, v))
      resul1 - resul2
    case Div(e1, e2) => 
      val (resul1, resul2) = parallel(evaluar(e1, a, v), evaluar(e2, a, v))
      resul1 / resul2
    case Logaritmo(e1) => scala.math.log(evaluar(e1, a, v))
    case Expo(e1, e2) => 
      val (resul1, resul2) = parallel(evaluar(e1, a, v), evaluar(e2, a, v))
      scala.math.pow(resul1, resul2)
  }

  def limpiar(f: Expr): Expr = f match {
    case Numero(d) => Numero(d)
    case Atomo(ato) => Atomo(ato)
    case Suma(e1, e2) =>
      val (resul1, resul2) = parallel(limpiar(e1), limpiar(e2))
      if (resul1 == Numero(0)) resul2
      else if (resul2 == Numero(0)) resul1
      else Suma(resul1, resul2)
    case Prod(e1, e2) =>
      val (resul1, resul2) = parallel(limpiar(e1), limpiar(e2))
      if (resul1 == Numero(1)) resul2
      else if (resul2 == Numero(1)) resul1
      else if (resul1 == Numero(0) || resul2 == Numero(0)) Numero(0)
      else Prod(resul1, resul2)
    case Resta(e1, e2) =>
      val (resul1, resul2) = parallel(limpiar(e1), limpiar(e2))
      if (resul1 == Numero(0)) resul2
      else if (resul2 == Numero(0)) resul1
      else Resta(resul1, resul2)
    case Div(e1, e2) =>
      val resul2 = limpiar(e2)
      if (limpiar(e2) == Numero(1)) limpiar(e1)
      else Div(limpiar(e1), resul2)
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case Expo(e1, e2) =>
      val (resul1, resul2) = parallel(limpiar(e1), limpiar(e2))
      Expo(resul1, resul2)
  }


  // Implementación paralelizada de la función raizNewton
  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    @annotation.tailrec
    def iter(x: Double): Double = {
      val (fx, dfx) = parallel(evaluar(f, a, x), evaluar(derivar(f, a), a, x))
      if (dfx == 0) x
      else {
        val nextX = x - fx / dfx
        if (ba(f, a, nextX)) nextX
        else iter(nextX)
      }
    }

    iter(x0)
  }

  // Criterio de buena aproximación
  def buenaAproxi(f: Expr, a: Atomo, x: Double): Boolean = {
    val fx = evaluar(f, a, x)
    scala.math.abs(fx) < 0.001
  }
}




