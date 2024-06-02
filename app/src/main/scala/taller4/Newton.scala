
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Newton{

  def mostrarFormula(expr: Expr): String = expr match {
    case Numero(d) => s"$d"
    case Atomo(x) => s"$x"
    case Suma(e1, e2) => s"(${mostrarFormula(e1)} + ${mostrarFormula(e2)})"
    case Prod(e1, e2) => s"(${mostrarFormula(e1)} * ${mostrarFormula(e2)})"
    case Resta(e1, e2) => s"(${mostrarFormula(e1)} - ${mostrarFormula(e2)})"
    case Div(e1, e2) => s"(${mostrarFormula(e1)} / ${mostrarFormula(e2)})"
    case Expo(e1, e2) => s"(${mostrarFormula(e1)} ^ ${mostrarFormula(e2)})"
    case Logaritmo(e1) => s"log(${mostrarFormula(e1)})"
  }

  def derivar(f:Expr,a:Atomo):Expr = f match {
    case Numero(d) => Numero(0)
    case Atomo(ato) => if (ato == a.x) Numero(1) else Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1,a),derivar(e2,a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1,a),e2),Prod(e1,derivar(e2,a)))
    case Resta(e1, e2) => Resta(derivar(e1,a),derivar(e2,a))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1,a),e2),Prod(e1,derivar(e2,a))), Expo(e2, Numero(2)))
    case Logaritmo(e1) => Div(derivar(e1,a), e1)
    case Expo(e1, e2) => Prod(e1,Suma( Div(Prod(derivar(e1,a),e2),e1)   ,  Prod(derivar(e2,a),Logaritmo(e1))))
  }

  def evaluar(f:Expr,a:Atomo , v: Double):Double = f match {
    case Numero(d) => d.toDouble
    case Atomo(x) => if (x == a.x) v else 0
    case Suma(e1, e2) => (evaluar(e1,a,v) + evaluar(e2,a,v))
    case Prod(e1, e2) => (evaluar(e1,a,v) * evaluar(e2,a,v))
    case Resta(e1, e2) => (evaluar(e1,a,v) - evaluar(e2,a,v))
    case Div(e1, e2) => (evaluar(e1,a,v) / evaluar(e2,a,v))
    case Logaritmo(e1) => scala.math.log(evaluar(e1,a,v))
    case Expo(e1, e2) => scala.math.pow(evaluar(e1,a,v),evaluar(e2,a,v) )
  }

  def limpiar(f:Expr):Expr = f match {
    case Numero(d) => Numero(d)
    case Atomo(ato) => Atomo(ato)
    case Suma(e1, e2) => if (limpiar(e1) == Numero(0)) limpiar(e2) else if (limpiar(e2) == Numero(0)) limpiar(e1) else Suma(limpiar(e1),limpiar(e2))
    case Prod(e1, e2) => 
      if (limpiar(e1) == Numero(1)) limpiar(e2) else if (limpiar(e2) == Numero(1)) limpiar(e1) else if (limpiar(e1) == Numero(0)) Numero(0) else if (limpiar(e2) == Numero(0)) Numero(0) else Prod(limpiar(e1),limpiar(e2))
    case Resta(e1, e2) => if (limpiar(e1) == Numero(0)) limpiar(e2) else if (limpiar(e2) == Numero(0)) limpiar(e1) else Resta(limpiar(e1),limpiar(e2))
    case Div(e1, e2) => 
      val resul2 = limpiar(e2)
      if (resul2 == Numero(1)) limpiar(e1) else Div(limpiar(e1),resul2)
    case Logaritmo(e1) => Logaritmo(e1)
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
  }

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    def iter(x: Double): Double = {
      if (ba(f, a, x)) x
      else {
        val fx = evaluar(f, a, x)
        val fpx = evaluar(derivar(f, a), a, x)
        iter(x - fx / fpx)
      }
    }

    iter(x0)
  }

  def buenaAprox(f: Expr, a: Atomo, x: Double): Boolean = {
    val fx = evaluar(f, a, x)
    scala.math.abs(fx) < 0.001
  }

  def main(args: Array[String]): Unit = {
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )
  }
 }