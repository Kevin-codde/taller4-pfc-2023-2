/**
 * Taller 4 - Programación Funcional
 * Autores: Kevin Andres Bejarano-2067678,Juan david Gutierrez-2060104,Johan Sebastian Acosta-2380393
 * Profesor: Carlos A Delgado
 */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

trait Expr
case class Numero ( d : Double ) extends Expr
case class Atomo ( x : Char ) extends Expr
case class Suma ( e1 : Expr , e2 : Expr ) extends Expr
case class Prod ( e1 : Expr , e2 : Expr ) extends Expr
case class Resta ( e1 : Expr , e2 : Expr ) extends Expr
case class Div ( e1 : Expr , e2 : Expr ) extends Expr
case class Expo ( e1 : Expr , e2 : Expr ) extends Expr
case class Logaritmo ( e1 : Expr ) extends Expr





object Taller4{

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )
    val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
    //val formula = Newton.mostrarFormula(expresion)
    val formula = NewtonPar.mostrarFormula(expresion)
    println(s"La fórmula es: $formula")
    //val derivada = Newton.derivar(expresion,Atomo('x'))
    val derivada = NewtonPar.derivar(expresion,Atomo('x'))
    println(s"la derivada es: $derivada")
    //val evaluar = Newton.evaluar(expresion,Atomo('x'),5.0)
    val evaluar = NewtonPar.evaluar(expresion,Atomo('x'),5.0)
    println(s"la evaluacion es: $evaluar")
    //val linpio = Newton.limpiar(derivada)
    val linpio = NewtonPar.limpiar(derivada)
    println(s"la limpieza es: $linpio")

    // Ejemplos de expresiones
    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
    val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
    val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))
    val e4 = Suma(Prod(Atomo('x'), Numero(5.0)), Numero(3.0))
    val e5 = Div(Suma(Numero(3.0), Prod(Atomo('x'), Numero(5.0))), Numero(2.0))

    // Invocaciones colocar las invocaciones pero las de la clase NewtonPar

    //val res1 = Newton.raizNewton(e1, Atomo('x'), 2.0, Newton.buenaAprox)
    val res1 = NewtonPar.raizNewton(e1, Atomo('x'), 2.0,NewtonPar.buenaAproxi)

    //val res2 = Newton.raizNewton(e2, Atomo('x'), 2.0, Newton.buenaAprox)
    val res2 = NewtonPar.raizNewton(e2, Atomo('x'), 2.0, NewtonPar.buenaAproxi)

    //val res3 = Newton.raizNewton(e3, Atomo('x'), 3.0, Newton.buenaAprox)
    val res3 = NewtonPar.raizNewton(e3, Atomo('x'), 3.0, NewtonPar.buenaAproxi)

    //val res4 = Newton.raizNewton(e4, Atomo('x'), 4.0, Newton.buenaAprox)
    val res4 = NewtonPar.raizNewton(e4, Atomo('x'), 4.0, NewtonPar.buenaAproxi)

    //val res5 = Newton.raizNewton(e5, Atomo('x'), 2.0, Newton.buenaAprox)
    val res5 = NewtonPar.raizNewton(e5, Atomo('x'), 2.0, NewtonPar.buenaAproxi)

    // Imprimir resultados
    println(res1)
    println(res2)
    println(res3)
    println(res4)
    println(res5)


  }
}




