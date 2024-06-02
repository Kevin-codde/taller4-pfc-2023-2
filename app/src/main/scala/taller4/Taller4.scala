
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

import common._

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
    val evaluar = Newton.evaluar(expresion,Atomo('x'),5.0)
    println(s"la evaluacion es: $evaluar")
    //val linpio = Newton.limpiar(derivada)
    val linpio = NewtonPar.limpiar(derivada)
    println(s"la limpieza es: $linpio")  

    val expresionNormal: Expr = Suma(
      Prod(Numero(2), Atomo('x')),
      Div(
        Resta(
          Numero(5),
          Prod(Atomo('y'), Numero(3))
        ),
        Numero(4)
      )
    )
    println(" pruebas tamaño expresionNormal")
    println(compararMostrar(Newton.mostrarFormula,NewtonPar.mostrarFormula)(expresionNormal))
    println(compararDerivar(Newton.derivar,NewtonPar.derivar)(expresionNormal,Atomo('x')))
    println(compararEvaluar(Newton.evaluar,NewtonPar.evaluar)(expresionNormal,Atomo('x'),5.0))
    println(compararLimpiar(Newton.limpiar,NewtonPar.limpiar)(expresionNormal))

    val expresion_grande: Expr = Resta(
      Suma(
        Prod(
          Numero(2),
          Atomo('x')
        ),
        Div(
          Resta(
            Numero(5),
            Prod(
              Atomo('y'),
              Numero(3)
            )
          ),
          Numero(4)
        )
      ),
      Expo(
        Logaritmo(
          Atomo('z')
        ),
        Numero(2)
      )
    )
    println(" pruebas tamaño expresion_grande")
    println(compararMostrar(Newton.mostrarFormula,NewtonPar.mostrarFormula)(expresion_grande))
    println(compararDerivar(Newton.derivar,NewtonPar.derivar)(expresion_grande,Atomo('x')))
    println(compararEvaluar(Newton.evaluar,NewtonPar.evaluar)(expresion_grande,Atomo('x'),5.0))
    println(compararLimpiar(Newton.limpiar,NewtonPar.limpiar)(expresion_grande))

  }
 
def compararMostrar(funcion1: (Expr) => String, funcion2: (Expr) => String)
                        (Expr1: Expr): (Double, Double, Double) = {

    val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
      funcion1(Expr1)
    }

    val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
      funcion2(Expr1)
    }

    val tiempo1: Double = tiempoFuncion1.value
    val tiempo2: Double = tiempoFuncion2.value

    val aceleracion = tiempo1 / tiempo2

    (tiempo1, tiempo2, aceleracion)
  }

def compararDerivar(funcion1: (Expr, Atomo) => Expr, funcion2: (Expr, Atomo) => Expr)
                   (expresion: Expr, atom: Atomo): (Double, Double, Double) = {

  val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
    funcion1(expresion, atom)
  }

  val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
    funcion2(expresion, atom)
  }

  val tiempo1: Double = tiempoFuncion1.value
  val tiempo2: Double = tiempoFuncion2.value

  val aceleracion = tiempo1 / tiempo2

  (tiempo1, tiempo2, aceleracion)
}

def compararEvaluar(funcion1: (Expr, Atomo, Double) => Double, funcion2: (Expr, Atomo, Double) => Double)
                    (expresion: Expr, atom: Atomo, valor: Double): (Double, Double, Double) = {

  val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
    funcion1(expresion, atom, valor)
  }

  val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
    funcion2(expresion, atom, valor)
  }

  val tiempo1: Double = tiempoFuncion1.value
  val tiempo2: Double = tiempoFuncion2.value

  val aceleracion = tiempo1 / tiempo2

  (tiempo1, tiempo2, aceleracion)
}

def compararLimpiar(funcion1: Expr => Expr, funcion2: Expr => Expr)
                    (expresion: Expr): (Double, Double, Double) = {

  val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
    funcion1(expresion)
  }

  val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
    funcion2(expresion)
  }

  val tiempo1: Double = tiempoFuncion1.value
  val tiempo2: Double = tiempoFuncion2.value

  val aceleracion = tiempo1 / tiempo2

  (tiempo1, tiempo2, aceleracion)
}
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