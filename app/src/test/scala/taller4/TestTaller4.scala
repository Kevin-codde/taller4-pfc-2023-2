/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
    test("testTaller4"){
        assert("Taller 4" == Taller4.saludo())
    }
    test("mostrarFormula"){
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        assert("(10.0 + (x * 5.0))" == Newton.mostrarFormula(expresion) && "(10.0 + (x * 5.0))" == NewtonPar.mostrarFormula(expresion))
    }
    test("derivada"){
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        val resultado = Suma(Numero(0.0),Suma(Prod(Numero(1.0),Numero(5.0)),Prod(Atomo('x'),Numero(0.0))))
        assert(resultado == Newton.derivar(expresion,Atomo('x')) && resultado == NewtonPar.derivar(expresion,Atomo('x')))
    }
    test("evaluacion"){
        val expresion = Suma(Numero(10.0), Prod(Atomo('x'), Numero(5.0)))
        assert(35.0 == Newton.evaluar(expresion,Atomo('x'),5.0) && 35.0 == NewtonPar.evaluar(expresion,Atomo('x'),5.0))
    }
    test("limpieza"){
        val expresion = Suma(Numero(0.0),Suma(Prod(Numero(1.0),Numero(5.0)),Prod(Atomo('x'),Numero(0.0))))
        val resultado = Numero(5.0)
        assert(resultado == Newton.limpiar(expresion) && resultado == NewtonPar.limpiar(expresion))
    }
}
