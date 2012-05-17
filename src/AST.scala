/*
Clases del AST de Nyota
Gastón Silva Echegaray 1162324
Rafael Santos Pérez 1161734
*/
package nyota {

  import scala.util.parsing.input.Position
  
  abstract class Tipo
   case object TInteger extends Tipo
   case object TBoolean extends Tipo
   case object TReal extends Tipo
   case object TString extends Tipo
   case object TVoid extends Tipo
   
  case class Pos(line: Int, column: Int) {
    //override def toString: String = "@["+line+", "+column+"]"//Para el AST
    override def toString: String = "línea: "+line+", posición: "+column//Para analisis semantico
  }
   
  sealed abstract class Nodo {
    val INDENT = "    "
    // Obtener el nombre simple de la instancia de esta clase.    
    def nombreClase = {
      val arreglo = this.getClass.getName.split("""\.""")
      "{"+arreglo(arreglo.length - 1)+"}"
    }
    def dump(): String = dump("")
    def dump(indt: String) = "%s%s".format(indt, nombreClase)        
    def dump(indt: String, lst: List[Nodo]): String =
      lst.map(_.dump(indt)).mkString
  }
  
  case class Programa (
    variables:  List[Variable], 
    procedimientos:     List[Procedimiento],
    enunciados:     List[Enunciado]
  ) extends Nodo {
    override def dump(indt: String) =
      "%n%s%s%n%s%s%s".format(indt, nombreClase, 
                          dump(indt + INDENT, variables),
                          dump(indt + INDENT, procedimientos),
                          dump(indt + INDENT, enunciados))
  }
  
  case class Variable(
    varID: Identificador, tipo:Tipo
  ) extends Nodo {
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s %s:%s".format(
        i, nombreClase, varID.nombre, tipo)
    }
  }
  
  case class Procedimiento(
    procID: Identificador, tipo:Tipo, parametros: List[Parametro],
    variables: List[Variable], enunciados: List[Enunciado]
  ) extends Nodo {
    override def dump(indt: String) ={
      val i = indt+INDENT
      "%n%s%s %s%s:%s %n%s%n%s%n%s ".format(
        i, nombreClase, procID.nombre, procID.pos, tipo,
        dump(i, parametros), dump(i, variables), dump(i, enunciados) )
    }
  }
  
  case class Parametro(paramID: Identificador, tipo:Tipo) extends Nodo {
    override def dump(indt: String) =
      "%n%s%s %s%s: %s".format(
         indt+INDENT, nombreClase, paramID.nombre, paramID.pos, tipo)
  }
  
  // enunciados
  abstract class Enunciado() extends Nodo {}
  
  case class EnunAsignacion(
    enunAsigID: Identificador, expr:Expresion
  ) extends Enunciado {
    override def dump(indt: String) ={
      val i = indt+INDENT
      "%n%s%s %s%s: %n%s".format(i, nombreClase, enunAsigID.nombre, enunAsigID.pos, expr.dump(i) )
    }
  }
  
  case class EnunInvocacion(
    invoID: Identificador, expresiones: List[Expresion]
  ) extends Enunciado {
    override def dump(indt: String) ={
      val i = indt+INDENT
      "%n%s%s %s%s: %n%s".format(i, nombreClase, invoID.nombre, invoID.pos, dump(i, expresiones) )
    }
  }
  
  case class EnunIf(
    enunciadosIfElse: List[ EnunElseIf ],
    enunciadosElse: List[Enunciado]
  ) extends Enunciado {
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s if: %n%s%n%s".
        format(i, nombreClase,
          dump(i+INDENT, enunciadosIfElse), dump(i+INDENT, enunciadosElse))
    }
  }
  
  // incluye al if solo
  case class EnunElseIf(
    enunElseIf: (Pos, Expresion, List[Enunciado])
  ) extends Enunciado {
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s elseif %s: %n%s".format(
        i, enunElseIf._1, enunElseIf._2, dump(i+INDENT, enunElseIf._3))
    }
  }
  
  case class EnunLoop( enunciados: List[Enunciado] ) extends Enunciado{
    override def dump(indt: String) ={
      val i = indt+INDENT
      "%n%s%s loop %s".format(i, nombreClase, dump(i, enunciados) )
    }
  }
  
  case class EnunReturn(
    posReturn: Pos, expr: Expresion
  ) extends Enunciado {
    override def dump(indt: String) = {
      val i:String = indt+INDENT
      "%n%s%s return%s: %s".format(i, nombreClase, posReturn, expr.dump(i) )
    }
  }
  
  case class EnunExit(
    posExit: Pos
  ) extends Enunciado {
    override def dump(indt: String) ={
      val i = indt+INDENT
      "%n%s%s return%s".format(i, nombreClase, posExit )
    }
  }
  
  // expresiones
  abstract class Expresion() extends Nodo {
    val posOper: Pos
  }
  
  case class Identificador(nombre: String, pos: Pos) extends Expresion {
    val posOper: Pos = pos
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s %s%s".format(i, nombreClase, nombre, pos)
    }
  }
  
  abstract class ExpresionUnaria() extends Expresion {
    val exprIzq: Expresion
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s %s: %s".format(i, nombreClase, posOper, exprIzq.dump(i+INDENT) )
    }
  }
  
  abstract class ExpresionBinaria() extends ExpresionUnaria {
    val exprDer: Expresion
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s %s: %s%s".format(
       i, nombreClase, posOper, exprIzq.dump(i+INDENT), exprDer.dump(i+INDENT) )
    }
  }
  
  abstract class ExpresionLogica() extends ExpresionBinaria
  
  case class ExpresionAnd(posAnd:Pos, izq: Expresion, der:Expresion) extends ExpresionLogica {
    val posOper:Pos = posAnd
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionAndAlso(posAndAlso:Pos, izq: Expresion, der:Expresion) extends ExpresionLogica {
    val posOper:Pos = posAndAlso
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionOr(posOr:Pos, izq: Expresion, der:Expresion) extends ExpresionLogica {
    val posOper:Pos = posOr
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionOrElse(posOrElse:Pos, izq: Expresion, der:Expresion) extends ExpresionLogica {
    val posOper:Pos = posOrElse
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionXor(posXor:Pos, izq: Expresion, der:Expresion) extends ExpresionLogica {
    val posOper:Pos = posXor
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  abstract class ExpresionRelacional() extends ExpresionBinaria
  
  case class ExpresionIgual(posIgual:Pos, izq: Expresion, der:Expresion) extends ExpresionRelacional {
    val posOper:Pos = posIgual
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionDiferente(posDif:Pos, izq: Expresion, der:Expresion) extends ExpresionRelacional {
    val posOper:Pos = posDif
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
    //println("Izq#\n\t\t"+exprIzq+"\nDer#\n\t\t"+exprDer)
  }
  
  case class ExpresionMenor(posMenor:Pos, izq: Expresion, der:Expresion) extends ExpresionRelacional {
    val posOper:Pos = posMenor
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq  
  }
  
  case class ExpresionMayor(posMayor:Pos, izq: Expresion, der:Expresion) extends ExpresionRelacional {
    val posOper:Pos = posMayor
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionMenorIgual(
    posMenorIgual:Pos, izq: Expresion, der:Expresion
  ) extends ExpresionRelacional {
  val posOper:Pos = posMenorIgual
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionMayorIgual(
    posMayorIgual:Pos, izq: Expresion, der:Expresion
  ) extends ExpresionRelacional {
    val posOper:Pos = posMayorIgual
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  abstract class ExpresionAritmetica extends ExpresionBinaria
  
  case class ExpresionSuma(posSuma:Pos, izq: Expresion, der:Expresion) extends ExpresionAritmetica {
    val posOper:Pos = posSuma
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionResta(posResta:Pos, izq: Expresion, der:Expresion) extends ExpresionAritmetica {
    val posOper:Pos = posResta
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionMultiplicacion(posMulti:Pos, izq: Expresion, der:Expresion) extends ExpresionAritmetica {
    val posOper:Pos = posMulti
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionDivision(posDiv:Pos, izq: Expresion, der:Expresion) extends ExpresionAritmetica {
    val posOper:Pos = posDiv
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  // division entera
  case class ExpresionCociente(posCociente:Pos, izq: Expresion, der:Expresion) extends ExpresionAritmetica {
    val posOper:Pos = posCociente
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionResiduo(posResiduo:Pos, izq: Expresion, der:Expresion) extends ExpresionAritmetica {
    val posOper:Pos = posResiduo
    val exprDer:Expresion = der
    val exprIzq:Expresion = izq
  }
  
  case class ExpresionNot(posNot: Pos, expr: Expresion) extends ExpresionUnaria {
    val posOper:Pos = posNot
    val exprIzq:Expresion = expr
  }
  
  case class ExpresionNegacion(posNegacion:Pos, expr: Expresion) extends ExpresionUnaria {
    val posOper:Pos = posNegacion
    val exprIzq:Expresion = expr
  }
  
  case class ExpresionInvocacion(
    invoID: Identificador, params: List[Expresion]) 
  extends Expresion {
    val posOper: Pos = invoID.pos
    override def dump(indt: String) = {
      val i = indt+INDENT
      "%n%s%s %s%s:%n%s".format(
        i, nombreClase, invoID.nombre, posOper, dump(i+INDENT, params) )
    }
  }
  
  // es la invocacion de la varibale
  case class ExpresionVariable(
    posID:Pos, nombre:String
  ) extends Expresion {
    val posOper: Pos = posID
    override def dump(indt: String) =
      "%n%s%s %s".format(indt+INDENT, nombre, posOper)
  }
  
  case class Literal(posLit: Pos, valor:String)  extends Expresion {
    val posOper: Pos = posLit
    override def dump(indt: String) =
      "%n%s%s%s %s".format(indt+INDENT, nombreClase, valor, posLit)
  }
  
  case class LiteralInteger(pos: Pos, v: String) extends Literal(pos, v)
  
  case class LiteralReal(pos:Pos, v: String) extends Literal(pos, v)
  
  case class LiteralString(pos:Pos, v: String) extends Literal(pos, v)
  
  case class LiteralBoolean(pos:Pos, v: String) extends Literal(pos, v)
  
  case class LiteralVoid() extends Expresion {
    val posOper: Pos = Pos(0, 0)
    override def dump(indt: String) = ""
  }
                     
}
