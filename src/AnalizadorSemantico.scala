/*
Analizador Semantico del Lenguaje Nyota
Gastón Silva Echegaray 1162324
Rafael Santos Pérez 1161734
*/
package nyota {
  
  import AnalizadorSemantico.TablaDeSimbolos
  
  class SemanticException(mensaje: String) extends Exception(mensaje)
  
  abstract class TableEntry(t: Tipo)
    case class VarLocalEntry(tipo: Tipo) extends TableEntry(tipo)
    case class VarEntry(tipo: Tipo) extends TableEntry(tipo)
    case class ParamEntry(tipo: Tipo) extends TableEntry(tipo)
    case class ProcEntry(
      tipo: Tipo, lstParams:List[Tipo], tabla: TablaDeSimbolos
    ) extends TableEntry(tipo)
    case class ProcLibEntry(
      t: Tipo, lst: List[Tipo]
    ) extends ProcEntry(t, lst, Map[String, TableEntry]())
  
  object AnalizadorSemantico {
  
    type TablaDeSimbolos = Map[String, TableEntry]
    
    def initTablaLib(): TablaDeSimbolos = Map[String, TableEntry]() +
        // biblioteca estandar
        ("WrInt" -> ProcLibEntry(TVoid, List(TInteger)))+
        ("WrReal" -> ProcLibEntry(TVoid, List(TReal)))+
        ("WrStr" -> ProcLibEntry(TVoid, List(TString)))+
        ("WrBool" -> ProcLibEntry(TVoid, List(TBoolean)))+
        ("WrLn" -> ProcLibEntry(TVoid, Nil))+
        ("RdInt" -> ProcLibEntry(TInteger, Nil))+
        ("RdReal" -> ProcLibEntry(TReal, Nil))+
        ("RdStr" -> ProcLibEntry(TString, Nil))+
        ("AtStr" -> ProcLibEntry(TString, List(TString, TInteger)))+
        ("LenStr" -> ProcLibEntry(TInteger, List(TString)))+
        ("CmpStr" -> ProcLibEntry(TInteger, List(TString, TString)))+
        ("CatStr" -> ProcLibEntry(TString, List(TString, TString)))+
        ("IntToReal" -> ProcLibEntry(TReal, List(TInteger)))+
        ("RealToInt" -> ProcLibEntry(TInteger, List(TReal)))+
        ("IntToStr" -> ProcLibEntry(TString, List(TInteger)))+
        ("StrToInt" -> ProcLibEntry(TInteger, List(TString)))+
        ("RealToStr" -> ProcLibEntry(TString, List(TReal)))+
        ("StrToReal" -> ProcLibEntry(TReal, List(TString)))+
        // biblioteca extendida
        ("Fibonacci" -> ProcLibEntry(TVoid, List(TInteger)))+
        ("Nones" -> ProcLibEntry(TVoid, List(TInteger, TInteger)))+
        ("Pares" -> ProcLibEntry(TVoid, List(TInteger, TInteger)))+
        ("Factorial" -> ProcLibEntry(TInteger, List(TInteger)))+
        ("Binario" -> ProcLibEntry(TInteger, List(TInteger)))+
        ("Pino" -> ProcLibEntry(TVoid, List(TInteger)))+
        ("GB" -> ProcLibEntry(TVoid, List(TInteger)))+
        ("FraseDelDia" -> ProcLibEntry(TString, Nil))+
        ("IntPow" -> ProcLibEntry(TInteger, List(TInteger, TInteger)))+
        ("RealPow" -> ProcLibEntry(TReal, List(TReal, TReal)))+
        ("Floor" -> ProcLibEntry(TReal, List(TReal)))+
        ("Ceil" -> ProcLibEntry(TReal, List(TReal)))+
        ("Time" -> ProcLibEntry(TString, Nil))+
        ("Date" -> ProcLibEntry(TString, Nil))+
        ("SqrtInt" -> ProcLibEntry(TReal, List(TInteger)))+
        ("SqrtReal" -> ProcLibEntry(TReal, List(TReal)))+
        ("AbsInt" -> ProcLibEntry(TReal, List(TInteger)))+
        ("AbsReal" -> ProcLibEntry(TReal, List(TReal)))+
        ("RandInt" -> ProcLibEntry(TInteger, Nil))+
        ("RandIntSeed" -> ProcLibEntry(TInteger, List(TInteger)))+
        ("RandReal" -> ProcLibEntry(TReal, Nil))+
        ("RandRealSeed" -> ProcLibEntry(TReal, List(TInteger)))+
        ("Rot13" -> ProcLibEntry(TString, List(TString)))
        
    def analizaPrograma(prog: Programa): TablaDeSimbolos = {
      var tabla = initTablaLib()
      // Meter variables a la tabla
      for (v <- prog.variables ) {
        val nombre = v.varID.nombre
        val pos = v.varID.pos
        val condicion = !tabla.contains(nombre)
        val mensaje = "Variable '%s' previamente declarada en %s".
                      format(nombre, pos)
        verifica(condicion,mensaje)
        tabla = tabla + (nombre -> VarEntry(v.tipo))
      }
      // Verificar/Analizar/Meter procedimientos a tabla global
      // fold, para pasar una tabla actualizada a cada procedimiento subsecuente
      tabla = (tabla/:prog.procedimientos)((acum,p)=>acum++List(analizaProcedimiento(p,acum)))
      // Analizar enunciados
      val exitProgram = false
      verificaReturn(prog.enunciados, tabla, exitProgram, TVoid)
      // regresar tabla
      tabla
    }

    def analizaProcedimiento(
      proc: Procedimiento, tabla: TablaDeSimbolos
    ): (String, TableEntry) = {
      val nombre = proc.procID.nombre
      val pos = proc.procID.pos
      val condicion = !tabla.contains(nombre)
      val mensaje = "Procedimiento '%s' previamente declarado en %s".
        format(nombre, pos)
      verifica(condicion, mensaje)
      // Generar lista de tipos de los parametros
      val lstParamTipos = for ( par <- proc.parametros ) yield par.tipo
      val tablaLocal: TablaDeSimbolos = {
          val parametros = Map[String, TableEntry]() ++ {for(p <- proc.parametros) yield (p.paramID.nombre,ParamEntry(p.tipo))}
          val variables =  Map[String, TableEntry]() ++ {
              for (v <- proc.variables if (!(parametros contains v.varID.nombre))) yield (v.varID.nombre, VarLocalEntry(v.tipo))
            }
          
          val wrong = proc.variables.filter(x=> !(variables contains x.varID.nombre))
          for (w <- wrong){
            verifica(false,"Variable %s no puede tener el mismo nombre que un parametro en %s".format(w.varID.nombre,w.varID.pos))
          }
          parametros ++ variables
      }
      val tablaProc = tabla ++ tablaLocal + (nombre -> ProcEntry(proc.tipo, lstParamTipos,tablaLocal))
      // valida enunciados
      val numReturn = verificaReturn(proc.enunciados, tablaProc, false, proc.tipo)
      verificaReturnAusente(proc.procID,numReturn, proc.tipo)
      ( nombre, ProcEntry(proc.tipo, lstParamTipos, tablaLocal ))
    }
    
    def analizaEnunciado(
      enun: Enunciado, tabla: TablaDeSimbolos,
      banderaExit:Boolean, procTipo: Tipo):Int = {
      enun match {
        
         case EnunLoop(enunList) => 
          /*var foundExit = 0
          var numReturns = 0
          for( en <- enunList) {
            if(en.isInstanceOf[EnunExit]) { 
              foundExit = foundExit+1
              val mensaje = "Enunciado '%s' inalcanzable en %s".
                format(en.nombreClase, en.asInstanceOf[EnunExit].posExit)
              verifica(!(foundExit>1), mensaje)
            } else {
              if !(en.is
              analizaEnunciado(en, tabla, true, procTipo)
            }
          }*/
          verificaReturn(enunList, tabla, true, procTipo)
          
        case enun: EnunExit =>
          val mensaje = "Enunciado '%s' fuera de contexto en %s".
              format(enun.nombreClase, enun.asInstanceOf[EnunExit].posExit)
            verifica(banderaExit, mensaje)
            
        case enun: EnunReturn =>
          val mensaje = "Enunciado '%s' fuera de contexto en %s".
              format(enun.nombreClase, enun.asInstanceOf[EnunReturn].posReturn)
            verifica(false, mensaje)// ese siempre false, porque tenemos verificaReturn
            
        case EnunInvocacion(
          invoID: Identificador, paramsLst) =>
          val nombre = invoID.nombre
          val pos = invoID.pos
          val condicion = tabla.contains(nombre)
          val mensaje = "Procedimiento '%s' no declarado en %s".
            format(nombre, pos)
          verifica(condicion, mensaje)
          val paramsVerificados: List[Tipo] = paramsLst.map(analizaExpresion(_, tabla))//(misma) tabla
          val procT = tabla(nombre).asInstanceOf[ProcEntry]
          if (procT.tipo != TVoid){
            verifica(false,"Invocación de Procedimiento %s no debe regresar nada en %s".format(nombre,pos))
          } 
          val paramsT = procT.lstParams// solo son tipos NO DEBE contener expresiones desde que se guarda en la tabla de simbolos
          val condicion2 = isParamsIguales(paramsVerificados,paramsT)
          val mensaje2 = "Procedimiento '%s' con argumentos incorrectos en %s esperaba %s, recibio %s".
            format(nombre, pos, if (paramsT.size>0) paramsT.mkString(",") else "-ninguno-", paramsVerificados.mkString(","))
          verifica(condicion2, mensaje2)
          
         case EnunIf(enunciadosIfElse, enunciadosElse) =>
         var numReturns=0
          // if
          for(enun <- enunciadosIfElse) {
            val ifExpr = enun.enunElseIf
            if( analizaExpresion(ifExpr._2, tabla) == TBoolean) {
              numReturns += verificaReturn(ifExpr._3, tabla, banderaExit, procTipo)
            } else {
              // la expresion if no es boolean
              val mensaje = "Expresion '%s' no es boolean en %s".
                format(ifExpr._2, ifExpr._1)
              verifica(false, mensaje)
            }
          }
          // else
          /*enunciadosElse.map(analizaEnunciado(_, tabla,banderaExit))*/
          numReturns += verificaReturn(enunciadosElse, tabla, banderaExit, procTipo)
          if (numReturns>=1) 1 else 0
          
          
         case EnunAsignacion(
          enunAsigID: Identificador, expr:Expresion) =>
          val nombre = enunAsigID.nombre
          val pos = enunAsigID.pos
          //Verificar que esta en la tabla
          var condicion = tabla.contains(nombre) 
          var mensaje = "Variable '%s' no fue declarada en %s".
            format(nombre, pos)
          verifica(condicion, mensaje)
          //Verificar que es una variable
          val entry = tabla(nombre)
          condicion = entry.isInstanceOf[VarLocalEntry] || entry.isInstanceOf[VarEntry] || entry.isInstanceOf[ParamEntry]
          mensaje = "Procedimiento '%s' no puede ser asignado en %s".
            format(nombre, pos)
          verifica(condicion, mensaje)            
          //Verificar que son tipos iguales
          val tipo= entry match{
            case e:VarLocalEntry =>e.tipo
            case e:VarEntry =>e.tipo
            case e:ParamEntry=>e.tipo
          }
          val tipoEncontrado =analizaExpresion(expr, tabla)
          if ( tipo == tipoEncontrado) {
            // todo bien, no hago nada de nada
            0
          } else {
            mensaje = "Variable '%s' no es de tipo %s, sino %s en %s".
              format(nombre, tipoEncontrado, tipo, pos)
            verifica(false, mensaje)
          }
      }

    }
    
    def analizaExpresion(exp: Expresion, tabla: TablaDeSimbolos): Tipo = {
      
      // verifica operaciones
      def verificaOpBinaria(exp2: Expresion, tipo: Tipo) {
        val exp = exp2.asInstanceOf[ExpresionBinaria]
        val condicion = tipo == analizaExpresion(exp.exprIzq, tabla) &&
          tipo == analizaExpresion(exp.exprDer, tabla)
        val mensaje = "Operador '%s' espera operandos de tipo %s en %s".
          format(exp.nombreClase, tipo, exp.posOper)
        verifica( condicion, mensaje)
      }
      
      def verificaOpUnaria(exp2: Expresion, tipo: Tipo) {
        val exp = exp2.asInstanceOf[ExpresionUnaria]
        val condicion = tipo == analizaExpresion(exp.exprIzq, tabla)
        val mensaje = "Operador '%s' espera operandos de tipo %s en %s".
          format(exp.nombreClase, tipo, exp.posOper)
        verifica( condicion, mensaje)
      }
      
      
      //probar cada expresion
      exp match {
        
        case exp: ExpresionLogica =>
          verificaOpBinaria(exp, TBoolean)
          TBoolean
        
        case exp: ExpresionRelacional =>
          verificaTipos(exp, List(TInteger, TReal, TBoolean), verificaOpBinaria)
          TBoolean
          
        case exp: ExpresionAritmetica =>
          verificaTipos(exp, List(TInteger, TReal), verificaOpBinaria)
          
        case exp: ExpresionNegacion =>
          verificaTipos(exp, List(TInteger, TReal), verificaOpUnaria)
        
        case exp: ExpresionNot =>
          verificaOpUnaria(exp, TBoolean)
          TBoolean
          
        case exp: ExpresionVariable =>
          val condicion = tabla.contains(exp.nombre)
          val mensaje = "Variable '%s' no declarada en %s".
            format(exp.nombre, exp.posOper)
          verifica(condicion, mensaje)
          tabla(exp.nombre) match {
            case v:VarLocalEntry => v.tipo
            case v:VarEntry => v.tipo
          }
        
        case ExpresionInvocacion(
          invoID: Identificador, paramsLst) =>
          val nombre = invoID.nombre
          val pos = invoID.pos
          val condicion = tabla.contains(nombre)
          val mensaje = "Procedimiento '%s' no declarado en %s".
            format(nombre, pos)
          verifica(condicion, mensaje)
          val paramsTiposVerificados: List[Tipo] = paramsLst.map(analizaExpresion(_, tabla))//(misma) tabla
          val procT = tabla(nombre).asInstanceOf[ProcEntry]
          val paramsT = procT.lstParams// solo son tipos NO DEBE contener expresiones desde que se guarda en la tabla de simbolos
          val condicion2 = isParamsIguales(paramsTiposVerificados,paramsT)
          
          val mensaje2 = "Procedimiento '%s' con argumentos incorrectos en %s esperaba %s, recibio %s".
            format(nombre, pos, if (paramsT.size>0) paramsT.mkString(",") else "-ninguno-", paramsTiposVerificados.mkString(","))
          verifica(condicion2, mensaje2)
          procT.tipo
          
          
        
        case LiteralInteger(pos: Pos, valor: String) =>
          try {
            valor.toInt  
          } catch {
            case ex: NumberFormatException =>
              verifica(false,
                "Número '%s' demasiado grande en %s".format(valor, pos))
          }
          TInteger
        
        case LiteralReal(pos: Pos, valor: String) =>
          try {
            valor.toDouble  
          } catch {
            case ex: NumberFormatException =>
              verifica(false,
                "Número '%s' demasiado grande en %s".format(valor, pos))
          }
          TReal
        
        case exp: LiteralString =>
          TString
          
        case exp: LiteralBoolean =>
          TBoolean
        
        case exp: LiteralVoid =>
          TVoid
          
        case Identificador(nombre, pos) =>
          val condicion = tabla.contains(nombre)
          val mensaje = "No se encuentra '%s' en %s".
            format(nombre, pos)
          verifica(condicion, mensaje)
          tabla(nombre) match{
            case VarEntry(tipo) => tipo
            case VarLocalEntry(tipo) => tipo
            case ParamEntry(tipo) => tipo
            case e : ProcEntry =>e.tipo
          }
          
      }
      
    }// analizaExpresion
   
    private def verifica(condicion: Boolean, mensaje: String):Int = {
      if (!condicion) throw new SemanticException(mensaje)
      0
    }
    
    private def verificaReturn(enuns: List[Enunciado], tabla: TablaDeSimbolos,
      exitFlag: Boolean, tipoEsperado: Tipo):Int= {
      var numReturn = 0;// los returns se cuentan apartir de 1
      var numExits = 0;
      
      for( enun <- enuns) {
        numExits+=verificaExit(enun,exitFlag,numExits)
        if(enun.isInstanceOf[EnunReturn]) {
          val en = enun.asInstanceOf[EnunReturn]
          val tipo = analizaExpresion(en.expr, tabla)
          val condicion = tipo == tipoEsperado
          var mensaje = "Enunciado '%s' debe regresar %s y encontro %s en %s".
            format(en.nombreClase, tipoEsperado, tipo, en.posReturn)
          verifica(condicion, mensaje)
          /*mensaje = "Enunciado '%s' inalcanzable en %s, numero %s".
            format(en.nombreClase, en.posReturn, numReturn)
          verifica(!(numReturn>0), mensaje)*/
          numReturn = numReturn + 1  
        } else {
          numReturn += analizaEnunciado(enun, tabla, exitFlag, tipoEsperado)
        }
      }
      numReturn //Regresar el total de returns
      
    }
    
    private def verificaReturnAusente(id:Identificador,numReturn:Int,tipoEsperado:Tipo){
      val condicion = !(numReturn == 0 && !(tipoEsperado==TVoid))
      val mensaje = "'%s' debe regresar %s y no encontro return en %s".
                    format(id.nombre, tipoEsperado, id.pos)
      verifica(condicion, mensaje)
    }
    
    private def verificaExit(en:Enunciado, banderaExit:Boolean, numExits: Int):Int={
     if(banderaExit && en.isInstanceOf[EnunExit]) { 
              val mensaje = "Enunciado '%s' inalcanzable en %s".
                format(en.nombreClase, en.asInstanceOf[EnunExit].posExit)
              verifica(!(numExits+1>1), mensaje)
              numExits+1
     } else{
       0
     }
    }
    
    private def getLiteral(tipo: Tipo){
      tipo match{
        case TInteger => LiteralInteger
        case TBoolean => LiteralBoolean
        case TReal => LiteralReal
        case TString => LiteralString
        case TVoid => LiteralVoid
      }
    }
    
    def verificaTipos(exp: Expresion, lstTipos: List[Tipo], fun: (Expresion, Tipo) => Unit): Tipo = {
      for( t <- lstTipos) {
        try {
          fun(exp, t)
          return t
        } catch { case e: SemanticException => "no hago nada" }
      }
      val mensaje = "Operador '%s' espera dos operandos del mismo tipo: %s en %s%n".
        format(exp.nombreClase, lstTipos.mkString(" ó "), exp.posOper)
      verifica(false, mensaje)// siempre lanza excepcion
      TVoid// esto nunca ocurre
    }
    
   private def isParamsIguales(paramsTiposVerificados:List[Tipo],paramsT:List[Tipo]): Boolean = {
      if ( (paramsTiposVerificados.size - paramsT.size) != 0 )
        return false
      val lst = paramsTiposVerificados.zip(paramsT)
      return lst.forall(x=>x._1==x._2)
   }
  }  
}
