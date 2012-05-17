package nyota {
  
  import AnalizadorSemantico.TablaDeSimbolos
  import scala.collection.mutable.Stack
  
  object GeneradorCIL {
    
    private val tiposCIL = Map[Tipo, String](TInteger -> "int32",
                                             TBoolean   -> "bool",
                                             TString -> "string",
                                             TReal -> "float64",
                                             TVoid -> "void")
                                             
    private val nombreClase = "ProgramaNyota"
    
    def genera(prog: Programa, tabla: TablaDeSimbolos) = (
      "// Codigo generado por el compilador de Nyota."
      + "%n%n.assembly 'nyota' {}%n"
      + "%n.assembly extern 'nyotalib' {}%n"
      + "%n.class public 'ProgramaNyota' extends ['mscorlib']'System'.'Object' {"
      //.field
      + "%s"
      + "%n\t.method public static void 'inicio'() {"
      + "%n\t\t.entrypoint"
      + "%n\t\t.maxstack 32"
      + "%s%s"
      + "%n\t\tret"
      + "%n\t}"
      + "%n%s"//procs
      + "%n}%n").
        format(
          // .field
          (for(v <- prog.variables) yield "%n\t.field  public static %s '%s'".
            format(tiposCIL(v.tipo), v.varID.nombre)).mkString,
          prog.variables.map(generaVariable(_, tabla)).mkString,
          prog.enunciados.map(generaEnunciado(_, tabla)).mkString,
          prog.procedimientos.map(generaProcedimiento(_, tabla)).mkString)
    
    // declara una variable
    def generaVariable(v: Variable, tabla: TablaDeSimbolos):String = {
      val expStr = v.tipo match {
        case TInteger => generaExpresion(LiteralInteger(v.varID.pos, "0"), tabla)
        case TBoolean => generaExpresion(LiteralBoolean(v.varID.pos, "false"), tabla)
        case TString => generaExpresion(LiteralString(v.varID.pos, "\"\""), tabla)
        case TReal => generaExpresion(LiteralReal(v.varID.pos, "0.0"), tabla)
      }
      val storeStr = tabla(v.varID.nombre) match {
        case te: VarEntry => "%n\t\tstsfld %s '%s'::'%s'".
                          format(tiposCIL(v.tipo),nombreClase, v.varID.nombre)
        case te: VarLocalEntry => "%n\t\tstloc '%s'".format(v.varID.nombre)
      }
      expStr + storeStr 
    }
    
    def generaProcedimiento(p: Procedimiento, tablaGlobal: TablaDeSimbolos):String = {
      val tablaLocal = tablaGlobal(p.procID.nombre).asInstanceOf[ProcEntry].tabla
      val lstParams:List[String] = for(param <- p.parametros) yield tiposCIL(param.tipo)+" "+param.paramID.nombre
      ("%n\t.method private static  hidebysig default %s %s (%s)  cil managed { "+
      "%n\t\t .maxstack 8"+
      // .locals de variables locales
      (for(v <- p.variables) yield
        ("%n\t\t.locals init ("+tiposCIL(v.tipo)+" '"+v.varID.nombre+"')").format()
      ).mkString+
      // inicializa variables locales
      p.variables.map(generaVariable(_, tablaLocal)).mkString+
      p.enunciados.map(generaEnunciado(_, tablaGlobal++tablaLocal)).mkString+
      {if (p.tipo == TVoid) "%n\t\tret ".format() else ""}+
      "%n\t}").format(tiposCIL(p.tipo), p.procID.nombre, lstParams.mkString(", "))
    }
    
    def generaEnunciadosElseIf(
      enunsElseIf: List[EnunElseIf],
      etiquetasElseIf: List[String],
      etiquetaEndIf: String,
      etiquetaElse: String,
      tabla: TablaDeSimbolos,
      result: String,
      bloqueElse: String):String = {
        if(enunsElseIf.isEmpty) {
          return result+bloqueElse
        } else if (enunsElseIf.size==1) {
          val en = enunsElseIf.head.enunElseIf
          val r ="%n%s:".format(etiquetasElseIf(0))+
          generaExpresion(en._2, tabla)+
          "%n\t\t brfalse '%s'".format(etiquetaElse)+
          en._3.map(generaEnunciado(_, tabla)).mkString+
          "%n\t\tbr '%s'".format(etiquetaEndIf)
          generaEnunciadosElseIf(
            enunsElseIf.tail, etiquetasElseIf.tail, etiquetaEndIf,
            etiquetaElse, tabla, result+r, bloqueElse)
        } else {
          val en = enunsElseIf.head.enunElseIf
          val r = "%n%s:".format(etiquetasElseIf(0))+
          generaExpresion(en._2, tabla)+
          "%n\t\t brfalse '%s'".format(etiquetasElseIf(1))+
          en._3.map(generaEnunciado(_, tabla)).mkString+
          "%n\t\tbr '%s'".format(etiquetaEndIf)
          generaEnunciadosElseIf(
            enunsElseIf.tail, etiquetasElseIf.tail, etiquetaEndIf,
            etiquetaElse, tabla, result+r, bloqueElse)
        }
    }
    
    def generaEnunciado(enu: Enunciado, tabla: TablaDeSimbolos): String = enu match {
      
      case EnunIf(enunIf::enunsElseIf, enunsElse) =>
        val etiquetaElse = generaEtiqueta()
        val etiquetaEndIf = generaEtiqueta()
        var etiquetasElseIf = for(e <- enunsElseIf) yield generaEtiqueta()
        // BLOQUE ELSE
        var bloqueElse = "%n%s: // else".format(etiquetaElse)
        if(enunsElse.isEmpty) {
          // aun asi hay que imprimir las etiquetas
          bloqueElse += "%n\t\t%s: // end if".format(etiquetaEndIf)  
        } else {
          bloqueElse += enunsElse.map(generaEnunciado(_, tabla)).mkString+
            "%n\t\t%s: // end if".format(etiquetaEndIf)
        }
        // BLOQUE IF
        val bloqueIf = generaExpresion(enunIf.enunElseIf._2, tabla)+
        "%n\t\tbrfalse '%s' // enun if".format(
          if (etiquetasElseIf.size>0) etiquetasElseIf(0) else etiquetaElse
        )+
        enunIf.enunElseIf._3.map(generaEnunciado(_, tabla)).mkString+
        "%n\t\tbr '%s'".format(etiquetaEndIf)
        // ELSE IF, este ya incluye al bloqueElse
        val bloqueElseIf= generaEnunciadosElseIf(
          enunsElseIf, etiquetasElseIf, etiquetaEndIf,
          etiquetaElse, tabla, "", bloqueElse)
        // resultado final
        bloqueIf+bloqueElseIf
        //bloqueIf+bloqueElseIf
        
      case EnunAsignacion(id, exp) =>
        generaExpresion(exp, tabla) +
        (tabla(id.nombre) match {
          case VarEntry(tipo) => "%n\t\tstsfld %s '%s'::'%s'".format(tiposCIL(tipo), nombreClase, id.nombre)
          case te: VarLocalEntry => "%n\t\tstloc '%s'".format(id.nombre)
          case te: ParamEntry => "%n\t\tstarg '%s'".format(id.nombre)
        })
      
      case EnunInvocacion(id, exprs) =>
        tabla(id.nombre) match {
          case te: ProcLibEntry =>
            val lstParams = te.lst.map(tiposCIL(_))
            exprs.map(generaExpresion(_, tabla)).mkString+
            "%n\t\tcall void class ['nyotalib']'Nyota'.'Utils'::%s(%s)".
              format(id.nombre, lstParams.mkString(","))
          case te: ProcEntry =>
            val lstParams = te.lstParams.map(tiposCIL(_))
            exprs.map(generaExpresion(_, tabla)).mkString+
            "%n\t\tcall void class %s::%s(%s)".
              format(nombreClase, id.nombre, lstParams.mkString(","))
        }
      
      case EnunLoop(enunciados) =>
        val etqIni = generaEtiqueta()
        val etqFin = generaEtiqueta()
        pilaEtiquetas.push(etqFin)
        (
        "%n"+etqIni+": // enun loop"+
        enunciados.map(generaEnunciado(_, tabla)).mkString+
        "%n\t\tbr '%s' // se cicla loop".format(etqIni)+
        "%n"+pilaEtiquetas.pop+": // end loop").format()
      
      case EnunExit(pos) =>
        "%n\t\tbr '%s' // exit".format(pilaEtiquetas.top)
      
      case EnunReturn(pos, exp) => 
        "%s %n\t\tret".format(generaExpresion(exp, tabla))
    
      case _ => "-.-'  404 Not Found"
    }
    
    def generaExpresion(exp: Expresion, tabla: TablaDeSimbolos): String = {
      
      def generaOpBin(izq: Expresion, der: Expresion, operacion: String) =
        generaExpresion(izq, tabla)+generaExpresion(der, tabla)+operacion
                 
      exp match {
        
        case Identificador(nom, pos) =>
          generaExpresion(ExpresionVariable(pos, nom), tabla)
        
        case ExpresionAnd(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tand".format())
          
        case ExpresionOr(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tor".format())
          
        case ExpresionAndAlso(pos, izq, der) =>
          val etiqueta1 = generaEtiqueta()
          val etiqueta2 = generaEtiqueta()
            "%n\t\t%s %n\t\tbrfalse '%s' %n\t\t%s %n\t\tbr.s '%s' %n \t\t'%s': ldc.i4.0 %n\t\t'%s':".
            format(
              generaExpresion(izq, tabla), etiqueta1, generaExpresion(der, tabla),
              etiqueta2, etiqueta1, etiqueta2)
        
        /*
        ldloc.0 z true 
        brtrue IL_000d
        
        ldloc.1 
        br.s IL_000e
        
        IL_000d:  ldc.i4.1 
        IL_000e:
        */
        case ExpresionOrElse(pos, izq, der) =>
        val etiqueta1 = generaEtiqueta()
        val etiqueta2 = generaEtiqueta()
          "%s %n\t\tbrtrue '%s' %s %n\t\tbr.s '%s' %n \t\t'%s': ldc.i4.1 %n\t\t'%s':".
          format(
            generaExpresion(izq, tabla), etiqueta1, generaExpresion(der, tabla),
            etiqueta2, etiqueta1, etiqueta2)     
          
        case ExpresionXor(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\txor".format())
          
        case ExpresionIgual(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tceq".format())
          
        /*ceq 
          ldc.i4.0 
          ceq*/
        case ExpresionDiferente(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tceq %n\t\tldc.i4.0 %n\t\tceq".format())
                                                                                     
        case ExpresionMenor(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tclt".format())
          
        case ExpresionMayor(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tcgt".format())
          
        /*cgt 
          ldc.i4.0 
          ceq*/
        case ExpresionMenorIgual(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tcgt %n\t\tldc.i4.0 %n\t\tceq".format())
          
        /*clt 
          ldc.i4.0 
          ceq*/
        case ExpresionMayorIgual(pos, izq, der) =>
          generaOpBin(izq, der, "%n\t\tclt %n\t\tldc.i4.0 %n\t\tceq".format())
          
        case ExpresionSuma(posCociente, izq, der)=>
          generaOpBin(izq, der, "%n\t\tadd.ovf".format())
          
        case ExpresionResta(posCociente, izq, der)=>
          generaOpBin(izq, der, "%n\t\tsub.ovf".format())
         
        case ExpresionMultiplicacion(posCociente, izq, der)=>
          generaOpBin(izq, der, "%n\t\tmul.ovf".format())
         
        case ExpresionDivision(posCociente, izq, der)=>
          generaOpBin(izq, der, "%n\t\tdiv".format())
          
        case ExpresionCociente(posCociente, izq, der) =>
          generaOpBin(izq, der, "%n\t\tdiv".format())
         
        case ExpresionResiduo(posResiduo, izq, der) =>
          generaOpBin(izq, der, "%n\t\trem".format())
          
        // UNARIAS
        case ExpresionNot(posNot, exp) =>
          "%s %n\t\tldc.i4.1 %n\t\txor".format(generaExpresion(exp, tabla))
        
        case ExpresionNegacion(_, exp) =>
          "%s %n\t\tneg".format(generaExpresion(exp, tabla))
        
        case ExpresionInvocacion(id, params)=>
          /*val procEntry = tabla(id.nombre).asInstanceOf[ProcEntry]
          val lstParams = procEntry.lstParams.map(tiposCIL(_))
          params.map(generaExpresion(_, tabla)).mkString+
          "%n\t\tcall %s class %s::%s(%s)".
            format(tiposCIL(procEntry.tipo), nombreClase, id.nombre,lstParams.mkString(","))
           */
          tabla(id.nombre) match {
            case te: ProcLibEntry =>
              val lstParams = te.lst.map(tiposCIL(_))
              "%s%n\t\tcall %s class ['nyotalib']'Nyota'.'Utils'::%s(%s)".
                format(
                  params.map(generaExpresion(_, tabla)).mkString,
                  tiposCIL(te.t), id.nombre, lstParams.mkString(", "))
            case te: ProcEntry =>
              val lstParams = te.lstParams.map(tiposCIL(_))
              params.map(generaExpresion(_, tabla)).mkString+
              "%n\t\tcall %s class %s::%s(%s)".
                format(tiposCIL(te.tipo), nombreClase, id.nombre, lstParams.mkString(","))
          }
          
        //invocacion variable
        case ExpresionVariable(_, nombre) =>
          tabla(nombre) match {
            case v: VarEntry =>
                 "%n\t\tldsfld %s '%s'::'%s'".format(tiposCIL(v.tipo), nombreClase, nombre)
            case v:VarLocalEntry =>
                 "%n\t\tldloc '%s'".format(nombre)
            case v:ParamEntry =>
                 "%n\t\tldarg '%s'".format(nombre)
          }
          
        
        case LiteralString(_, valor) =>
          "%n\t\tldstr %s".format(valor)
          
        case LiteralReal(_,valor) =>
          val num = valor.toDouble
          "%n\t\tldc.r8 %s".format(num)
          
        case LiteralInteger(_, valor) =>           
          val num = valor.toInt
          if (num <= 8)
            "%n\t\tldc.i4.%s".format(num)
          else if (num <= 127)
            "%n\t\tldc.i4.s %s".format(num)
          else
            "%n\t\tldc.i4 %s".format(num)
        
        case LiteralBoolean(_, valor) =>
          "%n\t\tldc.i4.%s".format(if (valor=="true") 1 else 0) 
          
        case e: LiteralVoid => ""
      }
      
    }
     
    private var contadorEtiquetas = 0
    
    private def generaEtiqueta() = {
      contadorEtiquetas += 1
      "$%06d".format(contadorEtiquetas)
    }
    
    var pilaEtiquetas = new Stack[String]
    
    var pilaEtiquetaIf = new Stack[String]
 
    // Para usarse desde las pruebas de unidad. 
    def resetEtiquetas() {
      contadorEtiquetas = 0
    }
  }
}
