package nyota.test {
  
  import org.junit.{Before, Test}
  import org.junit.Assert._
  import scala.collection.immutable.ListMap
  
  class GeneradorCILTest extends ParserNyota {
    
        
    var tabla = Map[String,TableEntry](
      "i"-> VarEntry(TInteger),
      "r" -> VarEntry(TReal),
      "foo" -> VarEntry(TReal),
      "s" -> VarEntry(TString),
      "b" -> VarEntry(TBoolean),
      "doAnything" -> ProcEntry(TString, List(TInteger), Map("x" -> ParamEntry(TInteger))),
      "getOne" -> ProcEntry(TInteger, List(), Map()),
      "negacion" -> ProcEntry(TBoolean, List(TBoolean), Map()),
      "proc" -> ProcEntry(TVoid, List(), Map())
    )
    
    @Before
    def setUp {
      GeneradorCIL.resetEtiquetas()
    }
    
    @Test
    def testLiterales {
      assertEquals(
        "%n\t\tldstr %s".format("\"hola\""),
        GeneradorCIL.generaExpresion(parseAll(literal, "\"hola\"").get,tabla)
      )
      assertEquals(
        "%n\t\tldstr %s".format("\"hfssf3223\""),
        GeneradorCIL.generaExpresion(parseAll(literal, "\"hfssf3223\"").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4.s %s".format(123),
        GeneradorCIL.generaExpresion(parseAll(literal, "123").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4 %s".format(1234235252),
        GeneradorCIL.generaExpresion(parseAll(literal, "1234235252").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4.0".format(),
        GeneradorCIL.generaExpresion(parseAll(literal, "false").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s".format("4.0E-45"),
        GeneradorCIL.generaExpresion(parseAll(literal, "4.0e-45").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s".format("4.0E45"),
        GeneradorCIL.generaExpresion(parseAll(literal, "4.0e+45").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s".format("4.0E45"),
        GeneradorCIL.generaExpresion(parseAll(literal, "4.0e45").get,tabla)
      )
    }
    
    @Test
    def testIdentificador {// ExpresionVariable
      assertEquals(
        "%n\t\tldsfld bool 'ProgramaNyota'::'b'".format(),
        GeneradorCIL.generaExpresion(parseAll(identificador, "b").get, tabla)
      )
      assertEquals(
        "%n\t\tldsfld int32 'ProgramaNyota'::'i'".format(),
        GeneradorCIL.generaExpresion(parseAll(identificador, "i").get, tabla)
      )
    }
   
    @Test
    def testExpresionInvocacion {
      // primera prueba
      assertEquals(
        "%n\t\tldc.i4.5".format()+
        "%n\t\tcall string class ProgramaNyota::doAnything(int32)".format(),
        GeneradorCIL.generaExpresion(
          parseAll(exprInvocacion, "doAnything(5)").get, tabla)
      )
      // segunda prueba
      assertEquals(
        "%n\t\tldc.i4.0".format()+
        "%n\t\tcall bool class ProgramaNyota::negacion(bool)".format(),
        GeneradorCIL.generaExpresion(
          parseAll(exprInvocacion, "negacion(false)").get, tabla)
      )
    }
    
    @Test
    def testExpresionNegacion {
      assertEquals(
        "%n\t\tldc.i4.%s %n\t\tneg".format(5),
        GeneradorCIL.generaExpresion(parseAll(exprUnaria, "-5").get, tabla)
      )
      assertEquals(
        "%n\t\tldc.i4 %s %n\t\tneg".format(121323),
        GeneradorCIL.generaExpresion(parseAll(exprUnaria, "-121323").get, tabla)
      )
      assertEquals(
        "%n\t\tldc.i4.s %s %n\t\tneg".format(100),
        GeneradorCIL.generaExpresion(parseAll(exprUnaria, "-100").get, tabla)
      )
    }
    
    @Test
    def testExpresionNot{
      assertEquals(
        "%n\t\tldc.i4.1 %n\t\tldc.i4.1 %n\t\txor".format(),
        GeneradorCIL.generaExpresion(parseAll(exprUnaria,"not true").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4.1 %n\t\tldc.i4.1 %n\t\txor %n\t\tldc.i4.1 %n\t\txor %n\t\tldc.i4.1 %n\t\txor".format(),
        GeneradorCIL.generaExpresion(parseAll(exprUnaria,"not not not true").get,tabla)
      )
      
    }
    
    @Test
    def testExpresionResiduo {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.%s%n\t\trem".format(5, 2),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"5 rem 2").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4.s %s%n\t\tldc.i4.s %s%n\t\trem".format(100, 100),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"100 rem 100").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4 %s%n\t\tldc.i4 %s%n\t\trem".format(300, 300),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"300 rem 300").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\trem".format(5.5, 2.5),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"5.5 rem 2.5").get,tabla)
      )
    }
    
    @Test
    def testExpresionCociente {
      assertEquals(
        "%n\t\tldc.i4.s %s%n\t\tldc.i4.s %s%n\t\tdiv".format(15, 9),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"15 div 9").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tdiv".format(15.5, 9.5),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"15.5 div 9.5").get,tabla)
      )
    }
    
    @Test
    def testExpresionDivision {
      assertEquals(
        "%n\t\tldc.i4 %s%n\t\tldc.i4 %s%n\t\tdiv".format(400, 200),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"400 / 200").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tdiv".format(40.5, 2.2),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"40.5 / 2.2").get,tabla)
      )
    }
    
    @Test
    def testExpresionMultiplicacion{
      assertEquals(
        "%n\t\tldc.i4.s %s%n\t\tldc.i4.%s%n\t\tmul.ovf".format(40, 2),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"40 * 2").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tmul.ovf".format(2.2, 43.4),
        GeneradorCIL.generaExpresion(parseAll(exprMultiplicativa,"2.2 * 43.4").get,tabla)
      )
    }
    
    @Test
    def testExpresionResta {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.s %s%n\t\tsub.ovf".format(4, 80),
        GeneradorCIL.generaExpresion(parseAll(exprAditiva,"4 - 80").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tsub.ovf".format(4.5, 8.5),
        GeneradorCIL.generaExpresion(parseAll(exprAditiva,"4.5 - 8.5").get,tabla)
      )
    }
    
    @Test
    def testExpresionSuma {
      assertEquals(
        "%n\t\tldc.i4.s %s%n\t\tldc.i4 %s%n\t\tadd.ovf".format(123, 5678),
        GeneradorCIL.generaExpresion(parseAll(exprAditiva, "123 + 5678").get, tabla))
      // segunda
      assertEquals(
        "%n\t\tldc.i4 %s%n\t\tldsfld int32 'ProgramaNyota'::'%s'%n\t\tadd.ovf".format(500, "i"),
        GeneradorCIL.generaExpresion(parseAll(exprAditiva,"500 + i").get,tabla)
      )      
    }
    
    @Test
    def testExpresionMayorIgual {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.%s%n\t\tclt %n\t\tldc.i4.0 %n\t\tceq".
          format(5, 6),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"5 >= 6").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tclt %n\t\tldc.i4.0 %n\t\tceq".
          format(5.5, 6.6),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"5.5 >= 6.6").get,tabla)
      )
    }
    
    @Test
    def testExpresionMenorIgual {
      assertEquals(
        "%n\t\tldsfld int32 'ProgramaNyota'::'%s'%n\t\tldsfld int32 'ProgramaNyota'::'%s'%n\t\tcgt %n\t\tldc.i4.0 %n\t\tceq".
          format("i","i"),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"i <= i").get,tabla)
      )
      assertEquals(
        "%n\t\tldsfld float64 'ProgramaNyota'::'%s'%n\t\tldsfld float64 'ProgramaNyota'::'%s'%n\t\tcgt %n\t\tldc.i4.0 %n\t\tceq".
          format("r","r"),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"r <= r").get,tabla)
      )
    }
    
    @Test
    def testExpresionMayor {
      assertEquals(
        "%n\t\tldc.i4 %s%n\t\tldc.i4 %s%n\t\tcgt".format(300, 400),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"300 > 400").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tcgt".format(3.3, 4.4),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"3.3 > 4.4").get,tabla)
      )
    }
    
    @Test
    def testExpresionMenor {
      assertEquals(
        "%n\t\tldc.i4 %s%n\t\tldc.i4 %s%n\t\tclt".format(666, 999),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"666 < 999").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tclt".format(6.6, 9.9),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"6.6 < 9.9").get,tabla)
      )
    }
    
    @Test
    def testExpresionDiferente {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.s %s%n\t\tceq %n\t\tldc.i4.0 %n\t\tceq".format(1, 100),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"1 <> 100").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tceq %n\t\tldc.i4.0 %n\t\tceq".format(1.1, 1.1),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"1.1 <> 1.1").get,tabla)
      )
    }
    
    @Test
    def testExpresionIgual {
      assertEquals(
        "%n\t\tldc.i4.s %s%n\t\tldc.i4.%s%n\t\tceq".format(42, 0),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"42 = 0").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.r8 %s%n\t\tldc.r8 %s%n\t\tceq".format(4.2, 4.0),
        GeneradorCIL.generaExpresion(parseAll(exprRel,"4.2 = 4.0").get,tabla)
      )
    }
    
    @Test
    def testExpresionXor {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.%s%n\t\txor".format(1, 0),
        GeneradorCIL.generaExpresion(parseAll(exprLog,"true xor false").get,tabla)
      )
    }
    
    @Test
    def testExpresionOrElse {
      val etiqueta1="$000001"
      val etiqueta2="$000002"
      assertEquals(
        "%s %n\t\tbrtrue '%s' %s %n\t\tbr.s '%s' %n \t\t'%s': ldc.i4.1 %n\t\t'%s':".
         format("%n\t\tldc.i4.%s".format(0), etiqueta1, "%n\t\tldc.i4.%s".format(0),
                etiqueta2, etiqueta1, etiqueta2),
        GeneradorCIL.generaExpresion(parseAll(exprLog,"false orelse false").get,tabla)
      )
    }
    
    @Test
    def testExpresionAndAlso {
      val etiqueta1="$000001"
      val etiqueta2="$000002"
      assertEquals(
        "%n\t\t%s %n\t\tbrfalse '%s' %n\t\t%s %n\t\tbr.s '%s' %n \t\t'%s': ldc.i4.0 %n\t\t'%s':".
         format("%n\t\tldc.i4.%s".format(1), etiqueta1, "%n\t\tldc.i4.%s".format(1),
                etiqueta2, etiqueta1, etiqueta2),
        GeneradorCIL.generaExpresion(parseAll(exprLog, "true andalso true").get,tabla)
      )
    }
    
    @Test
    def testExpresionOr {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.%s%n\t\tor".format(1, 0),
        GeneradorCIL.generaExpresion(parseAll(exprLog,"true or false").get,tabla)
      )
    }
    
    @Test
    def testExpresionAnd {
      assertEquals(
        "%n\t\tldc.i4.%s%n\t\tldc.i4.%s%n\t\tand".format(0, 1),
        GeneradorCIL.generaExpresion(parseAll(exprLog,"false and true").get,tabla)
      )
    }
    
    @Test
    def testEnunReturn {
      assertEquals(
        "%n\t\tldc.i4.%s %n\t\tret".format(0),
        GeneradorCIL.generaEnunciado(parseAll(enunReturn,"return 0;").get,tabla)
      )
      assertEquals(
        "%n\t\tldc.i4.%s %n\t\tret".format(0),
        GeneradorCIL.generaEnunciado(parseAll(enunReturn,"return false;").get,tabla)
      )
    }
    
    @Test
    def testEnunExit {
      GeneradorCIL.pilaEtiquetas.push("$000001")
      assertEquals(
        "%n\t\tbr '%s' // exit".format("$000001"),
        GeneradorCIL.generaEnunciado(parseAll(enunExit,"exit ;").get,tabla)
      )
    }
    
    @Test
    def testEnunLoop {
      val etiquetaIni = "$000001"
      val etiquetaFin = "$000002"
      GeneradorCIL.pilaEtiquetas.push(etiquetaFin)
      assertEquals(
        "%n%s: // enun loop%n\t\tbr '%s' // se cicla loop%n%s: // end loop".
          format(etiquetaIni,etiquetaIni,etiquetaFin),
        GeneradorCIL.generaEnunciado(parseAll(enunLoop,"loop end;").get,tabla)
      )
    }
    
    @Test
    def testEnunInvocacion {
      assertEquals(
        "%n\t\tcall void class ProgramaNyota::getOne()".format(),
        GeneradorCIL.generaEnunciado(parseAll(enunInvocacion,"getOne();").get,tabla)
      )

      assertEquals(
        "%n\t\tldc.i4.s %s%n\t\tcall void class ProgramaNyota::doAnything(int32)".format(67),
        GeneradorCIL.generaEnunciado(parseAll(enunInvocacion,"doAnything(67);").get, tabla)
      )
    }
    
    @Test
    def testEnunAsignacion {
      assertEquals(
        "%n\t\tldc.i4.4%n\t\tstsfld %s 'ProgramaNyota'::'%s'".format("int32","i"),
        GeneradorCIL.generaEnunciado(parseAll(enunAsignacion,"i:=4;").get,tabla)
      )
    }
    
    @Test
    def testEnunIf {
      // prueba 1
      val etiquetaElse = "$000001"
      val etiquetaEndIf = "$000002"
      assertEquals(
        ("%n\t\tldc.i4.%s"+// expr
        "%n\t\tbrfalse '%s' // enun if%n\t\tbr '%s'"+// bloque if
        ""+// bloque else if
        "%n%s: // else%n\t\t%s: // end if").// bloque else
          format(1, etiquetaElse, etiquetaEndIf, etiquetaElse, etiquetaEndIf),
        GeneradorCIL.generaEnunciado(parseAll(enunIf,"if true then else end;").get,tabla)
      )
    }
    
    @Test
    def testProcedimiento {
      // prueba 1
      assertEquals(
        ("%n\t.method private static  hidebysig default %s %s (%s)  cil managed { "+
        "%n\t\t .maxstack 8"+
        "%n\t\tret "+
        "%n\t}")
        .format("void","proc",""),
        GeneradorCIL.generaProcedimiento(parseAll(procedimiento,"procedure proc();begin end;").get,tabla)
      )
      // prueba 2
      val etiquetaIni = "$000001"
      val etiquetaFin = "$000002"
      val etiquetaElse1 = "$000003"
      val etiquetaEndIf1 = "$000004"
      val etiquetaElse2 = "$000005"
      val etiquetaEndIf2 = "$000006"
      val enunsIf = "%n\t\tldc.i4.0 %n\t\tret".format()
      val enunsLoop = (
        "%n\t\tldc.i4.%s"+// expr
        "%n\t\tbrfalse '%s' // enun if%s%n\t\tbr '%s'"+// bloque if
        ""+// bloque else if
        "%n%s: // else%n\t\t%s: // end if").// bloque else
          format(1, etiquetaElse1, enunsIf, etiquetaEndIf1, etiquetaElse1, etiquetaEndIf1) + // primer if
      (
        "%n\t\tldc.i4.%s"+// expr
        "%n\t\tbrfalse '%s' // enun if%s%n\t\tbr '%s'"+// bloque if
        ""+// bloque else if
        "%n%s: // else%n\t\t%s: // end if").// bloque else
          format(1, etiquetaElse2, enunsIf, etiquetaEndIf2, etiquetaElse2, etiquetaEndIf2) // segundo if
          
      val loop = "%n%s: // enun loop%s%n\t\tbr '%s' // se cicla loop%n%s: // end loop".
                    format(etiquetaIni, enunsLoop, etiquetaIni, etiquetaFin)//loop end;
      assertEquals(
        ("%n\t.method private static  hidebysig default %s %s (%s)  cil managed { "+
        "%n\t\t .maxstack 8"+
        "%s"+// end loop
        "%n\t}")
        .format("int32", "getOne", "", loop),
        GeneradorCIL.generaProcedimiento(parseAll(procedimiento,
          """procedure getOne():integer; begin 
              loop
                if true then return 0; end;
                if true then return 0; end;
              end;
             end;
          """).get,tabla)
      )
      
    }
    
    @Test
    def testVariable {
      val variables: List[Variable] = parseAll(declVars,"var foo: real;").get
      assertEquals( 
        "%n\t\tldc.r8 %s%n\t\tstsfld %s '%s'::'%s'".
          format(0.0, "float64", "ProgramaNyota", "foo"), generaVariables(variables, ""))
    }
    
    // auxiliar para generar variables porque junit es una nena
    def generaVariables(variables: List[Variable], r: String): String = {
      //(for(v <- variables) yield "VAR").mkString
      if(variables.isEmpty) {
        r
      } else {
        val v = GeneradorCIL.generaVariable(variables.head, tabla)
        generaVariables(variables.tail, r+v)
      }
    }
    
    @Test
    def testPrograma{
      val prog =("// Codigo generado por el compilador de Nyota."
      + "%n%n.assembly 'nyota' {}%n"
      + "%n.assembly extern 'nyotalib' {}%n"
      + "%n.class public 'ProgramaNyota' extends ['mscorlib']'System'.'Object' {"
      + "%n\t.method public static void 'inicio'() {"
      + "%n\t\t.entrypoint"
      + "%n\t\t.maxstack 32"
      + "%s%s"
      + "%n\t\tret"
      + "%n\t}"
      + "%n%s"//procs
      + "%n}%n").format("","","")
      
      assertEquals(
        prog,
        GeneradorCIL.genera(
          parseAll( programa, "program end.").get,tabla
        )
      )
    }
    
  }         
}
