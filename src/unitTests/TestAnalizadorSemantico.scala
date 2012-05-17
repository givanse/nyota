/*
Pruebas de Unidad del Analizador Semantico del Lenguaje Nyota
Gastón Silva Echegaray 1162324
Rafael Santos Pérez 1161734
*/
package nyota.test {
  
  import org.junit.Test
  import org.junit.Assert._    
  
  class AnalizadorSemanticoTest extends ParserNyota {

   var tabla = Map[String,TableEntry](
      "i"-> VarEntry(TInteger),
      "r" -> VarEntry(TReal),
      "s" -> VarEntry(TString),
      "b" -> VarEntry(TBoolean),
      "doAnything" -> ProcEntry(TString, List(TInteger), Map("x" -> ParamEntry(TInteger))),
      "getOne" -> ProcEntry(TInteger, List(), Map()),
      "negacion" -> ProcEntry(TBoolean, List(TBoolean), Map())
    )
    
    @Test
    def testAnalizaExpresion {
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "true or false").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "true orelse false").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "(true or false) xor true").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "true = false").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "1.4 = 1.3").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "3 <> 4").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "3 < 5").get, tabla))
        
      assertEquals(TInteger, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprAditiva, "123 + 42").get, tabla))
        
      assertEquals(TInteger, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprAditiva, "13 * 42").get, tabla))
        
      assertEquals(TReal, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprAditiva, "1.23 rem 4.2").get, tabla))
        
      assertEquals(TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprUnaria, "not true").get, tabla))
        
      assertEquals(TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprUnaria, "not ((not true) or (not false))").get, tabla))
        
      assertEquals(TReal, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprUnaria, "-1.23").get, tabla))
        
      AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "getOne() > getOne()").get, tabla)
    }
    
    @Test
    def testAnalizaEnunciado {
      AnalizadorSemantico.analizaEnunciado(parseAll(enunciado, "loop end;").get, tabla, false, TVoid)
      
      AnalizadorSemantico.analizaEnunciado(parseAll(enunciado, "loop exit; end;").get, tabla, false, TVoid)
      
      AnalizadorSemantico.analizaEnunciado(parseAll(enunciado, "loop i:=1+getOne(); end;").get, tabla, false, TVoid)
      
      AnalizadorSemantico.analizaEnunciado(parseAll(enunciado, "s:=doAnything(3);").get, tabla, false, TVoid)
      
      AnalizadorSemantico.analizaEnunciado(parseAll(enunciado, "if true then else end;").get, tabla, false, TVoid)
      
      AnalizadorSemantico.analizaEnunciado(parseAll(enunciado, "if getOne() = 0 then else end;").get, tabla, false, TVoid)
    }
    
    @Test
    def testExprLogica {
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "true or negacion(false)").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "negacion(true) orelse false").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "negacion((true or negacion(false)) xor true)").get, tabla))
        
    }
    
    @Test
    def testExprRelacional {
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "negacion(true) = negacion(false)").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "14.78 = 13.0").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "3 <> 3").get, tabla))
        
      assertEquals( TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprLog, "5 < 0").get, tabla))
        
    }
    
    @Test
    def testExpresionAritmetica {
      assertEquals(TInteger, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprAditiva, "13 * (552+5)").get, tabla))
        
      assertEquals(TInteger, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprAditiva, "1/3 * 42").get, tabla))
        
      assertEquals(TInteger, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprAditiva, "1 rem 2").get, tabla))
        
    }
    
    @Test
    def testExpresionUnaria {
      assertEquals(TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprUnaria, "not true").get, tabla))
        
      assertEquals(TBoolean, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprUnaria, "not ((not true) or (not false))").get, tabla))
        
      assertEquals(TReal, 
        AnalizadorSemantico.analizaExpresion(parseAll(exprUnaria, "-1.23").get, tabla))
        
    }
    
    @Test
    def testExpresionInvocacion {
      AnalizadorSemantico.analizaExpresion(parseAll(exprInvocacion, "getOne()").get, tabla)
      
      try{
        AnalizadorSemantico.analizaExpresion(parseAll(exprInvocacion, "answer0()").get, tabla)
        fail()
      }
      catch{
        
        case e:SemanticException =>assertEquals(
            "Procedimiento 'answer0' no declarado en línea: 1, posición: 1", 
            e.getMessage)
            
      }
    }
    
    @Test
    def testProcedimiento {
      // prueba 1
      AnalizadorSemantico.analizaProcedimiento(
        parseAll(procedimiento,
          "procedure miproc(): integer; begin return 5; end;").get, tabla)
      // prueba 2
      AnalizadorSemantico.analizaProcedimiento(
        parseAll(procedimiento,
          """procedure regresaInteger():integer; begin 
              loop
                if true then return 0; end;
                if true then return 0; end;
              end;
             end;
          """).get, tabla)
    }
    
   
  }         
}
