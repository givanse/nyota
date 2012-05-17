/*
Pruebas de Unidad del AST del Lenguaje Nyota
Gastón Silva Echegaray 1162324
Rafael Santos Pérez 1161734
*/
package nyota.test {
  
  import org.junit.Test
  import org.junit.Assert._

  class TestASTNyota extends ParserNyota {
    
    @Test
    def testliteralInteger {
      assertEquals( LiteralInteger(Pos(1,1),"2147483647"),
                    parseAll(literalInteger, "2147483647").get)
      assertEquals( LiteralInteger(Pos(1,1),"0"),
                    parseAll(literalInteger, "0").get )
    }
    
    @Test
    def testLiteralBoolean {
      assertEquals(LiteralBoolean(Pos(1,1),"true"),
                   parseAll(literalBoolean, "true").get)
      assertEquals(LiteralBoolean(Pos(1,1),"false"),
                   parseAll(literalBoolean, "false").get)
    }
    
    @Test
    def testLiteralReal {
      
      assertEquals(LiteralReal(Pos(1,1), "0.0"),
                   parseAll(literalReal, "0.0").get)
      assertEquals(LiteralReal(Pos(1,1), "1.0e10"),
                   parseAll(literalReal, "1.0e10").get)
      assertEquals(LiteralReal(Pos(1,1), "1.0e-10"),
                   parseAll(literalReal, "1.0e-10").get)
      assertEquals(LiteralReal(Pos(1,1), "1.0e+10"),
                   parseAll(literalReal, "1.0e+10").get)
      assertEquals(LiteralReal(Pos(1,1), "1.0E10"),
                   parseAll(literalReal, "1.0E10").get)
      assertEquals(LiteralReal(Pos(1,1), "1.0E-10"),
                   parseAll(literalReal, "1.0E-10").get)
      assertEquals(LiteralReal(Pos(1,1), "1.0E+10"),
                   parseAll(literalReal, "1.0E+10").get)                   
                   
    }
    
    @Test
    def testLiteralString{
     assertEquals(LiteralString(Pos(1,1),"\"1.0E+10.0\""), 
                  parseAll(literalString, "\"1.0E+10.0\"").get)
     assertEquals(LiteralString(Pos(1,1),"\"shlalala bla haha ahaha\""), 
                  parseAll(literalString, "\"shlalala bla haha ahaha\"").get)
     assertEquals(LiteralString(Pos(1,1),"\"\\\" \\\\  \""), 
                  parseAll(literalString, "\"\\\" \\\\  \"").get)
     assertEquals(LiteralString(Pos(1,1),"\"\""), 
                  parseAll(literalString, "\"\"").get)
     assertEquals(LiteralString(Pos(1,1),"\" \"  \""), 
                  parseAll(literalString, "\" \"  \"").get)                  
    }
    
    @Test
    def testIdentificador{
      assertEquals(Identificador("aVALOR",Pos(1,1)),
                   parseAll(identificador, "aVALOR").get)
      assertEquals(Identificador("aVALOR_BLABLA",Pos(1,1)),
                   parseAll(identificador, "aVALOR_BLABLA").get)
      assertEquals(Identificador("a_B__c_d",Pos(1,1)),
                   parseAll(identificador, "a_B__c_d").get)
      assertEquals(Identificador("A_valor",Pos(1,1)),
                   parseAll(identificador, "A_valor").get)
      assertEquals(Identificador("aVALOR50",Pos(1,1)),
                   parseAll(identificador, "aVALOR50").get)
      assertEquals(Identificador("aVALOR_BLABLA50",Pos(1,1)),
                   parseAll(identificador, "aVALOR_BLABLA50").get)
      assertEquals(Identificador("a1_B2_c3_d4",Pos(1,1)),
                   parseAll(identificador, "a1_B2_c3_d4").get)
      assertEquals(Identificador("A_70valor",Pos(1,1)),
                   parseAll(identificador, "A_70valor").get)                   
    }
    
    @Test
    def testTipo{
      assertEquals(TBoolean, parseAll(tipo, "boolean").get)
      assertEquals(TString, parseAll(tipo, "string").get)
      assertEquals(TInteger, parseAll(tipo, "integer").get)
      assertEquals(TReal, parseAll(tipo, "real").get)
    }
    
    @Test
    def testOpLogico{
      assertEquals(("andalso",Pos(1,1)), parseAll(opLogico,"andalso").get)
      assertEquals(("and",Pos(1,1)), parseAll(opLogico,"and").get)
      assertEquals(("orelse",Pos(1,1)), parseAll(opLogico,"orelse").get)
      assertEquals(("or",Pos(1,1)), parseAll(opLogico,"or").get)
      assertEquals(("xor",Pos(1,1)), parseAll(opLogico,"xor").get)
    }
    
    @Test
    def testOpRelacional{
      assertEquals(("<>",Pos(1,1)), parseAll(opRelacional,"<>").get)
      assertEquals(("<=",Pos(1,1)), parseAll(opRelacional,"<=").get)
      assertEquals((">=",Pos(1,1)), parseAll(opRelacional,">=").get)
      assertEquals(("<",Pos(1,1)), parseAll(opRelacional,"<").get)
      assertEquals((">",Pos(1,1)), parseAll(opRelacional,">").get)
      assertEquals(("=",Pos(1,1)), parseAll(opRelacional,"=").get)
    }
    
    @Test
    def testOpAditivo{
      assertEquals(("+",Pos(1,1)), parseAll(opAditivo,"+").get)
      assertEquals(("-",Pos(1,1)), parseAll(opAditivo,"-").get)
    }    
    
    @Test
    def testOpMultiplicativo{
      assertEquals(("*",Pos(1,1)), parseAll(opMultiplicativo,"*").get)
      assertEquals(("/",Pos(1,1)), parseAll(opMultiplicativo,"/").get)
      assertEquals(("div",Pos(1,1)), parseAll(opMultiplicativo,"div").get)
      assertEquals(("rem",Pos(1,1)), parseAll(opMultiplicativo,"rem").get)
    }
    
    @Test
    def testDeclVar{
      assertEquals(
        List(Variable(Identificador("a",Pos(1,1)), TInteger)),
                   parseAll(declVar, "a: integer;").get)
      assertEquals(
        List(Variable(Identificador("a",Pos(1,1)),TBoolean)),
                   parseAll(declVar, "a: boolean;").get)
      assertEquals(
        List(
          Variable(Identificador("a",Pos(1,1)), TString),
          Variable(Identificador("b",Pos(1,4)), TString),
          Variable(Identificador("c",Pos(1,7)), TString)),
                   parseAll(declVar, "a, b, c: string;").get)
      assertEquals(
        List(
          Variable(Identificador("b_chido",Pos(1,1)), TReal),
          Variable(Identificador("x__X",Pos(1,10)), TReal)),
                   parseAll(declVar, "b_chido, x__X: real;").get)                   
    }
    
    @Test
    def testParametro{
      assertEquals(List(Parametro(Identificador("a",Pos(1,1)),TInteger)),
                   parseAll(parametro, "a: integer").get)
      assertEquals(List(Parametro(Identificador("a",Pos(1,1)),TInteger),
                        Parametro(Identificador("b",Pos(1,13)),TReal),
                        Parametro(Identificador("c",Pos(1,22)),TString),
                        Parametro(Identificador("d",Pos(1,33)),TBoolean)),
                   parseAll(parametro, "a: integer, b: real, c: string, d: boolean").get)         
    }
    
    @Test
    def testExprLog{
      assertEquals(ExpresionNot(Pos(1,1),Identificador("something",Pos(1,5))),
                   parseAll(exprLog,"not something").get)
      assertEquals(ExpresionIgual(Pos(1,5),
                                  ExpresionSuma(Pos(1,2),
                                                LiteralInteger(Pos(1,1),"1"),
                                                LiteralInteger(Pos(1,3),"1")),
                                  LiteralInteger(Pos(1,7),"2")),
                   parseAll(exprLog,"1+1 = 2").get)  
      
      assertEquals(ExpresionAndAlso(Pos(1,13),
                                     ExpresionMenorIgual(Pos(1,6),
                                                         ExpresionMultiplicacion(Pos(1,3),
                                                                                 LiteralInteger(Pos(1,2),"5"),
                                                                                 LiteralInteger(Pos(1,4),"9")),
                                                         LiteralInteger(Pos(1,9),"40")),
                                     ExpresionMayor(Pos(1,29),
                                                    ExpresionSuma(Pos(1,24),
                                                                  LiteralInteger(Pos(1,23),"3"),
                                                                  ExpresionMultiplicacion(Pos(1,26),
                                                                                          LiteralInteger(Pos(1,25),"5"),
                                                                                          LiteralInteger(Pos(1,27),"8"))),
                                                    LiteralInteger(Pos(1,30),"1"))),
                   parseAll(exprLog, "(5*9 <= 40) andalso ((3+5*8)>1)").get)                       
    }
    
    @Test
    def testExprRel{
      assertEquals(LiteralString(Pos(1,1),"\"una literalString\""),
                  parseAll(exprRel,"\"una literalString\"").get)
      assertEquals(ExpresionDiferente(Pos(1,7),
                                      Identificador("b_bad", Pos(1,1)),
                                      Identificador("b_cool", Pos(1,10))),
                   parseAll(exprRel, "b_bad <> b_cool").get)
      assertEquals(ExpresionMayorIgual(Pos(1,6),
                                       ExpresionDivision(Pos(1,3),
                                                         LiteralInteger(Pos(1,1),"12"),
                                                         LiteralInteger(Pos(1,4),"6")),
                                       ExpresionResiduo(Pos(1,11),
                                                        LiteralInteger(Pos(1,9),"9"),
                                                        LiteralInteger(Pos(1,15),"3"))),
                   parseAll(exprRel,"12/6 >= 9 rem 3").get)
                        
    }
                 
    @Test
    def testExprAditiva{
      assertEquals(ExpresionSuma(Pos(1,3),
                                 LiteralInteger(Pos(1,1),"9"),
                                 Identificador("mi_id",Pos(1,5))),
                   parseAll(exprAditiva, "9 + mi_id").get)
      assertEquals(ExpresionResta(Pos(1,17),
                                  ExpresionSuma(Pos(1,14),
                                                ExpresionResta(Pos(1,10),
                                                               ExpresionSuma(Pos(1,7),
                                                                             ExpresionSuma(Pos(1,3),
                                                                                           LiteralInteger(Pos(1,1),"2"),
                                                                                           LiteralInteger(Pos(1,5),"3")),
                                                                             LiteralInteger(Pos(1,8),"4")),
                                                               LiteralInteger(Pos(1,12),"1")),
                                                LiteralInteger(Pos(1,15),"2")),
                                  LiteralInteger(Pos(1,19),"3")),
                  parseAll(exprAditiva, "2 + 3 +4 - 1 +2 - 3").get)                                                              
    }
    
    @Test
    def testExprMultiplicativa{
     assertEquals(ExpresionMultiplicacion(Pos(1,4),
                                          Identificador("b", Pos(1,2)),
                                          Identificador("a", Pos(1,6))),
                  parseAll(exprMultiplicativa, "(b * a)").get)
                  
     assertEquals(ExpresionCociente(Pos(1,8),
                                    ExpresionDivision(Pos(1,4),
                                                      LiteralInteger(Pos(1,2),"12"),
                                                      LiteralInteger(Pos(1,5),"6")),
                                    ExpresionResiduo(Pos(1,15),
                                                      LiteralInteger(Pos(1,13),"9"),
                                                      LiteralInteger(Pos(1,19),"3"))),
                  parseAll(exprMultiplicativa, "(12/6) div (9 rem 3)").get)
    }
    
    @Test
    def testExprUnaria{
      assertEquals(ExpresionNot(Pos(1,1), LiteralReal(Pos(1,5),"2.3e5")),
                   parseAll(exprUnaria, "not 2.3e5").get)
                   
      assertEquals(ExpresionNot(Pos(1,1),
                                ExpresionNot(Pos(1,5),
                                             ExpresionNegacion(Pos(1,9),
                                                               ExpresionNot(Pos(1,11),
                                                                            Identificador("posible",Pos(1,15)))))),
                   parseAll(exprUnaria, "not not - not posible").get)                                                                            
                                   
    }
    
    @Test
    def testExprSimple{
     assertEquals( LiteralReal(Pos(1,1),"1.0e-10"),parseAll(exprSimple,"1.0e-10").get)
     assertEquals( Identificador("OTR0_ID500",Pos(1,1)),parseAll(exprSimple,"OTR0_ID500").get)
     assertEquals( ExpresionInvocacion(Identificador("mi_id",Pos(1,1)),List()),parseAll(exprSimple,"mi_id()").get)
     assertEquals( ExpresionAndAlso(Pos(1,4),
                                    Identificador("a",Pos(1,2)),
                                    Identificador("b",Pos(1,12))),
                   parseAll(exprSimple,"(a andalso b)").get)
    }

    @Test
    def testExprInvocacion{
      assertEquals( ExpresionInvocacion(Identificador("invoo",Pos(1,1)),List()),parseAll(exprSimple,"invoo()").get)
      assertEquals( ExpresionInvocacion(Identificador("foo",Pos(1,1)),
                                        List(
                                        ExpresionInvocacion(Identificador("fun",Pos(1,5)),List()),
                                        LiteralBoolean(Pos(1,11),"false")
                                        )),
                    parseAll(exprSimple,"foo(fun(),false)").get)
      assertEquals( ExpresionInvocacion(Identificador("xy",Pos(1,1)),
                                        List(
                                        ExpresionMultiplicacion(Pos(1,6),
                                                                Identificador("x", Pos(1,5)),
                                                                Identificador("y", Pos(1,7)))
                                        )),parseAll(exprSimple,"xy( x*y )").get)                    
    }
    
    
    //
    //ENUNCIADOS
    //
    @Test 
		def testEnunExit {
			assertEquals(EnunExit(Pos(1, 1)), parseAll(enunExit, "exit;").get)
		}
		
		@Test
		def testEnunReturn {
			assertEquals(
        EnunReturn(Pos(1,1), LiteralBoolean(Pos(1,8), "true")),
        //EnunReturn(Pos(1,1), Identificador("true", Pos(1,8))),
        parseAll(enunReturn, "return true;").get)
			assertEquals(
        EnunReturn(Pos(1,1),
          ExpresionSuma(Pos(1,9),
            LiteralInteger(Pos(1,8), "2"), LiteralInteger(Pos(1,10), "2"))),
        parseAll(enunReturn, "return 2+2;").get)
			assertEquals(
        EnunReturn(Pos(1,1), 
          ExpresionNot(Pos(1,8), Identificador("my_var", Pos(1, 12)))),
        parseAll(enunReturn, "return not my_var;").get)
			assertEquals(
        EnunReturn(Pos(1,1), LiteralString(Pos(1,8), "\"str\"")),
        parseAll(enunReturn, "return \"str\";").get)
			assertEquals(
        EnunReturn(Pos(1,1), LiteralVoid()),
        parseAll(enunReturn, "return;").get)
		}
		
		@Test
		def testEnunLoop {
      // uno
			assertEquals(
        EnunLoop(
          List(
            EnunReturn(Pos(1,6), LiteralInteger(Pos(1,13), "0"))
            )
        ),
        parseAll(enunLoop, "loop return 0; end;").get
      )
      // dos
      assertEquals(
        EnunLoop(
          List(
            EnunIf(
              List(
                EnunElseIf(
                  (//tupla
                    Pos(1,6),
                    ExpresionIgual(Pos(1,16), Identificador("my_var", Pos(1,9)), LiteralBoolean(Pos(1,18), "true")), 
                    List(
                      EnunAsignacion(
                        Identificador("other_var", Pos(1, 28)), 
                        ExpresionSuma(Pos(1,49), Identificador("other_var", Pos(1, 40)), LiteralInteger(Pos(1,50), "1"))
                      )
                    )
                  )//tupla
                )
              ),
              Nil//List(Enunciado(),)
            )
          )
      ), 
      parseAll(enunLoop,
          "loop if my_var = true then other_var:=(other_var+1); end; end;").get)
      // tres
      assertEquals(
        EnunLoop(
          List(
            EnunIf(
              List(//IF
                EnunElseIf(
                (//tupla
                  Pos(1,6),
                  LiteralBoolean(Pos(1,9), "true"), 
                  List( // then
                    EnunInvocacion(Identificador("do_nothing", Pos(1, 19)), Nil)// proc 
                    )
                  )//tupla
                ),
                // ElseIf
                EnunElseIf(
                  (//tupla
                    Pos(1,33),
                    ExpresionInvocacion(Identificador("coke", Pos(1, 40)), Nil), 
                    List( // then
                      EnunInvocacion(Identificador("do_babes", Pos(1, 52)), Nil) 
                    )
                  )//tupla
                )
              ),
              List(// Else
                EnunInvocacion(Identificador("do_something", Pos(1, 69)), Nil)
              )
            )
          )
      ), 
      parseAll(enunLoop,
          "loop if true then do_nothing(); elseif coke() then do_babes(); else do_something(); end; end;").get)
      // cuatro
      // "loop if my_var = true then if other_var = true then return 0; end; else write_something(); end; end;").get)
    }
    
    @Test
		def testEnunInvocacion{
			assertEquals(
        EnunInvocacion(Identificador("id_ent", Pos(1,2)), Nil),
        parseAll(enunInvocacion, " id_ent();").get)
			assertEquals( 
        EnunInvocacion(Identificador("mi_id", Pos(1,1)), 
          List(
            LiteralInteger(Pos(1,7), "2")
          )),
        parseAll(enunInvocacion, "mi_id(2);").get )
      assertEquals( 
        EnunInvocacion(Identificador("mi_id", Pos(1,1)), 
          List(
            ExpresionOrElse(Pos(1,12), LiteralBoolean(Pos(1,7), "true"), LiteralBoolean(Pos(1,19), "false"))
          )),
        parseAll(enunInvocacion, "mi_id(true orelse false);").get )
			//assertTrue( parseAll(enunInvocacion, "mi_id(true, \"gnirts\", 2 rem xD);").successful )
			//assertTrue( parseAll(enunInvocacion, "mi_id(false div 4);").successful )
		}
    
    @Test
		def testEnunAsignacion {
			assertEquals(
        EnunAsignacion(
          Identificador("un_id", Pos(1,1)),
          ExpresionSuma(Pos(1,13),
            LiteralInteger(Pos(1,11), "1"), LiteralInteger(Pos(1,15), "2"))
        ),
        parseAll(enunAsignacion, "un_id := (1 + 2);").get)
      assertEquals(
        EnunAsignacion(
          Identificador("un_id", Pos(1,1)),
          ExpresionOr(Pos(1,15),
            LiteralBoolean(Pos(1,10), "true"), LiteralBoolean(Pos(1,18), "false"))
        ),
        parseAll(enunAsignacion, "un_id := true or false;").get)
			//assertTrue( parseAll(enunAsignacion, "un_id := 40.5e+10 / 3;").successful)
			//assertTrue( parseAll(enunAsignacion, "un_id := ID();").successful)
			//assertTrue( parseAll(enunAsignacion, "un_id := (5*9 <= 40) andalso ((3+5*8)>1);").successful)
		}
    
    @Test
		def testEnunIf {
			assertEquals(
        EnunIf(
              List(
                EnunElseIf(
                  (//tupla
                    Pos(1,1),
                    ExpresionSuma(Pos(1,6),
                      LiteralInteger(Pos(1,5), "1"), LiteralInteger(Pos(1,7), "2")), 
                    List( // then
                       EnunReturn(Pos(1,15), LiteralBoolean(Pos(1,22), "false"))
                    )
                  )//tupla
                )
              ),
              Nil// Else
            ),
        parseAll(enunIf, "if (1+2) then return false; end;").get)
        
      assertEquals(
        EnunIf(
              List( // IF
                EnunElseIf(
                (//tupla
                  Pos(1,1),
                  ExpresionSuma(Pos(1,6),
                    LiteralInteger(Pos(1,5), "1"), LiteralInteger(Pos(1,7), "2")), 
                  List( // then
                     EnunReturn(Pos(1,15), LiteralBoolean(Pos(1,22), "false"))
                    )
                  )//tupla
                ),
                // ElseIf
                EnunElseIf(
                  (//tupla
                    Pos(1, 29),
                    ExpresionDivision(Pos(1, 37),
                      LiteralInteger(Pos(1,36), "4"), LiteralInteger(Pos(1,38), "2")),
                    List( // then
                      EnunReturn(Pos(1,45), LiteralBoolean(Pos(1,52), "true"))
                    )
                  )//tupla
                )
              ),
              List(// Else
                EnunReturn(Pos(1,63), LiteralReal(Pos(1,70), "3.0e-6") )
              )
            ),
        parseAll(enunIf, "if (1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;").get) // resta o real???
			//assertTrue( parseAll(enunIf, "if (1+2) then return false; else return true; end;").successful)
			//assertTrue( parseAll(enunIf, "if (1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;").successful)
		}                
    
    @Test
    def testProcedimiento{
      assertEquals(Procedimiento(Identificador("MI_ID",Pos(1,11)),
                                 TVoid,
                                 List(
                                 Parametro(Identificador("x",Pos(1,18)),TBoolean)
                                 ),List(),List()
                                 ),
                   parseAll(procedimiento,"procedure MI_ID (x: boolean) ; begin end;").get)
      assertEquals(Procedimiento(Identificador("MI_ID",Pos(1,11)),
                                 TString,
                                 List(
                                 Parametro(Identificador("x",Pos(1,18)),TBoolean),
                                 Parametro(Identificador("y",Pos(1,30)),TInteger),
                                 Parametro(Identificador("z",Pos(1,42)),TReal)
                                 ),
                                 
                                 List(
                                 Variable(
                                          Identificador("id_var",Pos(1,64)),
                                          TInteger)
                                 ),
                                 List(
                                 EnunReturn(Pos(1,87),
                                            LiteralInteger(Pos(1,94),"0"))
                                 )
                                 ),
                   parseAll(procedimiento,"procedure MI_ID (x: boolean, y: integer, z: real):string ; var id_var: integer; begin return 0; end;").get)
    }
    
    
    @Test
    def testPrograma{
     assertEquals(Programa(List(), List(), List()),
                  parseAll(programa, "program end.").get)
                  
     assertEquals(Programa(List(
                                Variable(Identificador("a",Pos(1,5)),TString),
                                Variable(Identificador("b",Pos(1,8)),TString),
                                Variable(Identificador("c",Pos(1,11)),TString)
                           ),
                           List(
                           Procedimiento(Identificador("MI_ID",Pos(2,11)),
                                 TVoid,
                                 List(
                                 Parametro(Identificador("x",Pos(2,18)),TBoolean)
                                 ),List(),List()
                                 )
                           ),
                           List(
                                EnunReturn(Pos(4,1),
                                    LiteralInteger(Pos(4,8),"0"))
                           )),
                  parseAll(programa,
"""var a, b, c: string;
procedure MI_ID (x: boolean) ; begin end;
program
return 0;
end.
""").get)
                           
    }
  }
  
}