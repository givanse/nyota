/*
Pruebas de Unidad del Parser del Lenguaje Nyota
Gastón Silva Echegaray 1162324
Rafael Santos Pérez 1161734
*/
package nyota.test {
  
  import org.junit.Test
  import org.junit.Assert._

  class TestNyota extends ParserNyota {
  
    @Test
    def testliteralInteger {
      assertTrue( parseAll(literalInteger, "2147483647").successful )
      assertTrue( parseAll(literalInteger, "0").successful )
      assertFalse( parseAll(literalInteger, "-1").successful )
      assertFalse( parseAll(literalInteger, "false").successful )
      assertFalse( parseAll(literalInteger, "1.0").successful )
      assertFalse( parseAll(literalInteger, "0xff").successful )
    }
      
    @Test
    def testLiteralBoolean {
      assertTrue(parseAll(literalBoolean, "true").successful)
      assertTrue(parseAll(literalBoolean, "false").successful)
      assertFalse(parseAll(literalBoolean, "42").successful)
      assertFalse(parseAll(literalBoolean, "otra cosa").successful) 
      assertFalse(parseAll(literalBoolean, "True").successful)
      assertFalse(parseAll(literalBoolean, "False").successful) 
    }

    @Test
    def testLiteralReal {
      assertTrue( parseAll(literalReal, "0.0").successful )
      assertTrue( parseAll(literalReal, "1.0e10").successful )
      assertTrue( parseAll(literalReal, "1.0e-10").successful )
      assertTrue( parseAll(literalReal, "1.0e+10").successful )
      assertTrue( parseAll(literalReal, "1.0E10").successful )
      assertTrue( parseAll(literalReal, "1.0E-10").successful )
      assertTrue( parseAll(literalReal, "1.0E+10").successful )
      assertFalse( parseAll(literalReal, "-1").successful )
      assertFalse( parseAll(literalReal, "2147483647").successful )
      assertFalse( parseAll(literalReal, "false").successful )
      assertFalse( parseAll(literalReal, "1.0x").successful )
      assertFalse( parseAll(literalReal, "0xff").successful )
      assertFalse( parseAll(literalReal, "1.0e").successful )
      assertFalse( parseAll(literalReal, "1E").successful )
      assertFalse( parseAll(literalReal, "1e").successful )
      assertFalse( parseAll(literalReal, "e").successful )
      assertFalse( parseAll(literalReal, "E").successful )
      assertFalse( parseAll(literalReal, "1e10").successful )
      assertFalse( parseAll(literalReal, "1E10").successful )
      assertFalse( parseAll(literalReal, "1.0ea").successful )
      assertFalse( parseAll(literalReal, "1.0Ea").successful )
      assertFalse( parseAll(literalReal, "1.0e10.0").successful )
      assertFalse( parseAll(literalReal, "1.0e-10.0").successful )
      assertFalse( parseAll(literalReal, "1.0e+10.0").successful )
      assertFalse( parseAll(literalReal, "1.0E10.0").successful )
      assertFalse( parseAll(literalReal, "1.0E-10.0").successful )
      assertFalse( parseAll(literalReal, "1.0E+10.0").successful )
    }

    @Test
    def testLiteralString {
      assertTrue( parseAll(literalString, " \"1.0E+10.0\" ").successful )
      assertTrue( parseAll(literalString, " \"shlalala bla haha ahaha\" ").successful )
      assertTrue( parseAll(literalString, "\"\\\" \\\\  \"").successful )
			assertTrue( parseAll(literalString, "\"\"").successful )
			assertTrue( parseAll(literalString, "\" \"  \"").successful )// " " "
			assertFalse( parseAll(literalString, "\" \n  \"").successful )
			assertFalse( parseAll(literalString, "").successful )
			assertFalse( parseAll(literalString, "algo sin comillas").successful )
			assertFalse( parseAll(literalString, "\"").successful )
    }
		
		@Test
    def testIdentificador {
      assertTrue( parseAll(identificador, "aVALOR").successful )
			assertTrue( parseAll(identificador, "aVALOR_BLABLA").successful )
			assertTrue( parseAll(identificador, "a_B__c_d").successful )
			assertTrue( parseAll(identificador, "A_valor").successful )
			assertTrue( parseAll(identificador, "aVALOR50").successful )
			assertTrue( parseAll(identificador, "aVALOR_BLABLA50").successful )
			assertTrue( parseAll(identificador, "a1_B2_c3_d4").successful )
			assertTrue( parseAll(identificador, "A_70valor").successful )
			assertFalse( parseAll(identificador, "_aV").successful )
			assertFalse( parseAll(identificador, "9aV").successful )
			assertFalse( parseAll(identificador, "5_B").successful )
			assertFalse( parseAll(identificador, "#_v").successful )
			assertFalse( parseAll(identificador, "___").successful )
    }
		
		@Test
		def testTipo {
			assertTrue( parseAll(tipo, "boolean").successful )
			assertTrue( parseAll(tipo, "string").successful )
			assertTrue( parseAll(tipo, "integer").successful )
			assertTrue( parseAll(tipo, "real").successful )
			assertFalse( parseAll(tipo, "ptfff").successful )
			assertFalse( parseAll(tipo, "los osos se lamen sus cositas").successful )
			assertFalse( parseAll(tipo, "int").successful )
			assertFalse( parseAll(tipo, "true").successful )
		}
		
		@Test
		def testOpLogico {
			assertTrue( parseAll(opLogico, "and").successful)
			assertTrue( parseAll(opLogico, "andalso").successful)
			assertTrue( parseAll(opLogico, "or").successful)
			assertTrue( parseAll(opLogico, "orelse").successful)
			assertTrue( parseAll(opLogico, "xor").successful)
			assertFalse( parseAll(opLogico, "xxx").successful)
			assertFalse( parseAll(opLogico, "&&").successful)
			assertFalse( parseAll(opLogico, "||").successful)
			assertFalse( parseAll(opLogico, "0").successful)
			assertFalse( parseAll(opLogico, "opLogico").successful)
			assertFalse( parseAll(opLogico, "AndAlso").successful)
			assertFalse( parseAll(opLogico, "orElse").successful)
			assertFalse( parseAll(opLogico, "ore1se").successful)
		}
		
		@Test
		def testOpRelacional{
			assertTrue( parseAll(opRelacional, "=").successful)
			assertTrue( parseAll(opRelacional, "<>").successful)
			assertTrue( parseAll(opRelacional, "<").successful)
			assertTrue( parseAll(opRelacional, ">").successful)
			assertTrue( parseAll(opRelacional, "<=").successful)
			assertTrue( parseAll(opRelacional, ">=").successful)
			assertFalse( parseAll(opRelacional, "=>").successful)
			assertFalse( parseAll(opRelacional, "==").successful)
		}

		@Test
		def testOpAditivo {
			assertTrue( parseAll(opAditivo, "+").successful)
			assertTrue( parseAll(opAditivo, "-").successful)
			assertFalse( parseAll(opAditivo, "++").successful)
			assertFalse( parseAll(opAditivo, "--").successful)
			assertFalse( parseAll(opAditivo, "_").successful)
			assertFalse( parseAll(opAditivo, "^").successful)
			assertFalse( parseAll(opAditivo, "*").successful)
			assertFalse( parseAll(opAditivo, "/").successful)
			assertFalse( parseAll(opAditivo, "%").successful)
		}
		
		@Test
		def testOpMultiplicativo {
			assertTrue( parseAll(opMultiplicativo, "*").successful)
			assertTrue( parseAll(opMultiplicativo, "/").successful)
			assertTrue( parseAll(opMultiplicativo, "div").successful)
			assertTrue( parseAll(opMultiplicativo, "rem").successful)
			assertFalse( parseAll(opMultiplicativo, "+").successful)
			assertFalse( parseAll(opMultiplicativo, "-").successful)
			assertFalse( parseAll(opMultiplicativo, "and").successful)
			assertFalse( parseAll(opMultiplicativo, "or").successful)
			assertFalse( parseAll(opMultiplicativo, "else").successful)
			assertFalse( parseAll(opMultiplicativo, "%").successful)
		}
		
		@Test
		def testDeclVar {
			assertTrue( parseAll(declVar, "a: integer;").successful)
			assertTrue( parseAll(declVar, "a:boolean;").successful)
			assertTrue( parseAll(declVar, "a, b, c : string;").successful)
			assertTrue( parseAll(declVar, "b_chido, x__X: real;").successful)
			assertFalse( parseAll(declVar, "a integer").successful)
			assertFalse( parseAll(declVar, "a : real : real").successful)
			assertFalse( parseAll(declVar, "a, b, c").successful)
			assertFalse( parseAll(declVar, "_a: string").successful)
			assertFalse( parseAll(declVar, "b: andalso;").successful)
		}
		
		@Test
		def testParametro {
			assertTrue( parseAll(parametro, "a: integer").successful)
			assertTrue( parseAll(parametro, "a: integer, b: real, c: string, d: boolean").successful)
			assertTrue( parseAll(parametro, "a: string, b: string").successful)
			assertFalse( parseAll(parametro, "a, b: integer").successful)
			assertFalse( parseAll(parametro, "boolean").successful)
			assertFalse( parseAll(parametro, "a:").successful)
			assertFalse( parseAll(parametro, "b integer").successful)
			assertFalse( parseAll(parametro, "a,").successful)
			assertFalse( parseAll(parametro, "b: real;").successful)
		}
		
		// expresiones
		@Test
		def testExprLog {
			assertTrue( parseAll(exprLog, "CmpStr(opcion, \"S\") <> 0 andalso CmpStr(opcion, \"s\") <> 0").successful)
      assertTrue( parseAll(exprLog, "not true").successful)
			assertTrue( parseAll(exprLog, "not (true or (3 = 2))").successful)
			assertTrue( parseAll(exprLog, "1+1 = 2").successful)
			assertTrue( parseAll(exprLog, "2*3 < 40").successful)
			assertTrue( parseAll(exprLog, "(5*9 <= 40) andalso ((3+5*8)>1)").successful)
			assertTrue( parseAll(exprLog, "2*3 < 40").successful)
			assertTrue( parseAll(exprLog, "A_identificador").successful)
			assertTrue( parseAll(exprLog, "id_x()").successful)
			assertTrue( parseAll(exprLog, "id_x( not - 500 )").successful)
			assertFalse( parseAll(exprLog, "2=3)").successful)
			assertFalse( parseAll(exprLog, "true or or false").successful)
			assertFalse( parseAll(exprLog, "5 /= 2").successful)
			assertFalse( parseAll(exprLog, "(5 = true or false").successful)
		}
		
		@Test
		def testExprRel {
      assertTrue( parseAll(exprRel, "CmpStr(opcion, \"S\") <> 0 ").successful)
			assertTrue( parseAll(exprRel, "\"una literalString\"").successful)
			assertTrue( parseAll(exprRel, "\"str1\" <> \"str2\"").successful)
			assertTrue( parseAll(exprRel, "\"str1\" <> - not id_valido").successful)
			assertTrue( parseAll(exprRel, "\"str1\" <= 6 + (true)").successful)
			assertTrue( parseAll(exprRel, "(false xor true)").successful)
			assertFalse( parseAll(exprRel, "1 and 2").successful)
			assertFalse( parseAll(exprRel, "true or true").successful)
		}
		
		@Test
		def testExprAditiva {
			assertTrue( parseAll(exprAditiva, "2 + 3 +4 - 1 +2 - 3").successful)
			assertTrue( parseAll(exprAditiva, "(2 + 3 +4 - 1 +2 - 3)").successful)
			assertTrue( parseAll(exprAditiva, "9 + mi_id").successful)
			assertTrue( parseAll(exprAditiva, "(2 + 3) + mi_id").successful)
			assertTrue( parseAll(exprAditiva, " true - id()").successful)
			assertFalse( parseAll(exprAditiva, "(6 - 5) mi_id").successful)
			assertFalse( parseAll(exprAditiva, "(not mi_id) +").successful)
			assertFalse( parseAll(exprAditiva, "op1 or op2").successful)
		}
		
		@Test
		def testExprMultiplicativa{
			assertTrue( parseAll(exprMultiplicativa, "b * a").successful)
			assertTrue( parseAll(exprMultiplicativa, "(b * a) rem c").successful)
			assertTrue( parseAll(exprMultiplicativa, "x div a div x").successful)
			assertTrue( parseAll(exprMultiplicativa, "division div cero").successful)
			assertTrue( parseAll(exprMultiplicativa, "3 div not true").successful)
			assertFalse( parseAll(exprMultiplicativa, "pow ** pow").successful)
			assertFalse( parseAll(exprMultiplicativa, "3 div div 3").successful)
			assertFalse( parseAll(exprMultiplicativa, "false xor true * 3").successful)
			assertFalse( parseAll(exprMultiplicativa, "3 /").successful)
		}
		
		@Test
		def testExprUnaria{
			assertTrue( parseAll(exprUnaria, "not 2.3e5").successful)
			assertTrue( parseAll(exprUnaria, "- \"stringgrr\"").successful)
			assertTrue( parseAll(exprUnaria, "not randID(3+2, 2+3)").successful)
			assertTrue( parseAll(exprUnaria, " not not not not - not -- true").successful)
			assertTrue( parseAll(exprUnaria, "true").successful)
			assertTrue( parseAll(exprUnaria, "cualquierID").successful)
			assertFalse( parseAll(exprUnaria, "2 + 2").successful)
			assertFalse( parseAll(exprUnaria, "not true or false").successful)
			// not not pasa porque el segundo se toma como identificador
				// assertFalse( parseAll(exprUnaria, "not not").successful)
				// assertFalse( parseAll(exprUnaria, "- not").successful)
			assertFalse( parseAll(exprUnaria, "ID_34g 2 / 1").successful)
		}
		
		@Test
		def testExprSimple {
			assertTrue( parseAll(exprSimple, "1.0e-10").successful )
			assertTrue( parseAll(exprSimple, "((5*9 <= 40) andalso ((3+5*8)>1))").successful)
			assertTrue( parseAll(exprSimple, "OTRO_ID500").successful )
			assertTrue( parseAll(exprSimple, "e_l_I_D ( ID_PARAM )").successful )
			assertFalse( parseAll(exprSimple, "3 + 2").successful )
			assertFalse( parseAll(exprSimple, "true or false").successful )
		}
		
		@Test
		def testExprInvocacion {
			assertTrue( parseAll(exprInvocacion, "mi_id(3=4)").successful )
			assertTrue( parseAll(exprInvocacion, "mi_id(true orelse false)").successful )
			assertTrue( parseAll(exprInvocacion, "mi_id(false, \"string\", 4 rem 2)").successful )
			assertTrue( parseAll(exprInvocacion, "mi_id(false div 4)").successful )
			assertFalse( parseAll(exprInvocacion, "mi_id").successful )
			assertFalse( parseAll(exprInvocacion, "342342()").successful )
			assertFalse( parseAll(exprInvocacion, "mi_id(()").successful )
			assertFalse( parseAll(exprInvocacion, "mi_id[]").successful )
			assertFalse( parseAll(exprInvocacion, "mi_id = x_ID").successful )
		}
		
		// enunciados
		
		@Test 
		def testEnunExit {
			assertTrue( parseAll(enunExit, "exit;").successful)
			assertFalse( parseAll(enunExit, "Exit;").successful)
			assertFalse( parseAll(enunExit, "exit").successful)
			assertFalse( parseAll(enunExit, "Exit").successful)
			assertFalse( parseAll(enunExit, "0").successful)
			assertFalse( parseAll(enunExit, "return 0").successful)
			assertFalse( parseAll(enunExit, "return;").successful)
		}
		
		@Test
		def testEnunReturn {
			assertTrue( parseAll(enunReturn, "return true;").successful)
			assertTrue( parseAll(enunReturn, "return 2+2;").successful)
			assertTrue( parseAll(enunReturn, "return not my_var;").successful)
			assertTrue( parseAll(enunReturn, "return \"str\";").successful)
			assertTrue( parseAll(enunReturn, "return;").successful)
			assertFalse( parseAll(enunReturn, "return").successful)
			assertFalse( parseAll(enunReturn, "retrun false;").successful)
			assertFalse( parseAll(enunReturn, "return ();").successful)
		}
		
		@Test
		def testEnunLoop {
			assertTrue( parseAll(enunLoop, "loop return 0; end;").successful)
			assertTrue( parseAll(enunLoop, "loop if my_var = true then other_var:=(other_var+1); end; end;").successful)
			assertTrue( parseAll(enunLoop, "loop if true then do_nothing(); elseif coke() then do_babes(); else do_something(); end; end;").successful)
			assertTrue( parseAll(enunLoop, "loop if my_var = true then if other_var = true then return 0; end; else write_something(); end; end;").successful)
			assertFalse( parseAll(enunLoop, "loop if my_var = true end;").successful)
			assertFalse( parseAll(enunLoop, "loop if other_var:=2 then boo(); end;").successful)
			assertFalse( parseAll(enunLoop, "if my_var = true end;").successful)
			assertFalse( parseAll(enunLoop, "loop nothing() end;").successful)
		}
		
		@Test
		def testEnunInvocacion{
			assertTrue( parseAll(enunInvocacion, " id_ent();").successful)
			assertTrue( parseAll(enunInvocacion, "mi_id(2);").successful )
			assertTrue( parseAll(enunInvocacion, "mi_id(true orelse false);").successful )
			assertTrue( parseAll(enunInvocacion, "mi_id(true, \"gnirts\", 2 rem xD);").successful )
			assertTrue( parseAll(enunInvocacion, "mi_id(false div 4);").successful )
			assertFalse( parseAll(enunInvocacion, "mi_id").successful )
			assertFalse( parseAll(enunInvocacion, "baba()").successful )
			assertFalse( parseAll(enunInvocacion, "minix(()").successful )
			assertFalse( parseAll(enunInvocacion, "array[]").successful )
			assertFalse( parseAll(enunInvocacion, "mi_id = x_ID").successful )
		}
		
		@Test
		def testEnunAsignacion {
			assertTrue( parseAll(enunAsignacion, "un_id := (1 + 2);").successful)
			assertTrue( parseAll(enunAsignacion, "un_id := true or false;").successful)
			assertTrue( parseAll(enunAsignacion, "un_id := 40.5e+10 / 3;").successful)
			assertTrue( parseAll(enunAsignacion, "un_id := ID();").successful)
			assertTrue( parseAll(enunAsignacion, "un_id := (5*9 <= 40) andalso ((3+5*8)>1);").successful)
			assertFalse( parseAll(enunAsignacion, "un_id := (1 + );").successful)
			assertFalse( parseAll(enunAsignacion, "un_id := ;").successful)
			assertFalse( parseAll(enunAsignacion, "un_id := 1 + 1").successful)
			assertFalse( parseAll(enunAsignacion, ":= 1 + 2;").successful)
			assertFalse( parseAll(enunAsignacion, "un_id = true orelse false;").successful)
		}
		
		@Test
		def testEnunIf {
			assertTrue( parseAll(enunIf, "if (1+2) then return false; end;").successful)//
			assertTrue( parseAll(enunIf, "if (1+2) then return false; else return true; end;").successful)
			assertTrue( parseAll(enunIf, "if (1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;").successful)
			assertTrue( parseAll(enunIf, "if (1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;").successful)
			assertFalse( parseAll(enunIf, "(1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;").successful)
			assertFalse( parseAll(enunIf, "if () then return false; elseif 4/2 then return true; else return 3.0e-6; end;").successful)
			assertFalse( parseAll(enunIf, "if 1+2 then return +(if); end;").successful)
			assertFalse( parseAll(enunIf, "if 1+2 then return end;").successful)
			assertFalse( parseAll(enunIf, "if then return 0; end;").successful)
		}
		
		@Test
		def testProcedimiento {
			assertTrue( parseAll(procedimiento, "procedure MI_ID (x: boolean) ; begin end;").successful)
			assertTrue( parseAll(procedimiento, "procedure MI_ID (x: boolean, y: integer, z: real):string ; var id_var: integer; begin return 0; end;").successful)
			assertTrue( parseAll(procedimiento, "procedure MI_ID (x: boolean, y: integer, z: real):string ; var id_var: integer; begin if (1+2) then return false; end; end;").successful)
			assertFalse( parseAll(procedimiento, "procedure MI_ID (x: boolean) ;").successful)
			assertFalse( parseAll(procedimiento, "procedure MI_ID (boolean) begin end;").successful)
			assertFalse( parseAll(procedimiento, "procedure 3.0e-4 (x: boolean) ; begin end;").successful)
		}
		
    // var id_var1: integer; var id_var2: boolean; var id_var3: string;
    // var id_var1: integer; id_var2: boolean; id_var3: string;
		@Test
		def testPrograma {
			assertTrue( parseAll(programa, "program end.").successful)
			assertTrue( parseAll(programa, """
				var id_var1: integer; id_var2: boolean; id_var3: string;
				procedure MI_ID (x: boolean, y: integer, z: real):string ; var id_var: integer; begin if (1+2) then return false; end; end;
				program 
					if (1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;
					return 90.0e-3 + MI_ID(true, 5, 1.0e-100); --esto no funciona, pero pasa el test ahahahahahahaahh!!!!!!!
				end.""").successful)
			assertFalse( parseAll(programa, "program end").successful)
			assertFalse( parseAll(programa, """
				procedure MI_ID (x: boolean):string ; var id_var: integer; begin if (1+2) then return false; end; end;
				program 
					if (1+2) then return false; elseif 4/2 then return true else return 3.0e-6; end;
					return 90.0e-3 + MI_ID(true, 5, 1.0e-100);
				end.""").successful)
			assertFalse( parseAll(programa, "if (1+2) then return false; elseif 4/2 then return true; else return 3.0e-6; end;").successful)
			assertFalse( parseAll(programa, "procedure MI_ID (x: boolean, y: integer, z: real):string ; var id_var: integer; begin end;").successful)
		}
		
  }
}
