/*
Parser del Lenguaje Nyota
Gastón Silva Echegaray 1162324
Rafael Santos Pérez 1161734
*/
package nyota {
  
  import scala.util.parsing.combinator.RegexParsers
  
  class ParserNyota extends RegexParsers {
    
    // Ignorar espacios blancos y comentarios
    protected override val whiteSpace = """(\s|(--.*)|[(][*]([^*]|[*]+[^*)])*[*]+[)])+""".r
    
    //
    def pos(p: Parser[String]): Parser[(String, Pos)] = 
      new Parser[(String, Pos)] {
        def apply(in: Input): ParseResult[(String, Pos)] = {
          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)
          p(in) match {
            case Success(str, otherIn) =>
              val p = in.drop(start - offset).pos
              Success((str, Pos(p.line, p.column)), otherIn)
            case nosucc: NoSuccess => 
              nosucc
          }
        }
      }
    
    // parser
    def identificador: Parser[Identificador] = 
      pos("""[a-zA-Z][\w_]*""".r) ^^ {
        case (nombre, posId) => Identificador(nombre, posId)
      }
  
    def tipo: Parser[Tipo] = 
      ("boolean" | "string" | "integer" | "real") ^^ {
        case "boolean" => TBoolean
        case "string" => TString
        case "integer" => TInteger
        case "real" => TReal
      }
      
    // GLC
    def programa: Parser[Programa] =
      (declVars ~ rep(procedimiento) ~ "program" ~ rep(enunciado) ~ "end" ~ ".") ^^ {
        case variables ~ procedimientos ~ "program" ~ enunciados ~ "end" ~ "." =>
            Programa(variables, procedimientos, enunciados)
        }
    
    // posId nombre tipo parametros variables enunciados
    def procedimiento: Parser[Procedimiento] =
      (
      "procedure" ~> identificador ~ ("(" ~> (opt(parametro)^^{case Some(lstParams)=>lstParams
                                                               case None=> Nil})
                                             ) ~ (")"~> 
        ( // TIPO
          opt(":"~> tipo) ^^ {
            case Some(resultType) => resultType
            case None => TVoid
          }
        )) 
        ~ (";" ~> declVars) ~ ("begin"~> rep(enunciado)) <~ ("end" ~ ";")
      ) ^^ {
          case procID ~ paramLst ~ tipo ~ variables ~ enunciados =>
            Procedimiento(procID, tipo, paramLst, variables, enunciados)
        }
    
    // x, y, z: Int; Parser[nyota.Identificador]
    def declVar: Parser[List[Variable]] =
      identificador ~ (rep(","~>identificador)<~ ":") ~ (tipo <~ ";") ^^ {
      case varID ~ lstVars ~ tipo => Variable(varID, tipo)::(for(v <- lstVars) yield Variable(v, tipo))
    }
    
    def declVars: Parser[List[Variable]] =
      opt( "var" ~> rep1(declVar) ) ^^ {
        case Some(lstVars) => lstVars.flatten//List[List[nyota.Variable]]
        case None => Nil
      }
    
    def parametro: Parser[List[Parametro]] =
      (identificador <~ ":") ~ tipo ~ rep(("," ~> identificador ~ (":" ~> tipo)) ^^ {
        case paramID ~ tipo => Parametro(paramID, tipo)
        }) ^^ {
          case paramID ~ tipo ~ lstParams => Parametro(paramID, tipo)::lstParams
        }
    
    //literales
    def literal: Parser[Expresion] =
      literalReal | literalInteger | literalBoolean | literalString 
    
    def literalInteger: Parser[Expresion] =
      pos("""\d+""".r) ^^ {
        case (valor, pos) => LiteralInteger(pos, valor)
      }
    
    def literalBoolean: Parser[Expresion] =
      ( pos("true") | pos("false") )^^ { 
        case (valor, posBool) => LiteralBoolean(posBool, valor)
      }
      
    def literalReal: Parser[Expresion] =
      pos("""\d+[.]\d+([eE][+-]?\d+)?""".r) ^^ {
        case (valor, posReal) => LiteralReal(posReal, valor)
      }
      
    def literalString: Parser[Expresion] =
      pos("""[\"].*[\"]""".r) ^^{
        case (valor, posStr) => LiteralString(posStr,valor)
      }
      
   // operadores 
   def opLogico: Parser[(String,Pos)] =
      pos("andalso") | pos("and") | pos("orelse") | pos("or") | pos("xor")
      
   def opRelacional: Parser[(String,Pos)] =
      pos("<>") | pos("<=") | pos(">=") | pos("<") | pos(">") | pos("=")
      
   def opAditivo: Parser[(String,Pos)] =
      pos("+") | pos("-")
      
   def opMultiplicativo: Parser[(String,Pos)] =
      pos("*") | pos("/") | pos("div") | pos("rem")
      
   //Expresiones:
   def exprLog: Parser[Expresion] =
      exprRel ~ rep((opLogico ~ exprRel) ^^ {
       case ("andalso",pos) ~ expRel => (ExpresionAndAlso, expRel, pos)
       case ("and",pos) ~ expRel => (ExpresionAnd, expRel, pos)
       case ("orelse",pos) ~ expRel => (ExpresionOrElse, expRel, pos)
       case ("or",pos) ~ expRel => (ExpresionOr, expRel, pos)
       case ("xor",pos) ~ expRel => (ExpresionXor, expRel, pos)
     }) ^^ {
       case exprIzq ~ lstTuplas =>
         //println("exprIzq#\n\t\t"+exprIzq+"\nlstTuplas#\n\t\t"+lstTuplas)
         (exprIzq/:lstTuplas)((expI, tupla) => tupla._1(tupla._3, expI, tupla._2))
      }
  
   def exprRel: Parser[Expresion] =
      exprAditiva ~ rep(opRelacional ~ exprAditiva ^^{
       case ("<>",pos) ~ expAd => (ExpresionDiferente,expAd,pos)
       case ("<=",pos) ~ expAd => (ExpresionMenorIgual,expAd,pos)
       case (">=",pos) ~ expAd => (ExpresionMayorIgual,expAd,pos)
       case ("<",pos) ~ expAd => (ExpresionMenor,expAd,pos)
       case (">",pos) ~ expAd => (ExpresionMayor,expAd,pos)
       case ("=",pos) ~ expAd => (ExpresionIgual,expAd,pos)
     }) ^^ {
       case exprAd ~ lstTuplas =>
         //println("exprAd#\n\t\t"+exprAd+"\nlstTuplas#\n\t\t"+lstTuplas)
         (exprAd/:lstTuplas)((ac,tupla)=>tupla._1(tupla._3,ac,tupla._2))
      }
   
   // comprende todas las aritmeticas
   def exprAditiva: Parser[Expresion] =
     exprMultiplicativa ~ rep(opAditivo ~ exprMultiplicativa ^^{
       case ("+",pos) ~ expMult => (ExpresionSuma,expMult,pos)
       case ("-",pos) ~ expMult => (ExpresionResta,expMult,pos)
     }) ^^ {
       case exprMult ~ lstTuplas => (exprMult/:lstTuplas)((ac,tupla)=>tupla._1(tupla._3,ac,tupla._2))
      }
  
   def exprMultiplicativa: Parser[Expresion] =
      exprUnaria ~ rep(opMultiplicativo ~ exprUnaria^^{
       case ("*",pos) ~ expUn => (ExpresionMultiplicacion,expUn,pos)
       case ("/",pos) ~ expUn => (ExpresionDivision,expUn,pos)// real
       case ("div",pos) ~ expUn => (ExpresionCociente, expUn, pos)// entera
       case ("rem",pos) ~ expUn => (ExpresionResiduo,expUn,pos)
     }) ^^ {
       // las tuplas se inicializan en el fold
       case exprUn ~ lstTuplas => (exprUn/:lstTuplas)((ac,tupla)=>tupla._1(tupla._3,ac,tupla._2))
      }   
   
   def exprUnaria: Parser[Expresion] = 
    ( ( ( pos("not") ~ exprUnaria) | (pos("-") ~ exprUnaria) ) ^^ {
        case ("not", posNot) ~ expr => ExpresionNot( posNot, expr)
        case ("-", posNeg) ~ expr => ExpresionNegacion( posNeg, expr)
      } | exprSimple )
      
   // T is restricted to range only over subtypes of type Nodo
   //def exprSimple[ T <: Nodo]: Parser[T] =
   def exprSimple: Parser[Expresion] =
      // Expresion(Nodo) Enunciado(Nodo) Identificador(Expresion(Nodo)) Expresion(Nodo)
      ( ("(" ~> exprLog) <~ ")") | exprInvocacion | literal | identificador
      
   def exprInvocacion: Parser[Expresion] =
    (
      identificador ~ "(" ~ 
      ((
        opt( exprLog ~ rep( "," ~> exprLog ) )
      ) ^^ {
        case Some(expr ~ exprLst) =>
          expr::exprLst
        case None => Nil// ninguna expresion
      }) ~ ")"
    ) ^^ {
      case invoID ~ "(" ~ paramsLst ~ ")" =>
      //println("ID#\n\t\t"+invoID+"\nLst#\n\t\t"+paramsLst)
      ExpresionInvocacion(invoID, paramsLst)
    }
   
   def enunInvocacion: Parser[Enunciado] =
    ((identificador <~ "(") ~ ( (opt( exprLog ~ rep( (","~>exprLog) ) ) ^^ {
          case Some(expr ~ exprLst) => expr::exprLst
          case None => Nil// ninguna expresion
      }) <~ (")" ~ ";"))
    ) ^^ {
      case invoID ~ exprLst => EnunInvocacion(invoID, exprLst)
    }
      
   // enunciados
    
   def enunciado: Parser[Enunciado] =
      enunAsignacion | enunInvocacion | enunIf | enunLoop | enunReturn | enunExit
      
   def enunExit: Parser[Enunciado] =
   (pos("exit")<~";") ^^ {
     case (_, posExit) => EnunExit(posExit)
   }
  
   def enunReturn: Parser[Enunciado] =
   pos("return") ~ (opt(exprLog) ^^ { 
     case Some(expr) => expr
     case None => LiteralVoid()
    }) <~ ";" ^^ {
        case (_, posReturn) ~ expr => EnunReturn(posReturn, expr)
      }
   
   def enunAsignacion: Parser[Enunciado] =
    (identificador <~ ":=") ~ exprLog <~ ";" ^^ {
      case enunAsigID ~ expr => EnunAsignacion(enunAsigID, expr)
    }
   
   //enunciado if
   def enunIf: Parser[Enunciado] = 
    ifClause ~ enunElseIfLst ~ enunElseLst <~ ("end" ~ ";") ^^ {
      case ifClause ~ enunElseIfLst ~ enunElseLst =>
        EnunIf(ifClause::enunElseIfLst, enunElseLst)
    }
   
   def ifClause: Parser[EnunElseIf] =
    pos("if") ~ exprLog ~ ("then" ~> rep(enunciado)) ^^ {
      case (_, posIf) ~ expr ~ enunLst =>
        EnunElseIf((posIf, expr, enunLst))
   } 
   
   def enunElseIfLst: Parser[List[EnunElseIf]] =
    rep( 
      pos("elseif") ~ exprLog ~ ("then" ~> rep(enunciado)) ^^ {
        case (_, posElseIf) ~ expr ~ enunLst =>
          EnunElseIf(posElseIf, expr, enunLst)
        }
    ) ^^ { 
      case lst => lst
    }
    
   def enunElseLst: Parser[List[Enunciado]] =
     opt("else"~>rep(enunciado)) ^^ {
       case Some(enunLst) => enunLst
       case None => Nil// ninguna expresion Nil
     }
   
   // enunciado loop
   def enunLoop: Parser[Enunciado] =
    "loop" ~> (rep(enunciado) <~ ("end"~";")) ^^ {
      case enuns => EnunLoop(enuns)
    }
  }
  
}