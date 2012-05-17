package nyota {
  
  import java.io.{FileReader, FileWriter}
  
  object Main extends ParserNyota {
    
    def main(args: Array[String]) {
      
      if (args.length != 2) {
        System.err.println("Se debe indicar el nombre del archivo de entrada y el de salida.")
        exit(1)
      }
      
      val entrada = new FileReader(args(0))
      println(args(0))
      val resultado = parseAll(programa, entrada)
      
      resultado match {
        case Success(ast, _) =>
          println("Sintaxis OK!")
          var salida: FileWriter = null      
          try {                      
            val tabla = AnalizadorSemantico.analizaPrograma(ast)
            println("Analisis semantico OK!")
  
            salida = new FileWriter(args(1))
            salida.write(GeneradorCIL.genera(ast, tabla))
            println("Generacion de codigo OK!")
          } catch {  
            case e: SemanticException =>
              println(e.getMessage)
              exit(1)           
          } finally {
            if (salida != null)
              salida.close()
          }
          exit(0)
        case ns: NoSuccess =>
          println("Error de sintaxis en la linea " + ns.next.pos.line)
          println(ns.msg)
      }
    }
    
    private def imprimeTablaSimbolos(tabla:Map[String,TableEntry]){
      if (tabla.size == 0){
        println("---Vacía---")
      }else{
        for (k<-tabla.keys){
            tabla(k) match{
              case ProcEntry(tipo, lstParams, tablaLocal) =>
                                println("Procedimiento %s(%s):%s".format(k,lstParams.mkString(","),tipo))
                                println("Tabla Local:")
                                imprimeTablaSimbolos(tablaLocal)
                                println("---------------")
              case e => println("%s : %s".format( k, e ) )
            }
        }
      }
    }
    
  }
}