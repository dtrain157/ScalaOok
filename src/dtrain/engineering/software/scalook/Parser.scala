package dtrain.engineering.software.scalook

object Parser {
  
  def checkSyntax(fileContents: String): Boolean = {
    for (s <- fileContents.split("\\s+")) {
      if (!Constants.reservedWords.contains(s)) { return false }
    }
    
    return true
  }
  
  def parseFile(fileContents: String): List[String] = {
    val tuples = fileContents.split("\\s+").toList.sliding(2,2).toList.collect{ case List(x,y) => (x,y) }
    
    val symbols = for (tuple <- tuples) yield {
      tuple match {
      	case ("Ook.", "Ook.") => Constants.INCREMENT;
      	case ("Ook.", "Ook!") => Constants.READ;
      	case ("Ook.", "Ook?") => Constants.MOVE_NEXT;
      	case ("Ook!", "Ook.") => Constants.WRITE;
      	case ("Ook!", "Ook!") => Constants.DECREMENT;
      	case ("Ook!", "Ook?") => Constants.OPEN_LOOP;
      	case ("Ook?", "Ook.") => Constants.MOVE_PREV;
      	case ("Ook?", "Ook!") => Constants.CLOSE_LOOP;
      	case _ => Constants.ERROR;
      }
    }
    
    return symbols
  }
}