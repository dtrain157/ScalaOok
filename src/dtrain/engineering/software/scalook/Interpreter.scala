package dtrain.engineering.software.scalook

import scala.collection.immutable.Vector;
import scala.Enumeration;


object Interpreter {

  var workingVec = Vector(0)
  var pos = 0
  
  def interpret(filename: String) {
    val fileContent = getFileContents(filename);
   
    if (Parser.checkSyntax(fileContent)) {
      val instructions = Parser.parseFile(fileContent);
      execute(instructions, 0)
    }
  }
  
  def getFileContents(filename: String) : String = {
    scala.io.Source.fromFile(filename).mkString
  }
  
  def execute(instructions: List[String], pos: Int) {
    if (pos < instructions.length) {
	 
      instructions(pos) match  {
	      
	    case Constants.INCREMENT => { 
	      Interpreter.increment();
	      execute(instructions, pos+1)
	    }
	      
	    case Constants.DECREMENT => {
	      Interpreter.decrement();
	      execute(instructions, pos+1)
	    }
	      
	    case Constants.WRITE => {
	      Interpreter.write();
	      execute(instructions, pos+1)
	    }
	      
	    case Constants.READ => {
	      Interpreter.read();
	      execute(instructions, pos+1)
	    }
	      
	    case Constants.MOVE_NEXT => {
	      Interpreter.moveToNext();
	      execute(instructions, pos+1)
	    }
	      
	    case Constants.MOVE_PREV => {
	      Interpreter.moveToPrevious();
	      execute(instructions, pos+1)
	    }
	     
	    case Constants.OPEN_LOOP => {
	      execute(instructions, Interpreter.openBracket(instructions, pos))
	    }
	      
	    case Constants.CLOSE_LOOP => {
	      execute(instructions, Interpreter.closeBracket(instructions, pos))
	    }
	    
	  } 
    }
    
  }
     
  def moveToNext() {
    if (pos == workingVec.length-1) {
      workingVec = workingVec:+0;
    }
    pos = pos+1
  }
  
  def increment() {
    workingVec = workingVec.updated(pos, workingVec(pos)+1)
  }
  
  def read() {
    //TODO
  }
  
  def openBracket(instructions: List[String], currentPos: Int): Int = {
    if (workingVec(pos) == 0) {
    	findCloseBracket(instructions, currentPos, 0)
    } else {
      currentPos + 1;
    }
  }
  
  def write() {
    print(workingVec(pos).toChar)
  }
  
  def decrement() {
    workingVec = workingVec.updated(pos, workingVec(pos)-1)
  }
  
  def moveToPrevious() {
    if (pos == 0) {
      workingVec = 0+:workingVec;
      pos = pos + 1;
    }
    pos = pos - 1;
  }
  
  def closeBracket(instructions: List[String], currentPos: Int): Int = {
    if (workingVec(pos) != 0) {
      findOpenBracket(instructions, currentPos, 0)	
    } else{
      currentPos + 1;
    }
  }
  
  def findOpenBracket(instructions: List[String], currentPos: Int, numberOfBrackets: Int): Int = {
    if (instructions(currentPos) == Constants.CLOSE_LOOP) {
      findOpenBracket(instructions, currentPos-1, numberOfBrackets+1)
    } else if ((instructions(currentPos) == Constants.OPEN_LOOP) && (numberOfBrackets-1 == 0)) {
      currentPos + 1
      
    } else if ((instructions(currentPos) == Constants.OPEN_LOOP) && (numberOfBrackets-1 != 0)) {
      findOpenBracket(instructions, currentPos-1, numberOfBrackets-1)
    } else {
      findOpenBracket(instructions, currentPos-1, numberOfBrackets)
    }   
  }
  
  def findCloseBracket(instructions: List[String], currentPos: Int, numberOfBrackets: Int): Int = {
    if (instructions(currentPos) == Constants.OPEN_LOOP) {
      findOpenBracket(instructions, currentPos+1, numberOfBrackets+1)
    } else if ((instructions(currentPos) == Constants.CLOSE_LOOP) && (numberOfBrackets-1 == 0)) {
      currentPos + 1
    } else if ((instructions(currentPos) == Constants.CLOSE_LOOP) && (numberOfBrackets-1 != 0)) {
      findOpenBracket(instructions, currentPos+1, numberOfBrackets-1)
    } else {
      findOpenBracket(instructions, currentPos+1, numberOfBrackets)
    }
  }
}


