package interpreter

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 *
 * ETUDIANT 1 : Benjamin Wojtecki
 *
 *
 */

/** définition d'une exception pour le cas des listes vides
  */
case object ExceptionListeVide extends Exception

/** définition d'une exception pour le cas des listes de tailles différentes
  */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /** UN INTERPRETER POUR LE LANGAGE WHILE
    */

  /** GESTION DE LA MEMOIRE DE L'INTERPRETEUR
    */

  /** définition d'un type Memory pour représenter une mémoire
    */
  type Memory = List[(Variable, Value)]

  /** @param v
    *   : une variable
    * @param mem
    *   : une mémoire
    * @return
    *   m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem, la
    *   valeur par défaut si la variable v n'est pas présente dans la mémoire
    *   mem
    */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match{
      case Nil => NlValue
      case (key, value)::tail => 
        if (key == v){value}
        else{lookUp(v, tail)}
    }
  }

  /** @param v
    *   : une variable
    * @param d
    *   : une valeur
    * @param mem
    *   : une mémoire
    * @return
    *   la mémoire modifiée par l'affectation [v->d]
    */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match{
      case Nil => List((v,d))
      case (key, _) :: tail if (key == v) => (v, d) :: tail
      case head :: tail => head :: assign(v,d,tail)
    }
  }

  /** TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
    */

  /** @param expression
    *   : un AST décrivant une expression du langage WHILE
    * @return
    *   la valeur de l'expression
    */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match{
      case Nl => NlValue
      case Cst(name) => CstValue(name)
      case VarExp(name) => lookUp(Var(name), mem)
      case Cons(arg1, arg2) => ConsValue(interpreterExpr(arg1, mem), interpreterExpr(arg2, mem))
      case Hd(arg) => interpreterExpr(arg, mem) match{
        case ConsValue(h, _) => h
        case _ => NlValue
      }
      case Tl(arg) => interpreterExpr(arg, mem) match{
        case ConsValue(_, h) => h
        case _ => NlValue
      }
      case Eq(arg1, arg2) => 
        val value1 = interpreterExpr(arg1, mem)
        val value2 = interpreterExpr(arg2, mem)
        if (value1 == value2){ConsValue(NlValue, NlValue)} else {NlValue}
    
    }
  }

  /** la fonction interpreterExpr ci-dessus calcule la valeur associée à une
    * expression il peut être utile de produire à l'inverse une expression
    * associée à une valeur la fonction valueToExpression ci-dessous construira
    * l'expression la plus simple associée à une valeur
    *
    * @param value
    *   : une valeur du langage WHILE
    * @return
    *   l'AST décrivant l'expression de cette valeur
    */
  def valueToExpression(value: Value): Expression = {
    value match{
      case NlValue => Nl 
      case CstValue(name) => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1), valueToExpression(arg2))
    }
  }

  /** TRAITEMENT DES COMMANDES DU LANGAGE WHILE
    */

  /** @param command
    *   : un AST décrivant une commande du langage WHILE
    * @param memory
    *   : une mémoire
    * @return
    *   la mémoire après l'interprétation de command
    */
  def interpreterCommand(command: Command, memory: Memory): Memory ={
    command match{
      case Nop => memory
      case Set(var1, exp1) => assign(var1, interpreterExpr(exp1, memory), memory)
      case While(cond1, body) =>
        interpreterExpr(cond1, memory) match {
          case NlValue => memory
          case _ => 
          val newMemory = interpreterCommands(body, memory)
          interpreterCommand(While(cond1, body), newMemory)
        }
      case For(count1, body) => 
        interpreterExpr(count1, memory) match {
          case NlValue => memory
          case CstValue(_) => interpreterCommands(body, memory)         
          case ConsValue(a,b) => 
            var newMemory = interpreterCommands(body, memory)
            interpreterCommand(For(valueToExpression(b), body), newMemory)
        }
      case If(cond1, then_cond, else_cond) => 
        interpreterExpr(cond1, memory) match{
          case NlValue => interpreterCommands(else_cond, memory)
          case _ => interpreterCommands(then_cond, memory)
        }
    }
  }

  /** @param commands
    *   : une liste non vide d'AST décrivant une liste non vide de commandes du
    *   langage WHILE
    * @param memory
    *   : une mémoire
    * @return
    *   la mémoire après l'interprétation de la liste de commandes
    */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory ={
    var newMemory = memory
    for (commande <- commands){
      newMemory = interpreterCommand(commande, newMemory)
    }
    newMemory
  }

  /** TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
    */

  /** @param vars
    *   : une liste non vide décrivant les variables d'entrée d'un programme du
    *   langage WHILE
    * @param vals
    *   : une liste non vide de valeurs
    * @return
    *   une mémoire associant chaque valeur à la variable d'entrée correspondant
    */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    (vars, vals) match{
      case (Nil, Nil) => Nil
      case ((varHead::varTail), (valHead::valTail)) =>
        (varHead, valHead) :: interpreterMemorySet(varTail, valTail)
      case _ => throw new IllegalArgumentException
    }
  }

  /** @param vars
    *   : une liste non vide décrivant les variables de sortie d'un programme du
    *   langage WHILE
    * @param memory
    *   : une mémoire
    * @return
    *   la liste des valeurs des variables de sortie
    */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    def getValueForVariable(variable: Variable, memory: Memory): Value = {
      memory match {
        case Nil => NlValue
        case (memoryVariable, memoryValue) :: tail =>
          if (memoryVariable == variable) memoryValue
          else getValueForVariable(variable, tail)
      }
    }

    def getValuesForVariables(variables: List[Variable], memory: Memory): List[Value] = {
      variables match {
        case Nil => Nil
        case varHead :: varTail =>
          val value = getValueForVariable(varHead, memory)
          value :: getValuesForVariables(varTail, memory)
      }
    }

    getValuesForVariables(vars, memory)
  }

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param vals
    *   : une liste de valeurs
    * @return
    *   la liste des valeurs des variables de sortie
    */
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
  program match{
    case Progr(in,body,out) =>
      val initialMemory : Memory = interpreterMemorySet(in, vals)
      val finalMemory : Memory = interpreterCommands(body, initialMemory)
      interpreterMemoryGet(out, finalMemory)
  }
} 
}