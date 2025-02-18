package interpreter

import interpreter.Interpreter._

object TestsManuels extends App {

  def assertEquals(a: Any, b: Any): Boolean = a == b

  def assertNotEquals(a: Any, b: Any): Boolean = a != b

  def assertTrue(a: Boolean): Boolean = a

  val memory: Memory = List(
    (Var("X"), CstValue("xxx")),
    (Var("Y"), CstValue("yyy")),
    (Var("Z"), NlValue)
  )
// Tests sur les accès à la mémoire

  def Test_lookUp_positive1(): Boolean = {
    assertEquals(CstValue("xxx"), lookUp(Var("X"), memory))
  }
  println("Test_lookUp_positive1 : " + Test_lookUp_positive1())

  def Test_lookUp_positive2(): Boolean = {
    assertEquals(CstValue("yyy"), lookUp(Var("Y"), memory))
  }
  println("Test_lookUp_positive2 : " + Test_lookUp_positive2())

  def Test_lookUp_default1(): Boolean = {
    assertEquals(NlValue, lookUp(Var("X"), Nil))
  }
  println("Test_lookUp_default1 : " + Test_lookUp_default1())

  def Test_lookUp_default2(): Boolean = {
    assertEquals(NlValue, lookUp(Var("W"), memory))
  }
  println("Test_lookUp_default2 : " + Test_lookUp_default2())

  def Test_assign_positive1(): Boolean = {
    val expected = List(
      (Var("X"), CstValue("www")),
      (Var("Y"), CstValue("yyy")),
      (Var("Z"), NlValue)
    )
    val result = assign(Var("X"), CstValue("www"), memory)
    // println("expected : " + expected)
    // println("was      : " + result)

    expected.forall(a => result.contains(a)) && result.forall(a =>
      expected.contains(a)
    ) && expected.length == result.length

  }
  println("Test_assign_positive1 : " + Test_assign_positive1())

  def Test_assign_positive2(): Boolean = {
    val expected = List(
      (Var("X"), CstValue("xxx")),
      (Var("Y"), CstValue("www")),
      (Var("Z"), NlValue)
    )
    val result = assign(Var("Y"), CstValue("www"), memory)
    // println("expected : " + expected)
    // println("was      : " + result)

    expected.forall(a => result.contains(a)) && result.forall(a =>
      expected.contains(a)
    ) && expected.length == result.length

  }
  println("Test_assign_positive2 : " + Test_assign_positive2())

  def Test_assign_default1(): Boolean = {
    val expected = memory ::: List((Var("W"), CstValue("www")))
    val result = assign(Var("W"), CstValue("www"), memory)
    // println("expected : " + expected)
    // println("was      : " + result)

    expected.forall(a => result.contains(a)) && result.forall(a =>
      expected.contains(a)
    ) && expected.length == result.length

  }
  println("Test_assign_default1 : " + Test_assign_default1())

  def Test_assign_default2(): Boolean = {
    assertEquals(
      List((Var("W"), CstValue("www"))),
      assign(Var("W"), CstValue("www"), Nil)
    )
  }
  println("Test_assign_default2 : " + Test_assign_default2())

  // Tests sur les expressions

  val memoryExp: Memory = List(
    (Var("X"), CstValue("xxx")),
    (Var("Y"), ConsValue(CstValue("a-yyy"), CstValue("b-yyy"))),
    (Var("Z"), NlValue)
  )

  def Test_interpreterExpr_Nl(): Boolean = {
    assertEquals(NlValue, interpreterExpr(Nl, memoryExp))
  }
  println("Test_interpreterExpr_Nl : " + Test_interpreterExpr_Nl())

  def Test_interpreterExpr_Cst(): Boolean = {
    assertEquals(CstValue("xxx"), interpreterExpr(Cst("xxx"), memoryExp))
  }
  println("Test_interpreterExpr_Nl : " + Test_interpreterExpr_Nl())

  def Test_interpreterExprVar_presente1(): Boolean = {
    assertEquals(CstValue("xxx"), interpreterExpr(VarExp("X"), memoryExp))
  }
  println(  "Test_interpreterExprVar_presente1 : " + Test_interpreterExprVar_presente1())

  def Test_interpreterExprVar_presente2(): Boolean = {
    assertEquals(
      ConsValue(CstValue("a-yyy"), CstValue("b-yyy")),
      interpreterExpr(VarExp("Y"), memoryExp)
    )
  }
  println("Test_interpreterExprVar_presente2 : " + Test_interpreterExprVar_presente2())

  def Test_interpreterExprVar_presente3(): Boolean = {
    assertEquals(NlValue, interpreterExpr(VarExp("Z"), memoryExp))
  }
  println(
    "Test_interpreterExprVar_presente3 : " + Test_interpreterExprVar_presente3()
  )

  def Test_interpreterExprVar_absente(): Boolean = {
    assertEquals(NlValue, interpreterExpr(VarExp("W"), memoryExp))
  }
  println(
    "Test_interpreterExprVar_absente : " + Test_interpreterExprVar_absente()
  )

  def Test_interpreterExpr_Cons1(): Boolean = {
    assertEquals(
      ConsValue(NlValue, ConsValue(CstValue("a-yyy"), CstValue("b-yyy"))),
      interpreterExpr(Cons(Nl, Cons(Cst("a-yyy"), Cst("b-yyy"))), memoryExp)
    )
  }
  println("Test_interpreterExpr_Cons1 : " + Test_interpreterExpr_Cons1())

  def Test_interpreterExpr_Cons2(): Boolean = {
    assertEquals(
      ConsValue(
        CstValue("xxx"),
        ConsValue(CstValue("a-yyy"), CstValue("b-yyy"))
      ),
      interpreterExpr(Cons(VarExp("X"), VarExp("Y")), memoryExp)
    )
  }
  println("Test_interpreterExpr_Cons2 : " + Test_interpreterExpr_Cons2())

  def Test_interpreterExpr_Hd_OK1(): Boolean = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Hd(Cons(Cst("xxx"), VarExp("Y"))), memoryExp)
    )
  }
  println("Test_interpreterExpr_Hd_OK1 : " + Test_interpreterExpr_Hd_OK1())

  def Test_interpreterExpr_Hd_OK2(): Boolean = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Hd(Cons(VarExp("X"), VarExp("Y"))), memoryExp)
    )
  }
  println("Test_interpreterExpr_Hd_OK2 : " + Test_interpreterExpr_Hd_OK2())

  def Test_interpreterExpr_Hd_OK3(): Boolean = {
    assertEquals(CstValue("a-yyy"), interpreterExpr(Hd(VarExp("Y")), memoryExp))
  }
  println("Test_interpreterExpr_Hd_OK3 : " + Test_interpreterExpr_Hd_OK3())

  def Test_interpreterExpr_Hd_NOK1(): Boolean = {
    assertEquals(NlValue, interpreterExpr(Hd(VarExp("Z")), memoryExp))
  }
  println("Test_interpreterExpr_Hd_NOK1 : " + Test_interpreterExpr_Hd_NOK1())

  def Test_interpreterExpr_Hd_NOK2(): Boolean = {
    assertEquals(NlValue, interpreterExpr(Hd(VarExp("X")), memoryExp))
  }
  println("Test_interpreterExpr_Hd_NOK2 : " + Test_interpreterExpr_Hd_NOK2())

  def Test_interpreterExpr_Tl_OK1(): Boolean = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Tl(Cons(VarExp("Y"), Cst("xxx"))), memoryExp)
    )
  }
  println("Test_interpreterExpr_Tl_OK1 : " + Test_interpreterExpr_Tl_OK1())

  def Test_interpreterExpr_Tl_OK2(): Boolean = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Tl(Cons(VarExp("Y"), VarExp("X"))), memoryExp)
    )
  }
  println("Test_interpreterExpr_Tl_OK2 : " + Test_interpreterExpr_Tl_OK2())

  def Test_interpreterExpr_Tl_OK3(): Boolean = {
    assertEquals(CstValue("b-yyy"), interpreterExpr(Tl(VarExp("Y")), memoryExp))
  }
  println("Test_interpreterExpr_Tl_OK3 : " + Test_interpreterExpr_Tl_OK3())

  def Test_interpreterExpr_Tl_OK4(): Boolean = {
    assertEquals(
      ConsValue(CstValue("a-yyy"), CstValue("b-yyy")),
      interpreterExpr(Tl(Cons(VarExp("X"), VarExp("Y"))), memoryExp)
    )
  }
  println("Test_interpreterExpr_Tl_OK4 : " + Test_interpreterExpr_Tl_OK4())

  def Test_interpreterExpr_Tl_OK5(): Boolean = {
    assertEquals(CstValue("b-yyy"), interpreterExpr(Tl(VarExp("Y")), memoryExp))
  }
  println("Test_interpreterExpr_Tl_OK5 : " + Test_interpreterExpr_Tl_OK5())

  def Test_interpreterExpr_Tl_NOK1(): Boolean = {
    assertEquals(NlValue, interpreterExpr(Tl(VarExp("Z")), memoryExp))
  }
  println("Test_interpreterExpr_Tl_NOK1 : " + Test_interpreterExpr_Tl_NOK1())

  def Test_interpreterExpr_Tl_NOK2(): Boolean = {
    assertEquals(NlValue, interpreterExpr(Tl(VarExp("X")), memoryExp))
  }
  println("Test_interpreterExpr_Tl_NOK2 : " + Test_interpreterExpr_Tl_NOK2())

  def Test_interpreterExpr_Eq_true1(): Boolean = {
    assertNotEquals(NlValue, interpreterExpr(Eq(Nl, Nl), memoryExp))
  }
  println("Test_interpreterExpr_Eq_true1 : " + Test_interpreterExpr_Eq_true1())

  def Test_interpreterExpr_Eq_true2(): Boolean = {
    val e: Expression = Cons(VarExp("X"), VarExp("Y"))
    assertNotEquals(NlValue, interpreterExpr(Eq(VarExp("X"), Hd(e)), memoryExp))
  }
  println("Test_interpreterExpr_Eq_true2 : " + Test_interpreterExpr_Eq_true2())

  def Test_interpreterExpr_Eq_false1(): Boolean = {
    assertEquals(NlValue, interpreterExpr(Eq(Nl, Cst("xxx")), memoryExp))
  }
  println(
    "Test_interpreterExpr_Eq_false1 : " + Test_interpreterExpr_Eq_false1()
  )

  def Test_interpreterExpr_Eq_false2(): Boolean = {
    val e: Expression = Cons(VarExp("X"), VarExp("Y"))
    assertEquals(NlValue, interpreterExpr(Eq(VarExp("Y"), Hd(e)), memoryExp))
  }
  println(
    "Test_interpreterExpr_Eq_false2 : " + Test_interpreterExpr_Eq_false2()
  )

  def Test_interpreterExpr_Expr1(): Boolean = {
    val e: Expression = Cons(Hd(Cons(VarExp("X"), Cst("yyy"))), Tl(VarExp("W")))
    assertEquals(
      ConsValue(CstValue("xxx"), NlValue),
      interpreterExpr(e, memoryExp)
    )
  }
  println("Test_interpreterExpr_Expr1 : " + Test_interpreterExpr_Expr1())

  def Test_interpreterExpr_Expr2(): Boolean = {
    val e: Expression = Hd(
      Hd(Cons(Hd(Cons(VarExp("X"), Cst("yyy"))), Tl(VarExp("W"))))
    )
    assertEquals(NlValue, interpreterExpr(e, memoryExp))
  }
  println("Test_interpreterExpr_Expr2 : " + Test_interpreterExpr_Expr2())

  def Test_valueToExpression_Nl(): Boolean = {
    assertEquals(Nl, valueToExpression(NlValue))
  }
  println("Test_valueToExpression_Nl : " + Test_valueToExpression_Nl())

  def Test_valueToExpression_Cst(): Boolean = {
    assertEquals(Cst("aaa"), valueToExpression(CstValue("aaa")))
  }
  println("Test_valueToExpression_Cst : " + Test_valueToExpression_Cst())

  def Test_valueToExpression_Cons(): Boolean = {
    assertEquals(
      Cons(Cst("aaa"), Nl),
      valueToExpression(ConsValue(CstValue("aaa"), NlValue))
    )
  }
  println("Test_valueToExpression_Cons : " + Test_valueToExpression_Cons())

  def Test_valueToExpression_Value(): Boolean = {
    assertEquals(
      Cons(Cons(Nl, Cst("aaa")), Cons(Nl, Nl)),
      valueToExpression(
        ConsValue(
          ConsValue(NlValue, CstValue("aaa")),
          ConsValue(NlValue, NlValue)
        )
      )
    )
  }
  println("Test_valueToExpression_Value : " + Test_valueToExpression_Value())

  // Tests sur les commandes

  val memoryCom: Memory = List(
    (Var("X"), CstValue("xxx")),
    (Var("Y"), CstValue("yyy")),
    (Var("Z"), NlValue)
  )

  def Test_interpreterCommand_Nop(): Boolean = {
    val expected = memoryCom
    val result = interpreterCommand(Nop, memoryCom)
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_Nop : " + Test_interpreterCommand_Nop())

  def Test_interpreterCommand_Set1(): Boolean = {
    val expected: Memory = List(
      (Var("X"), CstValue("xxx")),
      (Var("Y"), ConsValue(CstValue("yyy"), NlValue)),
      (Var("Z"), NlValue)
    )
    val result =
      interpreterCommand(Set(Var("Y"), Cons(VarExp("Y"), Nl)), memoryCom)
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_Set1 : " + Test_interpreterCommand_Set1())

  def Test_interpreterCommand_Set2(): Boolean = {
    val expected: Memory = List(
      (Var("X"), CstValue("xxx")),
      (Var("Y"), CstValue("yyy")),
      (Var("Z"), NlValue),
      (Var("W"), ConsValue(CstValue("yyy"), NlValue))
    )
    val result =
      interpreterCommand(Set(Var("W"), Cons(VarExp("Y"), Nl)), memoryCom)
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_Set2 : " + Test_interpreterCommand_Set2())

  def Test_interpreterCommands_1(): Boolean = {
    val expected: Memory = memoryCom
    val result = interpreterCommands(List(Nop), memoryCom)
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommands_1 : " + Test_interpreterCommands_1())

  def Test_interpreterCommands_2(): Boolean = {
    val expected: Memory = List(
      (Var("X"), CstValue("xxx")),
      (Var("Y"), CstValue("yyy")),
      (Var("Z"), NlValue),
      (Var("W"), ConsValue(CstValue("yyy"), NlValue))
    )
    val result =
      interpreterCommands(List(Set(Var("W"), Cons(VarExp("Y"), Nl))), memoryCom)
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommands_2 : " + Test_interpreterCommands_2())

  def Test_interpreterCommands_3(): Boolean = {
    val expected: Memory = memoryCom
    val result = interpreterCommands(List(Nop, Nop), memoryCom)
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommands_3 : " + Test_interpreterCommands_3())

  def Test_interpreterCommands_4(): Boolean = {
    val expected: Memory = List(
      (Var("X"), CstValue("yyy")),
      (Var("Y"), CstValue("xxx")),
      (Var("Z"), NlValue),
      (Var("W"), CstValue("xxx"))
    )
    val result = interpreterCommands(
      List(
        Set(Var("W"), VarExp("X")),
        Set(Var("X"), VarExp("Y")),
        Set(Var("Y"), VarExp("W"))
      ),
      memoryCom
    )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommands_4 : " + Test_interpreterCommands_4())

  /*

  def test_interpreterCommands_NIl_5(): Boolean = {
    try {
      interpreterCommands(Nil, memoryCom);
      fail();
    } catch {
      case ExceptionListeVide  => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError => () // Rattrape l'exception matchError levée implicitement
    }
  }
   println("test_interpreterCommands_NIl_5 : " + test_interpreterCommands_NIl_5())

   */

  def Test_interpreterCommand_While_1(): Boolean = {
    val memoryCom1: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (Var("Y"), NlValue)
    )
    val result = interpreterCommand(
      While(
        VarExp("X"),
        List(
          Set(Var("Y"), Cons(Nl, VarExp("Y"))),
          Set(Var("X"), Tl(VarExp("X")))
        )
      ),
      memoryCom1
    )
    val expected: Memory = List(
      (Var("X"), NlValue),
      (
        Var("Y"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      )
    )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_While_1 : " + Test_interpreterCommand_While_1()
  )

  def Test_interpreterCommand_While_2(): Boolean = {
    val expected: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (Var("Y"), NlValue)
    )
    val result =
      interpreterCommand(
        While(
          VarExp("Y"),
          List(
            Set(Var("Y"), Cons(Nl, VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))
          )
        ),
        expected
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_While_2 : " + Test_interpreterCommand_While_2()
  )

  def Test_interpreterCommand_While_3(): Boolean = {
    val memoryCom1: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (Var("Y"), NlValue)
    )
    val expected: Memory = List((Var("X"), NlValue), (Var("Y"), NlValue))
    val result = interpreterCommand(
      While(VarExp("X"), List(Set(Var("X"), Tl(VarExp("X"))))),
      memoryCom1
    )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_While_3 : " + Test_interpreterCommand_While_3()
  )

  def Test_interpreterCommand_While_4(): Boolean = {
    val memoryCom1: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (Var("X"), NlValue),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue))),
      (Var("YY"), NlValue),
      (
        Var("Z"),
        ConsValue(
          NlValue,
          ConsValue(
            NlValue,
            ConsValue(
              NlValue,
              ConsValue(
                NlValue,
                ConsValue(NlValue, ConsValue(NlValue, NlValue))
              )
            )
          )
        )
      )
    )
    val result =
      interpreterCommand(
        While(
          VarExp("X"),
          List(
            Set(Var("YY"), VarExp("Y")),
            While(
              VarExp("YY"),
              List(
                Set(Var("Z"), Cons(Nl, VarExp("Z"))),
                Set(Var("YY"), Tl(VarExp("YY")))
              )
            ),
            Set(Var("X"), Tl(VarExp("X")))
          )
        ),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_While_4 : " + Test_interpreterCommand_While_4()
  )

  def Test_interpreterCommand_For_1(): Boolean = {
    val memoryCom1: Memory = List((Var("X"), NlValue))
    val expected: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      )
    )
    val result =
      interpreterCommand(
        For(
          Cons(Nl, Cons(Nl, Cons(Nl, Nl))),
          List(Set(Var("X"), Cons(Nl, VarExp("X"))))
        ),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_For_1 : " + Test_interpreterCommand_For_1())

  def Test_interpreterCommand_For_2(): Boolean = {
    val memoryCom1: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      )
    )
    val expected: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (
        Var("Y"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      )
    )
    val result =
      interpreterCommand(
        For(VarExp("X"), List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_For_2 : " + Test_interpreterCommand_For_2())

  def Test_interpreterCommand_For_3(): Boolean = {
    val memoryCom1: Memory = List((Var("X"), NlValue))
    val expected: Memory = List((Var("X"), NlValue))
    val result =
      interpreterCommand(
        For(VarExp("X"), List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_For_3 : " + Test_interpreterCommand_For_3())

  def Test_interpreterCommand_For_4(): Boolean = {
    val memoryCom1: Memory = List((Var("X"), CstValue("xxx")))
    val expected: Memory =
      List((Var("X"), CstValue("xxx")), (Var("Y"), ConsValue(NlValue, NlValue)))
    val result =
      interpreterCommand(
        For(VarExp("X"), List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_For_4 : " + Test_interpreterCommand_For_4())

  def Test_interpreterCommand_For_5(): Boolean = {
    val memoryCom1: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (
        Var("X"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      ),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue))),
      (
        Var("Z"),
        ConsValue(
          NlValue,
          ConsValue(
            NlValue,
            ConsValue(
              NlValue,
              ConsValue(
                NlValue,
                ConsValue(NlValue, ConsValue(NlValue, NlValue))
              )
            )
          )
        )
      )
    )
    val result =
      interpreterCommand(
        For(
          VarExp("X"),
          List(For(VarExp("Y"), List(Set(Var("Z"), Cons(Nl, VarExp("Z"))))))
        ),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterCommand_For_5 : " + Test_interpreterCommand_For_5())

  def Test_interpreterCommand_For_6(): Boolean = {
    val memoryCom1: Memory = List(
      (Var("X"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (
        Var("X"),
        ConsValue(
          NlValue,
          ConsValue(
            NlValue,
            ConsValue(
              NlValue,
              ConsValue(
                NlValue,
                ConsValue(
                  NlValue,
                  ConsValue(
                    NlValue,
                    ConsValue(NlValue, ConsValue(NlValue, NlValue))
                  )
                )
              )
            )
          )
        )
      )
    )
    val result =
      interpreterCommand(
        For(
          VarExp("X"),
          List(For(VarExp("X"), List(Set(Var("X"), Cons(Nl, VarExp("X"))))))
        ),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && result.length == expected.length
    )
  }
  println("Test_interpreterCommand_For_6 : " + Test_interpreterCommand_For_6())

  def Test_interpreterCommand_If_then1(): Boolean = {
    val memoryCom1: Memory = List(
      (Var("X"), ConsValue(NlValue, NlValue)),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (Var("X"), ConsValue(NlValue, ConsValue(NlValue, NlValue))),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val result = interpreterCommand(
      If(
        Eq(VarExp("X"), Cons(Nl, Nl)),
        List(Set(Var("X"), Cons(Nl, VarExp("X")))),
        List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))
      ),
      memoryCom1
    )
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_If_then1 : " + Test_interpreterCommand_If_then1()
  )

  def Test_interpreterCommand_If_then2(): Boolean = {
    val memoryCom1: Memory = List(
      (Var("X"), ConsValue(NlValue, NlValue)),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (Var("X"), ConsValue(NlValue, ConsValue(NlValue, NlValue))),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val result = interpreterCommand(
      If(
        Eq(VarExp("X"), VarExp("X")),
        List(Set(Var("X"), Cons(Nl, VarExp("X")))),
        List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))
      ),
      memoryCom1
    )
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_If_then2 : " + Test_interpreterCommand_If_then2()
  )

  def Test_interpreterCommand_If_else1(): Boolean = {
    val memoryCom1: Memory = List(
      (Var("X"), ConsValue(NlValue, NlValue)),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (Var("X"), ConsValue(NlValue, NlValue)),
      (
        Var("Y"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      )
    )
    val result =
      interpreterCommand(
        If(
          Eq(VarExp("X"), Cons(Nl, Cons(Nl, Nl))),
          List(Set(Var("X"), Cons(Nl, VarExp("X")))),
          List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))
        ),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_If_else1 : " + Test_interpreterCommand_If_else1()
  )

  def Test_interpreterCommand_If_else2(): Boolean = {
    val memoryCom1: Memory = List(
      (Var("X"), ConsValue(NlValue, NlValue)),
      (Var("Y"), ConsValue(NlValue, ConsValue(NlValue, NlValue)))
    )
    val expected: Memory = List(
      (Var("X"), ConsValue(NlValue, NlValue)),
      (
        Var("Y"),
        ConsValue(NlValue, ConsValue(NlValue, ConsValue(NlValue, NlValue)))
      )
    )
    val result =
      interpreterCommand(
        If(
          Eq(VarExp("X"), VarExp("Y")),
          List(Set(Var("X"), Cons(Nl, VarExp("X")))),
          List(Set(Var("Y"), Cons(Nl, VarExp("Y"))))
        ),
        memoryCom1
      )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println(
    "Test_interpreterCommand_If_else2 : " + Test_interpreterCommand_If_else2()
  )

  def Test_interpreterMemorySet_1(): Boolean = {
    val expected = List((Var("X"), CstValue("xxx")))
    val result = interpreterMemorySet(List(Var("X")), List(CstValue("xxx")))
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterMemorySet_1 : " + Test_interpreterMemorySet_1())

  def Test_interpreterMemorySet_2(): Boolean = {
    val expected = List(
      (Var("Z"), NlValue),
      (Var("Y"), CstValue("yyy")),
      (Var("X"), CstValue("xxx"))
    )
    val result = interpreterMemorySet(
      List(Var("X"), Var("Y"), Var("Z")),
      List(CstValue("xxx"), CstValue("yyy"), NlValue)
    )
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(
      expected.forall(a => result.contains(a)) && result.forall(a =>
        expected.contains(a)
      ) && expected.length == result.length
    )
  }
  println("Test_interpreterMemorySet_2 : " + Test_interpreterMemorySet_2())
  /*

  def test_interpreterMemorySet__3(): Boolean = {
    try {
      interpreterMemorySet(Nil,Nil);
      fail();
    } catch {
      case ExceptionListeVide => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  println("test_interpreterMemorySet__3 : " + test_interpreterMemorySet__3())


  def test_interpreterMemorySet__4(): Boolean = {
    try {
      interpreterMemorySet(List(Var("X"), Var("Y")),List(CstValue("xxx"), CstValue("yyy"), NlValue));
      fail();
    } catch {
      case ExceptionListesDeLongueursDifferentes => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  println("test_interpreterMemorySet__4 : " + test_interpreterMemorySet__4())


  def test_interpreterMemorySet__5(): Boolean = {
    try {
      interpreterMemorySet(Nil,List(CstValue("xxx"), CstValue("yyy"), NlValue));
      fail();
    } catch {
      case ExceptionListesDeLongueursDifferentes => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  println("test_interpreterMemorySet__5 : " + test_interpreterMemorySet__5())


  def test_interpreterMemorySet__6(): Boolean = {
    try {
      interpreterMemorySet(List(Var("X"), Var("Y"), Var("Z")),Nil);
      fail();
    } catch {
      case ExceptionListesDeLongueursDifferentes => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  println("test_interpreterMemorySet__6 : " + test_interpreterMemorySet__6())
   */

  def Test_interpreterMemoryGet_1(): Boolean = {
    assertEquals(
      List(CstValue("xxx")),
      interpreterMemoryGet(
        List(Var("X")),
        List(
          (Var("Z"), NlValue),
          (Var("Y"), CstValue("yyy")),
          (Var("X"), CstValue("xxx"))
        )
      )
    )
  }
  println("Test_interpreterMemoryGet_1 : " + Test_interpreterMemoryGet_1())

  def Test_interpreterMemoryGet_2(): Boolean = {
    assertEquals(
      List(CstValue("xxx"), NlValue),
      interpreterMemoryGet(
        List(Var("X"), Var("Z")),
        List(
          (Var("Z"), NlValue),
          (Var("Y"), CstValue("yyy")),
          (Var("X"), CstValue("xxx"))
        )
      )
    )
  }
  println("Test_interpreterMemoryGet_2 : " + Test_interpreterMemoryGet_2())

  /*

  def test_interpreterMemoryGet__3(): Boolean = {
    try {
      interpreterMemoryGet(Nil,Nil);
      fail();
    } catch {
      case ExceptionListeVide => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  println("test_interpreterMemoryGet__3 : " + test_interpreterMemoryGet__3())
   */

  def Test_interpreter(): Boolean = {
    val reverse: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))
            )
          )
        ),
        List(Var("Y"))
      )
    assertEquals(
      List(
        ConsValue(
          CstValue("ddd"),
          ConsValue(
            CstValue("ccc"),
            ConsValue(CstValue("bbb"), ConsValue(CstValue("aaa"), NlValue))
          )
        )
      ),
      interpreter(
        reverse,
        List(
          ConsValue(
            CstValue("aaa"),
            ConsValue(
              CstValue("bbb"),
              ConsValue(CstValue("ccc"), ConsValue(CstValue("ddd"), NlValue))
            )
          )
        )
      )
    )
  }
  println("Test_interpreter : " + Test_interpreter())

}
