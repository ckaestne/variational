package variational.stlc

import variational._

/**
 * Variational terms of the simply-typed lambda calculus.
 *
 * @author Tillmann Rendel
 */

trait Term extends VariationalContainer[Term.Structure, Term] {
  def newChoice(condition : Int, that : Term) =
    new Choice(condition, this, that) with ChoiceContainer[Term.Structure, Term] with Term
}

object Term {
  implicit def liftTerm : Lift[Structure, Term] = new Lift[Structure, Term] {
    def apply(value : Structure) =
      value
  }

  trait Structure extends Term with StructureLike[Term] with SelfContainer[Structure, Term]

  class Var(val identifier : String) extends Structure with Leaf[Term] {
    override def toString = prefix + "(\"" + identifier + "\")"
    def prefix = "Var"
  }

  object Var {
    def apply(identifier : String) : Term =
      new Var(identifier)
  }

  class App(val operator : Term, val operand : Term) extends Structure {
    def all(f : VFunction1) =
      App(f(operator), f(operand))

    def all2(f : VFunction2, that : Term) =
      if (that.isInstanceOf[App]) {
        val other = that.asInstanceOf[App]
        Some(App(f(operator, other.operator), f(operand, other.operand)))
      } else {
        None
      }

    def children = Seq(operator, operand)

    override def toString =
      prefix + "(" + operator + ", " + operand + ")"

    def prefix = "App"
  }

  object App {
    def apply(operator : Term, operand : Term) : Term =
      new App(operator, operand)
  }

  class Abs(val identifier : String, val qualifier : Type, val body : Term) extends Structure {
    def all(f : VFunction1) =
      Abs(identifier, f(qualifier), f(body))

    def all2(f : VFunction2, that : Abs#This) =
      if (that.isInstanceOf[Abs]) {
        val other = that.asInstanceOf[Abs]
        Some(Abs(identifier, f(qualifier, other.qualifier), f(body, other.body)))
      } else {
        None
      }

    def children = Seq(qualifier, body)

    override def toString =
      prefix + "(" + identifier + ", " + qualifier + ", " + body + ")"

    def prefix = "Abs"
  }

  object Abs {
    def apply(identifier : String, qualifier : Type, body : Term) : Term =
      new Abs(identifier, qualifier, body)
  }
}