package eu.jameshamilton

class Program(val function: FunctionDef)

class FunctionDef(val name: Token, val body: List<Statement>)

sealed class Statement

class ReturnStatement(val value: Expression) : Statement()

sealed class Expression

class Constant(val value: Int) : Expression()
