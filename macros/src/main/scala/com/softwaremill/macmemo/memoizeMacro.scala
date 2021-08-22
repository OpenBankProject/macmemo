package com.softwaremill.macmemo

import scala.reflect.macros._

object memoizeMacro {
  private val debug = new Debug()

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    case class MacroArgs(maxSize: Tree, expireAfter: Tree, concurrencyLevel: Tree = q"None")

    case class MemoIdentifier(methodName: TermName, generatedMemoValName: TermName)

    def reportInvalidAnnotationTarget(): Unit = {
      c.error(c.enclosingPosition, "This annotation can only be used on methods")
    }

    def prepareInjectedBody(cachedMethodId: MemoIdentifier, valDefs: List[List[ValDef]], bodyTree: Tree, returnTypeTree: Tree): c.type#Tree = {
      val names = valDefs.flatten.map(_.name)
      q"""
      def callRealBody(): $returnTypeTree = { $bodyTree }
      if (System.getProperty("macmemo.disable") != null) {
        callRealBody()
      }
      else {
        ${cachedMethodId.generatedMemoValName}.get($names, callRealBody())
      }"""
    }

    def createMemoVal(cachedMethodId: MemoIdentifier, returnTypeTree: Tree, macroArgs: MacroArgs): c.type#Tree = {

      def buildCacheBucketId: Tree = {
        val enclosingClassSymbol = c.internal.enclosingOwner
        val enclosureFullName = enclosingClassSymbol.fullName + (if (enclosingClassSymbol.isModuleClass) "$." else ".")
        Literal(Constant(
           enclosureFullName + cachedMethodId.methodName.toString))
      }

      def buildParams: Tree = {
        val maxSize = macroArgs.maxSize
        val ttl = macroArgs.expireAfter
        val concurrencyLevelOpt = macroArgs.concurrencyLevel
        q"""com.softwaremill.macmemo.MemoizeParams($maxSize, $ttl, $concurrencyLevelOpt)"""
      }

      q"""lazy val ${cachedMethodId.generatedMemoValName}: com.softwaremill.macmemo.Cache[$returnTypeTree] =
         com.softwaremill.macmemo.BuilderResolver.resolve($buildCacheBucketId).build[$returnTypeTree]($buildCacheBucketId, $buildParams)"""

    }

    def injectCacheUsage(cachedMethodId: MemoIdentifier, function: DefDef) = {
      val DefDef(mods, name, tparams, valDefs, returnTypeTree, bodyTree) = function
      val injectedBody = prepareInjectedBody(cachedMethodId, valDefs, bodyTree, returnTypeTree)
      DefDef(mods, name, tparams, valDefs, returnTypeTree, injectedBody)
    }

    def extractMacroArgs(application: Tree, annotation: Tree) = {
      debug(s"RAW application = ${reflect.runtime.universe.showRaw(application)}")
      val argsTree = application.children.head.children.head.children

      val indexes = annotation match {
        case q"new OBPMemoize(..$_)" => Array(2,1,3)
        case q"new memoize(..$_)" => Array(1, 2 ,3)
        case _ =>
          debug(s"annotation $annotation not change annotation param order")
          Array(1, 2 ,3)
      }

      val maxSize = argsTree.lift(indexes(0)) match {
        case Some(q"$_ = $maxSizeExp") => q"$maxSizeExp"
        case Some(v) => v
        case _ => q"Long.MaxValue"
      }
      val ttl = argsTree.lift(indexes(1)) match {
        case Some(q"$_ = $expiresAfterExp") => q"import scala.concurrent.duration._; $expiresAfterExp"
        case Some(v) => q"import scala.concurrent.duration._; $v"
        case _ => q"import scala.concurrent.duration._; 100 * 365 days"
      }
      val concurrencyLevelOpt = argsTree.lift(indexes(2)) match {
        case Some(q"concurrencyLevel=$v") => q"$v"
        case Some(v) => v
        case _ => q"None"
      }

      debug(s"maxSize arg : $maxSize")
      debug(s"ttl arg : $ttl")
      debug(s"concurrencyLevelOpt arg : $concurrencyLevelOpt")

      val args = MacroArgs(maxSize, ttl, concurrencyLevelOpt)
      debug(s"Macro args: $args")
      args
    }
    debug(s"annottees, mirror = ${c.mirror}")
    debug(s"annottees, prefix = ${c.prefix.tree}")
    debug(s"annottees, mirror = ${c.universe}")
    debug(s"annottees, internal = ${c.internal}")
    debug(s"annottees, PATTERNmode = ${c.PATTERNmode}")
    debug(s"annottees, TERMmode = ${c.TERMmode}")
    debug(s"annottees, TYPEmode = ${c.TYPEmode}")
    val inputs = annottees.map(_.tree).toList
    val (_, expandees) = inputs match {
      case (functionDefinition: DefDef) :: rest =>
        debug(s"Found annotated function [${functionDefinition.name}]")
        val DefDef(_, name: TermName, _, _, returnTypeTree, _) = functionDefinition
        val cachedMethodIdentifier = MemoIdentifier(name, TermName(c.freshName(s"memo_${name}_")))
        val macroArgs = extractMacroArgs(c.macroApplication, c.prefix.tree)
        val memoVal = createMemoVal(cachedMethodIdentifier, returnTypeTree, macroArgs)
        val newFunctionDef = injectCacheUsage(cachedMethodIdentifier, functionDefinition)
        (functionDefinition, (newFunctionDef :: rest) :+ memoVal)
      case _ => reportInvalidAnnotationTarget(); (EmptyTree, inputs)
    }

    debug(s"final method = ${show(expandees)}")

    c.Expr[Any](Block(expandees, Literal(Constant(()))))
  }

}

