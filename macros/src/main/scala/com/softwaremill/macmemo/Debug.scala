package com.softwaremill.macmemo

/**
 * Taken from MacWire https://github.com/adamw/macwire
 */
private[macmemo] class Debug {
  var ident = 0

  def apply(msg: => String): Unit = {
    if (enabled) {
      val prefix = "   " * ident
      println(s"$prefix[debug] $msg")
    }
  }

  def withBlock[T](msg: => String)(block: => T): T = {
    apply(msg)
    beginBlock()
    try {
      block
    } finally {
      endBlock()
    }
  }

  def beginBlock(): Unit = {
    ident += 1
  }

  def endBlock(): Unit = {
    ident -= 1
  }

  private val enabled =
    List(System.getProperty("macmemo.debug"),System.getProperty("macmemo_debug"),
      System.getenv("macmemo.debug"), System.getenv("macmemo_debug"))
    .exists(null !=)
}