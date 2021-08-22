package com.softwaremill.macmemo

import scala.annotation.StaticAnnotation
import scala.concurrent.duration._
import scala.language.experimental.macros

class memoize(maxSize: => Long, expiresAfter: => Duration, concurrencyLevel: => Option[Int] = None)
  extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro memoizeMacro.impl
}

/**
 *
 * @param ttl cache ttl, default is 100 years
 * @param maxSize cache item max count
 * @param concurrencyLevel concurrency count
 * @param cacheBuilder one implicit value of type MemoCacheBuilder
 */
class OBPMemoize(ttl: => Duration = 100 * 365 days,
                 maxSize: => Long = Long.MaxValue,
                 concurrencyLevel: => Option[Int] = None)
                (implicit val cacheBuilder: MemoCacheBuilder)
  extends memoize(maxSize, ttl, concurrencyLevel)
