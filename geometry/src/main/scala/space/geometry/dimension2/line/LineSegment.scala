package space.geometry
package dimension2
package line

import vector._

trait LineSegment {

  def points: (Vector, Vector)

  def toLine: Line

  def length: Double

}