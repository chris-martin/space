package space.geometry
package dimension2
package line

import vector._
import angle._

trait Line {

  def point: Vector

  def angle: SemicircleRadians

}