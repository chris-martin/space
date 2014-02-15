package space.geometry
package dimension2

trait Polygon {

  def arbitraryPath: Cycle

  def area: Double

  def perimeterLength: Double = arbitraryPath.length
}
