// Used AI (Claude) to help structure the display functions. All final implementation is our own.
// This file handles all console output and views for the system.

import EnergySource._

object Display {

  def showSummary(readings: List[EnergyReading]): Unit = {
    if (readings.isEmpty) {
      println("No data available. Try fetching the data first.")
    } else {
      println("\nREP Summary")
      allSources.foreach { source =>
        val sourceReadings = readings.filter(_.source == source)
        if (sourceReadings.nonEmpty) {
          val values = sourceReadings.map(_.value)
          println(s"\n${label(source)}:")
          println(s"  Readings : ${sourceReadings.length}")
          println(s"  Latest   : ${sourceReadings.last.value} MW")
          println(s"  Highest  : ${values.max} MW")
          println(s"  Lowest   : ${values.min} MW")
        }
      }
    }
  }
}