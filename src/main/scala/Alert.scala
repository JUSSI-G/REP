// AI STATEMENT: Claude used for learning, ideation, structure and debugging. All implementation is our own.

import EnergySource._
// This file handles detecting and reporting issues with energy sources.
object Alert {

  // If a reading drops below these values an alert is generated.
  private val lowOutputThreshold = 50.0   // MW
  private val dropThreshold      = 0.2    // 80% drop between consecutive readings

  case class Alert(source: SourceType, timestamp: String, value: Double, message: String)

  // Checks for readings below the low output threshold.
  // Returns alerts for low readings excluding solar because night readings are normally 0.
  private def checkLowOutput(readings: List[EnergyReading]): List[Alert] =
    readings
      .filter(r => r.source != Solar && r.value < lowOutputThreshold)
      .map(r => Alert(
        source    = r.source,
        timestamp = r.startTimeUtc,
        value     = r.value,
        message   = s"${label(r.source)}: ${r.value} MW is below threshold of $lowOutputThreshold MW."
      ))

  // Checks for sudden drops between consecutive readings.
  // A sudden drop is when a reading is less than 80% of the previous reading.
  // Solar is excluded because sunset causes a natural daily drop that is not a malfunction.
  private def drop(readings: List[EnergyReading]): List[Alert] = {
    def recurs(remaining: List[EnergyReading], acc: List[Alert]): List[Alert] =
      remaining match {
        case Nil          => acc
        case _ :: Nil     => acc
        case a :: b :: rest =>
          if (b.source != Solar && a.value > 0 && b.value < a.value * dropThreshold) {
            val alert = Alert(
              source    = b.source,
              timestamp = b.startTimeUtc,
              value     = b.value,
              message   = s"Drop detected for ${label(b.source)}: ${b.value} MW from ${a.value} MW."
            )
            recurs(b :: rest, alert :: acc)
          } else {
            recurs(b :: rest, acc)
          }
      }
    recurs(readings, List.empty)
  }

  // Runs all checks on all readings and prints alerts.
  def checkAll(readings: List[EnergyReading]): Unit = {
    if (readings.isEmpty) {
      println("No data available. Try fetching first.")
    } else {
      println("\nRunning alert checks...")

      val alerts = allSources.flatMap { source =>
        val sourceReadings = readings.filter(_.source == source)
        val sorted         = sourceReadings.sortBy(_.startTimeUtc)
        checkLowOutput(sorted) ++ drop(sorted)
      }

      if (alerts.isEmpty) {
        println("No alerts detected. All systems operating normally.")
      } else {
        println(s"${alerts.length} alert(s) detected:\n")
        alerts.foreach(a => println(s"  [${label(a.source)}] ${a.timestamp} - ${a.message}"))
      }
    }
  }
}