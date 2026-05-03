// Outside Sources:
// Fingrid API documentation: https://data.fingrid.fi/instructions
// And other spefific Fingrid dataset documentation

// This file handles the energy source models for the system.
// It Defines the types and structures used across all other files.

object EnergySource {

  // SourceType: sealed trait representing the three renewable energy sources in the plant.
  sealed trait SourceType
  case object Solar extends SourceType
  case object Wind  extends SourceType
  case object Hydro extends SourceType

  // Fingrid JSON structure (same for all 3 sources):
  //   "datasetId": 191,
  //   "startTime": "2026-05-02T15:03:00.000Z",
  //   "endTime":   "2026-05-02T15:06:00.000Z",
  //   "value":     749.674
  // Unit is MW for Wind and Hydro, MWh/h for Solar.

 case class EnergyReading(
                     source:       SourceType,
                     startTimeUtc: String,
                     endTimeUtc:   String,
                     value:        Double)

  // datasetIds: immutable Map linking each source type
  // to its Fingrid dataset ID used in API calls.
  // Solar = 248, Wind = 181, Hydro = 191
  val datasetIds: Map[SourceType, Int] = Map(
    Wind  -> 245, // Tuulivoimatuotanto - varttitieto (15 min)
    Hydro -> 191, // Vesivoimatuotanto - reaaliaikatieto (3 min, sampled to 15 min)
    Solar -> 248  // Aurinkovoiman tuotantoennuste (Prediction 15min)
  )

  val allSources: List[SourceType] = List(Wind, Hydro, Solar)

  def label(source: SourceType): String = source match {
    case Solar => "Solar"
    case Wind  => "Wind"
    case Hydro => "Hydro"
  }

  def parseSource(s: String): Option[SourceType] = s.toLowerCase match {
    case "solar" => Some(Solar)
    case "wind"  => Some(Wind)
    case "hydro" => Some(Hydro)
    case _       => None
  }
}