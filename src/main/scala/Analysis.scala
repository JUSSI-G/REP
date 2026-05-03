//AI STATEMENT: Claude used for ideation, structure and debugging. All implementation is our own.
//Outside Source:
//Fingrid documents
//Java API, LocalDate: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
//Java API, WeekFields: https://docs.oracle.com/javase/8/docs/api/java/time/temporal/WeekFields.html

// This file handles all data analysis for the REPS system.

import EnergySource._

// FilterResult[+A]: covariant generic wrapper for filter outcomes.
// Demonstrates type parameterization with variance (Lecture 5a).
sealed trait FilterResult[+A]
case class FilterSuccess[+A](data: List[A]) extends FilterResult[A]
case class FilterFailure(message: String)   extends FilterResult[Nothing]

object Analysis {

  // Extracting parts from a timestamp string
  // "2024-01-15T10:00:00Z" to "2024-01-15"
  private def extractDate(timestamp: String): String =
    timestamp.take(10)

  // "2024-01-15T10:00:00Z" to "10"
  private def extractHour(timestamp: String): String =
    timestamp.drop(11).take(2)

  // "2024-01-15T10:00:00Z" to "2024-01"
  private def extractMonth(timestamp: String): String =
    timestamp.take(7)

  // "2024-01-15T10:00:00Z" to "2026-W02"
  private def extractWeek(timestamp: String): String = {
    val date = java.time.LocalDate.parse(extractDate(timestamp))
    val week = date.get(java.time.temporal.WeekFields.ISO.weekOfWeekBasedYear())
    s"${date.getYear}-W$week"
  }

  // Filtering
  def filterByHour(readings: List[EnergyReading], date: String, hour: String): List[EnergyReading] =
    readings.filter(r => extractDate(r.startTimeUtc) == date && extractHour(r.startTimeUtc) == hour)

  def filterByDay(readings: List[EnergyReading], date: String): List[EnergyReading] =
    readings.filter(r => extractDate(r.startTimeUtc) == date)

  def filterByWeek(readings: List[EnergyReading], week: String): List[EnergyReading] =
    readings.filter(r => extractWeek(r.startTimeUtc) == week)

  def filterByMonth(readings: List[EnergyReading], month: String): List[EnergyReading] =
    readings.filter(r => extractMonth(r.startTimeUtc) == month)

  def filterBySource(readings: List[EnergyReading], source: SourceType): List[EnergyReading] =
    readings.filter(_.source == source)

  // Sorting
  def sortByValueAsc(readings: List[EnergyReading]): List[EnergyReading] =
    readings.sortBy(_.value)

  def sortByValueDesc(readings: List[EnergyReading]): List[EnergyReading] =
    readings.sortBy(_.value).reverse

  def sortByTime(readings: List[EnergyReading]): List[EnergyReading] =
    readings.sortBy(_.startTimeUtc)

  // Search
  def searchBySource(readings: List[EnergyReading], sourceStr: String): Either[String, List[EnergyReading]] =
    parseSource(sourceStr) match {
      case None => Left(s"Unknown source: $sourceStr. Use Solar, Wind or Hydro.")
      case Some(source) =>
        val results = filterBySource(readings, source)
        if (results.isEmpty) Left(s"No readings found for $sourceStr.")
        else Right(results)
    }

  def searchByDate(readings: List[EnergyReading], date: String): Either[String, List[EnergyReading]] = {
    val results = filterByDay(readings, date)
    if (results.isEmpty) Left(s"No readings found for date: $date. Please choose a date within the last 90 days.")
    else Right(results)
  }

  //checks if the date is in DD/MM/YYYY format and converts it to YYYY-MM-DD for internal use.
  private def validateAndConvertDate(input: String): Either[String, String] = {
    val pattern = """(\d{2})/(\d{2})/(\d{4})""".r
    input match {
      case pattern(day, month, year) =>
        Right(s"$year-$month-$day")
      case _ =>
        Left(
          s"Invalid date format. Please enter the date in the format 'DD/MM/YYYY'.\n" +
            s"For example, enter '12/04/2024' for April 12, 2024.\n" +
            s"Note: Only data from the last 90 days is available."
        )
    }
  }

  // Statistics. all take a List[Double] and return Option[Double]
  // None if the list is empty

  def mean(values: List[Double]): Option[Double] =
    if (values.isEmpty) None
    else Some(values.sum / values.length)

  def median(values: List[Double]): Option[Double] =
    if (values.isEmpty) None
    else {
      val sorted = values.sorted
      val mid    = sorted.length / 2
      if (sorted.length % 2 == 0) Some((sorted(mid - 1) + sorted(mid)) / 2.0)
      else Some(sorted(mid))
    }

  def mode(values: List[Double]): Option[Double] =
    if (values.isEmpty) None
    else {
      val grouped = values.groupBy(identity).map { case (v, vs) => (v, vs.length) }
      Some(grouped.maxBy(_._2)._1)
    }

  def range(values: List[Double]): Option[Double] =
    if (values.isEmpty) None
    else Some(values.max - values.min)

  def midrange(values: List[Double]): Option[Double] =
    if (values.isEmpty) None
    else Some((values.min + values.max) / 2.0)

  //Currying used: first takes the stat function, then the readings.
  def computeStat(stat: List[Double] => Option[Double])(readings: List[EnergyReading]): Option[Double] =
    stat(readings.map(_.value))

  // applyFilter: generic higher-order function using FilterResult[+A].
  // Demonstrates type parameterization + HOF together.
  def applyFilter[A](items: List[A], predicate: A => Boolean, errMsg: String): FilterResult[A] = {
    val results = items.filter(predicate)
    if (results.isEmpty) FilterFailure(errMsg)
    else FilterSuccess(results)
  }

  // Prints statistics for a list of readings
  private def showStatistics(readings: List[EnergyReading]): Unit = {
    if (readings.isEmpty) {
      println("No data available for statistics.")
    } else {
      println(s"\nStatistics for ${readings.length} readings:")
      computeStat(mean)(readings).foreach(v     => println(s"  Mean     : $v MW"))
      computeStat(median)(readings).foreach(v   => println(s"  Median   : $v MW"))
      computeStat(mode)(readings).foreach(v     => println(s"  Mode     : $v MW"))
      computeStat(range)(readings).foreach(v    => println(s"  Range    : $v MW"))
      computeStat(midrange)(readings).foreach(v => println(s"  Midrange : $v MW"))
    }
  }

  // Prints a list of readings
  private def printResults(readings: List[EnergyReading]): Unit = {
    if (readings.isEmpty) println("No results found.")
    else {
      println(s"\nFound ${readings.length} readings:")
      readings.foreach(r => println(s"  ${label(r.source)} | ${r.startTimeUtc} | ${r.value} MW"))
    }
  }

  // Analysis menu. Called from Main.scala
  def analysisMenu(readings: List[EnergyReading]): Unit = {
    println("\nAnalysis Menu:")
    println("  1. Filter by hour")
    println("  2. Filter by day")
    println("  3. Filter by week")
    println("  4. Filter by month")
    println("  5. Filter by source")
    println("  6. Sort by value ascending")
    println("  7. Sort by value descending")
    println("  8. Sort by time")
    println("  9. Search by source")
    println("  10. Search by date")
    println("  11. Show statistics")
    println("  12. Statistics per source")
    println("  0. Back")
    print("Enter choice: ")

    scala.io.StdIn.readLine().trim match {
      case "1" =>
        print("Enter date (DD/MM/YYYY): ")
        val input = scala.io.StdIn.readLine().trim
        validateAndConvertDate(input) match {
          case Left(err) => println(err)
          case Right(date) =>
            print("Enter hour (00-23): ")
            val hour = scala.io.StdIn.readLine().trim
            val results = filterByHour(readings, date, hour)
            if (results.isEmpty) println("Check the input format/date and try again. Please choose a date within the last 90 days.")
            else printResults(results)
        }
        analysisMenu(readings)

      case "2" =>
        print("Enter date (DD/MM/YYYY): ")
        val input = scala.io.StdIn.readLine().trim
        validateAndConvertDate(input) match {
          case Left(err) => println(err)
          case Right(date) =>
            val results = filterByDay(readings, date)
            if (results.isEmpty) println("Check the input format/date and try again. Please choose a date within the last 90 days.")
            else printResults(results)
        }
        analysisMenu(readings)

      case "3" =>
        print("Enter week (Example: 2026-W02): ")
        val week = scala.io.StdIn.readLine().trim
        val results = filterByWeek(readings, week)
        if (results.isEmpty) println("Check the input format/date and try again. Please choose a date within the last 90 days.")
        else printResults(results)
        analysisMenu(readings)

      case "4" =>
        print("Enter month (MM/YYYY): ")
        val input = scala.io.StdIn.readLine().trim
        val pattern = """(\d{2})/(\d{4})""".r
        input match {
          case pattern(month, year) =>
            val results = filterByMonth(readings, s"$year-$month")
            if (results.isEmpty) println("No available data for the selected month. Please choose a month within the last 90 days.")
            else printResults(results)
          case _ =>
            println("Invalid month format. Please enter the month in the format 'MM/YYYY'.\nFor example, enter '01/2024' for January 2024.\nNote: Only data from the last 90 days is available.")
        }
        analysisMenu(readings)

      case "5" =>
        print("Enter source (Solar, Wind, Hydro): ")
        val sourceStr = scala.io.StdIn.readLine().trim
        parseSource(sourceStr) match {
          case None => println(s"Unknown source: $sourceStr. Use Solar, Wind or Hydro.")
          case Some(source) =>
            applyFilter(readings, (r: EnergyReading) => r.source == source, s"No readings found for $sourceStr.") match {
              case FilterSuccess(results) => printResults(results)
              case FilterFailure(msg)     => println(msg)
            }
        }
        analysisMenu(readings)

      case "6" =>
        printResults(sortByValueAsc(readings))
        analysisMenu(readings)

      case "7" =>
        printResults(sortByValueDesc(readings))
        analysisMenu(readings)

      case "8" =>
        printResults(sortByTime(readings))
        analysisMenu(readings)

      case "9" =>
        print("Enter source (Solar, Wind, Hydro): ")
        val sourceStr = scala.io.StdIn.readLine().trim
        searchBySource(readings, sourceStr) match {
          case Left(err) => println(err)
          case Right(results) => printResults(results)
        }
        analysisMenu(readings)

      case "10" =>
        print("Enter date (DD/MM/YYYY): ")
        val input = scala.io.StdIn.readLine().trim
        validateAndConvertDate(input) match {
          case Left(err) => println(err)
          case Right(date) =>
            searchByDate(readings, date) match {
              case Left(err) => println(err)
              case Right(results) => printResults(results)
            }
        }
        analysisMenu(readings)

      case "11" =>
        showStatistics(readings)
        analysisMenu(readings)

      case "12" =>
        allSources.foreach { source =>
          val sr = readings.filter(_.source == source)
          if (sr.nonEmpty) {
            println(s"\nStatistics for ${label(source)} (${sr.length} readings):")
            computeStat(mean)(sr).foreach(v     => println(s"  Mean     : $v MW"))
            computeStat(median)(sr).foreach(v   => println(s"  Median   : $v MW"))
            computeStat(mode)(sr).foreach(v     => println(s"  Mode     : $v MW"))
            computeStat(range)(sr).foreach(v    => println(s"  Range    : $v MW"))
            computeStat(midrange)(sr).foreach(v => println(s"  Midrange : $v MW"))
          }
        }
        analysisMenu(readings)

      case "0" =>
        println("Returning to main menu.")

      case _ =>
        println("Invalid choice. Please try again.")
        analysisMenu(readings)
    }
  }
}