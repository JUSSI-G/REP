// Outside Sources:
// Fingrid API documentation: https://data.fingrid.fi/instructions
// And other specific Fingrid dataset documentation
// The Scala Toolkit: How to send a request? sttp guide: https://docs.scala-lang.org/toolkit/http-client-request.html
// File IO in Scala - Baeldung: https://www.baeldung.com/scala/file-io
// Used AI (Claude) to solve problems with API, Dates, and files. Also output formatting. Needed help to figure problems with Hydro API call. All final implementation is our own.

// This file handles fetching data from the Fingrid API
// and saving/reading it to/from CSV files.

import EnergySource._
import sttp.client4.quick.*
import sttp.client4.Response
import scala.io.Source
import java.io.{File, PrintWriter}

object FileHandling {

  private val dataDir   = "data"
  private val endTime   = java.time.Instant.now().toString
  private val midTime   = java.time.Instant.now().minus(45, java.time.temporal.ChronoUnit.DAYS).toString
  private val startTime = java.time.Instant.now().minus(90, java.time.temporal.ChronoUnit.DAYS).toString
  private val pageSize  = 20000

  // loadApi: reads the API key from config.properties.
  // Returns Either[String, String]. Left if missing, Right if found.
  private def loadApi(): Either[String, String] =
    try {
      val file = new File("config.properties")
      if (!file.exists())
        return Left("config.properties not found. Add your Fingrid API key to it.")

      val bufferedSource = Source.fromFile(file)
      val lines          = bufferedSource.getLines().toList
      bufferedSource.close()

      val key = lines
        .filter(_.startsWith("fingrid.api.key="))
        .flatMap(_.split("=").lift(1))
        .headOption
        .getOrElse("")

      if (key.nonEmpty && key != "YOUR_KEY_HERE") Right(key)
      else Left("API key not set. Edit config.properties and replace YOUR_KEY_HERE.")
    } catch {
      case e: Exception => Left(s"Could not load config: ${e.getMessage}")
    }

  // This returns the local file path for a given source.
  private def csvPath(source: SourceType): String =
    s"$dataDir/${label(source).toLowerCase}.csv"

  // fetchChunk: fetches one time range chunk from the API.
  // Returns Either[String, String] — Left if error, Right if success.
  private def fetchChunk(key: String, datasetId: Int, start: String, end: String): Either[String, String] = {
    val url = s"https://data.fingrid.fi/api/datasets/$datasetId/data" +
      s"?startTime=$start&endTime=$end" +
      s"&pageSize=$pageSize&sortOrder=asc&format=csv"
    try {
      val response = quickRequest
        .get(uri"$url")
        .header("x-api-key", key)
        .send()
      response.code.code match {
        case 200 =>
          val body = response.body
          if (body.startsWith("{"))
            Right(body.stripPrefix("{\"data\":\"").stripSuffix("\"}").replace("\\n", "\n"))
          else Right(body)
        case 401  => Left("API Error 401: Invalid or missing API key.")
        case 403  => Left("API Error 403: Access forbidden.")
        case 404  => Left(s"API Error 404: Dataset $datasetId not found.")
        case 429  => Left("API Error 429: Rate limit exceeded. Try again later.")
        case 503  => Left("API Error 503: Fingrid service unavailable.")
        case code => Left(s"API Error $code: Unexpected error.")
      }
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  // fetchAndSave: fetches one chunk for Solar (already 15-min, fits in one call).
  def fetchAndSave(source: SourceType): Either[String, Unit] =
    loadApi() match {
      case Left(err) => Left(err)
      case Right(key) =>
        fetchChunk(key, datasetIds(source), startTime, endTime) match {
          case Left(err) => Left(err)
          case Right(csv) =>
            val dir = new File(dataDir)
            if (!dir.exists()) dir.mkdirs()
            val writer = new PrintWriter(new File(csvPath(source)))
            writer.write(csv)
            writer.close()
            Right(())
        }
    }

  // fetchAndSaveBoth: fetches two 45-day chunks and combines them.
  // Used for Wind and Hydro to cover the full 90 days.
  def fetchAndSaveBoth(source: SourceType): Either[String, Unit] =
    loadApi() match {
      case Left(err) => Left(err)
      case Right(key) =>
        val datasetId = datasetIds(source)
        for {
          chunk1 <- fetchChunk(key, datasetId, startTime, midTime)
          _       = Thread.sleep(3000)
          chunk2  <- fetchChunk(key, datasetId, midTime, endTime)
        } yield {
          val dir = new File(dataDir)
          if (!dir.exists()) dir.mkdirs()
          val lines1 = chunk1.split("\n").toList
          val lines2 = chunk2.split("\n").toList.drop(1)
          val writer = new PrintWriter(new File(csvPath(source)))
          writer.write((lines1 ++ lines2).mkString("\n"))
          writer.close()
        }
    }

  // Reads a saved CSV file into a List of EnergyReading.
  // Fingrid CSV format: datasetId;startTime;endTime;value
  def parseCsv(source: SourceType): Either[String, List[EnergyReading]] = {
    val file = new File(csvPath(source))
    if (!file.exists())
      return Left(s"File not found: ${csvPath(source)}. Please fetch data first.")
    try {
      val bufferedSource = Source.fromFile(file, "UTF-8")
      val lines          = bufferedSource.getLines().toList
      bufferedSource.close()
      val readings = lines.drop(1).filter(_.trim.nonEmpty).flatMap(line => parseRow(source, line))
      Right(readings)
    } catch {
      case e: Exception => Left(s"Error reading ${csvPath(source)}: ${e.getMessage}")
    }
  }

  private def parseRow(source: SourceType, line: String): Option[EnergyReading] = {
    val cols = line.split(";")
    if (cols.length >= 4)
      try Some(EnergyReading(
        source       = source,
        startTimeUtc = cols(1).trim,
        endTimeUtc   = cols(2).trim,
        value        = cols(3).trim.toDouble
      ))
      catch {
        case _: NumberFormatException => None
      }
    else None
  }

  // takeSample: takes every nth reading to reduce data size
  // while keeping the full time range covered.
  // Used for Hydro which only has 3-minute data available.
  private def takeSample(readings: List[EnergyReading], n: Int): List[EnergyReading] =
    readings.zipWithIndex.filter(_._2 % n == 0).map(_._1)

  // Fetches all 3 sources from Fingrid, saves to CSV, then returns the combined list.
  // Falls back to cached CSV if the API call fails.
  def loadAllSources(): List[EnergyReading] = {
    def process(remaining: List[SourceType], acc: List[EnergyReading]): List[EnergyReading] =
      remaining match {
        case Nil => acc
        case source :: rest =>
          println(s"Fetching last 90 days of ${label(source)} data from Fingrid...")
          Thread.sleep(2000)

          val fetchResult = if (source == Solar) fetchAndSave(source) else fetchAndSaveBoth(source)
          fetchResult match {
            case Left(error) => println(s"Failed to fetch ${label(source)}: $error. Trying cached CSV...")
            case Right(_)    => println(s"${label(source)} data saved successfully.")
          }

          parseCsv(source) match {
            case Right(readings) =>
              val sampled = if (source == Hydro) takeSample(readings, 5) else readings
              println(s"Loaded ${sampled.length} readings for ${label(source)}.")
              process(rest, acc ++ sampled)
            case Left(error) =>
              println(s"Could not load ${label(source)}: $error. Skipping.")
              process(rest, acc)
          }
      }

    process(allSources, List.empty)
  }
}