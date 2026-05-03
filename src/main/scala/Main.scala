// AI STATEMENT: Claude used for learning, ideation, structure and debugging. All implementation is our own.
// If AI has been used more in implementation, it has been said in the file itself.


//This file loads the data and runs the main menu.

import EnergySource._

object Main extends App {

  println("Renewable Energy Plant System (REP)")
  println("Loading data from the last 90 days")

  val readings = FileHandling.loadAllSources()
  println(s"Total readings loaded: ${readings.length}")

  mainMenu(readings)

  def mainMenu(readings: List[EnergyReading]): Unit = {
    println("\nMain Menu:")
    println("  1. View summary")
    println("  2. Analyse data")
    println("  3. View alerts")
    println("  4. Exit")
    print("Enter choice: ")

    scala.io.StdIn.readLine().trim match {

      case "1" =>
        Display.showSummary(readings)
        mainMenu(readings)

      case "2" =>
        Analysis.analysisMenu(readings)
        mainMenu(readings)

      case "3" =>
        Alert.checkAll(readings)
        mainMenu(readings)

      case "4" =>
        println("Goodbye!")

      case _ =>
        println("Invalid choice. Please try again.")
        mainMenu(readings)
    }
  }
}