package chess

import scala.concurrent.duration._

// All durations are expressed in seconds
sealed trait Clock {
  val whiteLimit: Int
  val whiteIncrement: Int
  val blackLimit: Option[Int] // if None, same as white's
  val blackIncrement: Option[Int]
  val color: Color
  val whiteTime: Float
  val blackTime: Float
  val timerOption: Option[Double]

  val asymmetricTimeControls: Boolean = (
    Some(whiteLimit) != blackLimit ||
    Some(whiteIncrement) != blackIncrement
  )

  def limit(c: Color) = c.fold(whiteLimit, blackLimit getOrElse whiteLimit)

  def increment(c: Color) = c.fold(whiteIncrement, blackIncrement getOrElse whiteIncrement)

  def time(c: Color) = c.fold(whiteTime, blackTime)

  def outoftime(c: Color) = remainingTime(c) == 0

  def remainingTime(c: Color) = math.max(0, limit(c) - elapsedTime(c))

  def remainingTimes = Color.all map { c => c -> remainingTime(c) } toMap

  def elapsedTime(c: Color) = time(c)

  def limitInMinutes(c: Color) = limit(c) / 60

  def estimateTotalTime = (
    (limit(White) + 30 * increment(White) +
     limit(Black) + 30 * increment(Black)) / 2 
  )

  def emergTime: Int = math.round(math.min(30, math.max(2, estimateTotalTime / 12)))

  def stop: PausedClock

  def addTime(c: Color, t: Float): Clock

  def giveTime(c: Color, t: Float): Clock

  private def desc(c: Color, sep: String) = limitInMinutes(c) + sep + increment(c)

  def show = if(!asymmetricTimeControls) {
    desc(White, " + ")
  } else {
    desc(White, "+") + " v " + desc(Black, "+")
  } 

  def showCondensed = if(!asymmetricTimeControls) {
    desc(White, "+")
  } else {
    desc(White, "+") + "v" + desc(Black, "+")
  }

  def showTime(time: Float) = {
    val hours = math.floor(time / 3600).toInt
    val minutes = math.floor((time - hours * 3600) / 60).toInt
    val seconds = time.toInt % 60
    s"${if (hours > 0) hours else ""}:$minutes:$seconds"
  }

  def isRunning = timerOption.isDefined

  def isInit = elapsedTime(White) == 0 && elapsedTime(Black) == 0

  def switch: Clock

  def reset = Clock(
    whiteLimit = whiteLimit,
    whiteIncrement = whiteIncrement,
    blackLimit = blackLimit,
    blackIncrement = blackIncrement)

  protected def now = System.currentTimeMillis / 1000d
}

case class RunningClock(
    whiteLimit: Int,
    whiteIncrement: Int,
    blackLimit: Option[Int],
    blackIncrement: Option[Int],
    color: Color,
    whiteTime: Float,
    blackTime: Float,
    timer: Double) extends Clock {

  val timerOption = Some(timer)

  override def elapsedTime(c: Color) = time(c) + {
    if (c == color) now - timer else 0
  }.toFloat

  def step(lag: FiniteDuration = 0.millis) = {
    val t = now
    val spentTime = (t - timer).toFloat
    val lagSeconds = lag.toMillis.toFloat / 1000
    val lagCompensation = math.max(0,
      math.min(
        lagSeconds - Clock.naturalLag,
        Clock.maxLagToCompensate))
    addTime(
      color,
      (math.max(0, spentTime - lagCompensation) - increment(color))
    ).copy(
        color = !color,
        timer = t
      )
  }

  def stop = PausedClock(
    whiteLimit = whiteLimit,
    whiteIncrement = whiteIncrement,
    blackLimit = blackLimit,
    blackIncrement = blackIncrement,
    color = color,
    whiteTime = whiteTime + (if (color == White) (now - timer).toFloat else 0),
    blackTime = blackTime + (if (color == Black) (now - timer).toFloat else 0))

  // add = "count as elapsed"
  def addTime(c: Color, t: Float): RunningClock = c match {
    case White => copy(whiteTime = whiteTime + t)
    case Black => copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Float): RunningClock = addTime(c, -t)

  def switch: RunningClock = copy(color = !color)
}

case class PausedClock(
    whiteLimit: Int,
    whiteIncrement: Int,
    blackLimit: Option[Int],
    blackIncrement: Option[Int],
    color: Color,
    whiteTime: Float,
    blackTime: Float) extends Clock {

  val timerOption = None

  def stop = this

  def addTime(c: Color, t: Float): PausedClock = c match {
    case White => copy(whiteTime = whiteTime + t)
    case Black => copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Float): PausedClock = addTime(c, -t)

  def switch: PausedClock = copy(color = !color)

  def start = RunningClock(
    color = color,
    whiteTime = whiteTime,
    blackTime = blackTime,
    whiteLimit = whiteLimit,
    whiteIncrement = whiteIncrement,
    blackLimit = blackLimit,
    blackIncrement = blackIncrement,
    timer = now)
}

object Clock {

  val minInitLimit = 2f
  // no more than this time will be offered to the lagging player
  val maxLagToCompensate = 10f
  // substracted from lag compensation
  val naturalLag = 0f

  def apply(
    whiteLimit: Int,
    whiteIncrement: Int,
    blackLimit: Option[Int],
    blackIncrement: Option[Int]): PausedClock = {

    val clock0 = PausedClock(
      whiteLimit = whiteLimit / 60 * 60, // round to minutes
      whiteIncrement = whiteIncrement,
      blackLimit = blackLimit map (l => l / 60 * 60),
      blackIncrement = blackIncrement,
      color = White,
      whiteTime = 0f,
      blackTime = 0f)
    
    val clock1 = if (clock0.limit(White) == 0) clock0.giveTime(White, minInitLimit) else clock0
    val clock2 = if (clock1.limit(Black) == 0) clock1.giveTime(Black, minInitLimit) else clock1

    clock2
  }

  def apply(limit: Int, increment: Int): PausedClock = {
    apply(limit, increment, None, None)
  }

  def apply(limit: Int, increment: Int, blackLimit: Int, blackIncrement: Int): PausedClock = {
    apply(limit, increment, Some(blackLimit), Some(blackIncrement))
  }

  def timeString(time: Int) = periodFormatter.print(
    org.joda.time.Duration.standardSeconds(time).toPeriod
  )

  private val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder().
    printZeroAlways.
    minimumPrintedDigits(1).appendHours.appendSeparator(":").
    minimumPrintedDigits(2).appendMinutes.appendSeparator(":").
    appendSeconds.
    toFormatter
}
