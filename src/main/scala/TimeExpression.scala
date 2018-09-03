import java.time._
import java.time.temporal.TemporalAdjusters

object TimeExpression {

  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = {
    TimeExpressionImpl(localDate)
  }

  def daily(amountOfDay: Int, from: LocalDate): TimeExpression = {
    TimeExpressionDaily(from, amountOfDay)
  }

  def monthlyEvery(amountMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = {
    val fromDate: LocalDate = LocalDate.of(from.getYear, from.getMonthValue, dayOfMonth)
    TimeExpressionMonth(fromDate, dayOfMonth, amountMonth)
  }

  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = {
    val fromDate: LocalDate = LocalDate.of(from.getYear, from.getMonthValue, 1)
    TimeExpressionMonthWithDayOfWeek(fromDate, dayOfWeek, amountMonth, weekOfMonth)
  }

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = {
    val fromDate: LocalDate = LocalDate.of(fromYear, day.getMonthValue, day.getDayOfMonth)
    TimeExpressionYear(fromDate, amountOfYears)
  }

  def isDateAfterOrEquals(from: LocalDate, date: LocalDate): Boolean = date.isAfter(from) || from.isEqual(date)

  def isRecurrentMod(from: Int, date: Int, amount: Int): Boolean = ((date - from) % amount).equals(0)
}

trait TimeExpression {
  def isRecurringOn(localDate: LocalDate): Boolean
}

case class TimeExpressionImpl(from: LocalDate) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = TimeExpression.isDateAfterOrEquals(from, date)
}

case class TimeExpressionDaily(from: LocalDate, amountOfDay: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = {
    val diffBetweenDays = Duration.between(from.atStartOfDay, date.atStartOfDay()).toDays.toInt
    TimeExpression.isDateAfterOrEquals(from, date) && (diffBetweenDays % amountOfDay).equals(0)
  }
}

case class TimeExpressionMonth(from: LocalDate, dayOfMonth: Int, amountOfMonth: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = {
    TimeExpression.isDateAfterOrEquals(from, date) && TimeExpression.isRecurrentMod(from.getMonthValue, date.getMonthValue, amountOfMonth) && date.getDayOfMonth.equals(dayOfMonth)
  }
}

case class TimeExpressionYear(from: LocalDate, amountOfYears: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = TimeExpression.isDateAfterOrEquals(from, date) && TimeExpression.isRecurrentMod(from.getYear, date.getYear, amountOfYears)
}

case class TimeExpressionMonthWithDayOfWeek(from: LocalDate, dayOfWeek: DayOfWeek, amountOfMonth: Int, weekOfMonth: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = {
    val firstDayOfMonth = date.`with`(TemporalAdjusters.firstDayOfMonth())
    val dateWithWeek = weekOfMonth match {
      case 5 => firstDayOfMonth.`with`(TemporalAdjusters.lastInMonth(dayOfWeek))
      case _ => firstDayOfMonth.`with`(TemporalAdjusters.dayOfWeekInMonth(weekOfMonth, dayOfWeek))
    }
    TimeExpression.isDateAfterOrEquals(from, date) && TimeExpression.isRecurrentMod(from.getMonthValue, date.getMonthValue, amountOfMonth) && date.equals(dateWithWeek)
  }
}