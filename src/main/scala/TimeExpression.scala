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
    val fromDate: LocalDate = LocalDate.of(from.getYear, from.getMonthValue, dayOfWeek.getValue)
    TimeExpressionMonthWithDayOfWeek(fromDate, dayOfWeek, amountMonth, weekOfMonth)
  }

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = {
    val fromDate: LocalDate = LocalDate.of(fromYear, day.getMonthValue, day.getDayOfMonth)
    TimeExpressionYear(fromDate, amountOfYears)
  }

  def dateIsAfterOrEquals(from: LocalDate, date: LocalDate): Boolean = !from.isAfter(date) || from.isEqual(date)
}

case class TimeExpressionImpl(from: LocalDate) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = TimeExpression.dateIsAfterOrEquals(from, date)
}

case class TimeExpressionMonth(from: LocalDate, dayOfMonth: Int, amountOfMonth: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = {
    TimeExpression.dateIsAfterOrEquals(from, date) && (date.getMonthValue - from.getMonthValue) % amountOfMonth == 0 && date.getDayOfMonth.equals(dayOfMonth)
  }
}

case class TimeExpressionMonthWithDayOfWeek(from: LocalDate, dayOfWeek: DayOfWeek, amountOfMonth: Int, weekOfMonth: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = {
    val dateWithFirtsDay = date.`with`(TemporalAdjusters.firstDayOfMonth())
    val dateWithWeek = weekOfMonth match {
      case 5 => dateWithFirtsDay.`with`(TemporalAdjusters.lastInMonth(dayOfWeek))
      case _ => dateWithFirtsDay.`with`(TemporalAdjusters.dayOfWeekInMonth(weekOfMonth, dayOfWeek))
    }
    TimeExpression.dateIsAfterOrEquals(from, date) && (date.getMonthValue - from.getMonthValue) % amountOfMonth == 0 && date.equals(dateWithWeek)
  }
}

case class TimeExpressionYear(from: LocalDate, amountOfYears: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = TimeExpression.dateIsAfterOrEquals(from, date) && (from.getYear - date.getYear) % amountOfYears == 0
}

case class TimeExpressionDaily(from: LocalDate, amountOfDay: Int) extends TimeExpression {
  override def isRecurringOn(date: LocalDate): Boolean = TimeExpression.dateIsAfterOrEquals(from, date) && Duration.between(from.atStartOfDay, date.atStartOfDay()).toDays % amountOfDay == 0
}

trait TimeExpression {
  def isRecurringOn(localDate: LocalDate): Boolean
}