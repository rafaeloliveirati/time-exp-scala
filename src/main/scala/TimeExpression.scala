import java.time._
import java.time.temporal.{TemporalAdjuster, TemporalAdjusters}

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
    TimeExpressionImpl(from, amountOfDay = amountOfDay)
  }

  def monthlyEvery(amountMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = {
    val fromDate: LocalDate = LocalDate.of(from.getYear, from.getMonthValue, dayOfMonth)
    TimeExpressionImpl(fromDate, dayOfMonth, amountMonth)
  }

  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = {
    val date = LocalDate.of(from.getYear, from.getMonthValue, 1)
    val teste = date.`with`(TemporalAdjusters.dayOfWeekInMonth(weekOfMonth, DayOfWeek.FRIDAY))
    val fromDate: LocalDate = LocalDate.of(from.getYear, from.getMonthValue, dayOfWeek.getValue)
    TimeExpressionImpl(fromDate, amountOfMonth = amountMonth)
  }

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = {
    val fromDate: LocalDate = LocalDate.of(fromYear, day.getMonthValue, day.getDayOfMonth)
    TimeExpressionImpl(fromDate, amountOfYears = amountOfYears)
  }

  def yearIsRecurring(date: LocalDate, from: LocalDate, amountOfYears: Int): Boolean = (from.getYear - date.getYear) % amountOfYears == 0

  def monthIsRecurring(date: LocalDate, from: LocalDate, amountOfMonth: Int): Boolean = (date.getMonthValue - from.getMonthValue) % amountOfMonth == 0 && date.getDayOfMonth.equals(from.getDayOfMonth)

  def weekOfMonthIsRecurring(date: LocalDate, from: LocalDate, amountOfMonth: Int, weekOfMonth: Int, dayOfWeek: DayOfWeek): Boolean = {
    (date.getMonthValue - from.getMonthValue) % amountOfMonth == 0 && date.getDayOfWeek.equals(dayOfWeek)
  }

  def dayIsRecurring(date: LocalDate, from: LocalDate, amountOfDay: Int): Boolean = Duration.between(from.atStartOfDay, date.atStartOfDay()).toDays % amountOfDay == 0
}

case class TimeExpressionImpl(from: LocalDate, amountOfDay: Int = 0, amountOfMonth: Int = 0, amountOfYears: Int = 0, weekOfMonth: Int = 0, dayOfWeek: DayOfWeek = DayOfWeek.MONDAY) extends TimeExpression {

  override def isRecurringOn(date: LocalDate): Boolean = {
    //    date match {
    //      case dateMatch if dateMatch.isBefore(from) => false
    //      case dateMatch if date.isEqual(from) => true
    //      case dateMatch if amountOfYears != 0 => TimeExpression.yearIsRecurring(dateMatch, from, amountOfYears)
    //      case dateMatch if amountOfMonth != 0 => TimeExpression.monthIsRecurring(dateMatch, from, amountOfMonth)
    //      case dateMatch if amountOfDay != 0 => TimeExpression.dayIsRecurring(dateMatch, from, amountOfDay)
    //      case _ => false
    //    }

    if (from.isAfter(date)) false
    else if (from.isEqual(date)) true
    else {
      if (amountOfYears != 0) TimeExpression.yearIsRecurring(date, from, amountOfDay)
      else if (weekOfMonth != 0) TimeExpression.weekOfMonthIsRecurring(date, from, amountOfMonth, weekOfMonth, dayOfWeek)
      else if (amountOfMonth != 0) TimeExpression.monthIsRecurring(date, from, amountOfMonth)
      else if (amountOfDay != 0) TimeExpression.dayIsRecurring(date, from, amountOfYears)
      else false
    }
  }
}

trait TimeExpression {
  def isRecurringOn(localDate: LocalDate): Boolean
}