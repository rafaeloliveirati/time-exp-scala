import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}

import org.scalatest.{FlatSpec, Matchers}

class TimeExpressionSpec extends FlatSpec with Matchers {

  it should "not reccur" in {
    val today = LocalDate.now
    val todayTimeExp = TimeExpression(today)

    todayTimeExp.isRecurringOn(today) should be(true)
  }

  it should "reccur every day" in {
    val oneDay = 1
    val today = LocalDate.now
    val everyDayFromToday = TimeExpression.daily(oneDay, today)

    everyDayFromToday.isRecurringOn(today) should be(true)
    everyDayFromToday.isRecurringOn(today.plusDays(1)) should be(true)
    everyDayFromToday.isRecurringOn(today.plusDays(2)) should be(true)
    everyDayFromToday.isRecurringOn(today.plusDays(3)) should be(true)
    everyDayFromToday.isRecurringOn(today.plusDays(4)) should be(true)
    everyDayFromToday.isRecurringOn(today.plusDays(5)) should be(true)
    everyDayFromToday.isRecurringOn(today.plusDays(6)) should be(true)
    everyDayFromToday.isRecurringOn(today.plusYears(50)) should be(true)
  }

  it should "reccur every two days" in {
    val twoDays = 2
    val today = LocalDate.now
    val everyTwoDaysFromToday = TimeExpression.daily(twoDays, today)

    everyTwoDaysFromToday.isRecurringOn(today) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(1)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(2)) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(3)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(4)) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(5)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(6)) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(30001)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(30002)) should be(true)
  }

  it should "reccur every sixteen days" in {
    val twoDays = 16
    val today = LocalDate.now
    val everyTwoDaysFromToday = TimeExpression.daily(twoDays, today)

    everyTwoDaysFromToday.isRecurringOn(today) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(1)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(4)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(16)) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(32)) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(321)) should be(false)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(320)) should be(true)
  }

  it should "reccur every teen days" in {
    val teenDays = 10
    val today = LocalDate.now
    val everyTwoDaysFromToday = TimeExpression.daily(teenDays, today)

    everyTwoDaysFromToday.isRecurringOn(today) should be(true)
    everyTwoDaysFromToday.isRecurringOn(today.plusDays(10)) should be(true)
  }

  it should "reccur every month the second day" in {
    val oneMonth = 1
    val secondDayOfMonth = 2
    val januaryOf2012 = YearMonth.of(2012, 1)
    val everyMonthTheSecondDayFromJanuary2012 = TimeExpression.monthlyEvery(oneMonth, secondDayOfMonth, januaryOf2012)

    val secondDayOfJanuary2012 = LocalDate.of(2012, 1, 2)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(1)) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(2)) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(3)) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(4)) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(500)) should be(true)

    val firstDayOfJanuary2012 = LocalDate.of(2012, 1, 1)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(firstDayOfJanuary2012.plusMonths(2)) should be(false)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(firstDayOfJanuary2012.plusMonths(3)) should be(false)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(firstDayOfJanuary2012.plusMonths(500)) should be(false)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(firstDayOfJanuary2012.plusMonths(15).plusDays(1)) should be(true)
  }

  it should "reccur every two months the second day" in {
    val twoMonths = 2
    val secondDayOfMonth = 2
    val januaryOf2012 = YearMonth.of(2012, 1)
    val everyMonthTheSecondDayFromJanuary2012 = TimeExpression.monthlyEvery(twoMonths, secondDayOfMonth, januaryOf2012)

    val secondDayOfJanuary2012 = LocalDate.of(2012, 1, 2)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(1)) should be(false)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(2)) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(3)) should be(false)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(4)) should be(true)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(501)) should be(false)
    everyMonthTheSecondDayFromJanuary2012.isRecurringOn(secondDayOfJanuary2012.plusMonths(502)) should be(true)
  }

  it should "reccur every month the first friday" in {
    val oneMonth = 1
    val firstWeekOfMonth = 1
    val januaryOf2012 = YearMonth.of(2012, 1)
    val everyMonthTheFirstFridayFromJanuary2012 = TimeExpression.monthlyEvery(oneMonth, DayOfWeek.FRIDAY, firstWeekOfMonth, januaryOf2012)

    val firstFridayOfJanuary2012 = LocalDate.of(2012, 1, 6)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012) should be(true)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(1)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(2)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(3)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(4)) should be(true)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(5)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(6)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(7)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(8)) should be(true)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(9)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(10)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(11)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(12)) should be(false)
    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(13)) should be(true)
  }
  //
  //  it should "reccur every month the last friday" in {
  //    val oneMonth = 1
  //    val lastWeekOfMonth = 5
  //    val januaryOf2012 = YearMonth.of(2012, 1)
  //    val everyMonthTheFirstFridayFromJanuary2012 = TimeExpression.monthlyEvery(oneMonth, DayOfWeek.FRIDAY, lastWeekOfMonth, januaryOf2012)
  //
  //    val firstFridayOfJanuary2012 = LocalDate.of(2012, 1, 6)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(1)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(2)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(3)) should be(true)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(4)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(5)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(6)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(7))  should be(true)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(8)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(9)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(10)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(11)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(12)) should be(true)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(13)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(14)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(15)) should be(false)
  //    everyMonthTheFirstFridayFromJanuary2012.isRecurringOn(firstFridayOfJanuary2012.plusWeeks(16)) should be(true)
  //  }
  //
  it should "reccur every year the last friday" in {
    val oneYear = 1
    val augustTheEight = MonthDay.of(8, 8)
    val everyAugustTheEightFrom2012 = TimeExpression.yearlyEvery(oneYear, augustTheEight, 2012)

    val firstEightOfAugust = LocalDate.of(2012, 8, 8)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust) should be(true)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(1)) should be(true)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(2)) should be(true)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(3)) should be(true)
  }

  it should "reccur thirty year the last friday" in {
    val oneYear = 30
    val julyTheEight = MonthDay.of(8, 7)
    val everyAugustTheEightFrom2012 = TimeExpression.yearlyEvery(oneYear, julyTheEight, 2012)

    val firstEightOfAugust = LocalDate.of(2012, 8, 8)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust) should be(true)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(1)) should be(false)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(20)) should be(false)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(30)) should be(true)
    everyAugustTheEightFrom2012.isRecurringOn(firstEightOfAugust.plusYears(120)) should be(true)
  }
}
