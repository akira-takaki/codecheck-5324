import java.util

import scala.collection.immutable.HashMap

object Main {
  def main(args: Array[String]) = {
    val daysInYear = args(0).toInt  // 1年を構成する日数
    val daysInMonth = args(1).toInt // 1ヶ月を構成する日数
    val daysInWeek = args(2).toInt  // 1週間を構成する日数

    val date = args(3) // 日付(YYYY-MM-DD形式)

    println(BizarreCalendar(daysInYear, daysInMonth, daysInWeek).whatDayIsIt(date))
  }
}

object BizarreCalendar {
  def apply(daysInYear: Int, daysInMonth: Int, daysInWeek: Int): BizarreCalendar = new BizarreCalendar(daysInYear, daysInMonth, daysInWeek)
}
class BizarreCalendar(daysInYear: Int, daysInMonth: Int, daysInWeek: Int) {

  val alphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val dayOfWeek = (alphabets take daysInWeek) split "" // daysInWeek が 3 なら Array("A", "B", "C")

  /**
    * 指定された日付が何曜日かを返す
    * 日付が不正な場合は "-1" を返す
    *
    * @param date 日付(YYYY-MM-DD形式)
    * @return 曜日を表す文字列("A" - "Z") または "-1"
    */
  def whatDayIsIt(date: String): String = {
    val dateIntArray = (date split '-').map(_.toInt)
    val yyyy = dateIntArray(0)
    val mm = dateIntArray(1)
    val dd = dateIntArray(2)

    // 日付の日が 1ヶ月を構成する日数を超えたら NG
    if (dd > daysInMonth) return "-1"

    // 日付の月が 1年を構成する月数を超えたら NG
    // 閏月を考慮する必要あり！

    val dates = new scala.collection.mutable.HashMap[String, String]
    var currentYyyy = 1
    var currentMm = 1
    var currentDd = 0
    var currentDayOfWeek = 0
    var surplus = daysInYear % daysInMonth  // 閏月用の余りの日数
    var next = true
    while(next) {
      // 「日」を1つ進める
      currentDd += 1

      // 「曜日」を1つ進める
      currentDayOfWeek += 1
      if (currentDayOfWeek > dayOfWeek.size) {
        currentDayOfWeek = 1
      }

      if (currentDd > daysInMonth) {
        // 「日」が指定された1ヶ月の範囲を超えたら「月」を進める
        currentDd = 1  // 「日」は1にリセット
        currentMm += 1
      }

      if (currentMm > (daysInYear / daysInMonth)) {
        // 「月」が指定された1年の範囲を超えた

        if (surplus >= daysInMonth) {
          // 閏月用の余りの日数が1ヶ月を構成 *できる* 場合
          // 閏月用の余りの日数を1ヶ月分引き
          surplus -= daysInMonth

          // 1ヶ月分日付を作る
          for(i <- 1 to daysInMonth) {
            val currentDate = "%04d".format(currentYyyy) + "-" + "%02d".format(currentMm) + "-" + "%02d".format(i)
            // DEBUG START
            //println(currentDate + ", " + dayOfWeek(currentDayOfWeek -1) + ", surplus=" + surplus + " +")
            // DEBUG END
            dates.put(currentDate, dayOfWeek(currentDayOfWeek - 1))

            // 「曜日」を1つ進める
            currentDayOfWeek += 1
            if (currentDayOfWeek > dayOfWeek.size) {
              currentDayOfWeek = 1
            }
          }
        }

        currentMm = 1  // 「月」は1にリセット
        currentYyyy += 1

        // 閏月用の余りの日数を追加
        surplus += daysInYear % daysInMonth
      }

      // 日付を作る
      val currentDate = "%04d".format(currentYyyy) + "-" + "%02d".format(currentMm) + "-" + "%02d".format(currentDd)
      // DEBUG START
      //println(currentDate + ", " + dayOfWeek(currentDayOfWeek -1) + ", surplus=" + surplus)
      // DEBUG END
      dates.put(currentDate, dayOfWeek(currentDayOfWeek - 1))

      // currentYyyy、currentMm、currentDd が曜日の確認を求められた日付 yyyy、mm、dd を超えたら終了
      if (currentYyyy > yyyy) {
        next = false;
      }
    }

    return dates.get(date).getOrElse("-1")
  }

}
