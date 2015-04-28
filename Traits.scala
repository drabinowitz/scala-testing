object Traits {
  trait Ord {
    def < (that: Any): Boolean
    def <=(that: Any): Boolean =  (this < that) || (this == that)
    def > (that: Any): Boolean = !(this <= that)
    def >=(that: Any): Boolean = !(this < that)
  }

  class Date(y: Int, m: Int, d: Int) extends Ord {
    def year  = y
    def month = m
    def day   = d
    override def toString(): String = year + "-" + month + "-" + day
    override def equals(that: Any): Boolean = {
      that.isInstanceOf[Date] && {
        val o = that.asInstanceOf[Date]
        o.day == day && o.month == month && o.year == year
      }
    }
    override def <(that: Any): Boolean = {
      if (!that.isInstanceOf[Date])
        error("cannot compare " + that + " and a Date")

      val o = that.asInstanceOf[Date]
      (year < o.year) ||
      (year == o.year && (month < o.month ||
                         (month == o.month && day < o.day)))
    }
  }

  def main(args: Array[String]) {
    val date = new Date(2010, 11, 14)
    val date2 = new Date(2009, 11, 12)
    println(date.toString())
    println(date > date2)
  }
}
