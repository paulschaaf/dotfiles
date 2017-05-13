require 'extn/DateTime'

class BusinessDateTime < DateTime
  alias_method :to_s_long, :to_s
  alias_method :to_s, :to_s_short

  @@holidays = [
    [1],   [],    [],          [],
    [],    [],    [],          [],
    [],    [],    [24, 25],    []
  ]

  def self.parse(string)
    super
  rescue
    super(sprintf('%s/%s', string, self.now.year))
  end

  def holiday?
    @@holidays[month - 1].include?(day)
  end

  def business_day?
    self.weekday? and not self.holiday?
  end

  def previous_business_day
    answer = self - 1
    answer -= 1 until answer.business_day?
    answer
  end

  def prev(num=1)
    (1..num).inject(self) {|date, count| date.previous_business_day}
  end

  def next_business_day
    answer = self + 1
    answer += 1 until answer.business_day?
    answer
  end

  def next(num=1)
    (0..num).inject(self) {|date, count| date.next_business_day}
  end

  alias_method :prev_business_day, :prev
end


class String
  def to_datetime
    BusinessDateTime.parse(self)
  end
end
