require 'xmlrpc/datetime'

class DateTime
  def self.today
    self.now
  end

  def today?
    self.days_ago == 0
  end

  def days_ago
    (self.class.now - self).to_i
  end

  def prev(num=1)
    self - num
  end

  def to_s_short
    format = case self.days_ago
             when 0
               '%H:%M'
             when 0..1
               'Yestr %H:%M'
             when 0..8
               '%a %H:%M'
             else
               "%m/%d#{'/%y' unless self.year == self.class.today.year}"
             end
    self.strftime(format)
  end

  def weekday?
    (1..5) === self.wday
  end
end

