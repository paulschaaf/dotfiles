#/usr/bin/etc ruby
#/ -*- compile-command: (concat "svn commit -m ''; ssh satori 'cd " (file-name-directory buffer-file-name) " && svn update && rsync -avu --exclude=.svn . ~apache/html/jira-redirect'") -*-

# require '~/lib/guidewire/jira-redirect/ReportForCustomer.rb'

$debugLevel = 0

require 'enumerator'
require 'stringio'

class Parameter
  def self.translator
    # default value is the value itself
    return @translator if @translator
    @translator = Hash.new {|hash, key| key}.
      merge({
              :action        =>  :customfield_10151,
              :ascending     =>  :ASC,
              :cc            =>  10000,
              :closed        =>  6,
              :customerName  =>  :customfield_10060,
              :days_30       =>  2592000000,
              :days_7        =>  604800000,
              :descending    =>  :DESC,
              :ffs           =>  10040,
              :in_progress   =>  3,
              :key           =>  :issueKey,
              :open          =>  1,
              :order_1       =>  :'sorter/order',
              :order_2       =>  :'sorter/order',
              :order_3       =>  :'sorter/order',
              :pg            =>  10041,
              :project       =>  :pid,
              :reopened      =>  4,
              :resolved      =>  5,
              :sort_1        =>  :'sorter/field',
              :sort_2        =>  :'sorter/field',
              :sort_3        =>  :'sorter/field',
              :ss            =>  10080,
              :unresolved    =>  -1,
              :verified      =>  6,
            })
  end

  def initialize(aKey=nil, *values)
    @key, @value = aKey, values
  end

  def <<(value)
    self.basicValue << value
    self
  end

  def to_s
    "#{self.key}=" + self.value.join("&#{self.key}=")
  end

  def translate(*values)
    values.collect {|e| self.class.translator[e]}
  end

  def basicKey
    @key
  end

  def basicValue
    @value
  end

  def key
    self.translate(self.basicKey)[0]
  end

  def value
    self.translate(*self.basicValue)
  end
end


# == Like Hash, but enumerates elements in order of initial insertion
class OrderedHash < Hash
  attr_accessor :array

  def self.sample
    oh = OrderedHash[:one, 1, :two, 2, :three, 3]
    oh[:four] = 4; oh[:five] = 5; oh[:six]  = 6
    oh
  end

  def test
    self.each {|k, v| puts k}
  end

  def initialize
    @array = Array.new
  end

  def self.[](*array)
    answer = self.new
    array.each_slice(2) {|key, value| answer[key] = value}
    answer
  end

  def []=(key, value)
    key_already_present = self.has_key?(key)
    answer = super
    @array << key unless key_already_present
    answer
  end

  def delete(key)
    key_already_present = self.has_key?(key)
    super
    @array.delete(key) if key_already_present
  end

  alias_method :_each, :each

  def each
    # enumerate the contents in the order the keys were added to the array
    @array.each {|key| yield(key,self[key])}
    self
  end

  def join(separator)
    s = StringIO.new
    self.inject('') {|sep, pair|
      s << sep << pair.last.to_s
      separator
    }
    s.string
  end
end


class ParamList < OrderedHash
  def []=(key, value)
    param = Parameter.new(key, *value)
    super(key, param)
    param
  end

  def to_s
    self.join('&')
  end
end


class URL
  attr_accessor :location, :parameters

  def initialize(loc, param, *params)
    @location = loc
    @parameters = if param.is_a?(ParamList)
                    param
                  else
                    ParamList[param, *params]
                  end
  end

  def to_s
    return self.location if self.parameters.empty?
    self.location + '?' + self.parameters.to_s
  end
end


defaultParams = ParamList[
   :mode,     :show,
   :reset,    :true,
   :tempMax,   120,
   :project,  [:ffs, :ss],
   :status,   [:open, :in_progress, :reopened],
   :sort_1,   :key,
   :order_1,  :ascending,
   :sort_2,   :priority,
   :order_2,  :descending,
   :sort_3,   :status,
   :order_3,  :ascending,
]
defaultParams.to_s
p = defaultParams[:project]

basicUrl = URL.new('http://jira/jira/secure/IssueNavigator.jspa', defaultParams)
basicUrl.to_s

=begin
require '~/lib/guidewire/jira-redirect/ReportForCustomer.rb'
basicUrl.to_s


   # << Parameter.new(:project, :ffs) \
   # << Parameter.new(:project, :pg) \
   # << Parameter.new(:status,  :resolved)

customer_abbrev_partner = %w(
    AMERCO                  -          pschaaf
    Amica                   -          vrai
    AXA\ Canada             AXA_CA     vrai
    Canal                   -          pschaaf
    CIGA                    -          pschaaf
    CNA                     -          pkrishnan
    CNA\ Policy             -          apatel
    Dominion                -          pkrishnan
    Donegal                 -          pkrishnan
    EDIC                    -          pschaaf
    FCCI                    -          ggoon
    GAIC                    -          ggoon
    GEICO                   -          ggoon
    Hanover                 -          pschaaf
    Hastings\ Mutual        Hastings   pkrishnan
    ICAT                    -          pkrishnan
    Internal                -          dparker-rose
    KFB\ Policy             KFBPolcy   dparker-rose
    Kentucky\ Farm\ Bureau  KFB        vrai
    Liberty\ Mutual         Liberty    pkrishnan
    Liberty\ Surety         LibSure    pkrishnan
    MSA                     -          pschaaf
    Merastar                -          vrai
    Montana\ State          MSF        vrai
    New\ Jersey\ Mfg\ Grp   NJM        pschaaf
    New\ Mexico\ Mutual     NMM        vrai
    Privilege               -          ggoon
    Sentry                  -          apatel
    Sequoia                 -          vrai
    SUA                     -          dparker-rose
    Suncorp                 -          ggoon
)

$abbrev, $customer_to_partner = {}, {}
$partner_to_customer = Hash.new {|hash, key| hash[key] = []}

customer_abbrev_partner.each_slice(3) {|cust, abbrev, partner|
  $customer_to_partner[cust] = partner
  $partner_to_customer[partner] << cust
  $abbrev[cust] = abbrev unless abbrev == '-'
}

# ==========
# ==========
# ==========

$assignee = ENV['USER']
$this_app = $0.split('/')[-1].to_sym

function collectUserParams(paramString) {
   params = Array()
   userParamPairs = paramString.split('&')
   for (i in userParamPairs) {
      pair = userParamPairs[i].split('=')
      // translate parameters here
      switch (pair[0]) {
         case '':
            // ignore empty parameters
            break
         case 'debugLevel':
            $debugLevel = pair[1]
            document.write('debugLevel is now ', $debugLevel, '<p/>')
            break
         default:
            params[pair[0]] = pair[1]
      }
   }
   return params
}

function debugWrite(level) {
   if ((level || 1) >= $debugLevel) {
      arguments.shift()
      document.write(arguments)
   }
}

// If I add this as a function to Array, the iterators will treat this function as if it were a customer of the same name
function dump_array(array) {
   document.write('<hr/>')
   for (key in array) { document.write('<li>', key, ': ', array[key], '</li>'); }
   document.write('<hr/>')
}

userParamString = location.href.split('?')[1] || ''
userParams      = collectUserParams(userParamString)
if ($debugLevel >= 1) { dump_array(params) }

function paramAndValue(key, value) { return '&' + key + '=' + value }

function pageForCustomer(cust) {
   answer = basicUrl + '?'
   if (cust) {
      answer += paramAndValue(customerNameField, cust)
   }
   else {
      for (key in userParams) {
         answer += paramAndValue(key, userParams[key])
      }
      for (key in defaultParams) {
         if (! userParams[key]) {
            answer += paramAndValue(key, defaultParams[key])
         }
      }
   }
   return answer
}

// if customer specified, don't render the list of links, just go straight to jira
if (userParams[customerNameField]) {
   window.location = pageForCustomer()
}
else {
   document.write('<h1>' + document.title + '</h1>')
   basicUrl = ''
}

function renderList() {
   for (idx in customers) {
      cust = customers[idx]
      document.write(
        '<li>' +
           '<a href='' + pageForCustomer(cust) + ''>' + cust + '</a>' +
           '&nbsp;&nbsp;(<a target='_blank' href='' + pageForCustomer(cust) + '&view=excel-current' class='small'>xls</a>)' +
        '</li>'
      )
   }
}



// proj_ffs
// resolution_unresolved
// resolution_verified
// customerNameField = 'Liberty Mutual'
// status_open
// status_in_progress
// status_reopened
// status_resolved

// function sort_ascending_by(name) {
//    return '&sorter/field=' + name + '&sorter/order=ASC'
// }
// function sort_descending_by(name) {
//    return '&sorter/field=' + name + '&sorter/order=DESC'
// }

// sort_ascending_by(actionField)
// sort_ascending_by('resolution')
// sort_ascending_by('status')

// function pair(key, value) {
//    this.to_s  = function() {
//       return '&' + this.key + '=' + this.value
//    }

//    this.key   = key
//    this.value = value
// }

// Array.prototype.addKeyValue = function(key, value) {
//    this[key] = new pair(key, value)
//    return this[key]
// }

// Array.prototype.to_s = function() {
//    answer = ''
//    for (idx in this) {
//       answer += this[idx].to_s
//    }
//    return answer
// }

// function encode(name) { return name.replace(' ', '%20') }

// sort_ascending_by(actionField)
// sort_ascending_by(customerNameField)
// sort_ascending_by('issueKey')
// sort_descending_by('priority')
// sort_ascending_by('status')

=end
