#######################################################
class QuizQuestion
  attr_accessor :answers, :responses, :id, :quiz

  #attr_reader :question, :reference, :topic
  [:question, :reference, :topic].each do |field|
    attr_reader field
    eval "def #{field}=(arg); @#{field}=arg.strip; end"
  end

  attr_reader :choices
  def add_choice(choice)
    @choices << choice.strip
  end

  def initialize(init_hash={})
    @choices, @answers, @responses = [], [], []
    init_hash.each_pair {|key, value| self.send key, value}
    yield self if block_given?
  end

  def pretty_question
    (answers.count > 1) ? "#{question} [#{answers.count} answers]" : question
  end

  def pretty_choices
    choices_string=''
    choices.zip(:A..:Z) do |choice, letter|
      choices_string += "#{letter}. #{choice}\n"
    end
    choices_string
  end

  def to_s
    <<Q_TEXT
QUESTION ID #{id} : #{topic}
#{pretty_question}

#{pretty_choices}
>> CORRECT ANSWER IS: #{answers.join ' '}

#{reference}
Q_TEXT
  end

  def report
    puts ['=CONCATENATE(B2,": ",C2)', id, "\"#{responses}\"", '=IF(B2<>B1,1,IF(A2=A1,D1,D1+1))', quiz.student_id, "\"#{pretty_question}\n\n#{pretty_choices}\"", "\"#{answers}\""].join("\t")
  end
end

#######################################################
class Quiz
  attr_accessor :course, :title, :exam_date, :score,
                :student_name, :student_email, :student_company

  attr_reader :student_id
  def student_id=(an_id); @student_id = an_id.to_sym; end

  def initialize(init_hash={})
    init_hash.each_pair {|key, value| self.send("#{key}=", value)}
    yield self if block_given?
  end

  def questions; @questions ||= []; end

  def new_question(*args, &block)
    questions << (new_q = QuizQuestion.new(*args, &block))
    new_q.quiz = self
    new_q
  end

  def to_s
    q_title = "\"#{self.title}\" [#{self.questions.count} questions]"
    return q_title if self.student_name == nil
    "#{q_title}, results for #{self.student_name} (#{self.score}%)" if self.student_name != nil
  end

  def report
    puts ['Q#:Wrong', '#', 'Wrong', 'Unique #', 'User', 'Section', 'Quest', 'Answ', 'Source'].join("\t")
  end
end
