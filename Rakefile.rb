require 'rake/clean'

ERLC_FLAGS = "-Iinclude +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")

["ebin/*.beam",
 "ebin/*.app",
 "src/esyslog_config_lexer.erl",
 "src/esyslog_config_parser.erl"].each { |f| CLEAN.include(f) }

directory 'ebin'

file "src/esyslog_config_lexer.erl" => ["src/esyslog_config_lexer.xrl"] do |t|
  sh "erl -run leex file src/esyslog_config_lexer -run init stop"
end

file "src/esyslog_config_parser.erl" => ["src/esyslog_config_parser.yrl"] do |t|
  sh "erl -run yecc file src/esyslog_config_parser -run init stop"
end

file "ebin/esyslog.app" => ["etc/esyslog.app"] do |t|
  cp t.prerequisites.first, t.name
end  

task :app => [:generate, :compile, "ebin/esyslog.app"]
task :generate => ["src/esyslog_config_lexer.erl",
                   "src/esyslog_config_parser.erl"]
task :compile => ["ebin"] do
  sh "erlc -D EUNIT -pa ebin -W #{ERLC_FLAGS} -o ebin $(find src -name '*.erl')"
end
task :default => :app

task :test => [:app] do
  puts "Modules under test:"
  OBJ.each do |obj|
    obj[%r{.*/(.*).beam}]
    mod = $1
    test_output = `erl -pa ebin -run #{mod} test -run init stop`

    puts "#{mod}: #{test_output}"
  end
end

task :start => [:app] do
  sh "erl -sname esyslog -pa ebin -s esyslog -run init stop"
end

