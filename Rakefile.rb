require 'rake/clean'

ERLC_FLAGS = "-Iinclude +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")

CLEAN.include("ebin/*.beam")
CLEAN.include("ebin/*.app")

directory 'ebin'

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
  sh "erlc -D EUNIT -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

file "ebin/esyslog.app" => ["src/esyslog.app"] do |t|
  cp t.prerequisites.first, t.name
end  

task :app => [:compile, "ebin/esyslog.app"]
task :compile => ['ebin'] + OBJ
task :default => :app

task :test => [:compile] do
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

