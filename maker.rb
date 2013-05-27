#!/usr/bin/env ruby

#require 'rubygems'
require 'date'
require 'thread'
#require 'open4'
require 'rbconfig'
require 'find'
require 'erb'

Dir.chdir(File.dirname(__FILE__))

def project_name
  File.split(File.expand_path(File.dirname(__FILE__)))[-1]
end

def is_windows?
  RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/
end

def run(cmd)
  o = system(cmd)
  raise "error!" unless o
end

def clean_hpc
  system('mkdir -p dist/hpc')
  system('rm dist/hpc/* 2> /dev/null')
  system('rm *.tix 2> /dev/null')
  system('rm -r .hpc 2> /dev/null')
end

def clean
  clean_hpc
  system('rm -r dist')
end

def build_models
  @model_paths = []
  Find.find(File.join('src','Model')) do |filename|
    if filename =~ /\.hs\z/ && !filename.include?('#') && !filename.include?('~')
      @model_paths << filename
    end
  end
  @model_names = @model_paths.map do |path|
    pathlist = path.split(File::SEPARATOR)
    pathlist[1..-2].join('.') + '.' + pathlist[-1][0..-4]
  end
  File.open(File.join('src','Model.hs'), 'w') do |f|
    erb = File.read(File.join('src','Model.hs.erb'))
    f.write(ERB.new(erb).result())
  end
end

def build_actions
  @action_paths = []
  Find.find(File.join('src','Action')) do |filename|
    if filename =~ /\.hs\z/ && !filename.include?('#') && !filename.include?('~')
      @action_paths << filename
    end
  end
  @action_names = @action_paths.map do |path|
    pathlist = path.split(File::SEPARATOR)
    pathlist[1..-2].join('.') + '.' + pathlist[-1][0..-4]
  end
  File.open(File.join('src','Action.hs'), 'w') do |f|
    erb = File.read(File.join('src','Action.hs.erb'))
    f.write(ERB.new(erb).result())
  end
end

def build_tests
  @test_paths = []
  Find.find(File.join('src','Tests')) do |filename|
    if filename =~ /Tests?\.hs\z/ && !filename.include?('#') && !filename.include?('~')
      @test_paths << filename
    end
  end
  @test_modules = @test_paths.map do |path|
    pathlist = path.split(File::SEPARATOR)
    pathlist[1..-2].join('.') + '.' + pathlist[-1][0..-4]
  end
  File.open(File.join('src','Tests.hs'), 'w') do |f|
    erb = File.read(File.join('src','Tests.hs.erb'))
    f.write(ERB.new(erb).result())
  end
end

def build_cabal
  system('cabal configure')
  raise "cabal configure failed!" unless $?.success?
  system('cabal build')
  raise "cabal build failed!" unless $?.success?
end

def build
  build_models
  build_actions
  build_tests
  build_cabal
end

def install
  build
  system('cabal install')
end

def hlint
  system('mkdir -p dist')
  system('hlint src --report=dist/hlint.html')
  puts('Point your browser at file://' + `pwd`.strip + '/dist/hlint.html')
end

def test
  clean
  build
  run('mkdir -p dist/hpc')
  run("./dist/build/#{project_name}-tests/#{project_name}-tests")
  run("hpc report #{project_name}-tests.tix")
  run("hpc report #{project_name}-tests.tix > dist/hpc/report.txt")
  run("hpc markup #{project_name}-tests.tix --destdir=dist/hpc >> /dev/null")
  puts('Coverage data stored in dist/hpc')
  puts('Point your browser at file://' + `pwd`.strip + '/dist/hpc/hpc_index.html')
end

def build_examples
  Dir.glob("examples/*/build.sh") do |f|
    system(f)
    raise "example #{f} failed to build!" unless $?.success?
  end
end

def integration_test
  selenium = fork do
    exec('java -jar selenium-server-standalone.jar')
  end
  t0 = Time.now.to_i
  clean
  install
  build_examples
  system('cd examples/tests ; ghc --make Main.hs')
  raise "failed to build example tests!" unless $?.success?
  # give time for the selenium server to start
  sleep 0.1 until Time.now.to_i - t0 >= 5
  system('cd examples/tests ; ./Main')
  raise "examples failed!" unless $?.success?
ensure
  Process.kill("INT", selenium)
  Process.waitpid(selenium)
end

start = DateTime.now
runcount = 0
ARGV.each do |x|
  self.send(x.to_sym)
  runcount += 1
end
if runcount <= 0
  puts "Usage:   ./maker.rb [action1] [[action2]...]"
  puts "Example: ./maker.rb test"
else
  finish = DateTime.now
  diff = ((finish - start) * (24*60*60)).to_f
  puts("started: #{start.to_s} finished: #{finish.to_s}")
  puts("took #{diff} seconds")
end

