# Targets rakefile script.
require 'rake/clean'
require 'rake/packagetask'

[CLEAN, CLOBBER, Rake::FileList::DEFAULT_IGNORE_PATTERNS].each{|a| a.clear}
CLEAN.include('**/*.o', '*.log', '**/.coverage')
CLOBBER.include('build/*', 'build/.??*')

desc 'Help info'
task :help do
  puts "===== subproject: #{VARS.proj} =====\nHelp: #{RAKE} [COMPILER=\"$(COMPILER}\"] [task]"
  sh "#{RAKE} -T"
end

desc 'Run tests: rake test\[topt1,topt2\]'
task :test, [:topt1] => :testCompile do |t, topts|
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
  sh "LD_LIBRARY_PATH=#{ENV['LD_LIBRARY_PATH']} build/ts_main #{topts[:topt1]} #{topts.extras.join(' ')} || true"
end

desc 'Start REPL & test: rake repl_test'
task :repl_test, [:topt1] do |t, topts|
  sh "LD_LIBRARY_PATH=#{ENV['LD_LIBRARY_PATH']} rlwrap #{COMPILER} --load tests/test-suite.lisp --eval '(asdf:test-system :#{VARS.proj}/test)' #{topts[:topt1]} #{topts.extras.join(' ')} || true"
end

#----------------------------------------
desc 'Uninstall artifacts'
task :uninstall do 
  rm_rf("#{ENV["HOME"]}/quicklisp/local-projects/#{VARS.parent}/#{VARS.proj}") || true
  sh "rlwrap #{COMPILER} --eval '(progn (ql:register-local-projects) (uiop:quit))' || true"
end

desc 'Install artifacts'
task :install do
  ln_sf("#{ENV['PWD']}", "#{VARS.proj}") || true
  mv("#{VARS.proj}", "#{ENV["HOME"]}/quicklisp/local-projects/#{VARS.parent}/") || true
  sh "rlwrap #{COMPILER} --eval '(progn (ql:register-local-projects) (uiop:quit))' || true"
end

file "build/#{VARS.name}-#{VARS.version}" do |p|
  mkdir_p(p.name)
  # sh "zip -9 -q -x @exclude.lst -r - . | unzip -od #{p.name} -"
  sh "tar --posix -L -X exclude.lst -cf - . | tar -xpf - -C #{p.name}"
end
if defined? Rake::PackageTask
  Rake::PackageTask.new(VARS.name, VARS.version) do |p|
    # task("build/#{VARS.name}-#{VARS.version}").invoke
    
    ENV.fetch('FMTS', 'tar.gz').split(',').each{|fmt|
      if p.respond_to? "need_#{fmt.tr('.', '_')}="
        p.send("need_#{fmt.tr('.', '_')}=", true)
      else
        p.need_tar_gz = true
      end
    }
    task(:package).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz')}]"
    task(:repackage).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz')}]"
  end
else
  desc "[FMTS=#{ENV.fetch('FMTS', 'tar.gz')}] Package project distribution"
  task :dist => ["build/#{VARS.name}-#{VARS.version}"] do |t|
    distdir = "#{VARS.name}-#{VARS.version}"
    
    ENV.fetch('FMTS', 'tar.gz').split(',').each{|fmt|
      case fmt
      when 'zip'
        rm_rf("build/#{distdir}.zip") || true
        cd('build') {sh "zip -9 -q -r #{distdir}.zip #{distdir}" || true}
      else
        # tarext = `echo #{fmt} | grep -e '^tar$' -e '^tar.xz$' -e '^tar.bz2$' || echo tar.gz`.chomp
        tarext = fmt.match(%r{(^tar$|^tar.xz$|^tar.bz2$)}) ? fmt : 'tar.gz'
        rm_rf("build/#{distdir}.#{tarext}") || true
        cd('build') {sh "tar --posix -L -caf #{distdir}.#{tarext} #{distdir}" || true}
      end
    }
  end
end

desc 'Generate API documentation'
task :doc do
  rm_rf("build//html") || true
  mkdir_p('build/html')
  sh "rlwrap #{COMPILER} --load doc_gen.lisp --eval '(uiop:quit)' || true"
end
