# Targets rakefile script.
require 'rake/clean'
require 'rake/packagetask'

[CLEAN, CLOBBER, Rake::FileList::DEFAULT_IGNORE_PATTERNS].each{|a| a.clear}
CLEAN.include('**/*.o', '*.log', '**/.coverage')
CLOBBER.include('build/*', 'build/.??*')

desc 'Help info'
task :help do
  puts "===== subproject: #{VARS.proj} =====\nHelp: #{RAKE} [LISP=\"$(LISP}\"] [task]"
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
  if "clisp" == "#{LISP}"
    sh "LD_LIBRARY_PATH=#{ENV['LD_LIBRARY_PATH']} rlwrap #{LISP} -i tests/test-suite.lisp -x '(asdf:test-system :#{VARS.proj}/test)' #{topts[:topt1]} #{topts.extras.join(' ')} || true"
  else
    sh "LD_LIBRARY_PATH=#{ENV['LD_LIBRARY_PATH']} rlwrap #{LISP} --load tests/test-suite.lisp --eval '(asdf:test-system :#{VARS.proj}/test)' #{topts[:topt1]} #{topts.extras.join(' ')} || true"
  end
end

#----------------------------------------
desc 'Uninstall artifacts'
task :uninstall do 
  rm_rf("#{ENV["HOME"]}/quicklisp/local-projects/#{VARS.parent}/#{VARS.proj}") || true
  if "clisp" == "#{LISP}"
    sh "rlwrap #{LISP} -x '(progn (ql:register-local-projects) (format t \"~%~a~%\" (find (quote \"#{VARS.proj}\") (ql:list-local-systems) :test (quote equal))) (uiop:quit))' || true"
  else
    sh "rlwrap #{LISP} --eval '(progn (ql:register-local-projects) (format t \"~%~a~%\" (find (quote \"#{VARS.proj}\") (ql:list-local-systems) :test (quote equal))) (uiop:quit))' || true"
  end
end

desc 'Install artifacts'
task :install do
  ln_sf("#{ENV['PWD']}", "#{VARS.proj}") || true
  mv("#{VARS.proj}", "#{ENV["HOME"]}/quicklisp/local-projects/#{VARS.parent}/") || true
  if "clisp" == "#{LISP}"
    sh "rlwrap #{LISP} -x '(progn (ql:register-local-projects) (format t \"~%~a~%\" (find (quote \"#{VARS.proj}\") (ql:list-local-systems) :test (quote equal))) (uiop:quit))' || true"
  else
    sh "rlwrap #{LISP} --eval '(progn (ql:register-local-projects) (format t \"~%~a~%\" (find (quote \"#{VARS.proj}\") (ql:list-local-systems) :test (quote equal))) (uiop:quit))' || true"
  end
end

file "build/#{VARS.proj}-#{VARS.version}" do |p|
  mkdir_p(p.name)
  # sh "zip -9 -q -x @exclude.lst -r - . | unzip -od #{p.name} -"
  sh "tar --posix -L -X exclude.lst -cf - . | tar -xpf - -C #{p.name}"
end
if defined? Rake::PackageTask
  Rake::PackageTask.new(VARS.proj, VARS.version) do |p|
    # task("build/#{VARS.proj}-#{VARS.version}").invoke
    
    ENV.fetch('FMTS', 'tar.gz,zip').split(',').each{|fmt|
      if p.respond_to? "need_#{fmt.tr('.', '_')}="
        p.send("need_#{fmt.tr('.', '_')}=", true)
      else
        p.need_tar_gz = true
      end
    }
    task(:package).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz,zip')}]"
    task(:repackage).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz,zip')}]"
  end
else
  desc "[FMTS=#{ENV.fetch('FMTS', 'tar.gz,zip')}] Package project distribution"
  task :package => ["#{VARS.proj}-#{VARS.version}"] do |t|
    distdir = "#{VARS.proj}-#{VARS.version}"
    
    ENV.fetch('FMTS', 'tar.gz,zip').split(',').each{|fmt|
      case fmt
      when '7z'
        rm_rf("build/#{distdir}.7z") || true
        cd('build') {sh "7za a -t7z -mx=9 #{distdir}.7z #{distdir}" || true}
      when 'zip'
        rm_rf("build/#{distdir}.zip") || true
        cd('build') {sh "zip -9 -q -r #{distdir}.zip #{distdir}" || true}
      else
        # tarext = `echo #{fmt} | grep -e '^tar$' -e '^tar.xz$' -e '^tar.zst$' -e '^tar.bz2$' || echo tar.gz`.chomp
        tarext = fmt.match(%r{(^tar$|^tar.xz$|^tar.zst$|^tar.bz2$)}) ? fmt : 'tar.gz'
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
  if "clisp" == "#{LISP}"
    sh "rlwrap #{LISP} -i doc_gen.lisp -x '(uiop:quit)' || true"
  else
    sh "rlwrap #{LISP} --load doc_gen.lisp --eval '(uiop:quit)' || true"
  end
end
