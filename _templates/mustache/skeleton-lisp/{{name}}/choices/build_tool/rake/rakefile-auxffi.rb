# FFI auxiliary rakefile script

FFI_LIBDIR = `#{PKG_CONFIG} --variable=libdir intro_c-practice || echo .`
FFI_INCDIR = `#{PKG_CONFIG} --variable=includedir intro_c-practice || echo .`
ENV['LD_LIBRARY_PATH'] = "#{ENV['LD_LIBRARY_PATH']}:#{FFI_LIBDIR}"
sh "export LD_LIBRARY_PATH"

desc 'Prepare Swig files'
task :prep_swig do
  sh "swig -cffi -noswig-lisp -I#{FFI_INCDIR} -outdir build src/classic-c.i || true"
end
