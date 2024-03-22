cl-tab is a tabular data analysis library in Common Lisp with the
following structure:

* A Lisp library for tabular data analysis with ordinary Lisp code (in
  progress).

* A DSL which allows analysis code to treat database-hosted data
  accessible via SQL (clsql backend) exactly the same as in-memory
  data that is local to the client.  (Not yet started, see HOPE.txt
  for sketch of ideas.)

The DSL is more ambitious than the Lisp library, but given past
successes with cl-ana.makeres-cpp and cl-opencl, I am optimistic that
much progress can be made.
