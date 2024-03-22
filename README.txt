cl-tab is an attempt at a general structured or tabular data analysis
Common Lisp library that can be compiled into different final
implementation languages for execution.

The motivation for this library is well-summarized by a solution to
the following problem: You have medical data stored on two different
servers but with databases that have similar structures, and you wish
to compare metrics of this data between these two databases as well as
compute metrics that are complex functions of data from both servers.

The solution that has been implemented thus far by Evan and myself is
to use Python for client-side computing to perform the final analysis.
This involves a hard barrier between client and server as the language
used to express computations on the client (Python+Pandas) is
different from the language used to express computations on the server
(T-SQL).  However, many times during my own work I have noticed that
SQL is still well-suited to the analysis, and Python+Pandas is more
verbose and exhibits inconsistent behavior relative to SQL (e.g. the
handling of NULL data and empty strings), but the limitation of
computing on separate servers makes SQL impossible to use without
perhaps resorting to something like SSIS.

The approach of this project is to define a language that generally
operates on structured or tabular data with a mostly declarative or
functional syntax, although mutable state is not out of the question.
To that end, analogs of all SQL operations are the starting point for
defining the language, but making sure to construct the language in
such a way that objects are returned by every expression, and so that
the results of well-known SQL operations are expressed by composition
of operators.  This leads to expressions that describe chains of
function calls rather than idiosyncratic sentences in the
quasi-English language of SQL.


Basic principles & operators:

- Atoms are basic data types.  These will most likely be limited to
  the intersection between LISP and SQL data types.

- Tables are basic data types, and tables are not atoms.  Tables are
  always ordered, i.e. there is no such thing as unordered row access
  from the table.  (This requires special handling when compiling to
  SQL, but I believe this may be worth the effort to simplify the
  language, especially by always having an implicit primary key in the
  event that a primary key is not specified for a table.)

- Columns are generic functions that operate on tables to return all
  rows of the specified column.

- (tau &rest columns): Construct a table from columns.  (From Greek
  τράπεζα.)  E.g.,

  (tau (x table1) (y table2)) => construct a table using data from
  table1.x and table2.y in SQL.  The details of this operation need
  precise definitions, but there should be an implicit ordering of
  tables and a principle that data is not lost by default.  This means
  columns that are shorter will have NULL values inserted into the
  extra rows.

  A sister operation that only includes rows where all columns have
  available data is

  (tau* &rest columns): Construct a table from columns, but excluding
  rows that are unavailable in one or more columns.

  The resulting principle is that

  (x (tau  (x table))) = (x table) is always true, but
  (x (tau* (x table))) = (x table) is not always true.

- (sigma (&rest column-functions) table): SELECT

  Here SELECT is dethroned from its position as the universal query
  for returning a value of some sort, and instead only expresses
  operations that can alter the shape of its input table argument.

  Something like (sigma t table) would be equivalent to

  "select * from table"

  Here there is another idea I'm considering: Let columns be
  functions, so that writing something like (x table) is equivalent to
  table.x in SQL.  But, what type of value is (x table)?  I'm tempted
  to let this denote an entire column, so that something like

  (tau (x table)
       (y table))

  denotes the construction of a table.  If this is the meaning of tau,
  then sigma is essentially a shortened version of tau:

  (sigma (x y) table) <==> (tau (x table) (y table))

- (kappa column-indicator table): Return a column using its numerical
  index or name as a string.  (From Greek κολόνα.)

- (epsilon tab1 &rest (tab2 condition type &key column-prefix)):
  Analog to join (from Greek ενώνω).

  Hopefully the syntax is clear.  Each element of the &rest argument
  must have the form (tab2 condition &key type prefix) which denote a
  table to join, the condition on which these tables are joined, an
  type of join which does not have a default (differs from SQL) for
  the purpose of ensuring clear meaning, and an optional column-prefix
  which specifies a prefix for the column names and generic functions
  to access columns from tab2 by name in the table resulting from the
  join operation.  I'm debating whether to signal an error when
  columns are clobbered in the join.
  
Notes:

* I'm almost ready to completely jettison the idea of the sigma
  operator.  sigma as a replacement for SELECT brings along quite a
  lot of baggage that is likely unnecessary.  If formulas are always
  about columns, then the language quickly simplifies to simple vector
  operations.  Compilation would need to worry about how to express
  these operations as SELECT operations and ensure that appropriate
  orderings are used, but ultimately I would be more concerned with
  operations like sorting than being limited to an order-by operation.
  The sort operator could be compiled down to an order-by in the event
  that it is that simple.

* For implementation: I'm considering a simple approach where the
  entire system is in Common Lisp:

  1. All tables are Lisp objects.  Some directly store data; these
  would be arrays, maybe lists or other Lisp objects.  Others are
  handles to objects on SQL servers or other types of data assets,
  perhaps handles denoting future computations (ambitious).

  2.1. The language would not be declarative with deferred evaluation in
  the way I wrote cl-ana.makeres, but instead would immediately
  execute code as-written, and compiler macros would be used to
  optimize the code sent to a SQL server, with fetched results being
  returned afterwards.

  2.2. As a different option: Computation on remote resources would be
  handled in an deferred or lazy way, so that there would be a
  scheduler responsible for fetching results.  This would mean that
  whenever the value of a remote value is desired, its value would
  need to be awaited and fetched from the remote server.  In some ways
  this would be harder to implement since I think the Common Lisp eval
  function would need to be changed to allow this to happen.
  Essentially I'd have to create my own new REPL environment in which
  to use this new method of evaluation.  Doable, but probably
  difficult.

  2.3. As a third option: Computations on remote hardware are always
  written inside of a special context operator.  This operator (a Lisp
  macro) would transform the code it receives into code that would
  manage I/O between the Lisp image and the remote resource, fetching
  results if specified by the input code.  This would be the easiest
  to implement, but also the least flexible of the options listed so
  far.  The only thing simpler would be manual I/O management and
  instruction of the remote resource, which is what I'm trying to
  avoid with this system.

  3. As part of remote execution, there would be a working space that
  was used by the Lisp system to manage intermediate results and
  possibly to assist in code execution as well.  E.g. if many queries
  are needed, it could be better to send them all in a batch, place
  them into a table, and then use a cursor to fetch queries and
  sequentially execute the statements.  Although, this would be best
  suited for the case where a SQL job is desired so that the client
  can disconnect and reconnect at convenience without losing the
  computation.  I lean towards having this as an additional operator
  rather than the default behavior since it would make things
  complicated.  But, for the working space something like this would
  be useful to e.g. avoid recomputing results in the event of an error
  that needs handling.  Most of this should probably be handled on the
  client, however.

* I contemplated including views as direct objects in the underlying
  Lisp library, but this does not seem like such a good idea since it
  would require something more like generators in order to be useful,
  and then the accesses to the generated data would need memoization.
  I might return to this issue later, but in the spirit of getting
  work done I now leave it unaddressed.
