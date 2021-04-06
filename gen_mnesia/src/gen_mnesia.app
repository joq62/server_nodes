{application, gen_mnesia,
 [{description, " gen_mnesia"},
  {vsn, "1.0.0"},
  {modules, [gen_mnesia_app,
             gen_mnesia_sup,
	     gen_mnesia_lib,
	     gen_mnesia]},
  {registered, [gen_mnesia]},
  {applications, [kernel, stdlib]},
  {mod, {gen_mnesia_app, []}}
 ]}.
