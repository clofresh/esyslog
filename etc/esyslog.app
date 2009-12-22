{application, esyslog, [
	{description,  "Erlang Syslog Server"},
	{mod, {esyslog, []}},
	{env, [
		{port, 7777},
		{couchdb, {"localhost", 5984}}
	]}
]}.
